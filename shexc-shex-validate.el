;;; shexc-shex-validate.el --- ShEx conformance checking via flymake -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0") (rdf-store "0.1.0") (rdf-turtle "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A `flymake-diagnostic-functions' backend that tags non-conformant
;; RDF nodes directly in a Turtle (data) buffer, by validating it
;; against a separate ShExC schema buffer and a separate ShapeMap
;; buffer through the `rudof_emacs' dynamic module
;; (https://github.com/rudof-project/rudof's `rudof_emacs' crate --
;; not yet released; build it yourself with `cargo build --release -p
;; rudof_emacs' against a `rudof' checkout and point
;; `shexc-shex-validate-rudof-module-path' at the resulting shared
;; library).
;;
;; Three buffers, one `flymake-mode': the data buffer is the one this
;; backend actually attaches to (`shexc-shex-validate-link-buffers');
;; the schema and ShapeMap buffers are just plain text it reads from,
;; not buffers it adds any backend to themselves. `rudof_emacs' is a
;; *held*, mutable handle (a `user-ptr'), not a one-shot CLI
;; invocation -- so this backend only re-reads the schema/ShapeMap
;; buffers when their own `buffer-chars-modified-tick' has actually
;; changed since last loaded, and always re-reads the data buffer
;; (the one flymake is actually rechecking) every time. See
;; `rudof_emacs/README.md''s "Load order matters" section for why
;; schema+data must both be (re)loaded before the ShapeMap.
;;
;; Mapping a validation result's node string back to a position in
;; the data buffer (so flymake has somewhere to put the squiggly line)
;; uses `rdf-turtle-parse-buffer''s POSITIONS argument -- a *second*,
;; purely Emacs-side parse of the same buffer text `rudof-emacs-read-data'
;; also parses inside Rust. Deliberately not merged into one parse:
;; keeps this file independent of `rudof_emacs' internals (it only
;; ever sees the same plain node/shape/status/reason strings any other
;; caller of `rudof-emacs-validate-shex' would), at the cost of
;; parsing the buffer's Turtle text twice per check -- fine for
;; interactive use, revisit if it ever shows up in profiling.

;;; Code:

(require 'flymake)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)

;; Provided by the `rudof_emacs' dynamic module once loaded -- declared
;; so the byte-compiler doesn't warn about calls to them further down.
(declare-function rudof-emacs-new "rudof_emacs")
(declare-function rudof-emacs-read-shex "rudof_emacs")
(declare-function rudof-emacs-read-data "rudof_emacs")
(declare-function rudof-emacs-read-shapemap "rudof_emacs")
(declare-function rudof-emacs-validate-shex "rudof_emacs")
(declare-function module-load "module.c")
(declare-function buffer-chars-modified-tick "buffer.c")

(defgroup shexc-shex-validate nil
  "ShEx conformance checking for `turtle-ts-mode' buffers via flymake."
  :group 'shexc-ts)

(defcustom shexc-shex-validate-rudof-module-path nil
  "Path to the built `rudof_emacs' dynamic module
\(`librudof_emacs.dylib'/`.so'/`.dll', depending on platform), e.g.
\"~/checkouts/rudof-project/rudof/target/release/librudof_emacs.dylib\".
Required before any `shexc-shex-validate' function can run."
  :type '(choice (const :tag "Not configured" nil) file)
  :group 'shexc-shex-validate)

;;; Per-data-buffer state

(defvar-local shexc-shex-validate-schema-buffer nil
  "Buffer holding the ShExC schema to validate this (data) buffer's
RDF content against. Set via `shexc-shex-validate-link-buffers', not
directly.")

(defvar-local shexc-shex-validate-shapemap-buffer nil
  "Buffer holding compact-syntax ShapeMap text (e.g.
\"<node1>@<Shape1>,<node2>@<Shape2>\") naming which node(s) in this
\(data) buffer to validate against which shape(s). Set via
`shexc-shex-validate-link-buffers', not directly.")

(defvar-local shexc-shex-validate--rudof nil
  "This buffer's held `rudof-emacs-new' handle, created lazily on the
first check and reused (mutated in place) on every later one.")

(defvar-local shexc-shex-validate--schema-tick nil
  "`buffer-chars-modified-tick' of `shexc-shex-validate-schema-buffer'
as of the last time its text was loaded into
`shexc-shex-validate--rudof' -- nil means \"never loaded yet\".
Compared on every check so the schema (usually much larger and less
frequently edited than the data buffer) is only re-parsed when it
actually changed.")

(defvar-local shexc-shex-validate--shapemap-tick nil
  "Like `shexc-shex-validate--schema-tick', for
`shexc-shex-validate-shapemap-buffer'.")

;;; Module loading

(defun shexc-shex-validate--ensure-module ()
  "Load the `rudof_emacs' dynamic module if it isn't already."
  (unless (fboundp 'rudof-emacs-new)
    (unless shexc-shex-validate-rudof-module-path
      (user-error "Set `shexc-shex-validate-rudof-module-path' to the built rudof_emacs module first"))
    (module-load (expand-file-name shexc-shex-validate-rudof-module-path))
    (unless (fboundp 'rudof-emacs-new)
      (error "Loaded %s but `rudof-emacs-new' is still unbound"
             shexc-shex-validate-rudof-module-path))))

(defun shexc-shex-validate--buffer-base-iri (buffer)
  "A \"file://...\" URI for BUFFER's file, or nil if it has none --
passed as ShExC/Turtle's BASE so relative IRIs (including ShExC
`IMPORT') resolve the same way they would for that file on disk."
  (let ((file (buffer-file-name buffer)))
    (and file (concat "file://" file))))

;;; Running a check

(defun shexc-shex-validate--check ()
  "Validate the current (data) buffer against
`shexc-shex-validate-schema-buffer' per
`shexc-shex-validate-shapemap-buffer', returning
`rudof-emacs-validate-shex''s result. Signals `user-error' if either
buffer is unset/no longer live."
  (shexc-shex-validate--ensure-module)
  (unless (and shexc-shex-validate-schema-buffer (buffer-live-p shexc-shex-validate-schema-buffer))
    (user-error "No live schema buffer linked -- see `shexc-shex-validate-link-buffers'"))
  (unless (and shexc-shex-validate-shapemap-buffer (buffer-live-p shexc-shex-validate-shapemap-buffer))
    (user-error "No live ShapeMap buffer linked -- see `shexc-shex-validate-link-buffers'"))
  (unless shexc-shex-validate--rudof
    (setq shexc-shex-validate--rudof (rudof-emacs-new)))
  (let ((schema-tick (buffer-chars-modified-tick shexc-shex-validate-schema-buffer))
        (shapemap-tick (buffer-chars-modified-tick shexc-shex-validate-shapemap-buffer)))
    (unless (equal schema-tick shexc-shex-validate--schema-tick)
      (rudof-emacs-read-shex
       shexc-shex-validate--rudof
       (with-current-buffer shexc-shex-validate-schema-buffer (buffer-string))
       nil
       (shexc-shex-validate--buffer-base-iri shexc-shex-validate-schema-buffer))
      (setq shexc-shex-validate--schema-tick schema-tick)
      ;; A changed schema invalidates whatever ShapeMap was loaded
      ;; against the *previous* schema too, even if the ShapeMap
      ;; buffer's own text didn't change -- force its reload below.
      (setq shexc-shex-validate--shapemap-tick nil))
    (rudof-emacs-read-data
     shexc-shex-validate--rudof
     (buffer-string)
     nil
     (shexc-shex-validate--buffer-base-iri (current-buffer)))
    (unless (equal shapemap-tick shexc-shex-validate--shapemap-tick)
      (rudof-emacs-read-shapemap
       shexc-shex-validate--rudof
       (with-current-buffer shexc-shex-validate-shapemap-buffer (buffer-string))
       nil nil nil)
      (setq shexc-shex-validate--shapemap-tick shapemap-tick))
    (rudof-emacs-validate-shex shexc-shex-validate--rudof)))

;;; Diagnostics

(defun shexc-shex-validate--diagnostics (quadruples positions)
  "Build `flymake-diagnostic' objects for the current (data) buffer
from QUADRUPLES (`rudof-emacs-validate-shex''s result) and POSITIONS
\(`rdf-turtle-parse-buffer''s subject-position table for this same
buffer's current text). \"conformant\"/\"pending\" associations
produce no diagnostic. A node that never occurs as a subject in this
buffer at all (e.g. the ShapeMap names a node this buffer only ever
uses as an object) still gets one diagnostic, at `point-min', so the
failure isn't silently dropped."
  (let (diagnostics)
    (pcase-dolist (`(,node ,shape ,status ,reason) quadruples)
      (unless (member status '("conformant" "pending"))
        (let ((spans (gethash node positions))
              (message (format "ShEx %s against %s: %s" status shape reason)))
          (if spans
              (dolist (span spans)
                (push (flymake-make-diagnostic (current-buffer) (car span) (cdr span) :error message)
                      diagnostics))
            (push (flymake-make-diagnostic
                   (current-buffer) (point-min) (point-max) :error
                   (format "%s (node %s does not occur as a subject in this buffer)" message node))
                  diagnostics)))))
    diagnostics))

;;;###autoload
(defun shexc-shex-validate-flymake (report-fn &rest _args)
  "A `flymake-diagnostic-functions' backend: validates the current
buffer's RDF data against `shexc-shex-validate-schema-buffer' per
`shexc-shex-validate-shapemap-buffer' and reports the result via
REPORT-FN. A setup/configuration failure (module not loaded, buffers
not linked, schema/ShapeMap/data syntax error, ...) is reported as a
single whole-buffer diagnostic rather than left to flymake's own
backend-disabling behavior, so a transient mistake while mid-edit in
the schema buffer doesn't require manually re-enabling this backend
once it's fixed."
  (condition-case err
      (let ((quadruples (shexc-shex-validate--check))
            (positions (make-hash-table :test #'equal)))
        (rdf-turtle-parse-buffer (rdf-store-create) positions)
        (funcall report-fn (shexc-shex-validate--diagnostics quadruples positions)))
    (error
     (funcall report-fn
              (list (flymake-make-diagnostic
                     (current-buffer) (point-min) (point-max) :error
                     (error-message-string err)))))))

;;;###autoload
(defun shexc-shex-validate-link-buffers (schema-buffer shapemap-buffer)
  "Link the current (data) buffer to SCHEMA-BUFFER and SHAPEMAP-BUFFER
and enable `shexc-shex-validate-flymake' (plus `flymake-mode') in the
current buffer. Call this from the data buffer, once per buffer --
not from the schema/ShapeMap buffers, which get no backend of their
own."
  (interactive
   (list (read-buffer "Schema buffer: " nil t)
         (read-buffer "ShapeMap buffer: " nil t)))
  (setq shexc-shex-validate-schema-buffer (get-buffer schema-buffer))
  (setq shexc-shex-validate-shapemap-buffer (get-buffer shapemap-buffer))
  (add-hook 'flymake-diagnostic-functions #'shexc-shex-validate-flymake nil t)
  (flymake-mode 1))

(provide 'shexc-shex-validate)

;;; shexc-shex-validate.el ends here
