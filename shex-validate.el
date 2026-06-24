;;; shex-validate.el --- ShEx conformance checking via flymake -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0") (rdf-store "0.1.0") (rdf-turtle "0.1.0") (shexc-shexj "3.0.0"))
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
;; `shex-validate-rudof-module-path' at the resulting shared
;; library).
;;
;; Three buffers, one `flymake-mode': the data buffer is the one this
;; backend actually attaches to (`shex-validate-link-buffers');
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

(require 'seq)
(require 'flymake)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)
(require 'tabulated-list)
(require 'shexc-shexj)

;; Provided by the `rudof_emacs' dynamic module once loaded -- declared
;; so the byte-compiler doesn't warn about calls to them further down.
(declare-function rudof-emacs-new "rudof_emacs")
(declare-function rudof-emacs-read-shex "rudof_emacs")
(declare-function rudof-emacs-read-data "rudof_emacs")
(declare-function rudof-emacs-read-shapemap "rudof_emacs")
(declare-function treesit-buffer-root-node "treesit.c")
(declare-function treesit-query-capture "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function rudof-emacs-validate-shex "rudof_emacs")
(declare-function module-load "module.c")
(declare-function buffer-chars-modified-tick "buffer.c")

(defgroup shex-validate nil
  "ShEx conformance checking for `turtle-ts-mode' buffers via flymake."
  :group 'shexc-ts)

(defcustom shex-validate-rudof-module-path nil
  "Path to the built `rudof_emacs' dynamic module
\(`librudof_emacs.dylib'/`.so'/`.dll', depending on platform), e.g.
\"~/checkouts/rudof-project/rudof/target/release/librudof_emacs.dylib\".
Required before any `shex-validate' function can run."
  :type '(choice (const :tag "Not configured" nil) file)
  :group 'shex-validate)

;;; Per-data-buffer state

(defvar-local shex-validate-schema-buffer nil
  "Buffer holding the ShExC schema to validate this (data) buffer's
RDF content against. Set via `shex-validate-link-buffers', not
directly.")

(defvar-local shex-validate-shapemap-buffer nil
  "Buffer holding compact-syntax ShapeMap text (e.g.
\"<node1>@<Shape1>,<node2>@<Shape2>\") naming which node(s) in this
\(data) buffer to validate against which shape(s). Set via
`shex-validate-link-buffers', not directly.")

(defvar-local shex-validate--rudof nil
  "This buffer's held `rudof-emacs-new' handle, created lazily on the
first check and reused (mutated in place) on every later one.")

(defvar-local shex-validate--schema-tick nil
  "`buffer-chars-modified-tick' of `shex-validate-schema-buffer'
as of the last time its text was loaded into
`shex-validate--rudof' -- nil means \"never loaded yet\".
Compared on every check so the schema (usually much larger and less
frequently edited than the data buffer) is only re-parsed when it
actually changed.")

(defvar-local shex-validate--shapemap-tick nil
  "Like `shex-validate--schema-tick', for
`shex-validate-shapemap-buffer'.")

;;; Module loading

(defun shex-validate--ensure-module ()
  "Load the `rudof_emacs' dynamic module if it isn't already."
  (unless (fboundp 'rudof-emacs-new)
    (unless shex-validate-rudof-module-path
      (user-error "Set `shex-validate-rudof-module-path' to the built rudof_emacs module first"))
    (module-load (expand-file-name shex-validate-rudof-module-path))
    (unless (fboundp 'rudof-emacs-new)
      (error "Loaded %s but `rudof-emacs-new' is still unbound"
             shex-validate-rudof-module-path))))

(defun shex-validate--buffer-base-iri (buffer)
  "A \"file://...\" URI for BUFFER's file, or nil if it has none --
passed as ShExC/Turtle's BASE so relative IRIs (including ShExC
`IMPORT') resolve the same way they would for that file on disk."
  (let ((file (buffer-file-name buffer)))
    (and file (concat "file://" file))))

;;; Running a check

(defun shex-validate--check ()
  "Validate the current (data) buffer against
`shex-validate-schema-buffer' per
`shex-validate-shapemap-buffer', returning
`rudof-emacs-validate-shex''s result. Signals `user-error' if either
buffer is unset/no longer live."
  (shex-validate--ensure-module)
  (unless (and shex-validate-schema-buffer (buffer-live-p shex-validate-schema-buffer))
    (user-error "No live schema buffer linked -- see `shex-validate-link-buffers'"))
  (unless (and shex-validate-shapemap-buffer (buffer-live-p shex-validate-shapemap-buffer))
    (user-error "No live ShapeMap buffer linked -- see `shex-validate-link-buffers'"))
  (unless shex-validate--rudof
    (setq shex-validate--rudof (rudof-emacs-new)))
  (let ((schema-tick (buffer-chars-modified-tick shex-validate-schema-buffer))
        (shapemap-tick (buffer-chars-modified-tick shex-validate-shapemap-buffer)))
    (unless (equal schema-tick shex-validate--schema-tick)
      (rudof-emacs-read-shex
       shex-validate--rudof
       (with-current-buffer shex-validate-schema-buffer (buffer-string))
       nil
       (shex-validate--buffer-base-iri shex-validate-schema-buffer))
      (setq shex-validate--schema-tick schema-tick)
      ;; A changed schema invalidates whatever ShapeMap was loaded
      ;; against the *previous* schema too, even if the ShapeMap
      ;; buffer's own text didn't change -- force its reload below.
      (setq shex-validate--shapemap-tick nil))
    (rudof-emacs-read-data
     shex-validate--rudof
     (buffer-string)
     nil
     (shex-validate--buffer-base-iri (current-buffer)))
    (unless (equal shapemap-tick shex-validate--shapemap-tick)
      (rudof-emacs-read-shapemap
       shex-validate--rudof
       (with-current-buffer shex-validate-shapemap-buffer (buffer-string))
       nil nil nil)
      (setq shex-validate--shapemap-tick shapemap-tick))
    (rudof-emacs-validate-shex shex-validate--rudof)))

(defun shex-validate--check-with-positions (data-buffer)
  "Run `shex-validate--check' in DATA-BUFFER and return
\(QUADRUPLES . POSITIONS) -- POSITIONS is DATA-BUFFER's current
subject-position table (see `rdf-turtle-parse-buffer'), for mapping a
quadruple's node back to a position in DATA-BUFFER. Shared by
`shex-validate-flymake' and
`shex-validate-show-result-shapemap', the two front ends for the
same underlying check."
  (with-current-buffer data-buffer
    (let ((quadruples (shex-validate--check))
          (positions (make-hash-table :test #'equal)))
      (rdf-turtle-parse-buffer (rdf-store-create) positions)
      (cons quadruples positions))))

;;; Diagnostics

(defun shex-validate--diagnostics (quadruples positions)
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
(defun shex-validate-flymake (report-fn &rest _args)
  "A `flymake-diagnostic-functions' backend: validates the current
buffer's RDF data against `shex-validate-schema-buffer' per
`shex-validate-shapemap-buffer' and reports the result via
REPORT-FN. A setup/configuration failure (module not loaded, buffers
not linked, schema/ShapeMap/data syntax error, ...) is reported as a
single whole-buffer diagnostic rather than left to flymake's own
backend-disabling behavior, so a transient mistake while mid-edit in
the schema buffer doesn't require manually re-enabling this backend
once it's fixed."
  (condition-case err
      (let* ((checked (shex-validate--check-with-positions (current-buffer)))
             (quadruples (car checked))
             (positions (cdr checked)))
        (funcall report-fn (shex-validate--diagnostics quadruples positions)))
    (error
     (funcall report-fn
              (list (flymake-make-diagnostic
                     (current-buffer) (point-min) (point-max) :error
                     (error-message-string err)))))))

;;;###autoload
(defun shex-validate-link-buffers (schema-buffer shapemap-buffer)
  "Link the current (data) buffer to SCHEMA-BUFFER and SHAPEMAP-BUFFER
and enable `shex-validate-flymake' (plus `flymake-mode') in the
current buffer. Call this from the data buffer, once per buffer --
not from the schema/ShapeMap buffers, which get no backend of their
own."
  (interactive
   (list (read-buffer "Schema buffer: " nil t)
         (read-buffer "ShapeMap buffer: " nil t)))
  (setq shex-validate-schema-buffer (get-buffer schema-buffer))
  (setq shex-validate-shapemap-buffer (get-buffer shapemap-buffer))
  (add-hook 'flymake-diagnostic-functions #'shex-validate-flymake nil t)
  (flymake-mode 1))

;;; Result ShapeMap buffer
;;
;; Unlike `shex-validate-flymake' (which only ever surfaces
;; failures, in-place), this shows every node/shape association in
;; the ShapeMap -- "conformant" rows included -- in its own
;; `tabulated-list-mode' buffer, one row per association, sortable by
;; any column. `RET' jumps to that row's node in the data buffer; `g'
;; (`revert-buffer', bound by `tabulated-list-mode' already)
;; re-validates and redraws every row in place, exactly like
;; `list-processes'/`package-menu-mode'.

(defvar-local shex-validate-results--data-buffer nil
  "The data buffer this `*ShEx Result ShapeMap*' buffer reports on.")

(defvar-local shex-validate-results--positions nil
  "`shex-validate-results--data-buffer''s subject-position table
\(see `rdf-turtle-parse-buffer'), as of this results buffer's last
\(re)validation -- consulted by `shex-validate-results-goto-node'.")

(defun shex-validate-results--status-face (status)
  (pcase status
    ("conformant" 'success)
    ("pending" 'shadow)
    (_ 'error))) ; "nonconformant"/"inconsistent"

(defun shex-validate-results--entries (data-buffer)
  "`tabulated-list-entries' for DATA-BUFFER: validate it and return one
entry per `rudof-emacs-validate-shex' quadruple, ID'd by `(NODE
. SHAPE)' (a ShapeMap's node/shape associations are themselves a set,
so this is already unique). Refreshes
`shex-validate-results--positions' as a side effect, since this
function is called by `tabulated-list-print' (hence: on every `g')."
  (let ((checked (shex-validate--check-with-positions data-buffer)))
    (setq shex-validate-results--positions (cdr checked))
    (mapcar
     (lambda (quad)
       (pcase-let ((`(,node ,shape ,status ,reason) quad))
         (list (cons node shape)
               (vector node shape
                       (propertize status 'face (shex-validate-results--status-face status))
                       ;; rudof's own `reason' strings routinely carry an
                       ;; embedded/trailing newline (e.g. "Shape passed
                       ;; ...\n") -- left alone, `tabulated-list-print'
                       ;; renders that as a literal line break, silently
                       ;; splitting one logical row across two buffer
                       ;; lines and breaking line-based row navigation.
                       (string-trim (replace-regexp-in-string "\n+" " " reason))))))
     (car checked))))

(defun shex-validate-results-goto-node ()
  "Jump to the row at point's node in its data buffer, at the position
recorded as of this results buffer's last (re)validation -- see
`shex-validate-results--positions'. If the node occurs more than
once as a subject there, jumps to the first (topmost) occurrence."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (node (car id))
         (buffer shex-validate-results--data-buffer))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "Data buffer for this results buffer is no longer live"))
    (let ((spans (gethash node shex-validate-results--positions)))
      (unless spans
        (user-error "Node %s does not occur as a subject in %s" node buffer))
      (pop-to-buffer buffer)
      (goto-char (caar (last spans))))))

;;; Following point: highlighting the node/shape at point

(defface shex-validate-highlight
  '((t :inherit highlight))
  "Face for the node/shape of the result-shapemap row at point,
highlighted in the data/schema buffers -- see
`shex-validate-results--highlight-update'."
  :group 'shex-validate)

(defvar-local shex-validate-results--highlight-overlays nil
  "Overlays `shex-validate-results--highlight-update' put in the
data/schema buffers for the row currently at point.")

(defvar-local shex-validate-results--highlight-id nil
  "The `tabulated-list-get-id' last highlighted -- lets
`shex-validate-results--highlight-update' no-op while point
stays on the same row, e.g. on every character typed elsewhere.")

(defun shex-validate-results--clear-highlight ()
  (mapc #'delete-overlay shex-validate-results--highlight-overlays)
  (setq shex-validate-results--highlight-overlays nil))

(defun shex-validate--find-shape-decl (schema-buffer shape)
  "Return the `shape_expr_label' node in SCHEMA-BUFFER declaring SHAPE
\(a fully resolved IRI/`_:label' string, matching a
`rudof-emacs-validate-shex' quadruple's own SHAPE element), or nil.
Resolves each candidate label the same BASE/PREFIX-aware way
`shexc-ts-mode''s own flymake backend does
\(`shexc-shexj-resolve-label'), not by raw source text -- see
`shexc-ts-mode--flymake-undefined-shapes'."
  (with-current-buffer schema-buffer
    (let ((ctx (shexc-shexj-buffer-directive-ctx)))
      (seq-find
       (lambda (n) (equal (shexc-shexj-resolve-label ctx n) shape))
       (treesit-query-capture
        (treesit-buffer-root-node 'shexc)
        '((shape_expr_decl label: (shape_expr_label) @label))
        nil nil t)))))

(defun shex-validate-results--highlight-update ()
  "Highlight the row at point's node (in its data buffer) and shape
\(in its schema buffer), following point -- a buffer-local
`post-command-hook' in every `shex-validate-results-mode'
buffer. A node/shape with no resolvable location (see
`shex-validate--diagnostics'/`shex-validate--find-shape-decl')
is silently left unhighlighted rather than signaling an error on every
command."
  (let ((id (ignore-errors (tabulated-list-get-id))))
    (unless (equal id shex-validate-results--highlight-id)
      (shex-validate-results--clear-highlight)
      (setq shex-validate-results--highlight-id id)
      (when-let* ((node (car id))
                  (shape (cdr id))
                  (data-buffer shex-validate-results--data-buffer)
                  ((buffer-live-p data-buffer)))
        (dolist (span (gethash node shex-validate-results--positions))
          (let ((ov (make-overlay (car span) (cdr span) data-buffer)))
            (overlay-put ov 'face 'shex-validate-highlight)
            (push ov shex-validate-results--highlight-overlays)))
        (when-let* ((schema-buffer (buffer-local-value 'shex-validate-schema-buffer data-buffer))
                    ((buffer-live-p schema-buffer))
                    (label (shex-validate--find-shape-decl schema-buffer shape)))
          (let ((ov (make-overlay (treesit-node-start label) (treesit-node-end label) schema-buffer)))
            (overlay-put ov 'face 'shex-validate-highlight)
            (push ov shex-validate-results--highlight-overlays)))))))

(defvar-keymap shex-validate-results-mode-map
  :parent tabulated-list-mode-map
  "RET" #'shex-validate-results-goto-node
  "<mouse-2>" #'shex-validate-results-goto-node)

(define-derived-mode shex-validate-results-mode tabulated-list-mode "ShEx-Results"
  "Major mode for a ShEx validation result ShapeMap: one row per
node/shape association, `RET' jumps to that node in the data buffer.
Moving point over a row also highlights its node/shape in the
data/schema buffers -- see
`shex-validate-results--highlight-update'."
  (setq tabulated-list-format
        [("Node" 40 t) ("Shape" 30 t) ("Status" 14 t) ("Reason" 0 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (add-hook 'post-command-hook #'shex-validate-results--highlight-update nil t)
  (add-hook 'kill-buffer-hook #'shex-validate-results--clear-highlight nil t))

;;;###autoload
(defun shex-validate-show-result-shapemap (&optional data-buffer)
  "Show a `*ShEx Result ShapeMap: NAME*' buffer for DATA-BUFFER
\(default: the current buffer) in a bottom side window."
  (interactive)
  (let* ((data-buffer (or data-buffer (current-buffer)))
         (results-buffer (get-buffer-create (format "*ShEx Result ShapeMap: %s*" (buffer-name data-buffer)))))
    (with-current-buffer results-buffer
      (shex-validate-results-mode)
      (setq shex-validate-results--data-buffer data-buffer)
      (setq tabulated-list-entries
            (lambda () (shex-validate-results--entries data-buffer)))
      (tabulated-list-print))
    (display-buffer
     results-buffer
     '((display-buffer-in-side-window) (side . bottom) (window-height . 0.25)))))

(provide 'shex-validate)

;;; shex-validate.el ends here
