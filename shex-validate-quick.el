;;; shex-validate-quick.el --- Validate a node/shape pair on the fly -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-fable-5
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (shexc-ts-mode "3.0.0") (turtle-ts-mode "0.1.0") (shex-validate "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `shex-manifest-browser' validates a node/shape association as part
;; of a persistent YAML manifest entry; `shex-validate-link-buffers'
;; needs an already-written ShapeMap buffer. Neither is what you want
;; for "does this one node conform to this one shape" while just
;; poking around a schema or a data file -- this is that: two
;; commands, one bound in `shexc-ts-mode', one in `turtle-ts-mode',
;; that build a scratch one-association ShapeMap on the fly and show
;; the result, no manifest entry or saved ShapeMap file involved.
;;
;; `shexc-ts-mode-validate-node' (`C-c C-t' in a ShExC buffer): the
;; shape is implicit -- the one at point -- and it prompts for a
;; Turtle buffer (defaulting to the sole one visible in this frame, if
;; there's exactly one) and a node in it, completable against every
;; subject that buffer declares and defaulting to whichever one point
;; is on there, if any.
;;
;; `turtle-ts-mode-validate-node' (`C-c C-t' in a Turtle buffer): the
;; mirror image -- the node is implicit (point's own subject), and it
;; prompts for a schema buffer (same "sole visible one" default) and a
;; shape in it, completable/defaulting the same way.
;;
;; Both directions also turn on live `shex-validate-flymake'
;; diagnostics in the data buffer, via `shex-validate-link-buffers'
;; the same as any other use of that function.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'rdf-turtle)
(require 'shexc-ts-mode)
(require 'turtle-ts-mode)
(require 'shex-validate)

;;; Picking a buffer: default to the sole visible one, else prompt

(defun shex-validate-quick--visible-buffers-in-mode (mode)
  "Buffers derived from MODE currently shown in some window of the
selected frame, in `window-list' order.  Used to offer a sole visible
buffer as a smart default rather than making the user pick one they
can already see on screen."
  (delete-dups
   (mapcar #'window-buffer
           (seq-filter (lambda (w) (with-current-buffer (window-buffer w) (derived-mode-p mode)))
                       (window-list nil 'never)))))

(defun shex-validate-quick--read-buffer-in-mode (prompt mode)
  "Read a live buffer derived from MODE, defaulting to the sole one
currently visible in this frame
\(`shex-validate-quick--visible-buffers-in-mode') if there's exactly
one -- always completable/typeable to a different one regardless, via
REQUIRE-MATCH and a PREDICATE restricting candidates to MODE (so
`TAB' only ever offers buffers that actually make sense here)."
  (let* ((visible (shex-validate-quick--visible-buffers-in-mode mode))
         (default (and (= (length visible) 1) (buffer-name (car visible)))))
    (get-buffer
     (read-buffer prompt default t
                  (lambda (b)
                    (when-let* ((buf (get-buffer (if (consp b) (car b) b))))
                      (with-current-buffer buf (derived-mode-p mode))))))))

;;; Picking a node: default to point's own subject, else complete

(defun shex-validate-quick--turtle-positions (data-buffer)
  "DATA-BUFFER's subject-position table -- see `rdf-turtle-parse-buffer'."
  (with-current-buffer data-buffer
    (let ((positions (make-hash-table :test 'equal)))
      (rdf-turtle-parse-buffer nil positions)
      positions)))

(defun shex-validate-quick--node-at-position (positions pos)
  "The subject (a bare, fully-resolved-IRI or `_:label' string, per
`rdf-turtle-term-key') whose declaration -- per POSITIONS, `rdf-
turtle-parse-buffer''s own output -- contains POS, or nil if none does."
  (catch 'shex-validate-quick--node-at-position
    (maphash (lambda (key spans)
               (when (seq-some (lambda (span) (and (<= (car span) pos) (< pos (cdr span)))) spans)
                 (throw 'shex-validate-quick--node-at-position key)))
             positions)
    nil))

(defun shex-validate-quick--format-node (key)
  "KEY (a bare `rdf-turtle-term-key' string) as ShapeMap-syntax node
text: bracketed for a named node, as-is for a `_:label' blank node."
  (if (string-prefix-p "_:" key) key (concat "<" key ">")))

(defun shex-validate-quick--read-node (data-buffer)
  "Read a node from DATA-BUFFER (ShapeMap-syntax text, e.g. \"<#Alice>\"),
completing against every subject it declares
\(`shex-validate-quick--turtle-positions'), defaulting to whichever one
point is on there (`shex-validate-quick--node-at-position'), if any."
  (let* ((positions (shex-validate-quick--turtle-positions data-buffer))
         (candidates (mapcar #'shex-validate-quick--format-node (hash-table-keys positions)))
         (at-point (shex-validate-quick--node-at-position
                    positions (with-current-buffer data-buffer (point))))
         (default (and at-point (shex-validate-quick--format-node at-point))))
    (completing-read (format "Node%s: " (if default (format " (default %s)" default) ""))
                      candidates nil nil nil nil default)))

;;; Picking a shape: default to point's own shape, else complete

(defun shex-validate-quick--schema-shape-labels (schema-buffer)
  "Every DECLARED shape label's own source text (e.g. \"<TGeek>\") in
SCHEMA-BUFFER -- not references, which aren't sensible validation
targets themselves."
  (with-current-buffer schema-buffer
    (delete-dups
     (mapcar (lambda (n) (treesit-node-text n t))
             (shexc-ts-mode--query-labels
              '((shape_expr_decl label: (shape_expr_label) @label)))))))

(defun shex-validate-quick--read-shape (schema-buffer)
  "Read a shape label from SCHEMA-BUFFER (source text, e.g. \"<TGeek>\"),
completing against every shape it declares
\(`shex-validate-quick--schema-shape-labels'), defaulting to whichever
one point is on there (`shexc-ts-mode--label-at-point'), if any."
  (let* ((candidates (shex-validate-quick--schema-shape-labels schema-buffer))
         (default (with-current-buffer schema-buffer (shexc-ts-mode--label-at-point))))
    (completing-read (format "Shape%s: " (if default (format " (default %s)" default) ""))
                      candidates nil nil nil nil default)))

;;; The shared validate action

(defun shex-validate-quick--shapemap-buffer (data-buffer)
  "A scratch ShapeMap buffer for DATA-BUFFER, reused across repeated
`shex-validate-quick' calls (rather than a fresh buffer every time) so
re-running the command against a new node/shape doesn't leak a buffer
-- and a fresh `shex-validate--register-dependent' hook pointing at it
-- on every invocation."
  (get-buffer-create (format " *shex-validate-quick: %s*" (buffer-name data-buffer))))

(defun shex-validate-quick--validate (schema-buffer data-buffer node-text shape-text)
  "Validate NODE-TEXT (ShapeMap-syntax node text, e.g. \"<#Alice>\")
against SHAPE-TEXT (e.g. \"<TGeek>\") in DATA-BUFFER against SCHEMA-
BUFFER, via a scratch one-association ShapeMap buffer
\(`shex-validate-quick--shapemap-buffer') -- no manifest entry, no
saved ShapeMap file.  Links the buffers (`shex-validate-link-buffers',
which also turns on live `shex-validate-flymake' diagnostics in DATA-
BUFFER) and shows the result (`shex-validate-show-result-shapemap')."
  (let ((shapemap-buffer (shex-validate-quick--shapemap-buffer data-buffer)))
    (with-current-buffer shapemap-buffer
      (erase-buffer)
      (insert node-text "@" shape-text))
    (with-current-buffer data-buffer
      (shex-validate-link-buffers schema-buffer shapemap-buffer)
      (shex-validate-show-result-shapemap data-buffer))))

;;; Entry points

;;;###autoload
(defun shexc-ts-mode-validate-node (data-buffer node)
  "Validate NODE (from DATA-BUFFER, a `turtle-ts-mode' buffer) against
the ShExC shape at point, without creating a manifest entry or a saved
ShapeMap.

Interactively: DATA-BUFFER defaults to the sole `turtle-ts-mode'
buffer currently visible in this frame if there's exactly one,
otherwise prompts (always completable/typeable to a different one
regardless); NODE defaults to whichever subject point is on in DATA-
BUFFER if any, and is otherwise/also completable against every subject
DATA-BUFFER declares.  See `turtle-ts-mode-validate-node' for the
reverse direction."
  (interactive
   (let ((data-buffer (shex-validate-quick--read-buffer-in-mode "Turtle buffer: " 'turtle-ts-mode)))
     (list data-buffer (shex-validate-quick--read-node data-buffer))))
  (unless (derived-mode-p 'shexc-ts-mode) (user-error "Not in a shexc-ts-mode buffer"))
  (let ((shape (or (shexc-ts-mode--label-at-point)
                    (user-error "No shape label at point"))))
    (shex-validate-quick--validate (current-buffer) data-buffer node shape)))

;;;###autoload
(defun turtle-ts-mode-validate-node (schema-buffer shape)
  "Validate the RDF subject at point against SHAPE (from SCHEMA-BUFFER,
a `shexc-ts-mode' buffer), without creating a manifest entry or a
saved ShapeMap.

Interactively: SCHEMA-BUFFER defaults to the sole `shexc-ts-mode'
buffer currently visible in this frame if there's exactly one,
otherwise prompts (always completable/typeable to a different one
regardless); SHAPE defaults to whichever shape point is on in SCHEMA-
BUFFER if any, and is otherwise/also completable against every shape
SCHEMA-BUFFER declares.  See `shexc-ts-mode-validate-node' for the
reverse direction."
  (interactive
   (let ((schema-buffer (shex-validate-quick--read-buffer-in-mode "Schema buffer: " 'shexc-ts-mode)))
     (list schema-buffer (shex-validate-quick--read-shape schema-buffer))))
  (unless (derived-mode-p 'turtle-ts-mode) (user-error "Not in a turtle-ts-mode buffer"))
  (let* ((positions (shex-validate-quick--turtle-positions (current-buffer)))
         (node (or (shex-validate-quick--node-at-position positions (point))
                   (user-error "No RDF subject at point"))))
    (shex-validate-quick--validate schema-buffer (current-buffer)
                                    (shex-validate-quick--format-node node) shape)))

;;;###autoload
(with-eval-after-load 'shexc-ts-mode
  (define-key shexc-ts-mode-map (kbd "C-c C-t") #'shexc-ts-mode-validate-node))

;;;###autoload
(with-eval-after-load 'turtle-ts-mode
  (define-key turtle-ts-mode-map (kbd "C-c C-t") #'turtle-ts-mode-validate-node))

(provide 'shex-validate-quick)

;;; shex-validate-quick.el ends here
