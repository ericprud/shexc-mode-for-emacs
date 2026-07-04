;;; shex-manifest-browser.el --- Browse/run shex.js-style test manifests  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-5
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (yaml "0.5.0") (shex-validate "0.1.0") (shexc-shexj "3.0.0") (shexc-ts-mode "3.0.0") (turtle-ts-mode "0.1.0") (rdf-turtle "0.1.0") (rdf-store "0.1.0") (rdf-model "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A `tabulated-list-mode' browser for YAML test manifests in the
;; format shex.js/shex-webapp's own `examples/manifest.yaml' uses: a
;; top-level sequence of entries, each naming a schema, a data
;; document, and a queryMap (a ShapeMap-compact-syntax association
;; list, possibly using the `{FOCUS predicate object}'/
;; `{subject predicate FOCUS}' pattern-query extension and/or the
;; `START' keyword), given either inline (`schema:'/`data:'/
;; `queryMap:', a literal string) or by reference (`schemaURL:'/
;; `dataURL:'/`queryMapURL:', a path resolved relative to the manifest
;; file's own directory).  Each entry also names an expected `status'
;; ("conformant"/"nonconformant") -- see `shex-manifest-browser--pass-p'
;; for exactly what that means once `queryMap' resolves to more than
;; one node/shape association (as `{FOCUS ...}' can) or an association
;; is itself negated (a leading "!" on its shape label, e.g.
;; `<node>@!<Shape>' -- the ShapeMap-compact-syntax spelling for "node
;; must NOT conform to Shape", a shex.js/shex-webapp extension `emacs-
;; rudof' itself doesn't parse, so it's handled here instead: stripped
;; before the association reaches `read-shapemap', tracked separately
;; for comparing against the actual result).
;;
;; RET on an entry (`shex-manifest-browser-run-entry'): materializes
;; its schema/data/queryMap into buffers (inline text goes into a
;; temporary buffer named from `schemaLabel'/`dataLabel' when given,
;; reused in place across entries that share a label as long as the
;; operator hasn't edited it meanwhile -- see
;; `shex-manifest-browser--content-buffer'; a URL reference just visits
;; the real file), resolves `queryMap' to a fixed ShapeMap (substituting
;; every `{FOCUS ...}' pattern with the nodes it actually matches in the
;; data buffer, and stripping any `!' negation -- see
;; `shex-manifest-browser--resolve-querymap'), links the schema/
;; ShapeMap/data buffers for `shex-validate' exactly as
;; `shex-validate-link-buffers' would, and shows both the entry's own
;; PASS/FAIL verdict (this file) and the full per-association result
;; table (`shex-validate-show-result-shapemap', already in
;; `shex-validate.el' -- reused as-is, not reimplemented here).
;;
;; Scope deliberately *not* covered by `{...}' pattern resolution: a
;; fixed (non-`_'/`FOCUS') literal in a triplet slot, and a bare `_'
;; node selector matching every node in the graph -- neither is needed
;; by any entry in this package's own `tests/tests-manifest.yaml', and
;; both are straightforward to add later against a real need (see
;; `shex-manifest-browser--resolve-token').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'yaml)
(require 'shex-validate)
(require 'shexc-shexj)
(require 'rdf-turtle)
(require 'rdf-store)
(require 'rdf-model)
(require 'tabulated-list)
(require 'shexc-ts-mode)
(require 'turtle-ts-mode)

(declare-function treesit-ready-p "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-buffer-root-node "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defgroup shex-manifest-browser nil
  "Browse and run shex.js-style YAML test manifests."
  :group 'shex-validate)

;;; Manifest loading

(defun shex-manifest-browser--load-manifest (path)
  "Parse the YAML manifest at PATH into a list of entry alists (symbol
keys, e.g. `schemaLabel'/`schema'/`schemaURL'/.../`status'), in file
order."
  (with-temp-buffer
    (insert-file-contents path)
    (yaml-parse-string (buffer-string) :object-type 'alist :sequence-type 'list)))

(defun shex-manifest-browser--get (entry key)
  (cdr (assq key entry)))

(defun shex-manifest-browser--resolve-path (manifest-dir path)
  (expand-file-name path manifest-dir))

;;; Buffer provisioning
;;
;; Inline schema/data/queryMap text goes into a buffer named from its
;; label (falling back to a bare "*schema*"/"*data*"/"*queryMap*" when
;; the manifest gives none), reused in place -- erased and refilled --
;; across entries that share a label, *unless* the operator has
;; edited it since, in which case it's left alone and a fresh,
;; distinctly-named buffer takes over that label's slot from here on.
;; A URL reference instead visits the real file via `find-file-noselect'
;; (never reaped/reused -- it's the operator's own file, not a
;; scratch buffer this browser owns).

(defvar shex-manifest-browser--owned-buffers (make-hash-table :test 'equal)
  "Maps (KIND . LABEL) to the live buffer `shex-manifest-browser--content-buffer'
last returned for it.")

(defun shex-manifest-browser--buffer-name (kind label)
  (format "*%s%s*" (symbol-name kind) (if label (concat ": " label) "")))

(defun shex-manifest-browser--content-buffer (kind label content mode-fn)
  "Return a live buffer holding CONTENT, named from KIND (`schema'/
`data'/`queryMap'/`shapemap') and LABEL (a string, or nil for a bare
\"*KIND*\"), with MODE-FN (a major-mode function, or nil for
`fundamental-mode') applied.  See this file's Commentary for the
reuse/reap policy."
  (let* ((key (cons kind label))
         (existing (gethash key shex-manifest-browser--owned-buffers))
         (buffer (if (and existing (buffer-live-p existing) (not (buffer-modified-p existing)))
                     existing
                   (generate-new-buffer (shex-manifest-browser--buffer-name kind label)))))
    (with-current-buffer buffer
      (funcall (or mode-fn #'fundamental-mode))
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (puthash key buffer shex-manifest-browser--owned-buffers)
    buffer))

(defun shex-manifest-browser--schema-mode ()
  (if (treesit-ready-p 'shexc t) (shexc-ts-mode) (fundamental-mode)))

(defun shex-manifest-browser--data-mode ()
  (if (and (treesit-ready-p 'turtle t) (fboundp 'turtle-ts-mode))
      (turtle-ts-mode)
    (fundamental-mode)))

(defun shex-manifest-browser--schema-buffer (entry manifest-dir)
  (let ((url (shex-manifest-browser--get entry 'schemaURL)))
    (if url
        (find-file-noselect (shex-manifest-browser--resolve-path manifest-dir url))
      (shex-manifest-browser--content-buffer
       'schema (shex-manifest-browser--get entry 'schemaLabel)
       (or (shex-manifest-browser--get entry 'schema) "")
       #'shex-manifest-browser--schema-mode))))

(defun shex-manifest-browser--data-buffer (entry manifest-dir)
  (let ((url (shex-manifest-browser--get entry 'dataURL)))
    (if url
        (find-file-noselect (shex-manifest-browser--resolve-path manifest-dir url))
      (shex-manifest-browser--content-buffer
       'data (shex-manifest-browser--get entry 'dataLabel)
       (or (shex-manifest-browser--get entry 'data) "")
       #'shex-manifest-browser--data-mode))))

(defun shex-manifest-browser--querymap-text (entry manifest-dir)
  "The entry's raw queryMap text, from `queryMap' or `queryMapURL'."
  (let ((url (shex-manifest-browser--get entry 'queryMapURL)))
    (if url
        (with-temp-buffer
          (insert-file-contents (shex-manifest-browser--resolve-path manifest-dir url))
          (buffer-string))
      (or (shex-manifest-browser--get entry 'queryMap) ""))))

(defun shex-manifest-browser--querymap-buffer (entry manifest-dir)
  "A buffer holding ENTRY's raw (unresolved) queryMap text, for the
operator to inspect -- not what's actually linked for validation, see
`shex-manifest-browser--resolve-querymap' for that."
  (shex-manifest-browser--content-buffer
   'queryMap (shex-manifest-browser--get entry 'dataLabel)
   (shex-manifest-browser--querymap-text entry manifest-dir)
   nil))

;;; queryMap resolution: `{FOCUS ...}'/`{... FOCUS}' patterns, `!' negation
;;
;; `START' and any plain node/shape reference (absolute/relative IRI,
;; or prefixed name) need no resolution here at all: `emacs-rudof''s
;; own `read-shapemap' already recognizes the `START' keyword and
;; already resolves prefixed names against whichever schema/data text
;; was last loaded into that same handle (confirmed empirically -- see
;; this package's own commit history) -- the only reason
;; `shex-validate--check' passes explicit `base-nodes'/`base-shapes' at
;; all is for a *bare relative* `<...>' IRI reference, which has no
;; prefix to fall back on.

(defun shex-manifest-browser--split-top-level (text sep)
  "Split TEXT on SEP (a character), ignoring SEP inside \"...\"/<...>/{...}."
  (let ((depth 0) (in-str nil) (start 0) parts (n (length text)))
    (dotimes (i n)
      (let ((c (aref text i)))
        (cond
         (in-str (when (eq c ?\") (setq in-str nil)))
         ((eq c ?\") (setq in-str t))
         ((memq c '(?< ?{)) (setq depth (1+ depth)))
         ((memq c '(?> ?})) (setq depth (max 0 (1- depth))))
         ((and (eq c sep) (= depth 0))
          (push (substring text start i) parts)
          (setq start (1+ i))))))
    (push (substring text start) parts)
    (nreverse parts)))

(defun shex-manifest-browser--split-association (assoc-text)
  "Return (NODE-SELECTOR . SHAPE-LABEL), splitting ASSOC-TEXT on its
rightmost depth-0 `@'."
  (let ((depth 0) (in-str nil) (at-pos nil) (n (length assoc-text)))
    (dotimes (i n)
      (let ((c (aref assoc-text i)))
        (cond
         (in-str (when (eq c ?\") (setq in-str nil)))
         ((eq c ?\") (setq in-str t))
         ((memq c '(?< ?{)) (setq depth (1+ depth)))
         ((memq c '(?> ?})) (setq depth (max 0 (1- depth))))
         ((and (eq c ?@) (= depth 0)) (setq at-pos i)))))
    (unless at-pos
      (error "shex-manifest-browser: no `@' in queryMap association `%s'" assoc-text))
    (cons (string-trim (substring assoc-text 0 at-pos))
          (string-trim (substring assoc-text (1+ at-pos))))))

(defun shex-manifest-browser--parse-shape-label (label)
  "Return (NEGATED . TEXT) for LABEL, stripping a leading `!' (shex.js/
shex-webapp's ShapeMap-compact-syntax negation marker -- `emacs-rudof'
itself doesn't parse this, so it never reaches `read-shapemap')."
  (if (string-prefix-p "!" label)
      (cons t (string-trim (substring label 1)))
    (cons nil label)))

(defun shex-manifest-browser--turtle-ctx (data-buffer)
  "DATA-BUFFER's `rdf-turtle--ctx' (BASE/PREFIX only) -- the same table
`rdf-turtle-parse-buffer' resolved DATA-BUFFER's own triples against,
for resolving a `{...}' pattern's raw predicate/object tokens the same
way."
  (with-current-buffer data-buffer
    (let ((ctx (rdf-turtle--ctx-create)))
      (treesit-parser-create 'turtle)
      (dolist (statement (rdf-turtle--children-of-types (treesit-buffer-root-node 'turtle) '("statement")))
        (let ((child (treesit-node-child statement 0 t)))
          (when (equal (treesit-node-type child) "directive")
            (rdf-turtle--process-directive ctx child))))
      ctx)))

(defun shex-manifest-browser--resolve-token (token ctx)
  "Resolve one `{...}' triplet-pattern TOKEN: `FOCUS' -> the symbol
`focus', `_' -> nil (wildcard), `<iri>'/`prefix:local' -> an
`rdf-model' named-node term, resolved via CTX.  A fixed literal token
isn't supported (not needed by any pattern this browser is exercised
against so far -- see this file's Commentary)."
  (cond
   ((equal token "FOCUS") 'focus)
   ((equal token "_") nil)
   ((string-prefix-p "<" token)
    (rdf-model-named-node-create
     :value (rdf-turtle--resolve-iri ctx (substring token 1 (1- (length token))))))
   ((string-search ":" token)
    (let* ((colon (string-search ":" token))
           (prefix (substring token 0 colon))
           (local (substring token (1+ colon)))
           (base-iri (gethash prefix (rdf-turtle--ctx-prefixes ctx))))
      (unless base-iri
        (error "shex-manifest-browser: undefined prefix `%s:' in `%s'" prefix token))
      (rdf-model-named-node-create :value (concat base-iri local))))
   (t (error "shex-manifest-browser: don't know how to resolve queryMap token `%s'" token))))

(defun shex-manifest-browser--term-lex (term)
  "TERM (an `rdf-model' named-node/blank-node) as ShapeMap-compact-syntax
node text."
  (cond
   ((rdf-model-named-node-p term) (concat "<" (rdf-model-named-node-value term) ">"))
   ((rdf-model-blank-node-p term) (concat "_:" (rdf-model-blank-node-value term)))
   (t (error "shex-manifest-browser: don't know how to render term %S as a ShapeMap node" term))))

(defun shex-manifest-browser--resolve-pattern (node-selector data-buffer)
  "NODE-SELECTOR is a `{SUBJECT PREDICATE OBJECT}' triplet-pattern
string (already trimmed) with exactly one of SUBJECT/OBJECT as the
literal `FOCUS' and the other either `_' (wildcard) or a fixed IRI/
prefixed-name term; PREDICATE is always a concrete term.  Query
DATA-BUFFER (parsed via `rdf-turtle-parse-buffer') for every matching
triple and return the list of ShapeMap node-text strings for FOCUS's
resolved value, one per match."
  (let* ((inner (string-trim (substring node-selector 1 (1- (length node-selector)))))
         (tokens (split-string inner "[ \t\n]+" t)))
    (unless (= (length tokens) 3)
      (error "shex-manifest-browser: expected `{SUBJECT PREDICATE OBJECT}', got `%s'" node-selector))
    (unless (treesit-ready-p 'turtle t)
      (error "shex-manifest-browser: `{...}' queryMap patterns need the `turtle' tree-sitter grammar"))
    (cl-destructuring-bind (subj pred obj) tokens
      (let ((focus-pos (cond ((equal subj "FOCUS") 'subject)
                              ((equal obj "FOCUS") 'object)
                              (t (error "shex-manifest-browser: `{%s}' needs `FOCUS' as subject or object" inner)))))
        (let* ((ctx (shex-manifest-browser--turtle-ctx data-buffer))
               (subj-term (unless (eq focus-pos 'subject) (shex-manifest-browser--resolve-token subj ctx)))
               (pred-term (shex-manifest-browser--resolve-token pred ctx))
               (obj-term (unless (eq focus-pos 'object) (shex-manifest-browser--resolve-token obj ctx)))
               (store (with-current-buffer data-buffer (rdf-turtle-parse-buffer)))
               (matches (rdf-model-dataset-match store subj-term pred-term obj-term)))
          (mapcar (lambda (q)
                    (shex-manifest-browser--term-lex
                     (if (eq focus-pos 'subject) (rdf-model-quad-subject q) (rdf-model-quad-object q))))
                  matches))))))

(cl-defstruct (shex-manifest-browser--assoc)
  "One resolved queryMap association: NODE-TEXT@SHAPE-TEXT, plus
whether it was `!'-negated in the original queryMap (see
`shex-manifest-browser--pass-p')."
  node-text shape-text negated)

(defun shex-manifest-browser--resolve-querymap (querymap-text data-buffer)
  "Resolve QUERYMAP-TEXT (a `queryMap' entry's raw text, possibly with
`{FOCUS ...}'/`{... FOCUS}' patterns and/or `!'-negated associations)
against DATA-BUFFER into a list of `shex-manifest-browser--assoc'."
  (let (result)
    (dolist (assoc-text (shex-manifest-browser--split-top-level querymap-text ?,))
      (let ((trimmed (string-trim assoc-text)))
        (unless (string-empty-p trimmed)
          (pcase-let ((`(,node-selector . ,shape-label) (shex-manifest-browser--split-association trimmed)))
            (pcase-let ((`(,negated . ,shape-text) (shex-manifest-browser--parse-shape-label shape-label)))
              (dolist (node-text (if (string-prefix-p "{" node-selector)
                                      (shex-manifest-browser--resolve-pattern node-selector data-buffer)
                                    (list node-selector)))
                (push (make-shex-manifest-browser--assoc
                       :node-text node-text :shape-text shape-text :negated negated)
                      result)))))))
    (nreverse result)))

(defun shex-manifest-browser--shapemap-text (assocs)
  "The fixed ShapeMap-compact-syntax text for ASSOCS (a list of
`shex-manifest-browser--assoc'), `!' negation stripped -- `emacs-rudof'
itself doesn't parse that marker, see this file's Commentary."
  (mapconcat (lambda (a) (format "%s@%s"
                                  (shex-manifest-browser--assoc-node-text a)
                                  (shex-manifest-browser--assoc-shape-text a)))
             assocs ",\n"))

;;; Running an entry

(defun shex-manifest-browser--pass-p (entry assocs quadruples)
  "Non-nil if every association in ASSOCS got the status its (possibly
`!'-negated) expectation calls for, per QUADRUPLES (`shex-validate--check''s
raw `(NODE SHAPE STATUS REASON)' list) -- a negated association expects
\"nonconformant\" regardless of ENTRY's own `status'; every other
association expects ENTRY's `status' itself.  Returns (PASS . DETAILS),
DETAILS a list of \"NODE@SHAPE: expected E, got A\" strings for any
mismatch."
  (let ((entry-status (shex-manifest-browser--get entry 'status))
        details)
    (dolist (a assocs)
      (let* ((expected (if (shex-manifest-browser--assoc-negated a) "nonconformant" entry-status))
             (quad (seq-find (lambda (q) (equal (nth 0 q) (shex-manifest-browser--strip-angle-brackets
                                                            (shex-manifest-browser--assoc-node-text a))))
                              quadruples))
             (actual (and quad (nth 2 quad))))
        (unless (equal actual expected)
          (push (format "%s@%s: expected %s, got %s"
                         (shex-manifest-browser--assoc-node-text a)
                         (shex-manifest-browser--assoc-shape-text a)
                         expected (or actual "no result"))
                details))))
    (cons (null details) (nreverse details))))

(defun shex-manifest-browser--strip-angle-brackets (text)
  (if (and (string-prefix-p "<" text) (string-suffix-p ">" text))
      (substring text 1 (1- (length text)))
    text))

(defvar-local shex-manifest-browser--manifest-file nil)
(defvar-local shex-manifest-browser--manifest-dir nil)
(defvar-local shex-manifest-browser--entries nil)
(defvar-local shex-manifest-browser--results nil
  "Hash table, entry index -> (PASS . DETAILS), filled in by
`shex-manifest-browser-run-entry'/`shex-manifest-browser-run-all'.")

(defun shex-manifest-browser--run (entry)
  "Load ENTRY's schema/data/queryMap into buffers, resolve its queryMap,
link the buffers for `shex-validate', and return (PASS . DETAILS) per
`shex-manifest-browser--pass-p'.  Also shows the schema/data buffers
and the full per-association result table
\(`shex-validate-show-result-shapemap')."
  (let* ((manifest-dir shex-manifest-browser--manifest-dir)
         (schema-buffer (shex-manifest-browser--schema-buffer entry manifest-dir))
         (data-buffer (shex-manifest-browser--data-buffer entry manifest-dir))
         (querymap-text (shex-manifest-browser--querymap-text entry manifest-dir)))
    (shex-manifest-browser--querymap-buffer entry manifest-dir)
    (let* ((assocs (shex-manifest-browser--resolve-querymap querymap-text data-buffer))
           (shapemap-buffer (shex-manifest-browser--content-buffer
                              'shapemap (shex-manifest-browser--get entry 'dataLabel)
                              (shex-manifest-browser--shapemap-text assocs) nil)))
      (with-current-buffer data-buffer
        (setq-local shex-validate--rudof nil)
        (setq-local shex-validate--schema-tick nil)
        (setq-local shex-validate--shapemap-tick nil)
        (shex-validate-link-buffers schema-buffer shapemap-buffer)
        (let ((quadruples (shex-validate--check)))
          (display-buffer schema-buffer)
          (display-buffer data-buffer)
          (shex-validate-show-result-shapemap data-buffer)
          (shex-manifest-browser--pass-p entry assocs quadruples))))))

;;; The browser buffer

(defun shex-manifest-browser--row (index entry)
  (let ((result (gethash index shex-manifest-browser--results)))
    (list index
          (vector (number-to-string (1+ index))
                  (or (shex-manifest-browser--get entry 'schemaLabel) "")
                  (or (shex-manifest-browser--get entry 'dataLabel) "")
                  (or (shex-manifest-browser--get entry 'status) "")
                  (cond
                   ((null result) "")
                   ((car result) (propertize "PASS" 'face 'success))
                   (t (propertize (concat "FAIL: " (mapconcat #'identity (cdr result) "; ")) 'face 'error)))))))

(defun shex-manifest-browser--entries-fn ()
  (cl-loop for entry in shex-manifest-browser--entries
            for index from 0
            collect (shex-manifest-browser--row index entry)))

(defun shex-manifest-browser-run-entry ()
  "Run the manifest entry at point and update its Result column."
  (interactive)
  (let ((index (tabulated-list-get-id)))
    (unless index (user-error "No manifest entry at point"))
    (let* ((entry (nth index shex-manifest-browser--entries))
           (result (shex-manifest-browser--run entry)))
      (puthash index result shex-manifest-browser--results)
      (tabulated-list-print t)
      (message "%s: %s" (if (car result) "PASS" "FAIL")
               (or (shex-manifest-browser--get entry 'dataLabel) (shex-manifest-browser--get entry 'schemaLabel))))))

(defun shex-manifest-browser-run-all ()
  "Run every entry in this manifest and update the Result column."
  (interactive)
  (cl-loop for entry in shex-manifest-browser--entries
            for index from 0
            do (puthash index (shex-manifest-browser--run entry) shex-manifest-browser--results))
  (tabulated-list-print t)
  (message "shex-manifest-browser: ran %d entries, %d passed"
           (hash-table-count shex-manifest-browser--results)
           (cl-count-if #'car (hash-table-values shex-manifest-browser--results))))

(defun shex-manifest-browser-revert (&rest _)
  "Reparse this browser's manifest file from disk, discarding results."
  (interactive)
  (setq shex-manifest-browser--entries
        (shex-manifest-browser--load-manifest
         (expand-file-name shex-manifest-browser--manifest-file shex-manifest-browser--manifest-dir)))
  (clrhash shex-manifest-browser--results)
  (tabulated-list-print t))

(defun shex-manifest-browser--initialize (path)
  "Set up the current (already `shex-manifest-browser-mode') buffer's
state from the manifest at PATH."
  (setq shex-manifest-browser--manifest-file (file-name-nondirectory path))
  (setq shex-manifest-browser--manifest-dir (file-name-directory (expand-file-name path)))
  (setq shex-manifest-browser--entries (shex-manifest-browser--load-manifest path))
  (setq shex-manifest-browser--results (make-hash-table :test 'equal)))

(defvar-keymap shex-manifest-browser-mode-map
  :parent tabulated-list-mode-map
  "RET" #'shex-manifest-browser-run-entry
  "a" #'shex-manifest-browser-run-all)

(define-derived-mode shex-manifest-browser-mode tabulated-list-mode "ShEx-Manifest"
  "Major mode listing a YAML test manifest's entries -- see
`shex-manifest-browser-open'.  `RET' runs the entry at point;
`a' (`shex-manifest-browser-run-all') runs every entry; `g'
\(`revert-buffer', bound by `tabulated-list-mode' already) reparses the
manifest file, discarding results.

Turning this mode on directly (`M-x shex-manifest-browser-mode') in a
buffer already visiting a manifest file also works -- it loads that
same file's entries and detaches the buffer from the file (so `C-x
C-s' can't clobber the manifest with this mode's own rendering; re-run
`shex-manifest-browser-open' or `find-file' the manifest again to
resume editing its YAML directly)."
  (setq tabulated-list-format
        [("#" 4 nil) ("Schema" 30 t) ("Data" 30 t) ("Expected" 14 t) ("Result" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'shex-manifest-browser--entries-fn)
  (setq revert-buffer-function #'shex-manifest-browser-revert)
  (tabulated-list-init-header)
  (when (and (null shex-manifest-browser--entries) buffer-file-name)
    (let ((path buffer-file-name))
      (setq buffer-file-name nil)
      (shex-manifest-browser--initialize path)
      (tabulated-list-print)
      (set-buffer-modified-p nil))))

;;;###autoload
(defun shex-manifest-browser-open (path)
  "Open the YAML test manifest at PATH in a `shex-manifest-browser-mode' buffer."
  (interactive "fManifest file: ")
  (let ((buffer (get-buffer-create (format "*ShEx Manifest: %s*" (file-name-nondirectory path)))))
    (with-current-buffer buffer
      (shex-manifest-browser-mode)
      (shex-manifest-browser--initialize path)
      (tabulated-list-print)
      (set-buffer-modified-p nil))
    (pop-to-buffer buffer)))

(provide 'shex-manifest-browser)

;;; shex-manifest-browser.el ends here
