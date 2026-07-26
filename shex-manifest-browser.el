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
(require 'transient)
(require 'shexc-ts-mode)
(require 'turtle-ts-mode)
(require 'yaml-mode nil t)

(declare-function yaml-mode "yaml-mode")
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

(defun shex-manifest-browser--resolve-path (manifest-path path)
  "PATH (a manifest's `schemaURL'/`dataURL'/`queryMapURL' value)
resolved relative to MANIFEST-PATH's own directory -- the RFC 3986
\"relative to the manifest's BASE\" step for by-reference content."
  (expand-file-name path (file-name-directory manifest-path)))

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

(defun shex-manifest-browser--content-buffer (kind label content mode-fn manifest-path)
  "Return a live buffer holding CONTENT, named from KIND (`schema'/
`data'/`queryMap'/`shapemap') and LABEL (a string, or nil for a bare
\"*KIND*\"), with MODE-FN (a major-mode function, or nil for
`fundamental-mode') applied.  See this file's Commentary for the
reuse/reap policy.

MANIFEST-PATH (the manifest file CONTENT was inline in) anchors the
buffer two ways.  Its directory becomes the buffer's own
`default-directory' -- a freshly `generate-new-buffer'd buffer
otherwise just inherits whatever buffer happened to be current when
it was created (an Emacs-wide convention, not this mode's own doing),
which for a reused/temp buffer with no file of its own is
unpredictable and buffer-history-dependent, not the manifest's own
location a relative path *within* CONTENT (e.g. a ShExC `IMPORT', or
-- confirmed empirically -- a relative
`shex-validate-rudof-module-path') would sensibly resolve against.
And its URI becomes the buffer's `shex-validate-base-iri' -- inline
content inherits the manifest's own BASE, per RFC 3986's
\"encapsulating entity\" rule (matching shex-webapp, which resolves
an inline snippet's relative IRIs against the manifest's URL), so
e.g. a relative `<Obs1>' in inline data is a real, resolvable node
even though the buffer visits no file; a BASE directive inside
CONTENT still overrides it."
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
      (setq default-directory (file-name-directory manifest-path))
      (setq shex-validate-base-iri (shex-validate-file-base-iri manifest-path))
      (set-buffer-modified-p nil))
    (puthash key buffer shex-manifest-browser--owned-buffers)
    buffer))

(defun shex-manifest-browser--schema-mode ()
  (if (treesit-ready-p 'shexc t) (shexc-ts-mode) (fundamental-mode)))

(defun shex-manifest-browser--data-mode ()
  (if (and (treesit-ready-p 'turtle t) (fboundp 'turtle-ts-mode))
      (turtle-ts-mode)
    (fundamental-mode)))

(defun shex-manifest-browser--schema-buffer (entry manifest-path)
  (let ((url (shex-manifest-browser--get entry 'schemaURL)))
    (if url
        (find-file-noselect (shex-manifest-browser--resolve-path manifest-path url))
      (shex-manifest-browser--content-buffer
       'schema (shex-manifest-browser--get entry 'schemaLabel)
       (or (shex-manifest-browser--get entry 'schema) "")
       #'shex-manifest-browser--schema-mode manifest-path))))

(defun shex-manifest-browser--data-buffer (entry manifest-path)
  (let ((url (shex-manifest-browser--get entry 'dataURL)))
    (if url
        (find-file-noselect (shex-manifest-browser--resolve-path manifest-path url))
      (shex-manifest-browser--content-buffer
       'data (shex-manifest-browser--get entry 'dataLabel)
       (or (shex-manifest-browser--get entry 'data) "")
       #'shex-manifest-browser--data-mode manifest-path))))

(defun shex-manifest-browser--querymap-text (entry manifest-path)
  "The entry's raw queryMap text, from `queryMap' or `queryMapURL'."
  (let ((url (shex-manifest-browser--get entry 'queryMapURL)))
    (if url
        (with-temp-buffer
          (insert-file-contents (shex-manifest-browser--resolve-path manifest-path url))
          (buffer-string))
      (or (shex-manifest-browser--get entry 'queryMap) ""))))

(defun shex-manifest-browser--querymap-buffer (entry manifest-path)
  "A buffer holding ENTRY's raw (unresolved) queryMap text, for the
operator to inspect -- not what's actually linked for validation, see
`shex-manifest-browser--resolve-querymap' for that."
  (shex-manifest-browser--content-buffer
   'queryMap (shex-manifest-browser--get entry 'dataLabel)
   (shex-manifest-browser--querymap-text entry manifest-path)
   nil manifest-path))

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
way.  Seeded with DATA-BUFFER's own base
\(`shex-validate--buffer-base-iri' -- its manifest's URI for inline
content, its file otherwise), matching the seed
`shex-manifest-browser--resolve-pattern' gives its store parse of the
same buffer: a relative fixed token and a relative subject/object in
the data must resolve to the *same* absolute IRI or they never match."
  (with-current-buffer data-buffer
    (rdf-turtle-buffer-directive-ctx
     (shex-validate--buffer-base-iri data-buffer))))

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
               (store (with-current-buffer data-buffer
                        (rdf-turtle-parse-buffer
                         nil nil (shex-validate--buffer-base-iri data-buffer))))
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

(defun shex-manifest-browser--resolve-shape-text (schema-buffer shape-text)
  "SHAPE-TEXT (an association's own raw shape reference -- `<iri>' or
`START'/`Start', the only forms this package's own manifests ever put
in queryMap's shape position) resolved to the same fully-resolved
absolute-IRI form (or the literal string \"Start\") a
`shex-validate--check' quadruple reports its own SHAPE element in, per
SCHEMA-BUFFER's BASE -- so `shex-manifest-browser--pass-p' can match a
quadruple by (NODE . SHAPE), not NODE alone: a node tested against
several shapes (e.g. one entry's schema chaining through EXTENDS,
where `rudof-validate-shex' reports one quadruple per shape actually
evaluated) would otherwise risk a *different* shape's status getting
attributed to the association actually being checked.  Returns
SHAPE-TEXT itself, unresolved, for any other form (e.g.
`prefix:local' -- unused by this package's own manifests today, but
nothing stops a future one) or when SCHEMA-BUFFER isn't live --
callers should treat a match against the result as a preference, not
a guarantee, since there's no live `rudof' here to confirm this
resolves exactly the way it does (see
`shex-manifest-browser--pass-p')."
  (cond
   ((member shape-text '("START" "Start")) "Start")
   ((and schema-buffer (buffer-live-p schema-buffer)
         (string-prefix-p "<" shape-text) (string-suffix-p ">" shape-text))
    (with-current-buffer schema-buffer
      (shexc-shexj--resolve-iri
       (shexc-shexj-buffer-directive-ctx
        (shex-validate--buffer-base-iri schema-buffer))
       (substring shape-text 1 (1- (length shape-text))))))
   (t shape-text)))

(defun shex-manifest-browser--resolve-node-text (data-buffer node-text)
  "NODE-TEXT (an association's own raw node reference, already `<>'-
stripped by `shex-manifest-browser--strip-angle-brackets') resolved to
the same fully-resolved absolute-IRI form a `shex-validate--check'
quadruple reports its own NODE element in, per DATA-BUFFER's BASE --
`shex-validate--data-shapemap-base-iri', the very helper `shex-
validate--check-1' itself uses to resolve a ShapeMap's node references
before handing them to `read-shapemap' (confirmed empirically: a
`<#Alice>' node association's quadruple came back with NODE
`http://a.example/#Alice', not `#Alice' -- unlike SHAPE resolution,
this one isn't a guess).  Mirrors `shex-manifest-browser--resolve-
shape-text' for the NODE side of a `shex-manifest-browser--pass-p'
quadruple match.  NODE-TEXT unchanged for a `_:label' blank node
\(never relative to any BASE) or when DATA-BUFFER isn't live."
  (if-let* (((and data-buffer (buffer-live-p data-buffer)
                  (not (string-prefix-p "_:" node-text))))
            ;; Can be nil (a buffer with no file, no manifest to
            ;; inherit from, and no BASE directive) -- and
            ;; `url-expand-file-name' with a nil base *errors* on a
            ;; relative reference rather than passing it through, so
            ;; fall back to NODE-TEXT unresolved (`shex-manifest-
            ;; browser--pass-p''s tier-3 raw-text match) like every
            ;; other resolver here does, instead of taking the whole
            ;; check down.
            (base (shex-validate--data-shapemap-base-iri data-buffer)))
      (url-expand-file-name node-text base)
    node-text))

(defun shex-manifest-browser--pass-p (entry assocs quadruples schema-buffer data-buffer)
  "Non-nil if every association in ASSOCS got the status its (possibly
`!'-negated) expectation calls for, per QUADRUPLES (`shex-validate--check''s
raw `(NODE SHAPE STATUS REASON)' list) -- a negated association expects
\"nonconformant\" regardless of ENTRY's own `status'; every other
association expects ENTRY's `status' itself.  Returns (PASS . DETAILS),
DETAILS a list of \"NODE@SHAPE: expected E, got A\" strings for any
mismatch.

Matches each association's quadruple in three tiers, from most to
least precise, using whichever finds something first: (1) the fully-
resolved (NODE . SHAPE) pair (`shex-manifest-browser--resolve-node-
text'/`--resolve-shape-text', via DATA-BUFFER/SCHEMA-BUFFER's own
BASE, matching how `shex-validate--check-1' itself resolves a
ShapeMap before validating) -- needed when a node's tested against
several shapes in the same run (e.g. one entry's schema chaining
through EXTENDS, where `rudof-validate-shex' reports one quadruple
per shape actually evaluated) and NODE alone can't disambiguate; (2)
resolved NODE alone, if SHAPE resolution didn't land an exact match;
(3) the raw, `<>'-stripped-only NODE text, unresolved -- this
function's entire matching strategy before either resolution existed,
kept as the last resort since neither resolution step has a live
`rudof' to confirm its output format against (SHAPE resolution is
this package's own best-effort reading of the rules `shex-validate--
schema-shapemap-base-iri' documents; NODE resolution reuses `shex-
validate--data-shapemap-base-iri' directly, so it's on firmer ground,
but still not counted on to the exclusion of the old fallback)."
  (let ((entry-status (shex-manifest-browser--get entry 'status))
        details)
    (dolist (a assocs)
      (let* ((expected (if (shex-manifest-browser--assoc-negated a) "nonconformant" entry-status))
             (node (shex-manifest-browser--strip-angle-brackets (shex-manifest-browser--assoc-node-text a)))
             (resolved-node (shex-manifest-browser--resolve-node-text data-buffer node))
             (shape-text (shex-manifest-browser--assoc-shape-text a))
             (resolved-shape (shex-manifest-browser--resolve-shape-text schema-buffer shape-text))
             (quad (or (seq-find (lambda (q) (and (equal (nth 0 q) resolved-node) (equal (nth 1 q) resolved-shape)))
                                  quadruples)
                       (seq-find (lambda (q) (equal (nth 0 q) resolved-node)) quadruples)
                       (seq-find (lambda (q) (equal (nth 0 q) node)) quadruples)))
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

;; `permanent-local': survive `shex-manifest-browser-toggle-display'
;; switching the buffer's major mode back and forth (any major-mode
;; function calls `kill-all-local-variables', which would otherwise
;; wipe these) -- see `shex-manifest-browser--show-yaml'/`--show-table'.
(defvar-local shex-manifest-browser--manifest-file nil)
(put 'shex-manifest-browser--manifest-file 'permanent-local t)
(defvar-local shex-manifest-browser--manifest-dir nil)
(put 'shex-manifest-browser--manifest-dir 'permanent-local t)
(defvar-local shex-manifest-browser--entries nil)
(put 'shex-manifest-browser--entries 'permanent-local t)
(defvar-local shex-manifest-browser--results nil
  "Hash table, entry index -> (PASS . DETAILS), filled in by
`shex-manifest-browser-run-entry'/`shex-manifest-browser-run-all'.")
(put 'shex-manifest-browser--results 'permanent-local t)
(defvar-local shex-manifest-browser--saved-undo-list nil
  "`buffer-undo-list' captured from the YAML view just before
`shex-manifest-browser--show-table' switches away from it (any
major-mode change, via `kill-all-local-variables', would otherwise
silently reset it to empty) -- restored by
`shex-manifest-browser--show-yaml' when toggling back, so an
edit/toggle-to-table-to-run/toggle-back-to-YAML cycle doesn't erase
editing history from before the trip.  Only captured when the buffer
text about to be reloaded on the way back is known to match what's
on disk right now (saved, or already unmodified) -- see
`shex-manifest-browser--show-table' -- since a discarded edit means
the text `--show-yaml' re-reads won't match what this undo list was
recorded against.")
(put 'shex-manifest-browser--saved-undo-list 'permanent-local t)

;;; Side-by-side display, node/shape highlighting

(defvar shex-manifest-browser--highlight-overlays nil
  "Overlays `shex-manifest-browser--highlight-quadruples' put in a
schema/data buffer pair for the entry last run -- cleared and
recreated on every run, so only one entry's highlights are ever shown
at a time.")

(defun shex-manifest-browser--clear-highlights ()
  (mapc #'delete-overlay shex-manifest-browser--highlight-overlays)
  (setq shex-manifest-browser--highlight-overlays nil))

(defun shex-manifest-browser--schema-shape-iri (schema-buffer shape)
  "SHAPE (a `shex-validate--check' quadruple's own SHAPE element) as an
IRI `shex-validate--find-shape-decl' can look up: SCHEMA-BUFFER's own
`start = @<label>' target when SHAPE is literally \"Start\"
\(`emacs-rudof''s own spelling for an association resolved via
`@START', not itself a resolvable IRI -- see this package's own
`shex-manifest-browser--resolve-querymap' Commentary on why `START'
never needs resolving before reaching `read-shapemap' itself), SHAPE
unchanged otherwise (including when SCHEMA-BUFFER's `start' isn't a
plain shape reference, in which case there's nothing simple to
highlight and SHAPE is returned as-is, harmlessly not found by
`shex-validate--find-shape-decl')."
  (or (and (equal shape "Start")
           (with-current-buffer schema-buffer
             (let ((ctx (shexc-shexj--make-ctx)) start-node)
               (dolist (c (treesit-node-children (treesit-buffer-root-node 'shexc) t))
                 (pcase (treesit-node-type c)
                   ("base_decl" (shexc-shexj--apply-base ctx c))
                   ("prefix_decl" (shexc-shexj--apply-prefix ctx c))
                   ("start" (setq start-node c))))
               (when start-node
                 (let ((compiled (shexc-shexj--compile-shape-expr
                                   ctx (treesit-node-child-by-field-name start-node "shape_expr"))))
                   (and (stringp compiled) compiled))))))
      shape))

(defface shex-manifest-browser-nonconformant-highlight
  '((t :inherit error))
  "Face `shex-manifest-browser--highlight-quadruples' uses for a node/
shape whose association is currently nonconformant/inconsistent --
distinct from `shex-validate-highlight' (a neutral, `highlight'-based
face, used for conformant/pending ones), so a currently-failing node
or shape stands out from a merely last-checked one."
  :group 'shex-manifest-browser)

(defun shex-manifest-browser--highlight-quadruples (quadruples schema-buffer data-buffer positions)
  "Highlight every node in QUADRUPLES (`shex-validate--check''s raw
`(NODE SHAPE STATUS REASON)' list) at its position(s) in DATA-BUFFER
\(per POSITIONS, see `rdf-turtle-parse-buffer') and its SHAPE's
declaration in SCHEMA-BUFFER, in `shex-manifest-browser-nonconformant-
highlight' for a currently-failing association or plain
`shex-validate-highlight' otherwise.  A `{FOCUS ...}' pattern
association that matched several nodes already produced one quadruple
per match (see `shex-manifest-browser--resolve-pattern'), so
highlighting every quadruple highlights all of them, not just one."
  (dolist (q quadruples)
    (pcase-let ((`(,node ,shape ,status ,_reason) q))
      (let ((face (if (member status '("conformant" "pending"))
                       'shex-validate-highlight
                     'shex-manifest-browser-nonconformant-highlight)))
        (dolist (span (gethash node positions))
          (let ((ov (make-overlay (car span) (cdr span) data-buffer)))
            (overlay-put ov 'face face)
            (push ov shex-manifest-browser--highlight-overlays)))
        (when-let* ((label-node (shex-validate--find-shape-decl
                                  schema-buffer (shex-manifest-browser--schema-shape-iri schema-buffer shape))))
          (let ((ov (make-overlay (treesit-node-start label-node) (treesit-node-end label-node) schema-buffer)))
            (overlay-put ov 'face face)
            (push ov shex-manifest-browser--highlight-overlays)))))))

(defcustom shex-manifest-browser-window-height 10
  "Number of lines BROWSER-WINDOW (the manifest table itself) keeps
across the top when `shex-manifest-browser--show-side-by-side' arranges
the schema/data windows below it."
  :type 'natnum
  :group 'shex-manifest-browser)

(defun shex-manifest-browser--show-side-by-side (browser-window schema-buffer data-buffer)
  "Arrange BROWSER-WINDOW (this manifest buffer, kept on top, a fixed
`shex-manifest-browser-window-height' lines tall) with SCHEMA-BUFFER
and DATA-BUFFER in a row below it, split left/right -- replacing every
window other than BROWSER-WINDOW (so re-running an entry doesn't pile
up extra windows from a previous run).
`shex-validate-show-result-shapemap' (called separately, right after
this) then places its own results table in a bottom *side* window,
which spans beneath all of this rather than competing with it for
space in the same window tree."
  (dolist (w (window-list))
    (unless (eq w browser-window) (ignore-errors (delete-window w))))
  (let* ((middle-window (split-window browser-window shex-manifest-browser-window-height 'below))
         (data-window (split-window middle-window nil 'right)))
    (set-window-buffer middle-window schema-buffer)
    (set-window-buffer data-window data-buffer)))

;;; Live refresh: keep the table row, highlights, and results buffer
;;; current after a schema/data/queryMap edit, not just flymake
;;
;; `shex-validate-after-check-functions' (in shex-validate.el) already
;; fires on every `shex-validate--check', including ones
;; `shex-validate--register-dependent' triggers automatically from a
;; schema/ShapeMap edit -- `shex-manifest-browser--on-recheck' below is
;; what this package adds to that hook, so those same automatic
;; rechecks also keep everything else current, not just the data
;; buffer's own flymake diagnostics.

(defvar-local shex-manifest-browser--owner-browser-buffer nil
  "The `shex-manifest-browser-mode' buffer whose table has this data
buffer's current entry -- set by `shex-manifest-browser--run', read by
`shex-manifest-browser--on-recheck'.")

(defvar-local shex-manifest-browser--owner-index nil
  "Like `shex-manifest-browser--owner-browser-buffer', the entry's own
index into that buffer's `shex-manifest-browser--entries'.")

(defvar-local shex-manifest-browser--owner-querymap-buffer nil
  "The buffer holding this data buffer's current entry's raw queryMap
text (see `shex-manifest-browser--querymap-buffer') -- read fresh on
every recheck (rather than re-reading the entry's own original YAML
text) so an edit to the queryMap buffer itself is picked up too.")

(defvar-local shex-manifest-browser--schema-quadruples nil
  "The most recent `shex-validate--check' quadruples result whose SHAPE
elements are looked up in this (schema) buffer -- refreshed by
`shex-manifest-browser--on-recheck'/`--on-recheck-error', read by
`shex-manifest-browser--schema-flymake' to report a matching
diagnostic on each currently-nonconformant/inconsistent shape's own
declaration, mirroring what `shex-validate-flymake' already reports on
the data buffer's own failing node.")

(defun shex-manifest-browser--schema-flymake (report-fn &rest _args)
  "A `flymake-diagnostic-functions' backend for a schema buffer
`shex-manifest-browser--run' set up: reports one `:error' diagnostic
per currently-nonconformant/inconsistent quadruple in
`shex-manifest-browser--schema-quadruples', at that quadruple's own
SHAPE's declaration.  Distinct from `shexc-ts-mode-flymake' (which only
checks the schema's own internal consistency -- undefined prefixes/
shapes, duplicate predicates -- nothing about whether currently-linked
data actually conforms) and only reports something once at least one
entry has been run (`shex-manifest-browser--run' is what registers
this backend on a schema buffer in the first place)."
  (let (diagnostics)
    (pcase-dolist (`(,node ,shape ,status ,reason) shex-manifest-browser--schema-quadruples)
      (unless (member status '("conformant" "pending"))
        (when-let* ((label-node (shex-validate--find-shape-decl
                                  (current-buffer) (shex-manifest-browser--schema-shape-iri (current-buffer) shape))))
          (push (flymake-make-diagnostic
                 (current-buffer) (treesit-node-start label-node) (treesit-node-end label-node) :error
                 (format "ShEx %s for node %s: %s" status node reason))
                diagnostics))))
    (funcall report-fn diagnostics)))

(defun shex-manifest-browser--on-recheck (quadruples)
  "Added (buffer-local) to `shex-validate-after-check-functions' in
every data buffer `shex-manifest-browser--run' sets up: refreshes that
entry's row in `shex-manifest-browser--owner-browser-buffer' (the
table's Result column), re-highlights nodes/shapes in the schema/data
buffers (`shex-manifest-browser--highlight-quadruples', distinguishing
currently-nonconformant ones from conformant), and refreshes any
already-open `*ShEx Result ShapeMap: ...*' buffer for this data
buffer -- all from QUADRUPLES directly, never by calling
`shex-validate--check' again (which would re-trigger this same hook).

Also refreshes `shex-manifest-browser--schema-quadruples' in
SCHEMA-BUFFER and re-triggers its own flymake (`shex-manifest-browser--
schema-flymake'), so a nonconformant/inconsistent shape is flagged
there too -- even though nothing changed in SCHEMA-BUFFER itself
\(flymake's own automatic re-check triggers only on an edit *in* the
buffer being checked, which wouldn't fire from a DATA-only edit)."
  (let* ((data-buffer (current-buffer))
         (schema-buffer shex-validate-schema-buffer)
         (browser-buffer shex-manifest-browser--owner-browser-buffer)
         (index shex-manifest-browser--owner-index)
         (querymap-buffer shex-manifest-browser--owner-querymap-buffer)
         (positions (make-hash-table :test 'equal)))
    (rdf-turtle-parse-buffer nil positions)
    (shex-manifest-browser--clear-highlights)
    (when (and schema-buffer (buffer-live-p schema-buffer))
      (shex-manifest-browser--highlight-quadruples quadruples schema-buffer data-buffer positions)
      (with-current-buffer schema-buffer
        (setq-local shex-manifest-browser--schema-quadruples quadruples)
        (flymake-start)))
    (when (and browser-buffer (buffer-live-p browser-buffer) index)
      (with-current-buffer browser-buffer
        (when (< index (length shex-manifest-browser--entries))
          (let* ((entry (nth index shex-manifest-browser--entries))
                 (querymap-text (if (buffer-live-p querymap-buffer)
                                     (with-current-buffer querymap-buffer (buffer-string))
                                   (shex-manifest-browser--querymap-text
                                    entry
                                    (expand-file-name
                                     shex-manifest-browser--manifest-file
                                     shex-manifest-browser--manifest-dir))))
                 (assocs (shex-manifest-browser--resolve-querymap querymap-text data-buffer)))
            (puthash index (shex-manifest-browser--pass-p entry assocs quadruples schema-buffer data-buffer)
                     shex-manifest-browser--results)
            (tabulated-list-print t)))))
    (when-let* ((results-buffer (get-buffer (format "*ShEx Result ShapeMap: %s*" (buffer-name data-buffer)))))
      (let ((shex-validate-results--precomputed (cons quadruples positions)))
        (with-current-buffer results-buffer (tabulated-list-print t))))))

(defun shex-manifest-browser--on-recheck-error (err)
  "Added (buffer-local) to `shex-validate-check-error-functions' in
every data buffer `shex-manifest-browser--run' sets up: marks that
entry's row in `shex-manifest-browser--owner-browser-buffer' as
failing, with ERR's own message as the detail, and clears any existing
node/shape highlights -- a transient schema/data/ShapeMap parse
failure (e.g. mid-edit in the schema buffer, an undefined prefix)
means there are no fresh quadruples to highlight or to compute a real
pass/fail from.  Counterpart to `shex-manifest-browser--on-recheck' for
exactly the case that can't call it -- see
`shex-validate-check-error-functions'; the results buffer needs no
equivalent handling here since `shex-validate-results--entries' already
degrades to a one-row error display on its own.

Also clears `shex-manifest-browser--schema-quadruples' in the linked
schema buffer and re-triggers its flymake, so a stale nonconformant-
shape diagnostic from a previous successful check doesn't linger once
the schema/data/ShapeMap no longer even parses."
  (shex-manifest-browser--clear-highlights)
  (when-let* ((schema-buffer shex-validate-schema-buffer))
    (when (buffer-live-p schema-buffer)
      (with-current-buffer schema-buffer
        (setq-local shex-manifest-browser--schema-quadruples nil)
        (flymake-start))))
  (let ((browser-buffer shex-manifest-browser--owner-browser-buffer)
        (index shex-manifest-browser--owner-index))
    (when (and browser-buffer (buffer-live-p browser-buffer) index)
      (with-current-buffer browser-buffer
        (when (< index (length shex-manifest-browser--entries))
          (puthash index (cons nil (list (error-message-string err)))
                   shex-manifest-browser--results)
          (tabulated-list-print t))))))

(defun shex-manifest-browser--run (entry index)
  "Load ENTRY's (at INDEX in `shex-manifest-browser--entries') schema/
data/queryMap into buffers, resolve its queryMap, link the buffers for
`shex-validate', and return (PASS . DETAILS) per
`shex-manifest-browser--pass-p'.  Also shows the schema/data buffers
side by side with every resulting node/shape association highlighted
in each (see `shex-manifest-browser--highlight-quadruples'), and the
full per-association result table (`shex-validate-show-result-shapemap').

Registers `shex-manifest-browser--on-recheck' on the data buffer, so
editing the schema/data/queryMap afterward keeps this entry's table
row, highlights, and results buffer current too -- not just its
flymake diagnostics (see `shex-validate--register-dependent').

Also, for whichever of schema/data/queryMap ENTRY declares inline
\(rather than by `schemaURL'/`dataURL'/`queryMapURL' -- already a real
file-visiting buffer, already saveable the ordinary way\), makes that
buffer save back into the manifest file itself (see
`shex-manifest-browser--enable-save-back')."
  (let ((browser-buffer (current-buffer))
        (browser-window (selected-window))
        (manifest-path (expand-file-name shex-manifest-browser--manifest-file
                                          shex-manifest-browser--manifest-dir)))
    (let* ((schema-buffer (shex-manifest-browser--schema-buffer entry manifest-path))
           (data-buffer (shex-manifest-browser--data-buffer entry manifest-path))
           (querymap-buffer (shex-manifest-browser--querymap-buffer entry manifest-path))
           (querymap-text (with-current-buffer querymap-buffer (buffer-string))))
      (unless (shex-manifest-browser--get entry 'schemaURL)
        (shex-manifest-browser--enable-save-back schema-buffer manifest-path index 'schema))
      (unless (shex-manifest-browser--get entry 'dataURL)
        (shex-manifest-browser--enable-save-back data-buffer manifest-path index 'data))
      (unless (shex-manifest-browser--get entry 'queryMapURL)
        (shex-manifest-browser--enable-save-back querymap-buffer manifest-path index 'queryMap))
      (let* ((assocs (shex-manifest-browser--resolve-querymap querymap-text data-buffer))
             (shapemap-buffer (shex-manifest-browser--content-buffer
                               'shapemap (shex-manifest-browser--get entry 'dataLabel)
                               (shex-manifest-browser--shapemap-text assocs) nil manifest-path)))
        (with-current-buffer data-buffer
          (setq-local shex-validate--rudof nil)
          (setq-local shex-validate--schema-tick nil)
          (setq-local shex-validate--shapemap-tick nil)
          (setq-local shex-manifest-browser--owner-browser-buffer browser-buffer)
          (setq-local shex-manifest-browser--owner-index index)
          (setq-local shex-manifest-browser--owner-querymap-buffer querymap-buffer)
          (add-hook 'shex-validate-after-check-functions #'shex-manifest-browser--on-recheck nil t)
          (add-hook 'shex-validate-check-error-functions #'shex-manifest-browser--on-recheck-error nil t)
          (with-current-buffer schema-buffer
            (add-hook 'flymake-diagnostic-functions #'shex-manifest-browser--schema-flymake nil t))
          (shex-validate-link-buffers schema-buffer shapemap-buffer)
          (shex-validate--check)
          (shex-manifest-browser--show-side-by-side browser-window schema-buffer data-buffer)
          (let ((shex-validate--suppress-after-check-hook t))
            (shex-validate-show-result-shapemap data-buffer)))))
    (gethash index shex-manifest-browser--results)))

;;; The browser buffer
;;
;; Which columns are shown, and in what order -- customizable via
;; `shex-manifest-browser-columns'/`-hidden-columns' directly, or
;; (preferably) the live `shex-manifest-browser-columns-menu' ("m" in
;; the table view).

(defconst shex-manifest-browser--column-titles
  '((index . "#") (data . "Data") (schema . "Schema") (queryMap . "QueryMap")
    (expected . "Expected") (result . "Result"))
  "Display title for each key `shex-manifest-browser-columns' and
`shex-manifest-browser-hidden-columns' can name.")

(defcustom shex-manifest-browser-columns
  '(index data schema queryMap expected result)
  "Which columns `shex-manifest-browser-mode' shows, and their
relative left-to-right order -- a permutation of `index' (\"#\"),
`data', `schema', `queryMap', `expected', and `result'.  Visibility is
a separate concern, see `shex-manifest-browser-hidden-columns': hiding
a column doesn't remove it from here, so it reappears in its old spot
if shown again later.  Normally set via the live menu,
`shex-manifest-browser-columns-menu' (\"m\" in the table view) --
opinions differ on the \"right\" order (e.g. `Data' vs. `Schema'
first), so this is left to taste rather than fixed."
  :type '(repeat (choice (const :tag "#" index) (const :tag "Data" data)
                          (const :tag "Schema" schema) (const :tag "QueryMap" queryMap)
                          (const :tag "Expected" expected) (const :tag "Result" result)))
  :group 'shex-manifest-browser)

(defcustom shex-manifest-browser-hidden-columns '(queryMap)
  "Subset of `shex-manifest-browser-columns' currently hidden from the
manifest table.  `queryMap' starts out hidden -- its own resolved
ShapeMap text (as opposed to `Data'/`Schema', which just show
`dataLabel'/`schemaLabel') isn't usually needed for a glance at the
table, but it's one `shex-manifest-browser-columns-menu' toggle away.
See that menu."
  :type '(repeat (choice (const index) (const data) (const schema)
                          (const queryMap) (const expected) (const result)))
  :group 'shex-manifest-browser)

(defun shex-manifest-browser--column-title (column)
  (alist-get column shex-manifest-browser--column-titles))

(defun shex-manifest-browser--visible-columns ()
  "`shex-manifest-browser-columns' minus `shex-manifest-browser-
hidden-columns', in order -- never empty: falls back to the full
column list if that would hide every column (shouldn't normally
happen, since `shex-manifest-browser--toggle-column' itself refuses
to hide the last visible one, but this guards against
`shex-manifest-browser-hidden-columns' being `setq''d or customized
directly to something that would)."
  (or (seq-difference shex-manifest-browser-columns shex-manifest-browser-hidden-columns)
      shex-manifest-browser-columns))

(defun shex-manifest-browser--querymap-column-value (entry)
  "Single-line display value for ENTRY's QueryMap column cell: its own
`queryMapURL' path if referenced that way (not resolved/read --
unlike `dataLabel'/`schemaLabel', `queryMap' has no separate label
field of its own, and resolving a `{FOCUS ...}'/`{... FOCUS}' pattern
or reading a referenced file on every table redraw is too expensive
for a column value), otherwise its inline `queryMap' text with any
embedded newlines (a multi-association queryMap has one per line,
same as the trailing newline ShapeMap-compact-syntax puts after every
single association) collapsed to `\"; \"' so it can't break the
table's one-line-per-row layout.  Trimmed first, so the common single-
association case's own trailing newline doesn't turn into a dangling
`\"; \"' once collapsed."
  (or (shex-manifest-browser--get entry 'queryMapURL)
      (replace-regexp-in-string
       "\n+" "; " (string-trim (or (shex-manifest-browser--get entry 'queryMap) "")))))

(defun shex-manifest-browser--row (index entry)
  (let* ((result (gethash index shex-manifest-browser--results))
         (values
          `((index . ,(number-to-string (1+ index)))
            (data . ,(or (shex-manifest-browser--get entry 'dataLabel) ""))
            (schema . ,(or (shex-manifest-browser--get entry 'schemaLabel) ""))
            (queryMap . ,(shex-manifest-browser--querymap-column-value entry))
            (expected . ,(or (shex-manifest-browser--get entry 'status) ""))
            (result . ,(cond
                        ((null result) "")
                        ((car result) (propertize "PASS" 'face 'success))
                        (t (propertize (concat "FAIL: " (mapconcat #'identity (cdr result) "; ")) 'face 'error)))))))
    (list index
          (vconcat (mapcar (lambda (c) (alist-get c values))
                            (shex-manifest-browser--visible-columns))))))

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
           ;; `shex-manifest-browser--run' itself already stores the
           ;; result and redraws the table row, via the same
           ;; `shex-manifest-browser--on-recheck' callback that keeps
           ;; it current on later edits too.
           (result (shex-manifest-browser--run entry index)))
      (message "%s: %s" (if (car result) "PASS" "FAIL")
               (or (shex-manifest-browser--get entry 'dataLabel) (shex-manifest-browser--get entry 'schemaLabel))))))

(defun shex-manifest-browser-run-all ()
  "Run every entry in this manifest and update the Result column."
  (interactive)
  (cl-loop for entry in shex-manifest-browser--entries
            for index from 0
            do (shex-manifest-browser--run entry index))
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

(defun shex-manifest-browser--set-column-widths ()
  "Recompute `tabulated-list-format' from `shex-manifest-browser-
columns'/`-hidden-columns' and the selected window's current width,
then redraw: `index'/`expected' stay fixed (short, fixed vocabulary),
every other non-last column splits whatever's left evenly, and
whichever column ends up last (usually `result', but any column can
land there via `shex-manifest-browser-columns-menu') takes the rest
of the line regardless of its own declared width --
`tabulated-list-print-col' never truncates the last column, which is
also why this function doesn't need to hardcode which column is \"the
wide one.\"  Widening or narrowing a window showing this buffer keeps
the flexible columns readable instead of truncating at a width fixed
when the mode turned on.  Buffer-local on `window-configuration-
change-hook', so it re-runs on every resize."
  (when (derived-mode-p 'shex-manifest-browser-mode)
    (let* ((total (max 60 (window-width)))
           (cols (shex-manifest-browser--visible-columns))
           (last-col (car (last cols)))
           (fixed-width (lambda (c) (pcase c ('index 4) ('expected 14))))
           (fixed-total (apply #'+ (delq nil (mapcar (lambda (c)
                                                         (unless (eq c last-col)
                                                           (funcall fixed-width c)))
                                                       cols))))
           (flexible (seq-filter (lambda (c) (and (not (eq c last-col))
                                                    (not (funcall fixed-width c))))
                                  cols))
           (flex-total (max 30 (- total fixed-total (* 2 (length cols))))) ; gaps
           (flex-each (if flexible (/ flex-total (length flexible)) 0)))
      (setq tabulated-list-format
            (vconcat
             (mapcar (lambda (c)
                       (list (shex-manifest-browser--column-title c)
                             (cond ((eq c last-col) 0)
                                   ((funcall fixed-width c))
                                   (t flex-each))
                             (not (eq c 'index))))
                     cols)))
      (tabulated-list-init-header)
      (tabulated-list-print t))))

;;; Columns menu: which columns are shown, and in what order

(defun shex-manifest-browser--toggle-column (column)
  "Show/hide COLUMN in the manifest table, refreshing the display.
Refuses (with a message, not an error, so it works as a `:transient
t' `shex-manifest-browser-columns-menu' suffix without kicking the
user out of the menu) to hide the last visible column -- an empty
table isn't useful."
  (if (memq column shex-manifest-browser-hidden-columns)
      (setq shex-manifest-browser-hidden-columns
            (delq column shex-manifest-browser-hidden-columns))
    (if (= 1 (length (shex-manifest-browser--visible-columns)))
        (message "Can't hide the last visible column")
      (push column shex-manifest-browser-hidden-columns)))
  (shex-manifest-browser--set-column-widths))

(defun shex-manifest-browser-toggle-column-index ()
  "Show/hide the `#' column -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--toggle-column 'index))
(defun shex-manifest-browser-toggle-column-data ()
  "Show/hide the Data column -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--toggle-column 'data))
(defun shex-manifest-browser-toggle-column-schema ()
  "Show/hide the Schema column -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--toggle-column 'schema))
(defun shex-manifest-browser-toggle-column-querymap ()
  "Show/hide the QueryMap column -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--toggle-column 'queryMap))
(defun shex-manifest-browser-toggle-column-expected ()
  "Show/hide the Expected column -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--toggle-column 'expected))
(defun shex-manifest-browser-toggle-column-result ()
  "Show/hide the Result column -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--toggle-column 'result))

(defun shex-manifest-browser--swap-adjacent-columns (slot)
  "Swap the SLOT-th and (1+ SLOT)-th currently *visible* columns
\(0-indexed), by exchanging their positions within the master
`shex-manifest-browser-columns' order -- any hidden columns
interleaved between them there stay put, so toggling one back on
later still lands where it last was.  Any full reordering is reachable
this way, one adjacent swap at a time (the standard bubble-sort
argument), so `shex-manifest-browser-columns-menu' only needs one
swap suffix per adjacent pair rather than a control for every
possible permutation.  Silently does nothing if SLOT is out of range
for how many columns are currently visible (e.g. the 4th swap slot,
between visible columns 4 and 5, when a column is hidden and only 4
remain)."
  (let* ((visible (shex-manifest-browser--visible-columns))
         (a (nth slot visible))
         (b (nth (1+ slot) visible)))
    (when (and a b)
      (let ((cols (vconcat shex-manifest-browser-columns))
            (pos-a (seq-position shex-manifest-browser-columns a))
            (pos-b (seq-position shex-manifest-browser-columns b)))
        (cl-rotatef (aref cols pos-a) (aref cols pos-b))
        (setq shex-manifest-browser-columns (append cols nil)))))
  (shex-manifest-browser--set-column-widths))

(defun shex-manifest-browser-swap-columns-1-2 ()
  "Swap the 1st/2nd visible columns -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--swap-adjacent-columns 0))
(defun shex-manifest-browser-swap-columns-2-3 ()
  "Swap the 2nd/3rd visible columns -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--swap-adjacent-columns 1))
(defun shex-manifest-browser-swap-columns-3-4 ()
  "Swap the 3rd/4th visible columns -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--swap-adjacent-columns 2))
(defun shex-manifest-browser-swap-columns-4-5 ()
  "Swap the 4th/5th visible columns -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--swap-adjacent-columns 3))
(defun shex-manifest-browser-swap-columns-5-6 ()
  "Swap the 5th/6th visible columns -- see `shex-manifest-browser-columns-menu'."
  (interactive)
  (shex-manifest-browser--swap-adjacent-columns 4))

(defun shex-manifest-browser--swap-description (slot)
  "Label for the SLOT-th `shex-manifest-browser-columns-menu' swap
suffix: the two currently-adjacent visible column titles it would
exchange, or a note that this slot doesn't currently exist (fewer
than SLOT+2 columns visible)."
  (let* ((visible (shex-manifest-browser--visible-columns))
         (a (nth slot visible))
         (b (nth (1+ slot) visible)))
    (if (and a b)
        (format "%s <-> %s" (shex-manifest-browser--column-title a)
                (shex-manifest-browser--column-title b))
      "(n/a)")))

(transient-define-prefix shex-manifest-browser-columns-menu ()
  "Persistent submenu for customizing `shex-manifest-browser-mode's
table columns -- which are shown (`shex-manifest-browser-hidden-
columns') and their left-to-right order (`shex-manifest-browser-
columns').  Every suffix is `:transient t' (same convention as
`shexc-ts-mode-highlight-menu'): flip several settings in a row and
watch the table redraw live after each one, rather than reopening
this menu between selections; \"q\" exits."
  [:description
   (lambda ()
     (format "Columns, left to right: %s"
             (mapconcat #'shex-manifest-browser--column-title
                         (shex-manifest-browser--visible-columns) " | ")))
   ["Show/hide"
    ("i" shex-manifest-browser-toggle-column-index
     :transient t
     :description (lambda () (format "[%s] #"
                                      (if (memq 'index shex-manifest-browser-hidden-columns) " " "X"))))
    ("d" shex-manifest-browser-toggle-column-data
     :transient t
     :description (lambda () (format "[%s] Data"
                                      (if (memq 'data shex-manifest-browser-hidden-columns) " " "X"))))
    ("s" shex-manifest-browser-toggle-column-schema
     :transient t
     :description (lambda () (format "[%s] Schema"
                                      (if (memq 'schema shex-manifest-browser-hidden-columns) " " "X"))))
    ("Q" shex-manifest-browser-toggle-column-querymap
     :transient t
     :description (lambda () (format "[%s] QueryMap"
                                      (if (memq 'queryMap shex-manifest-browser-hidden-columns) " " "X"))))
    ("e" shex-manifest-browser-toggle-column-expected
     :transient t
     :description (lambda () (format "[%s] Expected"
                                      (if (memq 'expected shex-manifest-browser-hidden-columns) " " "X"))))
    ("r" shex-manifest-browser-toggle-column-result
     :transient t
     :description (lambda () (format "[%s] Result"
                                      (if (memq 'result shex-manifest-browser-hidden-columns) " " "X"))))]
   ["Reorder (swap adjacent visible columns)"
    ("1" shex-manifest-browser-swap-columns-1-2
     :transient t :description (lambda () (shex-manifest-browser--swap-description 0)))
    ("2" shex-manifest-browser-swap-columns-2-3
     :transient t :description (lambda () (shex-manifest-browser--swap-description 1)))
    ("3" shex-manifest-browser-swap-columns-3-4
     :transient t :description (lambda () (shex-manifest-browser--swap-description 2)))
    ("4" shex-manifest-browser-swap-columns-4-5
     :transient t :description (lambda () (shex-manifest-browser--swap-description 3)))
    ("5" shex-manifest-browser-swap-columns-5-6
     :transient t :description (lambda () (shex-manifest-browser--swap-description 4)))]
   [("q" "Done" transient-quit-all)]])

;;; Toggling table <-> raw YAML display, `image-toggle-display'-style
;;
;; Mirrors `image-mode's own `image-toggle-display'/`image-mode-as-
;; text'/`image-minor-mode' trio: the *table* state is a real major
;; mode (`shex-manifest-browser-mode' itself); the *YAML* state uses
;; whatever major mode is otherwise appropriate for editing YAML
;; (`yaml-mode' if installed, `fundamental-mode' otherwise) plus
;; `shex-manifest-browser-yaml-view-mode', a minor mode whose only job
;; is keeping `C-c C-c' bound back to
;; `shex-manifest-browser-toggle-display' regardless of major mode.
;; Unlike an image file, the YAML view is real, editable, saveable
;; text -- `buffer-file-name' is attached to the manifest file while
;; showing it (detached again once back in table view, so `C-x C-s'
;; can't clobber the manifest with a rendered table -- see
;; `shex-manifest-browser-mode's own Commentary on why).

(defvar-keymap shex-manifest-browser-yaml-view-mode-map
  "C-c C-c" #'shex-manifest-browser-toggle-display)

(define-minor-mode shex-manifest-browser-yaml-view-mode
  "Minor mode turned on while a `shex-manifest-browser' buffer shows
its manifest's raw YAML text rather than the terse tabulated-list
view -- provides `C-c C-c' (`shex-manifest-browser-toggle-display') to
switch back, regardless of whatever major mode is actually editing
the YAML."
  :lighter " YAML-view")

(defun shex-manifest-browser--entry-start-position (index)
  "Position of manifest entry INDEX's (0-indexed, matching
`tabulated-list-get-id') own `- ...' line in the current buffer's YAML
text.  Assumes the standard, unindented `- key: value' top-level
sequence-item marker this format's entries always start with (true of
every manifest this package has ever produced or read -- see this
file's own Commentary) rather than doing a real YAML parse, which
would need `yaml-parse-string-with-pos', still explicitly marked
experimental as of this package's own `yaml' dependency version."
  (save-excursion
    (goto-char (point-min))
    (let (pos)
      (dotimes (_ (1+ index))
        (unless (re-search-forward "^- " nil t)
          (error "shex-manifest-browser: fewer than %d entries found in this manifest's YAML text"
                 (1+ index)))
        (setq pos (match-beginning 0)))
      pos)))

(defun shex-manifest-browser--entry-index-at-point ()
  "Which manifest entry (0-indexed, matching `tabulated-list-get-id')
point is within, in the current buffer's YAML text -- the reverse of
`shex-manifest-browser--entry-start-position'.  nil if point is before
the first entry's `- ...' line (e.g. still within a leading comment
block)."
  (save-excursion
    (let ((here (point)) (index -1))
      (goto-char (point-min))
      (while (and (re-search-forward "^- " nil t) (<= (match-beginning 0) here))
        (setq index (1+ index)))
      (and (>= index 0) index))))

;;; Saving an ephemeral schema/data/queryMap buffer back into the
;;; manifest file
;;
;; An inline `schema:'/`data:'/`queryMap:' field (as opposed to a
;; `schemaURL:'/`dataURL:'/`queryMapURL:' reference, which is instead
;; a real file `find-file-noselect'd -- already saveable the ordinary
;; way, nothing special needed) has no file of its own for its buffer
;; to save to. `shex-manifest-browser--enable-save-back' makes such a
;; buffer behave like a file-visiting one anyway: modified via any
;; edit (an ordinary Emacs-wide convention, nothing special needed
;; here), `C-x C-s' writes the field's own block scalar back into the
;; manifest file (via `write-contents-functions', which pre-empts
;; `basic-save-buffer''s own "no file, save where?" prompt), and both
;; `save-some-buffers'/`save-buffers-kill-emacs' and killing the
;; buffer directly prompt about unsaved changes exactly as they would
;; for a real file (`buffer-offer-save'/`kill-buffer-query-functions'
;; respectively -- `buffer-offer-save' alone has no effect on
;; `kill-buffer', per its own docstring).

(defun shex-manifest-browser--entry-span (index)
  "(BEG . END) of manifest entry INDEX's own text span in the current
buffer (a manifest file's raw YAML) -- from its own `- ...' line up to
\(not including) the next entry's, or `point-max' for the last entry.
Reuses `shex-manifest-browser--entry-start-position' for both ends."
  (cons (shex-manifest-browser--entry-start-position index)
        (condition-case nil
            (shex-manifest-browser--entry-start-position (1+ index))
          (error (point-max)))))

(defconst shex-manifest-browser--column-yaml-keys
  '((data . (dataLabel data dataURL))
    (schema . (schemaLabel schema schemaURL))
    (queryMap . (queryMap queryMapURL))
    (expected . (status)))
  "For `shex-manifest-browser-toggle-display': which YAML keys (in
priority order -- the first one this entry actually declares wins)
`shex-manifest-browser--field-position' looks for when jumping from a
table column to that column's corresponding line in the entry's raw
YAML.  `index'/`result' have no entry alist keys of their own (`index'
is just this entry's row number; `result' is computed, not stored),
so they're absent here -- `shex-manifest-browser--field-position'
returns nil for them, and callers fall back to the entry's own
`- ...' start line.")

(defun shex-manifest-browser--top-level-key-regexp (&optional key)
  "Regexp matching a manifest entry's own top-level `KEY: ...' lines --
either the first key, sharing its line with the entry's own `- '
marker, or a later one at the same two-column indentation -- but not
a `KEY:'-shaped line buried inside another key's block-scalar body
\(e.g. some ShExC/Turtle comment a `schema: |'/`data: |' body happens
to contain), which sits at a deeper indentation.  KEY, if non-nil, a
symbol, restricts the match to that key by name (used by
`shex-manifest-browser--field-position'); otherwise any bare
identifier key name matches (used by
`shex-manifest-browser--column-at-yaml-point', which doesn't know the
key up front)."
  (format "^\\(?:- \\|  \\)\\(%s\\):" (if key (regexp-quote (symbol-name key)) "[A-Za-z]+")))

(defun shex-manifest-browser--field-position (beg end column)
  "Position of the first line within BEG..END (an entry's own span,
per `shex-manifest-browser--entry-span') declaring one of COLUMN's
candidate YAML keys (`shex-manifest-browser--column-yaml-keys',
tried in order) -- e.g. `dataLabel:'/`data:'/`data: |'/`dataURL:' for
`data'.  nil if COLUMN has no mapping there, or this entry declares
none of its candidate keys."
  (save-excursion
    (seq-some
     (lambda (key)
       (goto-char beg)
       (when (re-search-forward (shex-manifest-browser--top-level-key-regexp key) end t)
         (pos-bol)))
     (alist-get column shex-manifest-browser--column-yaml-keys))))

(defun shex-manifest-browser--column-for-yaml-key (key)
  "Which `shex-manifest-browser-columns' key has KEY (a string, e.g.
\"dataLabel\") among its `shex-manifest-browser--column-yaml-keys'
candidates -- the reverse of that alist.  nil if KEY isn't any
column's candidate (e.g. `queryMap')."
  (car (seq-find (lambda (kv) (memq (intern key) (cdr kv)))
                  shex-manifest-browser--column-yaml-keys)))

(defun shex-manifest-browser--column-at-point ()
  "Which `shex-manifest-browser-columns' key the tabulated-list cell
at point belongs to, via the `tabulated-list-column-name' text
property `tabulated-list-print-col' puts on every cell -- nil if
point isn't on a recognized column (e.g. the header line, or a
column hidden/renamed since this text was drawn)."
  (when-let* ((title (get-text-property (point) 'tabulated-list-column-name)))
    (car (rassoc title shex-manifest-browser--column-titles))))

(defun shex-manifest-browser--column-at-yaml-point ()
  "Which `shex-manifest-browser-columns' key point's current position
in the manifest's YAML text belongs to -- the reverse of
`shex-manifest-browser--field-position': the last top-level `KEY:'
line (`shex-manifest-browser--top-level-key-regexp') at or before
point within its enclosing entry, mapped back to a column via
`shex-manifest-browser--column-for-yaml-key' -- so point anywhere
inside a `KEY: |' block scalar body still resolves to KEY's own
column, not nil, the same way `shex-manifest-browser--entry-index-at-
point' resolves point anywhere within an entry to that entry's index
rather than only right at its `- ...' line.  nil if point isn't
within any entry yet (e.g. a leading comment block), or its nearest
key has no column mapping (e.g. `queryMap:')."
  (when-let* ((index (shex-manifest-browser--entry-index-at-point)))
    (pcase-let* ((`(,beg . ,end) (shex-manifest-browser--entry-span index))
                 (here (point))
                 (key nil))
      (save-excursion
        (goto-char beg)
        (while (and (re-search-forward (shex-manifest-browser--top-level-key-regexp) end t)
                    (<= (match-beginning 0) here))
          (setq key (match-string 1))))
      (and key (shex-manifest-browser--column-for-yaml-key key)))))

(defun shex-manifest-browser--goto-column (column)
  "Within the tabulated-list row point is currently on, move point to
the start of COLUMN's cell, via the `tabulated-list-column-name' text
property `tabulated-list-print-col' puts on every cell -- the reverse
of `shex-manifest-browser--column-at-point'.  Leaves point at the
row's own start (a no-op) if COLUMN isn't currently visible (e.g.
hidden via `shex-manifest-browser-columns-menu')."
  (let ((title (shex-manifest-browser--column-title column))
        (pos (pos-bol))
        (eol (pos-eol)))
    (while (and pos (< pos eol)
                (not (equal (get-text-property pos 'tabulated-list-column-name) title)))
      (setq pos (next-single-property-change pos 'tabulated-list-column-name nil eol)))
    (goto-char (if (and pos (< pos eol)) pos (pos-bol)))))

(defun shex-manifest-browser--key-block-region (beg end key)
  "Within BEG..END (an entry's own span, per
`shex-manifest-browser--entry-span'), the (BODY-BEG BODY-END INDENT)
of KEY's (a symbol, e.g. `schema') block-scalar body: BODY-BEG/END
bound the lines strictly more indented than `KEY: |' itself (blank
lines included, since a YAML block scalar's body can contain them),
running from right after that line through the last such line before
one at or below its own indentation, or END; INDENT is that body's own
established indentation (its first line's, or KEY's own plus 2 if the
body is empty). nil if KEY isn't declared as a block scalar within
this span at all (e.g. this entry uses `KEY URL' instead)."
  (save-excursion
    (goto-char beg)
    (when (re-search-forward (format "^\\( *\\)%s: |[+-]?[ \t]*$" (regexp-quote (symbol-name key))) end t)
      (let ((key-indent (length (match-string 1)))
            body-indent)
        (forward-line 1)
        (let ((body-beg (point)))
          (when (and (< (point) end) (looking-at "^\\( *\\)[^ \t\n]"))
            (setq body-indent (length (match-string 1))))
          (while (and (< (point) end)
                      (or (looking-at-p "^[ \t]*$")
                          (and (looking-at "^\\( *\\)")
                               (> (length (match-string 1)) key-indent))))
            (forward-line 1))
          (list body-beg (point) (or body-indent (+ key-indent 2))))))))

(defun shex-manifest-browser--replace-block-scalar (manifest-path index key new-content)
  "Replace manifest entry INDEX's KEY block-scalar body in the file at
MANIFEST-PATH with NEW-CONTENT (a buffer's own current text, e.g. via
`buffer-string'), preserving the rest of the file exactly -- every
other entry, comment, and this same entry's own other keys. Signals an
error, rather than silently doing nothing or corrupting the file, if
INDEX/KEY don't resolve to a block scalar the way
`shex-manifest-browser--key-block-region' expects (e.g. this entry
actually uses `KEY URL' instead)."
  (with-temp-buffer
    (insert-file-contents manifest-path)
    (pcase-let* ((`(,entry-beg . ,entry-end) (shex-manifest-browser--entry-span index))
                 (region (or (shex-manifest-browser--key-block-region entry-beg entry-end key)
                             (error "shex-manifest-browser: entry %d has no `%s: |' block scalar to write back to"
                                    index key))))
      (pcase-let ((`(,body-beg ,body-end ,body-indent) region))
        (goto-char body-beg)
        (delete-region body-beg body-end)
        (let ((indent (make-string body-indent ?\s))
              ;; A trailing "" from NEW-CONTENT's own final newline
              ;; shouldn't become an indented blank line of its own.
              (lines (split-string new-content "\n")))
          (when (and (cdr lines) (string-empty-p (car (last lines))))
            (setq lines (butlast lines)))
          (dolist (line lines)
            (insert (if (string-empty-p line) "" indent) line "\n")))))
    (write-region (point-min) (point-max) manifest-path nil 'quiet)))

(defvar-local shex-manifest-browser--save-back nil
  "(MANIFEST-PATH INDEX KEY) if this buffer's contents should be
written back into that manifest file entry's own KEY block scalar on
save -- set by `shex-manifest-browser--enable-save-back'. See
`shex-manifest-browser--write-contents'.")

(defun shex-manifest-browser--write-contents ()
  "`write-contents-functions' member for a buffer
`shex-manifest-browser--enable-save-back' set up: writes this buffer's
current text back into its manifest entry's own block scalar
\(`shex-manifest-browser--replace-block-scalar'), instead of
`basic-save-buffer' trying -- and failing, since these buffers have no
`buffer-file-name' of their own -- to prompt for a file to write.
Returns t, telling `basic-save-buffer' the save is already handled."
  (pcase-let ((`(,manifest-path ,index ,key) shex-manifest-browser--save-back))
    (shex-manifest-browser--replace-block-scalar manifest-path index key (buffer-string))
    (set-buffer-modified-p nil)
    (message "Wrote `%s' back into entry %d of %s" key index (file-name-nondirectory manifest-path)))
  t)

(defun shex-manifest-browser--kill-buffer-query ()
  "Buffer-local `kill-buffer-query-functions' member for a buffer
`shex-manifest-browser--enable-save-back' set up: prompts to save (or
confirms discarding) unsaved changes before killing, mirroring what
killing a modified file-visiting buffer already does natively --
`buffer-offer-save' has no effect on `kill-buffer' itself (per its own
docstring), and this buffer has no `buffer-file-name' for `kill-buffer'
to notice on its own."
  (or (not (buffer-modified-p))
      (if (y-or-n-p (format "Buffer %s modified; save before killing? " (buffer-name)))
          (progn (save-buffer) t)
        (y-or-n-p (format "Buffer %s modified; kill anyway? " (buffer-name))))))

(defun shex-manifest-browser--enable-save-back (buffer manifest-path index key)
  "Arrange for BUFFER (an ephemeral schema/data/queryMap buffer for
manifest entry INDEX's inline KEY field) to write its own text back
into that field's block scalar in the file at MANIFEST-PATH whenever
it's saved, and to be treated as save-worthy (offered by
`save-some-buffers'/`save-buffers-kill-emacs', prompted about at
`kill-buffer' time if modified) despite having no `buffer-file-name'
of its own."
  (with-current-buffer buffer
    (setq-local shex-manifest-browser--save-back (list manifest-path index key))
    (setq-local buffer-offer-save 'always)
    (add-hook 'write-contents-functions #'shex-manifest-browser--write-contents nil t)
    (add-hook 'kill-buffer-query-functions #'shex-manifest-browser--kill-buffer-query nil t)))

(defun shex-manifest-browser--goto-entry-row (index)
  "Move point to the row for manifest entry INDEX (0-indexed) in the
current (already redrawn) tabulated-list buffer -- wherever it
currently sits, not assuming file order, since `tabulated-list-mode's
own column sorting can reorder rows."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (equal (tabulated-list-get-id) index)))
    (forward-line 1)))

(defun shex-manifest-browser--show-yaml (&optional index column)
  "Switch this (already `shex-manifest-browser-mode') buffer to its
manifest's raw YAML text, editable and saveable back to the real
file -- the other half of `shex-manifest-browser-toggle-display'.
INDEX, if non-nil, is which entry (0-indexed, as `tabulated-list-get-id'
returns) point should land on -- the same entry as the row point was on
in the table, per `shex-manifest-browser--entry-start-position'.
COLUMN, if non-nil, additionally lands point on the YAML line
declaring that table column's underlying key within the entry (per
`shex-manifest-browser--column-yaml-keys'/`--field-position') rather
than just the entry's own start line -- e.g. toggling away from the
`Schema' column lands on that entry's `schemaLabel:'/`schema:'/
`schemaURL:' line, not just its `- ...' line, so which field you were
looking at in the table carries over to which line you start editing.
Falls back to the entry's start line if COLUMN has no YAML-key mapping
(`index'/`result') or this entry declares none of its candidate keys."
  (let ((path (expand-file-name shex-manifest-browser--manifest-file
                                 shex-manifest-browser--manifest-dir))
        (inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents path)
    (funcall (if (fboundp 'yaml-mode) #'yaml-mode #'fundamental-mode))
    ;; `buffer-read-only' is `permanent-local' (an Emacs-wide
    ;; convention, not this mode's own doing), so `tabulated-list-
    ;; mode's read-only buffer would otherwise stay read-only across
    ;; the major-mode switch above.
    (setq buffer-read-only nil)
    (setq buffer-file-name path)
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    ;; Restore the undo history from before the trip to table view --
    ;; see `shex-manifest-browser--saved-undo-list'.  Unconditional,
    ;; not just when there's a captured list to restore: `buffer-undo-
    ;; list' is itself a built-in permanent-local variable, so
    ;; `tabulated-list-mode's own `(setq-local buffer-undo-list t)'
    ;; (disabling undo for the table view -- appropriate there, it's a
    ;; computed display) would otherwise survive the switch back to
    ;; `yaml-mode' above and leave undo silently disabled going
    ;; forward, even with nothing to restore (e.g. the first-ever trip
    ;; to table view, before any history has been captured).
    (setq buffer-undo-list shex-manifest-browser--saved-undo-list)
    (setq shex-manifest-browser--saved-undo-list nil)
    (shex-manifest-browser-yaml-view-mode 1)
    (goto-char (if index (shex-manifest-browser--entry-start-position index) (point-min)))
    (when (and index column)
      (pcase-let ((`(,beg . ,end) (shex-manifest-browser--entry-span index)))
        (when-let* ((pos (shex-manifest-browser--field-position beg end column)))
          (goto-char pos))))))

(defun shex-manifest-browser--show-table (&optional index column)
  "Switch this buffer back to its `shex-manifest-browser-mode' table
view, re-reading the manifest from disk (so an edit made in the YAML
view -- e.g. to a `queryMap''s node/shape reference -- is reflected
immediately, though its Result column stays blank until the entry is
re-run) -- the other half of `shex-manifest-browser-toggle-display'.
If the YAML view has unsaved edits, offers to save (or discard) them
first, since they'd otherwise be silently lost when the manifest is
re-read.  INDEX, if non-nil, is which entry (0-indexed) point should
land on -- the same entry point was in within the YAML text, per
`shex-manifest-browser--entry-index-at-point'.  COLUMN, if non-nil,
additionally positions point on that entry row's cell for that column
\(`shex-manifest-browser--goto-column') -- the reverse of
`shex-manifest-browser--show-yaml's COLUMN argument: which key's line
point was on in the YAML (`shex-manifest-browser--column-at-yaml-
point') carries over to which column point lands in back in the
table."
  (when (buffer-modified-p)
    (if (y-or-n-p "Save changes to the manifest file before switching to table view? ")
        (save-buffer)
      (unless (y-or-n-p "Discard unsaved changes? ")
        (user-error "Not switching to table view"))))
  ;; Stash the undo history for `shex-manifest-browser--show-yaml' to
  ;; restore later, but only when the buffer text matches disk right
  ;; now (saved above, or already clean) -- if the user just discarded
  ;; unsaved edits instead, this undo list was recorded against text
  ;; that the later `insert-file-contents' won't reproduce, so it'd be
  ;; garbage to replay against the reloaded buffer.
  (setq shex-manifest-browser--saved-undo-list
        (and (not (buffer-modified-p)) buffer-undo-list))
  (let ((path (expand-file-name shex-manifest-browser--manifest-file
                                 shex-manifest-browser--manifest-dir)))
    (setq buffer-file-name nil)
    (shex-manifest-browser-mode)
    (shex-manifest-browser--initialize path)
    (tabulated-list-print)
    (set-buffer-modified-p nil)
    (when index
      (shex-manifest-browser--goto-entry-row index)
      (when column (shex-manifest-browser--goto-column column)))))

(defun shex-manifest-browser-toggle-display ()
  "Toggle between this manifest's terse tabulated-list view and its
raw YAML text -- like `image-toggle-display' toggles an image buffer
between the rendered image and its underlying bytes.  Either direction
puts point on the same entry it was on before toggling, and, in
either direction, on the corresponding column/YAML-key too: which
column point was in the table (`shex-manifest-browser--column-at-
point') decides which line `shex-manifest-browser--show-yaml' lands
on, and which key's line point was on in the YAML
\(`shex-manifest-browser--column-at-yaml-point') decides which column
`shex-manifest-browser--show-table' lands on."
  (interactive)
  (if (derived-mode-p 'shex-manifest-browser-mode)
      (shex-manifest-browser--show-yaml (tabulated-list-get-id)
                                         (shex-manifest-browser--column-at-point))
    (shex-manifest-browser--show-table (shex-manifest-browser--entry-index-at-point)
                                        (shex-manifest-browser--column-at-yaml-point))))

(defvar-keymap shex-manifest-browser-mode-map
  :parent tabulated-list-mode-map
  "RET" #'shex-manifest-browser-run-entry
  "a" #'shex-manifest-browser-run-all
  "m" #'shex-manifest-browser-columns-menu
  "C-c C-c" #'shex-manifest-browser-toggle-display)

(define-derived-mode shex-manifest-browser-mode tabulated-list-mode "ShEx-Manifest"
  "Major mode listing a YAML test manifest's entries -- see
`shex-manifest-browser-open'.  `RET' runs the entry at point;
`a' (`shex-manifest-browser-run-all') runs every entry; `g'
\(`revert-buffer', bound by `tabulated-list-mode' already) reparses the
manifest file, discarding results; `m'
\(`shex-manifest-browser-columns-menu') picks which columns are shown
and their order; `C-c C-c'
\(`shex-manifest-browser-toggle-display', `image-toggle-display'-style)
switches to editing the manifest's raw YAML text directly (`C-c C-c'
there switches back to this table).

Turning this mode on directly (`M-x shex-manifest-browser-mode') in a
buffer already visiting a manifest file also works -- it loads that
same file's entries and detaches the buffer from the file (so `C-x
C-s' can't clobber the manifest with this mode's own rendering; use
`C-c C-c' to resume editing its YAML directly, rather than `find-file'
or `shex-manifest-browser-open' again).

Columns are resized to fit whenever a window showing this buffer is
resized -- see `shex-manifest-browser--set-column-widths'."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'shex-manifest-browser--entries-fn)
  (setq revert-buffer-function #'shex-manifest-browser-revert)
  (shex-manifest-browser--set-column-widths)
  (add-hook 'window-configuration-change-hook #'shex-manifest-browser--set-column-widths nil t)
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
