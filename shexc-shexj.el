;;; shexc-shexj.el --- ShExC <-> ShExJ value-tree compiler/decompiler -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Compiles a `shexc-ts-mode' tree-sitter parse tree to a "value-tree" --
;; an Emacs Lisp plist/list shape isomorphic to the ShExJ JSON
;; serialization of ShEx (see https://shex.io/shex-semantics/#shexj) --
;; decompiles a value-tree back to ShExC text, and adapts a value-tree
;; to/from actual ShExJ JSON text via `json-serialize'/`json-parse-string'.
;;
;; Value-tree shape: JSON objects are plists with keyword keys
;; (`:type', `:predicate', ...); JSON arrays are Lisp lists; strings,
;; integers, and the symbol `t' (for JSON `true') are themselves.  This
;; is exactly `json-parse-string's `:object-type 'plist :array-type
;; 'list' shape, so `shexc-shexj-to-json'/`shexc-shexj-from-json' are
;; thin layers over Emacs's native JSON support.
;;
;; Round-tripping ShExC -> value-tree -> ShExC is allowed to lose
;; comments/whitespace/prefix shorthand but must preserve semantics
;; (verified by the ERT suite in shexc-shexj-tests.el against the
;; shexSpec/shexTest corpus).

;;; Code:

(require 'treesit)
(require 'cl-lib)
(require 'pcase)
(require 'json)
(require 'url-expand)

(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-children "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-child-count "treesit.c")
(declare-function treesit-node-eq "treesit.c")
(declare-function treesit-buffer-root-node "treesit.c")
(declare-function treesit-search-subtree "treesit.c")

(defun shexc-shexj--named-children (node)
  "Like `(treesit-node-children NODE t)', but excluding `comment' nodes.
`comment' is declared as a tree-sitter `extras' token (grammar.js), so
it can appear between *any* two siblings anywhere in the grammar --
every NAMED-children walk in this file must skip over one, or it gets
fed into logic that expects only semantically-meaningful children
(confirmed empirically: a comment inside a `[...]' value_set crashed
`shexc-shexj--compile-values' on `_all.shex')."
  (seq-remove (lambda (c) (equal (treesit-node-type c) "comment"))
              (treesit-node-children node t)))

(defconst shexc-shexj--context-iri "http://www.w3.org/ns/shex.jsonld"
  "The fixed ShExJ `@context' value.")

(defconst shexc-shexj--rdf-type-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defconst shexc-shexj--xsd-integer-iri "http://www.w3.org/2001/XMLSchema#integer")
(defconst shexc-shexj--xsd-decimal-iri "http://www.w3.org/2001/XMLSchema#decimal")
(defconst shexc-shexj--xsd-double-iri "http://www.w3.org/2001/XMLSchema#double")
(defconst shexc-shexj--xsd-boolean-iri "http://www.w3.org/2001/XMLSchema#boolean")

;; ---------------------------------------------------------------------
;; Compile-time context: base IRI + prefix table, threaded through a
;; single linear pass over a shex_doc's top-level directives (base/
;; prefix decls can only appear there, never inside a shape body, so
;; later IRIs always resolve against whatever directive most recently
;; preceded them in document order).
;; ---------------------------------------------------------------------

(cl-defstruct (shexc-shexj--ctx (:constructor shexc-shexj--make-ctx))
  (base nil)
  (prefixes (make-hash-table :test 'equal)))

(defun shexc-shexj--resolve-iri (ctx iri)
  "Resolve IRI (already escape-decoded) against CTX's current base."
  (if (shexc-shexj--ctx-base ctx)
      (url-expand-file-name iri (shexc-shexj--ctx-base ctx))
    iri))

;; ---------------------------------------------------------------------
;; Lexical unescaping (UCHAR \uXXXX / \UXXXXXXXX, ECHAR for strings,
;; PN_LOCAL_ESC backslash-escaped punctuation for prefixed-name locals).
;; ---------------------------------------------------------------------

(defconst shexc-shexj--echar-alist
  '((?' . "'") (?\" . "\"") (?\\ . "\\")
    (?b . "\b") (?f . "\f") (?n . "\n") (?r . "\r") (?t . "\t"))
  "ECHAR escapes recognized inside ShExC/Turtle string literals.")

(defun shexc-shexj--unescape (text &optional echar-alist)
  "Decode \\uXXXX/\\UXXXXXXXX and, per ECHAR-ALIST, single-char escapes in TEXT.
With ECHAR-ALIST nil, any other backslash-escaped character simply drops
its backslash (PN_LOCAL_ESC semantics)."
  (if (not (string-search "\\" text))
      text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; case-fold-search would make `u' match `U' and vice versa,
      ;; silently corrupting the choice between the two alternatives.
      (let ((case-fold-search nil))
      (while (re-search-forward
              "\\\\\\(u[0-9A-Fa-f]\\{4\\}\\|U[0-9A-Fa-f]\\{8\\}\\|.\\)" nil t)
        (let* ((whole (match-string 1))
               (repl
                (cond
                 ((string-prefix-p "u" whole)
                  (string (string-to-number (substring whole 1) 16)))
                 ((string-prefix-p "U" whole)
                  (string (string-to-number (substring whole 1) 16)))
                 ((and echar-alist (assoc (string-to-char whole) echar-alist))
                  (cdr (assoc (string-to-char whole) echar-alist)))
                 (t whole))))
          (replace-match repl t t))))
      (buffer-string))))

(defun shexc-shexj--strip-angle-brackets (text)
  (substring text 1 (1- (length text))))

(defun shexc-shexj--strip-quotes (text)
  (let ((n (if (or (string-prefix-p "'''" text) (string-prefix-p "\"\"\"" text)) 3 1)))
    (substring text n (- (length text) n))))

;; ---------------------------------------------------------------------
;; IRI / prefixed-name / predicate resolution
;; ---------------------------------------------------------------------

(defun shexc-shexj--compile-pname-ns (ctx node)
  (let* ((text (treesit-node-text node t))
         (iri (gethash text (shexc-shexj--ctx-prefixes ctx))))
    (or iri (error "shexc-shexj: undefined prefix `%s'" text))))

(defun shexc-shexj--compile-pname-ln (ctx node)
  (let* ((text (treesit-node-text node t))
         (colon (string-search ":" text))
         (prefix (substring text 0 (1+ colon)))
         (local (shexc-shexj--unescape (substring text (1+ colon))))
         (base-iri (gethash prefix (shexc-shexj--ctx-prefixes ctx))))
    (unless base-iri (error "shexc-shexj: undefined prefix `%s'" prefix))
    (concat base-iri local)))

(defun shexc-shexj--iri-node-text (ctx node)
  "Resolve NODE -- an irireference/prefixed_name/pname_ln/pname_ns/datatype/
blank_node node, or a wrapper (shape_expr_label, triple_expr_label, ...)
whose sole unnamed child is one of those -- to an absolute IRI string (or,
for a blank node, its `_:label' text unchanged)."
  (pcase (treesit-node-type node)
    ("irireference"
     (shexc-shexj--resolve-iri
      ctx (shexc-shexj--unescape (shexc-shexj--strip-angle-brackets (treesit-node-text node t)))))
    ("prefixed_name" (shexc-shexj--iri-node-text ctx (treesit-node-child node 0)))
    ("pname_ln" (shexc-shexj--compile-pname-ln ctx node))
    ("pname_ns" (shexc-shexj--compile-pname-ns ctx node))
    ("datatype" (shexc-shexj--iri-node-text ctx (treesit-node-child node 0)))
    ("blank_node" (treesit-node-text node t))
    ((or "shape_expr_label" "triple_expr_label")
     (shexc-shexj--iri-node-text ctx (treesit-node-child node 0)))
    (_ (error "shexc-shexj: expected IRI-like node, got `%s'" (treesit-node-type node)))))

(defun shexc-shexj--compile-predicate (ctx node)
  "NODE is a `predicate' node (irireference|prefixed_name|kw_a)."
  (let ((child (treesit-node-child node 0)))
    (if (equal (treesit-node-type child) "kw_a")
        shexc-shexj--rdf-type-iri
      (shexc-shexj--iri-node-text ctx child))))

;; ---------------------------------------------------------------------
;; Directives: BASE / PREFIX / IMPORT
;; ---------------------------------------------------------------------

(defun shexc-shexj--apply-base (ctx node)
  (let* ((iri-node (treesit-node-child-by-field-name node "iri"))
         (raw (shexc-shexj--unescape (shexc-shexj--strip-angle-brackets (treesit-node-text iri-node t)))))
    (setf (shexc-shexj--ctx-base ctx) (shexc-shexj--resolve-iri ctx raw))))

(defun shexc-shexj--apply-prefix (ctx node)
  (let* ((name-node (treesit-node-child-by-field-name node "name"))
         (iri-node (treesit-node-child-by-field-name node "iri"))
         (name (treesit-node-text name-node t))
         (raw (shexc-shexj--unescape (shexc-shexj--strip-angle-brackets (treesit-node-text iri-node t)))))
    (puthash name (shexc-shexj--resolve-iri ctx raw) (shexc-shexj--ctx-prefixes ctx))))

(defun shexc-shexj--compile-import (ctx node)
  (shexc-shexj--iri-node-text ctx (treesit-node-child-by-field-name node "iri")))

;; ---------------------------------------------------------------------
;; Literals
;; ---------------------------------------------------------------------

(defun shexc-shexj--compile-lang-string (node)
  "NODE is a `lang_string' token: a quoted string immediately followed
by a langtag, glued into one lexer token."
  (let* ((text (treesit-node-text node t))
         (m (string-match "@[A-Za-z]+\\(?:-[0-9A-Za-z]+\\)*\\'" text))
         (str-part (substring text 0 m))
         ;; ShExJ normalizes language tags to lowercase (confirmed empirically:
         ;; "...@en-UK" compiles to :language "en-uk").
         (lang (downcase (substring text (1+ m)))))
    (list :value (shexc-shexj--unescape (shexc-shexj--strip-quotes str-part) shexc-shexj--echar-alist)
          :language lang)))

(defun shexc-shexj--compile-rdf-literal (ctx node)
  (let ((first (treesit-node-child node 0)))
    (if (equal (treesit-node-type first) "lang_string")
        (shexc-shexj--compile-lang-string first)
      (let* ((value-node (treesit-node-child-by-field-name node "value"))
             (datatype-node (treesit-node-child-by-field-name node "datatype"))
             (value (shexc-shexj--unescape
                     (shexc-shexj--strip-quotes (treesit-node-text value-node t))
                     shexc-shexj--echar-alist)))
        (if datatype-node
            (list :value value :type (shexc-shexj--iri-node-text ctx datatype-node))
          (list :value value))))))

(defun shexc-shexj--compile-numeric-literal (node)
  (let* ((child (treesit-node-child node 0))
         (type (pcase (treesit-node-type child)
                 ("integer" shexc-shexj--xsd-integer-iri)
                 ("decimal" shexc-shexj--xsd-decimal-iri)
                 ("double" shexc-shexj--xsd-double-iri))))
    (list :value (treesit-node-text child t) :type type)))

(defun shexc-shexj--compile-boolean-literal (node)
  (let* ((child (treesit-node-child node 0))
         (val (if (equal (treesit-node-type child) "kw_true") "true" "false")))
    (list :value val :type shexc-shexj--xsd-boolean-iri)))

(defun shexc-shexj--compile-literal (ctx node)
  "NODE is a `literal' node (rdf_literal|numeric_literal|boolean_literal)."
  (let ((child (treesit-node-child node 0)))
    (pcase (treesit-node-type child)
      ("rdf_literal" (shexc-shexj--compile-rdf-literal ctx child))
      ("numeric_literal" (shexc-shexj--compile-numeric-literal child))
      ("boolean_literal" (shexc-shexj--compile-boolean-literal child)))))

;; ---------------------------------------------------------------------
;; Value sets
;; ---------------------------------------------------------------------

(defun shexc-shexj--stem-has-tilde-p (range-node stem-node)
  "Detect a trailing `~' on RANGE-NODE (an *_stem_range or *_exclusion
node) by comparing its end position to STEM-NODE's: the `~' token has
no node of its own, so its presence only shows up as extra span."
  (/= (treesit-node-end range-node) (treesit-node-end stem-node)))

(defun shexc-shexj--compile-iri-exclusion (ctx node)
  (let* ((stem-node (treesit-node-child-by-field-name node "stem"))
         (iri (shexc-shexj--iri-node-text ctx stem-node)))
    (if (shexc-shexj--stem-has-tilde-p node stem-node)
        (list :type "IriStem" :stem iri)
      iri)))

(defun shexc-shexj--compile-iri-stem-range (ctx node)
  (let* ((stem-node (treesit-node-child-by-field-name node "stem"))
         (iri (shexc-shexj--iri-node-text ctx stem-node))
         (exclusions (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "iri_exclusion")))))
    (cond
     ((not (shexc-shexj--stem-has-tilde-p node stem-node)) iri)
     (exclusions
      (list :type "IriStemRange" :stem iri
            :exclusions (mapcar (lambda (e) (shexc-shexj--compile-iri-exclusion ctx e)) exclusions)))
     (t (list :type "IriStem" :stem iri)))))

(defun shexc-shexj--compile-literal-exclusion (ctx node)
  (let* ((stem-node (treesit-node-child-by-field-name node "stem"))
         (lit (shexc-shexj--compile-literal ctx stem-node)))
    (if (shexc-shexj--stem-has-tilde-p node stem-node)
        (list :type "LiteralStem" :stem (plist-get lit :value))
      (plist-get lit :value))))

(defun shexc-shexj--compile-literal-stem-range (ctx node)
  (let* ((stem-node (treesit-node-child-by-field-name node "stem"))
         (lit (shexc-shexj--compile-literal ctx stem-node))
         (exclusions (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "literal_exclusion")))))
    (cond
     ((not (shexc-shexj--stem-has-tilde-p node stem-node)) lit)
     (exclusions
      (list :type "LiteralStemRange" :stem (plist-get lit :value)
            :exclusions (mapcar (lambda (e) (shexc-shexj--compile-literal-exclusion ctx e)) exclusions)))
     (t (list :type "LiteralStem" :stem (plist-get lit :value))))))

(defun shexc-shexj--compile-language-exclusion (node)
  (let* ((stem-node (treesit-node-child-by-field-name node "stem"))
         (lang (downcase (substring (treesit-node-text stem-node t) 1)))) ; drop leading @
    (if (shexc-shexj--stem-has-tilde-p node stem-node)
        (list :type "LanguageStem" :stem lang)
      lang)))

(defun shexc-shexj--compile-language-stem-range (node)
  (let* ((stem-node (treesit-node-child-by-field-name node "stem"))
         (lang (if stem-node (downcase (substring (treesit-node-text stem-node t) 1)) ""))
         (exclusions (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "language_exclusion")))))
    (cond
     ((and stem-node (not (shexc-shexj--stem-has-tilde-p node stem-node)))
      (list :type "Language" :languageTag lang))
     ;; The bare `@~' form (no preceding langtag) has no Wildcard concept of
     ;; its own -- empirically it's just an empty-string stem, unlike the
     ;; `.'-plus-exclusions form (`shexc-shexj--compile-dot-exclusions'),
     ;; which does use a real Wildcard object.
     (exclusions
      (list :type "LanguageStemRange" :stem lang
            :exclusions (mapcar #'shexc-shexj--compile-language-exclusion exclusions)))
     (t (list :type "LanguageStem" :stem lang)))))

(defun shexc-shexj--compile-value-set-value (ctx node)
  (pcase (treesit-node-type node)
    ("iri_stem_range" (shexc-shexj--compile-iri-stem-range ctx node))
    ("literal_stem_range" (shexc-shexj--compile-literal-stem-range ctx node))
    ("language_stem_range" (shexc-shexj--compile-language-stem-range node))
    (_ (error "shexc-shexj: unexpected value-set entry `%s'" (treesit-node-type node)))))

(defun shexc-shexj--compile-dot-exclusions (ctx node)
  "NODE is the `values'-field node_constraint wrapping a bare `.' wildcard
plus one or more *_exclusion children (all of the same kind)."
  (let ((excl (treesit-node-child node 0 t)))
    (pcase (treesit-node-type excl)
      ("iri_exclusion"
       (list :type "IriStemRange" :stem (list :type "Wildcard")
             :exclusions (mapcar (lambda (e) (shexc-shexj--compile-iri-exclusion ctx e))
                                  (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "iri_exclusion"))))))
      ("literal_exclusion"
       (list :type "LiteralStemRange" :stem (list :type "Wildcard")
             :exclusions (mapcar (lambda (e) (shexc-shexj--compile-literal-exclusion ctx e))
                                  (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "literal_exclusion"))))))
      ("language_exclusion"
       (list :type "LanguageStemRange" :stem (list :type "Wildcard")
             :exclusions (mapcar #'shexc-shexj--compile-language-exclusion
                                  (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "language_exclusion")))))))))

(defun shexc-shexj--compile-values (ctx values-node)
  "VALUES-NODE is the `values'-field node_constraint; its children are
*_stem_range entries, or (for the `. -excl -excl' wildcard-exclusion
form) a bare `.' anonymous token followed directly by *_exclusion
children with no enclosing *_stem_range."
  (let ((children (shexc-shexj--named-children values-node)))
    (if (and children (member (treesit-node-type (car children))
                               '("iri_exclusion" "literal_exclusion" "language_exclusion")))
        (list (shexc-shexj--compile-dot-exclusions ctx values-node))
      (mapcar (lambda (c) (shexc-shexj--compile-value-set-value ctx c)) children))))

;; ---------------------------------------------------------------------
;; Node constraints.  `shape_atom'/`inline_shape_atom' expose pieces of
;; a NodeConstraint as several *sibling* fields/children (node_kind,
;; constraint, datatype, values, plus a bare kw_literal and/or a bare
;; unfielded node_constraint for accompanying facets) rather than one
;; single nested node -- empirically confirmed via `tree-sitter parse',
;; not deducible from grammar.js alone.  We flatten all of it into one
;; merged NodeConstraint plist.
;; ---------------------------------------------------------------------

(defun shexc-shexj--parse-facet-number (text)
  "Parse TEXT (a facet's numeral source text) to a Lisp number, collapsing
a whole-valued float to an integer.  ShExJ facet numbers behave like JS
numbers, which have no separate int/float type: `MININCLUSIVE 5.0'
compiles to `\"mininclusive\": 5', not `5.0' -- confirmed empirically --
so `string-to-number's float result for whole values must be truncated
to compare/print `equal' to the real oracle's plain integer."
  (let ((n (string-to-number text)))
    (if (and (floatp n) (= n (truncate n))) (truncate n) n)))

(defun shexc-shexj--compile-facet (ctx node)
  "NODE is a string_facet or numeric_facet; returns (KEY . VALUE).
Unlike ObjectLiteral values (where exact source lexical form like
\"0E0\" must round-trip verbatim), ShExJ facet values are plain JSON
numbers, normalized like JS numbers -- see `shexc-shexj--parse-facet-number'."
  (ignore ctx) ; facet values never resolve IRIs; datatype on a ^^-typed
               ; facet value is intentionally discarded (see plan notes)
  (pcase (treesit-node-type node)
    ("string_facet"
     (let ((pattern (treesit-node-child-by-field-name node "pattern")))
       (if pattern
           (shexc-shexj--compile-regexp pattern)
         (let* ((name (treesit-node-text (treesit-node-child (treesit-node-child-by-field-name node "name") 0) t))
                (value (treesit-node-text (treesit-node-child-by-field-name node "value") t)))
           (list (cons (shexc-shexj--facet-key name) (shexc-shexj--parse-facet-number value)))))))
    ("numeric_facet"
     (let* ((name-node (treesit-node-child-by-field-name node "name"))
            (name (treesit-node-text (treesit-node-child name-node 0) t))
            (value-node (treesit-node-child-by-field-name node "value"))
            (value-text (if (equal (treesit-node-type value-node) "string")
                             (shexc-shexj--strip-quotes (treesit-node-text value-node t))
                           (treesit-node-text value-node t))))
       (list (cons (shexc-shexj--facet-key name) (shexc-shexj--parse-facet-number value-text)))))
    (_ (error "shexc-shexj: unexpected facet node `%s'" (treesit-node-type node)))))

(defun shexc-shexj--facet-key (kw-text)
  (intern (concat ":" (downcase kw-text))))

(defun shexc-shexj--unescape-regexp-body (body)
  "Decode only what's needed to recover the literal regex source: UCHAR
escapes, and `\\/' (escaped only so `/' could appear inside `/.../').
Every other backslash sequence (`\\.', `\\n', `\\d', `\\\\', ...) is regex
syntax meant for the eventual regex engine, not for us, and must pass
through verbatim, two characters at a time.

This must be a single sequential left-to-right walk, not a global
regexp search-and-replace: a verbatim-passed-through backslash pair
(e.g. the first half of `\\\\\\\\u0061', two escaped-backslash pairs
followed by literal text \"u0061\", confirmed empirically against
`1literalPattern_with_REGEXP_escapes_escaped.json') must not let its
second backslash be re-examined as the start of a *new* escape -- a
global search loop that merely skips a non-matching start position
(rather than consuming it) would wrongly resume scanning from inside
the pair and decode `\\u0061' anyway."
  (if (not (string-search "\\" body))
      body
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward "\\\\" nil t)
          (goto-char (match-beginning 0))
          (cond
           ((looking-at "\\\\\\(u[0-9A-Fa-f]\\{4\\}\\|U[0-9A-Fa-f]\\{8\\}\\)")
            (replace-match (string (string-to-number (substring (match-string 1) 1) 16)) t t))
           ((looking-at "\\\\/")
            (replace-match "/" t t))
           (t (goto-char (min (point-max) (+ (point) 2)))))))
      (buffer-string))))

(defun shexc-shexj--escape-regexp-body (body)
  "Inverse of `shexc-shexj--unescape-regexp-body's `/' handling, for
decompiling a pattern back into ShExC's `/.../' syntax."
  (replace-regexp-in-string "/" "\\\\/" body))

(defun shexc-shexj--compile-regexp (regexp-node)
  "REGEXP-NODE's text is `/body/flags'.
Returns ((:pattern . body) (:flags . flags)?)."
  ;; REGEXP_SRC = /(?:[^/\n\r]|\\[...]|UCHAR)+/[smix]*  -- the closing `/'
  ;; is wherever the trailing [smix]* run (if any) begins, minus one.
  (let* ((text (treesit-node-text regexp-node t))
         (i (1- (length text)))
         (flags-end i))
    (while (and (>= i 0) (memq (aref text i) '(?s ?m ?i ?x)))
      (setq i (1- i)))
    (let ((flags (substring text (1+ i) (1+ flags-end)))
          (body (substring text 1 i)))
      (append (list (cons :pattern (shexc-shexj--unescape-regexp-body body)))
              (when (> (length flags) 0) (list (cons :flags flags)))))))

(defun shexc-shexj--node-constraint-from-atom (ctx atom-node)
  "Gather every node_constraint-contributing child of ATOM-NODE (a
shape_atom/inline_shape_atom) into one merged NodeConstraint plist, or
nil if ATOM-NODE carries no node-constraint pieces at all."
  (let (acc (any nil))
    (dolist (c (shexc-shexj--named-children atom-node))
      (pcase (treesit-node-type c)
        ("node_constraint"
         (setq any t)
         (let ((field (treesit-node-field-name c)))
           (pcase field
             ("datatype" (setq acc (plist-put acc :datatype (shexc-shexj--iri-node-text ctx (treesit-node-child c 0)))))
             ("values" (setq acc (plist-put acc :values (shexc-shexj--compile-values ctx c))))
             ("node_kind"
              (setq acc (plist-put acc :nodeKind
                                    (pcase (treesit-node-type (treesit-node-child c 0))
                                      ("kw_iri" "iri") ("kw_bnode" "bnode") ("kw_nonliteral" "nonliteral")))))
             (_ ;; "constraint" field, or unfielded (LITERAL/datatype/values path's facets)
              (dolist (fc (shexc-shexj--named-children c))
                (when (member (treesit-node-type fc) '("string_facet" "numeric_facet"))
                  (dolist (kv (shexc-shexj--compile-facet ctx fc))
                    (setq acc (plist-put acc (car kv) (cdr kv))))))))))
        ("kw_literal" (setq any t) (setq acc (plist-put acc :nodeKind "literal")))))
    (when any acc)))

;; ---------------------------------------------------------------------
;; Shape expressions
;; ---------------------------------------------------------------------

(defun shexc-shexj--paren-shape-atom-p (node)
  "Is NODE (a shape_atom/inline_shape_atom) the literal `( shape_expr )'
form?  Distinguishing this from the *juxtaposition* form (`IRI {...}',
whose `shape_expr' field can only ever be a shape_definition/shape_ref)
is exactly what tells `shexc-shexj--and-or-operand' whether a nested
ShapeAnd/ShapeOr it finds there was a deliberate, semantically-
meaningful grouping (parens: never flatten) or just this compiler's own
synthesized wrapper for an implicit conjunction (juxtaposition: flatten
into an adjacent same-operator chain like any other operand)."
  (let ((se (treesit-node-child-by-field-name node "shape_expr")))
    (and se (member (treesit-node-type se) '("shape_and" "shape_or" "shape_not" "shape_atom")))))

(defun shexc-shexj--and-or-operand (ctx node same-type-names value-type)
  "Compile NODE -- a shape_and/shape_or `left'/`right' field -- as one or
more operands to splice into the enclosing chain.

Flattening (`A AND B AND C' -> one ShapeAnd with 3 shapeExprs, not
nested 2-element ones) must be decided from NODE's *grammar* type, not
blindly from the shape of its compiled value: an explicitly
parenthesized `A AND (B AND C)' has a `shape_atom' (the parens) as its
right operand, whose compiled value also happens to be a ShapeAnd --
but it must stay nested as one operand, not get flattened into the
outer chain, since the parens were semantically meaningful.

Two cases *do* splice: (1) NODE's own node type is again
`shape_and'/`shape_or' (continuing the same left-recursive,
unparenthesized operator chain) -- splice its already-flattened
shapeExprs list directly; (2) NODE is a juxtaposition-form shape_atom
(`IRI {...}', a non-parenthesized implicit-AND that this compiler
itself synthesizes as a 2-element ShapeAnd) sitting next to an explicit
same-operator chain -- confirmed empirically (`BNODE {...} AND CLOSED
{...}' compiles to one flat 3-element ShapeAnd, not a 2-element ShapeAnd
nesting a 2-element ShapeAnd) -- so its synthesized wrapper is
transparent here too, exactly like the unparenthesized chain case."
  (cond
   ((member (treesit-node-type node) same-type-names)
    (plist-get (shexc-shexj--compile-shape-expr ctx node) :shapeExprs))
   ((and (member (treesit-node-type node) '("shape_atom" "inline_shape_atom"))
         (not (shexc-shexj--paren-shape-atom-p node)))
    (let ((compiled (shexc-shexj--compile-shape-expr-required ctx node)))
      (if (and (consp compiled) (equal (plist-get compiled :type) value-type))
          (plist-get compiled :shapeExprs)
        (list compiled))))
   (t (list (shexc-shexj--compile-shape-expr-required ctx node)))))

(defun shexc-shexj--compile-shape-expr-required (ctx node)
  "Like `shexc-shexj--compile-shape-expr', but never nil: a bare `.'
operand (e.g. `NOT .') compiles to the trivial always-matching empty
Shape rather than to nil, since every grammar position that can hold a
*required* nested shape_expr field must produce a real ShExJ value --
only TripleConstraint's optional valueExpr is allowed to omit it."
  (or (shexc-shexj--compile-shape-expr ctx node) (list :type "Shape")))

(defun shexc-shexj--compile-shape-expr (ctx node)
  "Compile a shape_or/shape_and/shape_not/shape_atom node (or its inline_
variant) to a value-tree shapeExpr -- an object, or a bare IRI/blank-node
string when NODE is just a shape_ref."
  (pcase (treesit-node-type node)
    ((or "shape_or" "inline_shape_or")
     (list :type "ShapeOr"
           :shapeExprs (append (shexc-shexj--and-or-operand ctx (treesit-node-child-by-field-name node "left") '("shape_or" "inline_shape_or") "ShapeOr")
                                (shexc-shexj--and-or-operand ctx (treesit-node-child-by-field-name node "right") '("shape_or" "inline_shape_or") "ShapeOr"))))
    ((or "shape_and" "inline_shape_and")
     (list :type "ShapeAnd"
           :shapeExprs (append (shexc-shexj--and-or-operand ctx (treesit-node-child-by-field-name node "left") '("shape_and" "inline_shape_and") "ShapeAnd")
                                (shexc-shexj--and-or-operand ctx (treesit-node-child-by-field-name node "right") '("shape_and" "inline_shape_and") "ShapeAnd"))))
    ((or "shape_not" "inline_shape_not")
     (list :type "ShapeNot"
           :shapeExpr (shexc-shexj--compile-shape-expr-required ctx (treesit-node-child-by-field-name node "shape_expr"))))
    ((or "shape_atom" "inline_shape_atom")
     (shexc-shexj--compile-shape-atom ctx node))
    ("shape_ref" (shexc-shexj--iri-node-text ctx (treesit-node-child-by-field-name node "label")))
    ("shape_definition" (shexc-shexj--compile-shape-definition ctx node))
    ("inline_shape_definition" (shexc-shexj--compile-inline-shape-definition ctx node))
    (_ (error "shexc-shexj: unexpected shape_expr node `%s'" (treesit-node-type node)))))

(defun shexc-shexj--compile-shape-atom (ctx node)
  (let* ((is-dot (cl-some (lambda (c) (equal (treesit-node-type c) "shape_any")) (shexc-shexj--named-children node)))
         (nc (unless is-dot (shexc-shexj--node-constraint-from-atom ctx node)))
         (shape-expr-node (unless is-dot (treesit-node-child-by-field-name node "shape_expr")))
         (shape-expr (when shape-expr-node (shexc-shexj--compile-shape-expr ctx shape-expr-node))))
    (cond
     (is-dot nil) ; `.' -- caller (triple_constraint) omits :valueExpr entirely
     ((and nc shape-expr)
      ;; juxtaposition, e.g. `IRI @<Shape>' or `@<Shape> MinLength 3' --
      ;; shorthand for their conjunction.
      (list :type "ShapeAnd" :shapeExprs (list (append (list :type "NodeConstraint") nc) shape-expr)))
     (nc (append (list :type "NodeConstraint") nc))
     ;; `shape-expr' also covers the parenthesized `( shape_expr )' form --
     ;; its grammar alternative is `field('shape_expr', $._shape_expr)' too,
     ;; and never co-occurs with `nc' (it's a separate choice branch).
     (shape-expr shape-expr)
     (t nil))))

(defun shexc-shexj--compile-extensions-and-modifiers (ctx inline-shape-def-node)
  "Return (EXTENDS . (EXTRA . CLOSED)) for the modifiers attached directly
to INLINE-SHAPE-DEF-NODE (an inline_shape_definition)."
  (let (extends extra closed)
    (dolist (c (shexc-shexj--named-children inline-shape-def-node))
      (pcase (treesit-node-type c)
        ("extension"
         (let ((target (treesit-node-child-by-field-name c "shape_expr")))
           (push (shexc-shexj--compile-shape-expr ctx target) extends)))
        ("extra_property_set"
         (dolist (p (treesit-filter-child c (lambda (x) (equal (treesit-node-type x) "predicate"))))
           (push (shexc-shexj--compile-predicate ctx p) extra)))
        ("kw_closed" (setq closed t))))
    (list (nreverse extends) (nreverse extra) closed)))

(defun shexc-shexj--compile-inline-shape-definition (ctx node)
  (cl-destructuring-bind (extends extra closed) (shexc-shexj--compile-extensions-and-modifiers ctx node)
    (let* ((expr-node (treesit-node-child-by-field-name node "expression"))
           (expr (when expr-node (shexc-shexj--compile-triple-expression ctx expr-node))))
      (append (list :type "Shape")
              (when extends (list :extends extends))
              (when extra (list :extra extra))
              (when closed (list :closed t))
              (when expr (list :expression expr))))))

(defun shexc-shexj--compile-shape-definition (ctx node)
  "NODE is a `shape_definition' (inline_shape_definition + annotations +
semantic_actions, at the top of a shape_expr_decl)."
  (let* ((inline (treesit-node-child node 0)) ; first child is always inline_shape_definition
         (shape (shexc-shexj--compile-inline-shape-definition ctx inline))
         (annots (shexc-shexj--compile-annotations ctx node))
         (acts (shexc-shexj--compile-semantic-actions ctx node)))
    (append shape
            (when annots (list :annotations annots))
            (when acts (list :semActs acts)))))

;; ---------------------------------------------------------------------
;; Triple expressions
;; ---------------------------------------------------------------------

(defun shexc-shexj--compile-cardinality (node)
  (when node
    (let ((c (treesit-node-child node 0)))
      (pcase (treesit-node-type c)
        ("card_star" (list :min 0 :max -1))
        ("card_plus" (list :min 1 :max -1))
        ("card_opt" (list :min 0 :max 1))
        ("repeat_range" (shexc-shexj--compile-repeat-range c))))))

(defun shexc-shexj--compile-repeat-range (node)
  (let* ((text (treesit-node-text node t)) ; "{m,n}" / "{m,}" / "{m,*}" / "{m}"
         (body (substring text 1 (1- (length text))))
         (parts (split-string body ",")))
    (if (= (length parts) 1)
        (list :min (string-to-number (car parts)) :max (string-to-number (car parts)))
      (let ((min (string-to-number (car parts)))
            (max-text (string-trim (cadr parts))))
        (list :min min :max (if (or (string= max-text "") (string= max-text "*")) -1 (string-to-number max-text)))))))

(defun shexc-shexj--compile-annotations (ctx node)
  (mapcar (lambda (a)
            (let* ((pred (shexc-shexj--compile-predicate ctx (treesit-node-child-by-field-name a "predicate")))
                   (obj-node (treesit-node-child-by-field-name a "object"))
                   (obj (if (equal (treesit-node-type obj-node) "literal")
                            (shexc-shexj--compile-literal ctx obj-node)
                          (shexc-shexj--iri-node-text ctx obj-node))))
              (list :type "Annotation" :predicate pred :object obj)))
          (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "annotation")))))

(defun shexc-shexj--compile-semantic-actions (ctx node)
  "Semantic actions trailing NODE.  Usually wrapped in one
`semantic_actions' node (the normal case for a triple_constraint/
bracketed_triple_expr's own trailing `%act{}...'), but empirically
(`tree-sitter parse' on kitchenSink.shex's `BNODE %ex:foo{...}
%ex:bar{...}' line) `code_decl' nodes can also appear as direct,
unwrapped children of a `constraint'-fielded node_constraint -- handle
both shapes."
  (let ((sa (cl-find-if (lambda (c) (equal (treesit-node-type c) "semantic_actions")) (shexc-shexj--named-children node)))
        (bare (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "code_decl")))))
    (append (when sa (mapcar (lambda (cd) (shexc-shexj--compile-code-decl ctx cd))
                              (shexc-shexj--collect-code-decls sa)))
            (mapcar (lambda (cd) (shexc-shexj--compile-code-decl ctx cd)) bare))))

(defun shexc-shexj--collect-code-decls (node)
  "Flatten the left-recursive start_actions/semantic_actions shape into
an ordered list of code_decl nodes."
  (apply #'append
         (mapcar (lambda (c)
                   (pcase (treesit-node-type c)
                     ("code_decl" (list c))
                     ((or "start_actions" "semantic_actions") (shexc-shexj--collect-code-decls c))))
                 (shexc-shexj--named-children node))))

(defun shexc-shexj--compile-code-decl (ctx node)
  (let* ((name (shexc-shexj--iri-node-text ctx (treesit-node-child-by-field-name node "name")))
         (code-node (treesit-node-child-by-field-name node "code")))
    (append (list :type "SemAct" :name name)
            (when (equal (treesit-node-type code-node) "code")
              (let ((text (treesit-node-text code-node t)))
                (list :code (shexc-shexj--unescape (substring text 1 (- (length text) 2)) '((?% . "%") (?\\ . "\\")))))))))

(defun shexc-shexj--compile-include (ctx node)
  (shexc-shexj--iri-node-text ctx (treesit-node-child-by-field-name node "label")))

(defun shexc-shexj--gather-trailing-annotations (ctx atom-node fn)
  "Apply FN (`shexc-shexj--compile-annotations' or
`-compile-semantic-actions') to every node_constraint child of
ATOM-NODE and concatenate the results.

Empirically (`tree-sitter parse' on kitchenSink.shex's `ex:state [...]
// rdfs:label \"State\"// rdfs:description \"...\"' line), `// pred obj'
trailing a value_expr that's a bare NodeConstraint parses as a *second*,
unfielded `node_constraint' sibling nested inside the value_expr's
shape_atom/inline_shape_atom -- not as a direct child of the enclosing
triple_constraint, and not merged into the first (fielded) node_
constraint either.  ShExJ has no NodeConstraint-level annotations/
semActs -- they always belong on the owning TripleConstraint -- so the
caller must hoist whatever this finds up to that level."
  (when atom-node
    (apply #'append
           (mapcar (lambda (c) (funcall fn ctx c))
                   (treesit-filter-child atom-node (lambda (c) (equal (treesit-node-type c) "node_constraint")))))))

(defun shexc-shexj--compile-triple-constraint (ctx node)
  (let* ((inverse (treesit-node-child-by-field-name node "inverse"))
         (predicate (shexc-shexj--compile-predicate ctx (treesit-node-child-by-field-name node "predicate")))
         (value-expr-node (treesit-node-child-by-field-name node "value_expr"))
         (atom-node (and value-expr-node (treesit-node-child value-expr-node 0)))
         (value-expr (shexc-shexj--compile-shape-expr ctx atom-node))
         (card (shexc-shexj--compile-cardinality (treesit-node-child-by-field-name node "cardinality")))
         (annots (append (shexc-shexj--compile-annotations ctx node)
                          (shexc-shexj--gather-trailing-annotations ctx atom-node #'shexc-shexj--compile-annotations)))
         (acts (append (shexc-shexj--compile-semantic-actions ctx node)
                        (shexc-shexj--gather-trailing-annotations ctx atom-node #'shexc-shexj--compile-semantic-actions))))
    (append (list :type "TripleConstraint")
            (when inverse (list :inverse t))
            (list :predicate predicate)
            (when value-expr (list :valueExpr value-expr))
            card
            (when annots (list :annotations annots))
            (when acts (list :semActs acts)))))

(defun shexc-shexj--plist-merge-list-prop (plist key new-items)
  "Append NEW-ITEMS to PLIST's existing KEY list value (or set it fresh)
rather than risking a duplicate KEY entry from blindly `append'ing a
second `(KEY . items)' pair -- needed because a doubly-bracketed
singleton triple_expr, e.g. `((<p> .) %act1{}) %act2{} %act3{}', calls
`shexc-shexj--compile-bracketed-triple-expr' once per bracket level on
the *same* inner TripleConstraint, and each level's semActs/annotations
must accumulate onto that one object, confirmed empirically against
`openopen1dotcloseCode1closeCode3.shex'."
  (if (null new-items) plist
    (plist-put plist key (append (plist-get plist key) new-items))))

(defun shexc-shexj--compile-bracketed-triple-expr (ctx node)
  (let* ((inner (shexc-shexj--compile-triple-expression ctx (treesit-node-child-by-field-name node "expression")))
         (card (shexc-shexj--compile-cardinality (treesit-node-child-by-field-name node "cardinality")))
         (annots (shexc-shexj--compile-annotations ctx node))
         (acts (shexc-shexj--compile-semantic-actions ctx node)))
    ;; ShExJ has no way to attach cardinality/annotations/semActs to a
    ;; bare TripleExprRef (an `include' reference) -- `(&<ref>)+' is
    ;; syntactically legal but semantically unrepresentable, so the extra
    ;; attributes are best-effort dropped rather than erroring.
    (if (stringp inner)
        inner
      (let ((result (append inner card)))
        (setq result (shexc-shexj--plist-merge-list-prop result :annotations annots))
        (setq result (shexc-shexj--plist-merge-list-prop result :semActs acts))
        result))))

(defun shexc-shexj--compile-unary-triple-expr-list (ctx group-node)
  "Walk GROUP-NODE's (group_triple_expr) children in document order,
pairing each `$<id>' (field `id') with the very next `element' child."
  (let (result pending-id)
    (dolist (c (shexc-shexj--named-children group-node))
      (let ((field (treesit-node-field-name c)))
        (pcase field
          ("id" (setq pending-id (shexc-shexj--iri-node-text ctx c)))
          ("element"
           (let ((compiled
                  (pcase (treesit-node-type c)
                    ("triple_constraint" (shexc-shexj--compile-triple-constraint ctx c))
                    ("bracketed_triple_expr" (shexc-shexj--compile-bracketed-triple-expr ctx c))
                    ("include" (shexc-shexj--compile-include ctx c)))))
             (when (and pending-id (consp compiled))
               (setq compiled (plist-put compiled :id pending-id)))
             (setq pending-id nil)
             (push compiled result))))))
    (nreverse result)))

(defun shexc-shexj--compile-group-triple-expr (ctx node)
  (let ((elements (shexc-shexj--compile-unary-triple-expr-list ctx node)))
    (if (= (length elements) 1)
        (car elements)
      (list :type "EachOf" :expressions elements))))

(defun shexc-shexj--compile-one-of (ctx node)
  (list :type "OneOf"
        :expressions (mapcar (lambda (d) (shexc-shexj--compile-group-triple-expr ctx d))
                              (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "group_triple_expr"))))))

(defun shexc-shexj--compile-triple-expression (ctx node)
  "NODE is a `triple_expression' wrapper around group_triple_expr|one_of."
  (let ((child (treesit-node-child node 0)))
    (pcase (treesit-node-type child)
      ("group_triple_expr" (shexc-shexj--compile-group-triple-expr ctx child))
      ("one_of" (shexc-shexj--compile-one-of ctx child)))))

;; ---------------------------------------------------------------------
;; Top-level: shape_expr_decl, start, schema
;; ---------------------------------------------------------------------

(defun shexc-shexj--compile-restriction (ctx node)
  (shexc-shexj--compile-shape-expr ctx (treesit-node-child-by-field-name node "shape")))

(defun shexc-shexj--compile-shape-expr-decl (ctx node)
  (let* ((id (shexc-shexj--iri-node-text ctx (treesit-node-child-by-field-name node "label")))
         (abstract (cl-some (lambda (c) (equal (treesit-node-type c) "kw_abstract")) (shexc-shexj--named-children node)))
         (restricts (mapcar (lambda (r) (shexc-shexj--compile-restriction ctx r))
                             (treesit-filter-child node (lambda (c) (equal (treesit-node-type c) "restriction")))))
         (shape-expr-node (treesit-node-child-by-field-name node "shape_expr"))
         (shape-expr (unless (equal (treesit-node-type shape-expr-node) "kw_external")
                       (shexc-shexj--compile-shape-expr ctx shape-expr-node)))
         (external (equal (treesit-node-type shape-expr-node) "kw_external")))
    ;; RESTRICTS targets fold into the same :extends list a Shape would
    ;; carry; when the shape_expr itself is a bare Shape, merge directly,
    ;; otherwise wrap in a ShapeAnd alongside it.
    (when restricts
      (setq shape-expr
            (cond
             ((null shape-expr) (list :type "Shape" :extends restricts))
             ((and (consp shape-expr) (equal (plist-get shape-expr :type) "Shape"))
              (plist-put shape-expr :extends (append restricts (plist-get shape-expr :extends))))
             (t (list :type "ShapeAnd" :shapeExprs (cons (list :type "Shape" :extends restricts) (list shape-expr)))))))
    (append (list :type "ShapeDecl" :id id)
            (when abstract (list :abstract t))
            (list :shapeExpr (cond (external (list :type "ShapeExternal"))
                                    (shape-expr shape-expr)
                                    ;; bare `.': the trivial always-matching
                                    ;; shape. `shapeExpr' is required by
                                    ;; ShExJ, so this can't simply be omitted.
                                    (t (list :type "Shape")))))))

(defun shexc-shexj--compile-start (ctx node)
  (shexc-shexj--compile-shape-expr-required ctx (treesit-node-child-by-field-name node "shape_expr")))

(defun shexc-shexj--apply-directive-or-collect (ctx node acc)
  "Update CTX for a directive NODE, or push a compiled shape/start/import
onto the appropriate slot of ACC (a plist of :shapes/:start/:startActs/
:imports accumulator lists, each in reverse document order)."
  (pcase (treesit-node-type node)
    ("base_decl" (shexc-shexj--apply-base ctx node) acc)
    ("prefix_decl" (shexc-shexj--apply-prefix ctx node) acc)
    ("import_decl" (plist-put acc :imports (cons (shexc-shexj--compile-import ctx node) (plist-get acc :imports))))
    ("start" (plist-put acc :start (shexc-shexj--compile-start ctx node)))
    ("start_actions"
     (plist-put acc :startActs (mapcar (lambda (cd) (shexc-shexj--compile-code-decl ctx cd))
                                        (shexc-shexj--collect-code-decls node))))
    ("shape_expr_decl" (plist-put acc :shapes (cons (shexc-shexj--compile-shape-expr-decl ctx node) (plist-get acc :shapes))))
    (_ acc)))

(defun shexc-shexj--compile-schema-from-root (root ctx &optional stop-before)
  "Walk ROOT's (shex_doc) children, threading CTX, collecting a Schema
value-tree.  If STOP-BEFORE is non-nil, stop (without compiling it)
right before that child node is reached -- used by
`shexc-shexj-compile-node' to build correct prefix/base context for a
single target declaration without compiling the rest of the schema."
  (let (acc)
    (catch 'stop
      (dolist (c (shexc-shexj--named-children root))
        (if (and stop-before (treesit-node-eq c stop-before))
            (throw 'stop nil)
          (setq acc (shexc-shexj--apply-directive-or-collect ctx c acc)))))
    (append (list :context shexc-shexj--context-iri :type "Schema")
            (when (plist-get acc :imports) (list :imports (nreverse (plist-get acc :imports))))
            (when (plist-get acc :startActs) (list :startActs (plist-get acc :startActs)))
            (when (plist-get acc :start) (list :start (plist-get acc :start)))
            (when (plist-get acc :shapes) (list :shapes (nreverse (plist-get acc :shapes)))))))

;;;###autoload
(defun shexc-shexj--check-no-errors (root)
  "Signal a clear error rather than silently compiling a truncated/wrong
value-tree when ROOT's parse tree contains an ERROR node -- e.g. a raw
NUL byte in the buffer, which breaks tree-sitter-shexc's C lexer."
  (when (treesit-search-subtree root "ERROR" nil t)
    (error "shexc-shexj: buffer has a parse error (ERROR node) -- cannot compile to ShExJ")))

(defun shexc-shexj-compile-buffer ()
  "Compile the current buffer's ShExC parse tree to a ShExJ value-tree."
  (let ((root (treesit-buffer-root-node)))
    (shexc-shexj--check-no-errors root)
    (shexc-shexj--compile-schema-from-root root (shexc-shexj--make-ctx))))

;;;###autoload
(defun shexc-shexj-compile-node (node)
  "Compile NODE (a `shape_expr_decl') to a standalone single-shape Schema
value-tree, having first scanned the whole buffer for the prefix/base
context active at NODE's position."
  (let ((ctx (shexc-shexj--make-ctx))
        (root (treesit-buffer-root-node)))
    (shexc-shexj--check-no-errors root)
    (shexc-shexj--compile-schema-from-root root ctx node)
    (list :context shexc-shexj--context-iri :type "Schema"
          :shapes (list (shexc-shexj--compile-shape-expr-decl ctx node)))))

;; ---------------------------------------------------------------------
;; Decompiler: value-tree -> ShExC text.  Far simpler than the compiler
;; -- no ambiguity to resolve, just a `:type'-dispatched structural walk.
;; Always emits explicit AND/OR (never implicit juxtaposition), and
;; emits "reasonably line-broken" text relying on the caller to
;; `indent-region' the result afterward -- see plan notes.  By default
;; also always emits full `<IRI>' forms (never reconstructs
;; `prefix:local'/BASE-relative shorthand, since a value-tree has no
;; prefix table of its own) -- but see
;; `shexc-shexj-decompile-iri-shortener' for callers (e.g.
;; `shexc-ts-mode-convert.el') that have one to offer.
;; ---------------------------------------------------------------------

(defvar shexc-shexj-decompile-iri-shortener nil
  "When non-nil, a function called as (FUNC IRI) for every absolute IRI
`shexc-shexj-decompile' is about to emit in `<IRI>' form.  Returning a
non-nil string (e.g. \"ex:Foo\" or \"<#Foo>\", a complete replacement
token including any needed `<>') uses that instead; returning nil
falls back to the default full `<IRI>' form.  Let-bind this -- never
`setq' it -- around a `shexc-shexj-decompile' call to get PREFIX/BASE-
aware shorthand; nil by default, so plain callers are unaffected.")

(defun shexc-shexj--decompile-iri (iri)
  (or (and shexc-shexj-decompile-iri-shortener (funcall shexc-shexj-decompile-iri-shortener iri))
      (concat "<" iri ">")))

(defun shexc-shexj--decompile-label (iri-or-bnode)
  (if (string-prefix-p "_:" iri-or-bnode) iri-or-bnode (shexc-shexj--decompile-iri iri-or-bnode)))

(defun shexc-shexj--decompile-ref (iri-or-bnode) (concat "@" (shexc-shexj--decompile-label iri-or-bnode)))

(defun shexc-shexj--decompile-predicate (iri)
  (if (string= iri shexc-shexj--rdf-type-iri) "a" (shexc-shexj--decompile-iri iri)))

(defun shexc-shexj--escape-string (text)
  (replace-regexp-in-string
   "[\"\\\n\r\t]"
   (lambda (m) (pcase m ("\"" "\\\"") ("\\" "\\\\") ("\n" "\\n") ("\r" "\\r") ("\t" "\\t")))
   text t t))

(defun shexc-shexj--decompile-quoted-string (text) (concat "\"" (shexc-shexj--escape-string text) "\""))

(defun shexc-shexj--decompile-literal-value (lit)
  "LIT is an ObjectLiteral plist (:value [:type] [:language])."
  (let ((value (plist-get lit :value)) (type (plist-get lit :type)) (lang (plist-get lit :language)))
    (cond
     (lang (format "%s@%s" (shexc-shexj--decompile-quoted-string value) lang))
     (type (format "%s^^%s" (shexc-shexj--decompile-quoted-string value) (shexc-shexj--decompile-iri type)))
     (t (shexc-shexj--decompile-quoted-string value)))))

(defun shexc-shexj--escape-code (text)
  (replace-regexp-in-string "[%\\]" (lambda (m) (concat "\\" m)) text t t))

(defun shexc-shexj--decompile-sem-act (act)
  (let ((name (shexc-shexj--decompile-iri (plist-get act :name)))
        (code (plist-get act :code)))
    (if code (format "%%%s{%s%%}" name (shexc-shexj--escape-code code)) (format "%%%s%%" name))))

(defun shexc-shexj--decompile-sem-acts (acts)
  (mapconcat (lambda (a) (concat " " (shexc-shexj--decompile-sem-act a))) acts ""))

(defun shexc-shexj--decompile-annotations (annots)
  (mapconcat (lambda (a)
               (let* ((obj (plist-get a :object))
                      (obj-text (if (stringp obj) (shexc-shexj--decompile-iri obj) (shexc-shexj--decompile-literal-value obj))))
                 (format " // %s %s" (shexc-shexj--decompile-predicate (plist-get a :predicate)) obj-text)))
             annots ""))

(defun shexc-shexj--decompile-cardinality (te)
  (let ((min (plist-get te :min)) (max (plist-get te :max)))
    (cond
     ((null min) "")
     ((and (= min 0) (= max -1)) " *")
     ((and (= min 1) (= max -1)) " +")
     ((and (= min 0) (= max 1)) " ?")
     ((= min max) (format " {%d}" min))
     ((= max -1) (format " {%d,}" min))
     (t (format " {%d,%d}" min max)))))

;; -- value sets ---------------------------------------------------------

(defun shexc-shexj--wildcard-p (x) (and (consp x) (equal (plist-get x :type) "Wildcard")))

(defun shexc-shexj--decompile-iri-exclusion (e)
  (if (and (consp e) (equal (plist-get e :type) "IriStem"))
      (concat "-" (shexc-shexj--decompile-iri (plist-get e :stem)) "~")
    (concat "-" (shexc-shexj--decompile-iri e))))

(defun shexc-shexj--decompile-literal-exclusion (e)
  (if (and (consp e) (equal (plist-get e :type) "LiteralStem"))
      (concat "-" (shexc-shexj--decompile-quoted-string (plist-get e :stem)) "~")
    (concat "-" (shexc-shexj--decompile-quoted-string e))))

(defun shexc-shexj--decompile-language-exclusion (e)
  (if (and (consp e) (equal (plist-get e :type) "LanguageStem"))
      (concat "-@" (plist-get e :stem) "~")
    (concat "-@" e)))

(defun shexc-shexj--decompile-value-set-entry (v)
  (if (stringp v)
      (shexc-shexj--decompile-iri v)
    (pcase (plist-get v :type)
      ("IriStem" (concat (shexc-shexj--decompile-iri (plist-get v :stem)) "~"))
      ("IriStemRange"
       (if (shexc-shexj--wildcard-p (plist-get v :stem))
           (concat ". " (mapconcat #'shexc-shexj--decompile-iri-exclusion (plist-get v :exclusions) " "))
         (concat (shexc-shexj--decompile-iri (plist-get v :stem)) "~"
                 (mapconcat #'shexc-shexj--decompile-iri-exclusion (plist-get v :exclusions) " "))))
      ("LiteralStem" (concat (shexc-shexj--decompile-quoted-string (plist-get v :stem)) "~"))
      ("LiteralStemRange"
       (if (shexc-shexj--wildcard-p (plist-get v :stem))
           (concat ". " (mapconcat #'shexc-shexj--decompile-literal-exclusion (plist-get v :exclusions) " "))
         (concat (shexc-shexj--decompile-quoted-string (plist-get v :stem)) "~"
                 (mapconcat #'shexc-shexj--decompile-literal-exclusion (plist-get v :exclusions) " "))))
      ("Language" (concat "@" (plist-get v :languageTag)))
      ("LanguageStem"
       (if (shexc-shexj--wildcard-p (plist-get v :stem)) "@~" (concat "@" (plist-get v :stem) "~")))
      ("LanguageStemRange"
       (if (shexc-shexj--wildcard-p (plist-get v :stem))
           ;; Round-trips through the `. -@exclusion...' dot-form, not
           ;; `@~ ...': the latter reparses (via
           ;; `shexc-shexj--compile-language-stem-range') to an
           ;; empty-string stem, not Wildcard -- only the dot-exclusions
           ;; form (`shexc-shexj--compile-dot-exclusions') produces a real
           ;; Wildcard stem.
           (concat ". " (mapconcat #'shexc-shexj--decompile-language-exclusion (plist-get v :exclusions) " "))
         (concat "@" (plist-get v :stem) "~"
                 (mapconcat #'shexc-shexj--decompile-language-exclusion (plist-get v :exclusions) " "))))
      (_ (shexc-shexj--decompile-literal-value v)))))

(defun shexc-shexj--decompile-value-set (values)
  (concat "[" (mapconcat #'shexc-shexj--decompile-value-set-entry values " ") "]"))

;; -- node constraints -----------------------------------------------------

(defconst shexc-shexj--facet-keywords
  '((:length . "LENGTH") (:minlength . "MINLENGTH") (:maxlength . "MAXLENGTH")
    (:mininclusive . "MININCLUSIVE") (:minexclusive . "MINEXCLUSIVE")
    (:maxinclusive . "MAXINCLUSIVE") (:maxexclusive . "MAXEXCLUSIVE")
    (:totaldigits . "TOTALDIGITS") (:fractiondigits . "FRACTIONDIGITS")))

(defun shexc-shexj--decompile-facets (nc)
  (let (parts)
    (dolist (kv shexc-shexj--facet-keywords)
      (let ((v (plist-get nc (car kv))))
        (when v (push (format "%s %s" (cdr kv) v) parts))))
    (when (plist-get nc :pattern)
      (push (format "/%s/%s" (shexc-shexj--escape-regexp-body (plist-get nc :pattern)) (or (plist-get nc :flags) ""))
            parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun shexc-shexj--decompile-node-constraint (nc)
  (let* ((kind (plist-get nc :nodeKind))
         (datatype (plist-get nc :datatype))
         (values (plist-get nc :values))
         (head (cond
                ((equal kind "iri") "IRI")
                ((equal kind "bnode") "BNODE")
                ((equal kind "nonliteral") "NONLITERAL")
                ((equal kind "literal") "LITERAL")
                (datatype (shexc-shexj--decompile-iri datatype))
                (values (shexc-shexj--decompile-value-set values))
                (t nil)))
         (facets (shexc-shexj--decompile-facets nc)))
    (string-trim (mapconcat #'identity (delq nil (list head facets)) " "))))

;; -- shapes and shape expressions ------------------------------------------

(defun shexc-shexj--shape-expr-prec (se)
  (if (stringp se) 4
    (pcase (plist-get se :type) ("ShapeOr" 1) ("ShapeAnd" 2) ("ShapeNot" 3) (_ 4))))

(defun shexc-shexj--decompile-operand (se min-prec &optional same-type)
  "Decompile SE as an operand requiring at least MIN-PREC.  When
SAME-TYPE is given and SE's own :type is identical to it, *always*
parenthesize regardless of precedence: the compiler only ever produces
a ShapeAnd nested directly inside another ShapeAnd's (or ShapeOr inside
ShapeOr's) :shapeExprs when the source had explicit parens (unparen-
thesized chains are flattened into one level by
`shexc-shexj--and-or-operand') -- so bare same-precedence juxtaposition
here would silently flatten on the next recompile and fail to
round-trip."
  (let ((text (shexc-shexj--decompile-shape-expr-ref se)))
    (if (or (< (shexc-shexj--shape-expr-prec se) min-prec)
            (and same-type (consp se) (equal (plist-get se :type) same-type)))
        (concat "(" text ")")
      text)))

(defun shexc-shexj--decompile-shape-expr-ref (x)
  (if (stringp x) (shexc-shexj--decompile-ref x) (shexc-shexj--decompile-shape-expr x)))

(defun shexc-shexj--decompile-shape-expr (se)
  (pcase (plist-get se :type)
    ("ShapeAnd" (mapconcat (lambda (x) (shexc-shexj--decompile-operand x 2 "ShapeAnd")) (plist-get se :shapeExprs) " AND "))
    ("ShapeOr" (mapconcat (lambda (x) (shexc-shexj--decompile-operand x 1 "ShapeOr")) (plist-get se :shapeExprs) " OR "))
    ("ShapeNot" (concat "NOT " (shexc-shexj--decompile-operand (plist-get se :shapeExpr) 3)))
    ("NodeConstraint" (shexc-shexj--decompile-node-constraint se))
    ("Shape" (shexc-shexj--decompile-shape se))
    ("ShapeExternal" "EXTERNAL")
    (_ (error "shexc-shexj: cannot decompile shapeExpr of type `%s'" (plist-get se :type)))))

(defun shexc-shexj--decompile-shape (shape)
  (let ((extends (plist-get shape :extends))
        (extra (plist-get shape :extra))
        (closed (plist-get shape :closed))
        (expr (plist-get shape :expression)))
    (concat
     (mapconcat (lambda (e) (concat "EXTENDS " (shexc-shexj--decompile-ref e) " ")) extends "")
     (if extra (concat "EXTRA " (mapconcat #'shexc-shexj--decompile-iri extra " ") " ") "")
     (if closed "CLOSED " "")
     "{\n"
     (if expr (concat "  " (shexc-shexj--decompile-top-triple-expr expr) "\n") "")
     "}"
     (shexc-shexj--decompile-annotations (plist-get shape :annotations))
     (shexc-shexj--decompile-sem-acts (plist-get shape :semActs)))))

;; -- triple expressions ------------------------------------------------

(defun shexc-shexj--group-sep (te) (if (equal (plist-get te :type) "OneOf") " | " " ; "))

(defun shexc-shexj--decompile-group-inner (te)
  "Just TE's (EachOf/OneOf) `e1 SEP e2 ...' text, no wrapping/decoration."
  (mapconcat #'shexc-shexj--decompile-triple-expr (plist-get te :expressions) (shexc-shexj--group-sep te)))

(defun shexc-shexj--decompile-bracketed (te)
  "Always wraps TE (an EachOf/OneOf) in `( ... )', attaching its own
cardinality/annotations/semActs/id.  This form -- a `bracketed_triple_
expr' -- is the *only* way an EachOf/OneOf can appear as one element of
an enclosing group; a bare nested EachOf/OneOf is not valid ShExC."
  (concat (if (plist-get te :id) (concat "$" (shexc-shexj--decompile-label (plist-get te :id)) " ") "")
          "(" (shexc-shexj--decompile-group-inner te) ")"
          (shexc-shexj--decompile-cardinality te)
          (shexc-shexj--decompile-annotations (plist-get te :annotations))
          (shexc-shexj--decompile-sem-acts (plist-get te :semActs))))

(defun shexc-shexj--decompile-triple-expr (te)
  "TE as an ELEMENT of an enclosing group -- EachOf/OneOf always
parenthesized here; see `shexc-shexj--decompile-top-triple-expr' for the
one context (a Shape's bare top-level :expression) where that's not
required."
  (if (stringp te)
      (concat "&" (shexc-shexj--decompile-label te))
    (pcase (plist-get te :type)
      ("TripleConstraint" (shexc-shexj--decompile-triple-constraint te))
      ((or "EachOf" "OneOf") (shexc-shexj--decompile-bracketed te)))))

(defun shexc-shexj--decompile-top-triple-expr (te)
  "TE as a Shape's top-level :expression: bare unless TE itself carries
cardinality/semActs/annotations/id (reachable only when the Shape's
sole top-level element happened to be a singleton bracketed_triple_expr)."
  (if (stringp te)
      (concat "&" (shexc-shexj--decompile-label te))
    (let ((has-extra (or (plist-get te :min) (plist-get te :semActs) (plist-get te :annotations) (plist-get te :id))))
      (pcase (plist-get te :type)
        ("TripleConstraint" (shexc-shexj--decompile-triple-constraint te))
        ((guard has-extra) (shexc-shexj--decompile-bracketed te))
        (_ (shexc-shexj--decompile-group-inner te))))))

(defun shexc-shexj--decompile-triple-constraint (tc)
  (concat
   (if (plist-get tc :id) (concat "$" (shexc-shexj--decompile-label (plist-get tc :id)) " ") "")
   (if (plist-get tc :inverse) "^" "")
   (shexc-shexj--decompile-predicate (plist-get tc :predicate))
   " "
   (let ((ve (plist-get tc :valueExpr))) (if ve (shexc-shexj--decompile-shape-expr-ref ve) "."))
   (shexc-shexj--decompile-cardinality tc)
   (shexc-shexj--decompile-annotations (plist-get tc :annotations))
   (shexc-shexj--decompile-sem-acts (plist-get tc :semActs))))

;; -- top level: ShapeDecl / Schema ---------------------------------------

(defun shexc-shexj--decompile-shape-decl (decl)
  (format "%s%s %s\n"
          (if (plist-get decl :abstract) "ABSTRACT " "")
          (shexc-shexj--decompile-label (plist-get decl :id))
          (shexc-shexj--decompile-shape-expr-ref (plist-get decl :shapeExpr))))

(defun shexc-shexj--decompile-schema (schema)
  (let ((start (plist-get schema :start))
        (start-acts (plist-get schema :startActs))
        (imports (plist-get schema :imports))
        (shapes (plist-get schema :shapes)))
    (concat
     (mapconcat (lambda (i) (concat "IMPORT " (shexc-shexj--decompile-iri i) "\n")) imports "")
     (if start-acts (concat (mapconcat #'shexc-shexj--decompile-sem-act start-acts "\n") "\n") "")
     (if start (concat "START = " (shexc-shexj--decompile-shape-expr-ref start) "\n\n") "")
     (mapconcat #'shexc-shexj--decompile-shape-decl shapes "\n"))))

;;;###autoload
(defun shexc-shexj-decompile (value-tree)
  "Decompile VALUE-TREE -- a Schema or a single ShapeDecl -- to ShExC text."
  (pcase (plist-get value-tree :type)
    ("Schema" (shexc-shexj--decompile-schema value-tree))
    ("ShapeDecl" (shexc-shexj--decompile-shape-decl value-tree))
    (_ (error "shexc-shexj-decompile: expected a Schema or ShapeDecl value-tree, got %S" value-tree))))

;; ---------------------------------------------------------------------
;; JSON adapter: value-tree <-> ShExJ JSON text.
;;
;; The value-tree is already plist/list-shaped exactly like
;; `json-parse-string's `:object-type 'plist :array-type 'list' output,
;; so this is mostly bookkeeping: `@context' uses a JSON key Emacs's
;; keyword-interning can't round-trip through a plain `:context' name,
;; so it gets an explicit override.  Everything else is structurally
;; isomorphic already: NodeConstraint facet values (`length',
;; `mininclusive', ...) and `min'/`max' cardinality are real Lisp
;; numbers (the compiler parses them via `string-to-number' -- ShExJ
;; normalizes facet lexical form, e.g. `MININCLUSIVE 4.5E0' compiles to
;; `"mininclusive": 4.5', confirmed empirically, unlike ObjectLiteral
;; values where exact source lexical form like "0E0" must round-trip
;; verbatim, so those stay strings); `:abstract'/`:closed'/`:inverse'
;; are the symbol `t' when present, never stored when false.  ShExJ's
;; ObjectLiteral is *never* collapsed to a bare scalar for typed/
;; numeric/boolean values -- only plain IRI/blank-node references are
;; ever bare JSON strings -- so the value-tree's "bare string vs.
;; object" shape needs no extra collapsing/expansion logic either.
;; ---------------------------------------------------------------------

(defconst shexc-shexj--json-key-overrides '((:context . "@context")))

(defun shexc-shexj--json-key-name (key)
  (or (cdr (assq key shexc-shexj--json-key-overrides)) (substring (symbol-name key) 1)))

(defun shexc-shexj--plist-pairs (plist)
  (let (pairs)
    (while plist (push (cons (car plist) (cadr plist)) pairs) (setq plist (cddr plist)))
    (nreverse pairs)))

(defun shexc-shexj--print-json (value indent)
  (cond
   ((eq value t) "true")
   ((numberp value) (number-to-string value))
   ((stringp value) (shexc-shexj--decompile-quoted-string value)) ; same escaping rules as ShExC strings
   ((and (consp value) (keywordp (car value))) (shexc-shexj--print-json-object value indent))
   ((null value) "[]")
   ((listp value) (shexc-shexj--print-json-array value indent))
   (t (error "shexc-shexj: cannot print %S as JSON" value))))

(defun shexc-shexj--print-json-object (plist indent)
  (let ((pairs (shexc-shexj--plist-pairs plist)) (inner (+ indent 2)))
    (if (null pairs) "{}"
      (concat "{\n"
              (mapconcat
               (lambda (kv)
                 (concat (make-string inner ?\s) (shexc-shexj--decompile-quoted-string (shexc-shexj--json-key-name (car kv)))
                         ": " (shexc-shexj--print-json (cdr kv) inner)))
               pairs ",\n")
              "\n" (make-string indent ?\s) "}"))))

(defun shexc-shexj--print-json-array (lst indent)
  (if (null lst) "[]"
    (let ((inner (+ indent 2)))
      (concat "[\n"
              (mapconcat (lambda (v) (concat (make-string inner ?\s) (shexc-shexj--print-json v inner))) lst ",\n")
              "\n" (make-string indent ?\s) "]"))))

;;;###autoload
(defun shexc-shexj-to-json (value-tree)
  "Pretty-print VALUE-TREE as ShExJ JSON text."
  (shexc-shexj--print-json value-tree 0))

(defun shexc-shexj--normalize-from-json (value)
  "Rename `:@context' to `:context', recursively over a freshly-
`json-parse-string'd value-tree."
  (cond
   ((and (consp value) (keywordp (car value)))
    (let (out (tail value))
      (while tail
        (let* ((k (car tail)) (v (cadr tail))
               (k2 (if (eq k :@context) :context k)))
          (setq out (append out (list k2 (shexc-shexj--normalize-from-json v)))))
        (setq tail (cddr tail)))
      out))
   ((listp value) (mapcar #'shexc-shexj--normalize-from-json value))
   (t value)))

;;;###autoload
(defun shexc-shexj-from-json (json-text)
  "Parse JSON-TEXT (ShExJ) into a value-tree."
  (shexc-shexj--normalize-from-json
   (json-parse-string json-text :object-type 'plist :array-type 'list)))

(provide 'shexc-shexj)
;;; shexc-shexj.el ends here
