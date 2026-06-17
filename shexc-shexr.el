;;; shexc-shexr.el --- value-tree <-> canonical ShExR Turtle  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Serializes a `shexc-shexj' value-tree (see shexc-shexj.el) to a fixed,
;; bespoke canonical Turtle shape -- NOT general Turtle, and not
;; byte-matched against shexSpec/shexTest's own .ttl fixtures (which vary
;; in formatting across fixtures -- evidence of multiple hands/tools, not
;; one disciplined serializer).  The matching narrow parser
;; (`shexc-shexr-parse', a later milestone) only ever needs to recognize
;; this exact shape, never general Turtle/N3.
;;
;; Canonical shape, fixed by construction (see `shexc-shexr--serialize-body'
;; and `shexc-shexr--serialize-value'):
;; - One prefix, `@prefix sx: <http://www.w3.org/ns/shex#> .'; every other
;;   IRI always full `<...>'.
;; - Every value-tree node with a non-nil `:id' is hoisted to its own
;;   top-level statement and referenced elsewhere by IRI/blank-node label;
;;   everything else inlines as `[ ... ]'.  The document root (a Schema)
;;   has no `:id' of its own and is always the anonymous `[]' top-level
;;   statement.
;; - `a sx:Type' first, then remaining properties in a fixed deterministic
;;   order: alphabetical by JSON key name, with the "big nested"
;;   properties (valueExpr/shapeExpr/expression/expressions/shapeExprs/
;;   shapes) sorted last.
;; - Every JSON-array-typed property becomes an RDF list `( ... )',
;;   *except* `extra', which is a comma-separated object list on one line
;;   (`sx:extra <p1>, <p2> ;') -- confirmed against every EXTRA fixture in
;;   shexSpec/shexTest.
;; - Literal value-set entries are `"text"^^<full-iri>'/`"text"@lang',
;;   except xsd:boolean-typed ones (and the structural abstract/closed/
;;   inverse booleans, which are always Lisp `t' in the value-tree), which
;;   use Turtle's native `true'/`false' keyword.
;; - Most string-valued properties are IRI/blank-node-label references
;;   (`predicate', `datatype', value-set bare-string entries, ...); a
;;   fixed few are plain Turtle string literals instead (`code', `pattern',
;;   `flags', `languageTag', `nodeKind'), and `stem'/`exclusions' switch
;;   between the two depending on the enclosing node's `:type' (Iri* are
;;   IRIs, Literal*/Language* are plain text).

;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst shexc-shexr--prefix-line
  "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n")

(defconst shexc-shexr--xsd-boolean-iri "http://www.w3.org/2001/XMLSchema#boolean")

(defconst shexc-shexr--last-sorted-keys
  '(:valueExpr :shapeExpr :expression :expressions :shapeExprs :shapes)
  "Properties holding the \"big nested\" sub-structure, always emitted
last within a node's property list -- not RDF-semantically significant,
purely a canonical-shape convention of this bespoke serializer, so the
narrow parser can rely on a predictable position.")

(defconst shexc-shexr--plain-text-keys
  '(:code :pattern :flags :languageTag :nodeKind)
  "Property keys whose string value is canonical plain text (a Turtle
string literal), never an IRI/blank-node-label reference.")

;; ---------------------------------------------------------------------
;; Plist utilities (kept local rather than reaching into shexc-shexj's
;; private helpers, to keep this file's only real dependency on
;; shexc-shexj being "the value-tree's shape", per the project plan).
;; ---------------------------------------------------------------------

(defun shexc-shexr--plist-keys (plist)
  (let (keys)
    (while plist (push (car plist) keys) (setq plist (cddr plist)))
    (nreverse keys)))

(defun shexc-shexr--literal-value-p (node)
  "Is NODE a value-set literal entry plist (`:value'/`:type'/`:language')
rather than a ShExJ typed object?  The two are otherwise indistinguishable
by shape alone -- both are plists -- since a literal entry's `:type' key
means \"XSD datatype IRI\" while a typed object's `:type' key means \"ShExJ
type tag\"; presence of `:value' is the only reliable discriminator (no
ShExJ structural type ever uses that key)."
  (and (consp node) (keywordp (car node)) (plist-member node :value)))

;; ---------------------------------------------------------------------
;; Hoisting: every node with an :id becomes its own top-level statement.
;; ---------------------------------------------------------------------

(defun shexc-shexr--collect-hoisted (node acc)
  "Depth-first walk of NODE, pushing onto ACC (and returning the updated
ACC) every plist sub-node carrying a non-nil `:id'."
  (cond
   ((shexc-shexr--literal-value-p node) acc)
   ((and (consp node) (keywordp (car node)))
    (when (plist-get node :id) (push node acc))
    (let ((tail node))
      (while tail
        (setq acc (shexc-shexr--collect-hoisted (cadr tail) acc))
        (setq tail (cddr tail))))
    acc)
   ((and (listp node) node)
    (dolist (x node) (setq acc (shexc-shexr--collect-hoisted x acc)))
    acc)
   (t acc)))

(defun shexc-shexr--all-hoisted (schema)
  (nreverse (shexc-shexr--collect-hoisted schema nil)))

;; ---------------------------------------------------------------------
;; Leaf value serialization
;; ---------------------------------------------------------------------

(defun shexc-shexr--ref-text (id)
  "ID is an absolute IRI string or a `_:label' blank-node-label string."
  (if (string-prefix-p "_:" id) id (concat "<" id ">")))

(defun shexc-shexr--turtle-string (text)
  (concat "\""
          (replace-regexp-in-string
           "[\\\"\n\r\t\b\f]"
           (lambda (m)
             (cond ((string= m "\\") "\\\\") ((string= m "\"") "\\\"")
                   ((string= m "\n") "\\n") ((string= m "\r") "\\r")
                   ((string= m "\t") "\\t") ((string= m "\b") "\\b")
                   ((string= m "\f") "\\f")))
           text)
          "\""))

(defun shexc-shexr--serialize-literal (lit)
  (let ((value (plist-get lit :value))
        (type (plist-get lit :type))
        (lang (plist-get lit :language)))
    (cond
     ((and type (string= type shexc-shexr--xsd-boolean-iri)) value) ; native true/false keyword
     (type (concat (shexc-shexr--turtle-string value) "^^" (shexc-shexr--ref-text type)))
     (lang (concat (shexc-shexr--turtle-string value) "@" lang))
     (t (shexc-shexr--turtle-string value)))))

;; ---------------------------------------------------------------------
;; Property string-mode dispatch (IRI reference vs. plain text)
;; ---------------------------------------------------------------------

(defconst shexc-shexr--plain-text-types
  '("LiteralStem" "LiteralStemRange" "LanguageStem" "LanguageStemRange")
  "Types whose `:stem'/bare `:exclusions' entries are plain text, not
IRIs -- the complement (Iri*) uses IRI references instead.")

(defun shexc-shexr--key-string-mode (type key)
  "How a bare-string value of KEY (within a node of :type TYPE) should be
rendered: `plain' for a Turtle string literal, `iri' for an IRI/blank-
node-label reference."
  (cond
   ((memq key shexc-shexr--plain-text-keys) 'plain)
   ((memq key '(:stem :exclusions))
    (if (member type shexc-shexr--plain-text-types) 'plain 'iri))
   (t 'iri)))

;; ---------------------------------------------------------------------
;; General value/body serialization
;; ---------------------------------------------------------------------

(defun shexc-shexr--serialize-value (v &optional string-mode)
  "STRING-MODE, when V is a bare string, selects how to render it: `plain'
for a Turtle string literal, `iri' (the default) for an IRI/blank-node-
label reference."
  (cond
   ((eq v t) "true")
   ((numberp v) (number-to-string v))
   ((stringp v) (if (eq string-mode 'plain) (shexc-shexr--turtle-string v) (shexc-shexr--ref-text v)))
   ((shexc-shexr--literal-value-p v) (shexc-shexr--serialize-literal v))
   ((and (consp v) (keywordp (car v)))
    (if (plist-get v :id)
        (shexc-shexr--ref-text (plist-get v :id))
      (concat "[ " (shexc-shexr--serialize-body v) " ]")))
   ((listp v) (shexc-shexr--serialize-rdf-list v string-mode))
   (t (error "shexc-shexr: cannot serialize value %S" v))))

(defun shexc-shexr--serialize-rdf-list (items &optional string-mode)
  (concat "(" (mapconcat (lambda (x) (shexc-shexr--serialize-value x string-mode)) items " ") ")"))

(defun shexc-shexr--sort-keys (keys)
  "KEYS in canonical order: alphabetical, with the \"big nested\"
properties (`shexc-shexr--last-sorted-keys') sorted last."
  (let ((last (seq-filter (lambda (k) (memq k shexc-shexr--last-sorted-keys)) keys))
        (rest (seq-remove (lambda (k) (memq k shexc-shexr--last-sorted-keys)) keys)))
    (append (sort rest (lambda (a b) (string< (symbol-name a) (symbol-name b)))) last)))

(defun shexc-shexr--serialize-prop (type key value)
  (concat "sx:" (substring (symbol-name key) 1) " "
          (if (eq key :extra)
              (mapconcat #'shexc-shexr--ref-text value ", ")
            (shexc-shexr--serialize-value value (shexc-shexr--key-string-mode type key)))))

(defun shexc-shexr--serialize-body (node)
  "Render NODE's `a sx:Type ; prop val ; ...' content (no enclosing
`[ ]'/trailing `.') -- shared between an inline `[ ... ]' and a top-level
hoisted/root statement."
  (let* ((type (plist-get node :type))
         ;; `:context' is JSON-LD-adapter metadata (see shexc-shexj.el's
         ;; @context handling), not real RDF content -- excluded here so
         ;; the narrow parser never has to special-case it.
         (keys (shexc-shexr--sort-keys
                (remq :context (remq :type (remq :id (shexc-shexr--plist-keys node)))))))
    (mapconcat
     #'identity
     (cons (concat "a sx:" type)
           (mapcar (lambda (k) (shexc-shexr--serialize-prop type k (plist-get node k))) keys))
     " ;\n  ")))

;; ---------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------

;;;###autoload
(defun shexc-shexr-serialize (schema)
  "Serialize SCHEMA (a Schema value-tree, see shexc-shexj.el) to canonical
ShExR Turtle text."
  (let ((hoisted (shexc-shexr--all-hoisted schema)))
    (concat
     shexc-shexr--prefix-line
     "[] " (shexc-shexr--serialize-body schema) " .\n"
     (mapconcat
      (lambda (n) (concat "\n" (shexc-shexr--ref-text (plist-get n :id)) " " (shexc-shexr--serialize-body n) " .\n"))
      hoisted ""))))

(provide 'shexc-shexr)

;;; shexc-shexr.el ends here
