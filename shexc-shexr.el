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
(require 'pcase)

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
           text nil t) ; LITERAL=t -- the lambda's return value (e.g. "\\\\")
                       ; must not be re-interpreted as replace-match's own
                       ; `\N'-backreference syntax, or a doubled backslash
                       ; collapses right back to one and corrupts the escape.
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
rendered: `plain' for a Turtle string literal, `value' for a value-set/
Annotation-object entry (bare IRI, or a literal -- see
`shexc-shexr--parse-value-set-entry'), `ref' (the default) for a
wrapped `sx:Ref' id-reference.

`ref' wraps *every* other bare string, with no per-key exceptions,
rather than trying to enumerate exactly which ShExJ properties can
coincide with a same-document hoisted object's id (`:valueExpr'/
`:shapeExpr'/`:expression(s)' obviously can, since ShExJ's
shapeExprOrRef/tripleExprOrRef union types allow it deliberately -- but
so, empirically, can `:extends' and even `:datatype', confirmed via
kitchenSink.shex and _all.shex respectively).  A naked `<IRI>'/
`_:label' is reserved exclusively for \"dereference the matching
hoisted statement\" (see `shexc-shexr--resolve'); since that can never
be conflated with a real bare-string ShExJ value once *every* such
value is wrapped, there's no remaining enumeration to get wrong."
  (cond
   ((memq key shexc-shexr--plain-text-keys) 'plain)
   ((memq key '(:stem :exclusions))
    (if (member type shexc-shexr--plain-text-types) 'plain 'ref))
   ((memq key '(:values :object)) 'value)
   (t 'ref)))

;; ---------------------------------------------------------------------
;; General value/body serialization
;; ---------------------------------------------------------------------

(defun shexc-shexr--serialize-ref (id)
  "A bare ShExJ string VALUE (shape-ref/Include/Schema:start/:extends/
:datatype/..., not a reference to a same-document hoisted object) --
wrapped in `sx:Ref' rather than emitted as a naked `<IRI>'/`_:label', so
the parser can always tell the two apart syntactically.  See
`shexc-shexr--key-string-mode' for why this disambiguation is needed."
  (concat "[ a sx:Ref ; sx:id " (shexc-shexr--ref-text id) " ]"))

(defun shexc-shexr--serialize-value (v &optional string-mode)
  "STRING-MODE, when V is a bare string, selects how to render it: `plain'
for a Turtle string literal, `value' for a value-set/Annotation-object
entry, `ref' (the default) for a wrapped `sx:Ref' id-reference -- see
`shexc-shexr--key-string-mode'."
  (cond
   ((eq v t) "true")
   ((numberp v) (number-to-string v))
   ((stringp v) (pcase string-mode
                  ('plain (shexc-shexr--turtle-string v))
                  ('ref (shexc-shexr--serialize-ref v))
                  (_ (shexc-shexr--ref-text v))))
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

;; ---------------------------------------------------------------------
;; Narrow parser: the precise inverse of the serializer above, and
;; nothing more general.  Hand-written recursive descent over a buffer,
;; in two passes:
;;
;; Pass 1 parses the raw text structure into "raw" plists/lists, with
;; every naked `<IRI>'/`_:label' VALUE-position token left as a deferred
;; reference marker (`shexc-shexr--make-ref') rather than resolved
;; immediately -- resolving requires knowing the complete hoist table,
;; which isn't available until every top-level statement has been seen.
;;
;; Pass 2 (`shexc-shexr--resolve') walks the Schema root's raw tree and
;; replaces every deferred marker with the fully-resolved content of the
;; matching top-level hoisted statement, `:id' restored.
;;
;; By construction (see `shexc-shexr--key-string-mode'), a naked
;; `<IRI>'/`_:label' VALUE-position token can *only* come from the
;; serializer's object branch (`shexc-shexr--serialize-value's
;; `(plist-get v :id)' check) -- which is also exactly what triggers
;; hoisting that object to its own top-level statement in the first
;; place -- so a deferred marker is guaranteed to find a match.  Every
;; bare *string* ShExJ value (`:predicate', `:datatype', a shape-ref
;; `:valueExpr', an `:extends' target, an Include element, ...) is
;; instead always wrapped in `sx:Ref' at serialize time and unwrapped
;; straight back to that string in `shexc-shexr--parse-inline-object',
;; bypassing the deferred-marker/hoist-table machinery entirely -- so it
;; can never be mistaken for an object reference, regardless of whether
;; that same IRI also happens to be hoisted elsewhere (a real
;; possibility: kitchenSink.shex's `ex:reportedBy IRI @UserShape:' and
;; _all.shex's reuse of "IRI" as both a ShapeDecl id and a `:datatype'
;; value both demonstrate this isn't just theoretical).  `resolve''s
;; fallback to a bare string when no hoisted statement matches is
;; consequently dead code for machine-generated input, kept only as a
;; graceful-degradation safety net for hand-edited fenced ShExR text.

(define-error 'shexc-shexr-parse-error "ShExR parse error")

(defun shexc-shexr--fail (fmt &rest args)
  "Signal a structured `shexc-shexr-parse-error' at point, never an
uncaught generic Lisp error -- callers (e.g. a flymake backend) can
catch this and report (point) as the failing position."
  (signal 'shexc-shexr-parse-error (list (apply #'format fmt args) (point))))

(defun shexc-shexr--make-ref (id) (cons 'shexc-shexr--ref id))
(defun shexc-shexr--ref-p (x) (and (consp x) (eq (car x) 'shexc-shexr--ref)))

(defun shexc-shexr--skip-ws ()
  (skip-chars-forward " \t\n\r"))

(defun shexc-shexr--consume (literal)
  (shexc-shexr--skip-ws)
  (unless (looking-at (regexp-quote literal))
    (shexc-shexr--fail "expected `%s'" literal))
  (goto-char (+ (point) (length literal))))

(defun shexc-shexr--parse-iri-or-bnode ()
  (shexc-shexr--skip-ws)
  (cond
   ((looking-at "<\\([^>]*\\)>")
    (goto-char (match-end 0)) (match-string-no-properties 1))
   ;; PN_CHARS-based BLANK_NODE_LABEL spans huge Unicode ranges (combining
   ;; marks, CJK, emoji, ...) -- rather than reproduce that whole grammar,
   ;; just exclude the handful of ASCII delimiters that can legitimately
   ;; follow a label in this canonical shape: whitespace and `[](); ,'
   ;; (`.' is deliberately allowed mid-token: BLANK_NODE_LABEL permits an
   ;; internal `.', and since the upstream grammar already forbids a
   ;; *trailing* one, a real label can never be confused with the
   ;; following statement-terminating `.').
   ((looking-at "_:\\([^][(); \t\n\r,]+\\)")
    (goto-char (match-end 0)) (concat "_:" (match-string-no-properties 1)))
   (t (shexc-shexr--fail "expected an IRI or blank node label"))))

(defun shexc-shexr--unescape-turtle-string (raw)
  "Inverse of `shexc-shexr--turtle-string's escaping."
  (replace-regexp-in-string
   "\\\\\\(.\\)"
   (lambda (m)
     (let ((c (substring m 1)))
       (cond ((string= c "\"") "\"") ((string= c "\\") "\\")
             ((string= c "n") "\n") ((string= c "r") "\r")
             ((string= c "t") "\t") ((string= c "b") "\b")
             ((string= c "f") "\f") (t c))))
   raw nil t)) ; LITERAL=t -- see shexc-shexr--turtle-string's comment

(defun shexc-shexr--parse-string-token ()
  (shexc-shexr--skip-ws)
  (unless (looking-at "\"\\(\\(?:\\\\.\\|[^\"\\]\\)*\\)\"")
    (shexc-shexr--fail "expected a string literal"))
  (let ((raw (match-string-no-properties 1)))
    (goto-char (match-end 0))
    (shexc-shexr--unescape-turtle-string raw)))

(defun shexc-shexr--parse-number-token ()
  (shexc-shexr--skip-ws)
  (unless (looking-at "-?[0-9]+\\(\\.[0-9]+\\)?\\([eE][-+]?[0-9]+\\)?")
    (shexc-shexr--fail "expected a number"))
  (let ((text (match-string-no-properties 0)))
    (goto-char (match-end 0))
    (string-to-number text)))

(defun shexc-shexr--parse-sx-name ()
  (shexc-shexr--skip-ws)
  (unless (looking-at "sx:\\([A-Za-z][A-Za-z0-9]*\\)")
    (shexc-shexr--fail "expected an `sx:Name' token"))
  (goto-char (match-end 0))
  (match-string-no-properties 1))

(defun shexc-shexr--parse-prefix-line ()
  (shexc-shexr--skip-ws)
  (unless (looking-at "@prefix[ \t]+sx:[ \t]+<http://www\\.w3\\.org/ns/shex#>[ \t]*\\.")
    (shexc-shexr--fail "expected `@prefix sx: <http://www.w3.org/ns/shex#> .'"))
  (goto-char (match-end 0)))

(defun shexc-shexr--parse-typed-literal ()
  "A quoted string, optionally suffixed by `^^<iri>' or `@lang' -- a
value-set literal entry, returned as a `:value'/`:type'/`:language' plist."
  (let ((text (shexc-shexr--parse-string-token)))
    (cond
     ((looking-at "\\^\\^") (goto-char (match-end 0))
      (list :value text :type (shexc-shexr--parse-iri-or-bnode)))
     ((looking-at "@\\([A-Za-z][A-Za-z0-9-]*\\)") (goto-char (match-end 0))
      (list :value text :language (match-string-no-properties 1)))
     (t (list :value text)))))

(defun shexc-shexr--parse-plain-string ()
  (unless (looking-at "\"") (shexc-shexr--fail "expected a plain string literal"))
  (shexc-shexr--parse-string-token))

(defun shexc-shexr--parse-value-set-entry ()
  (shexc-shexr--skip-ws)
  (cond
   ((looking-at "true\\_>") (goto-char (match-end 0))
    (list :value "true" :type shexc-shexr--xsd-boolean-iri))
   ((looking-at "false\\_>") (goto-char (match-end 0))
    (list :value "false" :type shexc-shexr--xsd-boolean-iri))
   ((looking-at "\"") (shexc-shexr--parse-typed-literal))
   ;; a bare IRI/blank-node value-set entry is a plain string -- never a
   ;; reference to a hoisted same-document object (value-set entries are
   ;; never `:id'-bearing), so resolved immediately, not deferred.
   ((looking-at "<\\|_:") (shexc-shexr--parse-iri-or-bnode))
   (t (shexc-shexr--fail "expected a value-set entry"))))

(defun shexc-shexr--parse-rdf-value (&optional mode)
  "Parse one value at point.  MODE mirrors
`shexc-shexr--key-string-mode': `value' for a NodeConstraint value-set/
Annotation-object entry, `plain' for plain text, anything else
(including the usual `ref') falls through to the default dispatch --
inline objects, RDF lists, numbers, and booleans are recognized by
their own syntax regardless of MODE, and a bare `\"...\"' token never
needs to be expected here at all (every property whose value could
syntactically be a quoted string uses `plain' or `value' mode; see
`shexc-shexr--key-string-mode')."
  (shexc-shexr--skip-ws)
  (cond
   ((looking-at "\\[") (shexc-shexr--parse-inline-object))
   ((looking-at "(") (shexc-shexr--parse-rdf-list mode))
   ((eq mode 'value) (shexc-shexr--parse-value-set-entry))
   ((looking-at "true\\_>") (goto-char (match-end 0)) t)
   ((looking-at "false\\_>") (goto-char (match-end 0)) nil)
   ((looking-at "-?[0-9]") (shexc-shexr--parse-number-token))
   ((eq mode 'plain) (shexc-shexr--parse-plain-string))
   ((looking-at "<\\|_:") (shexc-shexr--make-ref (shexc-shexr--parse-iri-or-bnode)))
   (t (shexc-shexr--fail "expected a value"))))

(defun shexc-shexr--parse-rdf-list (mode)
  (shexc-shexr--consume "(")
  (let (items)
    (while (progn (shexc-shexr--skip-ws) (not (looking-at ")")))
      (push (shexc-shexr--parse-rdf-value mode) items))
    (shexc-shexr--consume ")")
    (nreverse items)))

(defun shexc-shexr--parse-comma-list ()
  "`sx:extra's comma-separated object list -- always bare predicate IRIs,
parsed directly rather than through `shexc-shexr--parse-rdf-value', since
an `extra' target is never itself a hoisted same-document object."
  (let ((items (list (shexc-shexr--parse-iri-or-bnode))))
    (while (progn (shexc-shexr--skip-ws) (looking-at ","))
      (goto-char (1+ (point)))
      (push (shexc-shexr--parse-iri-or-bnode) items))
    (nreverse items)))

(defun shexc-shexr--parse-predicate-object-list ()
  "Parse `a sx:Type ; sx:prop val ; ...' at point (stopping right before
the statement-terminating `.'/enclosing `]'), tolerating any order among
the `;'-separated pairs.  Returns a raw plist (`:type' plus whatever
properties were present, each value possibly still containing deferred
reference markers -- see the Commentary above)."
  (shexc-shexr--skip-ws)
  (unless (looking-at "a\\_>") (shexc-shexr--fail "expected `a sx:Type'"))
  (goto-char (match-end 0))
  (let ((type (shexc-shexr--parse-sx-name))
        (props nil))
    (while (progn (shexc-shexr--skip-ws) (looking-at ";"))
      (goto-char (1+ (point)))
      (shexc-shexr--skip-ws)
      (let* ((key-name (shexc-shexr--parse-sx-name))
             (kw (intern (concat ":" key-name))))
        (setq props
              (plist-put props kw
                         (if (string= key-name "extra")
                             (shexc-shexr--parse-comma-list)
                           (shexc-shexr--parse-rdf-value (shexc-shexr--key-string-mode type kw)))))))
    (append (list :type type) props)))

(defun shexc-shexr--parse-inline-object ()
  "Parse `[ a sx:Type ; ... ]' at point.  A `sx:Ref'-typed object (see
`shexc-shexr--serialize-ref') is unwrapped immediately to its bare IRI/
label text -- never deferred, and never confusable with a same-document
hoisted-object reference, regardless of what else the document hoists."
  (shexc-shexr--consume "[")
  (let ((body (shexc-shexr--parse-predicate-object-list)))
    (shexc-shexr--consume "]")
    (if (equal (plist-get body :type) "Ref")
        (let ((id-val (plist-get body :id)))
          (if (shexc-shexr--ref-p id-val) (cdr id-val) id-val))
      body)))

(defun shexc-shexr--parse-subject ()
  "Returns the symbol `shexc-shexr--root' for the anonymous `[]' Schema
subject, or an IRI/blank-node-label string for a hoisted subject."
  (shexc-shexr--skip-ws)
  (if (looking-at "\\[\\]")
      (progn (goto-char (match-end 0)) 'shexc-shexr--root)
    (shexc-shexr--parse-iri-or-bnode)))

(defun shexc-shexr--resolve (raw hoist-table)
  "Recursively replace every deferred ref marker in RAW with either the
fully-resolved content of the hoisted statement it points to (`:id'
restored), or, if HOIST-TABLE has no matching statement, the bare IRI/
label text itself."
  (cond
   ((shexc-shexr--ref-p raw)
    (let* ((id (cdr raw)) (target (assoc id hoist-table)))
      (if target
          (plist-put (shexc-shexr--resolve (cdr target) hoist-table) :id id)
        id)))
   ((and (consp raw) (keywordp (car raw)))
    (let (out (tail raw))
      (while tail
        (setq out (plist-put out (car tail) (shexc-shexr--resolve (cadr tail) hoist-table)))
        (setq tail (cddr tail)))
      out))
   ((listp raw) (mapcar (lambda (x) (shexc-shexr--resolve x hoist-table)) raw))
   (t raw)))

;;;###autoload
(defun shexc-shexr-parse (text)
  "Parse TEXT (canonical ShExR Turtle, see this file's Commentary) into a
value-tree, the inverse of `shexc-shexr-serialize'.  Signals a
`shexc-shexr-parse-error' (never an uncaught generic Lisp error) on
malformed or non-canonical input."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (shexc-shexr--parse-prefix-line)
    (let (root hoist-table)
      (shexc-shexr--skip-ws)
      (while (not (eobp))
        (let* ((subj (shexc-shexr--parse-subject))
               (raw (shexc-shexr--parse-predicate-object-list)))
          (shexc-shexr--skip-ws)
          (unless (looking-at "\\.") (shexc-shexr--fail "expected `.' terminating a top-level statement"))
          (goto-char (1+ (point)))
          (if (eq subj 'shexc-shexr--root)
              (if root (shexc-shexr--fail "more than one anonymous `[]' top-level subject")
                (setq root raw))
            (push (cons subj raw) hoist-table))
          (shexc-shexr--skip-ws)))
      (unless root (shexc-shexr--fail "no anonymous `[]' Schema root statement found"))
      (shexc-shexr--resolve root hoist-table))))

(provide 'shexc-shexr)

;;; shexc-shexr.el ends here
