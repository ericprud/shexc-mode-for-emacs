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
;; Canonical shape, fixed by construction (see `shexc-shexr--pretty-body'
;; and `shexc-shexr--pretty-value'):
;; - A nested `[ ... ]'/`( ... )' pair renders on one line if it fits
;;   within `shexc-shexr--fill-column' at its actual starting column;
;;   otherwise it breaks, one child per line, each indented two spaces
;;   deeper than its enclosing line, with the closing bracket back at the
;;   enclosing line's own indent (never aligned to the bracket's column).
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
  "@prefix sx: <http://www.w3.org/ns/shex#> .\n")

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

(defconst shexc-shexr--fill-column 80
  "A nested `[ ... ]'/`( ... )' pair is kept on one line if it fits within
this column count at its actual starting column; otherwise it's broken
across multiple lines, indented two spaces deeper than its enclosing
line.  Purely a pretty-printing convention -- the narrow parser is
whitespace-agnostic and accepts either form anywhere.")

;; ---------------------------------------------------------------------
;; Optional IRI shortening against a caller-supplied PREFIX/BASE table
;; (typically the converting ShExC buffer's own declarations -- see
;; shexc-ts-mode-convert.el).  `shexc-shexr-serialize' takes PREFIXES/
;; BASE as plain data (an alist, a string) rather than depending on
;; shexc-shexj's `ctx' struct, per this file's existing boundary (see
;; "Plist utilities" above) -- and re-implements the small longest-
;; namespace-match/safe-PN_LOCAL policy locally rather than calling
;; `shexc-ts-mode-convert--shorten-iri', for the same reason: this file
;; doesn't depend on shexc-ts-mode-convert.el at all today, and a single
;; ~15-line policy isn't worth introducing that dependency for.  Unlike
;; PREFIX/BASE-shortening on the way *back* to ShExC (which never adds
;; new declarations, since the buffer already has them), the fence here
;; must be self-contained Turtle the narrow parser can resolve with no
;; buffer context at all -- so only the PREFIX/BASE entries actually
;; used get a header line, in ShExC's own `PREFIX name: <ns>'/`BASE
;; <ns>' syntax (no `@prefix'/trailing `.') -- distinct from the fixed
;; `@prefix sx: <...> .' vocabulary line, the one part of this format
;; that's really meant to be read as Turtle.

(defun shexc-shexr--safe-pn-local-p (s)
  "Whether S can be emitted as a PN_LOCAL with no `%XX'/backslash
escaping -- deliberately conservative (e.g. rejects a trailing `.',
which PN_LOCAL disallows unescaped): false negatives just mean a
shortening opportunity is missed, never that something unparseable
gets emitted."
  (and (not (string-empty-p s))
       (string-match-p "\\`[A-Za-z0-9_][A-Za-z0-9_.-]*\\'" s)
       (not (string-suffix-p "." s))))

(defvar shexc-shexr--shorten-fn nil
  "Dynamically bound by `shexc-shexr-serialize' to a function IRI ->
shortened token string or nil; nil itself (the default -- e.g. for
direct/test calls with no PREFIXES/BASE) means never shorten.")

(defvar shexc-shexr--used-prefixes nil
  "Dynamically bound by `shexc-shexr-serialize'; see
`shexc-shexr--make-shortener'.")

(defvar shexc-shexr--used-base nil
  "Dynamically bound by `shexc-shexr-serialize'; see
`shexc-shexr--make-shortener'.")

(declare-function url-expand-file-name "url-expand" (url &optional default))

(defun shexc-shexr--make-shortener (prefixes base)
  "Return a function IRI -> shortened token string or nil, from PREFIXES
\(an alist of (NAME . NAMESPACE-IRI)) and BASE (a namespace IRI string
or nil) -- longest-namespace-match against PREFIXES first, then BASE
\(verified by re-resolving the candidate relative form, so an
unanticipated quirk of IRI-relative-resolution can never silently
produce the wrong reference).  Records every PREFIXES NAME actually
used onto `shexc-shexr--used-prefixes', and whether BASE was used onto
`shexc-shexr--used-base' -- both dynamically bound by
`shexc-shexr-serialize' around the call, so the caller knows which
header lines to emit."
  (lambda (iri)
    (let (best-name best-ns)
      (dolist (entry prefixes)
        (when (and (string-prefix-p (cdr entry) iri)
                   (or (not best-ns) (> (length (cdr entry)) (length best-ns))))
          (setq best-name (car entry) best-ns (cdr entry))))
      (cond
       ((and best-ns (shexc-shexr--safe-pn-local-p (substring iri (length best-ns))))
        (push best-name shexc-shexr--used-prefixes)
        (concat best-name ":" (substring iri (length best-ns))))
       ((and base (string-prefix-p base iri)
             (let ((relative (substring iri (length base))))
               (and (equal (url-expand-file-name relative base) iri) relative)))
        (setq shexc-shexr--used-base t)
        (concat "<" (substring iri (length base)) ">"))
       (t nil)))))

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
  (cond
   ((string-prefix-p "_:" id) id)
   ((and shexc-shexr--shorten-fn (funcall shexc-shexr--shorten-fn id)))
   (t (concat "<" id ">"))))

(defun shexc-shexr--ref-trailing-skip (ref-text)
  "Trailing decoration length in REF-TEXT (a `shexc-shexr--ref-text'
result) that isn't part of the IRI/label's own content -- 1 for the
closing `>' of a bracketed `<IRI>' form, 0 for a shortened PNAME or a
`_:label', neither of which has a trailing wrapper.  Needed because
which form `shexc-shexr--ref-text' picks (and thus how much trailing
decoration there is to skip when end-anchoring -- see
`shexc-shexr--mark') depends on whether `shexc-shexr--shorten-fn' is
active and matches, not on anything visible at the call site."
  (if (string-suffix-p ">" ref-text) 1 0))

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

(defun shexc-shexr--sort-keys (keys)
  "KEYS in canonical order: alphabetical, with the \"big nested\"
properties (`shexc-shexr--last-sorted-keys') sorted last."
  (let ((last (seq-filter (lambda (k) (memq k shexc-shexr--last-sorted-keys)) keys))
        (rest (seq-remove (lambda (k) (memq k shexc-shexr--last-sorted-keys)) keys)))
    (append (sort rest (lambda (a b) (string< (symbol-name a) (symbol-name b)))) last)))

(defun shexc-shexr--node-keys (node)
  "NODE's property keys (excluding `:type'/`:id'/`:context'), in canonical
order.  `:context' is JSON-LD-adapter metadata (see shexc-shexj.el's
@context handling), not real RDF content -- excluded here so the narrow
parser never has to special-case it."
  (shexc-shexr--sort-keys
   (remq :context (remq :type (remq :id (shexc-shexr--plist-keys node))))))

;; ---------------------------------------------------------------------
;; Optional output-position tracking: every render function below
;; returns a `(TEXT . OFFSET-OR-NIL)' pair rather than plain TEXT --
;; OFFSET-OR-NIL is the 0-indexed position within TEXT where the
;; dynamically-bound `shexc-shexr--position-target' value (matched by
;; `eq', so always a cons or a string -- see shexc-ts-mode-convert.el's
;; lookup, which never hands this a number/boolean/nil) begins
;; rendering, or nil if it isn't anywhere in TEXT.  A shared mutable
;; counter can't do this instead: the flat-vs-broken fits check (below)
;; renders a flat candidate purely to measure its length and may
;; discard it, so any "bump a counter as you go" scheme would have to
;; un-bump on discard, which doesn't compose; returning pairs does, by
;; construction. `shexc-shexr--position-target' is nil (its default)
;; for every existing caller, so the offset side of every pair is
;; always nil and the TEXT side is untouched -- the public entry point
;; `shexc-shexr-serialize' still returns a plain string in that case,
;; so this is invisible to all of today's callers/tests.
;; ---------------------------------------------------------------------

(defvar shexc-shexr--position-target nil
  "Dynamically bound by `shexc-shexr-serialize' (its TARGET argument) to
a value (by `eq') somewhere within the schema being serialized whose
output position should be located; nil means \"don't track\".")

(defun shexc-shexr--lit (text)
  "Wrap a plain literal TEXT fragment (no possible match inside it) as
the `(TEXT . OFFSET-OR-NIL)' pair every render function returns."
  (cons text nil))

(defun shexc-shexr--cat-list (pairs)
  "Concatenate PAIRS (each a `(TEXT . OFFSET-OR-NIL)', in left-to-right
emission order) into one such pair: TEXT is the concatenation,
OFFSET-OR-NIL is the first PAIR's own offset that's non-nil (there's at
most one, since `shexc-shexr--position-target' identifies a single
value), adjusted by the combined length of every fragment before it."
  (let ((running 0) offset)
    (dolist (p pairs)
      (when (and (not offset) (cdr p)) (setq offset (+ running (cdr p))))
      (setq running (+ running (length (car p)))))
    (cons (mapconcat #'car pairs "") offset)))

(defun shexc-shexr--cat (&rest pairs) (shexc-shexr--cat-list pairs))

(defun shexc-shexr--cat-join (pairs sep)
  "Like `shexc-shexr--cat-list' but interposing literal SEP between
PAIRS, mirroring `mapconcat'."
  (let (spliced (first t))
    (dolist (p pairs)
      (unless first (push (shexc-shexr--lit sep) spliced))
      (push p spliced)
      (setq first nil))
    (shexc-shexr--cat-list (nreverse spliced))))

(defun shexc-shexr--mark (v pair &optional trailing-skip)
  "Override PAIR's offset to point at the END of V's own literal text
within PAIR's TEXT (default: the very end of TEXT) if V itself is the
active `shexc-shexr--position-target', regardless of what PAIR's own
offset already was (always nil in practice: a value can't be `eq' to
one of its own descendants, so nothing nested inside V's normal
rendering could have matched if V itself is the target).

Anchored at the END, not the start: a value's rendering can gain or
lose an arbitrary-length PREFIX (a PNAME's `ex:' vs. a full namespace
IRI, an `sx:Ref'-wrapping decoration, ...) between hops, but the
trailing characters of V's own literal text -- e.g. a PNAME's local
part, the tail of an IRI -- stay the same, so measuring from the end
survives that change where measuring from the start wouldn't (see
`shexc-ts-mode-convert--locate-target').  TRAILING-SKIP lets a caller
that wraps V's own literal text in fixed decoration AFTER it (` ]',
say) say how many characters of PAIR's TEXT to trim off the end before
landing on V's own literal text -- the default of 0 is right whenever
PAIR's text already *is* V's own literal rendering with nothing after
it."
  (if (and shexc-shexr--position-target (eq v shexc-shexr--position-target))
      (cons (car pair) (- (length (car pair)) (or trailing-skip 0)))
    pair))

(defun shexc-shexr--mark-ref (id)
  "Render ID (an IRI/`_:label' string) via `shexc-shexr--ref-text',
marked as ID's own end-anchored position if ID is the active
`shexc-shexr--position-target' -- the common case of a bare reference
with no further decoration wrapped around it."
  (let ((text (shexc-shexr--ref-text id)))
    (shexc-shexr--mark id (shexc-shexr--lit text) (shexc-shexr--ref-trailing-skip text))))

;; -- Flat rendering: always one line, no indentation/breaking -- used both
;; as the actual compact output and, via its length, as the input to the
;; fits-on-one-line check in the pretty (depth-aware) renderer below.

(defun shexc-shexr--flat-value (v &optional string-mode)
  "STRING-MODE, when V is a bare string, selects how to render it: `plain'
for a Turtle string literal, `value' for a value-set/Annotation-object
entry, `ref' (the default) for a wrapped `sx:Ref' id-reference -- see
`shexc-shexr--key-string-mode'."
  (cond
   ((eq v t) (shexc-shexr--lit "true"))
   ((numberp v) (shexc-shexr--lit (number-to-string v)))
   ((stringp v)
    (pcase string-mode
      ('plain (shexc-shexr--mark v (shexc-shexr--lit (shexc-shexr--turtle-string v)) 1)) ; skip closing quote
      ('ref (let* ((prefix "[ a sx:Ref ; sx:id ")
                   (text (shexc-shexr--ref-text v)))
              (shexc-shexr--mark v (shexc-shexr--lit (concat prefix text " ]"))
                                  (+ (length " ]") (shexc-shexr--ref-trailing-skip text)))))
      (_ (shexc-shexr--mark-ref v))))
   ((shexc-shexr--literal-value-p v) (shexc-shexr--lit (shexc-shexr--serialize-literal v)))
   ((and (consp v) (keywordp (car v)))
    (if (plist-get v :id)
        (shexc-shexr--mark-ref (plist-get v :id))
      (shexc-shexr--mark v (shexc-shexr--cat (shexc-shexr--lit "[ ") (shexc-shexr--flat-body v) (shexc-shexr--lit " ]")) (length " ]"))))
   ((listp v)
    (shexc-shexr--mark
     v
     (shexc-shexr--cat
      (shexc-shexr--lit "(")
      (shexc-shexr--cat-join (mapcar (lambda (x) (shexc-shexr--flat-value x string-mode)) v) " ")
      (shexc-shexr--lit ")"))))
   (t (error "shexc-shexr: cannot serialize value %S" v))))

(defun shexc-shexr--flat-prop (type key value)
  (let ((label (concat "sx:" (substring (symbol-name key) 1) " ")))
    (if (eq key :extra)
        (shexc-shexr--cat
         (shexc-shexr--lit label)
         (shexc-shexr--cat-join
          (mapcar #'shexc-shexr--mark-ref value)
          ", "))
      (shexc-shexr--cat (shexc-shexr--lit label) (shexc-shexr--flat-value value (shexc-shexr--key-string-mode type key))))))

(defun shexc-shexr--flat-body (node)
  "Render NODE's `a sx:Type ; prop val ; ...' content on one line (no
enclosing `[ ]'/trailing `.')."
  (shexc-shexr--cat-join
   (cons (shexc-shexr--lit (concat "a sx:" (plist-get node :type)))
         (mapcar (lambda (k) (shexc-shexr--flat-prop (plist-get node :type) k (plist-get node k)))
                 (shexc-shexr--node-keys node)))
   " ; "))

;; -- Pretty rendering: depth-aware.  A nested `[ ... ]'/`( ... )' pair is
;; kept compact (the flat form above) if it fits within WIDTH columns at
;; its actual starting column; otherwise it's broken, one child per
;; line, indented two spaces deeper than the line it's nested in.
;; INDENT is always the *enclosing line's* indentation depth (used to
;; compute a broken pair's child indent), never the bracket's own
;; column; COL is the actual column the pair starts at on the current
;; line (used only for the fits-or-breaks decision).  WIDTH defaults to
;; `shexc-shexr--fill-column' but is caller-overridable (e.g.
;; shexc-ts-mode-convert.el passes the converting window's actual
;; width), so it's threaded through every pretty-* call rather than read
;; from the constant directly.

(defun shexc-shexr--fits-p (col width text)
  (<= (+ col (length text)) width))

(defun shexc-shexr--pretty-ref (id indent col width)
  "Depth-aware counterpart to `shexc-shexr--serialize-ref' -- that
function only ever produces its flat (always-one-line) form, so a long
ID (or a narrow WIDTH) needs this to break it into the same
`[\\n  a sx:Ref ;\\n  sx:id <iri>\\n]' shape any other nested object
would get."
  (let ((flat (shexc-shexr--serialize-ref id))
        (text (shexc-shexr--ref-text id)))
    (if (shexc-shexr--fits-p col width flat)
        (shexc-shexr--mark id (shexc-shexr--lit flat)
                            (+ (length " ]") (shexc-shexr--ref-trailing-skip text)))
      (let* ((inner (+ indent 2))
             (prefix (concat "[\n" (make-string inner ?\s) "a sx:Ref ;\n" (make-string inner ?\s) "sx:id "))
             (suffix (concat "\n" (make-string indent ?\s) "]")))
        (shexc-shexr--mark
         id
         (shexc-shexr--lit (concat prefix text suffix))
         (+ (length suffix) (shexc-shexr--ref-trailing-skip text)))))))

(defun shexc-shexr--pretty-value (v string-mode indent col width)
  (cond
   ((eq v t) (shexc-shexr--lit "true"))
   ((numberp v) (shexc-shexr--lit (number-to-string v)))
   ((stringp v) (pcase string-mode
                  ('plain (shexc-shexr--mark v (shexc-shexr--lit (shexc-shexr--turtle-string v)) 1))
                  ('ref (shexc-shexr--pretty-ref v indent col width))
                  (_ (shexc-shexr--mark-ref v))))
   ((shexc-shexr--literal-value-p v) (shexc-shexr--lit (shexc-shexr--serialize-literal v)))
   ((and (consp v) (keywordp (car v)))
    (if (plist-get v :id)
        (shexc-shexr--mark-ref (plist-get v :id))
      (let* ((flat-pair (shexc-shexr--flat-body v))
             (flat (concat "[ " (car flat-pair) " ]")))
        (if (shexc-shexr--fits-p col width flat)
            (shexc-shexr--mark v (shexc-shexr--cat (shexc-shexr--lit "[ ") flat-pair (shexc-shexr--lit " ]")) (length " ]"))
          (let* ((inner (+ indent 2))
                 (prefix (concat "[\n" (make-string inner ?\s)))
                 (suffix (concat "\n" (make-string indent ?\s) "]")))
            (shexc-shexr--mark
             v
             (shexc-shexr--cat
              (shexc-shexr--lit prefix)
              (shexc-shexr--pretty-body v inner width)
              (shexc-shexr--lit suffix))
             (length suffix)))))))
   ((listp v)
    (let ((flat-pair (shexc-shexr--flat-value v string-mode)))
      (if (or (null v) (shexc-shexr--fits-p col width (car flat-pair)))
          flat-pair
        (let* ((inner (+ indent 2))
               (prefix (concat "(\n" (make-string inner ?\s)))
               (suffix (concat "\n" (make-string indent ?\s) ")")))
          (shexc-shexr--mark
           v
           (shexc-shexr--cat
            (shexc-shexr--lit prefix)
            (shexc-shexr--cat-join
             (mapcar (lambda (x) (shexc-shexr--pretty-value x string-mode inner inner width)) v)
             (concat "\n" (make-string inner ?\s)))
            (shexc-shexr--lit suffix))
           (length suffix))))))
   (t (error "shexc-shexr: cannot serialize value %S" v))))

(defun shexc-shexr--pretty-prop (type key value indent width)
  (let ((label (concat "sx:" (substring (symbol-name key) 1) " ")))
    (if (eq key :extra)
        (shexc-shexr--cat
         (shexc-shexr--lit label)
         (shexc-shexr--cat-join
          (mapcar #'shexc-shexr--mark-ref value)
          ", "))
      (shexc-shexr--cat
       (shexc-shexr--lit label)
       (shexc-shexr--pretty-value value (shexc-shexr--key-string-mode type key)
                                   indent (+ indent (length label)) width)))))

(defun shexc-shexr--pretty-body (node indent width)
  "Render NODE's `a sx:Type ; prop val ; ...' content (no enclosing
`[ ]'/trailing `.'), broken one property per line at depth INDENT --
shared between a broken inline `[ ... ]' and a top-level hoisted/root
statement (which is always broken, regardless of length, by the existing
convention of one statement per blank-line-separated paragraph)."
  (let ((type (plist-get node :type))
        (pad (make-string indent ?\s)))
    (shexc-shexr--cat-join
     (cons (shexc-shexr--lit (concat "a sx:" type))
           (mapcar (lambda (k) (shexc-shexr--pretty-prop type k (plist-get node k) indent width))
                   (shexc-shexr--node-keys node)))
     (concat " ;\n" pad))))

;; ---------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------

;;;###autoload
(defun shexc-shexr-serialize (schema &optional width prefixes base target)
  "Serialize SCHEMA (a Schema value-tree, see shexc-shexj.el) to canonical
ShExR Turtle text.  WIDTH (default `shexc-shexr--fill-column') is the
column a nested `[ ... ]'/`( ... )' pair must fit within (at its actual
starting column) to stay on one line -- callers with a real display
width to fit (e.g. the converting window) can pass it here instead.

PREFIXES (an alist of (NAME . NAMESPACE-IRI)) and BASE (a namespace IRI
string), when given, are typically the converting ShExC buffer's own
PREFIX/BASE declarations (see shexc-ts-mode-convert.el) -- any IRI in
the output that falls under one of them is shortened accordingly (see
`shexc-shexr--make-shortener'), and only the entries that actually got
used are emitted as a header (ShExC's own `PREFIX name: <ns>'/`BASE
<ns>' syntax, no `@prefix'/trailing `.') right after the fixed
`@prefix sx: <...> .' line.

TARGET, if non-nil, is a value (by `eq') somewhere within SCHEMA to
locate in the output: with TARGET, this returns `(TEXT . OFFSET-OR-NIL)'
instead of plain TEXT (OFFSET-OR-NIL the 0-indexed position in TEXT
where TARGET's own rendering ENDS (not begins -- see `shexc-shexr--mark'
for why anchoring at the end survives a prefix-length change, e.g. a
PNAME shortened to/from a full namespace IRI, where anchoring at the
start wouldn't), nil if TARGET never matched
anything -- e.g. it's an atom like `t'/a number, which this never
treats as a locatable target, or it simply isn't part of SCHEMA)."
  (setq width (or width shexc-shexr--fill-column))
  (let* ((shexc-shexr--position-target target)
         (shexc-shexr--shorten-fn (and (or prefixes base) (shexc-shexr--make-shortener prefixes base)))
         (shexc-shexr--used-prefixes nil)
         (shexc-shexr--used-base nil)
         (hoisted (shexc-shexr--all-hoisted schema))
         (body-pair
          (shexc-shexr--cat
           (shexc-shexr--lit "[] ")
           (shexc-shexr--mark schema (shexc-shexr--pretty-body schema 2 width))
           (shexc-shexr--lit " .\n")
           (shexc-shexr--cat-list
            (mapcar
             (lambda (n)
               (shexc-shexr--cat
                (shexc-shexr--lit (concat "\n" (shexc-shexr--ref-text (plist-get n :id)) " "))
                (shexc-shexr--mark n (shexc-shexr--pretty-body n 2 width))
                (shexc-shexr--lit " .\n")))
             hoisted))))
         (header
          (concat
           (mapconcat (lambda (entry) (format "PREFIX %s: <%s>\n" (car entry) (cdr entry)))
                      (seq-filter (lambda (entry) (member (car entry) shexc-shexr--used-prefixes)) prefixes)
                      "")
           (if shexc-shexr--used-base (format "BASE <%s>\n" base) "")))
         (final-pair (shexc-shexr--cat (shexc-shexr--lit (concat shexc-shexr--prefix-line header "\n")) body-pair)))
    (if target final-pair (car final-pair))))

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

(defconst shexc-shexr--pname-re
  "\\([A-Za-z][A-Za-z0-9_-]*\\):\\([A-Za-z0-9_][A-Za-z0-9_.-]*\\)"
  "A `name:local' PNAME_LN token -- deliberately as restricted as
`shexc-shexr--safe-pn-local-p' (no `%XX'/backslash PN_LOCAL escapes):
the serializer only ever shortens into this exact safe subset, so the
parser only ever needs to recognize that subset back.")

(defconst shexc-shexr--iri-or-bnode-start-re
  (concat "<\\|_:\\|" shexc-shexr--pname-re)
  "Matches at the start of anything `shexc-shexr--parse-iri-or-bnode'
can parse -- used as a lookahead guard by callers (e.g.
`shexc-shexr--parse-rdf-value') that must distinguish \"there's an IRI/
bnode/prefixed-name here\" from \"there isn't, try something else\"
without committing to a parse.")

(defvar shexc-shexr--parse-prefixes nil
  "Dynamically bound by `shexc-shexr-parse' to the alist of (NAME .
NAMESPACE-IRI) pairs declared by the fence's own `PREFIX name: <ns>'
header lines -- see `shexc-shexr--parse-header'.")

(defvar shexc-shexr--parse-base nil
  "Dynamically bound by `shexc-shexr-parse' to the fence's own `BASE
<ns>' header line, or nil if it has none -- see
`shexc-shexr--parse-header'.")

(defun shexc-shexr--resolve-relative-iri (iri)
  "IRI is a `<...>' IRIREF's raw text -- resolved against the fence's own
in-scope `shexc-shexr--parse-base' if it's relative (lacks a
`scheme:' prefix); an absolute IRI passes through unchanged.  Mirrors
`shexc-shexr--make-shortener's BASE branch, which only ever produces a
relative form that round-trips back to the original via this exact
resolution."
  (if (and shexc-shexr--parse-base (not (string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*:" iri)))
      (url-expand-file-name iri shexc-shexr--parse-base)
    iri))

(defun shexc-shexr--parse-iri-or-bnode ()
  (shexc-shexr--skip-ws)
  (cond
   ((looking-at "<\\([^>]*\\)>")
    (goto-char (match-end 0))
    (shexc-shexr--resolve-relative-iri (match-string-no-properties 1)))
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
   ((looking-at shexc-shexr--pname-re)
    (goto-char (match-end 0))
    (let* ((name (match-string-no-properties 1)) (local (match-string-no-properties 2))
           (ns (cdr (assoc name shexc-shexr--parse-prefixes))))
      (unless ns (shexc-shexr--fail "undeclared PREFIX `%s:'" name))
      (concat ns local)))
   (t (shexc-shexr--fail "expected an IRI, blank node label, or prefix:local"))))

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

(defun shexc-shexr--parse-header ()
  "Parse the fixed `@prefix sx: <...> .' line, then any number of
ShExC-syntax `PREFIX name: <ns>'/`BASE <ns>' lines (no `@prefix'/
trailing `.', in any order) -- the inverse of `shexc-shexr-serialize's
header.  Populates `shexc-shexr--parse-prefixes'/`-parse-base' as a
side effect, consumed by every subsequent
`shexc-shexr--parse-iri-or-bnode' call."
  (shexc-shexr--skip-ws)
  (unless (looking-at "@prefix[ \t]+sx:[ \t]+<http://www\\.w3\\.org/ns/shex#>[ \t]*\\.")
    (shexc-shexr--fail "expected `@prefix sx: <http://www.w3.org/ns/shex#> .'"))
  (goto-char (match-end 0))
  (let (prefixes base)
    (catch 'done
      (while t
        (shexc-shexr--skip-ws)
        (cond
         ((looking-at "PREFIX[ \t]+\\([A-Za-z][A-Za-z0-9_-]*\\):[ \t]*<\\([^>]*\\)>")
          (goto-char (match-end 0))
          (push (cons (match-string-no-properties 1) (match-string-no-properties 2)) prefixes))
         ((looking-at "BASE[ \t]+<\\([^>]*\\)>")
          (goto-char (match-end 0))
          (setq base (match-string-no-properties 1)))
         (t (throw 'done nil)))))
    (setq shexc-shexr--parse-prefixes (nreverse prefixes)
          shexc-shexr--parse-base base)))

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
   ((looking-at shexc-shexr--iri-or-bnode-start-re) (shexc-shexr--parse-iri-or-bnode))
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
   ((looking-at shexc-shexr--iri-or-bnode-start-re) (shexc-shexr--make-ref (shexc-shexr--parse-iri-or-bnode)))
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
    (let (shexc-shexr--parse-prefixes shexc-shexr--parse-base root hoist-table)
      (shexc-shexr--parse-header)
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
