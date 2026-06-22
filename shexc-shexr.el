;;; shexc-shexr.el --- value-tree <-> canonical ShExR Turtle  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 4.0.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0") (rdf-store "0.1.0") (rdf-turtle "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Serializes a `shexc-shexj' value-tree (see shexc-shexj.el) to a fixed,
;; bespoke canonical Turtle shape -- NOT general Turtle, and not
;; byte-matched against shexSpec/shexTest's own .ttl fixtures (which vary
;; in formatting across fixtures -- evidence of multiple hands/tools, not
;; one disciplined serializer).
;;
;; `shexc-shexr-parse' (the reader) is, unlike the writer, general: it
;; parses arbitrary valid Turtle expressing this vocabulary -- not just
;; this file's own canonical output -- via `rdf-turtle.el' (a real
;; tree-sitter Turtle parser) and `rdf-model.el'/`rdf-store.el' (an
;; RDFJS-like quad API), so a hand-edited fence (different property
;; order, prefix choice, line-wrapping, ...) still reads back correctly.
;; See "RDF-graph parser" below.
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
;;   (value-set bare-string entries, a shape-ref `valueExpr', ...); a
;;   fixed few are plain Turtle string literals instead (`code', `pattern',
;;   `flags', `languageTag', `nodeKind', `stem'), and `exclusions' switches
;;   between the two depending on the enclosing node's `:type' (Iri* are
;;   IRIs, Literal*/Language* are plain text).
;; - `predicate'/`datatype'/Iri-flavored `exclusions' entries are *always*
;;   a bare `<iri>'/`_:label', never wrapped in `sx:Ref' -- per
;;   ShExR.shex, `sx:predicate'/`sx:datatype' are typed plain `IRI', with
;;   no object alternative, so there's no same-document-hoisted-object
;;   ambiguity to guard against for them (see `shexc-shexr--key-string-mode').
;;   A handful of other string-valued properties (`valueExpr'/`shapeExpr'/
;;   `expression(s)'/`shapeExprs'/`extends') genuinely can coincide with a
;;   same-document hoisted object's id and so still go through the
;;   `sx:Ref'-wrapping scheme described at `shexc-shexr--key-string-mode'
;;   and the narrow parser's Commentary below -- non-canonical relative to
;;   ShExR.shex for *those* properties specifically, pending a
;;   semantic-equivalence (rather than structural-equality) round-trip
;;   comparison that would let them drop the wrapper too.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pcase)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)

(defconst shexc-shexr--prefix-line
  "@prefix sx: <http://www.w3.org/ns/shex#> .\n")

(defconst shexc-shexr--xsd-boolean-iri "http://www.w3.org/2001/XMLSchema#boolean")
(defconst shexc-shexr--xsd-string-iri "http://www.w3.org/2001/XMLSchema#string")

(defconst shexc-shexr--last-sorted-keys
  '(:valueExpr :shapeExpr :expression :expressions :shapeExprs :shapes)
  "Properties holding the \"big nested\" sub-structure, always emitted
last within a node's property list -- not RDF-semantically significant,
purely a canonical-shape convention of this bespoke serializer, so the
narrow parser can rely on a predictable position.")

(defconst shexc-shexr--plain-text-keys
  '(:code :pattern :flags :languageTag :nodeKind :stem)
  "Property keys whose string value is canonical plain text (a Turtle
string literal), never an IRI/blank-node-label reference.  `:stem' is
here unconditionally -- per ShExR.shex, `sx:stem' is plain `xsd:string'
for every stem flavor (Iri/Literal/Language alike); only `:exclusions'
entries actually vary by flavor (see `shexc-shexr--plain-text-types').")

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
  "Types whose bare `:exclusions' entries are plain text, not IRIs -- the
complement (Iri*) uses IRI references instead.  (Unlike `:exclusions',
`:stem' itself is plain text regardless of type -- see
`shexc-shexr--plain-text-keys'.)")

(defun shexc-shexr--key-string-mode (type key)
  "How a bare-string value of KEY (within a node of :type TYPE) should be
rendered: `plain' for a Turtle string literal, `value' for a value-set/
Annotation-object entry (bare IRI, a literal, or a typed object like
`IriStem' -- see `shexc-shexr--decode-value'), `literal-iri' for a bare
`<iri>'/`_:label' that's *always* exactly that -- never itself
dereferenced, even if that very term happens to coincide with some
other node's subject elsewhere in the graph (so the decoder must read
it as plain identifier text unconditionally, never consulting the
term's own `rdf:type'), `ref' (the default) for a wrapped `sx:Ref'
id-reference.

`:predicate'/`:datatype'/`:id' are always `literal-iri': ShExR.shex
types `:predicate'/`:datatype' as plain `IRI', with no shapeExpr/
ShapeDecl alternative, so unlike `:valueExpr'/`:shapeExpr'/
`:expression(s)'/`:shapeExprs'/`:extends' \(whose union types
deliberately allow a same-document hoisted object's id to appear there
-- confirmed empirically via kitchenSink.shex and _all.shex) there's no
real ambiguity for `ref''s `sx:Ref' wrapping to guard against; wrapping
them anyway would also be non-conformant \(`sx:predicate'/`sx:datatype'
must be a bare IRI per ShExR.shex).  `:id' is never itself a nested
object, so it's never ambiguous either -- and is needed as an explicit
property (not just the hoisted statement's subject) so the RDF-graph
parser can tell a deliberately blank-node-labeled `:id' (e.g. ShExC's
`_:S2 { ... }') apart from a purely anonymous inline `[ ... ]', which
look identical as bare graph shape once parsed -- see
`shexc-shexr--decode-node-properties'.  Crucially, this also means
`literal-iri' must never be used for a key whose value could itself be
a same-document node's *own identifying IRI* (decoding `:id' as `ref'-
or `value'-style dereference would self-recurse forever, since that
very IRI is always also some node's subject elsewhere).

Iri-flavored `:exclusions' entries (the complement of
`shexc-shexr--plain-text-types') are `value', not `literal-iri', even
though a bare entry renders identically to `literal-iri' \(both fall to
the same bare-`<iri>'/`_:label' case in `shexc-shexr--flat-value';
they differ only for a non-bare entry -- an Iri-flavored `:exclusions'
entry can itself be a typed `IriStem'/`IriStemRange' object \(e.g.
`1val1dotMinusiriStem3.shex'), which `value' mode's decoder correctly
dereferences and `literal-iri' deliberately never does."
  (cond
   ((memq key shexc-shexr--plain-text-keys) 'plain)
   ((eq key :exclusions)
    (if (member type shexc-shexr--plain-text-types) 'plain 'value))
   ((memq key '(:predicate :datatype :id)) 'literal-iri)
   ((memq key '(:values :object)) 'value)
   (t 'ref)))

;; ---------------------------------------------------------------------
;; General value/body serialization
;; ---------------------------------------------------------------------

(defun shexc-shexr--serialize-ref (id)
  "A bare ShExJ string VALUE (shape-ref/Include/Schema:start/:extends/...,
not a reference to a same-document hoisted object) -- wrapped in
`sx:Ref' rather than emitted as a naked `<IRI>'/`_:label', so the parser
can always tell the two apart syntactically.  See
`shexc-shexr--key-string-mode' for why this disambiguation is needed --
and why `:predicate'/`:datatype'/Iri-flavored `:exclusions' (`literal-iri'
mode) never reach this function at all, unlike everything else listed
above."
  (concat "[ a sx:Ref ; sx:id " (shexc-shexr--ref-text id) " ]"))

(defconst shexc-shexr--key-name-overrides
  '((:exclusions . "exclusion") (:annotations . "annotation"))
  "ShExJ property keys whose RDF predicate local name isn't simply the
keyword name verbatim: ShExR.shex uses the singular `sx:exclusion'/
`sx:annotation' (an RDF list can hold any number of values under one
singular-named predicate) even though the ShExJ/JSON key -- and this
value-tree's plist key -- is plural `:exclusions'/`:annotations'.")

(defun shexc-shexr--key-label (key)
  "KEY's `sx:Name ' property label, honoring `shexc-shexr--key-name-overrides'."
  (concat "sx:" (or (cdr (assq key shexc-shexr--key-name-overrides))
                     (substring (symbol-name key) 1))
          " "))

(defun shexc-shexr--key-from-label-name (name)
  "Inverse of `shexc-shexr--key-label' for a parsed `sx:Name' local NAME."
  (or (car (rassoc name shexc-shexr--key-name-overrides))
      (intern (concat ":" name))))

(defun shexc-shexr--sort-keys (keys)
  "KEYS in canonical order: alphabetical, with the \"big nested\"
properties (`shexc-shexr--last-sorted-keys') sorted last."
  (let ((last (seq-filter (lambda (k) (memq k shexc-shexr--last-sorted-keys)) keys))
        (rest (seq-remove (lambda (k) (memq k shexc-shexr--last-sorted-keys)) keys)))
    (append (sort rest (lambda (a b) (string< (symbol-name a) (symbol-name b)))) last)))

(defun shexc-shexr--node-keys (node)
  "NODE's property keys (excluding `:type'/`:context'), in canonical
order.  `:context' is JSON-LD-adapter metadata (see shexc-shexj.el's
@context handling), not real RDF content -- excluded here so the
RDF-graph parser never has to special-case it.  `:id' *is* included
\(rendered via `:id''s `literal-iri' string-mode, see
`shexc-shexr--key-string-mode') -- every node reaching this function
with a non-nil `:id' is, by construction, a hoisted top-level
statement (an inline `[ ... ]' object never has one), so this never
affects an object's *own* rendering, only adds one property to its
hoisted statement."
  (shexc-shexr--sort-keys
   (remq :context (remq :type (shexc-shexr--plist-keys node)))))

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
entry, `ref' for a wrapped `sx:Ref' id-reference, anything else
\(including `literal-iri', the default) a bare `<iri>'/`_:label' -- see
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
  (let ((label (shexc-shexr--key-label key)))
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
  (let ((label (shexc-shexr--key-label key)))
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
;; RDF-graph parser: general Turtle, via `rdf-turtle.el'/`rdf-model.el'/
;; `rdf-store.el' rather than a hand-rolled recursive descent over text.
;; Reads any valid Turtle expressing this vocabulary -- not just this
;; file's own canonical output -- so a hand-edited fence (reordered
;; properties, a different prefix/IRI-shortening choice, reformatted
;; whitespace, ...) still parses correctly, which the old narrow parser
;; (replaced by this section) could not do.
;;
;; Three layers: `shexc-shexr-parse' finds the `a sx:Schema' root quad
;; and kicks off `shexc-shexr--decode-node'; `--decode-node' decodes one
;; RDF subject's triples into a value-tree node (`:type' plus
;; properties, recursing into `--decode-value' for each); `--decode-value'
;; decodes one RDF term per a key's `shexc-shexr--key-string-mode' (the
;; very same dispatch table the serializer above already uses, so the
;; two directions can never disagree about what a key means).
;;
;; `ref'-mode values (`:valueExpr'/`:shapeExpr'/`:expression(s)'/
;; `:shapeExprs'/`:extends'/`:start'/...) are the one case requiring a
;; real decision: a bare reference (no `sx:Ref' wrapper) is dereferenced
;; -- decoded recursively as a full nested node -- while an `sx:Ref'-
;; wrapped one is unwrapped straight to its bare `sx:id' string.
;;
;; Restoring `:id' on a dereferenced node needs its own explicit `sx:id'
;; property -- it can *not* be inferred from whether the node's RDF
;; subject is a NamedNode or a BlankNode.  IRI-shaped `:id's would make
;; that distinction look safe (a purely anonymous inline `[ ... ]'
;; always parses to a fresh blank node, never an IRI), but ShExC also
;; allows a blank-node-labeled shape/tripleExpr id (`_:S2 { ... }',
;; `grammar.js''s `shape_expr_label'/`triple_expr_label' both being
;; `choice($._iri, $.blank_node)') -- and a deliberately-labeled `_:S2'
;; is graph-shape-identical to an anonymous `[ ... ]''s synthetic blank
;; node once parsed (RDF itself draws no such distinction; only Turtle's
;; concrete syntax does, and that's exactly the information real Turtle
;; parsing discards).  So the serializer now emits an explicit `sx:id'
;; property on every hoisted statement (`shexc-shexr--node-keys' no
;; longer excludes `:id'; see `shexc-shexr--key-string-mode''s `:id'
;; case) -- inline `[ ... ]' objects never have one (by construction,
;; only hoisted nodes do), so this is unambiguous and requires no
;; heuristic at decode time: `:id' just falls out of the same generic
;; per-predicate decode loop as any other property.  (The Schema root
;; itself never has `:id' either way -- `shexc-shexr-parse' below builds
;; the Schema's own wrapper explicitly rather than via dereference.)
;;
;; List-vs-scalar needs no hardcoded "which ShExJ keys are arrays"
;; table: a property's RDF shape says so directly.  Exactly one quad
;; whose object isn't an RDF-list node (no `rdf:first', not `rdf:nil')
;; -> scalar.  Otherwise (more than one quad sharing that predicate, or
;; the one object *is* a list node) -> a list: walk the
;; `rdf:first'/`rdf:rest' chain for a single list-node object (this
;; serializer's own convention, preserving order); else decode each
;; quad's object independently (a tolerated, non-canonical repeated-
;; triple form -- real RDF has no inherent order for that shape anyway,
;; so this only ever matters for hand-edited input, never this file's
;; own output).  `:extra' is the one property always encoded as
;; repeated `sx:extra' triples, never an RDF list (matching the
;; serializer's own `shexc-shexr--flat-prop'/`--pretty-prop' special
;; case for it), so it's decoded that way unconditionally.

(define-error 'shexc-shexr-parse-error "ShExR parse error")

(defconst shexc-shexr--context-iri "http://www.w3.org/ns/shex.jsonld"
  "Duplicates the literal in `shexc-shexj--context-iri' -- kept local
rather than depending on shexc-shexj.el for one string, per this file's
existing \"no dependency on shexc-shexj beyond the value-tree's shape\"
boundary (see this file's Commentary).")

(defconst shexc-shexr--sx-ns "http://www.w3.org/ns/shex#")
(defconst shexc-shexr--rdf-ns "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(defconst shexc-shexr--rdf-type (rdf-model-named-node-create :value (concat shexc-shexr--rdf-ns "type")))
(defconst shexc-shexr--rdf-first (rdf-model-named-node-create :value (concat shexc-shexr--rdf-ns "first")))
(defconst shexc-shexr--rdf-rest (rdf-model-named-node-create :value (concat shexc-shexr--rdf-ns "rest")))
(defconst shexc-shexr--rdf-nil (rdf-model-named-node-create :value (concat shexc-shexr--rdf-ns "nil")))
(defconst shexc-shexr--sx-schema (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "Schema")))
(defconst shexc-shexr--sx-ref (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "Ref")))
(defconst shexc-shexr--sx-id (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "id")))
(defconst shexc-shexr--sx-extra (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "extra")))

(defconst shexc-shexr--omit '--shexc-shexr-omit--
  "Sentinel `shexc-shexr--decode-value' returns for a `false'-valued
non-`value'-mode boolean, meaning \"omit this property entirely\" --
the existing value-tree convention for these (`:closed'/`:abstract'/
`:inverse') never represents false as a present-but-nil key, only as
the key's absence.")

(defun shexc-shexr--fail (fmt &rest args)
  "Signal a structured `shexc-shexr-parse-error', never an uncaught
generic Lisp error -- callers (e.g. a flymake backend) can catch this
and report a message without crashing."
  (signal 'shexc-shexr-parse-error (list (apply #'format fmt args))))

(defun shexc-shexr--type-local-name (type-term)
  "TYPE-TERM is the object of a subject's `rdf:type' quad -- its local
name after stripping the `sx:' namespace, or a `shexc-shexr--fail' if
it isn't a NamedNode under that namespace at all."
  (unless (and (rdf-model-named-node-p type-term)
               (string-prefix-p shexc-shexr--sx-ns (rdf-model-named-node-value type-term)))
    (shexc-shexr--fail "expected an `sx:'-namespaced rdf:type, got %S" type-term))
  (substring (rdf-model-named-node-value type-term) (length shexc-shexr--sx-ns)))

(defun shexc-shexr--term-ref-text (term)
  "TERM (a NamedNode or BlankNode) as the bare reference string this
value-tree convention uses: the IRI itself, or `_:label' for a blank
node -- the inverse of `shexc-shexr--ref-text'."
  (cond
   ((rdf-model-named-node-p term) (rdf-model-named-node-value term))
   ((rdf-model-blank-node-p term) (concat "_:" (rdf-model-blank-node-value term)))
   (t (shexc-shexr--fail "expected an IRI or blank node, got %S" term))))

(defun shexc-shexr--quads-by-predicate (quads)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (q quads) (push q (gethash (rdf-model-quad-predicate q) table)))
    table))

(defun shexc-shexr--list-node-p (store term)
  "Whether TERM is the head of an RDF list -- `rdf:nil' itself, or
something with its own `rdf:first' quad."
  (or (equal term shexc-shexr--rdf-nil)
      (rdf-model-dataset-match store term shexc-shexr--rdf-first)))

(defun shexc-shexr--decode-rdf-list (store mode term)
  "Walk the `rdf:first'/`rdf:rest' chain headed at TERM, decoding each
item per MODE via `shexc-shexr--decode-value'."
  (let (items)
    (while (not (equal term shexc-shexr--rdf-nil))
      (let ((first (rdf-model-dataset-match store term shexc-shexr--rdf-first))
            (rest (rdf-model-dataset-match store term shexc-shexr--rdf-rest)))
        (unless (and (= (length first) 1) (= (length rest) 1))
          (shexc-shexr--fail "malformed RDF list at %S" term))
        (push (shexc-shexr--decode-value store mode (rdf-model-quad-object (car first))) items)
        (setq term (rdf-model-quad-object (car rest)))))
    (nreverse items)))

(defun shexc-shexr--decode-literal (lit)
  "LIT (an `rdf-model-literal') as a value-set/Annotation-object entry
plist (`:value' plus `:type'/`:language' when not the RDF-implicit
default) -- the inverse of `shexc-shexr--serialize-literal'."
  (let* ((value (rdf-model-literal-value lit))
         (lang (rdf-model-literal-language lit))
         (dt (rdf-model-literal-datatype lit))
         (dt-iri (and dt (rdf-model-named-node-value dt))))
    (cond
     (lang (list :value value :language lang))
     ((and dt-iri (not (string= dt-iri shexc-shexr--xsd-string-iri)))
      (list :value value :type dt-iri))
     (t (list :value value)))))

(defun shexc-shexr--numeric-datatype-p (iri)
  (member iri '("http://www.w3.org/2001/XMLSchema#integer"
                "http://www.w3.org/2001/XMLSchema#decimal"
                "http://www.w3.org/2001/XMLSchema#double")))

(defun shexc-shexr--decode-value (store mode term)
  "Decode one RDF TERM per MODE (mirrors `shexc-shexr--key-string-mode')."
  (let ((dt (and (rdf-model-literal-p term) (rdf-model-literal-datatype term))))
    (cond
     ;; Numbers/non-`value'-mode booleans are recognized by the term's
     ;; own shape before MODE is consulted -- mirrors `shexc-shexr--flat-value'.
     ((and dt (not (eq mode 'value)) (shexc-shexr--numeric-datatype-p (rdf-model-named-node-value dt)))
      (string-to-number (rdf-model-literal-value term)))
     ((and dt (not (eq mode 'value)) (string= (rdf-model-named-node-value dt) shexc-shexr--xsd-boolean-iri))
      (if (string= (rdf-model-literal-value term) "true") t shexc-shexr--omit))
     ;; `literal-iri' is read as plain identifier text unconditionally,
     ;; *never* dereferenced -- unlike every other mode below, it must
     ;; not even check the term's own `rdf:type': a `:predicate'/
     ;; `:datatype'/`:id' value's very IRI is always also some node's
     ;; subject elsewhere in the graph (`:id' especially -- it names the
     ;; node it's a property of), so treating it as dereferenceable
     ;; would self-recurse forever.  See `shexc-shexr--key-string-mode'.
     ((eq mode 'literal-iri) (shexc-shexr--term-ref-text term))
     ((rdf-model-literal-p term)
      (cond
       ((eq mode 'plain) (rdf-model-literal-value term))
       ((eq mode 'value) (shexc-shexr--decode-literal term))
       (t (shexc-shexr--fail "expected a reference, got literal %S" term))))
     (t
      ;; A non-literal term can itself be a typed object (`Language'/
      ;; `IriStem'/`LiteralStemRange'/`Wildcard'/...), not just a bare
      ;; IRI/blank-node reference -- dereferenced regardless of MODE,
      ;; mirroring `shexc-shexr--flat-value''s unconditional
      ;; object-inlining for any plist-valued property (e.g. a `:stem'
      ;; that's a `Wildcard' even though `:stem' is otherwise `plain'
      ;; mode, or an Iri-flavored `:exclusions' entry -- `value' mode --
      ;; that's an `IriStem').  Only `ref' mode additionally recognizes
      ;; the `sx:Ref' wrapper -- a `value'-mode entry is never
      ;; `sx:Ref'-wrapped (no same-document-object ambiguity to guard
      ;; against -- see this file's existing Commentary on why
      ;; value-set entries are never `:id'-bearing).
      (let ((type-quads (rdf-model-dataset-match store term shexc-shexr--rdf-type)))
        (cond
         ((null type-quads) (shexc-shexr--term-ref-text term))
         ((and (eq mode 'ref) (equal (rdf-model-quad-object (car type-quads)) shexc-shexr--sx-ref))
          (let ((id-quads (rdf-model-dataset-match store term shexc-shexr--sx-id)))
            (unless (= (length id-quads) 1) (shexc-shexr--fail "sx:Ref node missing sx:id at %S" term))
            (shexc-shexr--decode-value store 'literal-iri (rdf-model-quad-object (car id-quads)))))
         (t (shexc-shexr--decode-node store term))))))))

(defvar shexc-shexr--decode-memo nil
  "Dynamically bound by `shexc-shexr-parse' to a hash table memoizing
`shexc-shexr--decode-node' by subject term, both for efficiency (a
node referenced from multiple places is only decoded once) and to
guard against a cyclic reference (the in-progress sentinel below).")

(defconst shexc-shexr--decode-in-progress '--shexc-shexr-decode-in-progress--)
(defconst shexc-shexr--decode-not-memoized '--shexc-shexr-decode-not-memoized--)

(defun shexc-shexr--decode-node-properties (store subject)
  "SUBJECT's properties in STORE (including `:id', if it has an explicit
`sx:id' property), excluding its `rdf:type' (returned separately as
TYPE-NAME) -- shared by `shexc-shexr--decode-node' (which adds
`:type') and `shexc-shexr-parse' (which builds the Schema's own
wrapper instead).  Returns (TYPE-NAME . PROPERTY-PLIST)."
  (let* ((quads (rdf-model-dataset-match store subject))
         (type-quads (seq-filter (lambda (q) (equal (rdf-model-quad-predicate q) shexc-shexr--rdf-type)) quads))
         (other-quads (seq-remove (lambda (q) (equal (rdf-model-quad-predicate q) shexc-shexr--rdf-type)) quads)))
    (unless (= (length type-quads) 1)
      (shexc-shexr--fail "expected exactly one rdf:type for %S, found %d" subject (length type-quads)))
    (let* ((type-name (shexc-shexr--type-local-name (rdf-model-quad-object (car type-quads))))
           (by-predicate (shexc-shexr--quads-by-predicate other-quads))
           result)
      (maphash
       (lambda (predicate matching-quads)
         (if (equal predicate shexc-shexr--sx-extra)
             (setq result
                   (plist-put result :extra
                              (mapcar (lambda (q) (shexc-shexr--decode-value store 'literal-iri (rdf-model-quad-object q)))
                                      matching-quads)))
           (let* ((key (shexc-shexr--key-from-label-name
                        (substring (rdf-model-named-node-value predicate) (length shexc-shexr--sx-ns))))
                  (mode (shexc-shexr--key-string-mode type-name key))
                  (objects (mapcar #'rdf-model-quad-object matching-quads))
                  (value (if (and (= (length objects) 1) (not (shexc-shexr--list-node-p store (car objects))))
                             (shexc-shexr--decode-value store mode (car objects))
                           (if (= (length objects) 1)
                               (shexc-shexr--decode-rdf-list store mode (car objects))
                             (mapcar (lambda (o) (shexc-shexr--decode-value store mode o)) objects)))))
             (unless (eq value shexc-shexr--omit)
               (setq result (plist-put result key value))))))
       by-predicate)
      (cons type-name result))))

(defun shexc-shexr--decode-node (store subject)
  "Decode SUBJECT's triples in STORE into a value-tree node: `:type'
\(from its one mandatory `rdf:type') plus every other property,
including `:id' if SUBJECT has an explicit `sx:id' property (see
`shexc-shexr--key-string-mode''s `:id' case for why this needs to be a
real property, not inferred from SUBJECT's own IRI-vs-blank-node
shape -- a deliberately blank-node-labeled `:id' \(ShExC's `_:S2 {
... }') and a purely anonymous inline `[ ... ]' are graph-shape-
identical otherwise)."
  (let ((memoized (gethash subject shexc-shexr--decode-memo shexc-shexr--decode-not-memoized)))
    (cond
     ((eq memoized shexc-shexr--decode-in-progress)
      (shexc-shexr--fail "cyclic ShExR reference involving %S" subject))
     ((not (eq memoized shexc-shexr--decode-not-memoized)) memoized)
     (t
      (puthash subject shexc-shexr--decode-in-progress shexc-shexr--decode-memo)
      (pcase-let* ((`(,type-name . ,props) (shexc-shexr--decode-node-properties store subject))
                   (result (append (list :type type-name) props)))
        (puthash subject result shexc-shexr--decode-memo)
        result)))))

;;;###autoload
(defun shexc-shexr-parse (text)
  "Parse TEXT (ShExR Turtle -- any valid Turtle expressing the `sx:'
vocabulary, not just this file's own canonical output) into a
value-tree, the inverse of `shexc-shexr-serialize'.  Signals a
`shexc-shexr-parse-error' (never an uncaught generic Lisp error) on
malformed input."
  (condition-case err
      (let* ((shexc-shexr--decode-memo (make-hash-table :test #'equal))
             (store (rdf-turtle-parse-string text))
             (schema-quads (rdf-model-dataset-match store nil shexc-shexr--rdf-type shexc-shexr--sx-schema)))
        (unless (= (length schema-quads) 1)
          (shexc-shexr--fail "expected exactly one `a sx:Schema' statement, found %d" (length schema-quads)))
        (pcase-let* ((`(,_type-name . ,props)
                      (shexc-shexr--decode-node-properties store (rdf-model-quad-subject (car schema-quads)))))
          (append (list :context shexc-shexr--context-iri :type "Schema") props)))
    (shexc-shexr-parse-error (signal (car err) (cdr err)))
    (error (signal 'shexc-shexr-parse-error (list (error-message-string err))))))

(provide 'shexc-shexr)

;;; shexc-shexr.el ends here
