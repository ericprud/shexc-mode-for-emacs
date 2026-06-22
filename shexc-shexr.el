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
;;   statement.  `:id' is *never* rendered as an RDF triple -- neither
;;   `sx:id' nor `sx:Ref' are real ShExR vocabulary (confirmed: neither
;;   appears in any shexSpec/shexTest reference `.ttl'), so this
;;   serializer never emits them, for any type.  A hoisted ShapeDecl's
;;   `:id' is always recoverable on decode as the statement's own
;;   subject, since `rdf:type sx:ShapeDecl' is itself an unambiguous,
;;   never-anonymous marker (see `shexc-shexr--node-keys').  A hoisted
;;   TripleExpr's `:id' (TripleConstraint/EachOf/OneOf, from an explicit
;;   ShExC `$<id>' annotation) has no such shortcut -- the same RDF shape
;;   can equally be genuinely anonymous inline content for these types --
;;   so it's recovered only when the subject is found to be referenced
;;   from 2+ places in the graph, with an external ShExPath-keyed table
;;   (see shexc-shexpath.el) resolving *which* occurrence is the
;;   definition when more than one candidate exists (falling back to the
;;   textually-first occurrence with no table) -- see
;;   `shexc-shexr--ambiguous-id-winners'.
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
;;   `valueExpr'/`shapeExpr'/`shapeExprs'/`extends'/`start' are likewise
;;   always a bare `<iri>'/`_:label' when they hold a string at all --
;;   those properties only ever reference a ShapeDecl (never embed one),
;;   and a ShapeDecl is always unambiguously identifiable by its own
;;   `rdf:type'.  `expression(s)' (a bare string there references a
;;   *TripleExpr*, which -- unlike ShapeDecl -- can equally be genuinely
;;   anonymous inline content of the very same RDF shape) is also always
;;   a bare `<iri>'/`_:label' -- there is no wrapper at all to mark an
;;   Include reference apart from a definition; an external ShExPath
;;   table resolves that ambiguity when it actually arises (2+ bare
;;   references to the same hoisted TripleExpr) -- see
;;   `shexc-shexr--key-string-mode' and `shexc-shexr--ambiguous-id-winners'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pcase)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)
(require 'shexc-shexpath)

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
  '(:code :pattern :flags :languageTag :stem)
  "Property keys whose string value is canonical plain text (a Turtle
string literal), never an IRI/blank-node-label reference.  `:stem' is
here unconditionally -- per ShExR.shex, `sx:stem' is plain `xsd:string'
for every stem flavor (Iri/Literal/Language alike); only `:exclusions'
entries actually vary by flavor (see `shexc-shexr--plain-text-types').
`:nodeKind' is NOT here, despite also being a bare keyword string in
the value-tree (\"iri\"/\"bnode\"/\"literal\"/\"nonliteral\") -- per
ShExR.shex it's one of the four fixed `sx:iri'/`sx:bnode'/`sx:literal'/
`sx:nonliteral' IRIs, not a string literal (confirmed against every
`sx:nodeKind' occurrence in shexSpec/shexTest's own `.ttl' fixtures);
see the dedicated `node-kind' string-mode below.")

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
term's own `rdf:type'), `shape-decl-ref' for a bare `<iri>'/`_:label'
that always references a ShapeDecl, `ref' (the default) for a bare
`<iri>'/`_:label' that may reference a hoisted TripleExpr.
`shape-decl-ref' and `ref' render *identically* on encode (always a
plain bare reference -- there is no wrapper of any kind; see this
file's Commentary on why `sx:Ref' is never emitted) and differ only in
how `shexc-shexr--decode-value' resolves a dereference.

`:predicate'/`:datatype'/`:id' are always `literal-iri': ShExR.shex
types `:predicate'/`:datatype' as plain `IRI', with no shapeExpr/
ShapeDecl alternative.  `:id' is never itself a nested object, so it's
never ambiguous either -- but `:id' never actually reaches this
function in practice, since `shexc-shexr--node-keys' excludes it from
every node's rendered properties unconditionally (kept `literal-iri'
here only so a future caller that did pass it would get a safe,
non-dereferencing mode rather than silently mis-rendering it).  This
also means `literal-iri' must never be used for a key whose value could
itself be a same-document node's *own identifying IRI* (decoding `:id'
as `ref'- or `value'-style dereference would self-recurse forever,
since that very IRI is always also some node's subject elsewhere).

`:valueExpr'/`:shapeExpr'/`:shapeExprs'/`:extends'/`:start' are
`shape-decl-ref': their union types allow a same-document hoisted
ShapeDecl's id to appear there (confirmed empirically via
kitchenSink.shex and _all.shex) but never anything else string-valued,
and a ShapeDecl is always unambiguously identifiable by its own
`rdf:type sx:ShapeDecl' -- never genuinely anonymous inline content,
unlike a TripleExpr -- so the decoder can tell \"reference\" apart from
\"inline embed\" with no marker at all: see `shexc-shexr--decode-value'.
`:expression'/`:expressions' alone keep `ref', since a bare string
there references a TripleExpr, which -- unlike ShapeDecl -- really can
also appear as genuinely anonymous inline content of the very same RDF
shape; when the *same* TripleExpr subject turns out to be referenced
from 2+ places this way, `shexc-shexr--ambiguous-id-winners' (consulting
an external ShExPath-keyed table, or falling back to the textually-
first occurrence) decides which one is the definition -- see
shexc-shexpath.el.

Iri-flavored `:exclusions' entries (the complement of
`shexc-shexr--plain-text-types') are `value', not `literal-iri', even
though a bare entry renders identically to `literal-iri' \(both fall to
the same bare-`<iri>'/`_:label' case in `shexc-shexr--flat-value';
they differ only for a non-bare entry -- an Iri-flavored `:exclusions'
entry can itself be a typed `IriStem'/`IriStemRange' object \(e.g.
`1val1dotMinusiriStem3.shex'), which `value' mode's decoder correctly
dereferences and `literal-iri' deliberately never does.

`:nodeKind' is its own `node-kind' mode: its value-tree string
\(\"iri\"/\"bnode\"/\"literal\"/\"nonliteral\") renders as the fixed
`sx:iri'/`sx:bnode'/`sx:literal'/`sx:nonliteral' IRI, per ShExR.shex --
see `shexc-shexr--plain-text-keys'."
  (cond
   ((memq key shexc-shexr--plain-text-keys) 'plain)
   ((eq key :exclusions)
    (if (member type shexc-shexr--plain-text-types) 'plain 'value))
   ((memq key '(:predicate :datatype :id)) 'literal-iri)
   ((eq key :nodeKind) 'node-kind)
   ((memq key '(:values :object)) 'value)
   ((memq key '(:valueExpr :shapeExpr :shapeExprs :extends :start)) 'shape-decl-ref)
   (t 'ref)))

;; ---------------------------------------------------------------------
;; General value/body serialization
;; ---------------------------------------------------------------------

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
  "NODE's property keys (excluding `:type'/`:context'/`:id'), in
canonical order.  `:context' is JSON-LD-adapter metadata (see
shexc-shexj.el's @context handling), not real RDF content.  `:id' is
never rendered as an RDF triple -- neither `sx:id' nor `sx:Ref' are real
ShExR vocabulary (see this file's Commentary), so a node's `:id', when
present, is recovered on decode purely from the node being found to be
a hoisted top-level statement at all -- unconditionally for a
ShapeDecl, or (for a TripleConstraint/EachOf/OneOf) only when that
subject is independently found to be referenced from 2+ places -- see
`shexc-shexr--decode-node-properties'/`shexc-shexr--ambiguous-id-winners'.
Excluding both here means the RDF-graph parser never has to special-
case either."
  (shexc-shexr--sort-keys (remq :id (remq :context (remq :type (shexc-shexr--plist-keys node))))))

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
IRI, ...) between hops, but the trailing characters of V's own literal
text -- e.g. a PNAME's local part, the tail of an IRI -- stay the same,
so measuring from the end survives that change where measuring from
the start wouldn't (see `shexc-ts-mode-convert--locate-target').
TRAILING-SKIP lets a caller
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
for a Turtle string literal, anything else (including `literal-iri',
`shape-decl-ref', `ref' -- the default) a bare `<iri>'/`_:label' -- see
`shexc-shexr--key-string-mode'."
  (cond
   ((eq v t) (shexc-shexr--lit "true"))
   ((numberp v) (shexc-shexr--lit (number-to-string v)))
   ((stringp v)
    (pcase string-mode
      ('plain (shexc-shexr--mark v (shexc-shexr--lit (shexc-shexr--turtle-string v)) 1)) ; skip closing quote
      ('node-kind (shexc-shexr--mark v (shexc-shexr--lit (concat "sx:" v))))
      (_ (shexc-shexr--mark-ref v))))
   ((shexc-shexr--literal-value-p v) (shexc-shexr--lit (shexc-shexr--serialize-literal v)))
   ((and (consp v) (keywordp (car v)))
    (if (plist-get v :id)
        ;; V is itself hoisted to its own top-level statement (see
        ;; `shexc-shexr--all-hoisted'), so this is just a bare reference
        ;; to it, not V's own identity -- deliberately *not* marked
        ;; (`shexc-shexr--lit', not `--mark-ref'): V's `:id' string is
        ;; the very same `eq' value rendered again, unmarked-vs-marked,
        ;; every place V is referenced from, so marking here too would
        ;; make whichever reference happens to render first (almost
        ;; always *not* V's own hoisted statement, which always renders
        ;; last -- see `shexc-shexr-serialize') win over V's own `:id'
        ;; property, which is where a target identified by that exact
        ;; string should actually land.
        (shexc-shexr--lit (shexc-shexr--ref-text (plist-get v :id)))
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

(defun shexc-shexr--pretty-value (v string-mode indent col width)
  (cond
   ((eq v t) (shexc-shexr--lit "true"))
   ((numberp v) (shexc-shexr--lit (number-to-string v)))
   ((stringp v) (pcase string-mode
                  ('plain (shexc-shexr--mark v (shexc-shexr--lit (shexc-shexr--turtle-string v)) 1))
                  ('node-kind (shexc-shexr--mark v (shexc-shexr--lit (concat "sx:" v))))
                  (_ (shexc-shexr--mark-ref v))))
   ((shexc-shexr--literal-value-p v) (shexc-shexr--lit (shexc-shexr--serialize-literal v)))
   ((and (consp v) (keywordp (car v)))
    (if (plist-get v :id)
        ;; See the identical branch in `shexc-shexr--flat-value' for why
        ;; this is deliberately unmarked.
        (shexc-shexr--lit (shexc-shexr--ref-text (plist-get v :id)))
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
;; `shape-decl-ref'-mode values (`:valueExpr'/`:shapeExpr'/`:shapeExprs'/
;; `:extends'/`:start') need a real decision (reference vs. embed) but
;; have a free, always-available signal for it: a bare reference whose
;; dereferenced `rdf:type' is `sx:ShapeDecl' is always just a reference
;; (ShapeDecl is never a valid embedded value for these properties),
;; anything else is dereferenced.
;;
;; `ref'-mode values (`:expression'/`:expressions') need the same
;; decision but have no such free signal -- a TripleExpr
;; (TripleConstraint/EachOf/OneOf) genuinely can be either a reference to
;; a separately-defined, separately-hoisted sibling occurrence (a ShExC
;; `&<id>' Include) or perfectly ordinary anonymous inline content, and
;; the two are graph-shape-*identical*: Turtle's `[ ... ]' object syntax
;; and "a separate top-level statement plus a bare reference" produce
;; the exact same triples once parsed, so there is no marker -- in the
;; graph or in the concrete Turtle syntax -- capable of telling them
;; apart in general.  Neither is there one for which of several bare
;; references to the *same* subject is the original `$<id> (...)'
;; definition versus an Include.  Resolving this needs information from
;; outside the graph entirely: `shexc-shexr--ambiguous-id-winners'
;; consults an external table (`shexc-shexr--id-location-table', keyed
;; by ShExPath strings -- see shexc-shexpath.el) recording where each
;; id's definition was when last compiled from ShExC, falling back to
;; the textually-first occurrence when there's no table or no matching
;; entry.  `shexc-shexr--decode-node-properties' consults this *before*
;; `--decode-value' ever reaches its fully-dereferencing fallthrough, so
;; only the winning occurrence calls `shexc-shexr--decode-node' at all --
;; every losing occurrence resolves to a bare reference string instead.
;;
;; `:id' itself is consequently *never* read off an RDF property -- it
;; is synthesized from the subject's own identity, unconditionally for a
;; ShapeDecl (never a "purely anonymous inline" possibility in the first
;; place -- every `:shapes' entry is compiled with an `:id', always --
;; so `rdf:type sx:ShapeDecl' alone already unambiguously marks a hoisted
;; statement as one) and, for a TripleConstraint/EachOf/OneOf, only when
;; `shexc-shexr--ambiguous-id-winners' has independently established
;; that subject is referenced from 2+ places (a TripleExpr referenced
;; from 0 or 1 places needs no `:id' at all -- see that function's
;; docstring for the one accepted, narrow loss of fidelity this implies).
;; (The Schema root itself never has `:id' either way -- `shexc-shexr-parse'
;; below builds the Schema's own wrapper explicitly rather than via
;; dereference.)
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
(defconst shexc-shexr--sx-shape-decl (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "ShapeDecl")))
(defconst shexc-shexr--sx-extra (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "extra")))
(defconst shexc-shexr--sx-shapes (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "shapes")))
(defconst shexc-shexr--sx-shape-expr (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "shapeExpr")))
(defconst shexc-shexr--sx-shape-exprs (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "shapeExprs")))
(defconst shexc-shexr--sx-value-expr (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "valueExpr")))
(defconst shexc-shexr--sx-expression (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "expression")))
(defconst shexc-shexr--sx-expressions (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "expressions")))
(defconst shexc-shexr--sx-predicate-prop (rdf-model-named-node-create :value (concat shexc-shexr--sx-ns "predicate")))

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

(defun shexc-shexr--decode-rdf-list (store mode term &optional subject)
  "Walk the `rdf:first'/`rdf:rest' chain headed at TERM, decoding each
item per MODE via `shexc-shexr--decode-value'.  SUBJECT, when non-nil
\(only ever passed for MODE `ref'), is the containing node whose
property this list is -- combined with each item's own 0-based position
in the list into that item's occurrence identity, for
`shexc-shexr--ambiguous-id-winners'."
  (let (items (position 0))
    (while (not (equal term shexc-shexr--rdf-nil))
      (let ((first (rdf-model-dataset-match store term shexc-shexr--rdf-first))
            (rest (rdf-model-dataset-match store term shexc-shexr--rdf-rest)))
        (unless (and (= (length first) 1) (= (length rest) 1))
          (shexc-shexr--fail "malformed RDF list at %S" term))
        (push (shexc-shexr--decode-value store mode (rdf-model-quad-object (car first))
                                          (and subject (cons subject position)))
              items)
        (setq term (rdf-model-quad-object (car rest)))
        (setq position (1+ position))))
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

;; ---------------------------------------------------------------------
;; Resolving `ref'-mode ambiguity: a graph-side walk, the structural
;; mirror of `shexc-shexpath-id-locations' (driven by quads instead of
;; plist traversal), finding every place a `ref'-mode property
;; (`:expression'/`:expressions') points at a TripleExpr subject, then
;; grouping by that subject to find which ones are referenced from 2+
;; places -- the genuine ambiguity an in-graph marker can't resolve
;; (see this file's Commentary).  Run once per `shexc-shexr-parse' call,
;; before the main decode walk.
;; ---------------------------------------------------------------------

(defvar shexc-shexr--id-location-table nil
  "Alist (ID-STRING . SHEXPATH-STRING), let-bound by a caller of
`shexc-shexr-parse' -- see `shexc-shexpath-id-locations'.  Consulted
only when the graph being decoded has genuine multi-occurrence
ambiguity for some hoisted TripleExpr id; nil, or simply missing an
entry for some id, means \"no hint -- pick the textually-first
occurrence instead\" (see `shexc-shexr--ambiguous-id-winners').")

(defvar shexc-shexr--ambiguous-ids nil
  "Hash set (`equal'-keyed, value t), let-bound by `shexc-shexr-parse'
around the main decode walk: every RDF term referenced (`ref'-mode)
from 2+ occurrences in the graph being decoded.  nil (the default, for
every caller that never binds it) means \"no ambiguity at all -- every
`ref'-mode reference always embeds\", exactly today's pre-Part-C
behavior.  See `shexc-shexr--ambiguous-id-winners'.")

(defvar shexc-shexr--ambiguous-winners nil
  "Hash set (`equal'-keyed, value t) of winning occurrence identities,
let-bound alongside `shexc-shexr--ambiguous-ids'.  An occurrence
identity is `(CONTAINING-SUBJECT . POSITION)', POSITION nil for a
scalar `:expression' or the 0-based position of an `:expressions'
RDF-list/repeated-triple element -- see `shexc-shexr--decode-value'/
`shexc-shexr--decode-rdf-list'.")

(defun shexc-shexr--term-type-name (store term)
  "TERM's `rdf:type' local name (after stripping the `sx:' namespace), or
nil if TERM has no exactly-one `sx:'-namespaced `rdf:type' quad, or
isn't a NamedNode/BlankNode at all -- a lenient probe for this section's
graph walk, unlike `shexc-shexr--type-local-name' (used by the real
decode walk, which `shexc-shexr--fail's on anything unexpected): this
pre-pass only *scans* for ref-mode ambiguity, and a hand-edited,
possibly-malformed graph should never raise an error here before the
real decode walk gets a chance to give a more specific one."
  (and (or (rdf-model-named-node-p term) (rdf-model-blank-node-p term))
       (let ((type-quads (rdf-model-dataset-match store term shexc-shexr--rdf-type)))
         (and (= (length type-quads) 1)
              (rdf-model-named-node-p (rdf-model-quad-object (car type-quads)))
              (string-prefix-p shexc-shexr--sx-ns (rdf-model-named-node-value (rdf-model-quad-object (car type-quads))))
              (substring (rdf-model-named-node-value (rdf-model-quad-object (car type-quads)))
                         (length shexc-shexr--sx-ns))))))

(defun shexc-shexr--shexpath-label (term)
  "TERM as a ShExPath step label (`shexc-shexpath-make-shape-step'/
`-make-predicate-step''s LABEL argument): `<iri>'/`_:label', never
shortened against a prefix/base table -- this is an internal format,
not user-facing ShExC text.  Mirrors `shexc-shexpath--ref-label', kept
local for the same reason `shexc-shexr--context-iri' duplicates
`shexc-shexj--context-iri' rather than reaching into that file's
internals."
  (let ((ref (shexc-shexr--term-ref-text term)))
    (if (string-prefix-p "_:" ref) ref (concat "<" ref ">"))))

(defun shexc-shexr--rdf-list-cells (store term)
  "Raw `rdf:first'/`rdf:rest' walk headed at TERM, returning a list of
\(OBJECT . FIRST-QUAD) pairs in list order -- like
`shexc-shexr--decode-rdf-list' but undecoded (to avoid this pre-pass
depending on the very decode it's helping to disambiguate) and
additionally exposing each cell's own `rdf:first' quad, needed by
`shexc-shexr--ambiguous-id-winners' to rank an occurrence by its actual
source position (see `shexc-shexr--quad-document-rank').  Lenient like
`shexc-shexr--term-type-name': returns nil on any malformed list shape
instead of failing."
  (let (items)
    (catch 'malformed
      (while (not (equal term shexc-shexr--rdf-nil))
        (let ((first (rdf-model-dataset-match store term shexc-shexr--rdf-first))
              (rest (rdf-model-dataset-match store term shexc-shexr--rdf-rest)))
          (unless (and (= (length first) 1) (= (length rest) 1))
            (throw 'malformed nil))
          (push (cons (rdf-model-quad-object (car first)) (car first)) items)
          (setq term (rdf-model-quad-object (car rest)))))
      (nreverse items))))

(defun shexc-shexr--property-objects (store term predicate)
  "TERM's objects for PREDICATE in STORE, normalized exactly like
`shexc-shexr--decode-node-properties' does: one quad whose object isn't
an RDF-list node -> a single scalar (POSITION nil); otherwise a list,
walked via `shexc-shexr--rdf-list-cells' for this serializer's own
canonical single-list-node shape, or each matching quad independently
for the tolerated non-canonical \"repeated triple, no RDF list\" shape.
Returns a list of (OBJECT POSITION QUAD), POSITION nil for the scalar
case else a 0-based integer, QUAD the specific originating quad (used
only by `shexc-shexr--ambiguous-id-winners''s \"textually-first
occurrence\" ranking).

The repeated-triple case is explicitly reversed before assigning
positions: a direct single-predicate `rdf-model-dataset-match' call
returns the store's own reverse-document order, but the real decode
walk (`shexc-shexr--decode-node-properties') reaches this same quad set
via an *all-predicates* `dataset-match' bucketed by
`shexc-shexr--quads-by-predicate' (which `push'es) -- reversing the
direct-match order a second time, netting out to plain document order.
This function must reverse explicitly to match that."
  (let* ((quads (rdf-model-dataset-match store term predicate))
         (objects (mapcar #'rdf-model-quad-object quads)))
    (cond
     ((null objects) nil)
     ((and (= (length objects) 1) (not (shexc-shexr--list-node-p store (car objects))))
      (list (list (car objects) nil (car quads))))
     ((= (length objects) 1)
      (let ((cells (shexc-shexr--rdf-list-cells store (car objects))) (i 0) result)
        (dolist (cell cells)
          (push (list (car cell) i (cdr cell)) result)
          (setq i (1+ i)))
        (nreverse result)))
     (t
      (let ((i 0) result)
        (dolist (q (reverse quads))
          (push (list (rdf-model-quad-object q) i q) result)
          (setq i (1+ i)))
        (nreverse result))))))

(defun shexc-shexr--shape-expr-occurrences (store term path occurrences visited)
  "Graph-side mirror of `shexc-shexpath--walk-shape-expr': TERM is the
object of a `shape-decl-ref'-mode property; walk it for `ref'-mode
occurrences reachable through it, at the current ShExPath PATH (steps
in reverse order).  Mutates OCCURRENCES (an `equal'-keyed hash table
mapping occurrence identity to (TERM PATH-STRING QUAD), built by
`shexc-shexr--ref-mode-occurrences') and VISITED (a cycle/dedup guard,
keyed by TripleExpr subject)."
  (let ((type-name (shexc-shexr--term-type-name store term)))
    (cond
     ((equal type-name "Shape")
      (shexc-shexr--shape-property-occurrences store term path occurrences visited))
     ((member type-name '("ShapeAnd" "ShapeOr"))
      (dolist (triple (shexc-shexr--property-objects store term shexc-shexr--sx-shape-exprs))
        (shexc-shexr--shape-expr-occurrences store (nth 0 triple) path occurrences visited)))
     ((equal type-name "ShapeNot")
      (let ((triple (car (shexc-shexr--property-objects store term shexc-shexr--sx-shape-expr))))
        (when triple
          (shexc-shexr--shape-expr-occurrences store (nth 0 triple) path occurrences visited))))
     (t nil)))) ; a bare ShapeDecl reference (dead end -- found via its own top-level walk
                ; regardless of who references it), or NodeConstraint/ShapeExternal --
                ; no `ref'-mode occurrence ever reachable through either.

(defun shexc-shexr--shape-property-occurrences (store term path occurrences visited)
  "TERM is a Shape node; walk its `:expression' (mirrors
`shexc-shexpath--walk-shape-expr''s Shape case)."
  (let ((triple (car (shexc-shexr--property-objects store term shexc-shexr--sx-expression))))
    (when triple
      (shexc-shexr--triple-expr-occurrences
       store (nth 0 triple) term (nth 1 triple) (nth 2 triple) path occurrences visited))))

(defun shexc-shexr--triple-constraint-value-expr-occurrences (store tc-term path occurrences visited)
  "Mirrors `shexc-shexpath--walk-triple-constraint-value-expr': descend
into TC-TERM's `sx:valueExpr' object at the *same* PATH -- transparent
pass-through for an anonymous embedded shape-expr, a dead end for a
bare reference to a separately-labeled ShapeDecl (`rdf:type
sx:ShapeDecl', per `shexc-shexr--term-type-name')."
  (let ((triple (car (shexc-shexr--property-objects store tc-term shexc-shexr--sx-value-expr))))
    (when (and triple (not (equal (shexc-shexr--term-type-name store (nth 0 triple)) "ShapeDecl")))
      (shexc-shexr--shape-expr-occurrences store (nth 0 triple) path occurrences visited))))

(defun shexc-shexr--group-occurrences (store group-term path occurrences visited)
  "Mirrors `shexc-shexpath--walk-group-elements': walk GROUP-TERM's
`sx:expressions' RDF list, recording/recursing via
`shexc-shexr--triple-expr-occurrences' for each element at PATH
extended by the appropriate PredicateStep (a TripleConstraint child, by
its own `sx:predicate', 0-based same-predicate-sibling index) or
ContextStep (an EachOf/OneOf child, by its own type, 0-based
same-context-sibling index)."
  (let ((predicate-counts (make-hash-table :test #'equal))
        (context-counts (make-hash-table :test #'equal)))
    (dolist (triple (shexc-shexr--property-objects store group-term shexc-shexr--sx-expressions))
      (let* ((child (nth 0 triple)) (position (nth 1 triple)) (quad (nth 2 triple))
             (child-type (shexc-shexr--term-type-name store child)))
        (cond
         ((equal child-type "TripleConstraint")
          (let* ((pred-triple (car (shexc-shexr--property-objects store child shexc-shexr--sx-predicate-prop)))
                 (pred-label (and pred-triple (shexc-shexr--shexpath-label (nth 0 pred-triple))))
                 (index (gethash pred-label predicate-counts 0)))
            (puthash pred-label (1+ index) predicate-counts)
            (shexc-shexr--triple-expr-occurrences
             store child group-term position quad
             (cons (shexc-shexpath-make-predicate-step pred-label index) path)
             occurrences visited)))
         ((member child-type '("EachOf" "OneOf"))
          (let ((index (gethash child-type context-counts 0)))
            (puthash child-type (1+ index) context-counts)
            (shexc-shexr--triple-expr-occurrences
             store child group-term position quad
             (cons (shexc-shexpath-make-context-step child-type index) path)
             occurrences visited))))))))

(defun shexc-shexr--triple-expr-occurrences (store term containing-subject position quad path occurrences visited)
  "Record one `ref'-mode occurrence: TERM is referenced from
CONTAINING-SUBJECT's `:expression'/`:expressions' property at POSITION
\(nil for the scalar case), via originating QUAD, reached via PATH
\(steps, reverse order) -- mirrors `shexc-shexpath--walk-triple-expr',
but as a side-effecting graph walk (mutating OCCURRENCES/VISITED)
rather than building an alist, since multiple occurrences of the very
same TERM is exactly the case being detected here, not something to
merge away.  Recurses into TERM's own nested structure (a
TripleConstraint's `:valueExpr', an EachOf/OneOf's `:expressions') only
the first time TERM is seen (VISITED), both to terminate on a cyclic
reference and because TERM's own downstream structure is graph-shared --
identical regardless of which occurrence reached it.  (Accepted, narrow
limitation: if TERM's downstream structure itself contains a further,
independently-ambiguous `:id', the ShExPath recorded for *that* nested
id depends on which occurrence of TERM happened to be visited first,
which is otherwise arbitrary -- this only matters for multi-level
Include ambiguity nested inside other Include ambiguity, never
exercised by any worked example or shexTest fixture found so far.)"
  (puthash (cons containing-subject position)
           (list term (shexc-shexpath-to-string (reverse path)) quad)
           occurrences)
  (unless (gethash term visited)
    (puthash term t visited)
    (let ((type-name (shexc-shexr--term-type-name store term)))
      (cond
       ((equal type-name "TripleConstraint")
        (shexc-shexr--triple-constraint-value-expr-occurrences store term path occurrences visited))
       ((member type-name '("EachOf" "OneOf"))
        (shexc-shexr--group-occurrences store term path occurrences visited))))))

(defun shexc-shexr--ref-mode-occurrences (store schema-subject)
  "Forward walk from SCHEMA-SUBJECT (the `a sx:Schema' root) recording
every `ref'-mode occurrence reachable in STORE -- the graph-side mirror
of `shexc-shexpath-id-locations'.  Returns an `equal'-keyed hash table
mapping occurrence identity to (TERM PATH-STRING QUAD) -- see
`shexc-shexr--triple-expr-occurrences'."
  (let ((occurrences (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'equal)))
    (dolist (triple (shexc-shexr--property-objects store schema-subject shexc-shexr--sx-shapes))
      (let* ((decl (nth 0 triple))
             (path (list (shexc-shexpath-make-shape-step (shexc-shexr--shexpath-label decl))))
             (se-triple (car (shexc-shexr--property-objects store decl shexc-shexr--sx-shape-expr))))
        (when se-triple
          (shexc-shexr--shape-expr-occurrences store (nth 0 se-triple) path occurrences visited))))
    occurrences))

(defun shexc-shexr--quad-document-rank (store)
  "Map each quad in STORE to its index within `(rdf-model-dataset-quads
store)' itself (NOT document order -- `rdf-model-dataset-add' `push'es,
so that raw list is in *reverse* document order; a *larger* rank here
therefore means an *earlier* position in the actual source text).  An
`equal'-keyed hash table.  See `shexc-shexr--ambiguous-id-winners''s
\"textually-first occurrence\" fallback."
  (let ((table (make-hash-table :test #'equal)) (i 0))
    (dolist (q (rdf-model-dataset-quads store)) (puthash q i table) (setq i (1+ i)))
    table))

(defun shexc-shexr--ambiguous-id-winners (store occurrences)
  "Group OCCURRENCES (as built by `shexc-shexr--ref-mode-occurrences') by
referenced TripleExpr term; for every term referenced from 2+
occurrences, resolve exactly one winning occurrence identity --
`shexc-shexr--id-location-table' if it has an entry for that id and
exactly one occurrence's path matches it (`shexc-shexpath-equal'),
otherwise the textually-first occurrence (the candidate whose
originating quad has the highest `shexc-shexr--quad-document-rank').
Returns (AMBIGUOUS-IDS . WINNERS), both `equal'-keyed hash sets (value
t): AMBIGUOUS-IDS by term (consulted by
`shexc-shexr--decode-node-properties' to decide whether to synthesize
`:id' at all), WINNERS by occurrence identity (consulted by
`shexc-shexr--decode-value' to decide whether to embed or leave as a
bare reference)."
  (let ((by-term (make-hash-table :test #'equal))
        (rank (shexc-shexr--quad-document-rank store)))
    (maphash
     (lambda (occurrence-id entry) (push (cons occurrence-id entry) (gethash (nth 0 entry) by-term)))
     occurrences)
    (let ((ambiguous-ids (make-hash-table :test #'equal))
          (winners (make-hash-table :test #'equal)))
      (maphash
       (lambda (term candidates)
         (when (>= (length candidates) 2)
           (puthash term t ambiguous-ids)
           (let* ((table-string (and shexc-shexr--id-location-table
                                      (cdr (assoc (shexc-shexr--term-ref-text term)
                                                   shexc-shexr--id-location-table))))
                  (table-steps (and table-string
                                    (condition-case nil (shexc-shexpath-parse table-string) (error nil))))
                  (table-matches
                   (and table-steps
                        (seq-filter
                         (lambda (c)
                           (condition-case nil
                               (shexc-shexpath-equal (shexc-shexpath-parse (nth 2 c)) table-steps)
                             (error nil)))
                         candidates)))
                  (winner-occurrence-id
                   (if (= (length table-matches) 1)
                       (car (car table-matches))
                     (car (cl-reduce
                           (lambda (a b)
                             (if (> (gethash (nth 3 a) rank -1) (gethash (nth 3 b) rank -1)) a b))
                           candidates)))))
             (puthash winner-occurrence-id t winners))))
       by-term)
      (cons ambiguous-ids winners))))

(defun shexc-shexr--decode-value (store mode term &optional occurrence)
  "Decode one RDF TERM per MODE (mirrors `shexc-shexr--key-string-mode').
OCCURRENCE, when MODE is `ref', is this call's own occurrence identity
\(see `shexc-shexr--ambiguous-id-winners'), consulted only when TERM is
actually ambiguous."
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
     ;; `node-kind' is the fixed `sx:iri'/`sx:bnode'/`sx:literal'/
     ;; `sx:nonliteral' IRI -- read back to its bare local name, never
     ;; dereferenced (it's a closed enum, never a same-document node's
     ;; own id).  See `shexc-shexr--key-string-mode'.
     ((eq mode 'node-kind)
      (let ((iri (shexc-shexr--term-ref-text term)))
        (if (string-prefix-p shexc-shexr--sx-ns iri)
            (substring iri (length shexc-shexr--sx-ns))
          (shexc-shexr--fail "expected an `sx:'-namespaced nodeKind IRI, got %S" term))))
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
      ;; that's an `IriStem').  `shape-decl-ref' additionally treats a
      ;; dereferenced `rdf:type sx:ShapeDecl' as a bare reference rather
      ;; than embedding it -- ShapeDecl is never a valid embedded value
      ;; for these properties (see `shexc-shexr--key-string-mode'), so
      ;; this is unconditionally correct, not a heuristic.  `ref''s
      ;; analogous "is this a reference or an embed" decision for a
      ;; TripleExpr subject referenced from 2+ places has no such
      ;; rdf:type shortcut -- see `shexc-shexr--ambiguous-id-winners',
      ;; consulted by `shexc-shexr--decode-node-properties' before ever
      ;; reaching this fallthrough.
      (let ((type-quads (rdf-model-dataset-match store term shexc-shexr--rdf-type)))
        (cond
         ((null type-quads) (shexc-shexr--term-ref-text term))
         ((and (eq mode 'shape-decl-ref)
               (equal (rdf-model-quad-object (car type-quads)) shexc-shexr--sx-shape-decl))
          (shexc-shexr--term-ref-text term))
         ((and (eq mode 'ref) shexc-shexr--ambiguous-ids
               (gethash term shexc-shexr--ambiguous-ids)
               (not (gethash occurrence shexc-shexr--ambiguous-winners)))
          (shexc-shexr--term-ref-text term))
         (t (shexc-shexr--decode-node store term))))))))

(defvar shexc-shexr--decode-memo nil
  "Dynamically bound by `shexc-shexr-parse' to a hash table memoizing
`shexc-shexr--decode-node' by subject term, both for efficiency (a
node referenced from multiple places is only decoded once) and to
guard against a cyclic reference (the in-progress sentinel below).")

(defconst shexc-shexr--decode-in-progress '--shexc-shexr-decode-in-progress--)
(defconst shexc-shexr--decode-not-memoized '--shexc-shexr-decode-not-memoized--)

(defun shexc-shexr--decode-node-properties (store subject)
  "SUBJECT's properties in STORE (including `:id', synthesized from
SUBJECT itself -- never read off a property, see this file's
Commentary -- unconditionally for a ShapeDecl, or for a TripleExpr only
when `shexc-shexr--ambiguous-ids' marks SUBJECT as referenced from 2+
`ref'-mode occurrences; see `shexc-shexr--node-keys'), excluding its
`rdf:type' (returned separately as TYPE-NAME) -- shared by
`shexc-shexr--decode-node' (which adds `:type') and `shexc-shexr-parse'
\(which builds the Schema's own wrapper instead).  Returns (TYPE-NAME .
PROPERTY-PLIST)."
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
                             (shexc-shexr--decode-value store mode (car objects)
                                                         (and (eq mode 'ref) (cons subject nil)))
                           (if (= (length objects) 1)
                               (shexc-shexr--decode-rdf-list store mode (car objects)
                                                              (and (eq mode 'ref) subject))
                             (let ((i -1))
                               (mapcar (lambda (o)
                                         (setq i (1+ i))
                                         (shexc-shexr--decode-value store mode o
                                                                     (and (eq mode 'ref) (cons subject i))))
                                       objects))))))
             (unless (eq value shexc-shexr--omit)
               (setq result (plist-put result key value))))))
       by-predicate)
      (when (or (equal type-name "ShapeDecl")
                (and shexc-shexr--ambiguous-ids (gethash subject shexc-shexr--ambiguous-ids)))
        (setq result (plist-put result :id (shexc-shexr--term-ref-text subject))))
      (cons type-name result))))

(defun shexc-shexr--decode-node (store subject)
  "Decode SUBJECT's triples in STORE into a value-tree node: `:type'
\(from its one mandatory `rdf:type') plus every other property,
including `:id' -- for a ShapeDecl, always, recovered from SUBJECT
itself; for a TripleExpr, only if SUBJECT has an explicit `sx:id'
property, since unlike ShapeDecl, a TripleExpr's `:id' can *not* be
inferred from SUBJECT's own IRI-vs-blank-node shape -- a deliberately
blank-node-labeled `:id' \(ShExC's `$_:E1 (...)') and a purely
anonymous inline `[ ... ]' are graph-shape-identical otherwise.  See
`shexc-shexr--decode-node-properties'."
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
        (let* ((schema-subject (rdf-model-quad-subject (car schema-quads)))
               (winners-pair (shexc-shexr--ambiguous-id-winners
                              store (shexc-shexr--ref-mode-occurrences store schema-subject)))
               (shexc-shexr--ambiguous-ids (car winners-pair))
               (shexc-shexr--ambiguous-winners (cdr winners-pair)))
          (pcase-let* ((`(,_type-name . ,props)
                        (shexc-shexr--decode-node-properties store schema-subject)))
            (append (list :context shexc-shexr--context-iri :type "Schema") props))))
    (shexc-shexr-parse-error (signal (car err) (cdr err)))
    (error (signal 'shexc-shexr-parse-error (list (error-message-string err))))))

(provide 'shexc-shexr)

;;; shexc-shexr.el ends here
