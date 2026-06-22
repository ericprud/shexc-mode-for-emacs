;;; shexc-shexr-tests.el --- ERT tests for shexc-shexr -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Three kinds of test:
;; - Unit tests against hand-written expected Turtle strings, one per
;;   grammar feature of the canonical ShExR shape (see shexc-shexr.el's
;;   Commentary) -- independent of the parser/UI and of shexc-shexj's
;;   compiler (the value-trees here are hand-built literals, not
;;   compiled from ShExC source).
;; - Per the project plan's check (c): a round-trip test, one per
;;   shexSpec/shexTest manifest fixture (reusing shexc-shexj-tests.el's
;;   `shexc-shexj-test-shextest-path' defcustom and manifest-reading
;;   helpers) -- compile -> serialize -> parse -> compare the value-tree
;;   to the pre-serialization one.  Deliberately NOT compared against
;;   the upstream `.ttl' fixtures (different, non-canonical shape; see
;;   shexc-shexr.el's Commentary for why) -- this only proves the
;;   serializer and parser are faithful inverses of each other.
;; - An RDF-graph-isomorphism test, also one per manifest fixture (see
;;   "Semantic-equivalence..." below): compile -> serialize -> parse the
;;   *output* as plain Turtle (via `rdf-turtle.el', independent of
;;   `shexc-shexr-parse') -> compare, up to blank-node renaming, against
;;   the manifest's own `.ttl' reference fixture parsed the same way.
;;   This is the semantic-equivalence round-trip comparison needed
;;   because of the one remaining piece of bookkeeping structure
;;   `shexc-shexr-serialize' still adds beyond what upstream's own `.ttl'
;;   fixtures contain -- see "Semantic-equivalence..." below.  Unlike
;;   the other two kinds, this one isn't gated on a fixture's manifest
;;   status being `mf:Approved' -- see `shexc-shexr-test--run-isomorphic'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shexc-shexr)
(require 'shexc-shexj)
(require 'shexc-shexj-tests)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)
(require 'rdf-isomorphism)

(ert-deftest shexc-shexr-test-node-constraint ()
  (should (equal
           (shexc-shexr-serialize
            '(:type "Schema" :shapes
              ((:type "ShapeDecl" :id "http://a.example/S1"
                :shapeExpr (:type "NodeConstraint" :nodeKind "iri" :datatype "http://a.example/dt")))))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema ;\n"
            "  sx:shapes (<http://a.example/S1>) .\n"
            "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [\n"
            "    a sx:NodeConstraint ;\n"
            "    sx:datatype <http://a.example/dt> ;\n"
            "    sx:nodeKind sx:iri\n"
            "  ] .\n"))))

(ert-deftest shexc-shexr-test-extra-is-comma-list ()
  "EXTRA is the one array-typed property that's a comma-list, not an RDF list."
  (should (equal
           (shexc-shexr-serialize
            '(:type "Schema" :shapes
              ((:type "ShapeDecl" :id "http://a.example/S1"
                :shapeExpr (:type "Shape" :closed t
                            :extra ("http://a.example/p1" "http://a.example/p2"))))))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema ;\n"
            "  sx:shapes (<http://a.example/S1>) .\n"
            "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [\n"
            "    a sx:Shape ;\n"
            "    sx:closed true ;\n"
            "    sx:extra <http://a.example/p1>, <http://a.example/p2>\n"
            "  ] .\n"))))

(ert-deftest shexc-shexr-test-values-stems-wildcard-boolean ()
  "Value-set entries: bare IRI, native xsd:boolean keyword, IriStem,
and a Wildcard-stem IriStemRange."
  (should (equal
           (shexc-shexr-serialize
            '(:type "Schema" :shapes
              ((:type "ShapeDecl" :id "http://a.example/S1"
                :shapeExpr
                (:type "NodeConstraint" :values
                 ("http://a.example/v1"
                  (:value "true" :type "http://www.w3.org/2001/XMLSchema#boolean")
                  (:type "IriStem" :stem "http://a.example/#")
                  (:type "IriStemRange" :stem (:type "Wildcard")
                   :exclusions ("http://a.example/#x"))))))))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema ;\n"
            "  sx:shapes (<http://a.example/S1>) .\n"
            "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [\n"
            "    a sx:NodeConstraint ;\n"
            "    sx:values (\n"
            "      <http://a.example/v1>\n"
            "      true\n"
            "      [ a sx:IriStem ; sx:stem \"http://a.example/#\" ]\n"
            "      [\n"
            "        a sx:IriStemRange ;\n"
            "        sx:exclusion (<http://a.example/#x>) ;\n"
            "        sx:stem [ a sx:Wildcard ]\n"
            "      ]\n"
            "    )\n"
            "  ] .\n"))))

(ert-deftest shexc-shexr-test-literal-stem-is-plain-text ()
  "A LiteralStemRange's :stem/:exclusions are plain Turtle strings, not
IRI references -- unlike the structurally identical IriStemRange."
  (should (equal
           (shexc-shexr-serialize
            '(:type "Schema" :shapes
              ((:type "ShapeDecl" :id "http://a.example/S1"
                :shapeExpr
                (:type "NodeConstraint" :values
                 ((:type "LiteralStemRange" :stem "abc" :exclusions ("abcdef"))))))))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema ;\n"
            "  sx:shapes (<http://a.example/S1>) .\n"
            "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [\n"
            "    a sx:NodeConstraint ;\n"
            "    sx:values (\n"
            "      [ a sx:LiteralStemRange ; sx:exclusion (\"abcdef\") ; sx:stem \"abc\" ]\n"
            "    )\n"
            "  ] .\n"))))

(ert-deftest shexc-shexr-test-id-hoisting-and-include ()
  "A :id-bearing TripleExpr is hoisted to its own top-level statement,
referenced from its natural position (`:expression', a nested-object
position) and from an Include (`:expressions', a bare-string position)
by a naked IRI both times -- not inlined twice, and (unlike a
`shape-decl-ref'-mode reference) with no graph marker at all
distinguishing the two reference *sites*; resolving which is \"the
definition\" when decoding is `shexc-shexr--ambiguous-id-winners''s job,
not this serializer's -- see shexc-shexr.el's Commentary and
`shexc-shexpath.el'."
  (should (equal
           (shexc-shexr-serialize
            '(:type "Schema" :shapes
              ((:type "ShapeDecl" :id "http://a.example/S1"
                :shapeExpr
                (:type "Shape"
                 :expression
                 (:id "http://a.example/E1" :type "EachOf"
                  :expressions
                  ((:type "TripleConstraint" :predicate "http://a.example/p1")
                   "http://a.example/E1")))))))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema ;\n"
            "  sx:shapes (<http://a.example/S1>) .\n"
            "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [ a sx:Shape ; sx:expression <http://a.example/E1> ] .\n"
            "\n<http://a.example/E1> a sx:EachOf ;\n"
            "  sx:expressions (\n"
            "    [ a sx:TripleConstraint ; sx:predicate <http://a.example/p1> ]\n"
            "    <http://a.example/E1>\n"
            "  ) .\n"))))

(ert-deftest shexc-shexr-test-shape-ref-is-bare-no-wrapper ()
  "A bare shape-ref string :valueExpr pointing at another ShapeDecl's id
\(kitchenSink.shex's `ex:reportedBy IRI @UserShape:') renders as a naked
`<IRI>', not wrapped in `sx:Ref': every ShapeDecl is independently
hoisted to its own top-level statement (it always carries an :id) and
is always identifiable by its own `rdf:type sx:ShapeDecl' alone, so the
decoder never needs the wrapper to tell \"this is a reference\" apart
from \"this is an inline embed\" for `:valueExpr' (see
`shexc-shexr--key-string-mode') -- unlike `:expression'/`:expressions',
see `shexc-shexr-test-id-hoisting-and-include'.  `:predicate' has no
such ambiguity either and is exercised here right next to `:valueExpr',
specifically to show the two aren't conflated."
  (should (equal
           (shexc-shexr-serialize
            '(:type "Schema" :shapes
              ((:type "ShapeDecl" :id "http://a.example/S1"
                :shapeExpr (:type "TripleConstraint" :predicate "http://a.example/p1"
                            :valueExpr "http://a.example/S2"))
               (:type "ShapeDecl" :id "http://a.example/S2"
                :shapeExpr (:type "NodeConstraint" :nodeKind "iri")))))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema ;\n"
            "  sx:shapes (<http://a.example/S1> <http://a.example/S2>) .\n"
            "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [\n"
            "    a sx:TripleConstraint ;\n"
            "    sx:predicate <http://a.example/p1> ;\n"
            "    sx:valueExpr <http://a.example/S2>\n"
            "  ] .\n"
            "\n<http://a.example/S2> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [ a sx:NodeConstraint ; sx:nodeKind sx:iri ] .\n"))))

(ert-deftest shexc-shexr-test-shape-and-nested ()
  "Plain ShapeAnd of two NodeConstraints, exercising the generic
shapeExprs RDF-list path and property ordering (`a' first, alphabetical
after, no special-case needed since neither operand has a 'big nested'
key of its own here)."
  (should (equal
           (shexc-shexr-serialize
            '(:type "Schema" :shapes
              ((:type "ShapeDecl" :id "http://a.example/S1"
                :shapeExpr
                (:type "ShapeAnd" :shapeExprs
                 ((:type "NodeConstraint" :nodeKind "iri")
                  (:type "NodeConstraint" :pattern "^x")))))))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema ;\n"
            "  sx:shapes (<http://a.example/S1>) .\n"
            "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [\n"
            "    a sx:ShapeAnd ;\n"
            "    sx:shapeExprs (\n"
            "      [ a sx:NodeConstraint ; sx:nodeKind sx:iri ]\n"
            "      [ a sx:NodeConstraint ; sx:pattern \"^x\" ]\n"
            "    )\n"
            "  ] .\n"))))

(ert-deftest shexc-shexr-test-context-excluded ()
  "Schema's :context is JSON-LD-adapter metadata, not RDF content."
  (should (equal
           (shexc-shexr-serialize '(:type "Schema" :context "http://www.w3.org/ns/shex.jsonld"))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema .\n"))))

(ert-deftest shexc-shexr-test-parses-reordered-reformatted-reprefixed ()
  "The RDF-graph decoder (`rdf-turtle.el'/`rdf-model.el'/`rdf-store.el')
accepts arbitrary valid Turtle for the same graph -- unlike the old
hand-rolled recursive-descent parser, which was the precise inverse of
this file's own canonical pretty-printing convention and nothing more.
This hand-written Turtle uses a different `sx:' prefix name, reordered
properties, `a' last instead of first, and irregular whitespace -- none
of which `shexc-shexr-serialize' itself would ever produce -- and must
still decode to the same value-tree as the hand-built one in
`shexc-shexr-test-node-constraint'."
  (let ((expected
         '(:type "Schema" :shapes
           ((:type "ShapeDecl" :id "http://a.example/S1"
             :shapeExpr (:type "NodeConstraint" :nodeKind "iri" :datatype "http://a.example/dt"))))))
    (should
     (equal
      (shexc-shexr-test--normalize expected)
      (shexc-shexr-test--normalize
       (shexc-shexr-parse
        (concat
         "@prefix shex: <http://www.w3.org/ns/shex#> .\n"
         "\n"
         "<http://a.example/S1>\n"
         "  shex:shapeExpr [\n"
         "    shex:datatype <http://a.example/dt> ;\n"
         "    shex:nodeKind shex:iri ;\n"
         "    a shex:NodeConstraint\n"
         "  ] ;\n"
         "  shex:id <http://a.example/S1> ;\n"
         "  a shex:ShapeDecl .\n"
         "\n"
         "[] shex:shapes ( <http://a.example/S1> ) ;\n"
         "   a shex:Schema .\n")))))))

(ert-deftest shexc-shexr-test-decode-self-reference-resolved-not-cyclic ()
  "A node whose own `:expressions' dereferences back to itself is *not*
a genuine infinite cycle under the `ref'-mode ambiguity mechanism
\(Part C): E1 now has two `ref'-mode occurrences -- S1's `:expression'
and E1's own self-referencing list element -- so
`shexc-shexr--ambiguous-id-winners' treats the self-loop exactly like
any other multiply-referenced TripleExpr: exactly one occurrence
\(here, the textually-first -- S1's, since there's no
`shexc-shexr--id-location-table') wins and embeds (with a synthesized
`:id'); the other -- here, E1's own self-loop -- decodes as a bare
reference instead of recursing, so no actual cyclic value-tree
structure, and no `shexc-shexr--decode-memo' in-progress error, is ever
produced.  (A genuinely undecodable cycle would need a `shape-decl-ref'
self-reference instead -- but those are never dereferenced at all, see
`shexc-shexr-test-shape-ref-is-bare-no-wrapper' -- so there is, in fact,
no remaining way to construct a real decode cycle in this vocabulary;
this test replaces the old `shexc-shexr-test-decode-cycle-detection'.)"
  (should (equal
           (shexc-shexr-parse
            (concat
             "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
             "[] a sx:Schema ;\n"
             "  sx:shapes (<http://a.example/S1>) .\n"
             "\n<http://a.example/S1> a sx:ShapeDecl ;\n"
             "  sx:shapeExpr [ a sx:Shape ; sx:expression <http://a.example/E1> ] .\n"
             "\n<http://a.example/E1> a sx:EachOf ;\n"
             "  sx:expressions ( <http://a.example/E1> ) .\n"))
           '(:context "http://www.w3.org/ns/shex.jsonld" :type "Schema" :shapes
             ((:type "ShapeDecl"
               :shapeExpr
               (:type "Shape"
                :expression
                (:type "EachOf" :expressions ("http://a.example/E1")
                 :id "http://a.example/E1"))
               :id "http://a.example/S1"))))))

;; ---------------------------------------------------------------------
;; `ref'-mode ambiguity resolution (Part C): `shexc-shexr--id-location-
;; table'-or-textually-first-occurrence -- see shexc-shexr.el's
;; Commentary and `shexc-shexpath.el'.  Each test below builds the
;; *same* hand-built value-tree shexc-shexpath-tests.el's analogous
;; `shexc-shexpath-id-locations' test reproduces, serializes it (so the
;; Turtle under test is this serializer's own real canonical output,
;; not a hand-typed approximation of it), then exercises
;; `shexc-shexr-parse' against that Turtle with/without a table.
;; ---------------------------------------------------------------------

(defconst shexc-shexr-test--nested-include-tree
  (let* ((s2e-tc '(:type "TripleConstraint" :id "http://all.example/S2e"
                   :predicate "http://all.example/p1"
                   :valueExpr "http://all.example/S2" :min 0 :max 1))
         (tc-a '(:type "TripleConstraint" :predicate "http://all.example/p0"
                 :valueExpr (:type "Shape" :expression "http://all.example/S2e")))
         (tc-b `(:type "TripleConstraint" :predicate "http://all.example/p0"
                 :valueExpr (:type "Shape" :expression ,s2e-tc))))
    `(:type "Schema"
      :shapes
      ((:type "ShapeDecl" :id "http://all.example/S1"
        :shapeExpr (:type "Shape" :expression "http://all.example/S2e"))
       (:type "ShapeDecl" :id "http://all.example/S2"
        :shapeExpr (:type "Shape" :expression (:type "EachOf" :expressions (,tc-a ,tc-b)))))))
  "<S1> { &<S2e> }
<S2> { <p0> { &<S2e> } ; <p0> { $<S2e> <p1> @<S2>? } } -- the same
worked example `shexc-shexpath-tests.el's
`shexc-shexpath-test-id-locations-nested-in-second-predicate-sibling'
reproduces: S2e is referenced from 3 places (S1's `:expression', and
both of S2's `<p0>' occurrences), with its one true definition at the
*second*, 0-indexed, `<p0>'.")

(ert-deftest shexc-shexr-test-ambiguous-id-table-picks-the-real-definition ()
  "With `shexc-shexr--id-location-table' supplied (as a real caller --
shexc-ts-mode-convert.el -- would, from `shexc-shexpath-id-locations'
on the very tree being serialized), the table-indicated occurrence
\(S2's second `<p0>') wins and embeds with a synthesized `:id'; every
other bare reference to the same subject -- including S1's, which
isn't even a sibling of the winner -- stays a bare string."
  (let* ((turtle (shexc-shexr-serialize shexc-shexr-test--nested-include-tree))
         (shexc-shexr--id-location-table (shexc-shexpath-id-locations
                                           shexc-shexr-test--nested-include-tree))
         (parsed (shexc-shexr-parse turtle)))
    (should (equal (shexc-shexr-test--normalize parsed)
                   (shexc-shexr-test--normalize shexc-shexr-test--nested-include-tree)))))

(ert-deftest shexc-shexr-test-ambiguous-id-fallback-without-table-picks-textually-first ()
  "With no `shexc-shexr--id-location-table' at all, the *textually-
first* bare reference to S2e wins instead -- per
`shexc-shexr--ambiguous-id-winners', the occurrence whose originating
quad has the highest `shexc-shexr--quad-document-rank' (`rdf-model-
dataset-add' `push'es, so the store's own raw quad list is in reverse
document order).  S1's hoisted statement -- referencing S2e at
`:expression' -- is serialized before either of S2's `<p0>'
occurrences, so S1's is the winner here, not S2's real `$<S2e>'
definition: a deliberate, documented degradation (see this file's
Commentary), not a bug."
  (let* ((turtle (shexc-shexr-serialize shexc-shexr-test--nested-include-tree))
         (shexc-shexr--id-location-table nil)
         (parsed (shexc-shexr-parse turtle)))
    (should (equal
             (shexc-shexr-test--normalize parsed)
             (shexc-shexr-test--normalize
              '(:type "Schema"
                :shapes
                ((:type "ShapeDecl" :id "http://all.example/S1"
                  :shapeExpr
                  (:type "Shape"
                   :expression (:type "TripleConstraint" :id "http://all.example/S2e"
                                :predicate "http://all.example/p1"
                                :valueExpr "http://all.example/S2" :min 0 :max 1)))
                 (:type "ShapeDecl" :id "http://all.example/S2"
                  :shapeExpr
                  (:type "Shape"
                   :expression
                   (:type "EachOf" :expressions
                    ((:type "TripleConstraint" :predicate "http://all.example/p0"
                      :valueExpr (:type "Shape" :expression "http://all.example/S2e"))
                     (:type "TripleConstraint" :predicate "http://all.example/p0"
                      :valueExpr (:type "Shape" :expression "http://all.example/S2e")))))))))))))

(ert-deftest shexc-shexr-test-ambiguous-id-oneof-disambiguation ()
  "An EachOf containing two OneOf children, the first with one `<p0>'
TripleConstraint, the second with two -- the id on the second of those
two -- needs the table's `OneOf N' context step (not just a predicate
index) to land on the right occurrence; without it, the same
predicate/index pair would be ambiguous on its own."
  (let* ((s2e-tc '(:type "TripleConstraint" :id "http://all.example/S2e"
                   :predicate "http://all.example/p0"))
         (one-of-1 '(:type "OneOf" :expressions
                     ((:type "TripleConstraint" :predicate "http://all.example/p0"))))
         (one-of-2 `(:type "OneOf" :expressions
                     ((:type "TripleConstraint" :predicate "http://all.example/p0")
                      ,s2e-tc)))
         (tree
          `(:type "Schema"
            :shapes
            ((:type "ShapeDecl" :id "http://all.example/S1"
              :shapeExpr (:type "Shape" :expression "http://all.example/S2e"))
             (:type "ShapeDecl" :id "http://all.example/S2"
              :shapeExpr
              (:type "Shape"
               :expression (:type "EachOf" :expressions (,one-of-1 ,one-of-2)))))))
         (turtle (shexc-shexr-serialize tree))
         (shexc-shexr--id-location-table (shexc-shexpath-id-locations tree))
         (parsed (shexc-shexr-parse turtle)))
    (should (equal (shexc-shexr-test--normalize parsed) (shexc-shexr-test--normalize tree)))))

(ert-deftest shexc-shexr-test-id-hoisting-and-include-decodes-with-no-regression ()
  "Re-decoding `shexc-shexr-test-id-hoisting-and-include''s own fixture
\(an EachOf with one genuinely unambiguous -- single-occurrence --
hoisted TripleExpr child) with no `shexc-shexr--id-location-table' at
all reproduces the original tree exactly: zero ambiguity here (E1 is
referenced from exactly one *other* place, its own Include, plus its
own definition -- still only 2 occurrences, so this actually exercises
the same ambiguity path as the tests above, just confirming the
fallback gives the textually-first, i.e. the original definition's own
natural position, correct answer here too)."
  (let* ((tree
          '(:type "Schema" :shapes
            ((:type "ShapeDecl" :id "http://a.example/S1"
              :shapeExpr
              (:type "Shape"
               :expression
               (:id "http://a.example/E1" :type "EachOf"
                :expressions
                ((:type "TripleConstraint" :predicate "http://a.example/p1")
                 "http://a.example/E1")))))))
         (turtle (shexc-shexr-serialize tree))
         (shexc-shexr--id-location-table nil)
         (parsed (shexc-shexr-parse turtle)))
    (should (equal (shexc-shexr-test--normalize parsed) (shexc-shexr-test--normalize tree)))))

;; ---------------------------------------------------------------------
;; Round-trip tests against the shexSpec/shexTest corpus
;; ---------------------------------------------------------------------

(defconst shexc-shexr-test--known-unsupported
  shexc-shexj-test--known-unsupported
  "Same handful of fixtures excluded in shexc-shexj-tests.el (raw NUL/
control bytes break tree-sitter-shexc's lexer before this code ever
runs, so there's no value-tree to round-trip at all) -- see that
constant's docstring.")

(defconst shexc-shexr-test--known-id-roundtrip-divergences
  '("2EachInclude1-S2")
  "Fixtures whose TripleExpr `$<id>' annotation is purely decorative --
never actually `&<id>'-Included anywhere in that same fixture -- so it
has exactly one `ref'-mode occurrence (its own natural position) and is
therefore never marked ambiguous by `shexc-shexr--ambiguous-id-winners'
\(ambiguity requires 2+ occurrences); with no ambiguity, `:id' is never
synthesized on decode (see `shexc-shexr--decode-node-properties'), so
the annotation itself doesn't round-trip.  This is a deliberate,
accepted, narrow loss of fidelity -- see shexc-shexr.el's Commentary --
with zero effect on parse *semantics* (nothing ever references this id,
so nothing is lost from the schema's actual meaning) and only a
cosmetic effect on decompiled-ShExC output (the `$<id>' token itself
would be dropped).  `2EachInclude1-S2' is `2EachInclude1''s own
`<http://a.example/S2>' shape factored out as its own fixture -- the
combined `2EachInclude1' fixture *does* reference this same id from
`<http://a.example/S1>' too, so it round-trips correctly there; only
this standalone factoring exercises the unreferenced case.")

(defun shexc-shexr-test--normalize (v)
  "Like `shexc-shexj-test--normalize', but first dropping :context (which
`shexc-shexr-serialize' deliberately omits -- it's JSON-LD-adapter
metadata, not RDF content, see shexc-shexr.el's Commentary)."
  (shexc-shexj-test--normalize
   (if (and (consp v) (keywordp (car v)))
       (let ((p (copy-sequence v))) (cl-remf p :context) p)
     v)))

(defun shexc-shexr-test--run-roundtrip (shex-file approved name)
  (unless approved (ert-skip "manifest status is not mf:Approved"))
  (when (member name shexc-shexr-test--known-unsupported)
    (ert-skip "known tree-sitter-shexc lexer limitation -- see shexc-shexr-test--known-unsupported"))
  (when (member name shexc-shexr-test--known-id-roundtrip-divergences)
    (ert-skip "known unreferenced-$id divergence -- see shexc-shexr-test--known-id-roundtrip-divergences"))
  (let* ((shex-path (expand-file-name (concat "schemas/" shex-file) shexc-shexj-test-shextest-path))
         (compiled (shexc-shexj-test--compile-file shex-path))
         (serialized (shexc-shexr-serialize compiled))
         ;; A genuine `&id'-Include fixture has no in-graph marker telling
         ;; which bare reference is "the definition" (see shexc-shexr.el's
         ;; Commentary) -- exactly like a real caller (shexc-ts-mode-
         ;; convert.el), this test supplies the ShExPath table built from
         ;; the pre-serialization tree itself, the only point at which the
         ;; answer is unambiguous by construction.
         (shexc-shexr--id-location-table (shexc-shexpath-id-locations compiled))
         (parsed (shexc-shexr-parse serialized)))
    (should (equal (shexc-shexr-test--normalize compiled) (shexc-shexr-test--normalize parsed)))))

;;;###autoload
(defun shexc-shexr-test-regenerate ()
  "(Re)define one round-trip `ert-deftest' per shexTest schemas/ manifest entry."
  (interactive)
  (if (not shexc-shexj-test-shextest-path)
      (eval '(ert-deftest shexc-shexr-test-shextest-not-configured ()
               (ert-skip "shexc-shexj-test-shextest-path is unset -- see shexc-shexj-tests.el header"))
            t)
    (dolist (entry (shexc-shexj-test--manifest-entries))
      (let* ((name (plist-get entry :name))
             (status (plist-get entry :status))
             (shex-file (plist-get entry :shex)))
        (when shex-file
          (eval `(ert-deftest ,(intern (format "shexc-shexr-test-roundtrip-%s" (shexc-shexj-test--sanitize-name name))) ()
                   :tags '(shexc-shexr-shextest)
                   (shexc-shexr-test--run-roundtrip ,shex-file ,(equal status "mf:Approved") ,name))
                t))))))

(shexc-shexr-test-regenerate)

;; ---------------------------------------------------------------------
;; Semantic-equivalence (RDF graph isomorphism) against the upstream
;; shexSpec/shexTest `.ttl' reference fixtures
;;
;; `shexc-shexr-serialize' never emits `sx:id'/`sx:Ref' at all (neither
;; is real ShExR vocabulary -- see shexc-shexr.el's Commentary), so
;; there is no private bookkeeping structure left to normalize away
;; before comparing: this serializer's output is directly compared
;; against the upstream `.ttl' reference fixture with
;; `rdf-isomorphism-datasets-equal-p'.
;; ---------------------------------------------------------------------

(defconst shexc-shexr-test--known-isomorphism-divergences
  '("1decimalMaxexclusiveDOUBLE" "1decimalMaxinclusiveDOUBLE" "1decimalMinexclusiveDOUBLE"
    "1decimalMininclusiveDECIMALLeadTrail" "1decimalMininclusiveDOUBLE" "1decimalMininclusiveDOUBLELeadTrail"
    "1doubleMaxexclusiveDECIMALLeadTrail" "1doubleMaxexclusiveDOUBLE" "1doubleMaxexclusiveDOUBLELeadTrail"
    "1doubleMaxinclusiveDOUBLE" "1doubleMinexclusiveDOUBLE" "1doubleMininclusiveDECIMALLeadTrail"
    "1doubleMininclusiveDOUBLE" "1doubleMininclusiveDOUBLELeadTrail"
    "1floatMaxexclusiveDOUBLE" "1floatMaxinclusiveDOUBLE" "1floatMinexclusiveDOUBLE"
    "1floatMininclusiveDECIMALLeadTrail" "1floatMininclusiveDOUBLE" "1floatMininclusiveDOUBLELeadTrail"
    "1integerMininclusiveDECIMALLeadTrail" "1integerMininclusiveDOUBLE" "1integerMininclusiveDOUBLELeadTrail"
    "_all")
  "Fixtures whose NumericFacet (`mininclusive'/`maxinclusive'/
`minexclusive'/`maxexclusive') value is `xsd:decimal'- or `xsd:double'-
typed with a lexical form ShExJ can't retain: ShExJ represents it as a
bare JSON number (confirmed against these fixtures' own `.json'
references), which collapses both the decimal-vs-double distinction
and the original lexical form (leading/trailing zeros, exponent
notation, ...) -- e.g. ShExC `04.50E0' becomes the JSON number `4.5',
indistinguishable from a plain decimal `4.5'.  The upstream `.ttl'
reference fixtures, by contrast, preserve the *exact* original ShExC
source text (confirmed: `1doubleMininclusiveDOUBLELeadTrail.ttl' has
`\"04.50E0\"^^xsd:double', byte-for-byte the original source token) --
evidence they're generated from the ShExC source directly, not via the
lossy ShExJ value-tree this serializer necessarily starts from.  No fix
at this layer can recover information already gone by the time a
value-tree exists; `_all' is here because it happens to also exercise
one of these facets.")

(defun shexc-shexr-test--run-isomorphic (shex-file ttl-file name)
  "Unlike `shexc-shexr-test--run-roundtrip', this doesn't skip
non-`mf:Approved' fixtures: checked against every `mf:Proposed' entry
in shexSpec/shexTest as of this writing (20 of them), every single one
already passes -- so there's nothing tentative/unvetted about this
check's own correctness to hide behind that status, and skipping them
would just mean a future regression there goes uncaught."
  (when (member name shexc-shexr-test--known-unsupported)
    (ert-skip "known tree-sitter-shexc lexer limitation -- see shexc-shexr-test--known-unsupported"))
  (when (member name shexc-shexr-test--known-isomorphism-divergences)
    (ert-skip "known ShExJ lossy-numeric-facet divergence -- see shexc-shexr-test--known-isomorphism-divergences"))
  (let* ((shex-path (expand-file-name (concat "schemas/" shex-file) shexc-shexj-test-shextest-path))
         (ttl-path (expand-file-name (concat "schemas/" ttl-file) shexc-shexj-test-shextest-path))
         (compiled (shexc-shexj-test--compile-file shex-path))
         (serialized (shexc-shexr-serialize compiled))
         (ours (rdf-turtle-parse-string serialized))
         (reference (rdf-turtle-parse-file ttl-path)))
    (should (rdf-isomorphism-datasets-equal-p ours reference))))

;;;###autoload
(defun shexc-shexr-test-isomorphic-regenerate ()
  "(Re)define one RDF-graph-isomorphism `ert-deftest' per shexTest
schemas/ manifest entry -- see this section's top Commentary."
  (interactive)
  (if (not shexc-shexj-test-shextest-path)
      (eval '(ert-deftest shexc-shexr-test-isomorphic-shextest-not-configured ()
               (ert-skip "shexc-shexj-test-shextest-path is unset -- see shexc-shexj-tests.el header"))
            t)
    (dolist (entry (shexc-shexj-test--manifest-entries))
      (let* ((name (plist-get entry :name))
             (shex-file (plist-get entry :shex))
             (ttl-file (plist-get entry :ttl)))
        (when (and shex-file ttl-file)
          (eval `(ert-deftest ,(intern (format "shexc-shexr-test-isomorphic-%s" (shexc-shexj-test--sanitize-name name))) ()
                   :tags '(shexc-shexr-shextest shexc-shexr-isomorphism)
                   (shexc-shexr-test--run-isomorphic ,shex-file ,ttl-file ,name))
                t))))))

(shexc-shexr-test-isomorphic-regenerate)

(provide 'shexc-shexr-tests)

;;; shexc-shexr-tests.el ends here
