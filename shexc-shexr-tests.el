;;; shexc-shexr-tests.el --- ERT tests for shexc-shexr -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Unit tests against hand-written expected Turtle strings, one per
;; grammar feature of the canonical ShExR shape (see shexc-shexr.el's
;; Commentary) -- independent of the parser/UI and of shexc-shexj's
;; compiler (the value-trees here are hand-built literals, not compiled
;; from ShExC source).

;;; Code:

(require 'ert)
(require 'shexc-shexr)

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
            "  sx:shapeExpr [ a sx:NodeConstraint ;\n"
            "  sx:datatype <http://a.example/dt> ;\n"
            "  sx:nodeKind \"iri\" ] .\n"))))

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
            "  sx:shapeExpr [ a sx:Shape ;\n"
            "  sx:closed true ;\n"
            "  sx:extra <http://a.example/p1>, <http://a.example/p2> ] .\n"))))

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
            "  sx:shapeExpr [ a sx:NodeConstraint ;\n"
            "  sx:values (<http://a.example/v1> true [ a sx:IriStem ;\n"
            "  sx:stem <http://a.example/#> ] [ a sx:IriStemRange ;\n"
            "  sx:exclusions (<http://a.example/#x>) ;\n"
            "  sx:stem [ a sx:Wildcard ] ]) ] .\n"))))

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
            "  sx:shapeExpr [ a sx:NodeConstraint ;\n"
            "  sx:values ([ a sx:LiteralStemRange ;\n"
            "  sx:exclusions (\"abcdef\") ;\n"
            "  sx:stem \"abc\" ]) ] .\n"))))

(ert-deftest shexc-shexr-test-id-hoisting-and-include ()
  "A :id-bearing TripleExpr is hoisted to its own top-level statement and
referenced elsewhere by IRI -- not inlined twice."
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
            "  sx:shapeExpr [ a sx:Shape ;\n"
            "  sx:expression <http://a.example/E1> ] .\n"
            "\n<http://a.example/E1> a sx:EachOf ;\n"
            "  sx:expressions ([ a sx:TripleConstraint ;\n"
            "  sx:predicate <http://a.example/p1> ] <http://a.example/E1>) .\n"))))

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
            "  sx:shapeExpr [ a sx:ShapeAnd ;\n"
            "  sx:shapeExprs ([ a sx:NodeConstraint ;\n"
            "  sx:nodeKind \"iri\" ] [ a sx:NodeConstraint ;\n"
            "  sx:pattern \"^x\" ]) ] .\n"))))

(ert-deftest shexc-shexr-test-context-excluded ()
  "Schema's :context is JSON-LD-adapter metadata, not RDF content."
  (should (equal
           (shexc-shexr-serialize '(:type "Schema" :context "http://www.w3.org/ns/shex.jsonld"))
           (concat
            "@prefix sx: <http://www.w3.org/ns/shex#> .\n\n"
            "[] a sx:Schema .\n"))))

(provide 'shexc-shexr-tests)

;;; shexc-shexr-tests.el ends here
