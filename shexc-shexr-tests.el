;;; shexc-shexr-tests.el --- ERT tests for shexc-shexr -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Two kinds of test:
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

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shexc-shexr)
(require 'shexc-shexj)
(require 'shexc-shexj-tests)

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
            "    sx:datatype [ a sx:Ref ; sx:id <http://a.example/dt> ] ;\n"
            "    sx:nodeKind \"iri\"\n"
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
            "      [ a sx:IriStem ; sx:stem [ a sx:Ref ; sx:id <http://a.example/#> ] ]\n"
            "      [\n"
            "        a sx:IriStemRange ;\n"
            "        sx:exclusions ([ a sx:Ref ; sx:id <http://a.example/#x> ]) ;\n"
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
            "      [ a sx:LiteralStemRange ; sx:exclusions (\"abcdef\") ; sx:stem \"abc\" ]\n"
            "    )\n"
            "  ] .\n"))))

(ert-deftest shexc-shexr-test-id-hoisting-and-include ()
  "A :id-bearing TripleExpr is hoisted to its own top-level statement,
referenced from its natural position by a naked IRI (`:expression', a
nested-object position) and from an Include by a wrapped `sx:Ref'
(`:expressions', a bare-string position) -- not inlined twice, and the
two reference *forms* differ even though they point at the same IRI."
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
            "    [\n"
            "      a sx:TripleConstraint ;\n"
            "      sx:predicate [ a sx:Ref ; sx:id <http://a.example/p1> ]\n"
            "    ]\n"
            "    [ a sx:Ref ; sx:id <http://a.example/E1> ]\n"
            "  ) .\n"))))

(ert-deftest shexc-shexr-test-shape-ref-vs-hoisted-object-disambiguation ()
  "A bare shape-ref string :valueExpr pointing at another ShapeDecl's id
must be wrapped in `sx:Ref' -- confirmed empirically as a *real*
ambiguity (kitchenSink.shex's `ex:reportedBy IRI @UserShape:'): every
ShapeDecl is independently hoisted to its own top-level statement (it
always carries an :id), so a naked `<IRI>' here would be
indistinguishable from a reference to *that* hoisted statement, which
must be dereferenced/embedded rather than kept as the bare string it
actually is in the value-tree."
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
            "    sx:predicate [ a sx:Ref ; sx:id <http://a.example/p1> ] ;\n"
            "    sx:valueExpr [ a sx:Ref ; sx:id <http://a.example/S2> ]\n"
            "  ] .\n"
            "\n<http://a.example/S2> a sx:ShapeDecl ;\n"
            "  sx:shapeExpr [ a sx:NodeConstraint ; sx:nodeKind \"iri\" ] .\n"))))

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
            "      [ a sx:NodeConstraint ; sx:nodeKind \"iri\" ]\n"
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

;; ---------------------------------------------------------------------
;; Round-trip tests against the shexSpec/shexTest corpus
;; ---------------------------------------------------------------------

(defconst shexc-shexr-test--known-unsupported
  shexc-shexj-test--known-unsupported
  "Same handful of fixtures excluded in shexc-shexj-tests.el (raw NUL/
control bytes break tree-sitter-shexc's lexer before this code ever
runs, so there's no value-tree to round-trip at all) -- see that
constant's docstring.")

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
  (let* ((shex-path (expand-file-name (concat "schemas/" shex-file) shexc-shexj-test-shextest-path))
         (compiled (shexc-shexj-test--compile-file shex-path))
         (serialized (shexc-shexr-serialize compiled))
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

(provide 'shexc-shexr-tests)

;;; shexc-shexr-tests.el ends here
