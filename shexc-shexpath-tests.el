;;; shexc-shexpath-tests.el --- ERT suite for shexc-shexpath.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Two kinds of test: step representation/string-format round-trips
;; (constructors -> `shexc-shexpath-to-string' -> `shexc-shexpath-parse'
;; -> `shexc-shexpath-equal'), and `shexc-shexpath-id-locations' against
;; the worked examples from shexc-shexr.el's Commentary -- hand-built
;; ShExJ value-trees, not compiled from ShExC source, to keep this file
;; independent of shexc-shexj.el.

;;; Code:

(require 'ert)
(require 'shexc-shexpath)

;;; Step representation / string format round-trips

(ert-deftest shexc-shexpath-test-shape-step-round-trip ()
  (let ((steps (list (shexc-shexpath-make-shape-step "<http://a.example/S1>"))))
    (should (equal (shexc-shexpath-to-string steps) "@<http://a.example/S1>"))
    (should (shexc-shexpath-equal (shexc-shexpath-parse "@<http://a.example/S1>") steps))))

(ert-deftest shexc-shexpath-test-blank-node-shape-step ()
  (let ((steps (list (shexc-shexpath-make-shape-step "_:b1"))))
    (should (equal (shexc-shexpath-to-string steps) "@_:b1"))
    (should (shexc-shexpath-equal (shexc-shexpath-parse "@_:b1") steps))))

(ert-deftest shexc-shexpath-test-predicate-step-index-omitted-when-zero ()
  (let ((steps (list (shexc-shexpath-make-shape-step "<http://a.example/S1>")
                      (shexc-shexpath-make-predicate-step "<http://a.example/p0>"))))
    (should (equal (shexc-shexpath-to-string steps) "@<http://a.example/S1>/<http://a.example/p0>"))))

(ert-deftest shexc-shexpath-test-predicate-step-index-shown-when-nonzero ()
  (let ((steps (list (shexc-shexpath-make-shape-step "<http://a.example/S1>")
                      (shexc-shexpath-make-predicate-step "<http://a.example/p0>" 1))))
    (should (equal (shexc-shexpath-to-string steps) "@<http://a.example/S1>/<http://a.example/p0> 1"))
    (should (shexc-shexpath-equal
             (shexc-shexpath-parse "@<http://a.example/S1>/<http://a.example/p0> 1") steps))))

(ert-deftest shexc-shexpath-test-context-step-round-trip ()
  (let ((steps (list (shexc-shexpath-make-shape-step "<http://a.example/S1>")
                      (shexc-shexpath-make-context-step "OneOf" 1)
                      (shexc-shexpath-make-predicate-step "<http://a.example/p0>" 1))))
    (should (equal (shexc-shexpath-to-string steps)
                   "@<http://a.example/S1>/OneOf 1/<http://a.example/p0> 1"))
    (should (shexc-shexpath-equal
             (shexc-shexpath-parse "@<http://a.example/S1>/OneOf 1/<http://a.example/p0> 1")
             steps))))

(ert-deftest shexc-shexpath-test-equal-explicit-zero-same-as-omitted ()
  "An explicit `<label> 0' parses to the same step-list as the omitted
form -- `shexc-shexpath-equal' is plain `equal' precisely because both
sides always normalize a default index to the integer 0."
  (should (shexc-shexpath-equal
           (shexc-shexpath-parse "@<http://a.example/S1>/<http://a.example/p0> 0")
           (shexc-shexpath-parse "@<http://a.example/S1>/<http://a.example/p0>"))))

(ert-deftest shexc-shexpath-test-equal-distinguishes-different-index ()
  (should-not (shexc-shexpath-equal
               (shexc-shexpath-parse "@<http://a.example/S1>/<http://a.example/p0> 1")
               (shexc-shexpath-parse "@<http://a.example/S1>/<http://a.example/p0>"))))

(ert-deftest shexc-shexpath-test-parse-rejects-non-shape-first-step ()
  (should-error (shexc-shexpath-parse "<http://a.example/p0>") :type 'shexc-shexpath-error))

(ert-deftest shexc-shexpath-test-split-respects-iri-brackets ()
  "A `/' inside an IRI (e.g. its own scheme/path separators) is never
mistaken for a step separator."
  (let ((steps (shexc-shexpath-parse "@<http://a.example/S1>/<http://a.example/p/q> 1")))
    (should (equal (nth 1 (nth 1 steps)) "<http://a.example/p/q>"))
    (should (equal (nth 2 (nth 1 steps)) 1))))

;;; `shexc-shexpath-id-locations' -- the worked examples from
;;; shexc-shexr.el's Commentary

(ert-deftest shexc-shexpath-test-id-locations-singleton-expression ()
  "<S1> { &<S2e> }
<S2> { $<S2e> <p1> @<S2>? }
-- S2e is S2's whole :expression (a singleton group collapses to its
bare element, no EachOf step)."
  (let ((schema
         '(:type "Schema"
           :shapes
           ((:type "ShapeDecl" :id "http://all.example/S1"
             :shapeExpr (:type "Shape" :expression "http://all.example/S2e"))
            (:type "ShapeDecl" :id "http://all.example/S2"
             :shapeExpr
             (:type "Shape"
              :expression (:type "TripleConstraint" :id "http://all.example/S2e"
                           :predicate "http://all.example/p1"
                           :valueExpr "http://all.example/S2" :min 0 :max 1)))))))
    (should (equal (shexc-shexpath-id-locations schema)
                   '(("http://all.example/S2e" . "@<http://all.example/S2>"))))))

(ert-deftest shexc-shexpath-test-id-locations-nested-in-second-predicate-sibling ()
  "<S1> { &<S2e> }
<S2> { <p0> { &<S2e> } ; <p0> { $<S2e> <p1> @<S2>? } }
-- S2e is inside the *second*, 0-indexed, <p0> -- the anonymous Shape
ShExC's `<p0> { ... }' syntax wraps the TripleConstraint's body in is
entered transparently (no extra ShapeStep hop)."
  (let* ((s2e-tc '(:type "TripleConstraint" :id "http://all.example/S2e"
                   :predicate "http://all.example/p1"
                   :valueExpr "http://all.example/S2" :min 0 :max 1))
         (tc-a '(:type "TripleConstraint" :predicate "http://all.example/p0"
                 :valueExpr (:type "Shape" :expression "http://all.example/S2e")))
         (tc-b `(:type "TripleConstraint" :predicate "http://all.example/p0"
                 :valueExpr (:type "Shape" :expression ,s2e-tc)))
         (schema
          `(:type "Schema"
            :shapes
            ((:type "ShapeDecl" :id "http://all.example/S1"
              :shapeExpr (:type "Shape" :expression "http://all.example/S2e"))
             (:type "ShapeDecl" :id "http://all.example/S2"
              :shapeExpr
              (:type "Shape"
               :expression (:type "EachOf" :expressions (,tc-a ,tc-b))))))))
    (should (equal (shexc-shexpath-id-locations schema)
                   '(("http://all.example/S2e" . "@<http://all.example/S2>/<http://all.example/p0> 1"))))))

(ert-deftest shexc-shexpath-test-id-locations-oneof-disambiguation ()
  "A OneOf needs an explicit context-test step to disambiguate when the
same predicate appears on either branch: an EachOf containing two
OneOf children, the first with one <p0> TripleConstraint, the second
with two -- the id on the second of those two."
  (let* ((s2e-tc '(:type "TripleConstraint" :id "http://all.example/S2e"
                   :predicate "http://all.example/p0"))
         (one-of-1 '(:type "OneOf" :expressions
                     ((:type "TripleConstraint" :predicate "http://all.example/p0"))))
         (one-of-2 `(:type "OneOf" :expressions
                     ((:type "TripleConstraint" :predicate "http://all.example/p0")
                      ,s2e-tc)))
         (schema
          `(:type "Schema"
            :shapes
            ((:type "ShapeDecl" :id "http://all.example/S2"
              :shapeExpr
              (:type "Shape"
               :expression (:type "EachOf" :expressions (,one-of-1 ,one-of-2))))))))
    (should (equal (shexc-shexpath-id-locations schema)
                   '(("http://all.example/S2e" .
                      "@<http://all.example/S2>/OneOf 1/<http://all.example/p0> 1"))))))

(ert-deftest shexc-shexpath-test-id-locations-no-ids-is-empty ()
  (let ((schema '(:type "Schema" :shapes
                  ((:type "ShapeDecl" :id "http://a.example/S1"
                    :shapeExpr (:type "NodeConstraint" :nodeKind "iri"))))))
    (should (null (shexc-shexpath-id-locations schema)))))

(ert-deftest shexc-shexpath-test-id-locations-shape-decl-itself-never-listed ()
  "A ShapeDecl's own `:id' never appears as a *value* in the table --
the table only ever records where a TripleExpr's `:id' is defined,
since a ShapeDecl is always unambiguously identifiable by its own
`rdf:type sx:ShapeDecl' alone (see shexc-shexr.el's Commentary)."
  (let ((schema '(:type "Schema" :shapes
                  ((:type "ShapeDecl" :id "http://a.example/S1"
                    :shapeExpr (:type "NodeConstraint" :nodeKind "iri"))))))
    (should (null (assoc "http://a.example/S1" (shexc-shexpath-id-locations schema))))))

(provide 'shexc-shexpath-tests)

;;; shexc-shexpath-tests.el ends here
