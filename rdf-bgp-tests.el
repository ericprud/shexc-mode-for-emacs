;;; rdf-bgp-tests.el --- ERT suite for rdf-bgp.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Alongside the basic single-pattern cases, several tests here exist
;; specifically to demonstrate how `rdf-bgp-match' differs from
;; `rdf-isomorphism-datasets-equal-p' despite sharing a join engine
;; (`rdf-join.el'): homomorphism vs. bijection, pattern-subset-of-target
;; vs. exact dataset equality, and blank nodes as constants vs.
;; blank nodes as the very thing being matched up to renaming.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)
(require 'rdf-bgp)
(require 'rdf-isomorphism)

(defun rdf-bgp-test--nn (iri) (rdf-model-named-node-create :value iri))
(defun rdf-bgp-test--bn (label) (rdf-model-blank-node-create :value label))
(defun rdf-bgp-test--var (name) (rdf-model-variable-create :value name))

(defun rdf-bgp-test--quad (s p o &optional g)
  (rdf-model-quad-create :subject s :predicate p :object o
                          :graph (or g rdf-model-default-graph)))

(defun rdf-bgp-test--target (text) (rdf-turtle-parse-string text))

;;; Basic matching

(ert-deftest rdf-bgp-test-binds-variable-and-returns-solution ()
  (let* ((pattern (list (rdf-bgp-test--quad (rdf-bgp-test--var "x")
                                             (rdf-bgp-test--nn "http://a/p")
                                             (rdf-bgp-test--nn "http://a/o"))))
         (target (rdf-bgp-test--target "<http://a/s> <http://a/p> <http://a/o> ."))
         (solutions (rdf-bgp-match pattern target)))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc (rdf-bgp-test--var "x") (car solutions)))
                   (rdf-bgp-test--nn "http://a/s")))))

(ert-deftest rdf-bgp-test-no-match-returns-nil ()
  (let ((pattern (list (rdf-bgp-test--quad (rdf-bgp-test--var "x")
                                            (rdf-bgp-test--nn "http://a/p")
                                            (rdf-bgp-test--nn "http://a/missing")))))
    (should-not (rdf-bgp-match pattern (rdf-bgp-test--target "<http://a/s> <http://a/p> <http://a/o> .")))))

(ert-deftest rdf-bgp-test-returns-multiple-solutions ()
  (let* ((pattern (list (rdf-bgp-test--quad (rdf-bgp-test--var "x")
                                             (rdf-bgp-test--nn "http://a/p")
                                             (rdf-bgp-test--nn "http://a/o"))))
         (target (rdf-bgp-test--target
                  "<http://a/s1> <http://a/p> <http://a/o> . <http://a/s2> <http://a/p> <http://a/o> ."))
         (solutions (rdf-bgp-match pattern target)))
    (should (= (length solutions) 2))
    (should (equal (sort (mapcar (lambda (sol) (rdf-model-named-node-value (cdr (assoc (rdf-bgp-test--var "x") sol))))
                                  solutions)
                          #'string<)
                   (list "http://a/s1" "http://a/s2")))))

;;; Difference 1: homomorphism, not bijection -- two distinct pattern
;;; variables may legally collapse onto the same target term.

(ert-deftest rdf-bgp-test-allows-variables-to-collapse-onto-same-node ()
  (let* ((x (rdf-bgp-test--var "x")) (y (rdf-bgp-test--var "y")) (p (rdf-bgp-test--nn "http://a/p"))
         (pattern (list (rdf-bgp-test--quad x p y) (rdf-bgp-test--quad y p x)))
         (target (rdf-bgp-test--target "_:m <http://a/p> _:m .")) ; one self-looping blank node
         (solutions (rdf-bgp-match pattern target)))
    ;; A solution exists with ?x and ?y bound to the *same* node --
    ;; impossible for `rdf-isomorphism-datasets-equal-p', which would
    ;; also reject this pair outright on quad/blank-node count (2 vs 1)
    ;; before ever reaching its join.
    (should solutions)
    (should (equal (cdr (assoc x (car solutions))) (cdr (assoc y (car solutions)))))))

;;; Difference 2: PATTERN need not exhaust TARGET -- TARGET may have
;;; quads the pattern never mentions.  Isomorphism requires exact
;;; dataset equality (same quad count on both sides); BGP matching
;;; only requires PATTERN's bindings to be a subset of TARGET.

(ert-deftest rdf-bgp-test-matches-subset-of-larger-target ()
  (let* ((pattern (list (rdf-bgp-test--quad (rdf-bgp-test--var "x")
                                             (rdf-bgp-test--nn "http://a/p")
                                             (rdf-bgp-test--nn "http://a/o"))))
         (target (rdf-bgp-test--target
                  "<http://a/s> <http://a/p> <http://a/o> .
                   <http://a/unrelated1> <http://a/q> <http://a/unrelated2> .
                   <http://a/unrelated2> <http://a/q> <http://a/unrelated3> .")))
    (should (rdf-bgp-match pattern target))))

;;; Difference 3: a blank node written into a pattern is an ordinary
;;; constant, not something to bind -- exactly SPARQL's own treatment
;;; of blank nodes in a query's WHERE clause.  It must `equal' a
;;; specific target blank node, not match any blank node the way a
;;; variable (or, in `rdf-isomorphism.el', a pattern blank node) would.

(ert-deftest rdf-bgp-test-blank-node-in-pattern-is-a-constant-not-a-variable ()
  (let* ((m (rdf-bgp-test--bn "m"))
         (pattern (list (rdf-bgp-test--quad m (rdf-bgp-test--nn "http://a/p") (rdf-bgp-test--var "x"))))
         (matching-target (rdf-store-from-quads
                            (list (rdf-bgp-test--quad m (rdf-bgp-test--nn "http://a/p") (rdf-bgp-test--nn "http://a/o")))))
         (other-bnode-target (rdf-store-from-quads
                               (list (rdf-bgp-test--quad (rdf-bgp-test--bn "n")
                                                          (rdf-bgp-test--nn "http://a/p")
                                                          (rdf-bgp-test--nn "http://a/o"))))))
    (should (rdf-bgp-match pattern matching-target))
    ;; Same shape, but a *different* blank node -- a variable in this
    ;; position would happily bind to it; a constant must not match.
    (should-not (rdf-bgp-match pattern other-bnode-target))))

;;; The direct side-by-side: the same 4-cycle-vs-two-disjoint-2-cycles
;;; structure used in rdf-isomorphism-tests.el's torture test (chosen
;;; there because 1-WL color refinement can't tell the two graphs
;;; apart). Treating the 4-cycle's blank nodes as BGP variables instead
;;; finds a homomorphism into the two 2-cycles (collapsing opposite
;;; corners onto the same node) -- exactly the match dataset-equality
;;; correctly refuses.

(ert-deftest rdf-bgp-test-succeeds-where-isomorphism-fails-on-4cycle-vs-2x2cycles ()
  (let* ((a (rdf-bgp-test--var "a")) (b (rdf-bgp-test--var "b"))
         (c (rdf-bgp-test--var "c")) (d (rdf-bgp-test--var "d"))
         (next (rdf-bgp-test--nn "http://a/next"))
         (pattern (list (rdf-bgp-test--quad a next b) (rdf-bgp-test--quad b next c)
                        (rdf-bgp-test--quad c next d) (rdf-bgp-test--quad d next a)))
         (two-2cycles (rdf-bgp-test--target
                       "_:m <http://a/next> _:n . _:n <http://a/next> _:m .
                        _:p <http://a/next> _:q . _:q <http://a/next> _:p .")))
    (should (rdf-bgp-match pattern two-2cycles))
    (should-not (rdf-isomorphism-datasets-equal-p
                 (rdf-bgp-test--target
                  "_:a <http://a/next> _:b . _:b <http://a/next> _:c .
                   _:c <http://a/next> _:d . _:d <http://a/next> _:a .")
                 two-2cycles))))

(provide 'rdf-bgp-tests)

;;; rdf-bgp-tests.el ends here
