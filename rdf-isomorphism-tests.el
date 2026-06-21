;;; rdf-isomorphism-tests.el --- ERT suite for rdf-isomorphism.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small hand-built Dataset pairs, plus a stress test against the
;; largest-blank-node-count fixture in shexTest's corpus (`_all.ttl',
;; ~160 blank nodes) for actual correctness and runtime at realistic
;; scale, not just toy cases.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rdf-model)
(require 'rdf-store)
(require 'rdf-turtle)
(require 'rdf-isomorphism)
(require 'shexc-shexj-tests) ; for `shexc-shexj-test-shextest-path'

(defun rdf-isomorphism-test--relabel (store)
  "Same dataset as STORE, with every blank node replaced by a fresh one
\(different labels, same structure) -- a *real* renaming, unlike
re-parsing the same text (which would reuse identical `_:x' labels)."
  (let ((mapping (make-hash-table :test #'equal)))
    (cl-labels ((fresh (term)
                  (if (rdf-model-blank-node-p term)
                      (or (gethash term mapping)
                          (let ((n (rdf-model-blank-node-create :value (symbol-name (cl-gensym "relabeled")))))
                            (puthash term n mapping) n))
                    term)))
      (rdf-store-from-quads
       (mapcar (lambda (q) (rdf-model-quad-create :subject (fresh (rdf-model-quad-subject q))
                                                   :predicate (fresh (rdf-model-quad-predicate q))
                                                   :object (fresh (rdf-model-quad-object q))
                                                   :graph (fresh (rdf-model-quad-graph q))))
                (rdf-model-dataset-quads store))))))

(defun rdf-isomorphism-test--store (text) (rdf-turtle-parse-string text))

;;; Small hand-built cases

(ert-deftest rdf-isomorphism-test-identical-ground-graphs-match ()
  (should (rdf-isomorphism-datasets-equal-p
           (rdf-isomorphism-test--store "<http://a/s> <http://a/p> <http://a/o> .")
           (rdf-isomorphism-test--store "<http://a/s> <http://a/p> <http://a/o> ."))))

(ert-deftest rdf-isomorphism-test-different-ground-graphs-dont-match ()
  (should-not (rdf-isomorphism-datasets-equal-p
               (rdf-isomorphism-test--store "<http://a/s> <http://a/p> <http://a/o> .")
               (rdf-isomorphism-test--store "<http://a/s> <http://a/p> <http://a/other> ."))))

(ert-deftest rdf-isomorphism-test-renamed-blank-node-matches ()
  (let ((store (rdf-isomorphism-test--store "_:x <http://a/p> <http://a/o> .")))
    (should (rdf-isomorphism-datasets-equal-p store (rdf-isomorphism-test--relabel store)))))

(ert-deftest rdf-isomorphism-test-renamed-self-loop-matches ()
  (let ((store (rdf-isomorphism-test--store "_:x <http://a/p> _:x .")))
    (should (rdf-isomorphism-datasets-equal-p store (rdf-isomorphism-test--relabel store)))))

(ert-deftest rdf-isomorphism-test-different-bnode-count-doesnt-match ()
  (should-not (rdf-isomorphism-datasets-equal-p
               (rdf-isomorphism-test--store "_:x <http://a/p> <http://a/o> .")
               (rdf-isomorphism-test--store "_:x <http://a/p> _:y . _:y <http://a/q> <http://a/o> ."))))

(ert-deftest rdf-isomorphism-test-different-quad-count-doesnt-match ()
  (should-not (rdf-isomorphism-datasets-equal-p
               (rdf-isomorphism-test--store "<http://a/s> <http://a/p> <http://a/o> .")
               (rdf-isomorphism-test--store "<http://a/s> <http://a/p> <http://a/o> .\n<http://a/s> <http://a/p2> <http://a/o2> ."))))

(ert-deftest rdf-isomorphism-test-isomorphic-but-distinct-bnode-graph ()
  ;; Two blank nodes each pointing at one of two distinguishable named
  ;; targets via different predicates -- distinguishable enough that an
  ;; incorrect (e.g. always-first-candidate) matcher would still need
  ;; to get the assignment right, not just "any bijection".
  (let ((a (rdf-isomorphism-test--store
            "_:x <http://a/p1> <http://a/o1> . _:y <http://a/p2> <http://a/o2> ."))
        (b (rdf-isomorphism-test--store
            "_:m <http://a/p1> <http://a/o1> . _:n <http://a/p2> <http://a/o2> .")))
    (should (rdf-isomorphism-datasets-equal-p a b))))

(ert-deftest rdf-isomorphism-test-swapped-roles-dont-match ()
  ;; Same predicates/objects, but assigned to the "wrong" blank node --
  ;; not a valid bijection (no relabeling fixes a structural mismatch).
  (let ((a (rdf-isomorphism-test--store
            "_:x <http://a/p1> <http://a/o1> . _:y <http://a/p2> <http://a/o2> ."))
        (b (rdf-isomorphism-test--store
            "_:m <http://a/p2> <http://a/o1> . _:n <http://a/p1> <http://a/o2> .")))
    (should-not (rdf-isomorphism-datasets-equal-p a b))))

(ert-deftest rdf-isomorphism-test-chain-relabeled-matches ()
  (let ((store (rdf-isomorphism-test--store
                "_:a <http://a/next> _:b . _:b <http://a/next> _:c . _:c <http://a/val> <http://a/end> .")))
    (should (rdf-isomorphism-datasets-equal-p store (rdf-isomorphism-test--relabel store)))))

;;; Torture tests: a directed 4-cycle vs. two disjoint directed
;;; 2-cycles.  Both have 4 quads and 4 blank nodes, and -- this is the
;;; point -- every node in both graphs is structurally symmetric (each
;;; has exactly one outgoing and one incoming `next' edge to another
;;; blank node), so 1-dimensional color refinement (Weisfeiler-Leman)
;;; provably cannot tell them apart: it converges to *one* signature
;;; class covering all 4 nodes in each graph, with matching partition
;;; sizes (4 = 4).  This is the textbook example of 1-WL's blind spot.
;;; If `rdf-isomorphism--evaluate''s join weren't actually enforcing
;;; bijection/structure on its own merits -- i.e. if the signature
;;; filtering were silently carrying the correctness burden -- this
;;; pair would slip through as a false match, since every candidate
;;; looks equally plausible by signature alone.  No bijection from a
;;; 4-cycle onto two disjoint 2-cycles exists (the cycle can't be torn
;;; into two disconnected pieces by any relabeling), so this must fail.

(ert-deftest rdf-isomorphism-test-4cycle-vs-two-2cycles-doesnt-match ()
  (let ((cycle-4 (rdf-isomorphism-test--store
                  "_:a <http://a/next> _:b . _:b <http://a/next> _:c .
                   _:c <http://a/next> _:d . _:d <http://a/next> _:a ."))
        (two-2cycles (rdf-isomorphism-test--store
                      "_:m <http://a/next> _:n . _:n <http://a/next> _:m .
                       _:p <http://a/next> _:q . _:q <http://a/next> _:p .")))
    (should-not (rdf-isomorphism-datasets-equal-p cycle-4 two-2cycles))))

;; The completeness counterpart: a genuinely isomorphic pair sharing
;; the *exact same* color-refinement blind spot (the 4-cycle is just
;; as symmetric as above, so every node again lands in one shared
;; signature class with zero discriminating power) must still match.
;; Together with the test above, this proves the join is both sound
;; (rejects the non-isomorphic symmetric pair) and complete (accepts
;; the isomorphic symmetric pair) independent of how little the
;; signature heuristic narrows things down.

(ert-deftest rdf-isomorphism-test-4cycle-relabeled-matches-despite-symmetric-candidates ()
  (let ((cycle-4 (rdf-isomorphism-test--store
                  "_:a <http://a/next> _:b . _:b <http://a/next> _:c .
                   _:c <http://a/next> _:d . _:d <http://a/next> _:a .")))
    (should (rdf-isomorphism-datasets-equal-p cycle-4 (rdf-isomorphism-test--relabel cycle-4)))))

;;; Corpus-scale stress test

(ert-deftest rdf-isomorphism-test-large-fixture-relabeled-matches ()
  (unless shexc-shexj-test-shextest-path (ert-skip "shexc-shexj-test-shextest-path is unset -- see shexc-shexj-tests.el header"))
  (let* ((path (expand-file-name "schemas/_all.ttl" shexc-shexj-test-shextest-path))
         (store (rdf-turtle-parse-file path)))
    (should (> (rdf-model-dataset-size store) 100)) ; sanity: this really is the big one
    (should (rdf-isomorphism-datasets-equal-p store (rdf-isomorphism-test--relabel store)))))

(ert-deftest rdf-isomorphism-test-large-fixture-with-dropped-quad-doesnt-match ()
  (unless shexc-shexj-test-shextest-path (ert-skip "shexc-shexj-test-shextest-path is unset -- see shexc-shexj-tests.el header"))
  (let* ((path (expand-file-name "schemas/_all.ttl" shexc-shexj-test-shextest-path))
         (store (rdf-turtle-parse-file path))
         (relabeled (rdf-isomorphism-test--relabel store))
         (quads (rdf-model-dataset-quads relabeled)))
    (rdf-model-dataset-delete relabeled (car quads))
    (should-not (rdf-isomorphism-datasets-equal-p store relabeled))))

(ert-deftest rdf-isomorphism-test-distinct-real-fixtures-dont-match ()
  (unless shexc-shexj-test-shextest-path (ert-skip "shexc-shexj-test-shextest-path is unset -- see shexc-shexj-tests.el header"))
  (should-not
   (rdf-isomorphism-datasets-equal-p
    (rdf-turtle-parse-file (expand-file-name "schemas/1refbnode1.ttl" shexc-shexj-test-shextest-path))
    (rdf-turtle-parse-file (expand-file-name "schemas/_all.ttl" shexc-shexj-test-shextest-path)))))

(provide 'rdf-isomorphism-tests)

;;; rdf-isomorphism-tests.el ends here
