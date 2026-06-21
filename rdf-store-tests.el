;;; rdf-store-tests.el --- ERT suite for rdf-store.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'rdf-model)
(require 'rdf-store)

(defun rdf-store-test--quad (s p o)
  (rdf-model-quad-create
   :subject (rdf-model-named-node-create :value s)
   :predicate (rdf-model-named-node-create :value p)
   :object (rdf-model-literal-create :value o)))

(ert-deftest rdf-store-test-add-and-size ()
  (let ((store (rdf-store-create))
        (q (rdf-store-test--quad "s" "p" "o")))
    (should (= (rdf-model-dataset-size store) 0))
    (rdf-model-dataset-add store q)
    (should (= (rdf-model-dataset-size store) 1))))

(ert-deftest rdf-store-test-add-is-idempotent ()
  (let ((store (rdf-store-create))
        (q (rdf-store-test--quad "s" "p" "o")))
    (rdf-model-dataset-add store q)
    (rdf-model-dataset-add store (rdf-store-test--quad "s" "p" "o")) ; equal but distinct object
    (should (= (rdf-model-dataset-size store) 1))))

(ert-deftest rdf-store-test-has ()
  (let ((store (rdf-store-create))
        (q1 (rdf-store-test--quad "s" "p1" "o"))
        (q2 (rdf-store-test--quad "s" "p2" "o")))
    (rdf-model-dataset-add store q1)
    (should (rdf-model-dataset-has store q1))
    (should-not (rdf-model-dataset-has store q2))))

(ert-deftest rdf-store-test-delete ()
  (let ((store (rdf-store-create))
        (q1 (rdf-store-test--quad "s" "p1" "o"))
        (q2 (rdf-store-test--quad "s" "p2" "o")))
    (rdf-model-dataset-add store q1)
    (rdf-model-dataset-add store q2)
    (rdf-model-dataset-delete store q1)
    (should (= (rdf-model-dataset-size store) 1))
    (should-not (rdf-model-dataset-has store q1))
    (should (rdf-model-dataset-has store q2))))

(ert-deftest rdf-store-test-delete-absent-is-noop ()
  (let ((store (rdf-store-create))
        (q1 (rdf-store-test--quad "s" "p1" "o")))
    (rdf-model-dataset-delete store q1)
    (should (= (rdf-model-dataset-size store) 0))))

(ert-deftest rdf-store-test-match-by-each-position ()
  (let* ((store (rdf-store-create))
         (q1 (rdf-store-test--quad "s1" "p" "o1"))
         (q2 (rdf-store-test--quad "s2" "p" "o2")))
    (rdf-model-dataset-add store q1)
    (rdf-model-dataset-add store q2)
    (should (equal (rdf-model-dataset-match store (rdf-model-named-node-create :value "s1"))
                   (list q1)))
    (should (equal (sort (mapcar (lambda (q) (rdf-model-named-node-value (rdf-model-quad-subject q)))
                                  (rdf-model-dataset-match store nil (rdf-model-named-node-create :value "p")))
                          #'string<)
                   (list "s1" "s2")))
    (should (equal (rdf-model-dataset-match store nil nil (rdf-model-literal-create :value "o2"))
                   (list q2)))
    (should (equal (sort (mapcar (lambda (q) (rdf-model-named-node-value (rdf-model-quad-subject q)))
                                  (rdf-model-dataset-match store nil nil nil rdf-model-default-graph))
                          #'string<)
                   (list "s1" "s2")))))

(ert-deftest rdf-store-test-match-no-args-returns-all ()
  (let* ((store (rdf-store-create))
         (q1 (rdf-store-test--quad "s1" "p" "o1"))
         (q2 (rdf-store-test--quad "s2" "p" "o2")))
    (rdf-model-dataset-add store q1)
    (rdf-model-dataset-add store q2)
    (should (= (length (rdf-model-dataset-match store)) 2))))

(ert-deftest rdf-store-test-quads ()
  (let* ((store (rdf-store-create))
         (q1 (rdf-store-test--quad "s1" "p" "o1"))
         (q2 (rdf-store-test--quad "s2" "p" "o2")))
    (rdf-model-dataset-add store q1)
    (rdf-model-dataset-add store q2)
    (should (= (length (rdf-model-dataset-quads store)) 2))
    (should (member q1 (rdf-model-dataset-quads store)))
    (should (member q2 (rdf-model-dataset-quads store)))))

(ert-deftest rdf-store-test-from-quads ()
  (let* ((q1 (rdf-store-test--quad "s1" "p" "o1"))
         (q2 (rdf-store-test--quad "s2" "p" "o2"))
         (store (rdf-store-from-quads (list q1 q2 q1))))
    (should (= (rdf-model-dataset-size store) 2))))

(provide 'rdf-store-tests)

;;; rdf-store-tests.el ends here
