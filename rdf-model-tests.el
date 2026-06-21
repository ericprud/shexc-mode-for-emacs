;;; rdf-model-tests.el --- ERT suite for rdf-model.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Covers struct equality/term-type dispatch, and exercises the
;; `Dataset' protocol's generic dispatch against a trivial throwaway
;; in-memory implementation defined in this file (the real one,
;; `rdf-store.el', has its own test suite).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rdf-model)

;;; Term/quad struct equality

(ert-deftest rdf-model-test-named-node-equal ()
  (should (equal (rdf-model-named-node-create :value "http://example.org/Alice")
                 (rdf-model-named-node-create :value "http://example.org/Alice")))
  (should-not (equal (rdf-model-named-node-create :value "http://example.org/Alice")
                      (rdf-model-named-node-create :value "http://example.org/Bob"))))

(ert-deftest rdf-model-test-blank-node-equal ()
  (should (equal (rdf-model-blank-node-create :value "b0")
                 (rdf-model-blank-node-create :value "b0")))
  (should-not (equal (rdf-model-blank-node-create :value "b0")
                      (rdf-model-blank-node-create :value "b1"))))

(ert-deftest rdf-model-test-literal-equal ()
  (should (equal (rdf-model-literal-create :value "42" :datatype
                                            (rdf-model-named-node-create
                                             :value "http://www.w3.org/2001/XMLSchema#integer"))
                 (rdf-model-literal-create :value "42" :datatype
                                            (rdf-model-named-node-create
                                             :value "http://www.w3.org/2001/XMLSchema#integer"))))
  (should-not (equal (rdf-model-literal-create :value "hi" :language "en")
                      (rdf-model-literal-create :value "hi" :language "fr"))))

(ert-deftest rdf-model-test-default-graph-singleton-equal ()
  (should (equal rdf-model-default-graph rdf-model-default-graph))
  (should (equal (make-rdf-model--default-graph) rdf-model-default-graph)))

(ert-deftest rdf-model-test-quad-default-graph ()
  (let ((q (rdf-model-quad-create
            :subject (rdf-model-blank-node-create :value "b0")
            :predicate (rdf-model-named-node-create :value "http://example.org/p")
            :object (rdf-model-literal-create :value "1"))))
    (should (equal (rdf-model-quad-graph q) rdf-model-default-graph))))

(ert-deftest rdf-model-test-quad-equal ()
  (cl-flet ((mk () (rdf-model-quad-create
                     :subject (rdf-model-blank-node-create :value "b0")
                     :predicate (rdf-model-named-node-create :value "http://example.org/p")
                     :object (rdf-model-literal-create :value "1"))))
    (should (equal (mk) (mk)))))

;;; rdf-model-term-type

(ert-deftest rdf-model-test-term-type ()
  (should (eq (rdf-model-term-type (rdf-model-named-node-create :value "x")) 'named-node))
  (should (eq (rdf-model-term-type (rdf-model-blank-node-create :value "x")) 'blank-node))
  (should (eq (rdf-model-term-type (rdf-model-literal-create :value "x")) 'literal))
  (should (eq (rdf-model-term-type (rdf-model-variable-create :value "x")) 'variable))
  (should (eq (rdf-model-term-type rdf-model-default-graph) 'default-graph)))

(ert-deftest rdf-model-test-term-type-rejects-non-term ()
  (should-error (rdf-model-term-type "not a term") :type 'wrong-type-argument))

;;; Dataset protocol dispatch, against a throwaway list-backed impl

(cl-defstruct (rdf-model-test--list-dataset (:constructor rdf-model-test--list-dataset-create))
  (quads nil))

(cl-defmethod rdf-model-dataset-add ((ds rdf-model-test--list-dataset) quad)
  (unless (rdf-model-dataset-has ds quad)
    (push quad (rdf-model-test--list-dataset-quads ds)))
  ds)

(cl-defmethod rdf-model-dataset-delete ((ds rdf-model-test--list-dataset) quad)
  (setf (rdf-model-test--list-dataset-quads ds)
        (cl-remove quad (rdf-model-test--list-dataset-quads ds) :test #'equal))
  ds)

(cl-defmethod rdf-model-dataset-has ((ds rdf-model-test--list-dataset) quad)
  (and (member quad (rdf-model-test--list-dataset-quads ds)) t))

(cl-defmethod rdf-model-dataset-match ((ds rdf-model-test--list-dataset)
                                        &optional subject predicate object graph)
  (cl-remove-if-not
   (lambda (q)
     (and (or (null subject) (equal subject (rdf-model-quad-subject q)))
          (or (null predicate) (equal predicate (rdf-model-quad-predicate q)))
          (or (null object) (equal object (rdf-model-quad-object q)))
          (or (null graph) (equal graph (rdf-model-quad-graph q)))))
   (rdf-model-test--list-dataset-quads ds)))

(cl-defmethod rdf-model-dataset-quads ((ds rdf-model-test--list-dataset))
  (rdf-model-test--list-dataset-quads ds))

(cl-defmethod rdf-model-dataset-size ((ds rdf-model-test--list-dataset))
  (length (rdf-model-test--list-dataset-quads ds)))

(defun rdf-model-test--sample-quad (p)
  (rdf-model-quad-create
   :subject (rdf-model-named-node-create :value "http://example.org/s")
   :predicate (rdf-model-named-node-create :value p)
   :object (rdf-model-literal-create :value "1")))

(ert-deftest rdf-model-test-dataset-protocol-dispatch ()
  (let ((ds (rdf-model-test--list-dataset-create))
        (q1 (rdf-model-test--sample-quad "http://example.org/p1"))
        (q2 (rdf-model-test--sample-quad "http://example.org/p2")))
    (should (= (rdf-model-dataset-size ds) 0))
    (rdf-model-dataset-add ds q1)
    (rdf-model-dataset-add ds q2)
    (should (= (rdf-model-dataset-size ds) 2))
    (should (rdf-model-dataset-has ds q1))
    (should (equal (sort (mapcar #'rdf-model-quad-predicate (rdf-model-dataset-quads ds))
                          (lambda (a b) (string< (rdf-model-named-node-value a)
                                                  (rdf-model-named-node-value b))))
                   (list (rdf-model-named-node-create :value "http://example.org/p1")
                         (rdf-model-named-node-create :value "http://example.org/p2"))))
    (should (equal (rdf-model-dataset-match ds nil
                                             (rdf-model-named-node-create :value "http://example.org/p1"))
                   (list q1)))
    (rdf-model-dataset-delete ds q1)
    (should (= (rdf-model-dataset-size ds) 1))
    (should-not (rdf-model-dataset-has ds q1))))

(ert-deftest rdf-model-test-dataset-protocol-no-default-method ()
  "The protocol has no concrete implementation in rdf-model.el itself."
  (should-error (rdf-model-dataset-add "not a dataset" nil) :type 'cl-no-applicable-method))

(provide 'rdf-model-tests)

;;; rdf-model-tests.el ends here
