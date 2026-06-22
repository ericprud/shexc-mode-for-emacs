;;; rdf-turtle-tests.el --- ERT suite for rdf-turtle.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Unit tests against small hand-written Turtle snippets, plus a
;; full-corpus smoke test against shexSpec/shexTest's `schemas/*.ttl'
;; fixtures (reusing `shexc-shexj-test-shextest-path' -- see
;; shexc-shexj-tests.el's header for how to configure it; with it unset,
;; the corpus test reports itself skipped).

;;; Code:

(require 'ert)
(require 'rdf-model)
(require 'rdf-turtle)
(require 'shexc-shexj-tests) ; for `shexc-shexj-test-shextest-path'

;;; Small helpers

(defun rdf-turtle-test--quads (text)
  (rdf-model-dataset-quads (rdf-turtle-parse-string text)))

(defun rdf-turtle-test--nn (iri) (rdf-model-named-node-create :value iri))
(defun rdf-turtle-test--bn (label) (rdf-model-blank-node-create :value label))
(defun rdf-turtle-test--lit (value &optional language datatype)
  (rdf-model-literal-create :value value :language language
                             :datatype (if datatype (rdf-turtle-test--nn datatype))))

;;; Basic triples, prefixes, BASE

(ert-deftest rdf-turtle-test-simple-triple ()
  (should (equal (rdf-turtle-test--quads "<http://a/s> <http://a/p> <http://a/o> .")
                  (list (rdf-model-quad-create
                         :subject (rdf-turtle-test--nn "http://a/s")
                         :predicate (rdf-turtle-test--nn "http://a/p")
                         :object (rdf-turtle-test--nn "http://a/o"))))))

(ert-deftest rdf-turtle-test-prefixed-names ()
  (should (equal (rdf-turtle-test--quads
                  "PREFIX ex: <http://example.org/>\nex:s ex:p ex:o .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://example.org/s")
                        :predicate (rdf-turtle-test--nn "http://example.org/p")
                        :object (rdf-turtle-test--nn "http://example.org/o"))))))

(ert-deftest rdf-turtle-test-at-prefix-style ()
  (should (equal (rdf-turtle-test--quads
                  "@prefix ex: <http://example.org/> .\nex:s ex:p ex:o .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://example.org/s")
                        :predicate (rdf-turtle-test--nn "http://example.org/p")
                        :object (rdf-turtle-test--nn "http://example.org/o"))))))

(ert-deftest rdf-turtle-test-empty-prefix ()
  (should (equal (rdf-turtle-test--quads "PREFIX : <http://example.org/>\n:s :p :o .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://example.org/s")
                        :predicate (rdf-turtle-test--nn "http://example.org/p")
                        :object (rdf-turtle-test--nn "http://example.org/o"))))))

(ert-deftest rdf-turtle-test-a-keyword-is-rdf-type ()
  (should (equal (rdf-turtle-test--quads "<http://a/s> a <http://a/Type> .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://a/s")
                        :predicate (rdf-turtle-test--nn "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                        :object (rdf-turtle-test--nn "http://a/Type"))))))

(ert-deftest rdf-turtle-test-base-relative-iri ()
  (should (equal (rdf-turtle-test--quads
                  "BASE <http://a.example/>\n<s> <p> <#frag> .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://a.example/s")
                        :predicate (rdf-turtle-test--nn "http://a.example/p")
                        :object (rdf-turtle-test--nn "http://a.example/#frag"))))))

(ert-deftest rdf-turtle-test-undeclared-prefix-errors ()
  (should-error (rdf-turtle-test--quads "ex:s ex:p ex:o .")))

;;; Blank nodes

(ert-deftest rdf-turtle-test-labeled-blank-node-reused ()
  (should (equal (rdf-turtle-test--quads "_:x <http://a/p> _:x .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--bn "x")
                        :predicate (rdf-turtle-test--nn "http://a/p")
                        :object (rdf-turtle-test--bn "x"))))))

(ert-deftest rdf-turtle-test-anon-mints-fresh-distinct-bnodes ()
  (let* ((quads (rdf-turtle-test--quads "[] <http://a/p> [] ."))
         (q (car quads)))
    (should (= (length quads) 1))
    (should (rdf-model-blank-node-p (rdf-model-quad-subject q)))
    (should (rdf-model-blank-node-p (rdf-model-quad-object q)))
    (should-not (equal (rdf-model-quad-subject q) (rdf-model-quad-object q)))))

(ert-deftest rdf-turtle-test-blank-node-property-list-as-object ()
  (let ((quads (rdf-turtle-test--quads "<http://a/s> <http://a/p> [ <http://a/q> <http://a/r> ] .")))
    (should (= (length quads) 2))
    (let* ((outer (cl-find-if (lambda (q) (equal (rdf-model-quad-predicate q) (rdf-turtle-test--nn "http://a/p"))) quads))
           (inner-bnode (rdf-model-quad-object outer))
           (inner (cl-find-if (lambda (q) (equal (rdf-model-quad-predicate q) (rdf-turtle-test--nn "http://a/q"))) quads)))
      (should (rdf-model-blank-node-p inner-bnode))
      (should (equal (rdf-model-quad-subject inner) inner-bnode))
      (should (equal (rdf-model-quad-object inner) (rdf-turtle-test--nn "http://a/r"))))))

(ert-deftest rdf-turtle-test-blank-node-property-list-as-subject-with-extra-properties ()
  ;; `[ <p1> <o1> ] <p2> <o2> .' -- the bnode gets BOTH its own internal
  ;; property and the top-level "additional predicateObjectList".
  (let ((quads (rdf-turtle-test--quads
                "<http://a/p1> <http://a/p2> <http://a/p3> .\n[ <http://a/p1> <http://a/o1> ] <http://a/p2> <http://a/o2> .")))
    (should (= (length quads) 3))
    (let* ((bnode-quads (cl-remove-if-not (lambda (q) (rdf-model-blank-node-p (rdf-model-quad-subject q))) quads)))
      (should (= (length bnode-quads) 2))
      (should (equal (sort (mapcar (lambda (q) (rdf-model-named-node-value (rdf-model-quad-predicate q))) bnode-quads)
                            #'string<)
                     (list "http://a/p1" "http://a/p2"))))))

;;; Literals

(ert-deftest rdf-turtle-test-plain-string-literal-is-xsd-string ()
  (should (equal (rdf-turtle-test--quads "<http://a/s> <http://a/p> \"hi\" .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://a/s")
                        :predicate (rdf-turtle-test--nn "http://a/p")
                        :object (rdf-turtle-test--lit "hi" nil "http://www.w3.org/2001/XMLSchema#string"))))))

(ert-deftest rdf-turtle-test-lang-tagged-literal ()
  (should (equal (rdf-turtle-test--quads "<http://a/s> <http://a/p> \"hi\"@en-UK .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://a/s")
                        :predicate (rdf-turtle-test--nn "http://a/p")
                        :object (rdf-turtle-test--lit "hi" "en-UK" "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"))))))

(ert-deftest rdf-turtle-test-explicit-datatype-literal ()
  (should (equal (rdf-turtle-test--quads
                  "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n<http://a/s> <http://a/p> \"42\"^^xsd:integer .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://a/s")
                        :predicate (rdf-turtle-test--nn "http://a/p")
                        :object (rdf-turtle-test--lit "42" nil "http://www.w3.org/2001/XMLSchema#integer"))))))

(ert-deftest rdf-turtle-test-numeric-and-boolean-shorthand ()
  ;; `rdf-model-dataset-quads' is documented as unspecified order, so
  ;; compare as sets, not lists.
  (let ((expected (list (rdf-model-quad-create :subject (rdf-turtle-test--nn "http://a/s")
                                                :predicate (rdf-turtle-test--nn "http://a/p")
                                                :object (rdf-turtle-test--lit "42" nil "http://www.w3.org/2001/XMLSchema#integer"))
                         (rdf-model-quad-create :subject (rdf-turtle-test--nn "http://a/s")
                                                :predicate (rdf-turtle-test--nn "http://a/p")
                                                :object (rdf-turtle-test--lit "4.2" nil "http://www.w3.org/2001/XMLSchema#decimal"))
                         (rdf-model-quad-create :subject (rdf-turtle-test--nn "http://a/s")
                                                :predicate (rdf-turtle-test--nn "http://a/p")
                                                :object (rdf-turtle-test--lit "4.2e1" nil "http://www.w3.org/2001/XMLSchema#double"))
                         (rdf-model-quad-create :subject (rdf-turtle-test--nn "http://a/s")
                                                :predicate (rdf-turtle-test--nn "http://a/p")
                                                :object (rdf-turtle-test--lit "true" nil "http://www.w3.org/2001/XMLSchema#boolean"))))
        (actual (rdf-turtle-test--quads "<http://a/s> <http://a/p> 42, 4.2, 4.2e1, true .")))
    (should (= (length actual) (length expected)))
    (should (null (cl-set-difference actual expected :test #'equal)))
    (should (null (cl-set-difference expected actual :test #'equal)))))

(ert-deftest rdf-turtle-test-string-escapes ()
  (should (equal (rdf-model-literal-value
                  (rdf-model-quad-object (car (rdf-turtle-test--quads "<http://a/s> <http://a/p> \"a\\tb\\nc\\\"d\" ."))))
                  "a\tb\nc\"d")))

(ert-deftest rdf-turtle-test-uchar-escape ()
  (should (equal (rdf-model-literal-value
                  (rdf-model-quad-object (car (rdf-turtle-test--quads "<http://a/s> <http://a/p> \"\\u00e9\" ."))))
                 "é")))

(ert-deftest rdf-turtle-test-uchar-escape-8-digit ()
  "A `\\Uxxxxxxxx' (8-hex-digit) UCHAR escape, distinct from `\\uxxxx'
\(4-hex-digit) -- regression test for a `case-fold-search' bug where
the regex's lowercase `u' alternative also matched an uppercase `U',
misparsing the 8-hex escape as a 4-hex one plus four leftover literal
characters."
  (should (equal (rdf-model-literal-value
                  (rdf-model-quad-object (car (rdf-turtle-test--quads "<http://a/s> <http://a/p> \"\\U0001D4B8\" ."))))
                 "𝒸")))

(ert-deftest rdf-turtle-test-long-string-literal ()
  (should (equal (rdf-model-literal-value
                  (rdf-model-quad-object (car (rdf-turtle-test--quads "<http://a/s> <http://a/p> \"\"\"a\nb\"\"\" ."))))
                 "a\nb")))

;;; PN_LOCAL escaping

(ert-deftest rdf-turtle-test-pn-local-escape ()
  (should (equal (rdf-turtle-test--quads "PREFIX ex: <http://example.org/>\nex:a\\-b ex:p ex:o .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://example.org/a-b")
                        :predicate (rdf-turtle-test--nn "http://example.org/p")
                        :object (rdf-turtle-test--nn "http://example.org/o"))))))

;;; Collections

(ert-deftest rdf-turtle-test-empty-collection-is-rdf-nil ()
  (should (equal (rdf-turtle-test--quads "<http://a/s> <http://a/p> () .")
                 (list (rdf-model-quad-create
                        :subject (rdf-turtle-test--nn "http://a/s")
                        :predicate (rdf-turtle-test--nn "http://a/p")
                        :object (rdf-turtle-test--nn "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))))))

(ert-deftest rdf-turtle-test-collection-is-rdf-list-chain ()
  (let* ((store (rdf-turtle-parse-string "<http://a/s> <http://a/p> ( <http://a/x> <http://a/y> ) ."))
         (quads (rdf-model-dataset-quads store))
         (rdf-first (rdf-turtle-test--nn "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
         (rdf-rest (rdf-turtle-test--nn "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
         (rdf-nil (rdf-turtle-test--nn "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
         (head (rdf-model-quad-object (car (rdf-model-dataset-match store nil (rdf-turtle-test--nn "http://a/p"))))))
    (should (= (length quads) 5)) ; outer triple + 2 first + 2 rest (one to next cell, one to nil)
    (should (rdf-model-blank-node-p head))
    (let* ((first1 (car (rdf-model-dataset-match store head rdf-first)))
           (rest1 (car (rdf-model-dataset-match store head rdf-rest)))
           (cell2 (rdf-model-quad-object rest1)))
      (should (equal (rdf-model-quad-object first1) (rdf-turtle-test--nn "http://a/x")))
      (should (rdf-model-blank-node-p cell2))
      (let ((first2 (car (rdf-model-dataset-match store cell2 rdf-first)))
            (rest2 (car (rdf-model-dataset-match store cell2 rdf-rest))))
        (should (equal (rdf-model-quad-object first2) (rdf-turtle-test--nn "http://a/y")))
        (should (equal (rdf-model-quad-object rest2) rdf-nil))))))

(ert-deftest rdf-turtle-test-nested-collection ()
  "Doesn't error, and the inner collection resolves to a distinct bnode head."
  (let* ((store (rdf-turtle-parse-string "<http://a/s> <http://a/p> ( ( <http://a/x> ) ) ."))
         (rdf-first (rdf-turtle-test--nn "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")))
    (should (= (rdf-model-dataset-size store) 5)) ; outer triple + outer(first,rest) + inner(first,rest)
    (let* ((outer-head (rdf-model-quad-object (car (rdf-model-dataset-match store nil (rdf-turtle-test--nn "http://a/p")))))
           (inner-head (rdf-model-quad-object (car (rdf-model-dataset-match store outer-head rdf-first)))))
      (should (rdf-model-blank-node-p inner-head))
      (should-not (equal outer-head inner-head)))))

;;; shexTest corpus

(defconst rdf-turtle-test--known-unsupported '("meta.ttl")
  "`meta.ttl' references the prefix `sx:' without ever declaring it
\(it's a relationships-between-fixtures file, not real shape data, and
this looks like a copy-paste typo in two of its lines -- a genuine
upstream defect, not a `tree-sitter-turtle'/`rdf-turtle' limitation).
Confirmed: every *other* file among shexTest's 436 `schemas/*.ttl'
fixtures parses with zero errors.")

;;;###autoload
(defun rdf-turtle-test-regenerate-corpus-test ()
  "(Re)define the full-corpus parse-without-error test."
  (interactive)
  (if (not shexc-shexj-test-shextest-path)
      (eval '(ert-deftest rdf-turtle-test-shextest-not-configured ()
               (ert-skip "shexc-shexj-test-shextest-path is unset -- see shexc-shexj-tests.el header"))
            t)
    (eval
     `(ert-deftest rdf-turtle-test-corpus-parses-without-error ()
        (let ((dir ,(expand-file-name "schemas/" shexc-shexj-test-shextest-path))
              (failures nil))
          (dolist (f (directory-files dir t "\\.ttl\\'"))
            (unless (member (file-name-nondirectory f) rdf-turtle-test--known-unsupported)
              (condition-case err
                  (rdf-turtle-parse-file f)
                (error (push (cons (file-name-nondirectory f) err) failures)))))
          (should (equal nil failures))))
     t)))

(rdf-turtle-test-regenerate-corpus-test)

(provide 'rdf-turtle-tests)

;;; rdf-turtle-tests.el ends here
