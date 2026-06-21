;;; rdf-store.el --- In-memory RDF dataset  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A concrete, flat-list-backed implementation of `rdf-model.el's
;; `Dataset' protocol -- the RDF/JS equivalent of `@rdfjs/dataset' (or
;; N3.js's `Store').  A list is adequate (and simplest to get right)
;; for the dataset sizes this is built for so far: shexTest fixtures
;; and individual ShExR schemas, not arbitrary-scale RDF storage.
;;
;; This file is currently bundled inside the `shexc-ts-mode' MELPA
;; package rather than shipped as its own package -- see the README's
;; "Packaging" section.  Named after the package it's meant to become.

;;; Code:

(require 'cl-lib)
(require 'rdf-model)

(cl-defstruct (rdf-store (:constructor rdf-store-create))
  "A `Dataset' backed by a flat list of quads."
  (quads nil))

(cl-defmethod rdf-model-dataset-add ((store rdf-store) quad)
  (unless (rdf-model-dataset-has store quad)
    (push quad (rdf-store-quads store)))
  store)

(cl-defmethod rdf-model-dataset-delete ((store rdf-store) quad)
  (setf (rdf-store-quads store)
        (cl-remove quad (rdf-store-quads store) :test #'equal))
  store)

(cl-defmethod rdf-model-dataset-has ((store rdf-store) quad)
  (and (member quad (rdf-store-quads store)) t))

(cl-defmethod rdf-model-dataset-match ((store rdf-store)
                                        &optional subject predicate object graph)
  (cl-remove-if-not
   (lambda (q)
     (and (or (null subject) (equal subject (rdf-model-quad-subject q)))
          (or (null predicate) (equal predicate (rdf-model-quad-predicate q)))
          (or (null object) (equal object (rdf-model-quad-object q)))
          (or (null graph) (equal graph (rdf-model-quad-graph q)))))
   (rdf-store-quads store)))

(cl-defmethod rdf-model-dataset-quads ((store rdf-store))
  (rdf-store-quads store))

(cl-defmethod rdf-model-dataset-size ((store rdf-store))
  (length (rdf-store-quads store)))

(defun rdf-store-from-quads (quads)
  "Return a new `rdf-store' containing QUADS (a list), deduplicated."
  (let ((store (rdf-store-create)))
    (dolist (q quads) (rdf-model-dataset-add store q))
    store))

(provide 'rdf-store)

;;; rdf-store.el ends here
