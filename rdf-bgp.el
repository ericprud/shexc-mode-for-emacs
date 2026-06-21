;;; rdf-bgp.el --- SPARQL Basic Graph Pattern matching  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0") (rdf-join "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; SPARQL's own term for what this matches against a graph -- a Basic
;; Graph Pattern, i.e. a set of triple patterns sharing variables --
;; and for what comes back -- a (possibly empty) set of *solution
;; mappings*, each one a binding of every pattern variable to an RDF
;; term such that substituting the binding into the pattern yields a
;; subset of the target graph.  See `rdf-bgp-match', the public entry
;; point.
;;
;; Shares its join engine (`rdf-join.el') with `rdf-isomorphism.el',
;; but configures it very differently:
;; - The pattern's *variables* are `rdf-model-variable' terms (real
;;   SPARQL ?x-style variables), not blank nodes.  A blank node written
;;   directly into a pattern is matched as an ordinary constant --
;;   exactly SPARQL's own treatment of blank nodes in a query's WHERE
;;   clause (they're local to the query, but they're not variables).
;; - Matching is a *homomorphism*, not a bijection: two distinct
;;   pattern variables may legally bind to the same target term, and
;;   the target graph may contain quads the pattern never mentions.
;;   `rdf-isomorphism.el' needs the opposite of both.
;; - The result is the actual set of solution mappings, not a yes/no --
;;   that's the whole point of matching a *pattern* rather than
;;   checking two *datasets* for equality.
;;
;; This file is currently bundled inside the `shexc-ts-mode' MELPA
;; package rather than shipped as its own package -- see the README's
;; "Packaging" section.  Named after the package it's meant to become.

;;; Code:

(require 'cl-lib)
(require 'rdf-model)
(require 'rdf-join)

(defun rdf-bgp--pattern-variables (quad)
  "The `rdf-model-variable' terms among QUAD's subject/object/graph
\(never predicate -- RDF predicates are always IRIs, never variables)."
  (cl-remove-if-not #'rdf-model-variable-p
                     (list (rdf-model-quad-subject quad) (rdf-model-quad-object quad)
                           (rdf-model-quad-graph quad))))

(defun rdf-bgp--quad-order (pattern)
  "PATTERN's quads, greedily reordered so the next quad scheduled is
always the one sharing the most variables with quads already placed
\(ties broken by original order).  PATTERN quads are typically few
enough that this simple heuristic -- no statistics, no color
refinement -- is all that's worth doing; it only affects how early a
non-matching pattern gets pruned, not correctness."
  (let ((bound (make-hash-table :test #'equal))
        (remaining (copy-sequence pattern))
        order)
    (cl-labels ((score (q) (length (cl-remove-if-not (lambda (v) (gethash v bound))
                                                      (rdf-bgp--pattern-variables q)))))
      (while remaining
        (let ((next (car (sort (copy-sequence remaining) (lambda (x y) (> (score x) (score y)))))))
          (dolist (v (rdf-bgp--pattern-variables next)) (puthash v t bound))
          (push next order)
          (setq remaining (delq next remaining)))))
    (nreverse order)))

(defun rdf-bgp-match (pattern target)
  "Match PATTERN against TARGET, SPARQL-Basic-Graph-Pattern style.

PATTERN is a list of `rdf-model-quad's whose `rdf-model-variable'
terms are the pattern's variables; any blank node in PATTERN is
matched as a constant, not bound.  TARGET is an `rdf-model' dataset.

Returns a list of solution mappings -- alists ((variable . term) ...),
one per distinct way PATTERN's variables can be bound so that every
bound pattern quad is present in TARGET -- or nil if there is none.
Unlike `rdf-isomorphism-datasets-equal-p', this is a homomorphism
match: distinct variables may bind to the same term, and TARGET may
contain quads beyond anything PATTERN mentions."
  (rdf-join-evaluate (rdf-bgp--quad-order pattern)
                      (rdf-join-index-by-predicate (rdf-model-dataset-quads target))
                      #'rdf-model-variable-p))

(provide 'rdf-bgp)

;;; rdf-bgp.el ends here
