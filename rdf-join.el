;;; rdf-join.el --- Shared nested-loop join engine for RDF pattern matching  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The iterative, non-recursive join shared by `rdf-isomorphism.el'
;; (bijective blank-node matching) and `rdf-bgp.el' (SPARQL Basic
;; Graph Pattern matching): a `dolist' over an ordered list of pattern
;; quads, with every live partial match tracked breadth-first in a row
;; list (an alist of pattern-term -> target-term bindings per row),
;; rather than recursion per pattern variable -- this is what avoids
;; the `max-lisp-eval-depth' problem the original recursive
;; backtracking version of `rdf-isomorphism.el' hit on shexTest's
;; ~160-blank-node `_all.ttl' fixture.
;;
;; What differs between isomorphism and BGP matching is *not* the join
;; mechanics -- it's three orthogonal questions, each a parameter here:
;; - Which terms are "variables" to bind (blank nodes, for isomorphism;
;;   `rdf-model-variable's, for BGP) -- VARIABLE-P.
;; - Whether two distinct pattern variables may bind to the same target
;;   term.  Isomorphism requires a bijection, so no; ordinary SPARQL
;;   BGP matching computes a *homomorphism*, so yes -- INJECTIVE-P.
;; - Whether a binding needs an extra caller-specific sanity check
;;   before it's accepted (isomorphism additionally restricts
;;   candidates to a color-refinement signature class; BGP matching
;;   has no such filter) -- ALLOWED-P.
;;
;; This file is currently bundled inside the `shexc-ts-mode' MELPA
;; package rather than shipped as its own package -- see the README's
;; "Packaging" section.  Named after the package it's meant to become.

;;; Code:

(require 'cl-lib)
(require 'rdf-model)

(defun rdf-join-index-by-predicate (quads)
  "Hash table: predicate term -> list of QUADS with that predicate.
RDF predicates are always IRIs, never pattern variables, so this is
how every join step narrows candidates before checking subject/object/
graph at all."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (q quads) (push q (gethash (rdf-model-quad-predicate q) table)))
    table))

(defun rdf-join--resolve (term row variable-p)
  "TERM's current binding in ROW if VARIABLE-P says it's a variable
\(nil if unbound -- a wildcard), else TERM itself."
  (if (funcall variable-p term) (cdr (assoc term row)) term))

(defun rdf-join--consider (pat-term cand-term row-box variable-p injective-p allowed-p)
  "Extend (the row in) ROW-BOX to bind PAT-TERM to CAND-TERM, or verify
consistency if PAT-TERM is already bound.  Returns nil (ROW-BOX
unchanged) if the binding is invalid: inconsistent with an existing
binding for PAT-TERM, would collapse two distinct pattern variables
onto one target term while INJECTIVE-P is non-nil, or ALLOWED-P
\(if non-nil) rejects it.  Non-variable PAT-TERMs always pass --
matching against CAND-TERM already happened via the caller's
positional `equal' check before `rdf-join--consider' is reached."
  (if (not (funcall variable-p pat-term))
      t
    (let* ((row (car row-box))
           (existing (assoc pat-term row)))
      (if existing
          (equal (cdr existing) cand-term)
        (and (or (not injective-p) (not (rassoc cand-term row)))
             (or (not allowed-p) (funcall allowed-p pat-term cand-term))
             (progn (setcar row-box (cons (cons pat-term cand-term) row)) t))))))

(defun rdf-join-evaluate (quad-order index-by-predicate variable-p &optional injective-p allowed-p)
  "Iteratively join QUAD-ORDER (an ordered list of pattern quads)
against INDEX-BY-PREDICATE (target quads grouped by predicate, from
`rdf-join-index-by-predicate'), binding every term VARIABLE-P
identifies as a pattern variable.

Returns a list of every complete row of bindings -- alists
\((pattern-term . target-term) ...) -- or nil if no row makes every
pattern in QUAD-ORDER match some quad in INDEX-BY-PREDICATE.  An empty
QUAD-ORDER trivially returns a single row with no bindings.

INJECTIVE-P, if non-nil, rejects any row where two distinct pattern
variables would bind to the same target term (required for graph
isomorphism; leave nil for SPARQL-style homomorphism matching, where
that's allowed).

ALLOWED-P, if non-nil, is called as (ALLOWED-P pattern-term
candidate-term) the first time PATTERN-TERM is bound in a row and must
return non-nil for the binding to be accepted -- e.g. graph isomorphism
uses this to additionally restrict candidates to a color-refinement
signature class; ordinary BGP matching passes nil here."
  (let ((rows (list nil)))
    (catch 'rdf-join-no-match
      (dolist (pattern quad-order)
        (let (next-rows)
          (dolist (row rows)
            (let ((s (rdf-join--resolve (rdf-model-quad-subject pattern) row variable-p))
                  (o (rdf-join--resolve (rdf-model-quad-object pattern) row variable-p))
                  (g (rdf-join--resolve (rdf-model-quad-graph pattern) row variable-p)))
              (dolist (cand (gethash (rdf-model-quad-predicate pattern) index-by-predicate))
                (when (and (or (null s) (equal s (rdf-model-quad-subject cand)))
                           (or (null o) (equal o (rdf-model-quad-object cand)))
                           (or (null g) (equal g (rdf-model-quad-graph cand))))
                  (let ((row-box (list row)))
                    (when (and (rdf-join--consider (rdf-model-quad-subject pattern) (rdf-model-quad-subject cand)
                                                    row-box variable-p injective-p allowed-p)
                               (rdf-join--consider (rdf-model-quad-object pattern) (rdf-model-quad-object cand)
                                                    row-box variable-p injective-p allowed-p)
                               (rdf-join--consider (rdf-model-quad-graph pattern) (rdf-model-quad-graph cand)
                                                    row-box variable-p injective-p allowed-p))
                      (push (car row-box) next-rows)))))))
          (setq rows next-rows)
          (unless rows (throw 'rdf-join-no-match nil))))
      rows)))

(provide 'rdf-join)

;;; rdf-join.el ends here
