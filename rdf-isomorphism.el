;;; rdf-isomorphism.el --- Blank-node-bijection RDF dataset equality  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-model "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Checks whether two `rdf-model' datasets are equal up to blank-node
;; renaming -- the "blank node bijection" problem -- via SPARQL-BGP-style
;; matching: dataset A's quads are treated as triple *patterns* (its
;; blank nodes are the only "variables"), matched against dataset B's
;; quads as ground data.  Named nodes/literals must match exactly via
;; `equal'.  Deliberately *not* full RDF Dataset Canonicalization
;; (URDNA2015 and friends): a much bigger lift in elisp (or a JS
;; dependency) than the graphs this is built for need (shexTest
;; fixtures, individual ShExR schemas).
;;
;; Evaluation is an iterative nested-loop join over an ordered list of
;; patterns (`rdf-isomorphism--quad-order'), not recursion over blank
;; nodes -- depth is a `dolist' over patterns, with all live partial
;; matches tracked breadth-first in a result-row list, trading stack
;; depth for list width.  This avoids the `max-lisp-eval-depth' problem
;; a per-blank-node-recursive backtracking search hit against
;; shexTest's schemas/_all.ttl (~160 blank nodes) in an earlier version
;; of this file.
;;
;; Ordinary SPARQL BGP matching computes a graph *homomorphism*: two
;; pattern variables may legally bind to the same target node.  Graph
;; *isomorphism* (what dataset equality needs) requires a *bijection* --
;; two distinct blank nodes must never collapse onto one.  Every join
;; step here enforces that explicitly (`rdf-isomorphism--consider'),
;; not just the final result.
;;
;; Patterns are ordered by one frontier-expansion traversal (numbering
;; blank nodes by when they're first reached from a root, preferring
;; the most color-refinement-constrained node at each step -- see
;; `rdf-isomorphism--bnode-order') over the blank-node co-occurrence
;; graph, then scheduling each pattern right after the last of its
;; blank nodes becomes numbered.  A single visited-set traversal
;; uniformly covers trees, cycles, and disconnected components alike --
;; no separate cases needed.  (Ordering only affects how early
;; mismatches get pruned; correctness doesn't depend on it being a
;; particularly good order.)  A few rounds of color-refinement
;; (1-dimensional-Weisfeiler-Leman-style: a blank node's signature is
;; the shape of every quad it appears in, with neighbor blank nodes
;; folded in via *their* previous round's signature) computes, for
;; each of A's blank nodes, the restricted set of B's blank nodes it
;; could possibly match -- both for ordering and as a same-signature
;; filter applied at every join step, which is what actually keeps the
;; result-row list narrow on graphs with repeated local structure (like
;; _all.ttl's many similar shapes), not ordering alone.
;;
;; This file is currently bundled inside the `shexc-ts-mode' MELPA
;; package rather than shipped as its own package -- see the README's
;; "Packaging" section.  Named after the package it's meant to become.

;;; Code:

(require 'cl-lib)
(require 'rdf-model)

(defvar rdf-isomorphism-refinement-rounds 3
  "How many rounds of color-refinement to run before joining.
Round 0 distinguishes blank nodes only by their non-blank neighbors;
each further round also folds in neighbors' previous-round signatures,
so blank nodes whose *local* neighborhoods are identical but whose
neighbors-of-neighbors differ still end up in different classes.  This
only affects how early mismatches get pruned -- correctness doesn't
depend on it (this variable's value 0 is still correct, just
potentially slower for highly symmetric graphs).")

;;; Blank node collection

(defun rdf-isomorphism--blank-nodes (quads)
  "Distinct blank-node terms appearing anywhere in QUADS, in first-seen order."
  (let ((seen (make-hash-table :test #'equal)) (acc nil))
    (dolist (q quads)
      (dolist (term (list (rdf-model-quad-subject q) (rdf-model-quad-predicate q)
                           (rdf-model-quad-object q) (rdf-model-quad-graph q)))
        (when (and (rdf-model-blank-node-p term) (not (gethash term seen)))
          (puthash term t seen)
          (push term acc))))
    (nreverse acc)))

(defun rdf-isomorphism--quads-by-blank-node (quads bnodes)
  "Hash table: blank-node term -> list of quads it appears in."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (b bnodes) (puthash b nil table))
    (dolist (q quads)
      (dolist (term (list (rdf-model-quad-subject q) (rdf-model-quad-predicate q)
                           (rdf-model-quad-object q) (rdf-model-quad-graph q)))
        (when (rdf-model-blank-node-p term)
          (push q (gethash term table)))))
    table))

;;; Color refinement (signature classes, for ordering + join-time filtering)

(defun rdf-isomorphism--term-key (term prev-sig round)
  "TERM as a signature-component: itself if not a blank node; on round
0, a generic marker (no neighbor info yet); on later rounds, its
previous round's signature (from PREV-SIG, a hash table)."
  (cond
   ((not (rdf-model-blank-node-p term)) term)
   ((= round 0) 'rdf-isomorphism--blank)
   (t (or (gethash term prev-sig) 'rdf-isomorphism--blank))))

(defun rdf-isomorphism--bnode-signature (bnode quads prev-sig round)
  "BNODE's signature for ROUND: a string summarizing the shape of every
quad it appears in, with its own position(s) marked `self' and any
blank-node neighbor replaced by PREV-SIG's value for it (or a generic
marker on round 0)."
  (cl-labels ((key (term) (if (equal term bnode)
                               'rdf-isomorphism--self
                             (rdf-isomorphism--term-key term prev-sig round))))
    (format "%S"
            (sort (mapcar (lambda (q)
                             (format "%S" (list (key (rdf-model-quad-subject q))
                                                 (key (rdf-model-quad-predicate q))
                                                 (key (rdf-model-quad-object q))
                                                 (key (rdf-model-quad-graph q)))))
                           quads)
                  #'string<))))

(defun rdf-isomorphism--signatures (quads bnodes rounds)
  "Hash table: blank-node term -> its signature string after ROUNDS
rounds of refinement."
  (let ((quads-by-node (rdf-isomorphism--quads-by-blank-node quads bnodes))
        (sig (make-hash-table :test #'equal)))
    (dotimes (round (max 1 rounds))
      (let ((next (make-hash-table :test #'equal)))
        (dolist (b bnodes)
          (puthash b (rdf-isomorphism--bnode-signature b (gethash b quads-by-node) sig round) next))
        (setq sig next)))
    sig))

;;; Partition-histogram pre-check

(defun rdf-isomorphism--histogram (bnodes sig)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (b bnodes) (cl-incf (gethash (gethash b sig) h 0)))
    h))

(defun rdf-isomorphism--histograms-match-p (bnodes-a sig-a bnodes-b sig-b)
  (let ((hist-a (rdf-isomorphism--histogram bnodes-a sig-a))
        (hist-b (rdf-isomorphism--histogram bnodes-b sig-b)))
    (and (= (hash-table-count hist-a) (hash-table-count hist-b))
         (catch 'mismatch
           (maphash (lambda (k v) (unless (eql v (gethash k hist-b)) (throw 'mismatch nil))) hist-a)
           t))))

(defun rdf-isomorphism--candidates-by-sig (bnodes sig)
  "Hash table: signature string -> list of BNODES sharing it."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (b bnodes) (push b (gethash (gethash b sig) table)))
    table))

;;; Pattern ordering: one frontier-expansion traversal over A's blank
;;; nodes (trees/cycles/disconnected components all handled uniformly
;;; by the visited set), most color-refinement-constrained node first,
;;; then quads scheduled right after the last of their blank nodes is
;;; reached.

(defun rdf-isomorphism--bnode-order (bnodes sig candidates-by-sig-b quads-by-node)
  (let ((visited (make-hash-table :test #'equal))
        (order nil)
        (remaining (copy-sequence bnodes)))
    (cl-labels ((class-size (b) (length (gethash (gethash b sig) candidates-by-sig-b)))
                (unvisited-neighbors (b)
                  (let (acc)
                    (dolist (q (gethash b quads-by-node))
                      (dolist (term (list (rdf-model-quad-subject q) (rdf-model-quad-object q)))
                        (when (and (rdf-model-blank-node-p term) (not (equal term b))
                                   (not (gethash term visited))
                                   (not (member term acc)))
                          (push term acc))))
                    acc))
                (visit (b)
                  (puthash b t visited)
                  (push b order)
                  (setq remaining (delete b remaining))))
      (while remaining
        ;; start the next component at its most-constrained node
        (let* ((root (car (sort (copy-sequence remaining)
                                  (lambda (x y) (< (class-size x) (class-size y))))))
               (frontier (list root)))
          (while frontier
            (setq frontier (sort frontier (lambda (x y) (< (class-size x) (class-size y)))))
            (let ((b (pop frontier)))
              (unless (gethash b visited)
                (visit b)
                (setq frontier (append frontier (unvisited-neighbors b))))))))
      (nreverse order))))

(defun rdf-isomorphism--quad-order (quads bnode-order)
  "QUADS ordered so each one is scheduled right after the last (per
BNODE-ORDER) of its blank-node terms becomes available; quads with no
blank nodes sort first (rank -1, vacuously ready)."
  (let ((position (make-hash-table :test #'equal)) (i 0))
    (dolist (b bnode-order) (puthash b i position) (cl-incf i))
    (cl-labels ((quad-rank (q)
                  (let ((mx -1))
                    (dolist (term (list (rdf-model-quad-subject q) (rdf-model-quad-object q)))
                      (when (rdf-model-blank-node-p term)
                        (setq mx (max mx (gethash term position)))))
                    mx)))
      (sort (copy-sequence quads) (lambda (x y) (< (quad-rank x) (quad-rank y)))))))

;;; Evaluation: iterative join.  Each row is an alist
;;; ((pattern-blank-node . target-blank-node) ...) -- extended via
;;; `cons' (O(1), shares structure with its parent row) rather than
;;; hash-table copying.

(defun rdf-isomorphism--index-by-predicate (quads)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (q quads) (push q (gethash (rdf-model-quad-predicate q) table)))
    table))

(defun rdf-isomorphism--consider (pat-term cand-term row-box sig-a candidates-by-sig-b)
  "Extend (the row in) ROW-BOX to bind PAT-TERM (if it's a blank node
and not already bound) to CAND-TERM, or verify consistency if it's
already bound.  Returns nil (and leaves ROW-BOX's row unchanged) if
CAND-TERM can't possibly be valid for PAT-TERM: not a blank node, would
collapse two distinct pattern blank nodes onto one target blank node
\(the homomorphism-vs-isomorphism bijection requirement), or is outside
PAT-TERM's color-refinement candidate class."
  (if (not (rdf-model-blank-node-p pat-term))
      t
    (let* ((row (car row-box))
           (existing (assoc pat-term row)))
      (if existing
          (equal (cdr existing) cand-term)
        (and (rdf-model-blank-node-p cand-term)
             (not (rassoc cand-term row))
             (member cand-term (gethash (gethash pat-term sig-a) candidates-by-sig-b))
             (progn (setcar row-box (cons (cons pat-term cand-term) row)) t))))))

(defun rdf-isomorphism--evaluate (quad-order sig-a candidates-by-sig-b index-b)
  "Non-nil if some row of bindings makes every pattern in QUAD-ORDER
(A's quads) match a real quad in B (via INDEX-B, B's quads indexed by
predicate), injectively."
  (let ((rows (list nil))) ; one row so far: the empty binding alist
    (catch 'no-match
      (dolist (pattern quad-order)
        (let (next-rows)
          (dolist (row rows)
            (cl-labels ((resolve (term) (if (rdf-model-blank-node-p term) (cdr (assoc term row)) term)))
              (let ((s (resolve (rdf-model-quad-subject pattern)))
                    (o (resolve (rdf-model-quad-object pattern)))
                    (g (resolve (rdf-model-quad-graph pattern))))
                (dolist (cand (gethash (rdf-model-quad-predicate pattern) index-b))
                  (when (and (or (null s) (equal s (rdf-model-quad-subject cand)))
                             (or (null o) (equal o (rdf-model-quad-object cand)))
                             (or (null g) (equal g (rdf-model-quad-graph cand))))
                    (let ((row-box (list row)))
                      (when (and (rdf-isomorphism--consider (rdf-model-quad-subject pattern)
                                                              (rdf-model-quad-subject cand)
                                                              row-box sig-a candidates-by-sig-b)
                                 (rdf-isomorphism--consider (rdf-model-quad-object pattern)
                                                              (rdf-model-quad-object cand)
                                                              row-box sig-a candidates-by-sig-b)
                                 (rdf-isomorphism--consider (rdf-model-quad-graph pattern)
                                                              (rdf-model-quad-graph cand)
                                                              row-box sig-a candidates-by-sig-b))
                        (push (car row-box) next-rows))))))))
          (setq rows next-rows)
          (unless rows (throw 'no-match nil))))
      (and rows t))))

;;; Public API

(defun rdf-isomorphism--quad-has-blank-node-p (quad)
  (cl-some #'rdf-model-blank-node-p
           (list (rdf-model-quad-subject quad) (rdf-model-quad-predicate quad)
                 (rdf-model-quad-object quad) (rdf-model-quad-graph quad))))

(defun rdf-isomorphism-datasets-equal-p (store-a store-b)
  "Return non-nil if STORE-A and STORE-B (`rdf-model' datasets) are
equal up to blank-node renaming.  Named nodes/literals must match
exactly via `equal'; quad counts, ground (blank-node-free) quads, and
blank-node counts are checked first as cheap necessary conditions
before any join."
  (let ((quads-a (rdf-model-dataset-quads store-a))
        (quads-b (rdf-model-dataset-quads store-b)))
    (and (= (length quads-a) (length quads-b))
         (let* ((bnodes-a (rdf-isomorphism--blank-nodes quads-a))
                (bnodes-b (rdf-isomorphism--blank-nodes quads-b))
                (ground-a (cl-remove-if #'rdf-isomorphism--quad-has-blank-node-p quads-a)))
           (and (= (length bnodes-a) (length bnodes-b))
                (cl-every (lambda (q) (rdf-model-dataset-has store-b q)) ground-a)
                (let* ((quads-by-node-a (rdf-isomorphism--quads-by-blank-node quads-a bnodes-a))
                       (sig-a (rdf-isomorphism--signatures quads-a bnodes-a rdf-isomorphism-refinement-rounds))
                       (sig-b (rdf-isomorphism--signatures quads-b bnodes-b rdf-isomorphism-refinement-rounds)))
                  (and (rdf-isomorphism--histograms-match-p bnodes-a sig-a bnodes-b sig-b)
                       (let* ((candidates-by-sig-b (rdf-isomorphism--candidates-by-sig bnodes-b sig-b))
                              (bnode-order (rdf-isomorphism--bnode-order bnodes-a sig-a candidates-by-sig-b quads-by-node-a))
                              (quad-order (rdf-isomorphism--quad-order quads-a bnode-order))
                              (index-b (rdf-isomorphism--index-by-predicate quads-b)))
                         (rdf-isomorphism--evaluate quad-order sig-a candidates-by-sig-b index-b)))))))))

(provide 'rdf-isomorphism)

;;; rdf-isomorphism.el ends here
