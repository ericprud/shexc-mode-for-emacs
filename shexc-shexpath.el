;;; shexc-shexpath.el --- ShExPath step representation and string format  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; ShExPath (https://shexspec.github.io/spec/ShExPath) is a notation for
;; uniquely addressing a location within a ShEx schema.  This file
;; implements only the narrow subset shexc-shexr.el actually needs:
;; locating a `:id'-bearing TripleExpr (TripleConstraint/EachOf/OneOf),
;; the one case where the same content can legitimately be reached from
;; more than one place in a ShExJ value-tree (a ShExC `$<id> (...)'
;; definition plus zero or more `&<id>' Include references) -- see
;; shexc-shexr.el's Commentary for why that ambiguity can't be resolved
;; from the RDF graph alone.  `:id' is set in exactly two places in
;; shexc-shexj.el (every ShapeDecl, and a TripleExpr with an explicit
;; `$<id>' annotation), and a ShapeDecl never needs a path at all (its
;; own `rdf:type sx:ShapeDecl' already disambiguates unconditionally --
;; see shexc-shexr.el), so this implementation never needs to address a
;; ShapeAnd/ShapeOr/ShapeNot/NodeConstraint operand, EXTENDS, `start',
;; an annotation, or a semantic action -- none of those can ever be on
;; the path to a `:id'.  The official spec's lexical edge cases
;; (whitespace, IRI-escaping, the absolute-path form) are also largely
;; informal/underspecified; the choices made here are this
;; implementation's own, documented as such, not citations of the spec.
;;
;; Grammar implemented:
;;
;;   ShexPath      ::= ShapeStep ("/" Step)*
;;   ShapeStep     ::= "@" Label                ; Label = "<" IRI ">" | "_:" BNODE-LABEL
;;   Step          ::= PredicateStep | ContextStep | ShapeStep
;;   PredicateStep ::= Label SP Index?          ; Label = predicate IRI/_:label; SP = one space
;;   ContextStep   ::= ("OneOf" | "EachOf") SP Index?
;;   Index         ::= NON-NEGATIVE-INTEGER     ; 0-based; defaults to 0; omitted from output when 0
;;
;; A `ShapeStep' occurs as the root (the enclosing ShapeDecl's own
;; label) -- the *only* place this file's own builder/comparison logic
;; ever needs one: descending from a TripleConstraint into its own
;; `:valueExpr' is either a dead end (a bare reference to a separately-
;; labeled ShapeDecl, which gets its own independent root-relative walk
;; regardless of who references it) or transparent (an anonymous
;; embedded shape-expr, entered with no step at all, exactly like
;; ShapeAnd/ShapeOr/ShapeNot wrapping already is -- see
;; `shexc-shexpath--walk-triple-constraint-value-expr').  The grammar
;; still allows a `ShapeStep' mid-path (matching the wider spec's own
;; capability, e.g. for a hand-written table entry), but nothing in
;; this codebase currently produces one there.  A `PredicateStep'
;; addresses one TripleConstraint by predicate within the *current*
;; group, `Index' disambiguating same-predicate siblings.
;; A `ContextStep' (`OneOf N'/`EachOf N') is used only when descending
;; into a *nested* bracketed EachOf/OneOf that is itself one element of
;; an enclosing group -- never needed at a ShapeDecl's or
;; TripleConstraint's own top-level group, where predicate addressing
;; (plus ShExJ's own singleton-group-collapse rule) already
;; disambiguates.  No leading-`/' absolute form is implemented (every
;; path this file produces/consumes already starts at a ShapeStep, so
;; it's dead weight for this narrowed use).  No `/'-escaping inside IRIs
;; is implemented either: IRIs are always `<...>'-bracketed here (never
;; PNAME-shortened, since this is an internal format, not user-facing
;; ShExC text), so the parser scans to the matching `>' before looking
;; for the next `/' -- no ambiguity, no escaping needed.
;;
;; A real tree-sitter-shexpath grammar has been suggested as a future
;; need, but is deliberately out of scope here: `tree-sitter-shexc'
;; lives in its own separate repo
;; (https://github.com/ericprud/tree-sitter-shexc), and a ShExPath
;; grammar would need the same treatment -- not something to scaffold
;; inside this Emacs Lisp package's own repo.  The regex/string-based
;; parser below is sufficient for this file's actual, narrow, never
;; user-facing-as-syntax need.

;;; Code:

(require 'cl-lib)
(require 'seq)

(define-error 'shexc-shexpath-error "ShExPath syntax error")

(defun shexc-shexpath--fail (fmt &rest args)
  "Signal a structured `shexc-shexpath-error', never an uncaught generic
Lisp error -- mirrors shexc-shexr.el's `shexc-shexr--fail' convention."
  (signal 'shexc-shexpath-error (list (apply #'format fmt args))))

;; ---------------------------------------------------------------------
;; Step representation: a list of tagged lists, always starting with a
;; `:shape' step --
;;   (:shape LABEL)            ; LABEL already bracketed: "<iri>"/"_:label"
;;   (:predicate LABEL INDEX)  ; LABEL likewise; INDEX a non-negative integer, never nil
;;   (:context CONTEXT INDEX)  ; CONTEXT "OneOf" or "EachOf"
;; ---------------------------------------------------------------------

(defun shexc-shexpath-make-shape-step (label)
  "LABEL is already in bracketed reference form (\"<iri>\"/\"_:label\")."
  (list :shape label))

(defun shexc-shexpath-make-predicate-step (label &optional index)
  "LABEL is already in bracketed reference form.  INDEX (a non-negative
integer, default 0) disambiguates same-predicate siblings within the
enclosing group, 0-based."
  (list :predicate label (or index 0)))

(defun shexc-shexpath-make-context-step (context &optional index)
  "CONTEXT is \"OneOf\" or \"EachOf\".  INDEX as in
`shexc-shexpath-make-predicate-step'."
  (list :context context (or index 0)))

;; ---------------------------------------------------------------------
;; String format
;; ---------------------------------------------------------------------

(defun shexc-shexpath--step-to-string (step)
  (pcase (nth 0 step)
    (:shape (concat "@" (nth 1 step)))
    ((or :predicate :context)
     (let ((label (nth 1 step)) (index (nth 2 step)))
       (if (and index (/= index 0)) (format "%s %d" label index) label)))))

(defun shexc-shexpath-to-string (steps)
  "STEPS (as built by `shexc-shexpath-make-shape-step' et al.) -> the
canonical ShExPath string.  Signals `shexc-shexpath-error' if STEPS is
empty or doesn't begin with a `:shape' step."
  (unless (and steps (eq (nth 0 (car steps)) :shape))
    (shexc-shexpath--fail "a ShExPath must begin with a shape step, got %S" steps))
  (mapconcat #'shexc-shexpath--step-to-string steps "/"))

(defun shexc-shexpath--split-steps (string)
  "Split STRING on top-level `/' separators, respecting `<...>'
IRI-bracket nesting (a `/' inside an IRI -- exceedingly common, e.g.
\"http://...\" -- is never mistaken for a step separator)."
  (let ((len (length string)) (start 0) (depth 0) (i 0) steps)
    (while (< i len)
      (let ((c (aref string i)))
        (cond
         ((eq c ?<) (setq depth (1+ depth)))
         ((eq c ?>) (setq depth (max 0 (1- depth))))
         ((and (eq c ?/) (= depth 0))
          (push (substring string start i) steps)
          (setq start (1+ i)))))
      (setq i (1+ i)))
    (push (substring string start len) steps)
    (nreverse steps)))

(defun shexc-shexpath--parse-step (token)
  (cond
   ((string-prefix-p "@" token) (shexc-shexpath-make-shape-step (substring token 1)))
   ((string-empty-p token) (shexc-shexpath--fail "empty ShExPath step"))
   ((string-match " \\([0-9]+\\)\\'" token)
    (let ((label (substring token 0 (match-beginning 0)))
          (index (string-to-number (match-string 1 token))))
      (if (member label '("OneOf" "EachOf"))
          (shexc-shexpath-make-context-step label index)
        (shexc-shexpath-make-predicate-step label index))))
   ((member token '("OneOf" "EachOf")) (shexc-shexpath-make-context-step token))
   (t (shexc-shexpath-make-predicate-step token))))

(defun shexc-shexpath-parse (string)
  "STRING -> a list of steps, the inverse of `shexc-shexpath-to-string'.
Signals `shexc-shexpath-error' (never an uncaught generic Lisp error)
on malformed input."
  (let ((steps (condition-case err
                   (mapcar #'shexc-shexpath--parse-step (shexc-shexpath--split-steps string))
                 (shexc-shexpath-error (signal (car err) (cdr err)))
                 (error (signal 'shexc-shexpath-error (list (error-message-string err)))))))
    (unless (eq (nth 0 (car steps)) :shape)
      (shexc-shexpath--fail "ShExPath %S must begin with a shape step" string))
    steps))

(defun shexc-shexpath-equal (steps-a steps-b)
  "Structural equality of two step-lists.  Plain `equal' already
suffices: every constructor above normalizes a default/omitted INDEX
to the integer 0 (never nil), on both the value-tree-side builder
\(`shexc-shexpath-id-locations') and the parser above, so an explicit
\"0\" index and an omitted one always produce `equal' step-lists, never
needing a separate normalization pass here."
  (equal steps-a steps-b))

;; ---------------------------------------------------------------------
;; `shexc-shexpath-id-locations': a pure post-hoc walk of a *compiled,
;; unambiguous* ShExJ value-tree (see shexc-shexj.el), recording where
;; each `:id' is defined -- not a disambiguation decision (a freshly
;; compiled tree has, by construction, exactly one full-plist occurrence
;; per `:id' and zero or more bare-string Include references elsewhere,
;; so there is nothing to disambiguate here; the *actual* disambiguation
;; against possibly-hand-edited input happens in shexc-shexr.el's
;; `shexc-shexr--ambiguous-id-winners', which consults the table this
;; function builds).  No compiler-internals hook is needed (unlike
;; shexc-shexj.el's own `shexc-shexj--position-table', which exists only
;; because ts-node spans aren't recoverable from a finished value-tree;
;; here the finished tree's own shape already has everything needed).
;; Deliberately has no dependency on shexc-shexj.el/shexc-shexr.el
;; beyond each file's public value-tree shape, matching this project's
;; existing layering discipline (see shexc-shexr.el's Commentary on the
;; same principle).
;; ---------------------------------------------------------------------

(defun shexc-shexpath--ref-label (id)
  "ID (a bare absolute IRI or `_:label' string -- exactly `:id''s own
value-tree form, see shexc-shexj.el) as a ShExPath step label:
`<iri>'/`_:label' unchanged.  Mirrors `shexc-shexr--ref-text', kept
local rather than depending on shexc-shexr.el for one conditional."
  (if (string-prefix-p "_:" id) id (concat "<" id ">")))

(defun shexc-shexpath--walk-shape-expr (se path acc)
  "Walk ShExJ shapeExpr SE (a plist, a bare ShapeDecl-ref string -- a
dead end, never itself `:id'-bearing -- or nil) for `:id'-bearing
TripleExpr content reachable through it, at the current ShExPath PATH
\(a list of steps in *reverse* order -- most-recently-pushed first, for
cheap `push'-style growth; only reversed when actually recording a
location).  ACC accumulates `(id . ShExPath-string)' pairs, in reverse
order (see `shexc-shexpath-id-locations')."
  (cond
   ((or (null se) (stringp se)) acc)
   ((equal (plist-get se :type) "Shape")
    (shexc-shexpath--walk-triple-expr (plist-get se :expression) path acc))
   ((member (plist-get se :type) '("ShapeAnd" "ShapeOr"))
    (dolist (operand (plist-get se :shapeExprs))
      (setq acc (shexc-shexpath--walk-shape-expr operand path acc)))
    acc)
   ((equal (plist-get se :type) "ShapeNot")
    (shexc-shexpath--walk-shape-expr (plist-get se :shapeExpr) path acc))
   (t acc))) ; NodeConstraint/ShapeExternal -- no :id ever reachable through these

(defun shexc-shexpath--walk-triple-constraint-value-expr (tc path acc)
  "Descend into TC's (a TripleConstraint) own `:valueExpr' for further
`:id'-bearing content, at the *same* PATH -- a bare-string `:valueExpr'
\(a reference to a separately-labeled ShapeDecl) is a dead end here:
that ShapeDecl carries no `:id' of its own reachable through this
predicate at all, and any `:id'-bearing content nested *inside* it is
already found independently via that ShapeDecl's own top-level walk in
`shexc-shexpath-id-locations' -- chaining through the reference instead
would risk an infinite loop on a recursive schema and buys nothing,
since every ShapeDecl is walked from its own root regardless of who
references it.  An anonymous embedded shape-expr (a plist, no separate
label) is entered *transparently* -- no step pushed -- exactly like
`shexc-shexpath--walk-shape-expr' already does for ShapeAnd/ShapeOr/
ShapeNot wrapping: it isn't a separately-addressable point of its own,
just structure ShExC's grammar forces around a predicate's nested body
\(confirmed empirically: this is the difference between the two worked
examples in shexc-shexpath.el's Commentary -- the second one's `:id' is
found at `@<S2>/<p0> 1', with no further hop for the anonymous Shape
ShExC's `<p0> { ... }' syntax necessarily wraps the body in)."
  (let ((ve (plist-get tc :valueExpr)))
    (if (stringp ve) acc (shexc-shexpath--walk-shape-expr ve path acc))))

(defun shexc-shexpath--walk-group-elements (te path acc)
  "TE is an EachOf/OneOf; walk its `:expressions', pushing the
appropriate `PredicateStep'/`ContextStep' for each child to extend
PATH, mirroring shexc-shexj.el's own `shexc-shexj--compile-group-
triple-expr'/`--compile-one-of' structure -- see
`shexc-shexpath--walk-triple-expr'."
  (let ((predicate-counts (make-hash-table :test #'equal))
        (context-counts (make-hash-table :test #'equal)))
    (dolist (child (plist-get te :expressions))
      (cond
       ((stringp child) nil) ; bare Include reference as a group element -- no :id reachable through it
       ((equal (plist-get child :type) "TripleConstraint")
        (let* ((pred (shexc-shexpath--ref-label (plist-get child :predicate)))
               (index (gethash pred predicate-counts 0)))
          (puthash pred (1+ index) predicate-counts)
          (setq acc (shexc-shexpath--walk-triple-expr
                     child (cons (shexc-shexpath-make-predicate-step pred index) path) acc))))
       ((member (plist-get child :type) '("EachOf" "OneOf"))
        (let* ((ctx (plist-get child :type))
               (index (gethash ctx context-counts 0)))
          (puthash ctx (1+ index) context-counts)
          (setq acc (shexc-shexpath--walk-triple-expr
                     child (cons (shexc-shexpath-make-context-step ctx index) path) acc))))))
    acc))

(defun shexc-shexpath--walk-triple-expr (te path acc)
  "Walk ShExJ triple expression TE (a plist, or a bare Include-reference
string -- a dead end) for `:id'-bearing content, recording into ACC at
the current PATH if TE's own `:id' is non-nil, then recursing -- into a
TripleConstraint's `:valueExpr' (deeper shape-expr content), or an
EachOf/OneOf's `:expressions' (sibling triple-expr content)."
  (cond
   ((or (null te) (stringp te)) acc)
   (t
    (when (plist-get te :id)
      (setq acc (cons (cons (plist-get te :id) (shexc-shexpath-to-string (reverse path))) acc)))
    (cond
     ((equal (plist-get te :type) "TripleConstraint")
      (shexc-shexpath--walk-triple-constraint-value-expr te path acc))
     ((member (plist-get te :type) '("EachOf" "OneOf"))
      (shexc-shexpath--walk-group-elements te path acc))
     (t acc)))))

(defun shexc-shexpath-id-locations (schema)
  "Return an alist mapping every `:id' appearing in SCHEMA (a compiled,
unambiguous ShExJ value-tree, fresh from
`shexc-shexj-compile-buffer'/`-compile-node', or any value-tree
fragment with the same shape) to the canonical ShExPath string
describing where that id's content is defined -- see this section's
top Commentary for why this is a pure recording walk, not a
disambiguation decision."
  (let (acc)
    (dolist (decl (plist-get schema :shapes))
      (let ((path (list (shexc-shexpath-make-shape-step (shexc-shexpath--ref-label (plist-get decl :id))))))
        (setq acc (shexc-shexpath--walk-shape-expr (plist-get decl :shapeExpr) path acc))))
    (nreverse acc)))

(provide 'shexc-shexpath)

;;; shexc-shexpath.el ends here
