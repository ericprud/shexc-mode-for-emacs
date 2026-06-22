;;; rdf-turtle.el --- Turtle reader: tree-sitter-turtle parse tree -> rdf-model quads  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-core "0.1.0") (rdf-model "0.1.0") (rdf-store "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Walks a `tree-sitter-turtle' parse tree (the same grammar
;; `turtle-ts-mode' uses) into `rdf-model' quads/an `rdf-store' --
;; deliberately independent of `turtle-ts-mode.el' itself, so batch
;; consumers (tests, future validators) don't need to load any editing
;; machinery (font-lock, indent, hideshow) just to parse a file.  The
;; grammar's `treesit-language-source-alist' registration lives in
;; `rdf-core.el', shared by both this file and `turtle-ts-mode.el'.
;;
;; Scope: the default graph only (no `GRAPH'/TriG -- confirmed unneeded
;; against the full shexTest schemas/*.ttl corpus, see rdf-turtle-tests.el).
;; `( ... )' collections become the standard RDF list encoding
;; (`rdf:first'/`rdf:rest'/`rdf:nil'), matching how real Turtle->RDF
;; conversion (and the ShExR `.ttl' fixtures' own collections) works.
;;
;; This file is currently bundled inside the `shexc-ts-mode' MELPA
;; package rather than shipped as its own package -- see the README's
;; "Packaging" section.  Named after the package it's meant to become.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'url-expand)
(require 'rdf-core)
(require 'rdf-model)
(require 'rdf-store)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-buffer-root-node "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-count "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

;;; Vocabulary constants

(defconst rdf-turtle--rdf-ns "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(defconst rdf-turtle--xsd-ns "http://www.w3.org/2001/XMLSchema#")

(defun rdf-turtle--rdf (local)
  (rdf-model-named-node-create :value (concat rdf-turtle--rdf-ns local)))

(defun rdf-turtle--xsd (local)
  (rdf-model-named-node-create :value (concat rdf-turtle--xsd-ns local)))

;;; Parse context

(cl-defstruct (rdf-turtle--ctx (:constructor rdf-turtle--ctx-create))
  (prefixes (make-hash-table :test #'equal))
  (base nil)
  (store nil))

;;; String/IRI/local-name unescaping
;;
;; IRIREFs allow only UCHAR escapes (`\uXXXX'/`\UXXXXXXXX'); quoted
;; string literals allow both UCHAR and ECHAR (`\t\n\r\b\f\"\'\\').
;; PN_LOCAL allows `\'-escaping a fixed set of punctuation characters
;; (PN_LOCAL_ESC) to use them literally in a local name -- unlike
;; UCHAR/ECHAR, percent-encoded (`%XX') sequences in PN_LOCAL are *not*
;; decoded; they pass through verbatim as part of the resulting IRI.

(defconst rdf-turtle--uchar-rx "\\\\\\(?:u\\([0-9a-fA-F]\\{4\\}\\)\\|U\\([0-9a-fA-F]\\{8\\}\\)\\)")

(defun rdf-turtle--unescape-uchar (text)
  ;; `case-fold-search' would make the regex's lowercase `u' alternative
  ;; also match an uppercase `U' -- so a `\Uxxxxxxxx' (8-hex) escape
  ;; would be misparsed as `\u' (4-hex) plus four leftover literal
  ;; characters, silently corrupting the decoded codepoint.
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     rdf-turtle--uchar-rx
     (lambda (m)
       (save-match-data
         (string-match rdf-turtle--uchar-rx m)
         (string (string-to-number (or (match-string 1 m) (match-string 2 m)) 16))))
     text t t)))

(defconst rdf-turtle--echar-or-uchar-rx
  "\\\\\\(?:u\\([0-9a-fA-F]\\{4\\}\\)\\|U\\([0-9a-fA-F]\\{8\\}\\)\\|\\(.\\)\\)")

(defun rdf-turtle--unescape-string (text)
  ;; See `rdf-turtle--unescape-uchar' on why `case-fold-search' must be
  ;; nil here too.
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     rdf-turtle--echar-or-uchar-rx
     (lambda (m)
       (save-match-data
         (string-match rdf-turtle--echar-or-uchar-rx m)
         (cond
          ((match-string 1 m) (string (string-to-number (match-string 1 m) 16)))
          ((match-string 2 m) (string (string-to-number (match-string 2 m) 16)))
          (t (pcase (aref (match-string 3 m) 0)
               (?t "\t") (?n "\n") (?r "\r") (?b "\b") (?f "\f")
               (?\" "\"") (?\' "'") (?\\ "\\")
               (c (string c)))))))
     text t t)))

(defun rdf-turtle--unescape-pn-local (text)
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" text t))

(defun rdf-turtle--strip-quotes (text)
  "Strip a `string' node's surrounding quotes (1 or 3 of `\"'/`\\='')."
  (let ((long (or (string-prefix-p "\"\"\"" text) (string-prefix-p "'''" text))))
    (substring text (if long 3 1) (- (length text) (if long 3 1)))))

;;; Tree-sitter helpers

(defun rdf-turtle--child-of-type (node type)
  "First named child of NODE whose type is TYPE, or nil."
  (let ((n (treesit-node-child-count node t)) (i 0) (found nil))
    (while (and (< i n) (not found))
      (let ((c (treesit-node-child node i t)))
        (when (equal (treesit-node-type c) type) (setq found c)))
      (setq i (1+ i)))
    found))

(defun rdf-turtle--children-of-types (node types)
  "All named children of NODE whose type is in TYPES, in order."
  (let ((n (treesit-node-child-count node t)) (i 0) (acc nil))
    (while (< i n)
      (let ((c (treesit-node-child node i t)))
        (when (member (treesit-node-type c) types) (push c acc)))
      (setq i (1+ i)))
    (nreverse acc)))

;;; IRI/prefix resolution

(defun rdf-turtle--resolve-iri (ctx text)
  (let ((absolute (rdf-turtle--unescape-uchar text)))
    (if (rdf-turtle--ctx-base ctx)
        (url-expand-file-name absolute (rdf-turtle--ctx-base ctx))
      absolute)))

(defun rdf-turtle--iri-reference-term (ctx node)
  (let* ((text (treesit-node-text node t))
         (inner (substring text 1 (1- (length text)))))
    (rdf-model-named-node-create :value (rdf-turtle--resolve-iri ctx inner))))

(defun rdf-turtle--namespace-prefix-name (namespace-node)
  (let ((pfx (rdf-turtle--child-of-type namespace-node "pn_prefix")))
    (if pfx (treesit-node-text pfx t) "")))

(defun rdf-turtle--prefixed-name-term (ctx node)
  (let* ((namespace (rdf-turtle--child-of-type node "namespace"))
         (pn-local (rdf-turtle--child-of-type node "pn_local"))
         (prefix-name (rdf-turtle--namespace-prefix-name namespace))
         (base-iri (gethash prefix-name (rdf-turtle--ctx-prefixes ctx)))
         (local (if pn-local (rdf-turtle--unescape-pn-local (treesit-node-text pn-local t)) "")))
    (unless base-iri
      (error "rdf-turtle: undeclared prefix `%s:'" prefix-name))
    (rdf-model-named-node-create :value (concat base-iri local))))

;;; Literals

(defun rdf-turtle--rdf-literal-term (node)
  (let* ((value-node (treesit-node-child-by-field-name node "value"))
         (lexical (rdf-turtle--unescape-string (rdf-turtle--strip-quotes (treesit-node-text value-node t))))
         (lang-node (rdf-turtle--child-of-type node "lang_tag"))
         (datatype-node (rdf-turtle--child-of-type node "iri_reference")))
    (unless datatype-node
      (setq datatype-node (rdf-turtle--child-of-type node "prefixed_name")))
    (cond
     (lang-node
      (rdf-model-literal-create
       :value lexical
       :language (substring (treesit-node-text lang-node t) 1) ; drop leading "@"
       :datatype (rdf-turtle--rdf "langString")))
     (datatype-node
      ;; datatype-node here is a bare iri_reference/prefixed_name (not yet
      ;; resolved against prefixes/base) -- caller resolves via rdf-turtle--term.
      (rdf-model-literal-create :value lexical :language nil :datatype datatype-node))
     (t
      (rdf-model-literal-create :value lexical :language nil :datatype (rdf-turtle--xsd "string"))))))

;;; Term resolution (dispatches by node type)

(defun rdf-turtle--term (ctx node)
  "Resolve NODE (any object/subject-position node) to an `rdf-model' term.
May have side effects on CTX's store (collections, blank-node property
lists mint fresh blank nodes and emit quads about them)."
  (pcase (treesit-node-type node)
    ("iri_reference" (rdf-turtle--iri-reference-term ctx node))
    ("prefixed_name" (rdf-turtle--prefixed-name-term ctx node))
    ("blank_node_label"
     (rdf-model-blank-node-create :value (substring (treesit-node-text node t) 2)))
    ("anon" (rdf-model-blank-node-create :value (symbol-name (cl-gensym "anon"))))
    ("boolean_literal"
     (rdf-model-literal-create :value (treesit-node-text node t) :datatype (rdf-turtle--xsd "boolean")))
    ("integer"
     (rdf-model-literal-create :value (treesit-node-text node t) :datatype (rdf-turtle--xsd "integer")))
    ("decimal"
     (rdf-model-literal-create :value (treesit-node-text node t) :datatype (rdf-turtle--xsd "decimal")))
    ("double"
     (rdf-model-literal-create :value (treesit-node-text node t) :datatype (rdf-turtle--xsd "double")))
    ("rdf_literal"
     ;; If :datatype is still a raw tree-sitter node (set by
     ;; rdf-turtle--rdf-literal-term when an explicit `^^' datatype was
     ;; present), resolve it now via the ordinary term resolver, so
     ;; prefix/base lookups apply.
     (let* ((lit (rdf-turtle--rdf-literal-term node))
            (dt (rdf-model-literal-datatype lit)))
       (when (treesit-node-p dt)
         (setf (rdf-model-literal-datatype lit) (rdf-turtle--term ctx dt)))
       lit))
    ("collection" (rdf-turtle--collection-term ctx node))
    ("blank_node_property_list" (rdf-turtle--blank-node-property-list-term ctx node))
    (other (error "rdf-turtle: unexpected term node type `%s'" other))))

(defun rdf-turtle--collection-term (ctx node)
  (let* ((object-collection (rdf-turtle--child-of-type node "object_collection"))
         (items (if object-collection
                    (rdf-turtle--children-of-types
                     object-collection
                     '("anon" "blank_node_label" "blank_node_property_list" "boolean_literal"
                       "collection" "decimal" "double" "integer" "iri_reference"
                       "prefixed_name" "rdf_literal"))
                  nil)))
    (if (null items)
        (rdf-turtle--rdf "nil")
      (let ((store (rdf-turtle--ctx-store ctx))
            (head nil) (prev nil))
        (dolist (item-node items)
          (let* ((item-term (rdf-turtle--term ctx item-node))
                 (cell (rdf-model-blank-node-create :value (symbol-name (cl-gensym "list")))))
            (unless head (setq head cell))
            (rdf-model-dataset-add
             store (rdf-model-quad-create :subject cell :predicate (rdf-turtle--rdf "first") :object item-term))
            (when prev
              (rdf-model-dataset-add
               store (rdf-model-quad-create :subject prev :predicate (rdf-turtle--rdf "rest") :object cell)))
            (setq prev cell)))
        (rdf-model-dataset-add
         store (rdf-model-quad-create :subject prev :predicate (rdf-turtle--rdf "rest") :object (rdf-turtle--rdf "nil")))
        head))))

(defun rdf-turtle--blank-node-property-list-term (ctx node)
  (let ((bnode (rdf-model-blank-node-create :value (symbol-name (cl-gensym "anon"))))
        (property-list (rdf-turtle--child-of-type node "property_list")))
    (when property-list
      (rdf-turtle--process-property-list ctx bnode property-list))
    bnode))

;;; Statement processing

(defun rdf-turtle--predicate-term (ctx node)
  (if (= (treesit-node-child-count node t) 0)
      (rdf-turtle--rdf "type") ; bare `a'
    (rdf-turtle--term ctx (treesit-node-child node 0 t))))

(defun rdf-turtle--process-property-list (ctx subject property-list-node)
  (dolist (property (rdf-turtle--children-of-types property-list-node '("property")))
    (let* ((predicate-node (rdf-turtle--child-of-type property "predicate"))
           (predicate (rdf-turtle--predicate-term ctx predicate-node))
           (object-list-node (rdf-turtle--child-of-type property "object_list"))
           (object-nodes (rdf-turtle--children-of-types
                           object-list-node
                           '("anon" "blank_node_label" "blank_node_property_list" "boolean_literal"
                             "collection" "decimal" "double" "integer" "iri_reference"
                             "prefixed_name" "rdf_literal"))))
      (dolist (object-node object-nodes)
        (rdf-model-dataset-add
         (rdf-turtle--ctx-store ctx)
         (rdf-model-quad-create :subject subject :predicate predicate
                                 :object (rdf-turtle--term ctx object-node)))))))

(defun rdf-turtle--process-triples (ctx node)
  (let* ((subject-node (rdf-turtle--child-of-type node "subject"))
         (bnpl-node (rdf-turtle--child-of-type node "blank_node_property_list"))
         (property-list-node (rdf-turtle--child-of-type node "property_list"))
         (subject (if subject-node
                      (rdf-turtle--term ctx (treesit-node-child subject-node 0 t))
                    (rdf-turtle--blank-node-property-list-term ctx bnpl-node))))
    ;; Exactly one of subject-node/bnpl-node is present per the grammar;
    ;; either way, a top-level property_list here is this triples
    ;; statement's own predicateObjectList about SUBJECT -- for the
    ;; bnpl case this is the *additional* predicateObjectList after
    ;; `[...]' (its own internal properties were already processed
    ;; inside rdf-turtle--blank-node-property-list-term).
    (when property-list-node
      (rdf-turtle--process-property-list ctx subject property-list-node))))

(defun rdf-turtle--process-directive (ctx node)
  (let ((kind (treesit-node-type (treesit-node-child node 0 t))))
    (pcase kind
      ((or "prefix_id" "sparql_prefix")
       (let* ((directive (treesit-node-child node 0 t))
              (namespace (rdf-turtle--child-of-type directive "namespace"))
              (iri-node (rdf-turtle--child-of-type directive "iri_reference"))
              (prefix-name (rdf-turtle--namespace-prefix-name namespace))
              (iri-text (treesit-node-text iri-node t)))
         (puthash prefix-name
                  (rdf-turtle--resolve-iri ctx (substring iri-text 1 (1- (length iri-text))))
                  (rdf-turtle--ctx-prefixes ctx))))
      ((or "base" "sparql_base")
       (let* ((directive (treesit-node-child node 0 t))
              (iri-node (rdf-turtle--child-of-type directive "iri_reference"))
              (iri-text (treesit-node-text iri-node t)))
         (setf (rdf-turtle--ctx-base ctx)
               (rdf-turtle--resolve-iri ctx (substring iri-text 1 (1- (length iri-text))))))))))

(defun rdf-turtle--walk-document (ctx root)
  (dolist (statement (rdf-turtle--children-of-types root '("statement")))
    (let ((child (treesit-node-child statement 0 t)))
      (pcase (treesit-node-type child)
        ("directive" (rdf-turtle--process-directive ctx child))
        ("triples" (rdf-turtle--process-triples ctx child))))))

;;; Public API

(defun rdf-turtle-parse-buffer (&optional store)
  "Parse the current buffer's Turtle content into STORE (a new `rdf-store'
if nil) and return it."
  (let ((ctx (rdf-turtle--ctx-create :store (or store (rdf-store-create)))))
    (treesit-parser-create 'turtle)
    (rdf-turtle--walk-document ctx (treesit-buffer-root-node))
    (rdf-turtle--ctx-store ctx)))

(defun rdf-turtle-parse-string (text &optional store)
  "Like `rdf-turtle-parse-buffer', for TEXT (a string) rather than a buffer."
  (with-temp-buffer
    (insert text)
    (rdf-turtle-parse-buffer store)))

(defun rdf-turtle-parse-file (path &optional store)
  "Like `rdf-turtle-parse-buffer', for the file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (rdf-turtle-parse-buffer store)))

(provide 'rdf-turtle)

;;; rdf-turtle.el ends here
