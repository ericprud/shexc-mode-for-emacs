;;; rdf-core.el --- Shared infrastructure for tree-sitter RDF major modes  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Holds the pieces of `shexc-ts-mode' that turned out to have nothing
;; ShExC-specific about them once written down -- factored out for reuse
;; by `turtle-ts-mode' (and any future tree-sitter RDF-family mode), so
;; that what *is* genuinely shared (vocabulary-prefix data and lookup, an
;; on-demand tree-sitter grammar installer, a couple of small
;; tree-sitter-node utilities) is maintained once.  This file has no
;; dependency on any particular tree-sitter grammar's node-type names --
;; every function here that touches a `treesit-node' takes the
;; grammar-specific bits (a list of node types, a node to inspect, ...) as
;; plain arguments from the caller, rather than assuming any of its own.
;;
;; What's deliberately *not* here: font-lock rules, indentation rules, and
;; anything else that has to know a specific grammar's node-type names
;; throughout its body rather than just at its call sites -- those stay
;; in each mode's own file, even where the two modes' versions end up
;; looking similar in shape.

;;; Code:

(require 'seq)
(require 'xref)

(declare-function treesit-parent-until "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-install-language-grammar "treesit.c")
(declare-function treesit-language-available-p "treesit.c")

;;; On-demand tree-sitter grammar install
;;
;; A caller registers its own grammar in `treesit-language-source-alist'
;; (that's a single top-level `add-to-list' form naming the language
;; symbol, repo URL, and pinned commit -- see e.g. `shexc-ts-mode.el's,
;; right above its own `shexc-ts-mode-install-grammar') and then defines
;; its own autoloaded, interactive `install-grammar' command as a thin
;; wrapper around this function, which holds the actual (grammar-agnostic)
;; build/diagnose logic.

(defun rdf-core-install-grammar (language human-name abi mode file-regexp)
  "Download and compile the tree-sitter grammar for LANGUAGE.

LANGUAGE must already have an entry in `treesit-language-source-alist'
\(the caller's own top-level `add-to-list' form -- this function does
not register one).  HUMAN-NAME (e.g. \"tree-sitter-turtle\") and ABI
\(the grammar's pinned `LANGUAGE_VERSION', an integer) are used only to
word diagnostics.  On success, FILE-REGEXP is associated with MODE in
`auto-mode-alist' for the current session.

Installs into the \"tree-sitter\" subdirectory of
`user-emacs-directory' (one of the places Emacs searches for grammars
by default).  Requires `git', a C compiler (`cc', `gcc', or `c99'),
and a linker on `exec-path'; if the machine running Emacs has no C
compiler, build the grammar on another machine and copy the resulting
shared library (`.so'/`.dylib'/`.dll') into that directory instead."
  (unless (executable-find "git")
    (user-error "Cannot install grammar: `git' not found on `exec-path'"))
  (unless (seq-find #'executable-find '("cc" "gcc" "c99"))
    (user-error "Cannot install grammar: no C compiler (cc, gcc, or c99) \
found on `exec-path'.  Install one, or copy a pre-built shared library \
for %s into %s"
                human-name (locate-user-emacs-file "tree-sitter")))
  (treesit-install-language-grammar language)
  (pcase (treesit-language-available-p language t)
    (`(nil version-mismatch ,ver)
     (user-error "%s: grammar ABI version %s is not supported by this \
Emacs %s (this build requires ABI %s).  If a stale shared library \
remains from a previous install, delete it from %s and try again"
                 human-name ver emacs-version abi
                 (locate-user-emacs-file "tree-sitter")))
    (`(nil . ,_)
     (user-error "%s: grammar build failed -- check the *Warnings* \
buffer for details, or copy a pre-built shared library into %s"
                 human-name (locate-user-emacs-file "tree-sitter")))
    (_
     (add-to-list 'auto-mode-alist (cons file-regexp mode))
     (message "%s: grammar installed; matching files now use `%s'"
               human-name mode))))

;;; Prefix maps
;;
;; A "prefix map" associates short prefixes (as in Turtle/ShExC's `PREFIX
;; ex: <...>'/`@prefix ex: <...> .') with the IRIs they conventionally
;; abbreviate, along with metadata on where that association is
;; authoritatively published.  The two maps below (RDFa, Wikidata) are
;; vocabulary data with nothing ShExC- or Turtle-specific about them; a
;; consuming mode supplies its own `defcustom's naming which map(s) are
;; active (so that `:safe'/`:type'/buffer-local-via-`.dir-locals.el' all
;; keep working exactly as before) and calls the lookup functions below
;; with those as explicit arguments.

(defconst rdf-core-prefix-map-rdfa
  '(:source "https://www.w3.org/2011/rdfa-context/rdfa-1.1"
    :description
    "W3C RDFa Core 1.1 Initial Context: the default vocabulary prefixes \
recognized by RDFa processors, combining the W3C's own vocabularies \
with other widely-used ones (Dublin Core, FOAF, schema.org, etc.)."
    :prefixes
    (("as" . "https://www.w3.org/ns/activitystreams#")
     ("cc" . "http://creativecommons.org/ns#")
     ("csvw" . "http://www.w3.org/ns/csvw#")
     ("ctag" . "http://commontag.org/ns#")
     ("dc" . "http://purl.org/dc/terms/")
     ("dc11" . "http://purl.org/dc/elements/1.1/")
     ("dcat" . "http://www.w3.org/ns/dcat#")
     ("dcterms" . "http://purl.org/dc/terms/")
     ("dqv" . "http://www.w3.org/ns/dqv#")
     ("duv" . "https://www.w3.org/ns/duv#")
     ("foaf" . "http://xmlns.com/foaf/0.1/")
     ("gr" . "http://purl.org/goodrelations/v1#")
     ("grddl" . "http://www.w3.org/2003/g/data-view#")
     ("ical" . "http://www.w3.org/2002/12/cal/icaltzd#")
     ("jsonld" . "http://www.w3.org/ns/json-ld#")
     ("ldp" . "http://www.w3.org/ns/ldp#")
     ("ma" . "http://www.w3.org/ns/ma-ont#")
     ("oa" . "http://www.w3.org/ns/oa#")
     ("odrl" . "http://www.w3.org/ns/odrl/2/")
     ("og" . "http://ogp.me/ns#")
     ("org" . "http://www.w3.org/ns/org#")
     ("owl" . "http://www.w3.org/2002/07/owl#")
     ("prov" . "http://www.w3.org/ns/prov#")
     ("qb" . "http://purl.org/linked-data/cube#")
     ("rdf" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
     ("rdfa" . "http://www.w3.org/ns/rdfa#")
     ("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")
     ("rev" . "http://purl.org/stuff/rev#")
     ("rif" . "http://www.w3.org/2007/rif#")
     ("rr" . "http://www.w3.org/ns/r2rml#")
     ("schema" . "http://schema.org/")
     ("sd" . "http://www.w3.org/ns/sparql-service-description#")
     ("sioc" . "http://rdfs.org/sioc/ns#")
     ("skos" . "http://www.w3.org/2004/02/skos/core#")
     ("skosxl" . "http://www.w3.org/2008/05/skos-xl#")
     ("sosa" . "http://www.w3.org/ns/sosa/")
     ("ssn" . "http://www.w3.org/ns/ssn/")
     ("time" . "http://www.w3.org/2006/time#")
     ("v" . "http://rdf.data-vocabulary.org/#")
     ("vcard" . "http://www.w3.org/2006/vcard/ns#")
     ("void" . "http://rdfs.org/ns/void#")
     ("wdr" . "http://www.w3.org/2007/05/powder#")
     ("wdrs" . "http://www.w3.org/2007/05/powder-s#")
     ("xhv" . "http://www.w3.org/1999/xhtml/vocab#")
     ("xml" . "http://www.w3.org/XML/1998/namespace")
     ("xsd" . "http://www.w3.org/2001/XMLSchema#")))
  "Prefix map transcribed from the W3C RDFa Core 1.1 Initial Context.
See its `:source' for the authoritative, machine-readable (JSON-LD)
list this was transcribed from.")

(defconst rdf-core-prefix-map-wikidata
  '(:source
    "https://www.mediawiki.org/wiki/Wikibase/Indexing/RDF_Dump_Format"
    :description
    "Wikidata has no single canonical prefix-list page; this combines \
the RDF-dump namespace prefixes documented at its `:source' URL with \
the PREFIX block conventionally used at the top of Wikidata \
EntitySchemas, e.g. https://www.wikidata.org/wiki/EntitySchema:E10."
    :prefixes
    (("bd" . "http://www.bigdata.com/rdf#")
     ("geo" . "http://www.opengis.net/ont/geosparql#")
     ("owl" . "http://www.w3.org/2002/07/owl#")
     ("p" . "http://www.wikidata.org/prop/")
     ("pq" . "http://www.wikidata.org/prop/qualifier/")
     ("pqn" . "http://www.wikidata.org/prop/qualifier/value-normalized/")
     ("pqv" . "http://www.wikidata.org/prop/qualifier/value/")
     ("pr" . "http://www.wikidata.org/prop/reference/")
     ("prn" . "http://www.wikidata.org/prop/reference/value-normalized/")
     ("prov" . "http://www.w3.org/ns/prov#")
     ("prv" . "http://www.wikidata.org/prop/reference/value/")
     ("ps" . "http://www.wikidata.org/prop/statement/")
     ("psn" . "http://www.wikidata.org/prop/statement/value-normalized/")
     ("psv" . "http://www.wikidata.org/prop/statement/value/")
     ("rdf" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
     ("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")
     ("schema" . "http://schema.org/")
     ("skos" . "http://www.w3.org/2004/02/skos/core#")
     ("wd" . "http://www.wikidata.org/entity/")
     ("wdata" . "https://www.wikidata.org/wiki/Special:EntityData/")
     ("wdno" . "http://www.wikidata.org/prop/novalue/")
     ("wdref" . "http://www.wikidata.org/reference/")
     ("wds" . "http://www.wikidata.org/entity/statement/")
     ("wdt" . "http://www.wikidata.org/prop/direct/")
     ("wdtn" . "http://www.wikidata.org/prop/direct-normalized/")
     ("wdv" . "http://www.wikidata.org/value/")
     ("wikibase" . "http://wikiba.se/ontology#")
     ("xsd" . "http://www.w3.org/2001/XMLSchema#")))
  "Prefix map for Wikidata's RDF/EntitySchema namespaces.
See `:description' for the (two) sources this was compiled from.")

(defun rdf-core-prefix-map-names (active)
  "Normalize ACTIVE (a single map-name string, or already a list of
them) to a list -- ACTIVE is whatever shape a consuming mode's own
\"which map(s) are active\" `defcustom' holds."
  (if (listp active) active (list active)))

(defun rdf-core-prefix-map-names-string (active)
  "`rdf-core-prefix-map-names' on ACTIVE, joined for display."
  (mapconcat #'identity (rdf-core-prefix-map-names active) ", "))

(defun rdf-core-prefix-map-lookup (prefix maps active)
  "Return (IRI . MAP-NAME) for PREFIX within MAPS, an alist of
\(NAME . PLIST) -- see `rdf-core-prefix-map-rdfa' for PLIST's shape
\(`:source'/`:description'/`:prefixes') -- trying ACTIVE's named maps
\(see `rdf-core-prefix-map-names') in order, first match wins.
Return nil if PREFIX is empty or no active map has an entry for it."
  (unless (string-empty-p prefix)
    (catch 'rdf-core-prefix-map-lookup-found
      (dolist (name (rdf-core-prefix-map-names active))
        (let* ((map (cdr (assoc name maps)))
               (iri (cdr (assoc prefix (plist-get map :prefixes)))))
          (when iri
            (throw 'rdf-core-prefix-map-lookup-found (cons iri name)))))
      nil)))

;;; Generic tree-sitter node utilities

(defun rdf-core-ancestor-of-type (node types &optional include-node)
  "Return the closest ancestor of NODE whose type is a member of TYPES.
If INCLUDE-NODE is non-nil, NODE itself is considered first."
  (treesit-parent-until
   node
   (lambda (n) (member (treesit-node-type n) types))
   include-node))

;;; Xref helpers
;;
;; Building blocks for an `xref-backend-definitions'/`-references'
;; implementation: given the tree-sitter nodes a grammar-specific query
;; already found, these turn them into `xref-item's.  None of this
;; touches node *types* -- the query that produced NODE/NODES is the
;; caller's own grammar-specific code.

(defun rdf-core-node-line-summary (node)
  "Return a trimmed one-line summary of the source line containing NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (string-trim (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))

(defun rdf-core-xref-make (node)
  "Build an `xref-item' pointing at NODE."
  (xref-make (rdf-core-node-line-summary node)
             (xref-make-buffer-location (current-buffer) (treesit-node-start node))))

(defun rdf-core-matching-xrefs (nodes identifier)
  "Build `xref-item's for the nodes in NODES whose text equals IDENTIFIER."
  (delq nil
        (mapcar (lambda (node)
                  (when (string= (treesit-node-text node t) identifier)
                    (rdf-core-xref-make node)))
                nodes)))

;;; Syntax-propertize helper
;;
;; Both Turtle and ShExC reuse `#' as a line-comment starter *and* allow
;; it to appear literally inside a `<...>' IRIREF (a fragment separator,
;; e.g. `<http://example.org/onto#Class>') -- a plain syntax table can't
;; tell those two roles apart, which otherwise makes syntax-based
;; scanning (`forward-list'/`up-list'/...) believe a bracket later on
;; the same line is "inside a comment".

(defun rdf-core-neutralize-comment-char-in-iri (char beg end)
  "Give CHAR a neutral `syntax-table' property wherever it falls inside
a `<...>' IRIREF between BEG and END, so syntax-based scanning doesn't
mistake it for a comment-starter there.  Intended as (part of) a
mode's `syntax-propertize-function', with CHAR bound to whichever
character that mode's syntax table marks `<' b' (e.g. `?#').

CHAR is considered to be inside an IRIREF (and therefore not a comment
starter) when scanning backward to the beginning of the line finds an
unmatched `<' before any `>'."
  (goto-char beg)
  (while (re-search-forward (regexp-quote (char-to-string char)) end t)
    (let ((pos (match-beginning 0)))
      (when (save-excursion
              (let ((bol (line-beginning-position)))
                (and (re-search-backward "<" bol t)
                     (not (re-search-forward ">" pos t)))))
        (with-silent-modifications
          (put-text-property pos (1+ pos)
                              'syntax-table (string-to-syntax "_")))))))

(provide 'rdf-core)

;;; rdf-core.el ends here
