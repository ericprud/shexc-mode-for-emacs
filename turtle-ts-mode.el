;;; turtle-ts-mode.el --- Tree-sitter major mode for Turtle  -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (rdf-core "0.1.0"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A tree-sitter based major mode for editing Turtle documents, built
;; on the grammar at
;; https://github.com/GordianDziwis/tree-sitter-turtle -- and on the
;; shared infrastructure in `rdf-core.el', first factored out of the
;; sibling `shexc-ts-mode' (a major mode for ShEx Compact Syntax, which
;; adds shape validation on top of the Turtle/RDF data model this mode
;; edits -- see https://github.com/ericprud/shexc-mode-for-emacs if
;; that's of interest).
;;
;; It provides:
;; - syntax highlighting (`treesit-font-lock-rules')
;; - structure-aware indentation (`treesit-simple-indent-rules')
;; - line commenting (`# ...', M-; via `comment-dwim')
;; - folding of `[ ... ]'/`( ... )' via `hideshow' (`C-c C-f')
;; - inserting a `PREFIX'/`@prefix' declaration for the prefix at point,
;;   looked up against a configurable vocabulary-prefix map (`C-c C-p',
;;   shared data/lookup with `shexc-ts-mode' via `rdf-core.el')
;;
;; For documentation on Turtle, see:
;; https://www.w3.org/TR/turtle/

;;; Setup:
;;
;; (require 'turtle-ts-mode)
;;
;; ;; one-time: download and compile the `tree-sitter-turtle' grammar
;; ;; (requires `git', a C compiler, and a linker on `exec-path')
;; M-x turtle-ts-mode-install-grammar
;;
;; (add-to-list 'auto-mode-alist '("\\.ttl\\'" . turtle-ts-mode))

;;; Code:

(require 'treesit)
(require 'hideshow)
(require 'pcase)
(require 'transient)
(require 'rdf-core)

;; defined by `define-derived-mode' below; forward-declared so the
;; feature menu (which closes over it) can refer to it.
(defvar turtle-ts-mode-map)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-at "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-query-capture "treesit.c")

;; `M-x turtle-ts-mode-install-grammar' builds `tree-sitter-turtle'; see
;; the Setup section above.  Its `treesit-language-source-alist' entry
;; (the pinned tag, and why) lives in `rdf-core.el' now, shared with any
;; other consumer that wants the grammar without this mode's editing
;; machinery -- see the comment there.

;;;###autoload
(defun turtle-ts-mode-install-grammar ()
  "Download and compile the `tree-sitter-turtle' grammar for `turtle-ts-mode'.

See `rdf-core-install-grammar' for what this requires/does; on
success, `.ttl' files are immediately associated with `turtle-ts-mode'
in the current session."
  (interactive)
  (rdf-core-install-grammar
   'turtle "tree-sitter-turtle" 13 'turtle-ts-mode "\\.ttl\\'"))

(defgroup turtle-ts nil
  "Major mode for editing Turtle documents with tree-sitter."
  :group 'languages)

(defcustom turtle-ts-mode-indent-offset 2
  "Number of columns for each indentation step in `turtle-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'turtle-ts)

;;; Prefix maps
;;
;; See `rdf-core.el's "Prefix maps" section for the shared data/lookup
;; this delegates to; `turtle-ts-mode-prefix-maps'/`-prefix-map' mirror
;; `shexc-ts-mode-prefix-maps'/`-prefix-map' exactly (down to the default
;; value), so the same `.dir-locals.el' project-map convention works in
;; either mode.

(defcustom turtle-ts-mode-prefix-maps
  `(("rdfa" . ,rdf-core-prefix-map-rdfa)
    ("wikidata" . ,rdf-core-prefix-map-wikidata))
  "Alist of named prefix maps, for `turtle-ts-mode-prefix-map' to select.
See `shexc-ts-mode-prefix-maps' (shexc-ts-mode.el) for PLIST's shape
and an example of adding your own map via `.dir-locals.el'."
  :type 'sexp
  :group 'turtle-ts)

(defcustom turtle-ts-mode-prefix-map "rdfa"
  "Name (or, for several, a list of names) of the active entry/entries
in `turtle-ts-mode-prefix-maps'.  See `shexc-ts-mode-prefix-map' for
the full explanation of multi-map lookup order; this is its
`turtle-ts-mode' counterpart."
  :type '(choice string (repeat string))
  :safe (lambda (val) (or (stringp val) (and (listp val) (seq-every-p #'stringp val))))
  :group 'turtle-ts)

(defun turtle-ts-mode--prefix-map-names ()
  "`turtle-ts-mode-prefix-map' as a list of names."
  (rdf-core-prefix-map-names turtle-ts-mode-prefix-map))

(defun turtle-ts-mode--prefix-map-names-string ()
  "`turtle-ts-mode--prefix-map-names', joined for display in a message."
  (rdf-core-prefix-map-names-string turtle-ts-mode-prefix-map))

(defun turtle-ts-mode--prefix-map-lookup (prefix)
  "Return (IRI . MAP-NAME) for PREFIX -- see `rdf-core-prefix-map-lookup'."
  (rdf-core-prefix-map-lookup
   prefix turtle-ts-mode-prefix-maps turtle-ts-mode-prefix-map))

(defun turtle-ts-mode--prefix-name (text)
  "Return the part of TEXT (a `prefixed_name'/`namespace' node's text)
before its `:'.  E.g. \"ex\" for both `ex:p1' and `ex:', and \"\" for
`:p1'."
  (substring text 0 (string-search ":" text)))

(defun turtle-ts-mode--prefixed-name-at (pos)
  "Return the `prefixed_name' node at POS, or nil if none."
  (rdf-core-ancestor-of-type (treesit-node-at pos) '("prefixed_name") t))

(defun turtle-ts-mode--declared-prefixes ()
  "Return the list of prefix names (e.g. \"ex\") the buffer already
declares via `@prefix ex: <...> .'/`PREFIX ex: <...>'."
  (mapcar (lambda (n) (turtle-ts-mode--prefix-name (treesit-node-text n t)))
          (treesit-query-capture
           (treesit-buffer-root-node)
           '([(prefix_id (namespace) @name) (sparql_prefix (namespace) @name)])
           nil nil t)))

(defun turtle-ts-mode--insert-prefix-decl (prefix iri)
  "Insert the line `@prefix PREFIX: <IRI> .' into the current buffer.
It is placed immediately after the buffer's last directive
\(`prefix_id'/`base'/`sparql_prefix'/`sparql_base'), or at `point-min'
if the buffer has none of those."
  (let ((directives (treesit-query-capture
                      (treesit-buffer-root-node)
                      '([(prefix_id) (base) (sparql_prefix) (sparql_base)] @d)
                      nil nil t)))
    (if directives
        (progn
          (goto-char (apply #'max (mapcar #'treesit-node-end directives)))
          (end-of-line)
          (if (eobp)
              (insert "\n")
            (forward-char 1)))
      (goto-char (point-min)))
    (insert (format "@prefix %s: <%s> .\n" prefix iri))))

;;;###autoload
(defun turtle-ts-mode-insert-prefix ()
  "Insert a `@prefix' declaration for the prefixed name at point.

Looks up the prefix of the `prefixed_name' at point (e.g. \"ex\" in
`ex:Foo') in the active `turtle-ts-mode-prefix-map' and, if found
there, inserts `@prefix ex: <IRI> .' via
`turtle-ts-mode--insert-prefix-decl'.  A no-op (just a `message', not
an error) if the buffer already declares that prefix.

Signals a `user-error' if point is not on a prefixed name, or its
prefix has no entry in any of the active maps."
  (interactive)
  (let ((node (turtle-ts-mode--prefixed-name-at (point))))
    (unless node
      (user-error "No prefixed name at point"))
    (let ((prefix (turtle-ts-mode--prefix-name (treesit-node-text node t))))
      (if (member prefix (turtle-ts-mode--declared-prefixes))
          (message "Prefix `%s:' is already declared" prefix)
        (let ((found (turtle-ts-mode--prefix-map-lookup prefix)))
          (unless found
            (user-error "Prefix `%s:' is not in any of the active prefix maps (%s)"
                        prefix (turtle-ts-mode--prefix-map-names-string)))
          (let ((iri (car found)) (map-name (cdr found)))
            (save-excursion (turtle-ts-mode--insert-prefix-decl prefix iri))
            (message "Inserted `@prefix %s: <%s>' (from the %s map)" prefix iri map-name)))))))

;;;###autoload
(defun turtle-ts-mode-set-prefix-map (names)
  "Set the buffer-local active prefix map(s) to NAMES, for this buffer
only -- the `turtle-ts-mode' counterpart of `shexc-ts-mode-set-prefix-map'
\(see its docstring for the full explanation; identical mechanism, just
operating on `turtle-ts-mode-prefix-map'/`-prefix-maps' instead)."
  (interactive
   (list (completing-read-multiple
          "Prefix map(s) (first match wins): "
          (mapcar #'car turtle-ts-mode-prefix-maps)
          nil t (turtle-ts-mode--prefix-map-names-string))))
  (when (stringp names) (setq names (list names)))
  (setq-local turtle-ts-mode-prefix-map (if (cdr names) names (car names)))
  (message "Active prefix map%s: %s"
           (if (cdr names) "s" "") (mapconcat #'identity names ", ")))

;;; Switching `@prefix'/`@base' <-> `PREFIX'/`BASE' syntax
;;
;; Turtle and SPARQL spell the same prefix/base directive two ways --
;; `@prefix ex: <iri> .'/`@base <iri> .' (Turtle's own, dot-terminated)
;; vs. `PREFIX ex: <iri>'/`BASE <iri>' (borrowed from SPARQL, no dot) --
;; both accepted by the grammar as distinct node types
;; (`prefix_id'/`base' vs. `sparql_prefix'/`sparql_base'). This toggles
;; the one at point between the two, preserving the namespace prefix
;; and IRI text exactly (only the keyword/terminator changes).

(defun turtle-ts-mode--directive-at (pos)
  "Return the `prefix_id'/`sparql_prefix'/`base'/`sparql_base' node at
or around POS, or nil."
  (rdf-core-ancestor-of-type
   (treesit-node-at pos) '("prefix_id" "sparql_prefix" "base" "sparql_base") t))

(defun turtle-ts-mode--directive-replacement (node)
  "Replacement text for NODE (a directive node -- see
`turtle-ts-mode--directive-at'), toggled to the opposite of its
current `@prefix'/`@base' vs. `PREFIX'/`BASE' syntax."
  (pcase (treesit-node-type node)
    ("prefix_id"
     (format "PREFIX %s %s"
             (treesit-node-text (rdf-core-child-of-type node "namespace") t)
             (treesit-node-text (rdf-core-child-of-type node "iri_reference") t)))
    ("sparql_prefix"
     (format "@prefix %s %s ."
             (treesit-node-text (rdf-core-child-of-type node "namespace") t)
             (treesit-node-text (rdf-core-child-of-type node "iri_reference") t)))
    ("base"
     (format "BASE %s" (treesit-node-text (rdf-core-child-of-type node "iri_reference") t)))
    ("sparql_base"
     (format "@base %s ." (treesit-node-text (rdf-core-child-of-type node "iri_reference") t)))))

;;;###autoload
(defun turtle-ts-mode-toggle-prefix-style ()
  "Toggle the `@prefix'/`@base'/`PREFIX'/`BASE' directive at point
between Turtle's own dot-terminated syntax and SPARQL's (no dot).
Operates on the directive nearest point; the namespace prefix name
and IRI text are preserved exactly, only the keyword/terminator
changes."
  (interactive)
  (let ((node (turtle-ts-mode--directive-at (point))))
    (unless node
      (user-error "No `@prefix'/`@base'/`PREFIX'/`BASE' directive at point"))
    (let ((beg (treesit-node-start node))
          (end (treesit-node-end node))
          (replacement (turtle-ts-mode--directive-replacement node)))
      (goto-char beg)
      (delete-region beg end)
      (insert replacement))))

;;; Folding `[ ... ]'/`( ... )' via hideshow
;;
;; Unlike `shexc-ts-mode--shape-body-brace-at' (which has to skip past
;; `CLOSED'/`EXTRA ...' modifiers before a shape body's `{'),
;; `blank_node_property_list'/`collection' always *start* with their own
;; bracket, so the node's own start position is the fold target directly.

(defun turtle-ts-mode--fold-target-at (pos)
  "Return the position of the `['/`(' of the blank-node-property-list/
collection at or around POS that `turtle-ts-mode-toggle-fold' should
act on, or nil."
  (when-let* ((node (rdf-core-ancestor-of-type
                      (treesit-node-at pos)
                      '("blank_node_property_list" "collection")
                      t)))
    (treesit-node-start node)))

;;;###autoload
(defun turtle-ts-mode-toggle-fold ()
  "Fold or unfold the `[ ... ]'/`( ... )' at or around point.

Enables `hs-minor-mode' if it is not already on, then toggles hiding
of the nearest enclosing `blank_node_property_list'/`collection'."
  (interactive)
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if-let* ((bracket (turtle-ts-mode--fold-target-at (point))))
      (save-excursion
        (goto-char bracket)
        (hs-toggle-hiding))
    (user-error "No `[ ... ]'/`( ... )' at point")))

(add-to-list 'hs-special-modes-alist
             '(turtle-ts-mode "[[(]" "[])]" "#" nil nil))

;;; Feature menu (transient)
;;
;; Mirrors `shexc-ts-mode-menu''s structure (a `transient' prefix with
;; live-keybinding suffix descriptions, a nested submenu for the
;; prefix-map options) -- scaled down to what `turtle-ts-mode' actually
;; has: no shape-structure editing or navigation commands to show, so
;; there's only ever the one "Edit" column, not "Navigate"+"Edit".

(defun turtle-ts-mode--menu-desc (label command &optional width)
  "Like `shexc-ts-mode--menu-desc', looking COMMAND's binding up in
`turtle-ts-mode-map' instead."
  (let ((key (where-is-internal command turtle-ts-mode-map t)))
    (if key
        (format (format "%%-%ds %%s" (or width 36)) label (key-description key))
      label)))

(transient-define-prefix turtle-ts-mode-prefix-menu ()
  "Submenu of `PREFIX'-declaration commands -- the `turtle-ts-mode'
counterpart of `shexc-ts-mode-prefix-menu'."
  ["Prefix maps"
   ("p" turtle-ts-mode-insert-prefix
    :description
    (lambda ()
      (turtle-ts-mode--menu-desc
       (format "Insert `@prefix' for prefix at point (%s map)"
               (turtle-ts-mode--prefix-map-names-string))
       'turtle-ts-mode-insert-prefix)))
   ("m" turtle-ts-mode-set-prefix-map
    :description
    (lambda ()
      (turtle-ts-mode--menu-desc
       (format "Switch active prefix map (currently %s)"
               (turtle-ts-mode--prefix-map-names-string))
       'turtle-ts-mode-set-prefix-map)))
   ("q" "Done" transient-quit-all)])

(transient-define-prefix turtle-ts-mode-menu ()
  "Feature menu for `turtle-ts-mode', showing live keybindings."
  ["Edit"
   ("f" turtle-ts-mode-toggle-fold
    :description
    (lambda () (turtle-ts-mode--menu-desc
                "Fold/unfold `[ ... ]'/`( ... )'"
                'turtle-ts-mode-toggle-fold 32)))
   ("k" turtle-ts-mode-toggle-prefix-style
    :description
    (lambda () (turtle-ts-mode--menu-desc
                "`@prefix'/`@base' <-> `PREFIX'/`BASE'"
                'turtle-ts-mode-toggle-prefix-style 32)))
   ("p" turtle-ts-mode-prefix-menu
    :description
    (lambda ()
      (format "Prefix map options...           (%s)"
              (turtle-ts-mode--prefix-map-names-string))))])

;;; Syntax table

(defvar turtle-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for `turtle-ts-mode'.")

(defun turtle-ts-mode--syntax-propertize (beg end)
  "Neutralize non-comment `#' characters between BEG and END -- see
`rdf-core-neutralize-comment-char-in-iri'."
  (rdf-core-neutralize-comment-char-in-iri ?# beg end))

;;; Font-lock

(defvar turtle-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'turtle
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'turtle
   :feature 'string
   '([(string) (lang_tag)] @font-lock-string-face)

   :language 'turtle
   :feature 'keyword
   '((["@prefix" "@base" "PREFIX" "BASE"]) @font-lock-keyword-face
     (predicate "a") @font-lock-keyword-face)

   :language 'turtle
   :feature 'type
   '((rdf_literal datatype: (_) @font-lock-type-face))

   :language 'turtle
   :feature 'constant
   '([(iri_reference) (prefixed_name) (blank_node_label) (anon)
      (boolean_literal)] @font-lock-constant-face)

   :language 'turtle
   :feature 'number
   '([(integer) (decimal) (double)] @font-lock-number-face)

   :language 'turtle
   :feature 'bracket
   ;; No "{" "}" -- Turtle's grammar has no such tokens at all (not even
   ;; unused), so querying for them raises a hard `treesit-query-error'
   ;; at capture time (lazy validation -- compiling the query alone
   ;; doesn't catch it).
   '((["[" "]" "(" ")"]) @font-lock-bracket-face)

   :language 'turtle
   :feature 'delimiter
   '((["," ";" "."]) @font-lock-delimiter-face))
  "Tree-sitter font-lock settings for `turtle-ts-mode'.")

(defvar turtle-ts-mode--font-lock-feature-list
  '((comment)
    (keyword string)
    (constant number type)
    (bracket delimiter))
  "`treesit-font-lock-feature-list' for `turtle-ts-mode'.

Levels 1-3 (the default `treesit-font-lock-level') cover everything
but raw punctuation; level 4 additionally highlights brackets and
separators.")

;;; Indentation

(defvar turtle-ts-mode--indent-rules
  `((turtle
     ;; closing delimiters line up with the line that opened the block
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "}") parent-bol 0)

     ;; contents of [...] indent one step from the line that opened
     ;; the block.  NOTE: `parent-is' matches its TYPE argument as a
     ;; regexp *anywhere* in the parent's type name, so these are all
     ;; anchored with \\` and \\' -- left unanchored, "collection"
     ;; would also match the parent type `object_collection', and
     ;; "property_list" would also match `blank_node_property_list'.
     ((parent-is "\\`blank_node_property_list\\'") parent-bol turtle-ts-mode-indent-offset)

     ;; `collection''s ( ... ) items are wrapped in an
     ;; `object_collection' node with no concept of "first" vs. "rest".
     ;; The first item's start position coincides with
     ;; `object_collection''s own start, so indenting *it* actually
     ;; matches this `collection' rule (`treesit-simple-indent' climbs
     ;; from a node to its largest same-start ancestor below the root
     ;; before applying `parent-is', skipping right over
     ;; `object_collection'); later items' parent is genuinely
     ;; `object_collection', caught below, anchored on the first item
     ;; instead of recomputing from `collection' (which would compound
     ;; the offset once per item).
     ((parent-is "\\`collection\\'") parent-bol turtle-ts-mode-indent-offset)
     ((parent-is "\\`object_collection\\'") first-sibling 0)

     ;; each `;'-separated property in a property_list indents one step
     ;; from the line its property_list started on (the subject's line,
     ;; or a `['s line for a blank-node-property-list) -- all of them,
     ;; including the first, so they line up with each other
     ((parent-is "\\`property_list\\'") parent-bol turtle-ts-mode-indent-offset)

     ;; top-level statements start at column 0
     ((parent-is "\\`turtle_doc\\'") column-0 0)

     (catch-all parent-bol 0)))
  "`treesit-simple-indent-rules' for `turtle-ts-mode'.")

;;; Major mode

;;;###autoload
(define-derived-mode turtle-ts-mode prog-mode "Turtle[ts]"
  "Major mode for editing Turtle documents, powered by tree-sitter.

\\{turtle-ts-mode-map}"
  :syntax-table turtle-ts-mode--syntax-table
  (unless (treesit-ready-p 'turtle)
    (unless noninteractive
      (when (y-or-n-p "Tree-sitter grammar for `turtle' not found.  \
Install it now? (requires git and a C compiler on exec-path) ")
        (turtle-ts-mode-install-grammar)))
    (unless (treesit-ready-p 'turtle)
      (error "Tree-sitter grammar for `turtle' is not available; \
run M-x turtle-ts-mode-install-grammar")))

  (treesit-parser-create 'turtle)

  ;; disambiguate `#' as comment-starter vs. in-IRI fragment separator
  ;; (see `turtle-ts-mode--syntax-propertize')
  (setq-local syntax-propertize-function #'turtle-ts-mode--syntax-propertize)

  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")

  ;; inserting a `@prefix' declaration for the prefix at point, looked
  ;; up in the active `turtle-ts-mode-prefix-map'
  (define-key turtle-ts-mode-map (kbd "C-c C-p") #'turtle-ts-mode-insert-prefix)

  ;; toggling `@prefix'/`@base' <-> `PREFIX'/`BASE' syntax at point
  (define-key turtle-ts-mode-map (kbd "C-c C-k") #'turtle-ts-mode-toggle-prefix-style)

  ;; folding `[ ... ]'/`( ... )'
  (define-key turtle-ts-mode-map (kbd "C-c C-f") #'turtle-ts-mode-toggle-fold)
  (hs-minor-mode 1)

  ;; Magit-style feature menu
  (define-key turtle-ts-mode-map (kbd "C-c C-c") #'turtle-ts-mode-menu)

  ;; indentation
  (setq-local treesit-simple-indent-rules turtle-ts-mode--indent-rules)
  (setq-local indent-tabs-mode nil)
  (setq-local electric-indent-chars
              (append '(?\{ ?\} ?\[ ?\] ?\( ?\)) electric-indent-chars))

  ;; font-lock
  (setq-local treesit-font-lock-settings turtle-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list turtle-ts-mode--font-lock-feature-list)

  (treesit-major-mode-setup))

(provide 'turtle-ts-mode)

;;; turtle-ts-mode.el ends here
