;;; shexc-ts-mode.el --- Tree-sitter major mode for ShExC -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 3.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This mode is a tree-sitter based companion to `shexc-mode' (see
;; shexc-mode.el), built on the grammar at
;; https://github.com/ericprud/tree-sitter-shexc
;;
;; Shares its `rdf-core.el'/`turtle-ts-mode.el' infrastructure with the
;; sibling `turtle-ts-mode' (a major mode for Turtle/TriG/N-Triples).
;; Both currently ship bundled inside this package rather than as
;; independent MELPA dependencies -- they're new and not yet eligible
;; for their own MELPA submissions; `Package-Requires' will gain an
;; `rdf-core' entry once that changes. See the README for the planned
;; split.
;;
;; It provides:
;; - syntax highlighting (`treesit-font-lock-rules')
;; - structure-aware indentation (`treesit-simple-indent-rules')
;; - line/block commenting (M-; via `comment-dwim')
;; - imenu index of shape declarations
;; - "jump to shape definition" / "find references to shape" via `xref'
;;   (`M-.'/`M-,'/`M-?'), resolving `@<#Shape>', `EXTENDS @<#Shape>',
;;   `&<#Shape>' and `start = @<#Shape>' references to their
;;   `shape_expr_decl'.
;;
;; For documentation on ShExC, see:
;; https://shex.io/shex-semantics/#shexc

;;; Setup:
;;
;; (require 'shexc-ts-mode)
;;
;; ;; one-time: download and compile the `tree-sitter-shexc' grammar
;; ;; (requires `git', a C compiler, and a linker on `exec-path')
;; M-x shexc-ts-mode-install-grammar
;;
;; (add-to-list 'auto-mode-alist '("\\.shexc?\\'" . shexc-ts-mode))

;;; Code:

(require 'treesit)
(require 'xref)
(require 'cl-lib)
(require 'pcase)
(require 'hideshow)
(require 'flymake)
(require 'seq)
;; For `shexc-shexj-buffer-directive-ctx'/`-resolve-label', used by
;; `shexc-ts-mode--flymake-undefined-shapes' to compare shape references
;; by resolved IRI rather than raw source text.  Safe as an eager,
;; top-level `require' (no circularity): unlike `shexc-ts-mode-convert.el',
;; `shexc-shexj.el' has no dependency of its own on `shexc-ts-mode'.
(require 'shexc-shexj)
(require 'transient)
(require 'rdf-core)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-search-forward "treesit.c")

;; defined by `define-derived-mode' below; forward-declared so the
;; feature menu (which closes over it) can refer to it.
(defvar shexc-ts-mode-map)

;; Recipe for `M-x shexc-ts-mode-install-grammar' (and
;; `treesit-install-language-grammar' generally) to build the
;; `tree-sitter-shexc' grammar; see the Setup section in the README.
;;
;; Pinned to the `v0.2.0' tag, not a raw commit SHA:
;; `treesit-install-language-grammar' clones with `git clone ... -b
;; REVISION', and `-b' only accepts a branch/tag name -- a raw SHA
;; fails with "Remote branch ... not found in upstream origin"
;; (confirmed empirically).  `v0.2.0' targets ABI 14 (LANGUAGE_VERSION
;; 14), compatible with Emacs 29.1+ and Emacs 30+.  ABI 14 is
;; backward-compatible with Emacs 30, so no version-based switching is
;; needed: updating this pin is the only change required if a future
;; grammar version ever needs ABI 15.
(add-to-list 'treesit-language-source-alist
             '(shexc "https://github.com/ericprud/tree-sitter-shexc"
                     "v0.2.0"))

;;;###autoload
(defun shexc-ts-mode-install-grammar ()
  "Download and compile the `tree-sitter-shexc' grammar for `shexc-ts-mode'.

Clones https://github.com/ericprud/tree-sitter-shexc and compiles it,
installing the result into the \"tree-sitter\" subdirectory of
`user-emacs-directory' (one of the places Emacs searches for grammars
by default).  On success, `.shex'/`.shexc' files are immediately
associated with `shexc-ts-mode' in the current session.

Requires `git', a C compiler (`cc', `gcc', or `c99'), and a linker on
`exec-path'.  If the machine running Emacs has no C compiler, build
the grammar on another machine and copy the resulting
`libtree-sitter-shexc.so' (Linux) / `.dylib' (macOS) / `.dll'
(Windows) into the tree-sitter subdirectory of `user-emacs-directory';
see the shexc-ts-mode README for details."
  (interactive)
  (rdf-core-install-grammar
   'shexc "tree-sitter-shexc" 14 'shexc-ts-mode "\\.shexc?\\'"))

(defgroup shexc-ts nil
  "Major mode for editing ShExC documents with tree-sitter."
  :group 'languages)

(defcustom shexc-ts-mode-indent-offset 2
  "Number of columns for each indentation step in `shexc-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'shexc-ts)

;;; Prefix maps
;;
;; A "prefix map" associates short prefixes (as in `PREFIX ex: <...>')
;; with the IRIs they conventionally abbreviate, along with metadata
;; on where that association is authoritatively published.
;; `shexc-ts-mode-insert-prefix' and the "Undefined prefix" flymake
;; diagnostic (`shexc-ts-mode--flymake-undefined-prefixes') consult the
;; active map(s) (`shexc-ts-mode-prefix-map') to propose a `PREFIX'
;; declaration for an undeclared prefix.
;;
;; `shexc-ts-mode-prefix-map' may name more than one map, tried in
;; order with the first match winning -- the intended use is a small
;; project-specific map (added to `shexc-ts-mode-prefix-maps', e.g. via
;; a `.dir-locals.el', see that variable's docstring) listed *before* a
;; big general one like "rdfa", so the project map's own entries (and
;; any prefix the general map simply doesn't have, e.g. a bespoke
;; `shex:') take precedence without needing to fork or edit "rdfa"
;; itself.

(defcustom shexc-ts-mode-prefix-maps
  `(("rdfa" . ,rdf-core-prefix-map-rdfa)
    ("wikidata" . ,rdf-core-prefix-map-wikidata))
  "Alist of named prefix maps, for `shexc-ts-mode-prefix-map' to select.

Each element is (NAME . PLIST), where NAME is a string (matched
against `shexc-ts-mode-prefix-map') and PLIST has the keys:

  `:source'      URL where the map is authoritatively published, or nil.
  `:description' Free-form text on the map's provenance/contents.
  `:prefixes'    Alist of (PREFIX . IRI) strings, e.g.
                 (\"rdf\" . \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\").

Add your own by appending more (NAME . PLIST) elements, e.g. in your
init file:

  (with-eval-after-load \\='shexc-ts-mode
    (push \\='(\"my-project\"
            :source \"https://example.org/my-prefixes\"
            :description \"Prefixes used across my-project's schemas.\"
            :prefixes ((\"ex\" . \"http://example.org/ns#\")))
          shexc-ts-mode-prefix-maps))"
  :type 'sexp
  :group 'shexc-ts)

(defcustom shexc-ts-mode-prefix-map "rdfa"
  "Name (or, for several, a list of names) of the active entry/entries
in `shexc-ts-mode-prefix-maps'.

`shexc-ts-mode-insert-prefix' and the \"Undefined prefix\" flymake
diagnostic (`shexc-ts-mode--flymake-undefined-prefixes') look up
undeclared prefixes here.  When this is a list, e.g. `(\"my-project\"
\"rdfa\")', each named map is tried in order and the first one with a
matching prefix wins -- so a small project-specific map listed first
can supply (or override) entries the general map after it either
lacks or would otherwise give a different answer for.

Set this as a file- or directory-local variable to use a different map
\(or combination\) in specific buffers/projects, e.g. \"wikidata\" when
editing Wikidata EntitySchemas, or `(\"my-project\" \"rdfa\")' for a
project with its own conventions layered on top of RDFa's.  See
`shexc-ts-mode-prefix-maps' for how to define \"my-project\", and
`shexc-ts-mode-set-prefix-map' for an interactive way to change this
for just the current buffer."
  :type '(choice string (repeat string))
  :safe (lambda (val) (or (stringp val) (and (listp val) (seq-every-p #'stringp val))))
  :group 'shexc-ts)

;;; Syntax table

(defvar shexc-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; block comments: "/* ... */" (style a -- pairs with / and *)
    ;; line comments:  "# ..."    (style b -- pairs with # and \n)
    ;; Two-style setup like C-mode: forward-comment/syntax-ppss handle
    ;; both styles; a style-b `#' inside a style-a `/* */' comment is
    ;; not treated as a comment-start, and vice-versa.
    (modify-syntax-entry ?/ ". 14" table)  ; 1=1st of /**/, 4=2nd of **/
    (modify-syntax-entry ?* ". 23" table)  ; 2=2nd of /**/, 3=1st of **/
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; "." is part of a symbol (e.g. the wildcard shape `.' and PN_LOCAL)
    (modify-syntax-entry ?. "_" table)
    ;; punctuation that shouldn't be picked up by `thing-at-point' 'symbol
    ;; (? + | ^ stay as punctuation; * is already ". 23" above)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?^ "." table)
    table)
  "Syntax table for `shexc-ts-mode'.")

;; `#' both starts a line comment ("# ...") *and* routinely appears
;; inside `irireference's, e.g. the fragment separators in `<#Shape>'
;; or `<http://example.org/onto#Class>' -- ShExC shape labels are
;; conventionally local fragment IRIs like `<#Shape>'. The syntax
;; table can't tell those two roles apart (it would need to mark every
;; `#' as a comment starter, including the ones inside IRIs), which
;; otherwise makes the syntax engine believe a `{'/`}' following
;; `<#Shape>' on the same line is "inside a comment" -- breaking
;; `forward-list'/`up-list'/`backward-up-list' (`C-M-n'/`C-M-u' etc.)
;; and any other syntax-table-based scanning.  `treesit-node-at' can
;; tell them apart directly from the parsed tree (a real comment is a
;; `comment' node; an in-IRI `#' is just part of an `irireference'),
;; so use a `syntax-propertize-function' to neutralize the latter.
(defun shexc-ts-mode--syntax-propertize (beg end)
  "Neutralize non-comment `#' characters between BEG and END.

Give `#' characters that are not `#'-comment starters (e.g. the
fragment separator in `<#Shape>' IRI references) a neutral
`syntax-table' property, so that `forward-list'/`up-list' and
other syntax-based scanning don't mistake them for the start of
a line comment that swallows the rest of that line -- including
any `{'/`}' on it.

A `#' is considered to be inside an IRI (and therefore not a
comment starter) when scanning backward to the beginning of the
line finds an unmatched `<' before any `>'."
  (goto-char beg)
  (while (re-search-forward "#" end t)
    (let ((pos (match-beginning 0)))
      (when (save-excursion
              (let ((bol (line-beginning-position)))
                (and (re-search-backward "<" bol t)
                     (not (re-search-forward ">" pos t)))))
        (with-silent-modifications
          (put-text-property pos (1+ pos)
                             'syntax-table (string-to-syntax "_")))))))

;;; Comments
;;
;; ShExC has both `#'-prefixed line comments and C-style `/* ... */'
;; block comments (the grammar folds both into one `comment' node, so
;; font-lock already handles both -- see the `comment' feature above).
;; `comment-start'/`comment-end' can only describe one style at a
;; time, so -- mirroring `c-ts-mode-toggle-comment-style' -- we let
;; `M-;' default to line comments and provide a command to switch to
;; block comments for languages/regions where they read better (e.g.
;; commenting out a multi-line chunk of a schema).

(defun shexc-ts-mode-comment-dwim (arg)
  "Comment or uncomment the current line or region.
See `comment-dwim' for the meaning of ARG."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil))
    (comment-dwim arg)))

(defun shexc-ts-mode-toggle-comment-style (&optional arg)
  "Toggle the comment style between line (`#') and block (`/* */').
Optional numeric ARG, if supplied, switches to block comment
style when positive, to line comment style when negative, and
just toggles it when zero or omitted.  This only affects newly
inserted comments (via `comment-dwim'/`comment-region'/`M-;').

Note that `uncomment-region' looks for the *current* style's
delimiters first: in line-comment style it will not recognize
\(and so cannot strip) a pre-existing `/* ... */' comment, while
in block-comment style it can still strip a pre-existing `#'
comment (because `comment-start-skip' always matches both
styles, and a bare `#' has nothing further to match once its
prefix is removed).  Toggle to the matching style first if you
need to remove a comment written in the other style."
  (interactive "P")
  (let ((prevstate-line (string= comment-start "# ")))
    (when (or (not arg)
              (zerop (setq arg (prefix-numeric-value arg)))
              (xor (> 0 arg) prevstate-line))
      (if prevstate-line
          (progn
            (setq-local comment-start "/* "
                        comment-end   " */")
            ;; Block-comment style: wrap the whole region in one pair
            ;; (`/* ' at the start, ` */' at the end) rather than
            ;; bracketing each line individually.  `comment-style
            ;; 'multi-line' + empty `comment-continue' achieves this.
            (setq-local comment-style    'multi-line
                        comment-continue ""))
        (setq-local comment-start "# "
                    comment-end   "")
        ;; Line-comment style: back to one delimiter per line.
        (setq-local comment-style    'plain
                    comment-continue nil))
        ;; `comment-use-syntax' is normally derived once (from
        ;; `comment-start' and the syntax table) and cached
        ;; buffer-locally; force it to be re-derived for the new
        ;; style, or `comment-region'/`uncomment-region' may keep
        ;; using stale syntax-based scanning that doesn't recognize
        ;; the style we just switched to (our syntax table only
        ;; marks `#'/`\n', not `/'/`*', as comment delimiters).
        (setq-local comment-use-syntax 'undecided)
      (message "ShExC comment style: %s"
               (if prevstate-line "block (/* ... */)" "line (#)")))))

;;; Font-lock

(defvar shexc-ts-mode--keywords
  '("kw_a" "kw_abstract" "kw_and" "kw_base" "kw_bnode" "kw_closed"
    "kw_extends" "kw_external" "kw_extra" "kw_fractiondigits" "kw_import"
    "kw_iri" "kw_length" "kw_literal" "kw_maxexclusive" "kw_maxinclusive"
    "kw_maxlength" "kw_minexclusive" "kw_mininclusive" "kw_minlength"
    "kw_nonliteral" "kw_not" "kw_or" "kw_prefix" "kw_restricts" "kw_start"
    "kw_totaldigits")
  "ShExC keyword node types (excluding ones aliased from punctuation).")

(defun shexc-ts-mode--keyword-alternatives ()
  "Build the `[(kw_a) (kw_and) ...]' alternation for the keyword query."
  (apply #'vector (mapcar (lambda (kw) (list (intern kw)))
                          shexc-ts-mode--keywords)))

(defvar shexc-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'shexc
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'shexc
   :feature 'string
   '([(string) (lang_string) (langtag)] @font-lock-string-face)

   :language 'shexc
   :feature 'keyword
   `([,@(shexc-ts-mode--keyword-alternatives) (shape_any)] @font-lock-keyword-face)

   :language 'shexc
   :feature 'definition
   ;; NOTE: `group_triple_expr's `$<label>' is matched without an `id:'
   ;; field constraint -- tree-sitter's query compiler rejects `id:' on
   ;; `group_triple_expr' with a "Structure error" even though the field
   ;; is present on real parse-tree nodes (the field is assigned inside
   ;; the hidden `_unary_triple_expr' rule it's inlined from, and isn't
   ;; registered in the language's static field table). Matching the
   ;; bare child type works and is unambiguous: a `triple_expr_label'
   ;; appearing directly under `group_triple_expr' can only be its `$id',
   ;; never the `&label' of a nested `include' (which is one level deeper).
   :override t
   '((shape_expr_decl label: (shape_expr_label) @font-lock-function-name-face)
     (group_triple_expr (triple_expr_label) @font-lock-function-name-face)
     (include label: (triple_expr_label) @font-lock-function-name-face))

   :language 'shexc
   :feature 'reference
   :override t
   '((shape_ref label: (shape_expr_label) @font-lock-variable-name-face))

   ;; A bare IRI/prefixed-name directly inside a `node_constraint' can only
   ;; be its `datatype' (NodeKind/value-set/numeric-facet constraints never
   ;; carry one as a direct child) -- no need to climb to the `datatype:'
   ;; field on the enclosing `(inline_)shape_atom'.
   :language 'shexc
   :feature 'type
   :override t
   '((prefix_decl name: (_) @font-lock-type-face)
     (node_constraint [(irireference) (prefixed_name)] @font-lock-type-face))

   :language 'shexc
   :feature 'property
   :override t
   '((triple_constraint predicate: (predicate) @font-lock-property-name-face)
     (annotation predicate: (_) @font-lock-property-name-face))

   :language 'shexc
   :feature 'constant
   '([(boolean_literal) (kw_true) (kw_false)
      (irireference) (prefixed_name) (blank_node)] @font-lock-constant-face)

   :language 'shexc
   :feature 'number
   '([(integer) (decimal) (double)] @font-lock-number-face)

   :language 'shexc
   :feature 'preprocessor
   :override t
   '((code_decl name: (_) @font-lock-preprocessor-face)
     (code) @font-lock-preprocessor-face)

   :language 'shexc
   :feature 'bracket
   '((["{" "}" "[" "]" "(" ")"]) @font-lock-bracket-face)

   :language 'shexc
   :feature 'operator
   '((["." "|" "&" "@" "^" "~" "-" "="]) @font-lock-operator-face
     ([(card_star) (card_plus) (card_opt) (kw_inverse)]) @font-lock-operator-face)

   :language 'shexc
   :feature 'delimiter
   '((["," ";"]) @font-lock-delimiter-face))
  "Tree-sitter font-lock settings for `shexc-ts-mode'.")

(defvar shexc-ts-mode--font-lock-feature-list
  '((comment definition)
    (keyword string)
    (constant number preprocessor property reference type)
    (bracket delimiter operator))
  "`treesit-font-lock-feature-list' for `shexc-ts-mode'.

Levels 1-3 (the default `treesit-font-lock-level') cover everything
but raw punctuation; level 4 additionally highlights brackets,
separators and operators.")

;;; Indentation

(defvar shexc-ts-mode--indent-rules
  `((shexc
     ;; closing delimiters line up with the line that opened the block
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)

     ;; OR/AND continuations ("} OR {", "} AND {") line up with the
     ;; opening brace of their sibling, not indented further
     ((parent-is "shape_or") parent-bol 0)
     ((parent-is "shape_and") parent-bol 0)
     ((parent-is "shape_not") parent-bol 0)
     ((parent-is "inline_shape_or") parent-bol 0)
     ((parent-is "inline_shape_and") parent-bol 0)
     ((parent-is "inline_shape_not") parent-bol 0)

     ;; contents of {...}, (...), [...] indent one step from the line
     ;; that opened the block
     ((parent-is "inline_shape_definition") parent-bol shexc-ts-mode-indent-offset)
     ((parent-is "bracketed_triple_expr") parent-bol shexc-ts-mode-indent-offset)
     ((parent-is "value_set") parent-bol shexc-ts-mode-indent-offset)
     ((parent-is "one_of") parent-bol shexc-ts-mode-indent-offset)
     ((parent-is "shape_expr_decl") parent-bol shexc-ts-mode-indent-offset)

     ;; `group_triple_expr' is a flattening wrapper: it always starts at
     ;; the same position as its first `element', so its elements align
     ;; with it rather than stepping in further (the enclosing block --
     ;; `inline_shape_definition'/`bracketed_triple_expr'/`one_of' --
     ;; already applied the one indentation step for this level)
     ((parent-is "group_triple_expr") parent-bol 0)

     ;; top-level declarations start at column 0
     ((parent-is "shex_doc") column-0 0)

     (catch-all parent-bol 0)))
  "`treesit-simple-indent-rules' for `shexc-ts-mode'.")

;;; Imenu

(defun shexc-ts-mode--imenu-name (node)
  "Return the label text of shape declaration NODE for imenu."
  (let ((label (treesit-node-child-by-field-name node "label")))
    (treesit-node-text (or label node) t)))

(defvar shexc-ts-mode--imenu-settings
  `(("Shape" "\\`shape_expr_decl\\'" nil shexc-ts-mode--imenu-name))
  "`treesit-simple-imenu-settings' for `shexc-ts-mode'.")

;;; Following references (xref)

(defun shexc-ts-mode--label-node-at (pos)
  "Return the smallest `shape_expr_label' node enclosing POS, if any.
This matches both shape declarations (`<#S> { ... }') and shape
references (`@<#S>', `EXTENDS @<#S>', `&<#S>', `start = @<#S>')."
  (let ((node (treesit-node-at pos)))
    (treesit-parent-until
     node
     (lambda (n) (string= (treesit-node-type n) "shape_expr_label"))
     t)))

(defun shexc-ts-mode--label-at-point ()
  "Return the shape-label text at point, or nil."
  (let ((node (shexc-ts-mode--label-node-at (point))))
    (and node (treesit-node-text node t))))

(defun shexc-ts-mode--query-labels (pattern)
  "Return all `shape_expr_label' nodes captured as @label by PATTERN."
  (treesit-query-capture (treesit-buffer-root-node) pattern nil nil t))

(defun shexc-ts-mode--all-labels ()
  "Return all shape-label nodes in the buffer (declarations and references)."
  (append
   (shexc-ts-mode--query-labels
    '((shape_expr_decl label: (shape_expr_label) @label)))
   (shexc-ts-mode--query-labels
    '((shape_ref label: (shape_expr_label) @label)))))

(defun shexc-ts-mode--xref-backend ()
  "Return the xref backend symbol for `shexc-ts-mode'."
  'shexc-ts-mode)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql shexc-ts-mode)))
  "Return the shape label at point, for xref."
  (shexc-ts-mode--label-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql shexc-ts-mode)))
  "Return all shape labels in the buffer, for xref completion."
  (delete-dups (mapcar (lambda (n) (treesit-node-text n t))
                       (shexc-ts-mode--all-labels))))

(cl-defmethod xref-backend-definitions ((_backend (eql shexc-ts-mode)) identifier)
  "Return the `shape_expr_decl' xref defining IDENTIFIER."
  (rdf-core-matching-xrefs
   (shexc-ts-mode--query-labels
    '((shape_expr_decl label: (shape_expr_label) @label)))
   identifier))

(cl-defmethod xref-backend-references ((_backend (eql shexc-ts-mode)) identifier)
  "Return the `shape_ref' xrefs referencing IDENTIFIER."
  (rdf-core-matching-xrefs
   (shexc-ts-mode--query-labels
    '((shape_ref label: (shape_expr_label) @label)))
   identifier))

;;; Navigation: structural defun-nav, smart sexp, breadcrumb
;;
;; `treesit-thing-settings' tells `treesit-major-mode-setup' to wire up:
;; - `beginning-of-defun'/`end-of-defun' (`C-M-a'/`C-M-e') to jump
;;   between `shape_expr_decl's -- one "defun" per ShapeDecl.
;; - `forward-sexp'/`backward-sexp' (`C-M-f'/`C-M-b', and anything
;;   built on them, e.g. `transpose-sexps'/`C-M-t', `kill-sexp') to
;;   treat each "branch" of a shape expression -- an EXTENDS clause,
;;   an AND/OR operand, a NOT-negated shape, a `{ ... }' shape body, a
;;   OneOf/EachOf triple-expression group, or a single triple
;;   constraint -- as one sexp.

(defconst shexc-ts-mode--sexp-node-types
  '(;; shape declarations and references
    "shape_expr_decl" "shape_ref"
    ;; EXTENDS clauses
    "extension"
    ;; AND / OR / NOT combinations, and their `{ ... }'-body forms
    "shape_or" "inline_shape_or"
    "shape_and" "inline_shape_and"
    "shape_not" "inline_shape_not"
    "shape_atom" "inline_shape_atom"
    "shape_definition" "inline_shape_definition"
    ;; NodeConstraints
    "node_constraint" "value_set"
    ;; triple expressions: OneOf disjuncts, EachOf groups, bracketed
    ;; sub-expressions, and individual triple constraints
    "one_of" "group_triple_expr" "bracketed_triple_expr"
    "triple_constraint" "include")
  "Node types treated as a single `sexp' unit in `shexc-ts-mode'.

Each of these corresponds to a \"branch\" of a ShExC shape
expression or triple expression, so that sexp-based navigation
and editing commands move over a whole branch at a time.")

(defun shexc-ts-mode--current-defun-name ()
  "Return a breadcrumb describing the shape construct at point.

The breadcrumb has the form `<Shape> > predicate > predicate...',
where `<Shape>' is the label of the enclosing `shape_expr_decl' and
each `predicate' is the predicate of an enclosing `triple_constraint'
\(outermost first\), as when a `triple_constraint's value is itself a
`{ ... }' shape containing further `triple_constraint's.  Return nil
outside of any `shape_expr_decl'.

Used as `add-log-current-defun-function', which powers
`which-function-mode' and `add-log-current-defun'."
  (let ((node (treesit-node-at (point)))
        (label nil)
        (predicates nil))
    (while node
      (let ((type (treesit-node-type node)))
        (cond
         ((string= type "triple_constraint")
          (when-let* ((pred (treesit-node-child-by-field-name node "predicate")))
            (push (treesit-node-text pred t) predicates)))
         ((string= type "shape_expr_decl")
          (when-let* ((lbl (treesit-node-child-by-field-name node "label")))
            (setq label (treesit-node-text lbl t))))))
      (setq node (treesit-node-parent node)))
    (when label
      (mapconcat #'identity (cons label predicates) " > "))))

;;; Extended-shape highlighting (API for shex-manifest-browser)
;;
;; Terminology used throughout this section:
;;
;; - `shapeLabel' / `predicate' -- the ShapeDecl label, and the
;;   predicates of its shapeExpression, of the *focal*
;;   `shape_expr_decl', i.e. the one containing point.
;; - `reachable-shapeLabel' / `reachable-predicate' -- the label, and
;;   the predicates, of any *other* shape transitively reachable from
;;   the focal shape's shapeExpression, none of whose reaching paths of
;;   its strength cross an EXTENDS edge -- only a sole shape reference
;;   (e.g. `<Contact1> @<Contact>'), AND, OR, or NOT.  E.g. in
;;   `<S> { <p1> . } AND @<S2>' / `<S2> { <p2> . }', `<S2>' is a
;;   `reachable-shapeLabel' and `<p2>' a `reachable-predicate'.
;; - `not-'/`optional-' prefix on a `*-shapeLabel' term -- the shape is
;;   reachable (at that classification) only inside a NOT
;;   (`not-*-shapeLabel'), or via only some branch(es) of an OR
;;   (`optional-*-shapeLabel').  Reachable via AND or as a sole
;;   reference gets neither prefix; see
;;   `shexc-ts-mode--strength-required'/`-negated'/`-optional' below.
;; - `extended-' prefix on a `*-shapeLabel'/`*-predicate' term -- every
;;   reaching path of its strength crosses an EXTENDS edge, and none is
;;   a plain `reachable-*' path (sole reference/AND/OR/NOT).  E.g. if
;;   `<Person> EXTENDS @<Tools>' and `<Tools>' is otherwise unreachable
;;   from `<Person>', then with point in `<Person>': `<Tools>' is
;;   `extended-shapeLabel' (reached only by crossing the EXTENDS edge to
;;   it).
;; - `extended-reachable-' prefix -- every reaching path of its
;;   strength crosses *both* an EXTENDS edge and a plain edge.
;;   Continuing the example above, if `<Tools> { ex:tool @<TBoss> }',
;;   then `<TBoss>' is `extended-reachable-shapeLabel' (every path to it
;;   crosses the EXTENDS edge to `<Tools>', then the plain reference
;;   from `<Tools>' to `<TBoss>'), and `ex:tool' is an
;;   `extended-reachable-predicate'.  When
;;   `shexc-ts-mode-highlight-reachable-extends-trumps-reachable' is
;;   non-nil, `extended-reachable-*' shapes are instead folded into
;;   plain `extended-*', restoring the simpler rule that any EXTENDS
;;   edge on a reaching path governs the classification; see
;;   `shexc-ts-mode--effective-classification'.
;;
;; `shexc-ts-mode-highlight-reachable-shapes' highlights the focal
;; shape's own `shapeLabel' and `predicate's, plus every
;; `reachable-shapeLabel'/`extended-shapeLabel'/
;; `extended-reachable-shapeLabel' and their `-predicate's -- with
;; cycle/duplicate detection.  Each is classified by the *strength* of
;; its strongest reaching path:
;;
;; - `shexc-ts-mode--strength-required' -- a `*-shapeLabel' with
;;   neither a `not-' nor `optional-' prefix (reachable via EXTENDS,
;;   an AND-conjunct, or as a sole shape reference): every node
;;   conforming to the focal shape also conforms to this one.  Also
;;   the strength of the focal shape's own `predicate's.
;; - `shexc-ts-mode--strength-negated' -- a `not-*-shapeLabel':
;;   conformance to the focal shape requires *not* conforming to this
;;   one.
;; - `shexc-ts-mode--strength-optional' -- an `optional-*-shapeLabel':
;;   conformance to the focal shape *may* entail conformance to this
;;   one, depending on which OR-branch holds.
;;
;; "Required" preempts "negated"/"optional": if a shape is reachable
;; via both a required path and a weaker one, it is highlighted as
;; required.  Likewise, if every branch of an OR reaches the same
;; target shape, that target is not downgraded to "optional" by the
;; OR -- see `shexc-ts-mode--merge-or-branches'.
;;
;; Separately, whether reaching paths cross an EXTENDS edge, a plain
;; edge (sole reference/AND/OR/NOT), or both, is tracked alongside the
;; strength as a pair of sticky bits -- `extended' and `reachable' --
;; selecting one of the three `*-shapeLabel'/`*-predicate' prefixes
;; above (`reachable-shapeLabel' has neither bit named in its prefix,
;; but is the `(nil . t)' case); at equal strength,
;; `shexc-ts-mode--reachability-rank' breaks ties between
;; classifications, preferring plain `reachable-' over `extended-' over
;; `extended-reachable-'.  Each combination of role (`shapeLabel'
;; /`predicate' vs `reachable-*'/`extended-*'/`extended-reachable-*'),
;; strength (required/negated/optional), and EXTENDS/plain-edge
;; classification has its own face, built from a role color
;; (`highlight' for labels, `font-lock-warning-face' for predicates), a
;; strength decoration (none/`:strike-through'/`:slant italic' for
;; required/negated/optional), and an access decoration (`:box' for
;; `extended-' and `extended-reachable-', plus `:underline' on
;; `extended-reachable-' alone); see `shexc-ts-mode--pick-label-face'
;; and `shexc-ts-mode--pick-predicate-face'.  The focal shape's own
;; `shapeLabel' and `predicate's are always at
;; `shexc-ts-mode--strength-required' and neither `extended-' nor
;; `extended-reachable-'.

(defface shexc-ts-mode-label-face
  '((t :inherit highlight))
  "Face for a required, non-extended reachable shapeLabel.

A `shapeLabel' or `reachable-shapeLabel' at
`shexc-ts-mode--strength-required' that is not `extended-' --
reachable via an AND-conjunct or as a sole shape reference, with
neither a `not-' nor `optional-' prefix, and without passing through
an EXTENDS edge.  Also the face of the focal shape's own `shapeLabel'."
  :group 'shexc-ts)

(defface shexc-ts-mode-optional-label-face
  '((t :inherit highlight :slant italic))
  "Face for an optional, non-extended reachable shapeLabel.

An `optional-reachable-shapeLabel' that is not `extended-' --
reachable via one branch of an OR but not every branch, and without
passing through an EXTENDS edge (see
`shexc-ts-mode--strength-optional')."
  :group 'shexc-ts)

(defface shexc-ts-mode-negated-label-face
  '((t :inherit highlight :strike-through t))
  "Face for a negated, non-extended reachable shapeLabel.

A `not-reachable-shapeLabel' that is not `extended-' -- reachable only
inside a NOT, and without passing through an EXTENDS edge (see
`shexc-ts-mode--strength-negated')."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-label-face
  '((t :inherit highlight :box t))
  "Face for a required, extended shapeLabel.

An `extended-shapeLabel' at `shexc-ts-mode--strength-required' -- as
`shexc-ts-mode-label-face', but every reaching path of that strength
passes through an EXTENDS edge, and none also crosses a plain
reference/AND/OR/NOT edge.  See
`shexc-ts-mode-extended-reachable-label-face' for when every such path
crosses both kinds of edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-optional-label-face
  '((t :inherit highlight :slant italic :box t))
  "Face for an optional, extended shapeLabel.

An `extended-optional-shapeLabel' -- as
`shexc-ts-mode-optional-label-face', but every reaching path of that
strength passes through an EXTENDS edge, and none also crosses a plain
reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-negated-label-face
  '((t :inherit highlight :strike-through t :box t))
  "Face for a negated, extended shapeLabel.

An `extended-not-shapeLabel' -- as `shexc-ts-mode-negated-label-face',
but every reaching path of that strength passes through an EXTENDS
edge, and none also crosses a plain reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-reachable-label-face
  '((t :inherit highlight :box t :underline t))
  "Face for a required, extended-reachable shapeLabel.

An `extended-reachable-shapeLabel' at
`shexc-ts-mode--strength-required' -- as
`shexc-ts-mode-extended-label-face' (every reaching path of that
strength passes through an EXTENDS edge), but every such path also
crosses a plain reference/AND/OR/NOT edge.  See
`shexc-ts-mode-highlight-reachable-extends-trumps-reachable' to instead
treat these as plain `extended-shapeLabel's, gated by
`shexc-ts-mode-highlight-reachable-include-extended' alone."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-reachable-optional-label-face
  '((t :inherit highlight :slant italic :box t :underline t))
  "Face for an optional, extended-reachable shapeLabel.

An `extended-reachable-optional-shapeLabel' -- as
`shexc-ts-mode-extended-optional-label-face', but every reaching path
of that strength also crosses a plain reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-reachable-negated-label-face
  '((t :inherit highlight :strike-through t :box t :underline t))
  "Face for a negated, extended-reachable shapeLabel.

An `extended-reachable-not-shapeLabel' -- as
`shexc-ts-mode-extended-negated-label-face', but every reaching path
of that strength also crosses a plain reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-predicate-face
  '((t :inherit font-lock-warning-face))
  "Face for a required, non-extended reachable predicate.

A `predicate' or `reachable-predicate' at
`shexc-ts-mode--strength-required' that is not `extended-' --
reachable via an AND-conjunct or as a sole shape reference, with
neither a `not-' nor `optional-' prefix, and without passing through
an EXTENDS edge (a `predicate' of the focal shape itself is always at
this strength and never `extended-')."
  :group 'shexc-ts)

(defface shexc-ts-mode-optional-predicate-face
  '((t :inherit font-lock-warning-face :slant italic))
  "Face for an optional, non-extended reachable predicate.

An `optional-reachable-predicate' that is not `extended-' --
reachable via one branch of an OR but not every branch, and without
passing through an EXTENDS edge (see
`shexc-ts-mode--strength-optional')."
  :group 'shexc-ts)

(defface shexc-ts-mode-negated-predicate-face
  '((t :inherit font-lock-warning-face :strike-through t))
  "Face for a negated, non-extended reachable predicate.

A `not-reachable-predicate' that is not `extended-' -- reachable only
inside a NOT, and without passing through an EXTENDS edge (see
`shexc-ts-mode--strength-negated')."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-predicate-face
  '((t :inherit font-lock-warning-face :box t))
  "Face for a required, extended predicate.

An `extended-predicate' at `shexc-ts-mode--strength-required' -- as
`shexc-ts-mode-predicate-face', but every reaching path of that
strength passes through an EXTENDS edge, and none also crosses a plain
reference/AND/OR/NOT edge.  See
`shexc-ts-mode-extended-reachable-predicate-face' for when every such
path crosses both kinds of edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-optional-predicate-face
  '((t :inherit font-lock-warning-face :slant italic :box t))
  "Face for an optional, extended predicate.

An `extended-optional-predicate' -- as
`shexc-ts-mode-optional-predicate-face', but every reaching path of
that strength passes through an EXTENDS edge, and none also crosses a
plain reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-negated-predicate-face
  '((t :inherit font-lock-warning-face :strike-through t :box t))
  "Face for a negated, extended predicate.

An `extended-not-predicate' -- as
`shexc-ts-mode-negated-predicate-face', but every reaching path of
that strength passes through an EXTENDS edge, and none also crosses a
plain reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-reachable-predicate-face
  '((t :inherit font-lock-warning-face :box t :underline t))
  "Face for a required, extended-reachable predicate.

An `extended-reachable-predicate' at
`shexc-ts-mode--strength-required' -- as
`shexc-ts-mode-extended-predicate-face' (every reaching path of that
strength passes through an EXTENDS edge), but every such path also
crosses a plain reference/AND/OR/NOT edge.  See
`shexc-ts-mode-highlight-reachable-extends-trumps-reachable' to instead
treat these as plain `extended-predicate's, gated by
`shexc-ts-mode-highlight-reachable-include-extended' alone (in addition
to `shexc-ts-mode-highlight-reachable-include-predicates')."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-reachable-optional-predicate-face
  '((t :inherit font-lock-warning-face :slant italic :box t :underline t))
  "Face for an optional, extended-reachable predicate.

An `extended-reachable-optional-predicate' -- as
`shexc-ts-mode-extended-optional-predicate-face', but every reaching
path of that strength also crosses a plain reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defface shexc-ts-mode-extended-reachable-negated-predicate-face
  '((t :inherit font-lock-warning-face :strike-through t :box t :underline t))
  "Face for a negated, extended-reachable predicate.

An `extended-reachable-not-predicate' -- as
`shexc-ts-mode-extended-negated-predicate-face', but every reaching
path of that strength also crosses a plain reference/AND/OR/NOT edge."
  :group 'shexc-ts)

(defcustom shexc-ts-mode-highlight-reachable-include-current t
  "Whether to highlight the focal shape's own `shapeLabel'.
When non-nil, `shexc-ts-mode-highlight-reachable-mode' highlights it
using `shexc-ts-mode-label-face'.  See
`shexc-ts-mode-highlight-reachable-shapes'."
  :type 'boolean
  :group 'shexc-ts)

(defcustom shexc-ts-mode-highlight-reachable-include-non-extended t
  "Whether to highlight `reachable-shapeLabel's reached via a plain edge.
Those are shapes with at least one reaching path of their strength via
AND, OR, NOT, or a sole reference -- i.e. not `extended-' shapes (see
`shexc-ts-mode-highlight-reachable-include-extended').  When nil, plain
`reachable-shapeLabel's (and their predicates) are excluded entirely,
as if those references were not followed.  An
`extended-reachable-shapeLabel' is also excluded by this, unless
`shexc-ts-mode-highlight-reachable-extends-trumps-reachable' is
non-nil (see `shexc-ts-mode--effective-classification').  A shape also
reachable, at the same strength, via a remaining classification is
still shown, via that classification.  See
`shexc-ts-mode-highlight-reachable-shapes'."
  :type 'boolean
  :group 'shexc-ts)

(defcustom shexc-ts-mode-highlight-reachable-include-extended t
  "Whether to highlight `extended-shapeLabel's and their predicates.
Those are shapes with at least one reaching path of their strength
through an EXTENDS edge.  When nil, `extended-shapeLabel's (and their
predicates) are excluded entirely, as if EXTENDS edges were not
followed.  An `extended-reachable-shapeLabel' is also excluded by this,
unless `shexc-ts-mode-highlight-reachable-extends-trumps-reachable' is
non-nil, in which case it is instead governed by this option alone
\(see `shexc-ts-mode--effective-classification').  A shape also
reachable, at the same strength, via a remaining classification is
still shown, via that classification.  See
`shexc-ts-mode-highlight-reachable-shapes'."
  :type 'boolean
  :group 'shexc-ts)

(defcustom shexc-ts-mode-highlight-reachable-include-predicates t
  "Whether to highlight the `predicate's of any highlighted `*-shapeLabel'.
When non-nil, `shexc-ts-mode-highlight-reachable-mode' also highlights
the focal shape's own `predicate's (with `shexc-ts-mode-predicate-face')
and, for every other highlighted `*-shapeLabel', its `*-predicate's
\(with the predicate face matching that shape's classification --
see `shexc-ts-mode--pick-predicate-face').  When nil, only
`*-shapeLabel's themselves are highlighted, never `*-predicate's.
This does not affect which `*-shapeLabel's are highlighted; see
`shexc-ts-mode-highlight-reachable-include-current',
`-include-non-extended', `-include-extended', and
`-extends-trumps-reachable' for that.  See
`shexc-ts-mode-highlight-reachable-shapes'."
  :type 'boolean
  :group 'shexc-ts)

(defcustom shexc-ts-mode-highlight-reachable-extends-trumps-reachable nil
  "Whether an EXTENDS edge on a path makes its plain-reference bit irrelevant.
When nil (the default), an `extended-reachable-shapeLabel' -- a shape
with every reaching path of its strength crossing *both* an EXTENDS
edge and a plain reference/AND/OR/NOT edge -- gets its own
`shexc-ts-mode-extended-reachable-label-face' family, and requires
*both* `shexc-ts-mode-highlight-reachable-include-extended' and
`-include-non-extended' to be non-nil to be highlighted.

When non-nil, such a shape is instead folded into the plain
`extended-' classification -- as if its plain-reference bit were nil
-- so it is highlighted using `shexc-ts-mode-extended-label-face' (and
the corresponding predicate/strength faces) and gated by
`-include-extended' alone, ignoring `-include-non-extended'.  This
restores the single-flag behavior where any EXTENDS edge on a reaching
path governs the shape's classification.

See `shexc-ts-mode--effective-classification' and
`shexc-ts-mode-highlight-reachable-shapes'."
  :type 'boolean
  :group 'shexc-ts)

(defvar-local shexc-ts-mode--reachable-overlays nil
  "Overlays placed by `shexc-ts-mode-highlight-reachable-shapes'.")

(defun shexc-ts-mode-clear-reachable-overlays ()
  "Remove all overlays added by `shexc-ts-mode-highlight-reachable-shapes'.
Besides deleting everything in `shexc-ts-mode--reachable-overlays'
\(the normal, fast path\), also sweeps the whole buffer for any leftover
overlay carrying the `shexc-ts-mode-highlight-reachable' property --
defense in depth against that list ever getting out of sync with
reality \(e.g. a caller other than `shexc-ts-mode-highlight-reachable-shapes'
itself adding overlays, or a future bug\), which would otherwise leave
highlighting visible with no way left to remove it."
  (interactive)
  (mapc #'delete-overlay shexc-ts-mode--reachable-overlays)
  (setq shexc-ts-mode--reachable-overlays nil)
  (remove-overlays (point-min) (point-max) 'shexc-ts-mode-highlight-reachable t))

(defun shexc-ts-mode--decl-at (pos)
  "Return the `shape_expr_decl' node containing POS, or nil if none.
`treesit-node-at' falls back to the nearest preceding node when POS is
past the end of the buffer's last node -- e.g. in trailing whitespace,
or in a blank line between two declarations -- so the candidate decl's
span must still be checked against POS."
  (when-let* ((decl (treesit-parent-until
                      (treesit-node-at pos)
                      (lambda (n) (string= (treesit-node-type n) "shape_expr_decl"))
                      t)))
    (and (<= (treesit-node-start decl) pos (treesit-node-end decl))
         decl)))

(defconst shexc-ts-mode--strength-optional 0
  "Reference strength of an optional-reachable shapeLabel/predicate.

An `optional-reachable-shapeLabel' (or `-predicate') is reachable only
via one branch of an OR (and not via every branch), so the focal
shape's conformance only *may* entail conformance to it.")

(defconst shexc-ts-mode--strength-negated 1
  "Reference strength of a not-reachable shapeLabel/predicate.

A `not-reachable-shapeLabel' (or `-predicate') is reachable only inside
a NOT, so the focal shape's conformance entails *non*-conformance to
it.")

(defconst shexc-ts-mode--strength-required 2
  "Reference strength of a `reachable-shapeLabel' (or `-predicate')
with neither a `not-' nor `optional-' prefix: reachable via EXTENDS,
an AND-conjunct, or as a sole shape reference, so the focal shape's
conformance always entails conformance to it.  Also the strength of
the focal shape's own `predicate's.")

(defun shexc-ts-mode--reachability-rank (extended reachable)
  "Rank an (EXTENDED . REACHABLE) classification for tie-breaking.
At equal strength, `shexc-ts-mode--strongest-per-label' and
`shexc-ts-mode-highlight-reachable-shapes' prefer the classification
with the lower rank: plain `reachable-' (neither bit set, rank 0) over
`extended-' (EXTENDED only, rank 1) over `extended-reachable-' (both
bits set, rank 2)."
  (cond ((not extended) 0)
        ((not reachable) 1)
        (t 2)))

(defun shexc-ts-mode--effective-classification (extended reachable)
  "Return the effective (EXTENDED . REACHABLE) used for gating and faces.
When `shexc-ts-mode-highlight-reachable-extends-trumps-reachable' is
non-nil, an `extended-reachable-' classification (EXTENDED and
REACHABLE both non-nil) is folded into plain `extended-' (REACHABLE
forced to nil), so it is governed by
`shexc-ts-mode-highlight-reachable-include-extended' alone, ignoring
`-include-non-extended'."
  (if (and extended reachable
           shexc-ts-mode-highlight-reachable-extends-trumps-reachable)
      (cons extended nil)
    (cons extended reachable)))

(defun shexc-ts-mode--ref-included-p (extended reachable include-extended include-non-extended)
  "Whether a ref classified (EXTENDED . REACHABLE) should be highlighted.
EXTENDED and REACHABLE should already be the effective classification
from `shexc-ts-mode--effective-classification'.  An
`extended-reachable-' ref (both non-nil) requires both
INCLUDE-EXTENDED and INCLUDE-NON-EXTENDED; an `extended-' or plain
`reachable-' ref requires only the matching one of the two."
  (cond ((and extended reachable) (and include-extended include-non-extended))
        (extended include-extended)
        (t include-non-extended)))

(defun shexc-ts-mode--pick-label-face (strength extended reachable)
  "Return the face for a `shapeLabel'/`reachable-shapeLabel', strength STRENGTH.
With the `extended-' prefix iff EXTENDED is non-nil, and the
`extended-reachable-' face (rather than plain `extended-') iff
REACHABLE is also non-nil."
  (cond ((>= strength shexc-ts-mode--strength-required)
         (cond ((and extended reachable) 'shexc-ts-mode-extended-reachable-label-face)
               (extended 'shexc-ts-mode-extended-label-face)
               (t 'shexc-ts-mode-label-face)))
        ((= strength shexc-ts-mode--strength-negated)
         (cond ((and extended reachable) 'shexc-ts-mode-extended-reachable-negated-label-face)
               (extended 'shexc-ts-mode-extended-negated-label-face)
               (t 'shexc-ts-mode-negated-label-face)))
        (t
         (cond ((and extended reachable) 'shexc-ts-mode-extended-reachable-optional-label-face)
               (extended 'shexc-ts-mode-extended-optional-label-face)
               (t 'shexc-ts-mode-optional-label-face)))))

(defun shexc-ts-mode--pick-predicate-face (strength extended reachable)
  "Return the face for a `predicate'/`reachable-predicate' of reference STRENGTH.
With the `extended-' prefix iff EXTENDED is non-nil, and the
`extended-reachable-' face (rather than plain `extended-') iff
REACHABLE is also non-nil.  A `predicate' of the focal shape is always
at `shexc-ts-mode--strength-required' and neither EXTENDED nor
REACHABLE."
  (cond ((>= strength shexc-ts-mode--strength-required)
         (cond ((and extended reachable) 'shexc-ts-mode-extended-reachable-predicate-face)
               (extended 'shexc-ts-mode-extended-predicate-face)
               (t 'shexc-ts-mode-predicate-face)))
        ((= strength shexc-ts-mode--strength-negated)
         (cond ((and extended reachable) 'shexc-ts-mode-extended-reachable-negated-predicate-face)
               (extended 'shexc-ts-mode-extended-negated-predicate-face)
               (t 'shexc-ts-mode-negated-predicate-face)))
        (t
         (cond ((and extended reachable) 'shexc-ts-mode-extended-reachable-optional-predicate-face)
               (extended 'shexc-ts-mode-extended-optional-predicate-face)
               (t 'shexc-ts-mode-optional-predicate-face)))))

(defun shexc-ts-mode--decl-for-label-text (label-text)
  "Return the `shape_expr_decl' node whose label text equals LABEL-TEXT."
  (cl-some (lambda (label-node)
              (when (string= (treesit-node-text label-node t) label-text)
                (treesit-node-parent label-node)))
            (shexc-ts-mode--query-labels
             '((shape_expr_decl label: (shape_expr_label) @label)))))

(defun shexc-ts-mode--decl-predicate-nodes (decl)
  "Return all `predicate' nodes that are descendants of DECL."
  (treesit-query-capture
   (treesit-buffer-root-node)
   '((triple_constraint predicate: (predicate) @pred))
   (treesit-node-start decl) (treesit-node-end decl)
   t))

(defun shexc-ts-mode--direct-extensions (def-node)
  "Return the direct `extension' children of DEF-NODE.
DEF-NODE is a `shape_definition' or `inline_shape_definition' node.
Unwraps the former's single `inline_shape_definition' child first."
  (when (string= (treesit-node-type def-node) "shape_definition")
    (setq def-node (treesit-node-child def-node 0)))
  (let (result)
    (dotimes (i (treesit-node-child-count def-node))
      (let ((child (treesit-node-child def-node i)))
        (when (string= (treesit-node-type child) "extension")
          (push child result))))
    (nreverse result)))

(defun shexc-ts-mode--strongest-per-label (refs)
  "Collapse REFS to one entry per LABEL-TEXT, keeping the strongest STRENGTH.
REFS is a list of (LABEL-TEXT . (STRENGTH . (EXTENDED . REACHABLE))).
At equal STRENGTH, the entry with the lower
`shexc-ts-mode--reachability-rank' preempts the other."
  (let (result)
    (dolist (ref refs)
      (let* ((label (car ref))
             (strength (car (cdr ref)))
             (extended (car (cdr (cdr ref))))
             (reachable (cdr (cdr (cdr ref))))
             (existing (assoc label result)))
        (if existing
            (let* ((cur (cdr existing))
                   (cur-classification (cdr cur)))
              (when (or (> strength (car cur))
                        (and (= strength (car cur))
                             (< (shexc-ts-mode--reachability-rank extended reachable)
                                (shexc-ts-mode--reachability-rank
                                 (car cur-classification) (cdr cur-classification)))))
                (setcdr existing (cons strength (cons extended reachable)))))
          (push (cons label (cons strength (cons extended reachable))) result))))
    result))

(defun shexc-ts-mode--merge-or-branches (left right strength)
  "Merge OR-branch results LEFT and RIGHT into the OR's contribution at STRENGTH.
LEFT and RIGHT are each a `shexc-ts-mode--collect-shape-refs' result
computed at branch-root strength
`shexc-ts-mode--strength-required'.  Each entry is (LABEL-TEXT .
\(STRENGTH . (EXTENDED . REACHABLE))).

A `reachable-shapeLabel' reachable through *both* branches keeps the
weaker of its two per-branch strengths (no OR-downgrade, since every
branch entails it, so it is not made `optional-'), and its EXTENDED
\(respectively REACHABLE) bit is set only if *both* branches' bits are;
a `reachable-shapeLabel' reachable through only one branch is
downgraded to `shexc-ts-mode--strength-optional'
\(`optional-reachable-shapeLabel') and keeps that branch's EXTENDED and
REACHABLE bits as-is.  Either way the strength is then capped at
STRENGTH, since the OR itself may be reached via a NOT or via another
OR-branch."
  (let ((left-map (shexc-ts-mode--strongest-per-label left))
        (right-map (shexc-ts-mode--strongest-per-label right))
        result)
    (dolist (entry left-map)
      (let ((rentry (assoc (car entry) right-map)))
        (if rentry
            (push (cons (car entry)
                         (cons (min strength (car (cdr entry)) (car (cdr rentry)))
                               (cons (and (car (cdr (cdr entry))) (car (cdr (cdr rentry))))
                                     (and (cdr (cdr (cdr entry))) (cdr (cdr (cdr rentry)))))))
                  result)
          (push (cons (car entry)
                       (cons (min strength shexc-ts-mode--strength-optional)
                             (cdr (cdr entry))))
                result))))
    (dolist (entry right-map)
      (unless (assoc (car entry) left-map)
        (push (cons (car entry)
                     (cons (min strength shexc-ts-mode--strength-optional)
                           (cdr (cdr entry))))
              result)))
    (nreverse result)))

(defun shexc-ts-mode--collect-shape-refs (node strength extended reachable via-extends)
  "Collect (LABEL-TEXT . (STRENGTH . (EXTENDED . REACHABLE))) entries from NODE.
Each entry is for one `reachable-shapeLabel'/`extended-shapeLabel'/
`extended-reachable-shapeLabel' (`@<Label>' reference or EXTENDS
target) directly or transitively reachable from NODE, a
`shape_or'/`shape_and'/`shape_not'/`shape_atom' or one of their
`inline_*' counterparts.

STRENGTH determines whether the caller should treat it as plain
\(`shexc-ts-mode--strength-required'), `not-' (`-strength-negated'), or
`optional-' (`-strength-optional').

EXTENDED and REACHABLE are the sticky EXTENDS-edge and
plain-reference-edge bits accumulated for the path from the
traversal's root down to NODE: once either becomes t it stays t for
the rest of the path.  VIA-EXTENDS additionally says whether this call
represents the continuation of an EXTENDS edge -- i.e. whether NODE is
\(or is nested directly inside, via OR/AND/NOT/parens) the `shape_expr'
of an `extension' -- so that a `shape_ref' leaf reached with
VIA-EXTENDS non-nil sets EXTENDED (an EXTENDS target), while one
reached with VIA-EXTENDS nil sets REACHABLE (a plain `@<Label>'
reference).  All three are adjusted by OR/AND/NOT/EXTENDS along the
way -- see `shexc-ts-mode--merge-or-branches'.  Triple expressions
inside `{ ... }' are not descended into, so `reachable-predicate's are
not collected here (see `shexc-ts-mode--make-predicate-overlays')."
  (pcase (treesit-node-type node)
    ((or "shape_or" "inline_shape_or")
     (let ((left (shexc-ts-mode--collect-shape-refs
                   (treesit-node-child-by-field-name node "left")
                   shexc-ts-mode--strength-required extended reachable via-extends))
           (right (shexc-ts-mode--collect-shape-refs
                   (treesit-node-child-by-field-name node "right")
                   shexc-ts-mode--strength-required extended reachable via-extends)))
       (shexc-ts-mode--merge-or-branches left right strength)))
    ((or "shape_and" "inline_shape_and")
     (append (shexc-ts-mode--collect-shape-refs
              (treesit-node-child-by-field-name node "left") strength extended reachable via-extends)
             (shexc-ts-mode--collect-shape-refs
              (treesit-node-child-by-field-name node "right") strength extended reachable via-extends)))
    ((or "shape_not" "inline_shape_not")
     (shexc-ts-mode--collect-shape-refs
      (treesit-node-child-by-field-name node "shape_expr")
      (min strength shexc-ts-mode--strength-negated) extended reachable via-extends))
    ((or "shape_atom" "inline_shape_atom")
     (let ((inner (treesit-node-child-by-field-name node "shape_expr")))
       (when inner
         (cond
          ((string= (treesit-node-type inner) "shape_ref")
           (list (cons (treesit-node-text
                        (treesit-node-child-by-field-name inner "label") t)
                       (cons strength
                             (cons (or extended via-extends)
                                   (or reachable (not via-extends)))))))
          ((member (treesit-node-type inner)
                   '("shape_definition" "inline_shape_definition"))
           (mapcan (lambda (ext)
                     (shexc-ts-mode--collect-shape-refs
                      (treesit-node-child-by-field-name ext "shape_expr")
                      strength extended reachable t))
                   (shexc-ts-mode--direct-extensions inner)))
          (t
           ;; a parenthesized sub-expression: `( ... )' wraps its
           ;; contents in another shape_or/shape_and/shape_not/shape_atom
           ;; (or `inline_*' variant), which the cases above handle
           (shexc-ts-mode--collect-shape-refs inner strength extended reachable via-extends))))))
    (_ nil)))

(defun shexc-ts-mode--make-predicate-overlays (decl strength extended reachable)
  "Overlay each predicate in DECL with the face for STRENGTH/EXTENDED/REACHABLE.
Push the new overlays onto `shexc-ts-mode--reachable-overlays', and
return them.  Each predicate is a `predicate' if DECL is the focal
shape, or a `reachable-predicate' otherwise."
  (let (overlays)
    (dolist (pred-node (shexc-ts-mode--decl-predicate-nodes decl))
      (let ((ov (make-overlay (treesit-node-start pred-node)
                              (treesit-node-end pred-node))))
        (overlay-put ov 'face (shexc-ts-mode--pick-predicate-face strength extended reachable))
        (overlay-put ov 'shexc-ts-mode-highlight-reachable t)
        (push ov shexc-ts-mode--reachable-overlays)
        (push ov overlays)))
    overlays))

(defun shexc-ts-mode-highlight-reachable-shapes (pos)
  "Highlight `shapeLabel's reachable from the `shape_expr_decl' at POS.
If `shexc-ts-mode-highlight-reachable-include-current' is non-nil,
places an overlay on the focal shape's own ShapeDecl label using
`shexc-ts-mode-label-face'.  Places an overlay on the ShapeDecl label of
each shape transitively reachable from it via a sole shape reference
\(e.g. `<Contact1> @<Contact>'), AND, OR, NOT, or EXTENDS -- with
cycle/duplicate detection -- using the label face matching that shape's
plain/`not-'/`optional-' strength, combined with whether any reaching
path of that strength crosses an EXTENDS edge (`extended-') and/or a
plain reference/AND/OR/NOT edge (`reachable-'); see
`shexc-ts-mode--collect-shape-refs', `shexc-ts-mode--merge-or-branches',
`shexc-ts-mode--effective-classification', and
`shexc-ts-mode--pick-label-face' for exactly how that is computed.

`shexc-ts-mode-highlight-reachable-include-non-extended' gates plain
`reachable-shapeLabel's -- those with no EXTENDS edge on any reaching
path of their strength -- and
`shexc-ts-mode-highlight-reachable-include-extended' gates `extended-'
ones -- those with no plain reference/AND/OR/NOT edge on any such
path.  An `extended-reachable-shapeLabel' -- every reaching path of
that strength crosses *both* kinds of edge -- normally requires
*both* options to be non-nil; but when
`shexc-ts-mode-highlight-reachable-extends-trumps-reachable' is
non-nil, it is instead folded into the `extended-' classification and
gated by `-include-extended' alone (see
`shexc-ts-mode--effective-classification').  Either way, when a shape's
preferred classification is excluded but it is also reachable, at the
same strength, via a classification that is not, it is shown via that
other classification instead (see
`shexc-ts-mode--reachability-rank'); only a shape with no remaining
reaching classification is excluded entirely.

When `shexc-ts-mode-highlight-reachable-include-predicates' is
non-nil, also overlays:
- every `predicate' directly in the `shape_expr_decl' at POS, always
  at `shexc-ts-mode-predicate-face' (a `predicate' is always at
  `shexc-ts-mode--strength-required' and neither `extended-' nor
  `reachable-'); and
- every `reachable-predicate' inside each highlighted reachable shape,
  with the predicate face matching that shape's classification above.

Clears any overlays from a previous call first, and overlays nothing
if POS is not inside any `shape_expr_decl'.
Returns the list of label texts of the `reachable-shapeLabel's actually
highlighted (not including the focal shape's own label), in the order
they were first discovered (breadth-first)."
  (shexc-ts-mode-clear-reachable-overlays)
  (let* ((decl (shexc-ts-mode--decl-at pos))
         (best (make-hash-table :test 'equal))
         (label-overlays (make-hash-table :test 'equal))
         (predicate-overlays (make-hash-table :test 'equal))
         (queue (and decl (list (list decl shexc-ts-mode--strength-required nil nil))))
         result)
    (when decl
      (let ((label-node (treesit-node-child-by-field-name decl "label")))
        (when label-node
          (when shexc-ts-mode-highlight-reachable-include-current
            (let ((ov (make-overlay (treesit-node-start label-node)
                                    (treesit-node-end label-node))))
              (overlay-put ov 'face (shexc-ts-mode--pick-label-face
                                     shexc-ts-mode--strength-required nil nil))
              (overlay-put ov 'shexc-ts-mode-highlight-reachable t)
              (push ov shexc-ts-mode--reachable-overlays)))
          ;; seed `best' so a cycle back to the focal shape doesn't
          ;; place a second, possibly different-faced overlay on its
          ;; own label
          (puthash (treesit-node-text label-node t)
                   (cons shexc-ts-mode--strength-required (cons nil nil))
                   best)))
      (when shexc-ts-mode-highlight-reachable-include-predicates
        (shexc-ts-mode--make-predicate-overlays
         decl shexc-ts-mode--strength-required nil nil)))
    (while queue
      (pcase-let* ((`(,cur-decl ,cur-strength ,cur-extended ,cur-reachable) (pop queue))
                   (refs (shexc-ts-mode--collect-shape-refs
                          (treesit-node-child-by-field-name cur-decl "shape_expr")
                          cur-strength cur-extended cur-reachable nil)))
        (dolist (ref (shexc-ts-mode--strongest-per-label refs))
          (let* ((label-text (car ref))
                 (ref-strength (car (cdr ref)))
                 (ref-extended (car (cdr (cdr ref))))
                 (ref-reachable (cdr (cdr (cdr ref))))
                 (eff (shexc-ts-mode--effective-classification ref-extended ref-reachable))
                 (eff-extended (car eff))
                 (eff-reachable (cdr eff))
                 (prev (gethash label-text best))
                 (prev-strength (car prev))
                 (prev-extended (car (cdr prev)))
                 (prev-reachable (cdr (cdr prev))))
            (when (and (shexc-ts-mode--ref-included-p
                        eff-extended eff-reachable
                        shexc-ts-mode-highlight-reachable-include-extended
                        shexc-ts-mode-highlight-reachable-include-non-extended)
                       (or (not prev)
                           (> ref-strength prev-strength)
                           (and (= ref-strength prev-strength)
                                (< (shexc-ts-mode--reachability-rank eff-extended eff-reachable)
                                   (shexc-ts-mode--reachability-rank prev-extended prev-reachable)))))
              (puthash label-text (cons ref-strength (cons eff-extended eff-reachable)) best)
              (unless prev (push label-text result))
              (let ((ext-decl (shexc-ts-mode--decl-for-label-text label-text)))
                (when ext-decl
                  (let ((ov (gethash label-text label-overlays))
                        (label-node (treesit-node-child-by-field-name ext-decl "label")))
                    (if ov
                        (overlay-put ov 'face (shexc-ts-mode--pick-label-face
                                               ref-strength eff-extended eff-reachable))
                      (when label-node
                        (setq ov (make-overlay (treesit-node-start label-node)
                                               (treesit-node-end label-node)))
                        (overlay-put ov 'face (shexc-ts-mode--pick-label-face
                                               ref-strength eff-extended eff-reachable))
                        (overlay-put ov 'shexc-ts-mode-highlight-reachable t)
                        (push ov shexc-ts-mode--reachable-overlays)
                        (puthash label-text ov label-overlays))))
                  (when shexc-ts-mode-highlight-reachable-include-predicates
                    (let ((pred-ovs (gethash label-text predicate-overlays)))
                      (if pred-ovs
                          (dolist (ov pred-ovs)
                            (overlay-put ov 'face (shexc-ts-mode--pick-predicate-face
                                                   ref-strength eff-extended eff-reachable)))
                        (puthash label-text
                                 (shexc-ts-mode--make-predicate-overlays
                                  ext-decl ref-strength eff-extended eff-reachable)
                                 predicate-overlays))))
                  (push (list ext-decl ref-strength ref-extended ref-reachable) queue))))))))
    (nreverse result)))

(defvar-local shexc-ts-mode--highlight-reachable-last-decl-start nil
  "Start position of the last-highlighted `shape_expr_decl', or `none'.
Set by `shexc-ts-mode--highlight-reachable-update'.  The symbol
`none' means the last update found point outside any
`shape_expr_decl' and cleared the overlays.  Used to avoid
recomputing the highlight on every command when point hasn't left the
current shape.")

(defun shexc-ts-mode--highlight-reachable-update ()
  "Update the `reachable-shapeLabel'/`reachable-predicate' overlays for point.
Only recomputes if the `shape_expr_decl' at point has changed since
the last update.  Clears the overlays when point is outside any
`shape_expr_decl'.  Intended for `post-command-hook' via
`shexc-ts-mode-highlight-reachable-mode'."
  (let* ((decl (shexc-ts-mode--decl-at (point)))
         (key (if decl (treesit-node-start decl) 'none)))
    (unless (eq key shexc-ts-mode--highlight-reachable-last-decl-start)
      (setq shexc-ts-mode--highlight-reachable-last-decl-start key)
      (if decl
          (shexc-ts-mode-highlight-reachable-shapes (point))
        (shexc-ts-mode-clear-reachable-overlays)))))

;;;###autoload
(define-minor-mode shexc-ts-mode-highlight-reachable-mode
  "Toggle live highlighting of shapes reachable from the shape at point.

While enabled, the focal shape's own `shapeLabel'/`predicate's and the
`reachable-shapeLabel's/`reachable-predicate's of the `shape_expr_decl'
containing point, as found by `shexc-ts-mode-highlight-reachable-shapes',
are highlighted, updating automatically as point moves between shapes
and clearing when point leaves all `shape_expr_decl's.  Which of those
are included is controlled independently by
`shexc-ts-mode-highlight-reachable-include-current',
`-include-non-extended', `-include-extended',
`-extends-trumps-reachable', and `-include-predicates' (see
`shexc-ts-mode-menu'); toggling those options does not disable this
mode, so turning this mode off and back on (e.g. with
\\[shexc-ts-mode-highlight-reachable-mode]) restores the same selection.

This is the standalone, manifest-free counterpart to the highlighting
`shex-manifest-browser-mode' drives from `sht:shape'; the two can be
used independently or together."
  :lighter " ExtH"
  (if shexc-ts-mode-highlight-reachable-mode
      (progn
        (add-hook 'post-command-hook #'shexc-ts-mode--highlight-reachable-update nil t)
        (shexc-ts-mode--highlight-reachable-update))
    (remove-hook 'post-command-hook #'shexc-ts-mode--highlight-reachable-update t)
    (setq shexc-ts-mode--highlight-reachable-last-decl-start nil)
    (shexc-ts-mode-clear-reachable-overlays)))

(defun shexc-ts-mode--refresh-highlight-reachable ()
  "Recompute the `shexc-ts-mode-highlight-reachable-mode' overlays at point.
Does nothing if that mode is not currently enabled."
  (when shexc-ts-mode-highlight-reachable-mode
    (if (shexc-ts-mode--decl-at (point))
        (shexc-ts-mode-highlight-reachable-shapes (point))
      (shexc-ts-mode-clear-reachable-overlays))))

;;;###autoload
(defun shexc-ts-mode-toggle-highlight-reachable-current ()
  "Toggle `shexc-ts-mode-highlight-reachable-include-current'.

That option controls whether `shexc-ts-mode-highlight-reachable-mode'
highlights the focal shape's own `shapeLabel'.  If
`shexc-ts-mode-highlight-reachable-mode' is currently enabled, its
overlays are refreshed immediately to reflect the new setting."
  (interactive)
  (setq shexc-ts-mode-highlight-reachable-include-current
        (not shexc-ts-mode-highlight-reachable-include-current))
  (shexc-ts-mode--refresh-highlight-reachable)
  (message "shexc-ts-mode-highlight-reachable-include-current: %s"
           shexc-ts-mode-highlight-reachable-include-current))

;;;###autoload
(defun shexc-ts-mode-toggle-highlight-reachable-non-extended ()
  "Toggle `shexc-ts-mode-highlight-reachable-include-non-extended'.

That option controls whether `shexc-ts-mode-highlight-reachable-mode'
highlights `reachable-shapeLabel's reached via a plain edge (and their
`reachable-predicate's), as well as -- together with
`-include-extended' -- `extended-reachable-shapeLabel's, unless
`shexc-ts-mode-highlight-reachable-extends-trumps-reachable' is
non-nil.  If `shexc-ts-mode-highlight-reachable-mode' is currently
enabled, its overlays are refreshed immediately to reflect the new
setting."
  (interactive)
  (setq shexc-ts-mode-highlight-reachable-include-non-extended
        (not shexc-ts-mode-highlight-reachable-include-non-extended))
  (shexc-ts-mode--refresh-highlight-reachable)
  (message "shexc-ts-mode-highlight-reachable-include-non-extended: %s"
           shexc-ts-mode-highlight-reachable-include-non-extended))

;;;###autoload
(defun shexc-ts-mode-toggle-highlight-reachable-extended ()
  "Toggle `shexc-ts-mode-highlight-reachable-include-extended'.

That option controls whether `shexc-ts-mode-highlight-reachable-mode'
also highlights `extended-shapeLabel's and `extended-predicate's --
those reached via an EXTENDS edge -- as well as, together with
`-include-non-extended' (or alone when
`shexc-ts-mode-highlight-reachable-extends-trumps-reachable' is
non-nil), `extended-reachable-shapeLabel's.  If
`shexc-ts-mode-highlight-reachable-mode' is currently enabled, its
overlays are refreshed immediately to reflect the new setting."
  (interactive)
  (setq shexc-ts-mode-highlight-reachable-include-extended
        (not shexc-ts-mode-highlight-reachable-include-extended))
  (shexc-ts-mode--refresh-highlight-reachable)
  (message "shexc-ts-mode-highlight-reachable-include-extended: %s"
           shexc-ts-mode-highlight-reachable-include-extended))

;;;###autoload
(defun shexc-ts-mode-toggle-highlight-reachable-extends-trumps-reachable ()
  "Toggle `shexc-ts-mode-highlight-reachable-extends-trumps-reachable'.

That option controls whether an `extended-reachable-shapeLabel' -- one
whose every reaching path of its strength crosses both an EXTENDS edge
and a plain reference/AND/OR/NOT edge -- is folded into the plain
`extended-' classification (and gated by
`shexc-ts-mode-highlight-reachable-include-extended' alone) rather
than getting its own `extended-reachable-' classification, gated by
both `-include-extended' and `-include-non-extended'.  If
`shexc-ts-mode-highlight-reachable-mode' is currently enabled, its
overlays are refreshed immediately to reflect the new setting."
  (interactive)
  (setq shexc-ts-mode-highlight-reachable-extends-trumps-reachable
        (not shexc-ts-mode-highlight-reachable-extends-trumps-reachable))
  (shexc-ts-mode--refresh-highlight-reachable)
  (message "shexc-ts-mode-highlight-reachable-extends-trumps-reachable: %s"
           shexc-ts-mode-highlight-reachable-extends-trumps-reachable))

;;;###autoload
(defun shexc-ts-mode-toggle-highlight-reachable-predicates ()
  "Toggle `shexc-ts-mode-highlight-reachable-include-predicates'.

That option controls whether `shexc-ts-mode-highlight-reachable-mode'
also highlights the `predicate's of every highlighted `*-shapeLabel' --
the focal shape's own `predicate's, and each reachable shape's
`*-predicate's, with the predicate face matching that shape's
classification.  If `shexc-ts-mode-highlight-reachable-mode' is
currently enabled, its overlays are refreshed immediately to reflect
the new setting."
  (interactive)
  (setq shexc-ts-mode-highlight-reachable-include-predicates
        (not shexc-ts-mode-highlight-reachable-include-predicates))
  (shexc-ts-mode--refresh-highlight-reachable)
  (message "shexc-ts-mode-highlight-reachable-include-predicates: %s"
           shexc-ts-mode-highlight-reachable-include-predicates))

;;; Structural editing: unwrapping/wrapping `<predicate> { ... }' shapes
;;
;; These mirror the "splice"/"unwrap" and "wrap" operations familiar
;; from `paredit'/`combobulate', specialized to ShExC's
;; `<predicate> { ... }' nested-shape construct -- e.g. turning
;;
;;   <#foo> {
;;     <p1> {
;;       <p11> @<#bar> ;
;;       <p12> @<#bar>
;;     }
;;   }
;;
;; into
;;
;;   <#foo> {
;;     <p11> @<#bar> ;
;;     <p12> @<#bar>
;;   }
;;
;; and back.

(defun shexc-ts-mode--plain-inline-shape-definition (value-expr)
  "Return VALUE-EXPR's `inline_shape_definition' if it is simply a `{ ... }' shape.
VALUE-EXPR is an `inline_shape_expression'.  \"Simply a `{ ... }'
shape\" means an `inline_shape_definition' with no NodeConstraint and
no AND/OR/NOT combination; otherwise returns nil."
  (when (and value-expr
             (string= (treesit-node-type value-expr) "inline_shape_expression"))
    (let ((atom (treesit-node-child value-expr 0 t)))
      (when (and atom (string= (treesit-node-type atom) "inline_shape_atom")
                 (not (treesit-node-child-by-field-name atom "constraint")))
        (let ((shape-expr (treesit-node-child-by-field-name atom "shape_expr")))
          (and shape-expr
               (string= (treesit-node-type shape-expr) "inline_shape_definition")
               shape-expr))))))

(defun shexc-ts-mode--shape-wrapper-at (pos)
  "Return (TRIPLE-CONSTRAINT . INLINE-SHAPE-DEFINITION) for POS, or nil.
For the nearest `<predicate> { ... }' construct enclosing POS, or nil
if there is none.  Tries both POS and POS-1 as starting points: a
wrapper's closing brace coincides with the end of several
ancestor nodes at once, so which node `treesit-node-at' returns
right after `}' depends on which side of that shared boundary it
resolves to."
  (catch 'found
    (dolist (start (delete-dups (delq nil (list (treesit-node-at pos)
                                                 (and (> pos (point-min))
                                                      (treesit-node-at (1- pos)))))))
      (let ((node start))
        (while node
          (when (string= (treesit-node-type node) "triple_constraint")
            (let ((shape-def (shexc-ts-mode--plain-inline-shape-definition
                              (treesit-node-child-by-field-name node "value_expr"))))
              (when shape-def
                (throw 'found (cons node shape-def)))))
          (setq node (treesit-node-parent node)))))
    nil))

(defun shexc-ts-mode-unwrap-shape ()
  "Remove the `<predicate> { ... }' wrapper enclosing point.
Keeps its triple expressions, re-indenting them in place \(the
structural-editing \"splice\"/\"unwrap\"/\"raise\" operation, e.g.
`paredit-splice-sexp', specialized to ShExC's nested-shape
construct).
For example, with point inside

    <#foo> {
      <p1> {
        <p11> @<#bar> ;
        <p12> @<#bar>
      }
    }

this produces

    <#foo> {
      <p11> @<#bar> ;
      <p12> @<#bar>
    }

This only handles the simple case where the nested shape is the
predicate's entire value -- no NodeConstraint, no AND/OR/NOT
combination; anything else is reported with a `user-error'.
Note that any cardinality (`*'/`+'/`?') or annotations on the
removed triple constraint are discarded along with its
predicate."
  (interactive)
  (let ((found (shexc-ts-mode--shape-wrapper-at (point))))
    (unless found
      (user-error "No simple `<predicate> { ... }' shape to unwrap here"))
    (pcase-let* ((`(,triple-constraint . ,shape-def) found)
                 (beg (treesit-node-start triple-constraint))
                 (end (treesit-node-end triple-constraint))
                 (expression (treesit-node-child-by-field-name shape-def "expression"))
                 (inner (if expression (treesit-node-text expression t) "")))
      (delete-region beg end)
      (goto-char beg)
      (insert inner)
      (indent-region beg (point)))))

(defun shexc-ts-mode-wrap-in-shape ()
  "Wrap the triple expression(s) at point, or in the active region.
In a new `PREDICATE { ... }' shape that you are prompted for,
re-indenting the result \(the inverse \"wrap\" operation, e.g.
`paredit-wrap-round'/`embrace-add', specialized to inject a ShExC
predicate along with the braces).
For example, selecting

    <#foo> {
      <p11> @<#bar> ;
      <p12> @<#bar>
    }

and supplying `<p1>' as the predicate produces

    <#foo> {
      <p1> {
        <p11> @<#bar> ;
        <p12> @<#bar>
      }
    }

With no active region, it wraps just the single triple
expression element at point."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (if (use-region-p)
                                   (cons (region-beginning) (region-end))
                                 (cons (point) (1+ (point)))))
               (probe (or (treesit-node-on beg end) (treesit-node-at beg)))
               (group (treesit-parent-until
                       probe
                       (lambda (n) (string= (treesit-node-type n) "group_triple_expr"))
                       t)))
    (unless group
      (user-error "No triple expression to wrap here"))
    (let ((elements (seq-filter
                     (lambda (c)
                       (and (equal (treesit-node-field-name c) "element")
                            (< (treesit-node-start c) end)
                            (> (treesit-node-end c) beg)))
                     (treesit-node-children group))))
      (unless elements
        (user-error "No triple expression to wrap here"))
      (let* ((new-beg (treesit-node-start (car elements)))
             (new-end (treesit-node-end (car (last elements))))
             (inner (buffer-substring-no-properties new-beg new-end))
             (predicate (string-trim
                         (read-string "Wrap in shape with predicate (e.g. <p1> or ex:p1): "))))
        (when (string-empty-p predicate)
          (user-error "A predicate is required"))
        (delete-region new-beg new-end)
        (goto-char new-beg)
        (insert predicate " {\n" inner "\n}")
        (indent-region new-beg (point))))))

;;; Folding (hideshow)

(defun shexc-ts-mode--def-open-brace (def)
  "Return the position of the `{' that opens shape-body node DEF.

DEF is a `shape_definition' or `inline_shape_definition'.  Its `{'
may be preceded by modifiers such as `CLOSED'/`EXTRA ...', so it is
located by scanning DEF's children rather than assuming it is the
first one."
  (cl-loop for i below (treesit-node-child-count def)
           for child = (treesit-node-child def i)
           when (string= (treesit-node-type child) "{")
           return (treesit-node-start child)))

(defun shexc-ts-mode--first-brace-in (node)
  "Return the position of the first `{' token in NODE's subtree, or nil.

A plain depth-first walk over all children (including anonymous
ones, like `{' itself) -- `treesit-search-subtree' does not visit
anonymous nodes via a function PREDICATE."
  (cl-block found
    (cl-labels ((walk (n)
                  (if (string= (treesit-node-type n) "{")
                      (cl-return-from found (treesit-node-start n))
                    (dotimes (i (treesit-node-child-count n))
                      (walk (treesit-node-child n i))))))
      (walk node)
      nil)))

(defun shexc-ts-mode--shape-body-brace-at (pos)
  "Return the position of the `{' of the shape body to fold at POS, or nil.
This is the `{' of the `{ ... }' shape body that
`shexc-ts-mode-toggle-fold' should act on.

If POS is inside a `shape_definition'/`inline_shape_definition', that
is the body in question.  Otherwise, if POS is elsewhere in an
enclosing `shape_expr_decl' -- e.g. on a `<#Shape> EXTENDS
@<#Other>' header line, before its `{ ... }' body -- fall back to
that declaration's first `{ ... }'."
  (if-let* ((def (rdf-core-ancestor-of-type
                   (treesit-node-at pos)
                   '("shape_definition" "inline_shape_definition")
                   t)))
      (shexc-ts-mode--def-open-brace def)
    (when-let* ((decl (shexc-ts-mode--decl-at pos)))
      (shexc-ts-mode--first-brace-in decl))))

;;;###autoload
(defun shexc-ts-mode-toggle-fold ()
  "Fold or unfold the `{ ... }' shape body at or around point.

Enables `hs-minor-mode' if it is not already on, then toggles hiding
of the nearest enclosing `{ ... }' (a `shape_definition' or
`inline_shape_definition')."
  (interactive)
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if-let* ((brace (shexc-ts-mode--shape-body-brace-at (point))))
      (save-excursion
        (goto-char brace)
        (hs-toggle-hiding))
    (user-error "No `{ ... }' shape body at point")))

(add-to-list 'hs-special-modes-alist
              '(shexc-ts-mode "{" "}" "#\\|/\\*" nil nil))

;;; Renaming shape labels

;;;###autoload
(defun shexc-ts-mode-rename-shape (new-label)
  "Rename the shape label at point to NEW-LABEL everywhere in the buffer.

Renames the `shape_expr_label' at point -- whether on the
`shape_expr_decl' that declares it (`<#Old> { ... }') or on any
`shape_ref' to it (`@<#Old>', `EXTENDS @<#Old>', `&<#Old>',
`start = @<#Old>') -- and every other occurrence of the same label,
including the declaration."
  (interactive
   (let ((old (shexc-ts-mode--label-at-point)))
     (unless old
       (user-error "No shape label at point"))
     (list (read-string (format "Rename %s to: " old) old))))
  (let ((old (shexc-ts-mode--label-at-point)))
    (unless old
      (user-error "No shape label at point"))
    (when (string= old new-label)
      (user-error "New label is the same as the old one"))
    (when (cl-some (lambda (n) (string= (treesit-node-text n t) new-label))
                    (shexc-ts-mode--all-labels))
      (user-error "Label %s is already in use" new-label))
    (let ((ranges
           (sort
            (mapcar (lambda (n) (cons (treesit-node-start n) (treesit-node-end n)))
                    (seq-filter (lambda (n) (string= (treesit-node-text n t) old))
                                 (shexc-ts-mode--all-labels)))
            (lambda (a b) (> (car a) (car b))))))
      (unless ranges
        (user-error "No occurrences of %s found" old))
      (dolist (range ranges)
        (goto-char (car range))
        (delete-region (car range) (cdr range))
        (insert new-label))
      (message "Renamed %d occurrence%s of %s to %s"
               (length ranges) (if (= (length ranges) 1) "" "s") old new-label))))

;;; Flymake: undefined shape references/prefixes, syntax errors,
;;; duplicate predicates

(defun shexc-ts-mode--prefix-name (text)
  "Return the part of prefixed-name TEXT before its `:'.
E.g. \"ex\" for both `ex:p1' and `ex:', and \"\" for `:p1'."
  (substring text 0 (string-search ":" text)))

;;;; Prefix maps: lookup and insertion

(defun shexc-ts-mode--prefix-map-names ()
  "`shexc-ts-mode-prefix-map' as a list of names, regardless of whether
that variable currently holds a single string or already a list."
  (rdf-core-prefix-map-names shexc-ts-mode-prefix-map))

(defun shexc-ts-mode--prefix-map-names-string ()
  "`shexc-ts-mode--prefix-map-names', joined for display in a message."
  (rdf-core-prefix-map-names-string shexc-ts-mode-prefix-map))

(defun shexc-ts-mode--prefix-map-lookup (prefix)
  "Return (IRI . MAP-NAME) for PREFIX: each of `shexc-ts-mode-prefix-map's
named maps (see `shexc-ts-mode--prefix-map-names') is tried in order,
and MAP-NAME is whichever one first has a matching entry.  Return nil
if PREFIX is empty or no active map has an entry for it."
  (rdf-core-prefix-map-lookup
   prefix shexc-ts-mode-prefix-maps shexc-ts-mode-prefix-map))

(defun shexc-ts-mode--prefixed-name-at (pos)
  "Return the `prefixed_name' node at POS, or nil if none."
  (treesit-parent-until
   (treesit-node-at pos)
   (lambda (n) (string= (treesit-node-type n) "prefixed_name"))
   t))

(defun shexc-ts-mode--declared-prefixes ()
  "Return the list of prefix names (e.g. \"ex\") the buffer already
declares via `PREFIX ex: <...>'."
  (mapcar (lambda (n) (shexc-ts-mode--prefix-name (treesit-node-text n t)))
          (treesit-query-capture
           (treesit-buffer-root-node)
           '((prefix_decl name: (pname_ns) @name)) nil nil t)))

(defun shexc-ts-mode--insert-prefix-decl (prefix iri)
  "Insert the line `PREFIX PREFIX: <IRI>' into the current buffer.
It is placed immediately after the buffer's last
`base_decl'/`prefix_decl'/`import_decl', or at `point-min' if the
buffer has none of those."
  (let ((directives (treesit-query-capture
                      (treesit-buffer-root-node)
                      '([(base_decl) (prefix_decl) (import_decl)] @d)
                      nil nil t)))
    (if directives
        (progn
          (goto-char (apply #'max (mapcar #'treesit-node-end directives)))
          (end-of-line)
          (if (eobp)
              (insert "\n")
            (forward-char 1)))
      (goto-char (point-min)))
    (insert (format "PREFIX %s: <%s>\n" prefix iri))))

;;;###autoload
(defun shexc-ts-mode-insert-prefix ()
  "Insert a `PREFIX' declaration for the prefixed name at point.

Looks up the prefix of the `prefixed_name' at point (e.g. \"ex\" in
`ex:Foo') in the active `shexc-ts-mode-prefix-map' and, if found there,
inserts `PREFIX ex: <IRI>' via `shexc-ts-mode--insert-prefix-decl'.  A
no-op (just a `message', not an error) if the buffer already declares
that prefix -- safe to invoke repeatedly, e.g. from a kbd macro fixing
up several `prefixed_name's at once, without piling up duplicate
declarations.

Signals a `user-error' if point is not on a prefixed name, or its
prefix has no entry in any of the active maps (see
`shexc-ts-mode-prefix-map') -- in the latter case, either add the
prefix to `shexc-ts-mode-prefix-maps' or declare it directly with
`PREFIX ex: <...>'."
  (interactive)
  (let ((node (shexc-ts-mode--prefixed-name-at (point))))
    (unless node
      (user-error "No prefixed name at point"))
    (let ((prefix (shexc-ts-mode--prefix-name (treesit-node-text node t))))
      (if (member prefix (shexc-ts-mode--declared-prefixes))
          (message "Prefix `%s:' is already declared" prefix)
        (let ((found (shexc-ts-mode--prefix-map-lookup prefix)))
          (unless found
            (user-error "Prefix `%s:' is not in any of the active prefix maps (%s)"
                        prefix (shexc-ts-mode--prefix-map-names-string)))
          (let ((iri (car found)) (map-name (cdr found)))
            (save-excursion (shexc-ts-mode--insert-prefix-decl prefix iri))
            (message "Inserted `PREFIX %s: <%s>' (from the %s map)" prefix iri map-name)))))))

;;;###autoload
(defun shexc-ts-mode-set-prefix-map (names)
  "Set the buffer-local active prefix map(s) to NAMES, for this buffer only.

NAMES is a key of `shexc-ts-mode-prefix-maps' (e.g. \"rdfa\") or a list
of keys (e.g. `(\"my-project\" \"rdfa\")', tried in order with the
first match winning -- see `shexc-ts-mode-prefix-map'); a plain string
is accepted too, for callers that only ever want one map.
`shexc-ts-mode-insert-prefix' and the \"Undefined prefix\" flymake
diagnostic then look up prefixes there.

Interactively, select one or more maps (`completing-read-multiple' --
type a comma between names to pick more than one).

This setting is buffer-local and does not persist when the buffer is
reopened -- to make a choice persistent for a whole project, set
`shexc-ts-mode-prefix-map' as a directory-local variable instead (see
`shexc-ts-mode-prefix-maps' for an example `.dir-locals.el')."
  (interactive
   (list (completing-read-multiple
          "Prefix map(s) (first match wins): "
          (mapcar #'car shexc-ts-mode-prefix-maps)
          nil t (shexc-ts-mode--prefix-map-names-string))))
  (when (stringp names) (setq names (list names)))
  (setq-local shexc-ts-mode-prefix-map (if (cdr names) names (car names)))
  (message "Active prefix map%s: %s"
           (if (cdr names) "s" "") (mapconcat #'identity names ", ")))

(defun shexc-ts-mode--flymake-undefined-prefixes ()
  "Return `:error' diagnostics for `prefixed_name's with an undeclared prefix.
I.e. a `prefixed_name' (`ex:p1', `ex:', `x:Foo', a value-set IRI, a
datatype, ...) whose prefix has no matching `PREFIX' declaration.

When the prefix has an entry in the active `shexc-ts-mode-prefix-map',
the diagnostic message names the IRI that `shexc-ts-mode-insert-prefix'
would declare for it."
  (let ((declared (shexc-ts-mode--declared-prefixes)))
    (delq nil
          (mapcar
           (lambda (n)
             (let* ((text (treesit-node-text n t))
                    (prefix (shexc-ts-mode--prefix-name text)))
               (unless (member prefix declared)
                 (let ((found (shexc-ts-mode--prefix-map-lookup prefix)))
                   (flymake-make-diagnostic
                    (current-buffer)
                    (treesit-node-start n) (treesit-node-end n)
                    :error
                    (if found
                        (format "Undefined prefix %s: `M-x shexc-ts-mode-insert-prefix' \
adds `PREFIX %s: <%s>' from the %s map"
                                prefix prefix (car found) (cdr found))
                      (format "Undefined prefix %s:" prefix)))))))
           (treesit-query-capture
            (treesit-buffer-root-node) '((prefixed_name) @n) nil nil t)))))

(defun shexc-ts-mode--flymake-syntax-errors ()
  "Return `:error' diagnostics for tree-sitter ERROR nodes.
I.e. text the parser could not make sense of, e.g. the `p3' in
`ex: p3 . ;' \(a valid `ex:' prefixed name with empty local part,
followed by an unexpected token\)."
  (let (seen diagnostics)
    (dolist (n (treesit-query-capture
                 (treesit-buffer-root-node) '((ERROR) @e) nil nil t))
      (let ((range (cons (treesit-node-start n) (treesit-node-end n))))
        (unless (member range seen)
          (push range seen)
          (push (flymake-make-diagnostic
                 (current-buffer) (car range) (cdr range)
                 :error "Syntax error")
                diagnostics))))
    diagnostics))

(defun shexc-ts-mode--flymake-undefined-shapes ()
  "Return `:error' diagnostics for undefined shape references.
I.e. shape references with no matching `shape_expr_decl', e.g.
`@<#Typo>' or `EXTENDS @<#Typo>'.

Compares labels by their fully resolved IRI (via
`shexc-shexj-resolve-label', against a BASE/PREFIX context built once
for the whole buffer), not by raw source text -- so e.g. a shape
declared `<http://a.example/TLabor>' and referenced `@<TLabor>' with a
matching `BASE <http://a.example/>' in scope are correctly recognized
as the same shape, never flagged as undefined just because one
occurrence happened to be written more/less abbreviated than the
other."
  (let* ((ctx (shexc-shexj-buffer-directive-ctx))
         (decl-labels
          (mapcar (lambda (n) (shexc-shexj-resolve-label ctx n))
                  (shexc-ts-mode--query-labels
                   '((shape_expr_decl label: (shape_expr_label) @label))))))
    (delq nil
          (mapcar
           (lambda (ref)
             (unless (member (shexc-shexj-resolve-label ctx ref) decl-labels)
               (flymake-make-diagnostic
                (current-buffer)
                (treesit-node-start ref) (treesit-node-end ref)
                :error
                (format "Undefined shape %s" (treesit-node-text ref t)))))
           (shexc-ts-mode--query-labels
            '((shape_ref label: (shape_expr_label) @label)))))))

(defun shexc-ts-mode--flymake-duplicate-predicates ()
  "Return `:note' diagnostics for predicates repeated in a disjunct group.
I.e. predicates that occur more than once as direct elements of the
same EachOf/OneOf-disjunct group \(`group_triple_expr'\).

Repeated predicates are not an error -- ShEx allows them, e.g. with
different cardinalities or value constraints -- but they are often a
sign of a typo or a missing `|'/cardinality, so they are flagged here
as `:note's for orientation, one per duplicated `triple_constraint'."
  (let (diagnostics)
    (dolist (group (treesit-query-capture
                     (treesit-buffer-root-node)
                     '((group_triple_expr) @g) nil nil t))
      (let ((by-predicate (make-hash-table :test #'equal)))
        (dolist (element (treesit-node-children group))
          (when (and (equal (treesit-node-field-name element) "element")
                     (string= (treesit-node-type element) "triple_constraint"))
            (when-let* ((pred (treesit-node-child-by-field-name element "predicate")))
              (push element (gethash (treesit-node-text pred t) by-predicate)))))
        (maphash
         (lambda (predicate-text elements)
           (when (cdr elements)
             (dolist (element elements)
               (let ((pred (treesit-node-child-by-field-name element "predicate")))
                 (push (flymake-make-diagnostic
                        (current-buffer)
                        (treesit-node-start pred) (treesit-node-end pred)
                        :note
                        (format "Predicate %s appears %d times in this group"
                                predicate-text (length elements)))
                       diagnostics)))))
         by-predicate)))
    diagnostics))

;;;###autoload
(defun shexc-ts-mode-flymake (report-fn &rest _args)
  "Flymake backend for `shexc-ts-mode'.

Reports `:error' diagnostics for shape references with no matching
declaration (see `shexc-ts-mode--flymake-undefined-shapes'),
prefixed names with no matching `PREFIX' declaration (see
`shexc-ts-mode--flymake-undefined-prefixes'), and text the parser
could not make sense of (see `shexc-ts-mode--flymake-syntax-errors');
and `:note' diagnostics for predicates repeated within the same
EachOf/OneOf-disjunct group (see
`shexc-ts-mode--flymake-duplicate-predicates').

REPORT-FN is Flymake's callback; see `flymake-diagnostic-functions'."
  (funcall report-fn
           (append (shexc-ts-mode--flymake-undefined-shapes)
                   (shexc-ts-mode--flymake-undefined-prefixes)
                   (shexc-ts-mode--flymake-syntax-errors)
                   (shexc-ts-mode--flymake-duplicate-predicates))))

;;; Feature menu (transient)

(defun shexc-ts-mode--menu-desc (label command &optional width)
  "Return LABEL annotated with COMMAND's key binding in `shexc-ts-mode-map'.
LABEL is left-justified to WIDTH (default 36) before the key binding,
so that bindings line up within a single `transient' column; callers
sharing a column should pass the same WIDTH.  For use as a `transient'
suffix description."
  (let ((key (where-is-internal command shexc-ts-mode-map t)))
    (if key
        (format (format "%%-%ds %%s" (or width 36)) label (key-description key))
      label)))

(transient-define-prefix shexc-ts-mode-highlight-menu ()
  "Persistent submenu of `shexc-ts-mode-highlight-reachable-mode' options.

Unlike a normal `shexc-ts-mode-menu' suffix (which exits the menu after
one selection), every toggle here is `:transient t', so the menu stays
open across multiple selections -- flip several options in a row and
watch the buffer's highlighting update live after each one, rather
than reopening the menu (`C-c C-c') between every toggle.  \"q\" exits
all the way back to normal editing (not just back to
`shexc-ts-mode-menu', the way `C-g'/`ESC' would -- see
`transient-quit-all')."
  ["Highlighting"
   ("l" shexc-ts-mode-highlight-reachable-mode
    :transient t
    :description
    (lambda ()
      (shexc-ts-mode--menu-desc
       (format "Toggle highlighting on/off [%s]"
               (if shexc-ts-mode-highlight-reachable-mode "ON" "off"))
       'shexc-ts-mode-highlight-reachable-mode)))
   ("h" shexc-ts-mode-toggle-highlight-reachable-current
    :transient t
    :description
    (lambda ()
      (format "Highlight current shape (under cursor) [%s]"
              (if shexc-ts-mode-highlight-reachable-include-current "X" " "))))
   ("e" shexc-ts-mode-toggle-highlight-reachable-non-extended
    :transient t
    :description
    (lambda ()
      (format "Highlight shapes reachable from current shape [%s]"
              (if shexc-ts-mode-highlight-reachable-include-non-extended "X" " "))))
   ("x" shexc-ts-mode-toggle-highlight-reachable-extended
    :transient t
    :description
    (lambda ()
      (format "Highlight shapes extended by current shape [%s]"
              (if shexc-ts-mode-highlight-reachable-include-extended "X" " "))))
   ("t" shexc-ts-mode-toggle-highlight-reachable-extends-trumps-reachable
    :transient t
    :description
    (lambda ()
      (format "Extends trumps reachable [%s]"
              (if shexc-ts-mode-highlight-reachable-extends-trumps-reachable "X" " "))))
   ("p" shexc-ts-mode-toggle-highlight-reachable-predicates
    :transient t
    :description
    (lambda ()
      (format "Highlight predicates of highlighted shapes [%s]"
              (if shexc-ts-mode-highlight-reachable-include-predicates "X" " "))))
   ("q" "Done" transient-quit-all)])

(transient-define-prefix shexc-ts-mode-prefix-menu ()
  "Submenu of `PREFIX'-declaration commands -- kept out of
`shexc-ts-mode-menu' itself (reached there via a single \"p\" entry in
the \"Edit\" column), the same way `shexc-ts-mode-convert-menu' is."
  ["Prefix maps"
   ("p" shexc-ts-mode-insert-prefix
    :description
    (lambda ()
      (shexc-ts-mode--menu-desc
       (format "Insert `PREFIX' for prefix at point (%s map)"
               (shexc-ts-mode--prefix-map-names-string))
       'shexc-ts-mode-insert-prefix)))
   ("m" shexc-ts-mode-set-prefix-map
    :description
    (lambda ()
      (shexc-ts-mode--menu-desc
       (format "Switch active prefix map (currently %s)"
               (shexc-ts-mode--prefix-map-names-string))
       'shexc-ts-mode-set-prefix-map)))
   ("q" "Done" transient-quit-all)])

(transient-define-prefix shexc-ts-mode-menu ()
  "Feature menu for `shexc-ts-mode', showing live keybindings."
  [["Navigate"
    ("h" shexc-ts-mode-highlight-menu
     :description
     (lambda ()
       (format "Highlighting options...    [%s]"
               (if shexc-ts-mode-highlight-reachable-mode "ON" "off"))))
    ("f" shexc-ts-mode-toggle-fold
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Fold/unfold shape body"
                 'shexc-ts-mode-toggle-fold 24)))
    (:info "C-M-a / C-M-e    Prev/next shape")
    (:info "C-M-f / C-M-b    Forward/back expr")
    (:info "M-. / M-, / M-?  push/pop/refs to")]
   ["Edit"
    ("u" shexc-ts-mode-unwrap-shape
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Unwrap `<predicate> { ... }'"
                 'shexc-ts-mode-unwrap-shape 29)))
    ("w" shexc-ts-mode-wrap-in-shape
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Wrap in `<predicate> { ... }'"
                 'shexc-ts-mode-wrap-in-shape 29)))
    ("r" shexc-ts-mode-rename-shape
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Rename shape label refs"
                 'shexc-ts-mode-rename-shape 29)))
    ("k" shexc-ts-mode-toggle-comment-style
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "`#' / `/* */' comment style"
                 'shexc-ts-mode-toggle-comment-style 29)))
    ("p" shexc-ts-mode-prefix-menu
     :description
     (lambda ()
       (format "Prefix map options...         (%s)"
               (shexc-ts-mode--prefix-map-names-string))))]])

;;; Major mode

;;;###autoload
(define-derived-mode shexc-ts-mode prog-mode "ShEx[ts]"
  "Major mode for editing ShExC documents, powered by tree-sitter.

\\{shexc-ts-mode-map}"
  :syntax-table shexc-ts-mode--syntax-table
  (unless (treesit-ready-p 'shexc)
    (unless noninteractive
      (when (y-or-n-p "Tree-sitter grammar for `shexc' not found.  \
Install it now? (requires git and a C compiler on exec-path) ")
        (shexc-ts-mode-install-grammar)))
    (unless (treesit-ready-p 'shexc)
      (error "Tree-sitter grammar for `shexc' is not available; \
run M-x shexc-ts-mode-install-grammar")))

  (treesit-parser-create 'shexc)

  ;; disambiguate `#' as comment-starter vs. in-IRI fragment separator
  ;; (see `shexc-ts-mode--syntax-propertize')
  (setq-local syntax-propertize-function #'shexc-ts-mode--syntax-propertize)

  ;; comments: "# ..." (default) or "/* ... */" (see
  ;; `shexc-ts-mode-toggle-comment-style', bound to `C-c C-k' as in
  ;; `c-ts-mode'). The skip regexps recognize *both* styles so that
  ;; `comment-dwim'/`uncomment-region' can find and remove existing
  ;; comments regardless of which style is currently active.
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (or (+ "#") (seq "/" (+ "*")))
                                     (* (syntax whitespace))))
  (setq-local comment-end-skip
              (rx (* (syntax whitespace))
                  (group (or (syntax comment-end)
                             (seq (+ "*") "/")))))
  (setq-local comment-multi-line t)
  (define-key shexc-ts-mode-map [remap comment-dwim] #'shexc-ts-mode-comment-dwim)
  (define-key shexc-ts-mode-map (kbd "C-c C-k") #'shexc-ts-mode-toggle-comment-style)

  ;; structural editing: unwrap/wrap/rename `<predicate> { ... }' shapes
  (define-key shexc-ts-mode-map (kbd "C-c C-u") #'shexc-ts-mode-unwrap-shape)
  (define-key shexc-ts-mode-map (kbd "C-c C-w") #'shexc-ts-mode-wrap-in-shape)
  (define-key shexc-ts-mode-map (kbd "C-c C-r") #'shexc-ts-mode-rename-shape)

  ;; insert a `PREFIX' declaration for the prefix at point, looked up
  ;; in the active `shexc-ts-mode-prefix-map'
  (define-key shexc-ts-mode-map (kbd "C-c C-p") #'shexc-ts-mode-insert-prefix)

  ;; live "germane shapes" highlighting, following point
  (define-key shexc-ts-mode-map (kbd "C-c C-l") #'shexc-ts-mode-highlight-reachable-mode)

  ;; folding `{ ... }' shape bodies
  (define-key shexc-ts-mode-map (kbd "C-c C-f") #'shexc-ts-mode-toggle-fold)
  (hs-minor-mode 1)

  ;; Magit-style feature menu
  (define-key shexc-ts-mode-map (kbd "C-c C-c") #'shexc-ts-mode-menu)

  ;; indentation
  (setq-local treesit-simple-indent-rules shexc-ts-mode--indent-rules)
  (setq-local indent-tabs-mode nil)
  (setq-local electric-indent-chars
              (append '(?\{ ?\} ?\[ ?\] ?\( ?\)) electric-indent-chars))

  ;; font-lock
  (setq-local treesit-font-lock-settings shexc-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list shexc-ts-mode--font-lock-feature-list)

  ;; imenu
  (setq-local treesit-simple-imenu-settings shexc-ts-mode--imenu-settings)

  ;; navigation: structural defun-nav (`C-M-a'/`C-M-e') over
  ;; `shape_expr_decl's, and smart sexp (`C-M-f'/`C-M-b', `C-M-t',
  ;; `kill-sexp', etc.) over shape-expression/triple-expression
  ;; branches -- see `shexc-ts-mode--sexp-node-types'.
  (setq-local treesit-thing-settings
              `((shexc
                 (defun "\\`shape_expr_decl\\'")
                 (sexp ,(concat "\\`"
                                 (regexp-opt shexc-ts-mode--sexp-node-types)
                                 "\\'")))))

  ;; breadcrumb: `add-log-current-defun'/`which-function-mode' show
  ;; "<Shape> > predicate > ..." for point's position
  (setq-local add-log-current-defun-function #'shexc-ts-mode--current-defun-name)

  ;; following shape references
  (add-hook 'xref-backend-functions #'shexc-ts-mode--xref-backend nil t)

  ;; diagnostics: undefined shape references and duplicate predicates
  (add-hook 'flymake-diagnostic-functions #'shexc-ts-mode-flymake nil t)
  (flymake-mode 1)

  ;; Optional companion feature (in-place ShExC <-> ShExJ/ShExR
  ;; conversion, `C-c C-v'): pulled in here, lazily, on first use of the
  ;; mode -- not via a top-level `require' above, since
  ;; shexc-ts-mode-convert.el itself `require's `shexc-ts-mode', which
  ;; would be a circular `require' if attempted while this file is still
  ;; being loaded.  By the time a buffer actually turns on `shexc-ts-mode',
  ;; this file has already finished loading and called `provide', so the
  ;; require below is safe; NOERROR so the base mode still works for
  ;; anyone who only fetched shexc-ts-mode.el.
  (require 'shexc-ts-mode-convert nil t)

  (treesit-major-mode-setup))

;; Prefer the tree-sitter mode for .shex(c) files when its grammar is
;; available; `shexc-mode' remains the fallback otherwise.  `require'
;; `treesit' first since this form runs from the autoloads file, before
;; `treesit-ready-p' would otherwise be defined.
;;;###autoload
(progn
  (require 'treesit)
  (when (treesit-ready-p 'shexc t)
    (add-to-list 'auto-mode-alist '("\\.shexc?\\'" . shexc-ts-mode))))

(provide 'shexc-ts-mode)

;;; shexc-ts-mode.el ends here
