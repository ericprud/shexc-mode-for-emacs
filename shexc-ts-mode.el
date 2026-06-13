;;; shexc-ts-mode.el --- Tree-sitter major mode for ShExC -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Keywords: languages

;; This mode is a tree-sitter based companion to `shexc-mode' (see
;; shexc-mode.el), built on the grammar at
;; https://github.com/ericprud/tree-sitter-shexc
;;
;; It provides:
;; - syntax highlighting (`treesit-font-lock-rules')
;; - structure-aware indentation (`treesit-simple-indent-rules')
;; - line/block commenting (M-; via `comment-dwim')
;; - imenu index of shape declarations
;; - "jump to shape definition" / "find references to shape" via `xref'
;;   (M-. / M-, / M-?), resolving `@<#Shape>', `EXTENDS @<#Shape>',
;;   `&<#Shape>' and `start = @<#Shape>' references to their
;;   `shape_expr_decl'.
;;
;; For documentation on ShExC, see:
;; https://shex.io/shex-semantics/#shexc

;;; Setup:
;;
;; (require 'shexc-ts-mode)
;;
;; ;; tell Emacs where to find the compiled grammar (built with
;; ;; `tree-sitter build -o ~/.emacs.d/tree-sitter/libtree-sitter-shexc.dylib /path/to/tree-sitter-shexc')
;; (add-to-list 'treesit-extra-load-path (expand-file-name "~/.emacs.d/tree-sitter/"))
;;
;; (add-to-list 'auto-mode-alist '("\\.shexc?\\'" . shexc-ts-mode))

;;; Code:

(require 'treesit)
(require 'xref)
(require 'cl-lib)
(require 'pcase)
(require 'hideshow)
(require 'flymake)
(require 'transient)

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

;; defined by the optional, separate `shex-manifest-browser' package;
;; forward-declared so `shexc-ts-mode-menu' can offer it when present
;; (guarded at both compile and run time by
;; `shexc-ts-mode--manifest-browser-loaded-p').
(defvar shex-manifest-browser-highlight-extended-predicates)

(defgroup shexc nil
  "Major mode for editing ShExC documents with tree-sitter."
  :group 'languages)

(defcustom shexc-ts-mode-indent-offset 2
  "Number of columns for each indentation step in `shexc-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'shexc)

;;; Syntax table

(defvar shexc-ts-mode--syntax-table nil
  "Syntax table for `shexc-ts-mode'.")
;; Use `setq' (not just `defvar') so that `load-file'/`eval-buffer'
;; always rebuilds the table -- `defvar' is a no-op when the variable
;; is already bound.
(setq shexc-ts-mode--syntax-table
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
    table))

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
  "Give `#' characters that are not `#'-comment starters (e.g.
the fragment separator in `<#Shape>' IRI references) a neutral
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
(and so cannot strip) a pre-existing `/* ... */' comment, while
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
   '((["." "|" "&" "@" "^" "~" "-" "="]
      ) @font-lock-operator-face
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

(defun shexc-ts-mode--label-line-summary (node)
  "Return a trimmed one-line summary of the source line containing NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (string-trim (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))

(defun shexc-ts-mode--xref-make (node)
  "Build an `xref-item' pointing at the shape label NODE."
  (xref-make (shexc-ts-mode--label-line-summary node)
             (xref-make-buffer-location (current-buffer) (treesit-node-start node))))

(defun shexc-ts-mode--matching-xrefs (nodes identifier)
  "Build `xref-item's for the nodes in NODES whose text equals IDENTIFIER."
  (delq nil
        (mapcar (lambda (node)
                  (when (string= (treesit-node-text node t) identifier)
                    (shexc-ts-mode--xref-make node)))
                nodes)))

(defun shexc-ts-mode--all-labels ()
  "Return all shape-label nodes in the buffer (declarations and references)."
  (append
   (shexc-ts-mode--query-labels
    '((shape_expr_decl label: (shape_expr_label) @label)))
   (shexc-ts-mode--query-labels
    '((shape_ref label: (shape_expr_label) @label)))))

;;;###autoload
(defun shexc-ts-mode--xref-backend ()
  "Return the xref backend symbol for `shexc-ts-mode'."
  'shexc-ts-mode)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql shexc-ts-mode)))
  (shexc-ts-mode--label-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql shexc-ts-mode)))
  (delete-dups (mapcar (lambda (n) (treesit-node-text n t))
                       (shexc-ts-mode--all-labels))))

(cl-defmethod xref-backend-definitions ((_backend (eql shexc-ts-mode)) identifier)
  (shexc-ts-mode--matching-xrefs
   (shexc-ts-mode--query-labels
    '((shape_expr_decl label: (shape_expr_label) @label)))
   identifier))

(cl-defmethod xref-backend-references ((_backend (eql shexc-ts-mode)) identifier)
  (shexc-ts-mode--matching-xrefs
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

(defun shexc-ts-mode--ancestor-of-type (node types &optional include-node)
  "Return the closest ancestor of NODE whose type is a member of TYPES.
If INCLUDE-NODE is non-nil, NODE itself is considered first."
  (treesit-parent-until
   node
   (lambda (n) (member (treesit-node-type n) types))
   include-node))

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
;; `shexc-ts-mode-highlight-extended-shapes' highlights every shape
;; "germane" to a focal `shape_expr_decl': itself, plus every shape
;; transitively reachable from its shape expression via EXTENDS, a
;; sole shape reference (e.g. `<Contact1> @<Contact>'), AND, OR, and
;; NOT.  Each reachable shape is classified by the *strength* of its
;; strongest reaching path:
;;
;; - `shexc-ts-mode--strength-required' -- the focal shape, anything
;;   it EXTENDS, any AND-conjunct, or a sole shape reference: every
;;   node conforming to the focal shape also conforms to this one.
;; - `shexc-ts-mode--strength-negated' -- reachable only inside a NOT:
;;   conformance to the focal shape requires *not* conforming to this
;;   one.
;; - `shexc-ts-mode--strength-optional' -- reachable only via one
;;   branch of an OR (and not via every branch): conformance to the
;;   focal shape *may* entail conformance to this one, depending on
;;   which OR-branch holds.
;;
;; "Required" preempts "negated"/"optional": if a shape is reachable
;; via both a required path and a weaker one, it is highlighted as
;; required.  Likewise, if every branch of an OR reaches the same
;; target shape, that target is not downgraded to "optional" by the
;; OR -- see `shexc-ts-mode--merge-or-branches'.

(defface shexc-ts-mode-extends-label-face
  '((t :inherit highlight :weight bold))
  "Face for ShapeDecl labels of shapes required by the focused shape
\(directly, via EXTENDS, or via AND -- see
`shexc-ts-mode--strength-required')."
  :group 'shexc)

(defface shexc-ts-mode-extends-label-face-optional
  '((t :inherit shadow :weight bold))
  "Face for ShapeDecl labels of shapes only optionally relevant to the
focused shape -- reachable via one branch of an OR but not every
branch (see `shexc-ts-mode--strength-optional')."
  :group 'shexc)

(defface shexc-ts-mode-extends-label-face-negated
  '((t :inherit error :weight bold))
  "Face for ShapeDecl labels of shapes the focused shape requires
non-conformance to -- reachable only inside a NOT (see
`shexc-ts-mode--strength-negated')."
  :group 'shexc)

(defface shexc-ts-mode-extends-predicate-face
  '((t :inherit font-lock-warning-face))
  "Face for predicates in shapes required by the focused shape
\(directly, via EXTENDS, or via AND -- see
`shexc-ts-mode--strength-required')."
  :group 'shexc)

(defface shexc-ts-mode-extends-predicate-face-optional
  '((t :inherit shadow))
  "Face for predicates in shapes only optionally relevant to the
focused shape -- reachable via one branch of an OR but not every
branch (see `shexc-ts-mode--strength-optional')."
  :group 'shexc)

(defface shexc-ts-mode-extends-predicate-face-negated
  '((t :inherit error))
  "Face for predicates in shapes the focused shape requires
non-conformance to -- reachable only inside a NOT (see
`shexc-ts-mode--strength-negated')."
  :group 'shexc)

(defcustom shexc-ts-mode-highlight-extends-include-predicates t
  "Whether `shexc-ts-mode-highlight-extends-mode' also highlights
predicates (not just ShapeDecl labels) of shapes germane to the
shape at point.  See `shexc-ts-mode-highlight-extended-shapes'."
  :type 'boolean
  :group 'shexc)

(defvar-local shexc-ts-mode--extends-overlays nil
  "Overlays placed by `shexc-ts-mode-highlight-extended-shapes'.")

(defun shexc-ts-mode-clear-extends-overlays ()
  "Remove all overlays added by `shexc-ts-mode-highlight-extended-shapes'."
  (interactive)
  (mapc #'delete-overlay shexc-ts-mode--extends-overlays)
  (setq shexc-ts-mode--extends-overlays nil))

(defun shexc-ts-mode--decl-at (pos)
  "Return the `shape_expr_decl' node enclosing POS, or nil."
  (treesit-parent-until
   (treesit-node-at pos)
   (lambda (n) (string= (treesit-node-type n) "shape_expr_decl"))
   t))

(defconst shexc-ts-mode--strength-optional 0
  "Reference strength of a shape reachable only via one branch of an
OR (and not via every branch): the focal shape's conformance only
*may* entail conformance to it.")

(defconst shexc-ts-mode--strength-negated 1
  "Reference strength of a shape reachable only inside a NOT: the
focal shape's conformance entails *non*-conformance to it.")

(defconst shexc-ts-mode--strength-required 2
  "Reference strength of a shape reachable via EXTENDS, an
AND-conjunct, or as a sole shape reference: the focal shape's
conformance always entails conformance to it.")

(defun shexc-ts-mode--label-face (strength)
  "Return the ShapeDecl-label face for reference STRENGTH."
  (cond ((>= strength shexc-ts-mode--strength-required)
         'shexc-ts-mode-extends-label-face)
        ((= strength shexc-ts-mode--strength-negated)
         'shexc-ts-mode-extends-label-face-negated)
        (t 'shexc-ts-mode-extends-label-face-optional)))

(defun shexc-ts-mode--predicate-face (strength)
  "Return the predicate face for reference STRENGTH."
  (cond ((>= strength shexc-ts-mode--strength-required)
         'shexc-ts-mode-extends-predicate-face)
        ((= strength shexc-ts-mode--strength-negated)
         'shexc-ts-mode-extends-predicate-face-negated)
        (t 'shexc-ts-mode-extends-predicate-face-optional)))

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
  "Return the direct `extension' children of DEF-NODE, a
`shape_definition' or `inline_shape_definition' node (unwrapping the
former's single `inline_shape_definition' child first)."
  (when (string= (treesit-node-type def-node) "shape_definition")
    (setq def-node (treesit-node-child def-node 0)))
  (let (result)
    (dotimes (i (treesit-node-child-count def-node))
      (let ((child (treesit-node-child def-node i)))
        (when (string= (treesit-node-type child) "extension")
          (push child result))))
    (nreverse result)))

(defun shexc-ts-mode--strongest-per-label (refs)
  "Collapse REFS, a list of (LABEL-TEXT . STRENGTH), to one entry per
LABEL-TEXT, keeping each label's strongest (maximum) STRENGTH."
  (let (result)
    (dolist (ref refs)
      (let ((existing (assoc (car ref) result)))
        (if existing
            (setcdr existing (max (cdr existing) (cdr ref)))
          (push (cons (car ref) (cdr ref)) result))))
    result))

(defun shexc-ts-mode--merge-or-branches (left right strength)
  "Merge the `shexc-ts-mode--collect-shape-refs' results LEFT and
RIGHT of an OR's two branches (each computed at branch-root strength
`shexc-ts-mode--strength-required') into the OR's contribution at its
own incoming STRENGTH.

A label reachable through *both* branches keeps the weaker of its two
per-branch strengths (no OR-downgrade, since every branch entails it);
a label reachable through only one branch is downgraded to
`shexc-ts-mode--strength-optional'.  Either way the result is then
capped at STRENGTH, since the OR itself may be reached via a NOT or
via another OR-branch."
  (let ((left-map (shexc-ts-mode--strongest-per-label left))
        (right-map (shexc-ts-mode--strongest-per-label right))
        result)
    (dolist (entry left-map)
      (let* ((rstr (cdr (assoc (car entry) right-map)))
             (combined (if rstr (min (cdr entry) rstr)
                         shexc-ts-mode--strength-optional)))
        (push (cons (car entry) (min strength combined)) result)))
    (dolist (entry right-map)
      (unless (assoc (car entry) left-map)
        (push (cons (car entry) (min strength shexc-ts-mode--strength-optional))
              result)))
    (nreverse result)))

(defun shexc-ts-mode--collect-shape-refs (node strength)
  "Collect (LABEL-TEXT . STRENGTH) pairs for each shape reference
\(`@<Label>' or EXTENDS target) reachable from NODE, a
`shape_or'/`shape_and'/`shape_not'/`shape_atom' or one of their
`inline_*' counterparts.  STRENGTH is the strength of the path from
the traversal's root down to NODE; see
`shexc-ts-mode--strength-required'/`-negated'/`-optional' and
`shexc-ts-mode--merge-or-branches' for how OR/AND/NOT adjust it along
the way.  Triple expressions inside `{ ... }' are not descended into."
  (pcase (treesit-node-type node)
    ((or "shape_or" "inline_shape_or")
     (let ((left (shexc-ts-mode--collect-shape-refs
                   (treesit-node-child-by-field-name node "left")
                   shexc-ts-mode--strength-required))
           (right (shexc-ts-mode--collect-shape-refs
                   (treesit-node-child-by-field-name node "right")
                   shexc-ts-mode--strength-required)))
       (shexc-ts-mode--merge-or-branches left right strength)))
    ((or "shape_and" "inline_shape_and")
     (append (shexc-ts-mode--collect-shape-refs
              (treesit-node-child-by-field-name node "left") strength)
             (shexc-ts-mode--collect-shape-refs
              (treesit-node-child-by-field-name node "right") strength)))
    ((or "shape_not" "inline_shape_not")
     (shexc-ts-mode--collect-shape-refs
      (treesit-node-child-by-field-name node "shape_expr")
      (min strength shexc-ts-mode--strength-negated)))
    ((or "shape_atom" "inline_shape_atom")
     (let ((inner (treesit-node-child-by-field-name node "shape_expr")))
       (when inner
         (cond
          ((string= (treesit-node-type inner) "shape_ref")
           (list (cons (treesit-node-text
                        (treesit-node-child-by-field-name inner "label") t)
                       strength)))
          ((member (treesit-node-type inner)
                   '("shape_definition" "inline_shape_definition"))
           (mapcan (lambda (ext)
                     (shexc-ts-mode--collect-shape-refs
                      (treesit-node-child-by-field-name ext "shape_expr")
                      strength))
                   (shexc-ts-mode--direct-extensions inner)))
          (t
           ;; a parenthesized sub-expression: `( ... )' wraps its
           ;; contents in another shape_or/shape_and/shape_not/shape_atom
           ;; (or `inline_*' variant), which the cases above handle
           (shexc-ts-mode--collect-shape-refs inner strength))))))
    (_ nil)))

(defun shexc-ts-mode--make-predicate-overlays (decl strength)
  "Overlay each predicate in DECL with the face for STRENGTH, push the
new overlays onto `shexc-ts-mode--extends-overlays', and return them."
  (let (overlays)
    (dolist (pred-node (shexc-ts-mode--decl-predicate-nodes decl))
      (let ((ov (make-overlay (treesit-node-start pred-node)
                              (treesit-node-end pred-node))))
        (overlay-put ov 'face (shexc-ts-mode--predicate-face strength))
        (push ov shexc-ts-mode--extends-overlays)
        (push ov overlays)))
    overlays))

(defun shexc-ts-mode-highlight-extended-shapes (pos &optional include-predicates)
  "Highlight shapes germane to the `shape_expr_decl' at POS.
Places an overlay on the ShapeDecl label of each shape transitively
reachable from the one at POS via EXTENDS, a sole shape reference
\(e.g. `<Contact1> @<Contact>'), AND, OR, or NOT -- with cycle/duplicate
detection -- using `shexc-ts-mode-extends-label-face',
`shexc-ts-mode-extends-label-face-negated', or
`shexc-ts-mode-extends-label-face-optional' according to the strongest
\(`shexc-ts-mode--strength-required' > `-negated' > `-optional')
strength of any path that reaches it; see
`shexc-ts-mode--collect-shape-refs' and
`shexc-ts-mode--merge-or-branches' for exactly how that strength is
computed.
When INCLUDE-PREDICATES is non-nil, also overlays all predicates
inside the `shape_expr_decl' at POS itself (always at
`shexc-ts-mode-extends-predicate-face', since it is always
\"required\") and inside each reachable shape, with the predicate face
matching that shape's strength.
Clears any overlays from a previous call first.
Returns the list of label texts of the reachable shapes, in the order
they were first discovered (breadth-first)."
  (shexc-ts-mode-clear-extends-overlays)
  (let* ((decl (shexc-ts-mode--decl-at pos))
         (best (make-hash-table :test 'equal))
         (label-overlays (make-hash-table :test 'equal))
         (predicate-overlays (make-hash-table :test 'equal))
         (queue (and decl (list (cons decl shexc-ts-mode--strength-required))))
         result)
    (when (and decl include-predicates)
      (shexc-ts-mode--make-predicate-overlays decl shexc-ts-mode--strength-required))
    (while queue
      (pcase-let* ((`(,cur-decl . ,cur-strength) (pop queue))
                   (refs (shexc-ts-mode--collect-shape-refs
                          (treesit-node-child-by-field-name cur-decl "shape_expr")
                          cur-strength)))
        (dolist (ref (shexc-ts-mode--strongest-per-label refs))
          (let* ((label-text (car ref))
                 (ref-strength (cdr ref))
                 (prev (gethash label-text best)))
            (when (or (not prev) (> ref-strength prev))
              (puthash label-text ref-strength best)
              (unless prev (push label-text result))
              (let ((ext-decl (shexc-ts-mode--decl-for-label-text label-text)))
                (when ext-decl
                  (let ((ov (gethash label-text label-overlays))
                        (label-node (treesit-node-child-by-field-name ext-decl "label")))
                    (if ov
                        (overlay-put ov 'face (shexc-ts-mode--label-face ref-strength))
                      (when label-node
                        (setq ov (make-overlay (treesit-node-start label-node)
                                               (treesit-node-end label-node)))
                        (overlay-put ov 'face (shexc-ts-mode--label-face ref-strength))
                        (push ov shexc-ts-mode--extends-overlays)
                        (puthash label-text ov label-overlays))))
                  (when include-predicates
                    (let ((pred-ovs (gethash label-text predicate-overlays)))
                      (if pred-ovs
                          (dolist (ov pred-ovs)
                            (overlay-put ov 'face (shexc-ts-mode--predicate-face ref-strength)))
                        (puthash label-text
                                 (shexc-ts-mode--make-predicate-overlays ext-decl ref-strength)
                                 predicate-overlays))))
                  (push (cons ext-decl ref-strength) queue))))))))
    (nreverse result)))

(defvar-local shexc-ts-mode--highlight-extends-last-decl-start nil
  "Start position of the `shape_expr_decl' last highlighted by
`shexc-ts-mode--highlight-extends-update', or the symbol `none' if the
last update found point outside any `shape_expr_decl' (and so cleared
the overlays).  Used to avoid recomputing the highlight on every
command when point hasn't left the current shape.")

(defun shexc-ts-mode--highlight-extends-update ()
  "Update the extended-shape overlays for the `shape_expr_decl' at
point, if it has changed since the last update; clear the overlays
when point is outside any `shape_expr_decl'.  Intended for
`post-command-hook' via `shexc-ts-mode-highlight-extends-mode'."
  (let* ((decl (shexc-ts-mode--decl-at (point)))
         (key (if decl (treesit-node-start decl) 'none)))
    (unless (eq key shexc-ts-mode--highlight-extends-last-decl-start)
      (setq shexc-ts-mode--highlight-extends-last-decl-start key)
      (if decl
          (shexc-ts-mode-highlight-extended-shapes
           (point) shexc-ts-mode-highlight-extends-include-predicates)
        (shexc-ts-mode-clear-extends-overlays)))))

;;;###autoload
(define-minor-mode shexc-ts-mode-highlight-extends-mode
  "Toggle live highlighting of shapes germane to the shape at point.

While enabled, the ShapeDecl label -- and, if
`shexc-ts-mode-highlight-extends-include-predicates' is non-nil, the
predicates -- of every shape `shexc-ts-mode-highlight-extended-shapes'
finds germane to the `shape_expr_decl' containing point are
highlighted, updating automatically as point moves between shapes and
clearing when point leaves all `shape_expr_decl's.

This is the standalone, manifest-free counterpart to the highlighting
`shex-manifest-browser-mode' drives from `sht:shape'; the two can be
used independently or together."
  :lighter " ExtH"
  (if shexc-ts-mode-highlight-extends-mode
      (progn
        (add-hook 'post-command-hook #'shexc-ts-mode--highlight-extends-update nil t)
        (shexc-ts-mode--highlight-extends-update))
    (remove-hook 'post-command-hook #'shexc-ts-mode--highlight-extends-update t)
    (setq shexc-ts-mode--highlight-extends-last-decl-start nil)
    (shexc-ts-mode-clear-extends-overlays)))

;;;###autoload
(defun shexc-ts-mode-toggle-highlight-extends-predicates ()
  "Toggle `shexc-ts-mode-highlight-extends-include-predicates'.

That option controls whether `shexc-ts-mode-highlight-extends-mode'
also highlights the predicates of the shape at point and of the
shapes it extends, not just their ShapeDecl labels.  If
`shexc-ts-mode-highlight-extends-mode' is currently enabled, its
overlays are refreshed immediately to reflect the new setting."
  (interactive)
  (setq shexc-ts-mode-highlight-extends-include-predicates
        (not shexc-ts-mode-highlight-extends-include-predicates))
  (when shexc-ts-mode-highlight-extends-mode
    (if (shexc-ts-mode--decl-at (point))
        (shexc-ts-mode-highlight-extended-shapes
         (point) shexc-ts-mode-highlight-extends-include-predicates)
      (shexc-ts-mode-clear-extends-overlays)))
  (message "shexc-ts-mode-highlight-extends-include-predicates: %s"
           shexc-ts-mode-highlight-extends-include-predicates))

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
  "If VALUE-EXPR (an `inline_shape_expression') is simply a
`{ ... }' shape -- an `inline_shape_definition' with no
NodeConstraint and no AND/OR/NOT combination -- return that
`inline_shape_definition' node; otherwise return nil."
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
  "Return (TRIPLE-CONSTRAINT . INLINE-SHAPE-DEFINITION) for the
nearest `<predicate> { ... }' construct enclosing POS, or nil if
there is none.  Tries both POS and POS-1 as starting points: a
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
  "Remove the `<predicate> { ... }' wrapper enclosing point,
keeping its triple expressions and re-indenting them in place
\(the structural-editing \"splice\"/\"unwrap\"/\"raise\"
operation, e.g. `paredit-splice-sexp', specialized to ShExC's
nested-shape construct).  For example, with point inside

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
  "Wrap the triple expression(s) at point, or in the active
region, in a new `PREDICATE { ... }' shape that you are prompted
for, re-indenting the result (the inverse \"wrap\" operation,
e.g. `paredit-wrap-round'/`embrace-add', specialized to inject a
ShExC predicate along with the braces).  For example, selecting

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
  "Return the position of the first `{' token in NODE's subtree, or
nil if it has none.

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
  "Return the position of the `{' of the `{ ... }' shape body that
`shexc-ts-mode-toggle-fold' should act on at POS, or nil if there is
none.

If POS is inside a `shape_definition'/`inline_shape_definition', that
is the body in question.  Otherwise, if POS is elsewhere in an
enclosing `shape_expr_decl' -- e.g. on a `<#Shape> EXTENDS
@<#Other>' header line, before its `{ ... }' body -- fall back to
that declaration's first `{ ... }'."
  (if-let* ((def (shexc-ts-mode--ancestor-of-type
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
  "Return the prefix portion of prefixed-name TEXT, i.e. everything
before its `:' -- \"ex\" for both `ex:p1' and `ex:', and \"\" for
`:p1'."
  (substring text 0 (string-search ":" text)))

(defun shexc-ts-mode--flymake-undefined-prefixes ()
  "Return `:error' diagnostics for `prefixed_name's (`ex:p1', `ex:',
`x:Foo', a value-set IRI, a datatype, ...) whose prefix has no
matching `PREFIX' declaration."
  (let ((declared
         (mapcar (lambda (n) (shexc-ts-mode--prefix-name (treesit-node-text n t)))
                 (treesit-query-capture
                  (treesit-buffer-root-node)
                  '((prefix_decl name: (pname_ns) @name)) nil nil t))))
    (delq nil
          (mapcar
           (lambda (n)
             (let* ((text (treesit-node-text n t))
                    (prefix (shexc-ts-mode--prefix-name text)))
               (unless (member prefix declared)
                 (flymake-make-diagnostic
                  (current-buffer)
                  (treesit-node-start n) (treesit-node-end n)
                  :error
                  (format "Undefined prefix %s:" prefix)))))
           (treesit-query-capture
            (treesit-buffer-root-node) '((prefixed_name) @n) nil nil t)))))

(defun shexc-ts-mode--flymake-syntax-errors ()
  "Return `:error' diagnostics for tree-sitter ERROR nodes, i.e. text
the parser could not make sense of, e.g. the `p3' in `ex: p3 . ;'
\(a valid `ex:' prefixed name with empty local part, followed by an
unexpected token\)."
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
  "Return `:error' diagnostics for shape references with no matching
`shape_expr_decl', e.g. `@<#Typo>' or `EXTENDS @<#Typo>'."
  (let ((decl-labels
         (mapcar (lambda (n) (treesit-node-text n t))
                 (shexc-ts-mode--query-labels
                  '((shape_expr_decl label: (shape_expr_label) @label))))))
    (delq nil
          (mapcar
           (lambda (ref)
             (let ((text (treesit-node-text ref t)))
               (unless (member text decl-labels)
                 (flymake-make-diagnostic
                  (current-buffer)
                  (treesit-node-start ref) (treesit-node-end ref)
                  :error
                  (format "Undefined shape %s" text)))))
           (shexc-ts-mode--query-labels
            '((shape_ref label: (shape_expr_label) @label)))))))

(defun shexc-ts-mode--flymake-duplicate-predicates ()
  "Return `:note' diagnostics for predicates that occur more than once
as direct elements of the same EachOf/OneOf-disjunct group
\(`group_triple_expr'\).

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
`shexc-ts-mode--flymake-duplicate-predicates')."
  (funcall report-fn
           (append (shexc-ts-mode--flymake-undefined-shapes)
                   (shexc-ts-mode--flymake-undefined-prefixes)
                   (shexc-ts-mode--flymake-syntax-errors)
                   (shexc-ts-mode--flymake-duplicate-predicates))))

;;; shex-manifest-browser integration

(defun shexc-ts-mode--manifest-browser-loaded-p ()
  "Return non-nil if the optional `shex-manifest-browser' package has
been loaded, i.e. `shexc-ts-mode-menu' should offer its settings."
  (boundp 'shex-manifest-browser-highlight-extended-predicates))

;;;###autoload
(defun shexc-ts-mode-toggle-manifest-browser-predicates ()
  "Toggle `shex-manifest-browser-highlight-extended-predicates'.

That option controls whether `shex-manifest-browser' also highlights
the predicates of extended shapes (not just their ShapeDecl labels)
when it highlights a manifest entry's schema. The new value takes
effect the next time `shex-manifest-browser' highlights an entry."
  (interactive)
  (unless (shexc-ts-mode--manifest-browser-loaded-p)
    (user-error "shex-manifest-browser is not loaded"))
  (setq shex-manifest-browser-highlight-extended-predicates
        (not shex-manifest-browser-highlight-extended-predicates))
  (message "shex-manifest-browser-highlight-extended-predicates: %s"
           shex-manifest-browser-highlight-extended-predicates))

;;; Feature menu (transient)

(defun shexc-ts-mode--menu-desc (label command)
  "Return LABEL annotated with COMMAND's key binding in
`shexc-ts-mode-map', for use as a `transient' suffix description."
  (let ((key (where-is-internal command shexc-ts-mode-map t)))
    (if key
        (format "%-36s %s" label (key-description key))
      label)))

(transient-define-prefix shexc-ts-mode-menu ()
  "Feature menu for `shexc-ts-mode', showing live keybindings."
  [["Navigate"
    ("h" shexc-ts-mode-highlight-extends-mode
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Highlight shapes reachable from point"
                 'shexc-ts-mode-highlight-extends-mode)))
    ("P" shexc-ts-mode-toggle-highlight-extends-predicates
     :description
     (lambda ()
       (format "Also highlight predicates of those shapes [%s]"
               (if shexc-ts-mode-highlight-extends-include-predicates "X" " "))))
    ("f" shexc-ts-mode-toggle-fold
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Fold/unfold shape body at point"
                 'shexc-ts-mode-toggle-fold)))
    (:info "C-M-a / C-M-e    Previous/next shape declaration")
    (:info "C-M-f / C-M-b    Forward/backward over a shape branch")
    (:info "M-. / M-, / M-?  Go to / pop back from / find refs to shape")]
   ["Edit"
    ("u" shexc-ts-mode-unwrap-shape
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Unwrap `<predicate> { ... }' shape"
                 'shexc-ts-mode-unwrap-shape)))
    ("w" shexc-ts-mode-wrap-in-shape
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Wrap region in `<predicate> { ... }'"
                 'shexc-ts-mode-wrap-in-shape)))
    ("r" shexc-ts-mode-rename-shape
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Rename shape label everywhere"
                 'shexc-ts-mode-rename-shape)))
    ("k" shexc-ts-mode-toggle-comment-style
     :description
     (lambda () (shexc-ts-mode--menu-desc
                 "Toggle `#' / `/* */' comment style"
                 'shexc-ts-mode-toggle-comment-style)))]
   ["Manifest browser"
    :if shexc-ts-mode--manifest-browser-loaded-p
    ("p" shexc-ts-mode-toggle-manifest-browser-predicates
     :description
     (lambda ()
       (format "Also highlight extended shapes' predicates [%s]"
               (if shex-manifest-browser-highlight-extended-predicates
                   "X" " "))))]])

;;; Major mode

;;;###autoload
(define-derived-mode shexc-ts-mode prog-mode "ShEx[ts]"
  "Major mode for editing ShExC documents, powered by tree-sitter.

\\{shexc-ts-mode-map}"
  :syntax-table shexc-ts-mode--syntax-table
  (unless (treesit-ready-p 'shexc)
    (error "Tree-sitter grammar for `shexc' is not available -- \
see the Setup section in shexc-ts-mode.el"))

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

  ;; live "germane shapes" highlighting, following point
  (define-key shexc-ts-mode-map (kbd "C-c C-h") #'shexc-ts-mode-highlight-extends-mode)

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

  (treesit-major-mode-setup))

;; Prefer the tree-sitter mode for .shex(c) files when its grammar is
;; available; `shexc-mode' remains the fallback otherwise.
;;;###autoload
(when (treesit-ready-p 'shexc t)
  (add-to-list 'auto-mode-alist '("\\.shexc?\\'" . shexc-ts-mode)))

(provide 'shexc-ts-mode)

;;; shexc-ts-mode.el ends here
