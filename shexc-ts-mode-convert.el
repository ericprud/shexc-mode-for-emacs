;;; shexc-ts-mode-convert.el --- ShExC <-> ShExJ/ShExR in-place conversion -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/ericprud/shexc-mode-for-emacs
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Lets you convert a ShExC shape/schema to ShExJ (JSON) or ShExR
;; (canonical Turtle) text in place, edit it there, then convert back --
;; losing comments/whitespace but preserving semantics (see
;; shexc-shexj.el/shexc-shexr.el for the compiler/decompiler and
;; serializer/parser this is built on).
;;
;; The converted region is fenced as a single `/* ... */' block comment
;; with sentinel markers on its first/last lines:
;;
;;   /* shexc-ts-mode:BEGIN-SHEXJ <id>
;;   <pretty-printed JSON, however many lines>
;;   shexc-ts-mode:END-SHEXJ <id> */
;;
;; (`-SHEXR' for Turtle.)  Since `comment' is declared as a tree-sitter
;; `extras' token (grammar.js), this keeps the rest of the file's parse
;; tree completely unaffected -- no ERROR nodes, nothing to teach font-
;; lock/indent/xref/flymake about -- and answers "what happens on save"
;; trivially: it's just text, byte for byte, like any other comment.
;; tree-sitter-shexc's `/* ... */' lexer (like C's) has no escape
;; mechanism of its own -- it matches the *first* literal `*/' it finds
;; -- so a content `*/' (e.g. inside a regex-facet pattern string) would
;; prematurely terminate the comment.  `shexc-ts-mode-convert--escape-
;; content' defuses every such occurrence before embedding the content,
;; and `--unescape-content' reverses it on the way back out -- this is
;; the one bit of complexity `#'-prefixed line comments didn't need;
;; the win is a single comment node `shexc-ts-mode-convert--fence-at'
;; can locate directly via `treesit-node-at', rather than scanning
;; buffer lines by hand.
;;
;; Commands: `shexc-ts-mode-convert-to-shexj', `-to-shexr',
;; `-fence-to-shexc' (auto-detects direction from the sentinel), and
;; `-at-point' (bound to `C-c C-v'), which cycles the shape/schema at
;; point (or active region) through ShExC -> ShExJ -> ShExR -> ShExC --
;; outside any fence it converts to ShExJ; inside a ShExJ fence, onward
;; to ShExR; inside a ShExR fence, back to ShExC, closing the loop.
;; Also registers an additional flymake backend (via `shexc-ts-mode-hook',
;; so this file never has to modify shexc-ts-mode.el itself) that flags a
;; fence whose content doesn't parse, or whose BEGIN/END markers don't
;; match.

;;; Code:

(require 'shexc-ts-mode)
(require 'shexc-shexj)
(require 'shexc-shexr)
(require 'treesit)
(require 'flymake)
(require 'pcase)
(require 'seq)

(declare-function transient-append-suffix "transient")

(defgroup shexc-convert nil
  "ShExC <-> ShExJ/ShExR in-place conversion for `shexc-ts-mode'."
  :group 'shexc-ts)

;; ---------------------------------------------------------------------
;; Fence format
;; ---------------------------------------------------------------------

(defconst shexc-ts-mode-convert--zwsp (string ?\x200b)
  "Zero-width space (U+200B), used to defuse a literal `*/' inside fenced
content -- see `shexc-ts-mode-convert--escape-content'.")

(defconst shexc-ts-mode-convert--fence-re
  (concat "\\`/\\* shexc-ts-mode:BEGIN-\\(SHEXJ\\|SHEXR\\) \\([0-9]+\\)\n"
          "\\(\\(?:.\\|\n\\)*\\)"
          "\nshexc-ts-mode:END-\\1 \\2 \\*/\\'")
  "Matches a whole fence's text -- a `comment' node's `treesit-node-text'
\(including the `/*'/`*/' delimiters\) -- against `string-match'.  Group
1: KIND.  Group 2: ID.  Group 3: the content, still
`shexc-ts-mode-convert--escape-content'-escaped.  The `\\1' backreference
requires the END marker's KIND/ID to match the BEGIN marker's exactly.")

(defvar-local shexc-ts-mode-convert--next-id 0
  "Per-buffer counter for fence ids.  Cosmetic only -- the id has no
semantic meaning, it just makes two adjacent fences visually
distinguishable and a stray BEGIN/END mismatch easier to spot by eye.")

(defun shexc-ts-mode-convert--escape-content (text)
  "Insert a zero-width space between every literal `*' and `/' in TEXT,
so it can be embedded in a `/* ... */' block comment without
prematurely terminating it -- tree-sitter-shexc's comment lexer (like
C's) has no escape mechanism of its own, it just scans for the first
literal `*/'.  `shexc-ts-mode-convert--unescape-content' reverses this."
  (replace-regexp-in-string "\\*/" (concat "*" shexc-ts-mode-convert--zwsp "/") text nil t))

(defun shexc-ts-mode-convert--unescape-content (text)
  (replace-regexp-in-string (regexp-quote shexc-ts-mode-convert--zwsp) "" text nil t))

(defun shexc-ts-mode-convert--fence-text (kind id text)
  "KIND is \"SHEXJ\" or \"SHEXR\"; TEXT is the pretty-printed content."
  (let ((body (if (string-suffix-p "\n" text) (substring text 0 -1) text)))
    (format "/* shexc-ts-mode:BEGIN-%s %d\n%s\nshexc-ts-mode:END-%s %d */"
            kind id (shexc-ts-mode-convert--escape-content body) kind id)))

(defun shexc-ts-mode-convert--comment-node-at (pos)
  (let ((node (treesit-node-at pos)))
    (and node (string= (treesit-node-type node) "comment") node)))

(defun shexc-ts-mode-convert--fence-at (pos)
  "Return (KIND ID BEG END CONTENT) for the fence comment containing POS,
or nil if POS isn't inside (or just past the closing `*/' of) one, or
the comment's BEGIN/END markers don't match -- treated the same as \"no
fence here\" rather than guessing which part of a corrupted fence to
trust.

BEG/END are the comment node's own `treesit-node-start'/`-end' -- the
exact `/* ... */' span, nothing more -- so whatever followed the fence
in the original buffer (a blank line, the next declaration, ...) is
always left untouched outside [BEG,END).  CONTENT has already been
unescaped (see `shexc-ts-mode-convert--unescape-content')."
  (let* ((node (or (shexc-ts-mode-convert--comment-node-at pos)
                   ;; Point naturally ends up right *after* the closing
                   ;; `*/' after inserting a fresh fence -- check one
                   ;; character back too, rather than requiring the
                   ;; caller to know to reposition first.
                   (shexc-ts-mode-convert--comment-node-at (1- pos))))
         (text (and node (treesit-node-text node t))))
    (when (and text (string-match shexc-ts-mode-convert--fence-re text))
      (list (match-string 1 text) (match-string 2 text)
            (treesit-node-start node) (treesit-node-end node)
            (shexc-ts-mode-convert--unescape-content (match-string 3 text))))))

;; ---------------------------------------------------------------------
;; Convert-to-fence
;; ---------------------------------------------------------------------

(defun shexc-ts-mode-convert--directives-end ()
  "End position of the buffer's leading run of `base_decl'/`prefix_decl'/
`import_decl' nodes, or `point-min' if there are none.  Per the grammar
\(`shex_doc: repeat($._directive), optional(...)' \), these always form a
contiguous prefix before any shape declaration -- never interspersed --
so a single scan suffices.  Used so a whole-buffer conversion leaves
the BASE/PREFIX preamble itself in place rather than swallowing it into
the fence, where `shexc-shexj-buffer-directive-ctx' could no longer
find it to shorten IRIs when converting back."
  (let ((end (point-min)))
    (catch 'done
      (dolist (c (treesit-node-children (treesit-buffer-root-node) t))
        (if (member (treesit-node-type c) '("base_decl" "prefix_decl" "import_decl"))
            (setq end (treesit-node-end c))
          (throw 'done nil))))
    (when (> end (point-min))
      ;; Consume up to two of the newlines right after the last
      ;; directive -- the mandatory line break plus an optional blank-
      ;; line separator -- so they're excluded from (and thus survive)
      ;; the caller's delete-region, instead of the fence text getting
      ;; jammed directly onto the directive's own line.
      (save-excursion
        (goto-char end)
        (when (looking-at "\n") (forward-char 1))
        (when (looking-at "\n") (forward-char 1))
        (setq end (point))))
    end))

(defun shexc-ts-mode-convert--target ()
  "Return (BEG END VALUE-TREE) for the shape/schema to convert at point:
the active region, snapped outward to the smallest enclosing
`shape_expr_decl' if the region doesn't already align with one (and to
the whole buffer -- minus its leading BASE/PREFIX/IMPORT directives,
see `shexc-ts-mode-convert--directives-end' -- if no single decl
encloses it); else the `shape_expr_decl' at point; else likewise the
whole buffer minus its leading directives."
  (let* ((beg (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (region-end) (point)))
         (probe (treesit-node-at beg))
         (decl (treesit-parent-until
                probe
                (lambda (n) (and (string= (treesit-node-type n) "shape_expr_decl")
                                  (<= (treesit-node-start n) beg)
                                  (>= (treesit-node-end n) end)))
                t)))
    (if decl
        (list (treesit-node-start decl) (treesit-node-end decl) (shexc-shexj-compile-node decl))
      (list (shexc-ts-mode-convert--directives-end) (point-max) (shexc-shexj-compile-buffer)))))

(defun shexc-ts-mode-convert--indent-fence-opening-line (beg)
  "Indent just the line starting at BEG -- the fence's opening `/*
shexc-ts-mode:BEGIN-...' line -- to the correct column for its
context, without touching anything after it.  Inserted fence text is
already fully, correctly indented internally by the JSON/Turtle
serializer; `indent-region' over the whole span would instead flatten
or re-derive every interior line's indentation as if it were ShExC
code continuing a multi-line comment, destroying that nesting."
  (save-excursion (goto-char beg) (indent-according-to-mode)))

(defun shexc-ts-mode-convert--ctx-prefixes-alist (ctx)
  "CTX's prefix table (a hash table, see shexc-shexj.el's `ctx' struct)
as a plain (NAME . NAMESPACE-IRI) alist -- `shexc-shexr-serialize'
takes plain data, not shexc-shexj's `ctx' struct, per shexc-shexr.el's
existing boundary (see its \"Plist utilities\" commentary).  CTX's own
keys include the trailing `:' (e.g. \"ex:\", per
`shexc-shexj--apply-prefix'/`--compile-pname-ln's PNAME_NS token
convention) -- stripped here since `shexc-shexr-serialize' expects a
bare name and supplies its own `:' wherever one is needed."
  (let (acc)
    (maphash (lambda (name ns) (push (cons (substring name 0 -1) ns) acc))
              (shexc-shexj--ctx-prefixes ctx))
    acc))

(defun shexc-ts-mode-convert--shexr-serialize (tree)
  "Like `shexc-shexr-serialize', but fitting nested `[ ... ]'/`( ... )'
pairs to the converting window's actual text width rather than the
library's fixed 80-column default (a fence this wide is going to be
read in this window, so that's the width that matters), and inheriting
the buffer's own PREFIX/BASE declarations so the fence can use them
too -- only the ones it actually ends up using get a header line (see
`shexc-shexr-serialize')."
  (let ((ctx (shexc-shexj-buffer-directive-ctx)))
    (shexc-shexr-serialize tree (window-body-width)
                            (shexc-ts-mode-convert--ctx-prefixes-alist ctx)
                            (shexc-shexj--ctx-base ctx))))

(defun shexc-ts-mode-convert--do (renderer kind)
  (pcase-let ((`(,beg ,end ,tree) (shexc-ts-mode-convert--target)))
    (let* ((text (funcall renderer tree))
           (id (cl-incf shexc-ts-mode-convert--next-id))
           (fence (shexc-ts-mode-convert--fence-text kind id text)))
      (delete-region beg end)
      (goto-char beg)
      (insert fence)
      (shexc-ts-mode-convert--indent-fence-opening-line beg))))

;;;###autoload
(defun shexc-ts-mode-convert-to-shexj ()
  "Convert the shape/schema at point (or active region) to ShExJ, in
place, fenced as a `/* ... */' block comment.  See this file's
Commentary for the fence format and `shexc-ts-mode-convert--target' for
what \"the shape/schema at point\" means."
  (interactive)
  (shexc-ts-mode-convert--do #'shexc-shexj-to-json "SHEXJ"))

;;;###autoload
(defun shexc-ts-mode-convert-to-shexr ()
  "Convert the shape/schema at point (or active region) to ShExR
\(canonical Turtle\), in place, fenced as a `/* ... */' block comment."
  (interactive)
  (shexc-ts-mode-convert--do #'shexc-ts-mode-convert--shexr-serialize "SHEXR"))

;; ---------------------------------------------------------------------
;; Fence-to-ShExC
;; ---------------------------------------------------------------------

(defun shexc-ts-mode-convert--parse-fence-content (kind content)
  "Parse CONTENT (already stripped of `# ' prefixes) per KIND
\(\"SHEXJ\"/\"SHEXR\"\), signaling a `user-error' -- not a raw parser
error -- on failure, so the calling command's `delete-region' never
runs and the buffer is left untouched."
  (condition-case err
      (pcase kind
        ("SHEXJ" (shexc-shexj-from-json content))
        ("SHEXR" (shexc-shexr-parse content)))
    (error (user-error "shexc-ts-mode: cannot parse fenced %s: %s" kind (error-message-string err)))))

(defun shexc-ts-mode-convert--fence-tree (fence)
  "Parse FENCE's (as returned by `shexc-ts-mode-convert--fence-at') content
into a value-tree."
  (pcase-let ((`(,kind ,_id ,_beg ,_end ,content) fence))
    (shexc-ts-mode-convert--parse-fence-content kind content)))

(defun shexc-ts-mode-convert--replace-fence (fence new-text &optional new-text-is-fence)
  "Replace the whole of FENCE (the `/* ... */' comment, in full) with
NEW-TEXT.  Whatever followed the fence in the buffer (a blank line, the
next declaration, ...) sits immediately past END (see
`shexc-ts-mode-convert--fence-at') and is left untouched, so NEW-TEXT
must supply no trailing newline of its own -- e.g. `shexc-shexj-decompile'
always ends its output with one, unlike `shexc-ts-mode-convert--fence-text'
-- or that original separator would be pushed one line further out,
silently swallowing a blank line.

NEW-TEXT-IS-FENCE selects how NEW-TEXT gets indented: nil (NEW-TEXT is
real ShExC code, e.g. from `shexc-shexj-decompile', which doesn't
attempt its own indentation) runs `indent-region' over the whole span
as usual; non-nil (NEW-TEXT is itself a freshly built fence, already
fully and correctly indented internally by the JSON/Turtle serializer)
only indents the opening line -- see
`shexc-ts-mode-convert--indent-fence-opening-line'."
  (pcase-let ((`(,_kind ,_id ,beg ,end ,_content) fence))
    (delete-region beg end)
    (goto-char beg)
    (insert (if (string-suffix-p "\n" new-text) (substring new-text 0 -1) new-text))
    (if new-text-is-fence
        (shexc-ts-mode-convert--indent-fence-opening-line beg)
      (indent-region beg (point)))))

;; ---------------------------------------------------------------------
;; Reusing the buffer's own PREFIX/BASE declarations when decompiling
;; ---------------------------------------------------------------------

(defun shexc-ts-mode-convert--safe-pn-local-p (s)
  "Whether S can be emitted as a PN_LOCAL with no `%XX'/backslash
escaping -- deliberately conservative (e.g. rejects a trailing `.',
which PN_LOCAL disallows unescaped): false negatives just mean a
shortening opportunity is missed, never that something unparseable
gets emitted."
  (and (not (string-empty-p s))
       (string-match-p "\\`[A-Za-z0-9_][A-Za-z0-9_.-]*\\'" s)
       (not (string-suffix-p "." s))))

(defun shexc-ts-mode-convert--shorten-iri (ctx iri)
  "Try to shorten IRI using CTX's PREFIX table (longest-namespace-match,
local part verified safe to emit unescaped) or, failing that, CTX's
BASE (verified by re-resolving the candidate relative form and
confirming it reproduces IRI exactly, so an unanticipated quirk of IRI-
relative-resolution can never silently produce the wrong reference).
Returns the complete replacement token (`prefix:local' or `<relative>'),
or nil if neither applies -- meaning \"emit the full `<IRI>' as-is\'."
  (let (best-prefix best-ns)
    (maphash (lambda (name ns)
               (when (and (string-prefix-p ns iri)
                          (or (not best-ns) (> (length ns) (length best-ns))))
                 (setq best-prefix name best-ns ns)))
             (shexc-shexj--ctx-prefixes ctx))
    (cond
     ((and best-ns
           (shexc-ts-mode-convert--safe-pn-local-p (substring iri (length best-ns))))
      (concat best-prefix (substring iri (length best-ns))))
     ((let ((base (shexc-shexj--ctx-base ctx)))
        (and base (string-prefix-p base iri)
             (let ((relative (substring iri (length base))))
               (and (equal (url-expand-file-name relative base) iri) relative))))
      (concat "<" (let ((base (shexc-shexj--ctx-base ctx))) (substring iri (length base))) ">"))
     (t nil))))

;;;###autoload
(defun shexc-ts-mode-convert-fence-to-shexc ()
  "Convert the ShExJ/ShExR fence at point back to ShExC text, replacing
the whole fence (the `/* ... */' comment, in full).  IRIs that match a
PREFIX or BASE already declared elsewhere in the buffer are shortened
accordingly (see `shexc-ts-mode-convert--shorten-iri'); nothing is
ever *added* to the buffer's own declarations."
  (interactive)
  (let ((fence (shexc-ts-mode-convert--fence-at (point))))
    (unless fence
      (user-error "No shexc-ts-mode ShExJ/ShExR fence here"))
    (let* ((ctx (shexc-shexj-buffer-directive-ctx))
           (shexc-shexj-decompile-iri-shortener
            (lambda (iri) (shexc-ts-mode-convert--shorten-iri ctx iri))))
      (shexc-ts-mode-convert--replace-fence
       fence (shexc-shexj-decompile (shexc-ts-mode-convert--fence-tree fence))))))

(defun shexc-ts-mode-convert--fence-to-other-fence (fence target-kind renderer)
  "Replace FENCE with a new fence of TARGET-KIND (\"SHEXJ\"/\"SHEXR\"),
rendering FENCE's parsed value-tree via RENDERER."
  (let* ((tree (shexc-ts-mode-convert--fence-tree fence))
         (text (funcall renderer tree))
         (id (cl-incf shexc-ts-mode-convert--next-id)))
    (shexc-ts-mode-convert--replace-fence fence (shexc-ts-mode-convert--fence-text target-kind id text) t)))

;; ---------------------------------------------------------------------
;; Dispatcher and menu
;; ---------------------------------------------------------------------

;;;###autoload
(defun shexc-ts-mode-convert-at-point ()
  "Cycle the shape/schema at point (or active region) through ShExC ->
ShExJ -> ShExR -> ShExC: outside any fence, convert to ShExJ; inside a
ShExJ fence, convert onward to ShExR; inside a ShExR fence, convert
back to ShExC, closing the loop."
  (interactive)
  (let ((fence (shexc-ts-mode-convert--fence-at (point))))
    (cond
     ((not fence) (shexc-ts-mode-convert-to-shexj))
     ((string= (car fence) "SHEXJ")
      (shexc-ts-mode-convert--fence-to-other-fence fence "SHEXR" #'shexc-ts-mode-convert--shexr-serialize))
     (t (shexc-ts-mode-convert-fence-to-shexc)))))

;;;###autoload
(define-key shexc-ts-mode-map (kbd "C-c C-v") #'shexc-ts-mode-convert-at-point)

(with-eval-after-load 'transient
  ;; LOC '(0 1) -- shexc-ts-mode-menu's whole `[[...]["Edit"...]]' body is
  ;; one implicit top-level "columns" group (layout index 0), whose own
  ;; children are the "Navigate"/"Edit" groups (index 1 = "Edit") -- append
  ;; after that to add "Convert" as a third sibling column.
  (transient-append-suffix 'shexc-ts-mode-menu '(0 1)
    ["Convert"
     ("v" shexc-ts-mode-convert-at-point
      :description
      (lambda () (shexc-ts-mode--menu-desc
                  "Cycle ShExC -> ShExJ -> ShExR -> ShExC"
                  'shexc-ts-mode-convert-at-point)))
     ("j" shexc-ts-mode-convert-to-shexj
      :description
      (lambda () (shexc-ts-mode--menu-desc
                  "Convert to ShExJ" 'shexc-ts-mode-convert-to-shexj)))
     ("r" shexc-ts-mode-convert-to-shexr
      :description
      (lambda () (shexc-ts-mode--menu-desc
                  "Convert to ShExR (Turtle)" 'shexc-ts-mode-convert-to-shexr)))
     ("b" shexc-ts-mode-convert-fence-to-shexc
      :description
      (lambda () (shexc-ts-mode--menu-desc
                  "Convert fence back to ShExC" 'shexc-ts-mode-convert-fence-to-shexc)))]))

;; ---------------------------------------------------------------------
;; Flymake: flag a fence that doesn't parse, or whose markers don't match
;; ---------------------------------------------------------------------

(defun shexc-ts-mode-convert--fence-candidate-comments ()
  "Every `comment' node in the buffer that looks like it's trying to be
a shexc-ts-mode fence (starts with the BEGIN sentinel), whether or not
it actually parses as one -- the candidates `shexc-ts-mode-convert--
flymake-fence-errors' checks."
  (seq-filter
   (lambda (node) (string-prefix-p "/* shexc-ts-mode:BEGIN-" (treesit-node-text node t)))
   (treesit-query-capture (treesit-buffer-root-node) '((comment) @c) nil nil t)))

(defun shexc-ts-mode-convert--flymake-fence-errors ()
  "One flymake diagnostic per malformed fenced ShExJ/ShExR block in the
current buffer."
  (let (diags)
    (dolist (node (shexc-ts-mode-convert--fence-candidate-comments))
      (let ((fence (shexc-ts-mode-convert--fence-at (treesit-node-start node))))
        (if (not fence)
            (push (flymake-make-diagnostic
                   (current-buffer) (treesit-node-start node) (treesit-node-end node)
                   :error "shexc-ts-mode: malformed fence (missing/mismatched END marker)")
                  diags)
          (pcase-let ((`(,kind ,_id ,beg ,end ,content) fence))
            (condition-case err
                (pcase kind
                  ("SHEXJ" (shexc-shexj-from-json content))
                  ("SHEXR" (shexc-shexr-parse content)))
              (error
               (push (flymake-make-diagnostic
                      (current-buffer) beg end
                      :error (format "shexc-ts-mode: cannot parse fenced %s: %s"
                                      kind (error-message-string err)))
                     diags)))))))
    (nreverse diags)))

(defun shexc-ts-mode-convert--flymake-backend (report-fn &rest _args)
  (funcall report-fn (shexc-ts-mode-convert--flymake-fence-errors)))

(defun shexc-ts-mode-convert--setup ()
  (add-hook 'flymake-diagnostic-functions #'shexc-ts-mode-convert--flymake-backend nil t))

;;;###autoload
(add-hook 'shexc-ts-mode-hook #'shexc-ts-mode-convert--setup)

(provide 'shexc-ts-mode-convert)

;;; shexc-ts-mode-convert.el ends here
