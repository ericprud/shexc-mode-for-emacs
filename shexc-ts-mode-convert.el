;;; shexc-ts-mode-convert.el --- ShExC <-> ShExJ/ShExR in-place conversion -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; Version: 3.0.0
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
(require 'shexc-shexpath)
(require 'shexc-shexr)
(require 'treesit)
(require 'flymake)
(require 'pcase)
(require 'seq)
(require 'rdf-core)
(require 'xref)

(declare-function transient-append-suffix "transient")

;; `turtle-ts-mode'/`json-ts-mode' are `require'd lazily, only once
;; `treesit-ready-p' (quietly) confirms the grammar is actually
;; available -- see `shexc-ts-mode-convert--require-embedded-mode'.
;; Merely loading either file unconditionally would be enough to
;; trigger the very "grammar unavailable" warning this is trying to
;; avoid: both files' own top-level `treesit-font-lock-rules' calls
;; try to compile their queries against the language right away,
;; which warns just like an unguarded `treesit-ready-p' call would --
;; confirmed empirically, including that it happens a second time on
;; every subsequent `shexc-ts-mode' buffer if this file's own setup
;; code *also* called `treesit-ready-p' without its QUIET argument.
;; These `defvar's are forward declarations only (no value), just so
;; the byte-compiler doesn't warn about a free variable reference
;; further down in this file.
(defvar turtle-ts-mode--font-lock-settings)
(defvar turtle-ts-mode--font-lock-feature-list)
(defvar turtle-ts-mode--indent-rules)
(defvar json-ts-mode--font-lock-settings)
(defvar json-ts--indent-rules)

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

(defvar-local shexc-ts-mode-convert--fence-targets (make-hash-table :test 'eql)
  "Maps a fence's numeric id to `(PATH . SOURCE-OFFSET)' -- see
`shexc-ts-mode-convert--value-path' -- for fences created by a tracked
conversion (point was on something locatable at the time), so a later
fence-to-fence hop can keep point on \"the same thing\" too, even
though that hop re-parses the fence's *text* into a value-tree that's
structurally identical to, but never `eq' to, the one that was
originally serialized into it.")

(defvar-local shexc-ts-mode-convert--id-locations nil
  "An alist (ID-STRING . SHEXPATH-STRING) -- see
`shexc-shexpath-id-locations' -- rebuilt fresh by
`shexc-ts-mode-convert--target' every time a shape/schema is converted
*to* ShExJ/ShExR, from the value-tree compiled at that moment (the only
point at which a hoisted TripleExpr's \"real\" definition site is
unambiguous by construction).  Consulted by
`shexc-ts-mode-convert--parse-fence-content' when converting a ShExR
fence back, to resolve the analogous ambiguity that a hand-edited fence
otherwise has no in-graph way to settle -- see shexc-shexr.el's
Commentary and `shexc-shexr--ambiguous-id-winners'.  In-memory,
session-only, like `shexc-ts-mode-convert--fence-targets' -- never
written into the fence text itself.")

(defun shexc-ts-mode-convert--value-path-1 (root target acc)
  (when (eq root target) (throw 'shexc-ts-mode-convert--value-path-found (reverse acc)))
  (cond
   ((and (consp root) (keywordp (car root)))
    (let ((tail root))
      (while tail
        (shexc-ts-mode-convert--value-path-1 (cadr tail) target (cons (car tail) acc))
        (setq tail (cddr tail)))))
   ((and (listp root) root)
    (let ((i 0))
      (dolist (x root)
        (shexc-ts-mode-convert--value-path-1 x target (cons i acc))
        (setq i (1+ i)))))))

(defun shexc-ts-mode-convert--value-path (root target)
  "Return a list of plist-keys/list-indices locating TARGET (by `eq')
within ROOT, or nil if TARGET isn't anywhere in ROOT.  Unlike locating
TARGET by `eq' directly (only ever valid against the *exact* Lisp
objects TARGET came from), a path is purely structural, so it stays
valid against a different, freshly-parsed value-tree with the same
shape -- see `shexc-ts-mode-convert--fence-targets'."
  (catch 'shexc-ts-mode-convert--value-path-found
    (shexc-ts-mode-convert--value-path-1 root target nil)
    nil))

(defun shexc-ts-mode-convert--resolve-path (root path)
  "Walk PATH (as returned by `shexc-ts-mode-convert--value-path') down
ROOT, returning the value found there, or nil if PATH no longer
resolves (e.g. the fence was hand-edited in a way that changed its
shape)."
  (let ((v root))
    (catch 'shexc-ts-mode-convert--resolve-path-fail
      (dolist (key path)
        (setq v (if (keywordp key)
                    (if (and (consp v) (keywordp (car v))) (plist-get v key)
                      (throw 'shexc-ts-mode-convert--resolve-path-fail nil))
                  (if (and (listp v) (>= key 0) (> (length v) key)) (nth key v)
                    (throw 'shexc-ts-mode-convert--resolve-path-fail nil)))))
      v)))

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
  ;; Explicit `\='shexc -- `treesit-node-at' with no language guesses
  ;; via `treesit-language-at', which (once `shexc-ts-mode-convert--
  ;; setup-fence-embedding' has run) reports `turtle'/`json' for a
  ;; position inside a fence's *content* -- exactly the position this
  ;; is most often called at (point left inside the fence by a
  ;; previous conversion).  Confirmed empirically: omitting this made
  ;; `shexc-ts-mode-convert-at-point' stop recognizing point was
  ;; inside a SHEXR fence at all, falling through to "convert the
  ;; whole buffer to ShExJ" instead of closing the ShExR->ShExC loop.
  (let ((node (treesit-node-at pos 'shexc)))
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
      (dolist (c (treesit-node-children (treesit-buffer-root-node 'shexc) t))
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

(defun shexc-ts-mode-convert--locate-target (root-node orig-point)
  "Return (TARGET-VALUE . SOURCE-OFFSET): TARGET-VALUE identifies, within
whatever was just compiled from ROOT-NODE (with
`shexc-shexj--position-table' bound around that compile call -- the
caller's job), the value-tree node/leaf that ORIG-POINT's tree-sitter
element corresponds to; SOURCE-OFFSET is ORIG-POINT's distance from the
END of that element's own span (not the start -- see
`shexc-shexr--mark': a value's rendering can gain or lose an
arbitrary-length prefix between hops, e.g. a PNAME's `ex:' vs. a full
namespace IRI, but the trailing characters -- a PNAME's local part, an
IRI's tail -- stay the same, so measuring from the end is what survives
that).  nil if ORIG-POINT isn't inside ROOT-NODE's span at all.  Walks
up from the smallest node at ORIG-POINT until the position table has a
recorded value that's a cons or string -- atoms like `t'/numbers are
skipped since they're Lisp singletons, not reliably locatable by `eq'
on the output side (see shexc-shexr.el/shexc-shexj.el's `--mark')."
  (when (and (<= (treesit-node-start root-node) orig-point)
             (>= (treesit-node-end root-node) orig-point))
    (let ((n (treesit-node-at orig-point 'shexc)))
      (catch 'found
        (while n
          (let ((val (gethash (cons (treesit-node-start n) (treesit-node-end n))
                               shexc-shexj--position-table)))
            (when (or (consp val) (stringp val))
              (throw 'found
                     (cons val (max 0 (min (- (treesit-node-end n) orig-point)
                                            (- (treesit-node-end n) (treesit-node-start n))))))))
          (setq n (treesit-node-parent n)))
        nil))))

(defun shexc-ts-mode-convert--target ()
  "Return (BEG END VALUE-TREE TARGET SOURCE-OFFSET) for the shape/schema
to convert at point: BEG/END/VALUE-TREE as before -- the active region,
snapped outward to the smallest enclosing `shape_expr_decl' if the
region doesn't already align with one (and to the whole buffer -- minus
its leading BASE/PREFIX/IMPORT directives, see
`shexc-ts-mode-convert--directives-end' -- if no single decl encloses
it); else the `shape_expr_decl' at point; else likewise the whole
buffer minus its leading directives.

TARGET/SOURCE-OFFSET (both nil together if there's no match) identify
the value-tree node/leaf that `(point)' -- captured here before any of
the above region snapping -- corresponds to, for callers (e.g.
`shexc-ts-mode-convert--do') that want to keep point on \"the same
thing\" across conversion; see `shexc-ts-mode-convert--locate-target'."
  (let* ((orig-point (point))
         (beg (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (region-end) (point)))
         (probe (treesit-node-at beg 'shexc))
         (decl (treesit-parent-until
                probe
                (lambda (n) (and (string= (treesit-node-type n) "shape_expr_decl")
                                  (<= (treesit-node-start n) beg)
                                  (>= (treesit-node-end n) end)))
                t))
         (shexc-shexj--position-table (make-hash-table :test 'equal)))
    (if decl
        (let* ((tree (shexc-shexj-compile-node decl))
               (located (shexc-ts-mode-convert--locate-target decl orig-point)))
          (setq shexc-ts-mode-convert--id-locations (shexc-shexpath-id-locations tree))
          (list (treesit-node-start decl) (treesit-node-end decl) tree (car located) (cdr located)))
      (let* ((tree (shexc-shexj-compile-buffer))
             (located (shexc-ts-mode-convert--locate-target (treesit-buffer-root-node 'shexc) orig-point)))
        (setq shexc-ts-mode-convert--id-locations (shexc-shexpath-id-locations tree))
        (list (shexc-ts-mode-convert--directives-end) (point-max) tree (car located) (cdr located))))))

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

(defun shexc-ts-mode-convert--shexr-serialize (tree &optional target)
  "Like `shexc-shexr-serialize', but fitting nested `[ ... ]'/`( ... )'
pairs to the converting window's actual text width rather than the
library's fixed 80-column default (a fence this wide is going to be
read in this window, so that's the width that matters), and inheriting
the buffer's own PREFIX/BASE declarations so the fence can use them
too -- only the ones it actually ends up using get a header line (see
`shexc-shexr-serialize').  TARGET is passed straight through."
  (let ((ctx (shexc-shexj-buffer-directive-ctx)))
    (shexc-shexr-serialize tree (window-body-width)
                            (shexc-ts-mode-convert--ctx-prefixes-alist ctx)
                            (shexc-shexj--ctx-base ctx)
                            target)))

(defun shexc-ts-mode-convert--shexj-serialize (tree &optional target)
  "Like `shexc-shexj-to-json', renamed/wrapped only so `shexc-ts-mode-
convert-to-shexj' has the same `(tree &optional target)' calling
convention as `shexc-ts-mode-convert--shexr-serialize'."
  (shexc-shexj-to-json tree target))

(defun shexc-ts-mode-convert--escaped-offset (body offset)
  "OFFSET into unescaped BODY, translated to account for every ZWSP
`shexc-ts-mode-convert--escape-content' inserts before it (one per
literal `*/' occurrence) -- without this, a target sitting after such
an occurrence (a regex facet pattern containing `*/', say) would be
off by however many ZWSPs preceded it."
  (let ((count 0) (pos 0))
    (while (and (setq pos (string-search "*/" body pos)) (< pos offset))
      (setq count (1+ count) pos (1+ pos)))
    (+ offset count)))

(defun shexc-ts-mode-convert--do (renderer kind)
  (pcase-let ((`(,beg ,end ,tree ,target ,source-offset) (shexc-ts-mode-convert--target)))
    (let* ((result (funcall renderer tree target))
           (text (if target (car result) result))
           (out-offset (and target (cdr result)))
           (id (cl-incf shexc-ts-mode-convert--next-id))
           (fence (shexc-ts-mode-convert--fence-text kind id text)))
      (delete-region beg end)
      (goto-char beg)
      (insert fence)
      (shexc-ts-mode-convert--indent-fence-opening-line beg)
      (when out-offset
        (puthash id (cons (shexc-ts-mode-convert--value-path tree target) source-offset)
                 shexc-ts-mode-convert--fence-targets))
      (if out-offset
          (goto-char (max beg
                           (min (point-max)
                                (+ beg (1+ (string-search "\n" fence))
                                   (shexc-ts-mode-convert--escaped-offset text out-offset)
                                   (- source-offset)))))
        (goto-char beg)))))

;;;###autoload
(defun shexc-ts-mode-convert-to-shexj ()
  "Convert the shape/schema at point (or active region) to ShExJ, in
place, fenced as a `/* ... */' block comment.  See this file's
Commentary for the fence format and `shexc-ts-mode-convert--target' for
what \"the shape/schema at point\" means."
  (interactive)
  (shexc-ts-mode-convert--do #'shexc-ts-mode-convert--shexj-serialize "SHEXJ"))

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
runs and the buffer is left untouched.  For \"SHEXR\", consults
`shexc-ts-mode-convert--id-locations' (see
`shexc-shexr--id-location-table') to resolve which occurrence of a
multiply-referenced TripleExpr is the definition -- the buffer's own
table, from the last time this region was converted *to* ShExR, is the
best available hint for a hand-edited fence; absent a usable entry, the
textually-first occurrence wins (see `shexc-shexr--ambiguous-id-winners')."
  (condition-case err
      (pcase kind
        ("SHEXJ" (shexc-shexj-from-json content))
        ("SHEXR" (let ((shexc-shexr--id-location-table shexc-ts-mode-convert--id-locations))
                   (shexc-shexr-parse content))))
    (error (user-error "shexc-ts-mode: cannot parse fenced %s: %s" kind (error-message-string err)))))

(defun shexc-ts-mode-convert--fence-tree (fence)
  "Parse FENCE's (as returned by `shexc-ts-mode-convert--fence-at') content
into a value-tree."
  (pcase-let ((`(,kind ,_id ,_beg ,_end ,content) fence))
    (shexc-ts-mode-convert--parse-fence-content kind content)))

(defun shexc-ts-mode-convert--text-line-col (text offset)
  "Decompose OFFSET (a 0-indexed position within TEXT) into (LINE .
COL-FROM-CONTENT): LINE is OFFSET's 0-indexed line number within TEXT;
COL-FROM-CONTENT is OFFSET's distance from that line's first non-
whitespace character.  Used instead of a raw character count to locate
the analogous position in the buffer AFTER `indent-region' has run --
`indent-region' only ever rewrites a line's leading whitespace, never
anything after it, so a line/distance-from-content pair survives that
rewrite where a flat character offset into the pre-indent TEXT
wouldn't (see `shexc-ts-mode-convert--goto-text-line-col')."
  (let ((line 0) (line-start 0) (search-from 0) next)
    (while (and (setq next (string-search "\n" text search-from))
                (< next offset))
      (setq line (1+ line) line-start (1+ next) search-from (1+ next)))
    (let* ((line-end (or (string-search "\n" text line-start) (length text)))
           (line-text (substring text line-start line-end))
           (content-offset (or (string-match "[^ \t]" line-text) (length line-text))))
      (cons line (- (- offset line-start) content-offset)))))

(defun shexc-ts-mode-convert--goto-text-line-col (beg line col-from-content)
  "Move point to BEG's LINEth line (0-indexed), then COL-FROM-CONTENT
characters past that line's first non-whitespace character (see
`shexc-ts-mode-convert--text-line-col'), clamped to the line's own end."
  (goto-char beg)
  (forward-line line)
  (back-to-indentation)
  (goto-char (min (line-end-position) (+ (point) (max 0 col-from-content)))))

(defun shexc-ts-mode-convert--replace-fence (fence new-text &optional new-text-is-fence target-offset)
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
`shexc-ts-mode-convert--indent-fence-opening-line'.

TARGET-OFFSET, if non-nil, is a 0-indexed position within NEW-TEXT (as
returned by `shexc-shexj-decompile' with a TARGET) to land point on
afterward -- via `shexc-ts-mode-convert--text-line-col'/`-goto-text-
line-col', which survive `indent-region' changing NEW-TEXT's leading
whitespace out from under a raw character offset; nil leaves point at
the (arbitrary) position `indent-region' itself happens to leave it."
  (pcase-let ((`(,_kind ,_id ,beg ,end ,_content) fence))
    (delete-region beg end)
    (goto-char beg)
    (insert (if (string-suffix-p "\n" new-text) (substring new-text 0 -1) new-text))
    (if new-text-is-fence
        (shexc-ts-mode-convert--indent-fence-opening-line beg)
      (indent-region beg (point)))
    (when target-offset
      (pcase-let ((`(,line . ,col) (shexc-ts-mode-convert--text-line-col new-text target-offset)))
        (shexc-ts-mode-convert--goto-text-line-col beg line col)))))

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
ever *added* to the buffer's own declarations.

If FENCE's own id has a stored conversion target (see
`shexc-ts-mode-convert--fence-targets' -- i.e. FENCE was itself created
by a tracked conversion and the user hasn't converted anything else in
between), keeps point on \"the same thing\" across this hop too -- the
PATH is re-resolved against the *freshly parsed* value-tree the same
way `shexc-ts-mode-convert--fence-to-other-fence' does, but landing in
the buffer via `shexc-ts-mode-convert--text-line-col'/`-goto-text-line-
col' rather than a flat character offset, since decompiling back to
ShExC is exactly the hop most likely to change whitespace out from
under one (`shexc-shexj-decompile' emits \"reasonably line-broken\"
text relying on `indent-region', called inside `shexc-ts-mode-convert--
replace-fence', to fix it up -- a raw offset into the pre-indent text
wouldn't survive that)."
  (interactive)
  (let ((fence (shexc-ts-mode-convert--fence-at (point))))
    (unless fence
      (user-error "No shexc-ts-mode ShExJ/ShExR fence here"))
    (pcase-let ((`(,_kind ,id ,_beg ,_end ,_content) fence))
      (let* ((ctx (shexc-shexj-buffer-directive-ctx))
             (shexc-shexj-decompile-iri-shortener
              (lambda (iri) (shexc-ts-mode-convert--shorten-iri ctx iri)))
             (tree (shexc-ts-mode-convert--fence-tree fence))
             (stored (gethash (string-to-number id) shexc-ts-mode-convert--fence-targets))
             (path (car stored))
             (source-offset (cdr stored))
             (target (and path (shexc-ts-mode-convert--resolve-path tree path)))
             (result (shexc-shexj-decompile tree target))
             (out-offset (and target (cdr result))))
        (shexc-ts-mode-convert--replace-fence
         fence (if target (car result) result) nil
         (and out-offset (max 0 (- out-offset source-offset))))))))

(defun shexc-ts-mode-convert--fence-to-other-fence (fence target-kind renderer)
  "Replace FENCE with a new fence of TARGET-KIND (\"SHEXJ\"/\"SHEXR\"),
rendering FENCE's parsed value-tree via RENDERER.  If FENCE's own id
has a stored conversion target (see
`shexc-ts-mode-convert--fence-targets' -- i.e. FENCE was itself created
by a tracked conversion and the user hasn't converted anything else in
between), keeps point on \"the same thing\" across this hop too: the
stored PATH is re-resolved against the *freshly parsed* TREE (never
`eq' to whatever was originally serialized into FENCE, since FENCE's
text was just parsed back from scratch) rather than relying on object
identity, and re-stored under the new fence's id so a further hop keeps
working the same way."
  (pcase-let ((`(,_kind ,old-id ,beg ,end ,_content) fence))
    (let* ((tree (shexc-ts-mode-convert--fence-tree fence))
           ;; OLD-ID is a string (a regexp capture group, see
           ;; `shexc-ts-mode-convert--fence-at'); `--fence-targets' is
           ;; keyed by the integer `shexc-ts-mode-convert--next-id'
           ;; produces, so this must be converted before lookup.
           (stored (gethash (string-to-number old-id) shexc-ts-mode-convert--fence-targets))
           (path (car stored))
           (source-offset (cdr stored))
           (target (and path (shexc-ts-mode-convert--resolve-path tree path)))
           (result (funcall renderer tree target))
           (text (if target (car result) result))
           (out-offset (and target (cdr result)))
           (id (cl-incf shexc-ts-mode-convert--next-id))
           (new-fence (shexc-ts-mode-convert--fence-text target-kind id text)))
      (delete-region beg end)
      (goto-char beg)
      (insert new-fence)
      (shexc-ts-mode-convert--indent-fence-opening-line beg)
      (when out-offset
        (puthash id (cons path source-offset) shexc-ts-mode-convert--fence-targets))
      (if out-offset
          (goto-char (max beg
                           (min (point-max)
                                (+ beg (1+ (string-search "\n" new-fence))
                                   (shexc-ts-mode-convert--escaped-offset text out-offset)
                                   (- source-offset)))))
        (goto-char beg)))))

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
(with-eval-after-load 'shexc-ts-mode
  (define-key shexc-ts-mode-map (kbd "C-c C-v") #'shexc-ts-mode-convert-at-point))

(transient-define-prefix shexc-ts-mode-convert-menu ()
  "Submenu of `shexc-ts-mode-convert' format-conversion commands -- kept
out of `shexc-ts-mode-menu' itself (reached there via a single \"v\"
entry in the \"Edit\" column) so that menu doesn't need a third column
just for this."
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
                "Convert fence back to ShExC" 'shexc-ts-mode-convert-fence-to-shexc)))
   ("q" "Done" transient-quit-all)])

(with-eval-after-load 'transient
  ;; Append as a sibling of the "Edit" column's last entry (found by its
  ;; command, not a layout-index path, so this stays correct regardless
  ;; of how many entries "Edit" itself ends up with) -- one more line in
  ;; that column, rather than a third column of its own.
  (transient-append-suffix 'shexc-ts-mode-menu 'shexc-ts-mode-prefix-menu
    '("v" shexc-ts-mode-convert-menu
      :description
      (lambda () (shexc-ts-mode--menu-desc
                  "Convert to ShExJ/ShExR..." 'shexc-ts-mode-convert-at-point)))))

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
   (treesit-query-capture (treesit-buffer-root-node 'shexc) '((comment) @c) nil nil t)))

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

;; ---------------------------------------------------------------------
;; Undefined-shape flymake check: every ShapeDecl declared inside a
;; fence counts as declared too -- see
;; `shexc-ts-mode-extra-declared-shape-labels-functions'.
;; ---------------------------------------------------------------------

(defun shexc-ts-mode-convert--fenced-decl-labels ()
  "Every ShapeDecl `:id' declared inside a ShExJ/ShExR fence in the
current buffer -- skips any fence that fails to parse (already
separately reported by `shexc-ts-mode-convert--flymake-fence-errors').
Fence-derived `:id's are always fully-resolved absolute IRIs (ShExJ/
ShExR never shorten), directly comparable against
`shexc-shexj-resolve-label''s output."
  (let (labels)
    (dolist (node (shexc-ts-mode-convert--fence-candidate-comments))
      (when-let* ((fence (shexc-ts-mode-convert--fence-at (treesit-node-start node)))
                  (tree (ignore-errors (shexc-ts-mode-convert--fence-tree fence))))
        (dolist (decl (plist-get tree :shapes))
          (push (plist-get decl :id) labels))))
    labels))

;; ---------------------------------------------------------------------
;; Embedded highlighting: a real `turtle'/`json' tree-sitter parser,
;; ranged to just a fence's content, so a ShExR/ShExJ fence is
;; font-locked (and indented) as actual Turtle/JSON rather than one
;; flat `font-lock-comment-face' span.
;;
;; The host `shexc' grammar has no internal structure for a fence's
;; content -- `comment' is a single opaque `extras' token (see this
;; file's top Commentary), so a `treesit-range-rules' QUERY (a
;; tree-sitter query against the host's own parse tree) can't target a
;; sub-range of it.  This uses that function's other form instead: a
;; plain Lisp function that sets every embedded parser's
;; `treesit-parser-set-included-ranges' directly, scanning fence
;; comments via the same `shexc-ts-mode-convert--fence-candidate-
;; comments'/`--fence-re' this file already uses for flymake -- so the
;; embedded range is always exactly the BEGIN/END-delimited content,
;; regardless of how many digits the fence id happens to have (a fixed
;; `:offset' trim, the usual technique for e.g. HTML's `<script>'/
;; `<style>' tags, can't express that here).
;;
;; Merged into the buffer's *existing* (single-language) settings
;; rather than replacing them, since `shexc-ts-mode-convert--setup'
;; runs from `shexc-ts-mode-hook' -- after `shexc-ts-mode' has already
;; called `treesit-major-mode-setup' once with its own settings alone.
;; The embedded languages' font-lock rules are prepended, not appended:
;; `shexc-ts-mode--font-lock-settings' has its own unconditional
;; `(comment) @font-lock-comment-face' rule with no `:override', which
;; would otherwise win first (it's a `:language \='shexc' rule, so it
;; still matches the whole comment node regardless of any range
;; restriction placed on the *embedded* parser) and paint right over
;; whatever the embedded rules would have set -- with no `:override'
;; either, fontification skips spans a prior rule already painted, so
;; running the embedded rules first lets that earlier rule's own
;; capture simply not repaint whatever they already painted.
;;
;; That earlier rule turns out *not* to repaint what's left over
;; either, though (confirmed empirically): once the embedded parser's
;; pass has claimed part of a comment node's span, the BEGIN/END
;; marker lines -- never claimed by the embedded parser, never painted
;; by anything -- are left with no face at all, not even `font-lock-
;; comment-face'.  `shexc-ts-mode-convert--fontify-fence-markers' is a
;; small dedicated rule, appended *last* with `:override t', that
;; explicitly repaints just those marker-line spans.

(defconst shexc-ts-mode-convert--fence-embedded-language
  '(("SHEXR" . turtle) ("SHEXJ" . json))
  "Which tree-sitter language embeds inside each fence KIND's content.")

;; `treesit-parser-list' (and therefore `treesit-buffer-root-node'/
;; `treesit-node-at' called with no explicit language, as the rest of
;; `shexc-ts-mode'/`shexc-ts-mode-convert.el'/`shexc-shexj.el' all do,
;; assuming there's exactly one parser) orders parsers *most-recently-
;; created first* -- confirmed empirically: creating a second parser
;; for an embedded language made it, not `shexc', the implicit default
;; everywhere, corrupting every one of those call sites (including
;; ones in the ShExC->ShExJ compiler itself) the moment this feature
;; activated.
;;
;; Tagging the embedded parsers (so they're excluded from the default,
;; untagged `treesit-parser-list') looks like the obvious fix, but
;; breaks the *other* direction instead: `treesit-font-lock-rules''
;; `:language' dispatch -- confirmed empirically too -- only finds an
;; *untagged* parser for that language, so a tagged embedded parser
;; never gets its rules applied at all (its content kept the host's
;; flat `font-lock-comment-face', not a single embedded-language face
;; anywhere).  So the embedded parsers must stay untagged, and `shexc'
;; must instead be re-established as the newest -- deleted and
;; recreated *after* them, right before `treesit-major-mode-setup''s
;; own `treesit-primary-parser' default would otherwise go stale.

(defun shexc-ts-mode-convert--reclaim-primary-parser-order ()
  "Recreate the `shexc' parser so it's newest (hence first in
`treesit-parser-list', the implicit default `treesit-buffer-root-node'/
`treesit-node-at' use) again, after the embedded parsers below were
created -- see this section's Commentary just above."
  (let ((old (car (treesit-parser-list nil 'shexc))))
    (when old (treesit-parser-delete old)))
  (setq-local treesit-primary-parser (treesit-parser-create 'shexc)))

(defun shexc-ts-mode-convert--fence-content-bounds (node)
  "Return (KIND CONTENT-BEG CONTENT-END) for fence comment NODE (as
returned by `shexc-ts-mode-convert--fence-candidate-comments'), or nil
if NODE's BEGIN/END markers don't match.  Unlike `shexc-ts-mode-convert
--fence-at', CONTENT-BEG/-END exclude the `BEGIN-KIND ID'/`END-KIND ID'
marker lines themselves (and their adjacent newlines) -- exactly the
span an embedded parser for KIND's content language should see."
  (let* ((beg (treesit-node-start node))
         (text (treesit-node-text node t)))
    (when (string-match shexc-ts-mode-convert--fence-re text)
      (list (match-string 1 text)
            (+ beg (match-beginning 3))
            (+ beg (match-end 3))))))

(defvar-local shexc-ts-mode-convert--fence-embedded-ready-languages nil
  "Subset of `shexc-ts-mode-convert--fence-embedded-language''s values
whose tree-sitter grammar is actually available in this session --
computed once in `shexc-ts-mode-convert--setup-fence-embedding', so
the range-update function below never calls `treesit-parser-create'
for a language with no grammar installed.")

(defun shexc-ts-mode-convert--update-embedded-ranges (&rest _start-end)
  "Set every ready embedded-language parser's included ranges to the
content spans of its kind of fence in the current buffer.  This is the
function form of a `treesit-range-rules' QUERY (see that function's
docstring: \"QUERY can also be a function that takes two arguments,
START and END ... It is OK for this function to set ranges in a larger
region\") -- always recomputes across the whole buffer (cheap; there
are normally only a handful of fences) rather than trying to
incrementally honor its own START/END arguments.

A parser given an explicit-but-empty ranges list still parses nothing,
unlike `nil' (which means \"parse the whole buffer\" -- see
`treesit-parser-set-included-ranges') -- so a language with zero
fences of its kind right now still gets a real, non-nil, zero-width
range rather than being left to fall back to parsing everything."
  (dolist (lang shexc-ts-mode-convert--fence-embedded-ready-languages)
    (let (ranges)
      (dolist (node (shexc-ts-mode-convert--fence-candidate-comments))
        (pcase (shexc-ts-mode-convert--fence-content-bounds node)
          (`(,kind ,beg ,end)
           (when (eq (cdr (assoc kind shexc-ts-mode-convert--fence-embedded-language)) lang)
             (push (cons beg end) ranges)))))
      (treesit-parser-set-included-ranges
       (treesit-parser-create lang)
       (or (sort ranges (lambda (a b) (< (car a) (car b))))
           (list (cons (point-min) (point-min))))))))

(defun shexc-ts-mode-convert--language-at-point (point)
  "`treesit-language-at-point-function' for `shexc-ts-mode': `turtle'/
`json' inside the matching kind of fence's content, else `shexc'."
  (let ((node (treesit-node-at point 'shexc)))
    (or (and node (string= (treesit-node-type node) "comment")
             (pcase (shexc-ts-mode-convert--fence-content-bounds node)
               (`(,kind ,beg ,end)
                (and (<= beg point) (< point end)
                     (let ((lang (cdr (assoc kind shexc-ts-mode-convert--fence-embedded-language))))
                       (and (memq lang shexc-ts-mode-convert--fence-embedded-ready-languages) lang))))))
        'shexc)))

(defun shexc-ts-mode-convert--fontify-fence-markers (node override start end &rest _)
  "Paint `font-lock-comment-face' on a fence comment NODE's BEGIN/END
marker lines only -- the embedded turtle/json parser already painted
its content (see this section's top Commentary).  A `treesit-font-
lock-rules' capture function (NODE/OVERRIDE/START/END per that
function's docstring), registered with `:override t' last, so it
always applies regardless of pass ordering: the plain, unconditional
`(comment) @font-lock-comment-face' rule in `shexc-ts-mode--font-lock-
settings' -- run earlier, for every comment including fence ones --
turns out not to repaint a fence's marker lines at all once the
embedded parser has claimed (and painted) the rest of that same
comment node's span (confirmed empirically: the marker lines were
left with no face whatsoever, not even `font-lock-comment-face',
despite that earlier rule's own capture covering the *whole* node).
A non-fence comment is untouched here (already fully painted by that
earlier rule) -- `shexc-ts-mode-convert--fence-content-bounds' returns
nil for it."
  (pcase (shexc-ts-mode-convert--fence-content-bounds node)
    (`(,_kind ,content-beg ,content-end)
     (treesit-fontify-with-override
      (treesit-node-start node) content-beg 'font-lock-comment-face override start end)
     (treesit-fontify-with-override
      content-end (treesit-node-end node) 'font-lock-comment-face override start end))))

(defun shexc-ts-mode-convert--merge-feature-lists (&rest lists)
  "Merge LISTS (each a `treesit-font-lock-feature-list') level by level."
  (let (result)
    (dotimes (i (apply #'max (mapcar #'length lists)))
      (push (delete-dups (apply #'append (mapcar (lambda (l) (nth i l)) lists))) result))
    (nreverse result)))

(defconst shexc-ts-mode-convert--fence-embedded-mode-feature
  '((turtle . turtle-ts-mode) (json . json-ts-mode))
  "Which Emacs feature/file to `require' for each embedded LANG, lazily
\(see `shexc-ts-mode-convert--require-embedded-mode').")

(defun shexc-ts-mode-convert--require-embedded-mode (lang)
  "If LANG's tree-sitter grammar is quietly confirmed available,
`require' its mode file and return LANG; otherwise return nil without
ever loading that file or emitting a warning -- see this section's top
Commentary on why both matter (either one alone still warns)."
  (and (treesit-ready-p lang t)
       (require (cdr (assq lang shexc-ts-mode-convert--fence-embedded-mode-feature)) nil t)
       lang))

(defun shexc-ts-mode-convert--setup-fence-embedding ()
  "Give every fence's content a real, ranged `turtle'/`json' tree-sitter
parser of its own for font-lock and indentation, merged into the
buffer's existing (single-language) settings -- see this section's
top Commentary."
  (setq shexc-ts-mode-convert--fence-embedded-ready-languages
        (delq nil (mapcar (lambda (kind+lang)
                             (shexc-ts-mode-convert--require-embedded-mode (cdr kind+lang)))
                           shexc-ts-mode-convert--fence-embedded-language)))
  (when shexc-ts-mode-convert--fence-embedded-ready-languages
    (let (font-lock-settings-by-lang indent-rules-by-lang feature-lists)
      (when (memq 'turtle shexc-ts-mode-convert--fence-embedded-ready-languages)
        (push turtle-ts-mode--font-lock-settings font-lock-settings-by-lang)
        (push turtle-ts-mode--indent-rules indent-rules-by-lang)
        (push turtle-ts-mode--font-lock-feature-list feature-lists))
      (when (memq 'json shexc-ts-mode-convert--fence-embedded-ready-languages)
        (push json-ts-mode--font-lock-settings font-lock-settings-by-lang)
        (push json-ts--indent-rules indent-rules-by-lang)
        (push '((comment constant number pair string)
                (escape-sequence) (bracket delimiter error))
              feature-lists))
      (setq-local treesit-font-lock-settings
                  (apply #'append (append font-lock-settings-by-lang
                                           (list treesit-font-lock-settings)
                                           (list (treesit-font-lock-rules
                                                  :language 'shexc
                                                  :feature 'comment
                                                  :override t
                                                  '((comment) @shexc-ts-mode-convert--fontify-fence-markers))))))
      (setq-local treesit-simple-indent-rules
                  (apply #'append treesit-simple-indent-rules indent-rules-by-lang))
      (setq-local treesit-font-lock-feature-list
                  (apply #'shexc-ts-mode-convert--merge-feature-lists
                         treesit-font-lock-feature-list feature-lists))
      (setq-local treesit-language-at-point-function
                  #'shexc-ts-mode-convert--language-at-point)
      (setq-local treesit-range-settings
                  (treesit-range-rules #'shexc-ts-mode-convert--update-embedded-ranges))
      ;; Create the embedded parsers now (rather than leaving it to
      ;; `--update-embedded-ranges''s own `treesit-parser-create' call,
      ;; the next thing `treesit-update-ranges' below triggers), so
      ;; `shexc' can be reclaimed as newest *after* them.
      (mapc #'treesit-parser-create shexc-ts-mode-convert--fence-embedded-ready-languages)
      (shexc-ts-mode-convert--reclaim-primary-parser-order)
      (treesit-update-ranges)
      (treesit-font-lock-recompute-features)
      (font-lock-flush))))

;; ---------------------------------------------------------------------
;; xref into/out of a SHEXR fence
;;
;; "Out of": `shexc-ts-mode--label-at-point' only ever consults the
;; live ShExC parse tree (explicitly, as of the fix described in this
;; section's earlier Commentary), so it always returns nil for a
;; position inside a fence's *content* -- there's no `shape_expr_label'
;; there, only `turtle' nodes.  `shexc-ts-mode-convert--identifier-at-
;; point' (registered on `shexc-ts-mode-extra-identifier-at-point-
;; functions') instead asks the embedded `turtle' parser for the
;; smallest enclosing IRI/prefixed-name/blank-node-label node at point
;; and returns *its* raw text -- e.g. hovering `<Item>' in `sx:id
;; <Item>' returns the string `"<Item>"', exactly the same string a
;; `@<Item>' shape-ref node elsewhere in the live ShExC tree would
;; have produced, so the existing (unchanged) raw-text matching in
;; `xref-backend-definitions'/`-references' picks it up with no
;; further work.  Only JSON (no analogous embedded-parser path is
;; wired up here yet) and only SHEXR fences (ShExJ has no `subject'-
;; style hoisting marker to exploit the same way -- see "Into" below)
;; are covered.
;;
;; "Into": a hoisted ShapeDecl/TripleExpr's own identity, by this
;; file's serializer convention (see shexc-shexr.el's Commentary), is
;; always the *subject* of its own top-level Turtle statement -- never
;; nested inside a `[ ... ]'.  Tree-sitter-turtle gives every top-level
;; statement's subject its own wrapper node type, conveniently named
;; `subject' (confirmed empirically against a real fence's parse tree)
;; -- so "every fenced ShapeDecl/TripleExpr declaration" is simply
;; "every `subject' node in the embedded `turtle' parser's tree",
;; independent of which specific fence each one belongs to (the
;; parser's ranges already restrict it to fence content only).  A
;; reference, symmetrically, is the object of an `sx:id' property --
;; whether that property sits on an anonymous `sx:Ref' wrapper (a
;; "real" forward reference) or directly on a hoisted node's own
;; statement (that node's self-identifying `:id', registered for the
;; same reason `shexc-shexr--node-keys' always emits it -- see that
;; section's Commentary in shexc-shexr.el) isn't distinguished here;
;; both are legitimate "occurrences of this identifier" for `find-
;; references' to surface.
;;
;; SHEXJ fences get the same two directions, by the same reasoning
;; applied to JSON instead of Turtle: a hoisted node's identity is the
;; value of its own "id" key (see shexc-shexj.el's Commentary: every
;; ShapeDecl, and any TripleExpr with an explicit ShExC `$<id>'
;; annotation, always gets one -- the exact same two cases ShExR's
;; `subject' marks), and a reference is any other IRI-shaped JSON
;; string -- in an `extends'/`shapeExprs'/`values' array entry, or a
;; `predicate'/`datatype'/`shapeExpr' pair value, etc. -- there's no
;; analogue of ShExR's anonymous `sx:Ref' wrapper to special-case here,
;; since ShExJ represents a bare forward reference as nothing more
;; than the IRI string itself.
;;
;; The one wrinkle specific to JSON: ShExJ never abbreviates -- every
;; IRI is always written out in full (`"http://a.example/Item"', never
;; `<Item>' or `ex:Item') -- so raw JSON text never directly text-
;; equals the live ShExC tree's (typically abbreviated) label text the
;; way ShExR's Turtle output happens to (see above).  `--json-iri-
;; token' bridges this by rendering a JSON value's absolute IRI back
;; into ShExC surface syntax, shortened against the buffer's own
;; PREFIX/BASE table exactly the way `shexc-ts-mode-convert-fence-to-
;; shexc' already does when decompiling -- so a JSON "id"/reference
;; value becomes directly, raw-text comparable against everything
;; else once more.

(defconst shexc-ts-mode-convert--iri-scheme-re "\\`[A-Za-z][A-Za-z0-9+.-]*:"
  "Matches the start of an absolute IRI (has a scheme) -- used to tell a
ShExJ string value that's plausibly an IRI (e.g. an `id'/`extends'/
`predicate' value) apart from one that plainly isn't (e.g. a `type'
discriminator like \"ShapeDecl\", or `nodeKind' value like \"iri\" --
neither of which ever contains a `:').")

(defun shexc-ts-mode-convert--json-string-text (node)
  "Raw unquoted text of a tree-sitter-json `string' NODE.  No escape
decoding -- adequate for the plain-ASCII IRIs this is used for; see
`shexc-shexj--unescape' for the general case this deliberately skips."
  (let ((text (treesit-node-text node t)))
    (substring text 1 (1- (length text)))))

(defun shexc-ts-mode-convert--json-iri-token (iri)
  "Render absolute IRI string IRI the way a ShExC shape label would --
shortened against the buffer's own PREFIX/BASE table when possible
(see `shexc-ts-mode-convert--shorten-iri'), else a full `<IRI>' -- so
it's directly raw-text comparable against the live ShExC tree's own
label nodes and a ShExR fence's Turtle nodes, both of which already
follow this convention."
  (or (shexc-ts-mode-convert--shorten-iri (shexc-shexj-buffer-directive-ctx) iri)
      (concat "<" iri ">")))

(defun shexc-ts-mode-convert--identifier-at-point ()
  "See this section's top Commentary.  Registered on
`shexc-ts-mode-extra-identifier-at-point-functions'."
  (pcase (treesit-language-at (point))
    ('turtle
     (when-let* ((node (treesit-node-at (point) 'turtle))
                 (term (treesit-parent-until
                        node
                        (lambda (n) (member (treesit-node-type n)
                                             '("iri_reference" "prefixed_name" "blank_node_label")))
                        t)))
       (treesit-node-text term t)))
    ('json
     (when-let* ((node (treesit-node-at (point) 'json))
                 (str (treesit-parent-until
                       node (lambda (n) (equal (treesit-node-type n) "string")) t))
                 (text (shexc-ts-mode-convert--json-string-text str))
                 ((string-match-p shexc-ts-mode-convert--iri-scheme-re text)))
       (shexc-ts-mode-convert--json-iri-token text)))))

(defun shexc-ts-mode-convert--fence-language-root (lang)
  "The embedded LANG parser's root node, or nil if no fence of the kind
that embeds LANG has activated it yet, or the grammar isn't installed
-- see `shexc-ts-mode-convert--fence-embedded-ready-languages'."
  (and (memq lang shexc-ts-mode-convert--fence-embedded-ready-languages)
       (treesit-buffer-root-node lang)))

(defun shexc-ts-mode-convert--fence-xref-definitions (identifier)
  "Xref-items for every fenced ShapeDecl/TripleExpr declaration across
all SHEXR/SHEXJ fences whose identity equals IDENTIFIER: a Turtle
`subject' node (SHEXR) or an `id' pair's value (SHEXJ) -- see this
section's top Commentary.  Registered on
`shexc-ts-mode-extra-xref-definitions-functions'."
  (append
   (when-let* ((root (shexc-ts-mode-convert--fence-language-root 'turtle)))
     (mapcar #'rdf-core-xref-make
             (seq-filter (lambda (n) (equal (treesit-node-text n t) identifier))
                         (treesit-query-capture root '((subject) @s) nil nil t))))
   (when-let* ((root (shexc-ts-mode-convert--fence-language-root 'json)))
     (mapcar (lambda (p) (rdf-core-xref-make (treesit-node-child-by-field-name p "value")))
             (seq-filter
              (lambda (p)
                (and (equal (shexc-ts-mode-convert--json-string-text
                             (treesit-node-child-by-field-name p "key"))
                            "id")
                     (equal (shexc-ts-mode-convert--json-iri-token
                             (shexc-ts-mode-convert--json-string-text
                              (treesit-node-child-by-field-name p "value")))
                            identifier)))
              (treesit-query-capture root '((pair) @p) nil nil t))))))

(defun shexc-ts-mode-convert--fence-xref-references (identifier)
  "Xref-items for every fenced reference to IDENTIFIER across all SHEXR/
SHEXJ fences: a Turtle `sx:id' property's object (SHEXR) or any IRI-
shaped JSON string (SHEXJ) -- see this section's top Commentary.
Registered on `shexc-ts-mode-extra-xref-references-functions'."
  (append
   (when-let* ((root (shexc-ts-mode-convert--fence-language-root 'turtle)))
     (let (items pred-text)
       (dolist (cap (treesit-query-capture
                     root '((property (predicate) @pred (object_list (_) @obj))) nil nil))
         (pcase (car cap)
           ('pred (setq pred-text (treesit-node-text (cdr cap) t)))
           ('obj (when (and (equal pred-text "sx:id") (equal (treesit-node-text (cdr cap) t) identifier))
                   (push (rdf-core-xref-make (cdr cap)) items)))))
       (nreverse items)))
   (when-let* ((root (shexc-ts-mode-convert--fence-language-root 'json)))
     (mapcan
      (lambda (n)
        (let ((text (shexc-ts-mode-convert--json-string-text n)))
          (when (and (string-match-p shexc-ts-mode-convert--iri-scheme-re text)
                     (equal (shexc-ts-mode-convert--json-iri-token text) identifier))
            (list (rdf-core-xref-make n)))))
      (treesit-query-capture root '((string) @s) nil nil t)))))

(defun shexc-ts-mode-convert--setup ()
  (add-hook 'flymake-diagnostic-functions #'shexc-ts-mode-convert--flymake-backend nil t)
  (add-hook 'shexc-ts-mode-extra-declared-shape-labels-functions
            #'shexc-ts-mode-convert--fenced-decl-labels nil t)
  (add-hook 'shexc-ts-mode-extra-identifier-at-point-functions
            #'shexc-ts-mode-convert--identifier-at-point nil t)
  (add-hook 'shexc-ts-mode-extra-xref-definitions-functions
            #'shexc-ts-mode-convert--fence-xref-definitions nil t)
  (add-hook 'shexc-ts-mode-extra-xref-references-functions
            #'shexc-ts-mode-convert--fence-xref-references nil t)
  (shexc-ts-mode-convert--setup-fence-embedding))

;;;###autoload
(add-hook 'shexc-ts-mode-hook #'shexc-ts-mode-convert--setup)

(provide 'shexc-ts-mode-convert)

;;; shexc-ts-mode-convert.el ends here
