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
;; The converted region is fenced as a run of ordinary `#'-prefixed line
;; comments with sentinel markers:
;;
;;   # shexc-ts-mode:BEGIN-SHEXJ <id> lines=N
;;   # <one line of pretty-printed JSON>
;;   # ...
;;   # shexc-ts-mode:END-SHEXJ <id> lines=N
;;
;; (`-SHEXR' for Turtle.)  Since `comment' is declared as a tree-sitter
;; `extras' token (grammar.js), this keeps the rest of the file's parse
;; tree completely unaffected -- no ERROR nodes, nothing to teach font-
;; lock/indent/xref/flymake about -- and answers "what happens on save"
;; trivially: it's just text, byte for byte, like any other comment.
;; The `lines=N' count is cheap insurance against a truncated/corrupted
;; fence being silently mis-parsed.
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

(defconst shexc-ts-mode-convert--begin-re
  "^# shexc-ts-mode:BEGIN-\\(SHEXJ\\|SHEXR\\) \\([0-9]+\\) lines=\\([0-9]+\\)$")
(defconst shexc-ts-mode-convert--end-re
  "^# shexc-ts-mode:END-\\(SHEXJ\\|SHEXR\\) \\([0-9]+\\) lines=\\([0-9]+\\)$")

(defvar-local shexc-ts-mode-convert--next-id 0
  "Per-buffer counter for fence ids.  Cosmetic only -- the id has no
semantic meaning, it just makes two adjacent fences visually
distinguishable and a stray BEGIN/END mismatch easier to spot by eye.")

(defun shexc-ts-mode-convert--fence-text (kind id text)
  "KIND is \"SHEXJ\" or \"SHEXR\"; TEXT is the pretty-printed content."
  (let* ((lines (split-string text "\n"))
         (lines (if (and lines (string-empty-p (car (last lines)))) (butlast lines) lines))
         (n (length lines)))
    (concat
     (format "# shexc-ts-mode:BEGIN-%s %d lines=%d\n" kind id n)
     (mapconcat (lambda (l) (concat "# " l "\n")) lines "")
     (format "# shexc-ts-mode:END-%s %d lines=%d" kind id n))))

(defun shexc-ts-mode-convert--strip-fence-prefixes (beg end)
  "Strip the `# ' line-comment prefix from each line in the buffer span
[BEG,END).  Tolerates a line missing its prefix (e.g. a hand-edit that
deleted it) by stripping a bare `#' or nothing at all, rather than
hard-failing here -- malformed *content* under the stripped prefix is
caught later by the JSON/Turtle parser itself."
  (let ((lines (seq-remove #'string-empty-p
                            (split-string (buffer-substring-no-properties beg end) "\n"))))
    (mapconcat (lambda (l) (cond ((string-prefix-p "# " l) (substring l 2))
                                  ((string-prefix-p "#" l) (substring l 1))
                                  (t l)))
               lines "\n")))

(defun shexc-ts-mode-convert--fence-at (pos)
  "Return (KIND ID BEG END CONTENT-BEG CONTENT-END) for the fence whose
BEGIN..END span contains POS, or nil if POS isn't inside one, or the
fence's BEGIN/END markers don't match (id, kind, or `lines=N' count) --
treated the same as \"no fence here\" rather than guessing which part of
a corrupted fence to trust.

BEG is the start of the BEGIN line; END is the end of the END line's own
text, deliberately *not* past its trailing newline -- mirroring
`shexc-ts-mode-convert--fence-text's deliberate lack of one, so that
whatever followed the fence in the original buffer (a blank line, a
following declaration, ...) is left entirely outside [BEG,END) and
`shexc-ts-mode-convert--replace-fence' never has to either invent or
swallow a newline to compensate."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let (begin-pos kind id n (first t))
      (catch 'done
        (while t
          (cond
           ((looking-at shexc-ts-mode-convert--begin-re)
            (setq begin-pos (point) kind (match-string 1) id (match-string 2)
                  n (string-to-number (match-string 3)))
            (throw 'done nil))
           ;; An END line only means "gone too far" on a line we've moved
           ;; to while scanning backward -- POS itself may legitimately
           ;; start out sitting on its own fence's END line.
           ((and (not first) (looking-at shexc-ts-mode-convert--end-re)) (throw 'done nil))
           ((bobp) (throw 'done nil))
           (t (setq first nil) (forward-line -1)))))
      (when begin-pos
        (forward-line 1)
        (let ((content-beg (point)))
          (forward-line n)
          (when (and (looking-at shexc-ts-mode-convert--end-re)
                     (equal (match-string 1) kind) (equal (match-string 2) id)
                     (= (string-to-number (match-string 3)) n))
            (let ((content-end (point)) (fence-end (match-end 0)))
              (when (<= begin-pos pos fence-end)
                (list kind id begin-pos fence-end content-beg content-end)))))))))

;; ---------------------------------------------------------------------
;; Convert-to-fence
;; ---------------------------------------------------------------------

(defun shexc-ts-mode-convert--target ()
  "Return (BEG END VALUE-TREE) for the shape/schema to convert at point:
the active region, snapped outward to the smallest enclosing
`shape_expr_decl' if the region doesn't already align with one (and to
the whole buffer if no single decl encloses it); else the
`shape_expr_decl' at point; else the whole buffer."
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
      (list (point-min) (point-max) (shexc-shexj-compile-buffer)))))

(defun shexc-ts-mode-convert--do (renderer kind)
  (pcase-let ((`(,beg ,end ,tree) (shexc-ts-mode-convert--target)))
    (let* ((text (funcall renderer tree))
           (id (cl-incf shexc-ts-mode-convert--next-id))
           (fence (shexc-ts-mode-convert--fence-text kind id text)))
      (delete-region beg end)
      (goto-char beg)
      (insert fence)
      (indent-region beg (point)))))

;;;###autoload
(defun shexc-ts-mode-convert-to-shexj ()
  "Convert the shape/schema at point (or active region) to ShExJ, in
place, fenced as `#'-comments.  See this file's Commentary for the
fence format and `shexc-ts-mode-convert--target' for what \"the shape/
schema at point\" means."
  (interactive)
  (shexc-ts-mode-convert--do #'shexc-shexj-to-json "SHEXJ"))

;;;###autoload
(defun shexc-ts-mode-convert-to-shexr ()
  "Convert the shape/schema at point (or active region) to ShExR
\(canonical Turtle\), in place, fenced as `#'-comments."
  (interactive)
  (shexc-ts-mode-convert--do #'shexc-shexr-serialize "SHEXR"))

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
  (pcase-let ((`(,kind ,_id ,_beg ,_end ,content-beg ,content-end) fence))
    (shexc-ts-mode-convert--parse-fence-content
     kind (shexc-ts-mode-convert--strip-fence-prefixes content-beg content-end))))

(defun shexc-ts-mode-convert--replace-fence (fence new-text)
  "Replace the whole of FENCE (BEGIN line through END line inclusive)
with NEW-TEXT, re-indented.  Whatever followed the fence in the buffer
\(a blank line, the next declaration, ...\) sits immediately past END
\(see `shexc-ts-mode-convert--fence-at') and is left untouched, so
NEW-TEXT must supply no trailing newline of its own -- e.g.
`shexc-shexj-decompile' always ends its output with one, unlike
`shexc-ts-mode-convert--fence-text' -- or that original separator would
be pushed one line further out, silently swallowing a blank line."
  (pcase-let ((`(,_kind ,_id ,beg ,end ,_content-beg ,_content-end) fence))
    (delete-region beg end)
    (goto-char beg)
    (insert (if (string-suffix-p "\n" new-text) (substring new-text 0 -1) new-text))
    (indent-region beg (point))))

;; ---------------------------------------------------------------------
;; Reusing the buffer's own PREFIX/BASE declarations when decompiling
;; ---------------------------------------------------------------------

(defun shexc-ts-mode-convert--directive-ctx ()
  "A `shexc-shexj--ctx' reflecting every BASE/PREFIX declaration in the
current buffer's `shex_doc', applied in document order exactly as the
compiler would -- i.e. the BASE/PREFIX table active by the *end* of the
buffer.  Reuses the compiler's own directive-application code (rather
than re-deriving the same escaping/resolution rules here) since
base/prefix decls can only ever appear as direct top-level children of
the root."
  (let ((ctx (shexc-shexj--make-ctx)))
    (dolist (c (treesit-node-children (treesit-buffer-root-node) t))
      (pcase (treesit-node-type c)
        ("base_decl" (shexc-shexj--apply-base ctx c))
        ("prefix_decl" (shexc-shexj--apply-prefix ctx c))))
    ctx))

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
the whole fence (BEGIN line through END line inclusive).  IRIs that
match a PREFIX or BASE already declared elsewhere in the buffer are
shortened accordingly (see `shexc-ts-mode-convert--shorten-iri');
nothing is ever *added* to the buffer's own declarations."
  (interactive)
  (let ((fence (shexc-ts-mode-convert--fence-at (point))))
    (unless fence
      (user-error "No shexc-ts-mode ShExJ/ShExR fence here"))
    (let* ((ctx (shexc-ts-mode-convert--directive-ctx))
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
    (shexc-ts-mode-convert--replace-fence fence (shexc-ts-mode-convert--fence-text target-kind id text))))

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
      (shexc-ts-mode-convert--fence-to-other-fence fence "SHEXR" #'shexc-shexr-serialize))
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

(defun shexc-ts-mode-convert--flymake-fence-errors ()
  "One flymake diagnostic per malformed fenced ShExJ/ShExR block in the
current buffer."
  (let (diags)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward shexc-ts-mode-convert--begin-re nil t)
        (let* ((begin-pos (line-beginning-position))
               (fence (shexc-ts-mode-convert--fence-at begin-pos)))
          (if (not fence)
              (push (flymake-make-diagnostic
                     (current-buffer) begin-pos (line-end-position)
                     :error "shexc-ts-mode: malformed fence (missing/mismatched END marker or wrong `lines=N')")
                    diags)
            (pcase-let ((`(,kind ,_id ,_beg ,_end ,content-beg ,content-end) fence))
              (let ((content (shexc-ts-mode-convert--strip-fence-prefixes content-beg content-end)))
                (condition-case err
                    (pcase kind
                      ("SHEXJ" (shexc-shexj-from-json content))
                      ("SHEXR" (shexc-shexr-parse content)))
                  (error
                   (push (flymake-make-diagnostic
                          (current-buffer) content-beg content-end
                          :error (format "shexc-ts-mode: cannot parse fenced %s: %s"
                                          kind (error-message-string err)))
                         diags)))))))
        (goto-char (line-end-position))))
    (nreverse diags)))

(defun shexc-ts-mode-convert--flymake-backend (report-fn &rest _args)
  (funcall report-fn (shexc-ts-mode-convert--flymake-fence-errors)))

(defun shexc-ts-mode-convert--setup ()
  (add-hook 'flymake-diagnostic-functions #'shexc-ts-mode-convert--flymake-backend nil t))

;;;###autoload
(add-hook 'shexc-ts-mode-hook #'shexc-ts-mode-convert--setup)

(provide 'shexc-ts-mode-convert)

;;; shexc-ts-mode-convert.el ends here
