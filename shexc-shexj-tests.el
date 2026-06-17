;;; shexc-shexj-tests.el --- ERT suite for shexc-shexj.el against shexTest -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-4-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Validates `shexc-shexj.el's compiler/decompiler/JSON-adapter against
;; the shexSpec/shexTest corpus: https://github.com/shexSpec/shexTest
;;
;; Clone it somewhere local, then either:
;;
;;   (setq shexc-shexj-test-shextest-path "/path/to/shexTest")
;;   (require 'shexc-shexj-tests)
;;
;; or, if shexc-shexj-tests is already loaded, customize the variable
;; and call `shexc-shexj-test-regenerate' to (re)generate the per-fixture
;; tests.  With the variable unset, a single test reports as skipped
;; rather than failing, so `M-x ert' stays clean for contributors
;; without a local shexTest checkout.
;;
;; Each manifest entry generates one `ert-deftest':
;; - compiles the `.shex' fixture and compares (order-insensitively)
;;   against the `.json' fixture parsed via `shexc-shexj-from-json' --
;;   the real semantic-correctness oracle;
;; - decompiles that compiled value-tree back to ShExC and recompiles
;;   it, checking the value-tree is unchanged (idempotence) -- comments/
;;   whitespace/prefix-shorthand loss is fine, semantic drift is not.
;; Entries not marked `mf:Approved' in the manifest are still defined as
;; tests (so they show up and can be inspected) but `ert-skip', so they
;; can't fail the suite.

;;; Code:

(require 'ert)
(require 'json)
(require 'treesit)
(require 'shexc-ts-mode)
(require 'shexc-shexj)

(defgroup shexc-shexj-test nil
  "ERT suite configuration for `shexc-shexj'."
  :group 'shexc-ts)

(defcustom shexc-shexj-test-shextest-path nil
  "Path to a local clone of https://github.com/shexSpec/shexTest.
When nil, the ERT suite defines a single test that reports itself
skipped rather than generating (or failing) the full per-fixture suite."
  :type '(choice (const :tag "Not configured" nil) directory)
  :group 'shexc-shexj-test)

(defun shexc-shexj-test--manifest-entries ()
  (let* ((path (expand-file-name "schemas/manifest.jsonld" shexc-shexj-test-shextest-path))
         (text (with-temp-buffer (insert-file-contents path) (buffer-string)))
         (data (json-parse-string text :object-type 'plist :array-type 'list)))
    (plist-get (car (plist-get data :@graph)) :entries)))

(defun shexc-shexj-test--normalize (v)
  "Recursively sort plist keys, for `equal' comparisons that don't care
about emission order."
  (cond
   ((and (consp v) (keywordp (car v)))
    (let ((pairs (shexc-shexj--plist-pairs v)))
      (apply #'append
             (mapcar (lambda (kv) (list (car kv) (shexc-shexj-test--normalize (cdr kv))))
                     (sort pairs (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))))
   ((listp v) (mapcar #'shexc-shexj-test--normalize v))
   (t v)))

(defun shexc-shexj-test--compile-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (shexc-ts-mode)
    (treesit-buffer-root-node) ; force a parse
    (shexc-shexj-compile-buffer)))

(defun shexc-shexj-test--compile-text (text)
  (with-temp-buffer
    (insert text)
    (shexc-ts-mode)
    (treesit-buffer-root-node)
    (shexc-shexj-compile-buffer)))

(defconst shexc-shexj-test--known-unsupported
  '("1literalPattern_with_ascii_boundaries" "1literalPattern_with_all_controls"
    "1val1STRING_LITERAL1_with_ascii_boundaries" "1val1STRING_LITERAL1_with_all_controls")
  "Fixtures whose source contains raw NUL/C0-control bytes inside a
string or regexp literal.  This isn't a `shexc-shexj' bug: tree-sitter-
shexc's C lexer itself produces an ERROR node for these (verified via
the standalone `tree-sitter parse' CLI, independent of this package) --
almost certainly because tree-sitter's C internals use a NUL-terminated
buffer representation. Out of scope to fix here (it's the grammar
repo's lexer, not this package); skipped with a clear reason instead of
failing opaquely.")

(defun shexc-shexj-test--run-fixture (shex-file json-file approved name)
  (unless approved (ert-skip "manifest status is not mf:Approved"))
  (when (member name shexc-shexj-test--known-unsupported)
    (ert-skip "known tree-sitter-shexc lexer limitation (raw NUL/control bytes) -- see shexc-shexj-test--known-unsupported"))
  (let* ((shex-path (expand-file-name (concat "schemas/" shex-file) shexc-shexj-test-shextest-path))
         (json-path (expand-file-name (concat "schemas/" json-file) shexc-shexj-test-shextest-path))
         (compiled (shexc-shexj-test--compile-file shex-path))
         (expected (shexc-shexj-from-json (with-temp-buffer (insert-file-contents json-path) (buffer-string)))))
    (should (equal (shexc-shexj-test--normalize expected) (shexc-shexj-test--normalize compiled)))
    (let* ((decompiled (shexc-shexj-decompile compiled))
           (recompiled (shexc-shexj-test--compile-text decompiled)))
      (should (equal (shexc-shexj-test--normalize compiled) (shexc-shexj-test--normalize recompiled))))))

(defun shexc-shexj-test--sanitize-name (name)
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" name))

;;;###autoload
(defun shexc-shexj-test-regenerate ()
  "(Re)define one `ert-deftest' per shexTest schemas/ manifest entry."
  (interactive)
  (if (not shexc-shexj-test-shextest-path)
      (eval '(ert-deftest shexc-shexj-test-shextest-not-configured ()
               (ert-skip "shexc-shexj-test-shextest-path is unset -- see shexc-shexj-tests.el header"))
            t)
    (dolist (entry (shexc-shexj-test--manifest-entries))
      (let* ((name (plist-get entry :name))
             (status (plist-get entry :status))
             (shex-file (plist-get entry :shex))
             (json-file (plist-get entry :json)))
        (when (and shex-file json-file)
          (eval `(ert-deftest ,(intern (format "shexc-shexj-test-%s" (shexc-shexj-test--sanitize-name name))) ()
                   :tags '(shexc-shexj-shextest)
                   (shexc-shexj-test--run-fixture ,shex-file ,json-file ,(equal status "mf:Approved") ,name))
                t))))))

(shexc-shexj-test-regenerate)

(provide 'shexc-shexj-tests)
;;; shexc-shexj-tests.el ends here
