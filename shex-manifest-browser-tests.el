;;; shex-manifest-browser-tests.el --- ERT suite for shex-manifest-browser.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-5
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Most of this suite needs nothing beyond `yaml'/`treesit' (the
;; `shexc'/`turtle' grammars), both already required by
;; `shex-manifest-browser' itself.  A handful of tests -- anything that
;; actually validates (`shex-manifest-browser-test--run-all-on-real-
;; manifest', `-edit-round-trip-updates-result') -- need the
;; `emacs-rudof' dynamic module and report themselves skipped, exactly
;; like `shexc-shexj-tests.el's `shexTest'-corpus tests do, unless
;; `shex-validate-rudof-module-path' is already configured (see
;; `shex-validate.el's own docstring for that variable) before this
;; file is loaded.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'shex-manifest-browser)

;;; Small helpers

(defmacro shex-manifest-browser-test--with-data-buffer (text &rest body)
  "Evaluate BODY in a temp buffer holding Turtle TEXT, as `(current-buffer)'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     ,@body))

(defmacro shex-manifest-browser-test--with-schema-buffer (text &rest body)
  "Evaluate BODY in a temp buffer holding ShExC TEXT with a `shexc'
tree-sitter parser already created, as `(current-buffer)'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (treesit-parser-create 'shexc)
     ,@body))

(defun shex-manifest-browser-test--assoc (node shape &optional negated)
  (make-shex-manifest-browser--assoc :node-text node :shape-text shape :negated negated))

(defun shex-manifest-browser-test--rudof-available-p ()
  (and (boundp 'shex-validate-rudof-module-path)
       shex-validate-rudof-module-path
       (file-exists-p (expand-file-name shex-validate-rudof-module-path))))

(defconst shex-manifest-browser-test--manifest-path
  (expand-file-name "tests/tests-manifest.yaml"
                     (file-name-directory (or load-file-name buffer-file-name)))
  "This package's own `tests/tests-manifest.yaml' fixture.")

;;; `shex-manifest-browser--get'/`--resolve-path'

(ert-deftest shex-manifest-browser-test-get ()
  (should (equal (shex-manifest-browser--get '((schemaLabel . "foo") (status . "conformant")) 'schemaLabel)
                 "foo"))
  (should (null (shex-manifest-browser--get '((schemaLabel . "foo")) 'dataLabel))))

(ert-deftest shex-manifest-browser-test-resolve-path ()
  (should (equal (shex-manifest-browser--resolve-path "/a/b/" "../c/d.shex")
                 "/a/c/d.shex")))

;;; `shex-manifest-browser--load-manifest' against the real fixture

(ert-deftest shex-manifest-browser-test-load-manifest-real-file ()
  (let ((entries (shex-manifest-browser--load-manifest shex-manifest-browser-test--manifest-path)))
    (should (= (length entries) 6))
    (should (equal (shex-manifest-browser--get (nth 0 entries) 'schemaLabel)
                   "shex-validate BASE resolution"))
    (should (equal (shex-manifest-browser--get (nth 0 entries) 'schemaURL)
                   "../example/person-extends.shex"))
    (should (equal (shex-manifest-browser--get (nth 3 entries) 'status) "conformant"))
    (should (equal (shex-manifest-browser--get (nth 4 entries) 'status) "nonconformant"))
    (should (string-search "FOCUS" (shex-manifest-browser--get (nth 3 entries) 'queryMap)))))

;;; `shex-manifest-browser--split-top-level'/`--split-association'

(ert-deftest shex-manifest-browser-test-split-top-level-simple ()
  (should (equal (shex-manifest-browser--split-top-level "a,b,c" ?,) '("a" "b" "c"))))

(ert-deftest shex-manifest-browser-test-split-top-level-respects-brackets ()
  ;; a comma inside `{...}' or `<...>' must not split the association.
  (should (equal (shex-manifest-browser--split-top-level "{FOCUS ex:p _}@START,<a>@<b>" ?,)
                 '("{FOCUS ex:p _}@START" "<a>@<b>"))))

(ert-deftest shex-manifest-browser-test-split-association ()
  (should (equal (shex-manifest-browser--split-association "<http://a/n>@<http://a/S>")
                 (cons "<http://a/n>" "<http://a/S>"))))

(ert-deftest shex-manifest-browser-test-split-association-picks-rightmost-at ()
  ;; the node selector itself may be a `{...}' pattern containing no
  ;; `@' at depth 0, so the rightmost (only) `@' is unambiguous.
  (should (equal (shex-manifest-browser--split-association "{FOCUS ex:p _}@START")
                 (cons "{FOCUS ex:p _}" "START"))))

(ert-deftest shex-manifest-browser-test-split-association-no-at-errors ()
  (should-error (shex-manifest-browser--split-association "<http://a/n>")))

;;; `shex-manifest-browser--parse-shape-label'

(ert-deftest shex-manifest-browser-test-parse-shape-label-plain ()
  (should (equal (shex-manifest-browser--parse-shape-label "<http://a/S>")
                 (cons nil "<http://a/S>"))))

(ert-deftest shex-manifest-browser-test-parse-shape-label-negated ()
  (should (equal (shex-manifest-browser--parse-shape-label "!<http://a/S>")
                 (cons t "<http://a/S>"))))

;;; `shex-manifest-browser--strip-angle-brackets'

(ert-deftest shex-manifest-browser-test-strip-angle-brackets ()
  (should (equal (shex-manifest-browser--strip-angle-brackets "<http://a/n>") "http://a/n"))
  (should (equal (shex-manifest-browser--strip-angle-brackets "http://a/n") "http://a/n")))

;;; `shex-manifest-browser--shapemap-text'

(ert-deftest shex-manifest-browser-test-shapemap-text ()
  (should (equal (shex-manifest-browser--shapemap-text
                   (list (shex-manifest-browser-test--assoc "<http://a/n1>" "<http://a/S1>")
                         (shex-manifest-browser-test--assoc "<http://a/n2>" "<http://a/S2>")))
                 "<http://a/n1>@<http://a/S1>,\n<http://a/n2>@<http://a/S2>")))

;;; `shex-manifest-browser--pass-p'
;;
;; SCHEMA-BUFFER/DATA-BUFFER are nil in most of these -- every NODE/
;; SHAPE here is already an absolute `<http://...>' IRI, so `--resolve-
;; node-text'/`--resolve-shape-text' have nothing to resolve regardless
;; of whether a real buffer backs them (see their own "unchanged... or
;; when [the buffer] isn't live" fallback).  The BASE-relative cases
;; below (`-resolves-relative-node-against-data-base'/`-resolves-
;; relative-shape-against-schema-base') are what actually need real
;; buffers, and use the `--with-schema-buffer'/`--with-data-buffer'
;; macros for that.

(ert-deftest shex-manifest-browser-test-pass-p-match ()
  (let* ((entry '((status . "conformant")))
         (assocs (list (shex-manifest-browser-test--assoc "<http://a/n>" "<http://a/S>")))
         (quadruples '(("http://a/n" "http://a/S" "conformant" "ok"))))
    (should (car (shex-manifest-browser--pass-p entry assocs quadruples nil nil)))))

(ert-deftest shex-manifest-browser-test-pass-p-mismatch ()
  (let* ((entry '((status . "conformant")))
         (assocs (list (shex-manifest-browser-test--assoc "<http://a/n>" "<http://a/S>")))
         (quadruples '(("http://a/n" "http://a/S" "nonconformant" "nope"))))
    (let ((result (shex-manifest-browser--pass-p entry assocs quadruples nil nil)))
      (should-not (car result))
      (should (= (length (cdr result)) 1))
      (should (string-search "expected conformant, got nonconformant" (car (cdr result)))))))

(ert-deftest shex-manifest-browser-test-pass-p-missing-result-fails ()
  (let* ((entry '((status . "conformant")))
         (assocs (list (shex-manifest-browser-test--assoc "<http://a/n>" "<http://a/S>"))))
    (let ((result (shex-manifest-browser--pass-p entry assocs '() nil nil)))
      (should-not (car result))
      (should (string-search "no result" (car (cdr result)))))))

(ert-deftest shex-manifest-browser-test-pass-p-negated-association-expects-nonconformant ()
  ;; a `!'-negated association expects "nonconformant" regardless of
  ;; the entry's own `status' -- see the real manifest's mixed FOCUS +
  ;; negated-association entries in shex-webapp's own example format.
  (let* ((entry '((status . "conformant")))
         (assocs (list (shex-manifest-browser-test--assoc "<http://a/n>" "<http://a/S>" t)))
         (quadruples '(("http://a/n" "http://a/S" "nonconformant" "correctly failed"))))
    (should (car (shex-manifest-browser--pass-p entry assocs quadruples nil nil)))))

(ert-deftest shex-manifest-browser-test-pass-p-negated-association-conformant-fails ()
  (let* ((entry '((status . "conformant")))
         (assocs (list (shex-manifest-browser-test--assoc "<http://a/n>" "<http://a/S>" t)))
         (quadruples '(("http://a/n" "http://a/S" "conformant" "shouldn't have passed"))))
    (should-not (car (shex-manifest-browser--pass-p entry assocs quadruples nil nil)))))

(ert-deftest shex-manifest-browser-test-pass-p-resolves-relative-node-against-data-base ()
  ;; Regression test for the bug reported live against tests-manifest.yaml
  ;; entry "<Person>" (schemaURL-referenced): a quadruple's own NODE is
  ;; fully resolved against BASE (confirmed against a real `rudof' run --
  ;; "<#Alice>" came back as "http://a.example/#Alice", not "#Alice"), so
  ;; matching the association's own `<>'-stripped-only node text against
  ;; it always failed, reporting "no result" even though a matching
  ;; quadruple was right there.
  (shex-manifest-browser-test--with-data-buffer "BASE <http://a.example/>\n<#Alice> a <x> .\n"
    (let* ((data-buffer (current-buffer))
           (entry '((status . "conformant")))
           (assocs (list (shex-manifest-browser-test--assoc "<#Alice>" "<http://a.example/S>")))
           (quadruples '(("http://a.example/#Alice" "http://a.example/S" "conformant" "ok"))))
      (should (car (shex-manifest-browser--pass-p entry assocs quadruples nil data-buffer))))))

(ert-deftest shex-manifest-browser-test-pass-p-resolves-relative-shape-against-schema-base ()
  ;; Same as above, for the SHAPE half of the match: a relative shape
  ;; reference (e.g. "<TGeek>") resolves against the SCHEMA's own BASE,
  ;; matching how a quadruple's own SHAPE comes back fully resolved.
  (shex-manifest-browser-test--with-schema-buffer "BASE <http://a.example/>\n<S> { }\n"
    (let* ((schema-buffer (current-buffer))
           (entry '((status . "conformant")))
           (assocs (list (shex-manifest-browser-test--assoc "<http://a.example/n>" "<S>")))
           (quadruples '(("http://a.example/n" "http://a.example/S" "conformant" "ok"))))
      (should (car (shex-manifest-browser--pass-p entry assocs quadruples schema-buffer nil))))))

(ert-deftest shex-manifest-browser-test-pass-p-disambiguates-node-tested-against-several-shapes ()
  ;; Regression test for the ORIGINAL bug report: a node's schema
  ;; chaining through EXTENDS (e.g. <TGeek> EXTENDS @<Item>) makes
  ;; `rudof-validate-shex' report one quadruple per shape actually
  ;; evaluated, not just the one requested -- matching by NODE alone
  ;; risked picking a *different* shape's status.  The "wrong" one
  ;; (Item, nonconformant) deliberately sorts first here; only the
  ;; (node . shape) match, preferred over the plain NODE fallback,
  ;; gets this right regardless of quadruple order.
  (shex-manifest-browser-test--with-schema-buffer "BASE <http://a.example/>\n<Item> { }\n<TGeek> EXTENDS @<Item> { }\n"
    (let* ((schema-buffer (current-buffer))
           (entry '((status . "conformant")))
           (assocs (list (shex-manifest-browser-test--assoc "<http://a.example/n>" "<TGeek>")))
           (quadruples '(("http://a.example/n" "http://a.example/Item" "nonconformant" "extends-chain")
                         ("http://a.example/n" "http://a.example/TGeek" "conformant" "own-check"))))
      (should (car (shex-manifest-browser--pass-p entry assocs quadruples schema-buffer nil))))))

;;; `shex-manifest-browser--content-buffer' reuse/reap policy

(ert-deftest shex-manifest-browser-test-content-buffer-reused-when-unmodified ()
  (let ((b1 (shex-manifest-browser--content-buffer 'data "reuse-test" "one" nil "/tmp/")))
    (unwind-protect
        (let ((b2 (shex-manifest-browser--content-buffer 'data "reuse-test" "two" nil "/tmp/")))
          (should (eq b1 b2))
          (should (equal (with-current-buffer b2 (buffer-string)) "two")))
      (kill-buffer b1))))

(ert-deftest shex-manifest-browser-test-content-buffer-sets-default-directory ()
  "A freshly created buffer otherwise inherits `default-directory'
unpredictably from whatever buffer happened to be current -- see the
regression this guards against in this file's own history."
  (let ((b1 (shex-manifest-browser--content-buffer 'data "dir-test" "x" nil "/tmp/some/dir/")))
    (unwind-protect
        (should (equal (with-current-buffer b1 default-directory) "/tmp/some/dir/"))
      (kill-buffer b1))))

(ert-deftest shex-manifest-browser-test-content-buffer-new-when-modified ()
  (let ((b1 (shex-manifest-browser--content-buffer 'data "modified-test" "one" nil "/tmp/")))
    (unwind-protect
        (progn
          (with-current-buffer b1 (insert "operator edit"))
          (let ((b2 (shex-manifest-browser--content-buffer 'data "modified-test" "two" nil "/tmp/")))
            (unwind-protect
                (progn
                  (should-not (eq b1 b2))
                  (should (equal (with-current-buffer b1 (buffer-string)) "operator editone"))
                  (should (equal (with-current-buffer b2 (buffer-string)) "two")))
              (kill-buffer b2))))
      (kill-buffer b1))))

;;; `shex-manifest-browser--resolve-querymap': plain pass-through and `{FOCUS ...}'

(ert-deftest shex-manifest-browser-test-resolve-querymap-plain-passthrough ()
  (shex-manifest-browser-test--with-data-buffer "PREFIX ex: <http://a.example/ns#>\n<http://a.example/n> ex:p ex:o .\n"
    (let ((assocs (shex-manifest-browser--resolve-querymap "<http://a.example/n>@<http://a.example/S>" (current-buffer))))
      (should (= (length assocs) 1))
      (should (equal (shex-manifest-browser--assoc-node-text (car assocs)) "<http://a.example/n>"))
      (should (equal (shex-manifest-browser--assoc-shape-text (car assocs)) "<http://a.example/S>"))
      (should-not (shex-manifest-browser--assoc-negated (car assocs))))))

(ert-deftest shex-manifest-browser-test-resolve-querymap-negation-stripped-and-tracked ()
  (shex-manifest-browser-test--with-data-buffer "PREFIX ex: <http://a.example/ns#>\n<http://a.example/n> ex:p ex:o .\n"
    (let ((assocs (shex-manifest-browser--resolve-querymap "<http://a.example/n>@!<http://a.example/S>" (current-buffer))))
      (should (shex-manifest-browser--assoc-negated (car assocs)))
      (should (equal (shex-manifest-browser--assoc-shape-text (car assocs)) "<http://a.example/S>"))
      ;; and the rendered ShapeMap text must never carry the "!" through
      ;; to `read-shapemap', which doesn't parse it.
      (should-not (string-search "!" (shex-manifest-browser--shapemap-text assocs))))))

(ert-deftest shex-manifest-browser-test-resolve-querymap-focus-subject-position ()
  (shex-manifest-browser-test--with-data-buffer
      "PREFIX ex: <http://a.example/ns#>\n<http://a.example/o1> ex:status \"final\" .\n"
    (let ((assocs (shex-manifest-browser--resolve-querymap "{FOCUS ex:status _}@START" (current-buffer))))
      (should (= (length assocs) 1))
      (should (equal (shex-manifest-browser--assoc-node-text (car assocs)) "<http://a.example/o1>"))
      (should (equal (shex-manifest-browser--assoc-shape-text (car assocs)) "START")))))

(ert-deftest shex-manifest-browser-test-resolve-querymap-focus-object-position ()
  (shex-manifest-browser-test--with-data-buffer
      "PREFIX ex: <http://a.example/ns#>\n<http://a.example/team1> ex:manager <http://a.example/mgr1> .\n"
    (let ((assocs (shex-manifest-browser--resolve-querymap "{_ ex:manager FOCUS}@START" (current-buffer))))
      (should (= (length assocs) 1))
      (should (equal (shex-manifest-browser--assoc-node-text (car assocs)) "<http://a.example/mgr1>")))))

(ert-deftest shex-manifest-browser-test-resolve-querymap-focus-multi-match ()
  (shex-manifest-browser-test--with-data-buffer
      (concat "PREFIX ex: <http://a.example/ns#>\n"
              "<http://a.example/o1> ex:status \"final\" .\n"
              "<http://a.example/o2> ex:status \"final\" .\n")
    (let ((assocs (shex-manifest-browser--resolve-querymap "{FOCUS ex:status _}@START" (current-buffer))))
      (should (= (length assocs) 2))
      (should (equal (sort (mapcar #'shex-manifest-browser--assoc-node-text assocs) #'string<)
                     '("<http://a.example/o1>" "<http://a.example/o2>"))))))

(ert-deftest shex-manifest-browser-test-resolve-querymap-multiple-associations ()
  (shex-manifest-browser-test--with-data-buffer
      "PREFIX ex: <http://a.example/ns#>\n<http://a.example/n1> ex:p ex:o .\n<http://a.example/n2> ex:p ex:o .\n"
    (let ((assocs (shex-manifest-browser--resolve-querymap
                    "<http://a.example/n1>@<http://a.example/S1>,\n<http://a.example/n2>@<http://a.example/S2>"
                    (current-buffer))))
      (should (= (length assocs) 2))
      (should (equal (mapcar #'shex-manifest-browser--assoc-node-text assocs)
                     '("<http://a.example/n1>" "<http://a.example/n2>"))))))

;;; `shex-manifest-browser--entry-start-position'/`--entry-index-at-point'

(defconst shex-manifest-browser-test--fake-yaml
  "---
# a leading comment, before any entry
- schemaLabel: first
  status: conformant
- schemaLabel: second
  status: nonconformant
- schemaLabel: third
  status: conformant
")

(ert-deftest shex-manifest-browser-test-entry-start-position ()
  (with-temp-buffer
    (insert shex-manifest-browser-test--fake-yaml)
    (dotimes (index 3)
      (goto-char (shex-manifest-browser--entry-start-position index))
      (should (looking-at (format "- schemaLabel: %s" (nth index '("first" "second" "third"))))))))

(ert-deftest shex-manifest-browser-test-entry-start-position-out-of-range-errors ()
  (with-temp-buffer
    (insert shex-manifest-browser-test--fake-yaml)
    (should-error (shex-manifest-browser--entry-start-position 5))))

(ert-deftest shex-manifest-browser-test-entry-index-at-point ()
  (with-temp-buffer
    (insert shex-manifest-browser-test--fake-yaml)
    ;; before any entry (still in the leading comment) -> nil
    (goto-char (point-min))
    (should (null (shex-manifest-browser--entry-index-at-point)))
    ;; anywhere within the Nth entry's block, including mid-block -> N
    (goto-char (shex-manifest-browser--entry-start-position 1))
    (should (= (shex-manifest-browser--entry-index-at-point) 1))
    (forward-line 1) ; still within entry 1's own "status:" line
    (should (= (shex-manifest-browser--entry-index-at-point) 1))
    (goto-char (point-max))
    (should (= (shex-manifest-browser--entry-index-at-point) 2))))

(ert-deftest shex-manifest-browser-test-entry-position-round-trip ()
  (with-temp-buffer
    (insert shex-manifest-browser-test--fake-yaml)
    (dotimes (index 3)
      (goto-char (shex-manifest-browser--entry-start-position index))
      (forward-line 1) ; move off the exact "- " line, still inside the entry
      (should (= (shex-manifest-browser--entry-index-at-point) index)))))

;;; `shex-manifest-browser--schema-shape-iri'

(ert-deftest shex-manifest-browser-test-schema-shape-iri-resolves-start ()
  (shex-manifest-browser-test--with-schema-buffer
      "BASE <http://a.example/>\nstart = @<Item>\n<Item> { <http://a.example/ns#id> . }\n"
    (should (equal (shex-manifest-browser--schema-shape-iri (current-buffer) "Start")
                   "http://a.example/Item"))))

(ert-deftest shex-manifest-browser-test-schema-shape-iri-passthrough-when-not-start ()
  (shex-manifest-browser-test--with-schema-buffer
      "BASE <http://a.example/>\nstart = @<Item>\n<Item> { <http://a.example/ns#id> . }\n"
    (should (equal (shex-manifest-browser--schema-shape-iri (current-buffer) "http://a.example/Other")
                   "http://a.example/Other"))))

(ert-deftest shex-manifest-browser-test-schema-shape-iri-no-start-directive ()
  (shex-manifest-browser-test--with-schema-buffer
      "BASE <http://a.example/>\n<Item> { <http://a.example/ns#id> . }\n"
    ;; no `start' at all -- "Start" has nothing to resolve to, so it's
    ;; returned unchanged (harmlessly not found by any shape lookup).
    (should (equal (shex-manifest-browser--schema-shape-iri (current-buffer) "Start") "Start"))))

;;; `shex-manifest-browser--highlight-quadruples'

(ert-deftest shex-manifest-browser-test-highlight-quadruples-creates-overlays ()
  (shex-manifest-browser-test--with-schema-buffer
      "BASE <http://a.example/>\n<Item> { <http://a.example/ns#id> . }\n"
    (let ((schema-buffer (current-buffer)))
      (shex-manifest-browser-test--with-data-buffer
          "PREFIX ex: <http://a.example/ns#>\n<http://a.example/n> ex:id ex:x .\n"
        (let* ((data-buffer (current-buffer))
               (positions (make-hash-table :test 'equal))
               (quadruples '(("http://a.example/n" "http://a.example/Item" "conformant" "ok"))))
          (rdf-turtle-parse-buffer nil positions)
          (shex-manifest-browser--clear-highlights)
          (unwind-protect
              (progn
                (shex-manifest-browser--highlight-quadruples quadruples schema-buffer data-buffer positions)
                (should (= (length shex-manifest-browser--highlight-overlays) 2))
                (let ((texts (mapcar (lambda (ov)
                                       (with-current-buffer (overlay-buffer ov)
                                         (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
                                     shex-manifest-browser--highlight-overlays)))
                  (should (member "<Item>" texts))
                  (should (member "<http://a.example/n>" texts)))
                (should (seq-every-p (lambda (ov) (eq (overlay-get ov 'face) 'shex-validate-highlight))
                                     shex-manifest-browser--highlight-overlays)))
            (shex-manifest-browser--clear-highlights)))))))

;;; `shex-manifest-browser--show-side-by-side'

(ert-deftest shex-manifest-browser-test-show-side-by-side-layout ()
  (let ((browser-buffer (generate-new-buffer "shex-manifest-browser-test-browser"))
        (schema-buffer (generate-new-buffer "shex-manifest-browser-test-schema"))
        (data-buffer (generate-new-buffer "shex-manifest-browser-test-data")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) browser-buffer)
          (shex-manifest-browser--show-side-by-side (selected-window) schema-buffer data-buffer)
          (should (= (length (window-list)) 3))
          (should (eq (window-buffer (selected-window)) browser-buffer))
          (let ((other-buffers (mapcar #'window-buffer (remq (selected-window) (window-list)))))
            (should (memq schema-buffer other-buffers))
            (should (memq data-buffer other-buffers))))
      (kill-buffer browser-buffer)
      (kill-buffer schema-buffer)
      (kill-buffer data-buffer))))

(ert-deftest shex-manifest-browser-test-show-side-by-side-replaces-previous-windows ()
  "Re-running doesn't pile up extra windows from a previous arrangement."
  (let ((browser-buffer (generate-new-buffer "shex-manifest-browser-test-browser2"))
        (schema-buffer (generate-new-buffer "shex-manifest-browser-test-schema2"))
        (data-buffer (generate-new-buffer "shex-manifest-browser-test-data2"))
        (stray-buffer (generate-new-buffer "shex-manifest-browser-test-stray")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) browser-buffer)
          (split-window (selected-window) nil 'right)
          (split-window (selected-window) nil 'right)
          (should (= (length (window-list)) 3)) ; browser + 2 leftover windows first
          (shex-manifest-browser--show-side-by-side (selected-window) schema-buffer data-buffer)
          (should (= (length (window-list)) 3))) ; still 3, not 5
      (kill-buffer browser-buffer)
      (kill-buffer schema-buffer)
      (kill-buffer data-buffer)
      (kill-buffer stray-buffer))))

;;; Full manifest run and toggle-display -- needs `shex-manifest-browser-open'

(defmacro shex-manifest-browser-test--with-open-manifest (var &rest body)
  "Bind VAR to a fresh `shex-manifest-browser-open' buffer on this
package's own `tests/tests-manifest.yaml' fixture for BODY, then kill
every buffer that didn't already exist before this macro ran (the
browser buffer itself, plus whatever schema/data/queryMap/shapemap/
results buffers -- or, for a `schemaURL'/`dataURL' entry, real
file-visiting buffers -- running an entry created), leaving anything
the operator already had open untouched."
  (declare (indent 1))
  (let ((before (gensym "before")))
    `(let ((,before (buffer-list)))
       (unwind-protect
           (let ((,var (progn (shex-manifest-browser-open shex-manifest-browser-test--manifest-path)
                               (current-buffer))))
             ,@body)
         ;; A buffer `shex-manifest-browser--enable-save-back' set up
         ;; (e.g. left modified by a test that edits a schema/data
         ;; buffer without saving) has its own buffer-local `kill-
         ;; buffer-query-functions' entry that prompts via `y-or-n-p'
         ;; -- confirmed empirically that a plain `(let ((kill-buffer-
         ;; query-functions nil)) ...)' here does NOT suppress it (a
         ;; buffer-local hook value is looked up in the buffer being
         ;; killed, not shadowed by a dynamic let established in
         ;; *this* buffer), so a real `y-or-n-p' prompt fires with no
         ;; tty to answer it in batch, the kill is treated as
         ;; declined, and the stale buffer leaks into the next test.
         ;; Answering "no" to "save before killing" (declining to
         ;; save -- a test's own transient edit shouldn't get written
         ;; into the real fixture file as a cleanup side effect) and
         ;; "yes" to the follow-up "kill anyway" (so the kill isn't
         ;; itself declined, which would leak the stale buffer into
         ;; the next test) via one stub, keyed on the prompt text.
         (let ((kill-buffer-query-functions nil))
           (cl-letf (((symbol-function 'y-or-n-p)
                      (lambda (prompt &rest _) (not (string-search "save before killing" prompt)))))
             (dolist (buf (buffer-list))
               (unless (memq buf ,before)
                 (ignore-errors (kill-buffer buf))))))))))

(ert-deftest shex-manifest-browser-test-open-populates-entries ()
  (shex-manifest-browser-test--with-open-manifest buf
    (with-current-buffer buf
      (should (eq major-mode 'shex-manifest-browser-mode))
      (should (= (length shex-manifest-browser--entries) 6))
      (should (null buffer-file-name)))))

(ert-deftest shex-manifest-browser-test-toggle-display-preserves-position-both-ways ()
  "The point-preservation round trip (table -> YAML -> table) needs no
validation run at all, so it's exercised regardless of whether
`emacs-rudof' is available."
  (shex-manifest-browser-test--with-open-manifest buf
    (with-current-buffer buf
      (goto-char (point-min))
      (shex-manifest-browser--goto-entry-row 3)
      (shex-manifest-browser-toggle-display) ; -> YAML, landing on entry 3
      (should shex-manifest-browser-yaml-view-mode) ; a minor mode, not major -- no `derived-mode-p'
      (should (= (shex-manifest-browser--entry-index-at-point) 3))
      (should-not buffer-read-only)
      (forward-line 2) ; move around within the same entry's block
      (shex-manifest-browser-toggle-display) ; -> table, landing back on row 3
      (should (eq major-mode 'shex-manifest-browser-mode))
      (should (equal (tabulated-list-get-id) 3)))))

(ert-deftest shex-manifest-browser-test-run-all-needs-rudof ()
  (unless (shex-manifest-browser-test--rudof-available-p)
    (ert-skip "`shex-validate-rudof-module-path' isn't configured/found -- see this file's Commentary"))
  (save-window-excursion
    (shex-manifest-browser-test--with-open-manifest buf
      (with-current-buffer buf
        (shex-manifest-browser-run-all)
        (should (= (hash-table-count shex-manifest-browser--results) 6))
        (should (cl-every #'car (hash-table-values shex-manifest-browser--results)))))))

(ert-deftest shex-manifest-browser-test-edit-round-trip-updates-result ()
  "Editing the YAML (here: breaking a previously-conformant data
literal), saving, and toggling back updates the table, and re-running
reflects the edit -- the scenario verified interactively while
building this feature.  Restores the real fixture file's original
content afterward (captured before the test touches it), since this
really does save to it (through the same `save-buffer' call the
operator's own `y'/`n' answer would); `y-or-n-p' is stubbed to answer
\"yes\" since a batch test run can't answer an interactive prompt."
  (unless (shex-manifest-browser-test--rudof-available-p)
    (ert-skip "`shex-validate-rudof-module-path' isn't configured/found -- see this file's Commentary"))
  (let ((original-content (with-temp-buffer
                             (insert-file-contents shex-manifest-browser-test--manifest-path)
                             (buffer-string))))
    (unwind-protect
        (save-window-excursion
          (shex-manifest-browser-test--with-open-manifest buf
            (with-current-buffer buf
              (shex-manifest-browser--goto-entry-row 3)
              (shex-manifest-browser-run-entry)
              (should (car (gethash 3 shex-manifest-browser--results)))
              (shex-manifest-browser--goto-entry-row 3)
              (shex-manifest-browser-toggle-display) ; -> YAML, on entry 3
              (goto-char (point-min))
              (search-forward "ex:status \"final\" .")
              (replace-match "ex:status \"bogus\" .")
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (shex-manifest-browser-toggle-display)) ; -> table, saved+reparsed for real
              (should (equal (tabulated-list-get-id) 3))
              (shex-manifest-browser-run-entry)
              (should-not (car (gethash 3 shex-manifest-browser--results))))))
      (write-region original-content nil shex-manifest-browser-test--manifest-path))))

(ert-deftest shex-manifest-browser-test-live-refresh-on-schema-edit ()
  "Editing the SCHEMA buffer directly -- not the YAML, and without
re-running the entry -- updates the table's Result column and the
node/shape highlight faces automatically: exercises
`shex-manifest-browser--on-recheck' (added to `shex-validate-after-
check-functions' by `shex-manifest-browser--run' itself, see that
function's docstring)."
  (unless (shex-manifest-browser-test--rudof-available-p)
    (ert-skip "`shex-validate-rudof-module-path' isn't configured/found -- see this file's Commentary"))
  (save-window-excursion
    (shex-manifest-browser-test--with-open-manifest buf
      (with-current-buffer buf
        ;; Entry 3: "one item, final status" -- conformant against
        ;; `<Item> { ex:status ["final" "preliminary"] }'.
        (shex-manifest-browser--goto-entry-row 3)
        (shex-manifest-browser-run-entry)
        (should (car (gethash 3 shex-manifest-browser--results)))
        (let ((schema-buffer (get-buffer "*schema: FOCUS/START queryMap resolution*"))
              (data-buffer (get-buffer "*data: one item, final status*")))
          (should schema-buffer)
          (should data-buffer)
          ;; Tighten the value set to exclude "final" -- this entry's
          ;; own data now fails, purely from a schema edit; no edit to
          ;; DATA and no explicit re-run.
          (with-current-buffer schema-buffer
            (goto-char (point-min))
            (search-forward "[\"final\" \"preliminary\"]")
            (replace-match "[\"preliminary\"]"))
          ;; The real idle timer that would normally trigger this
          ;; recheck never fires in a batch test -- call it directly,
          ;; matching `shex-validate-test-schema-edit-refreshes-data-
          ;; diagnostics''s own approach for the same reason.
          (with-current-buffer data-buffer (shex-validate--check))
          (should-not (car (gethash 3 shex-manifest-browser--results)))
          (should (memq 'shex-manifest-browser-nonconformant-highlight
                        (with-current-buffer data-buffer
                          (mapcar (lambda (ov) (overlay-get ov 'face))
                                  (overlays-in (point-min) (point-max))))))
          (should (memq 'shex-manifest-browser-nonconformant-highlight
                        (with-current-buffer schema-buffer
                          (mapcar (lambda (ov) (overlay-get ov 'face))
                                  (overlays-in (point-min) (point-max))))))
          ;; The schema's own flymake (`shex-manifest-browser--schema-
          ;; flymake') should report a real diagnostic on `<Item>''s
          ;; own declaration too -- not just an overlay highlight --
          ;; so `flymake-show-diagnostics-at-point'/`flymake-goto-
          ;; next-error' work there the same way they already do on
          ;; the data buffer's own failing node.
          (with-current-buffer schema-buffer
            (flymake-start)
            (let ((diags (flymake-diagnostics (point-min) (point-max))))
              (should (= (length diags) 1))
              (should (string-match-p "http://a.example/obs1"
                                      (flymake-diagnostic-text (car diags)))))))))))

(ert-deftest shex-manifest-browser-test-save-back-writes-block-scalar ()
  "Editing an ephemeral (inline, not `schemaURL') schema buffer directly
and saving it (`save-buffer', not the YAML view) writes the change back
into the manifest file's own `schema: |' block scalar for that entry,
leaving the rest of the file untouched -- exercises
`shex-manifest-browser--enable-save-back'/`--write-contents'/`--replace-
block-scalar'.  Restores the real fixture file's original content
afterward, like `-test-edit-round-trip-updates-result' does for the
same reason."
  (unless (shex-manifest-browser-test--rudof-available-p)
    (ert-skip "`shex-validate-rudof-module-path' isn't configured/found -- see this file's Commentary"))
  (let ((original-content (with-temp-buffer
                             (insert-file-contents shex-manifest-browser-test--manifest-path)
                             (buffer-string))))
    (unwind-protect
        (save-window-excursion
          (shex-manifest-browser-test--with-open-manifest buf
            (with-current-buffer buf
              ;; Entry 3: "one item, final status" -- inline `schema:'.
              (shex-manifest-browser--goto-entry-row 3)
              (shex-manifest-browser-run-entry)
              (let ((schema-buffer (get-buffer "*schema: FOCUS/START queryMap resolution*")))
                (should schema-buffer)
                (with-current-buffer schema-buffer
                  (should (eq buffer-offer-save 'always))
                  (goto-char (point-min))
                  (search-forward "[\"final\" \"preliminary\"]")
                  (replace-match "[\"preliminary\"]")
                  (should (buffer-modified-p))
                  (save-buffer)
                  (should-not (buffer-modified-p)))))))
      (let* ((original-entries (yaml-parse-string original-content :object-type 'alist :sequence-type 'list))
             (reloaded (shex-manifest-browser--load-manifest shex-manifest-browser-test--manifest-path))
             (entry (nth 3 reloaded)))
        (should (equal (shex-manifest-browser--get entry 'schema)
                       "BASE <http://a.example/>
PREFIX ex: <http://a.example/ns#>

start = @<Item>

<Item> { ex:status [\"preliminary\"] }
"))
        ;; Every other entry -- including the immediate neighbors --
        ;; came back exactly as loaded before this test touched
        ;; anything.
        (dotimes (i (length reloaded))
          (unless (= i 3)
            (should (equal (nth i reloaded) (nth i original-entries)))))
        (write-region original-content nil shex-manifest-browser-test--manifest-path)))))

(provide 'shex-manifest-browser-tests)

;;; shex-manifest-browser-tests.el ends here
