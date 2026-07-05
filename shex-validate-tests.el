;;; shex-validate-tests.el --- ERT suite for shex-validate.el -*- lexical-binding: t; -*-

;; Author: Eric Prud'hommeaux <eric@w3.org>
;; Assisted-by: Claude:claude-sonnet-5
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Most of this suite tests `shex-validate--register-dependent'/
;; `shex-validate--notify-dependents' (the mechanism that re-checks a
;; data buffer's own `flymake' diagnostics whenever its linked schema/
;; ShapeMap buffer changes, not just when the data buffer itself
;; does), which needs no `emacs-rudof' module at all -- it only ever
;; schedules a recheck, via flymake's own `flymake--schedule-timer-
;; maybe' idle-timer debounce (confirmed empirically: `run-with-idle-
;; timer' never fires during a batch `ert-run-tests-batch-and-exit'
;; run, since batch Emacs has no real idle loop -- so these tests
;; assert a timer got *scheduled*, not that it fired, and force
;; `flymake-start' directly wherever they need to observe the
;; resulting diagnostics).
;;
;; The one true end-to-end test (`shex-validate-test-schema-edit-
;; refreshes-data-diagnostics') needs `emacs-rudof' to actually
;; validate, and reports itself skipped rather than failing, exactly
;; like `shexc-shexj-tests.el's `shexTest'-corpus tests, unless
;; `shex-validate-rudof-module-path' is already configured (see
;; `shex-validate.el's own docstring for that variable) before this
;; file is loaded.

;;; Code:

(require 'ert)
(require 'shex-validate)

(defun shex-validate-test--rudof-available-p ()
  (and (boundp 'shex-validate-rudof-module-path)
       shex-validate-rudof-module-path
       (file-exists-p (expand-file-name shex-validate-rudof-module-path))))

(defmacro shex-validate-test--with-buffers (bindings &rest body)
  "BINDINGS is a list of (VAR CONTENT-STRING) -- bind each VAR to a
fresh temp buffer holding CONTENT-STRING for BODY, killing all of them
afterward."
  (declare (indent 1))
  `(let ,(mapcar (lambda (b) (list (car b) '(generate-new-buffer " *temp*"))) bindings)
     (unwind-protect
         (progn
           ,@(mapcar (lambda (b) `(with-current-buffer ,(car b) (insert ,(cadr b)))) bindings)
           ,@body)
       ,@(mapcar (lambda (b) `(kill-buffer ,(car b))) bindings))))

;;; `shex-validate--register-dependent'/`--notify-dependents'

(ert-deftest shex-validate-test-register-dependent-installs-hook ()
  (shex-validate-test--with-buffers ((schema-buffer "") (data-buffer ""))
    (shex-validate--register-dependent schema-buffer data-buffer)
    (with-current-buffer schema-buffer
      (should (memq data-buffer shex-validate--dependent-data-buffers))
      (should (memq #'shex-validate--notify-dependents (mapcar #'identity after-change-functions))))))

(ert-deftest shex-validate-test-register-dependent-accumulates-multiple ()
  "One schema buffer shared by several data buffers (e.g. several
`shex-manifest-browser' entries reusing a schema by label) registers
all of them, not just the last."
  (shex-validate-test--with-buffers ((schema-buffer "") (data-buffer-1 "") (data-buffer-2 ""))
    (shex-validate--register-dependent schema-buffer data-buffer-1)
    (shex-validate--register-dependent schema-buffer data-buffer-2)
    (with-current-buffer schema-buffer
      (should (memq data-buffer-1 shex-validate--dependent-data-buffers))
      (should (memq data-buffer-2 shex-validate--dependent-data-buffers)))))

(ert-deftest shex-validate-test-register-dependent-is-idempotent ()
  (shex-validate-test--with-buffers ((schema-buffer "") (data-buffer ""))
    (shex-validate--register-dependent schema-buffer data-buffer)
    (shex-validate--register-dependent schema-buffer data-buffer)
    (with-current-buffer schema-buffer
      (should (= (length shex-validate--dependent-data-buffers) 1)))))

(ert-deftest shex-validate-test-notify-dependents-schedules-timer-on-flymake-buffers ()
  (shex-validate-test--with-buffers ((schema-buffer "") (data-buffer ""))
    (with-current-buffer data-buffer
      (setq-local flymake-mode t) ; simulate flymake being on, without needing a real backend
      (setq-local flymake-no-changes-timeout 0.2))
    (shex-validate--register-dependent schema-buffer data-buffer)
    (with-current-buffer schema-buffer
      (shex-validate--notify-dependents))
    (with-current-buffer data-buffer
      (should (timerp flymake-timer)))))

(ert-deftest shex-validate-test-notify-dependents-skips-non-flymake-buffers ()
  (shex-validate-test--with-buffers ((schema-buffer "") (data-buffer ""))
    (with-current-buffer data-buffer
      (setq-local flymake-mode nil))
    (shex-validate--register-dependent schema-buffer data-buffer)
    (with-current-buffer schema-buffer
      (shex-validate--notify-dependents))
    (with-current-buffer data-buffer
      (should-not (timerp flymake-timer)))))

(ert-deftest shex-validate-test-notify-dependents-skips-dead-buffers ()
  "A killed dependent is silently skipped, not an error -- it's never
removed from the list (harmless; just never notified again)."
  (let ((schema-buffer (generate-new-buffer " *temp-schema*"))
        (data-buffer (generate-new-buffer " *temp-data*")))
    (unwind-protect
        (progn
          (shex-validate--register-dependent schema-buffer data-buffer)
          (kill-buffer data-buffer)
          (with-current-buffer schema-buffer
            (should-not (condition-case nil
                            (progn (shex-validate--notify-dependents) nil)
                          (error t)))))
      (kill-buffer schema-buffer))))

(ert-deftest shex-validate-test-link-buffers-registers-dependents ()
  (shex-validate-test--with-buffers ((schema-buffer "") (shapemap-buffer "") (data-buffer ""))
    (with-current-buffer data-buffer
      (shex-validate-link-buffers schema-buffer shapemap-buffer))
    (with-current-buffer schema-buffer
      (should (memq data-buffer shex-validate--dependent-data-buffers)))
    (with-current-buffer shapemap-buffer
      (should (memq data-buffer shex-validate--dependent-data-buffers)))))

;;; End-to-end: editing the schema alone refreshes the data buffer's
;;; own flymake diagnostics -- the scenario this mechanism exists for.

(ert-deftest shex-validate-test-schema-edit-refreshes-data-diagnostics ()
  (unless (shex-validate-test--rudof-available-p)
    (ert-skip "`shex-validate-rudof-module-path' isn't configured/found -- see this file's Commentary"))
  (require 'shexc-ts-mode)
  (require 'turtle-ts-mode)
  (shex-validate-test--with-buffers
      ((schema-buffer "BASE <http://example.org/>
<#Employee> { <http://example.org/ns#name> . }
")
       (data-buffer "BASE <http://example.org/>
<#Alice> <http://example.org/ns#name> \"Alice\" .
")
       (shapemap-buffer "<#Alice>@<#Employee>"))
    (with-current-buffer schema-buffer (shexc-ts-mode))
    (with-current-buffer data-buffer
      (turtle-ts-mode)
      (shex-validate-link-buffers schema-buffer shapemap-buffer)
      (flymake-start)
      (should (null (flymake-diagnostics))))
    ;; Edit ONLY the schema -- tighten the constraint so the (unchanged)
    ;; data no longer conforms -- and never touch *data* directly.
    (with-current-buffer schema-buffer
      (goto-char (point-min))
      (search-forward "<http://example.org/ns#name> .")
      (replace-match "<http://example.org/ns#name> <http://example.org/ns#WrongDatatype>"))
    ;; The schema edit alone must have scheduled a recheck.
    (with-current-buffer data-buffer
      (should (timerp flymake-timer))
      ;; Force the check flymake's own idle timer would otherwise run,
      ;; since batch Emacs never actually fires idle timers.
      (flymake-start)
      (should (flymake-diagnostics)))))

(provide 'shex-validate-tests)

;;; shex-validate-tests.el ends here
