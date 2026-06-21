;;; melpa-install-test.el --- Verify a built tarball installs and loads cleanly  -*- lexical-binding: t; -*-

;; Catches the class of bug where a package builds fine but fails at
;; install/load time because a real dependency (e.g. a newer `transient'
;; than the one bundled with Emacs) isn't declared in Package-Requires.
;; `package-install-file' alone is not enough to catch this: it can report
;; success and `package-installed-p' can return t even when byte-compilation
;; failed and `require' would error.  So this script always also `require's
;; the package after installing it.
;;
;; Required environment variables:
;;   TARBALL       - path to the .tar produced by melpa-build.el
;;   PACKAGE_NAME  - name of the package/feature to require, e.g. "shexc-ts-mode"

(let ((tarball (or (getenv "TARBALL") (error "TARBALL is not set")))
      (package-name (or (getenv "PACKAGE_NAME") (error "PACKAGE_NAME is not set")))
      (package-user-dir (make-temp-file "melpa-install-test-" t)))
  (require 'package)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (package-refresh-contents)
  (package-install-file tarball)
  (unless (package-installed-p (intern package-name))
    (error "package-install-file reported success but %s is not installed"
           package-name))
  (require (intern package-name))
  (message "OK: %s installed and loaded cleanly" package-name))
