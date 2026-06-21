;;; melpa-upgrade-test.el --- Verify upgrading from the live MELPA package works  -*- lexical-binding: t; -*-

;; Catches two related bugs:
;;
;; 1. Version regression: if the new build's version string does not sort
;;    higher than the currently-published one, package.el will never
;;    activate the new code for users who already have the old version
;;    installed, no matter how many times they reinstall.
;; 2. Stale activation: even when versions sort correctly, leftover files
;;    or a wrong load-path order could cause `require' to silently load
;;    the OLD version's code after "upgrading".  Checking that
;;    `locate-library' resolves into the NEW package's directory catches
;;    that; just checking that `require' succeeds does not.
;;
;; If no version of PACKAGE_NAME is currently published on MELPA yet (e.g.
;; this is the first release), OLD_TARBALL is left unset and this script
;; just confirms the new build, alone, is installable -- there is nothing
;; to regress against yet.
;;
;; Required environment variables:
;;   NEW_TARBALL   - path to the newly built .tar
;;   PACKAGE_NAME  - name of the package/feature, e.g. "shexc-ts-mode"
;; Optional:
;;   OLD_TARBALL   - path to the currently-published .tar, if any

(let ((new-tarball (or (getenv "NEW_TARBALL") (error "NEW_TARBALL is not set")))
      (old-tarball (let ((v (getenv "OLD_TARBALL"))) (and v (not (string-empty-p v)) v)))
      (package-name (or (getenv "PACKAGE_NAME") (error "PACKAGE_NAME is not set")))
      (package-user-dir (make-temp-file "melpa-upgrade-test-" t)))
  (require 'package)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)

  (if (not old-tarball)
      (message "No published version of %s found; skipping upgrade check, just installing fresh."
               package-name)
    (package-install-file old-tarball)
    (let ((old-version (package-desc-version
                         (cadr (assq (intern package-name) package-alist))))
          (new-version
           (version-to-list
            (replace-regexp-in-string
             (format "\\`%s-\\|\\.tar\\'" (regexp-quote package-name)) ""
             (file-name-nondirectory new-tarball)))))
      (unless (version-list-< old-version new-version)
        (error (concat "Version regression: new build %S does not sort higher "
                        "than the published version %S. Users who already have "
                        "the old version installed will never receive this update.")
               new-version old-version))))

  (package-refresh-contents)
  (package-install-file new-tarball)
  (package-initialize t)
  (require (intern package-name))
  (let ((loaded-from (locate-library package-name))
        (new-dir-name (file-name-sans-extension (file-name-nondirectory new-tarball))))
    (unless (and loaded-from (string-match-p (regexp-quote new-dir-name) loaded-from))
      (error (concat "After \"upgrading\", `require' loaded %s instead of the new "
                      "version (expected a path under %s). The new package was "
                      "installed but old code is still what actually runs.")
             loaded-from new-dir-name)))
  (message "OK: upgrade to %s activates the new code" package-name))
