;;; melpa-build.el --- Build the MELPA tarball from the current checkout  -*- lexical-binding: t; -*-

;; Mirrors the recipe at https://github.com/melpa/melpa/blob/master/recipes/shexc-ts-mode
;; and the channel configs from melpa/melpa's Makefile, so the artifact this
;; produces matches what melpa.org/stable.melpa.org would actually publish
;; from this commit.  If the upstream recipe's `:files' spec changes, update
;; PACKAGE-FILES below to match.
;;
;; Required environment variables:
;;   PACKAGE_BUILD_DIR  - checkout of https://github.com/melpa/package-build
;;   REPO_DIR           - checkout of this repository (the commit under test)
;;   OUT_DIR            - where to write the built tarball
;; Optional:
;;   CHANNEL            - "unstable" (default, what melpa.org serves) or
;;                        "stable" (what stable.melpa.org serves -- only
;;                        produces a version when the commit is a tagged
;;                        release; mirrors what happens on `git push --tags')

(defconst package-name "shexc-ts-mode")
(defconst package-files '(:defaults (:exclude "shexc-mode.el")))

(let ((package-build-dir (or (getenv "PACKAGE_BUILD_DIR")
                              (error "PACKAGE_BUILD_DIR is not set")))
      (repo-dir (or (getenv "REPO_DIR") (error "REPO_DIR is not set")))
      (out-dir (or (getenv "OUT_DIR") (error "OUT_DIR is not set")))
      (channel (or (getenv "CHANNEL") "unstable")))
  (add-to-list 'load-path package-build-dir)
  (require 'package-build)

  (setq package-build-build-function 'package-build--build-multi-file-package)
  (cond
   ((equal channel "unstable")
    (setq package-build-releases nil)
    (setq package-build-snapshot-version-functions '(package-build-timestamp-version)))
   ((equal channel "stable")
    (setq package-build-releases t)
    (setq package-build-all-publishable nil)
    (setq package-build-release-version-functions '(package-build-tag-version)))
   (t (error "Unknown CHANNEL: %s" channel)))

  (make-directory out-dir t)
  (let ((working-dir (expand-file-name "working/" out-dir))
        (recipes-dir (expand-file-name "recipes/" out-dir)))
    (make-directory recipes-dir t)
    ;; Point the recipe at the local checkout, not GitHub, so this builds
    ;; the commit under test instead of whatever is already on master.
    (with-temp-file (expand-file-name package-name recipes-dir)
      (insert (format "(%s :fetcher git :url %S :files %S)\n"
                       package-name repo-dir package-files)))
    (setq package-build-working-dir working-dir)
    (setq package-build-archive-dir (expand-file-name "packages/" out-dir))
    (setq package-build-recipes-dir recipes-dir))

  (package-build-archive package-name))
