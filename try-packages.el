(progn

  (define-key ctl-x-map "\M-f" 'find-file-at-point-with-line)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "C-+") 'text-scale-increase)

  (global-set-key (kbd "M-<left>") 'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)
  (global-set-key (kbd "M-<up>") 'windmove-up)
  (global-set-key (kbd "M-<down>") 'windmove-down)
  (global-set-key (kbd "M-S-<right>") 'next-buffer)
  (global-set-key (kbd "M-S-<left>") 'previous-buffer)

  (require 'package)
  (package-initialize)
  (let ((here (file-name-directory (or load-file-name buffer-file-name))))
    (add-to-list 'load-path here)
    ;; A relative path here is resolved against THIS file's own
    ;; directory (via `here', computed once, right now), not whatever
    ;; buffer happens to be current whenever `shex-validate' first
    ;; loads the module -- which, unlike this `setq', can run
    ;; arbitrarily later, well after `default-directory' has drifted
    ;; away from wherever Emacs started: confirmed empirically that
    ;; opening a manifest file directly (`find-file') and turning on
    ;; `shex-manifest-browser-mode' on it leaves `default-directory'
    ;; at that file's own directory for the rest of the session (major
    ;; modes don't reset it), which every schema/data buffer an entry
    ;; creates then inherits in turn. Kept as a literal relative
    ;; string (not pre-`expand-file-name'd away) so `ffap'/`find-file-
    ;; at-point' still resolves it correctly with point on it, right
    ;; here in this file.
    (setq shex-validate-rudof-module-path
          (expand-file-name "../rudof/target/release/libemacs_rudof.so" here))
    ;; Recompile-if-stale + load every top-level tracked library file,
    ;; rather than a hand-maintained list of entry points: new .el
    ;; files just show up here on the next run, and a source file
    ;; edited since its last `.elc' (the `rdf-turtle-buffer-directive-
    ;; ctx' bug: a stale `.elc' shadowed a newer `defun' in its `.el')
    ;; gets rebuilt instead of silently loading old bytecode. Limited
    ;; to git-tracked, top-level, non-test files: `-tests.el' files
    ;; register ERT tests rather than defining the packages themselves,
    ;; and anything below a subdirectory (example/, .github/scripts/)
    ;; is demo or CI scaffolding, not part of the loadable package set.
    (let ((self (file-name-nondirectory (or load-file-name buffer-file-name)))
          (tracked (with-temp-buffer
                     (let ((default-directory here))
                       (call-process "git" nil t nil "ls-files" "--" "*.el"))
                     (split-string (buffer-string) "\n" t))))
      (dolist (f tracked)
        (when (and (not (string-match-p "/" f))
                   (not (string-suffix-p "-tests.el" f))
                   (not (string= f self)))
          (byte-recompile-file (expand-file-name f here) nil 0 t)))))
)
