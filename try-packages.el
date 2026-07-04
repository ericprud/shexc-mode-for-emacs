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
    (load-library (expand-file-name "shexc-ts-mode.el" here))
    (load-library (expand-file-name "shex-validate.el" here))
    (load-library (expand-file-name "shex-manifest-browser.el" here)))
)
