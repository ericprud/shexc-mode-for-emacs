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
  (add-to-list 'load-path "./")
  (setq shex-validate-rudof-module-path "../../rudof/target/release/libemacs_rudof.so")
  (load-library "./shexc-ts-mode.el")
  (load-library "./shex-validate.el")
  (load-library "./shex-manifest-browser.el")
)
