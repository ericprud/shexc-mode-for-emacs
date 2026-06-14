;; Minimal init for recording the shexc-ts-mode demo with VHS.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(load-theme 'modus-vivendi t)

(add-to-list 'treesit-extra-load-path (expand-file-name "~/.emacs.d/tree-sitter/"))
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(require 'shexc-ts-mode)

;; Make flymake re-check quickly for the demo.
(setq flymake-no-changes-timeout 0.2)

(setq-default mode-line-format
              '("  " mode-name "  " (:eval (flymake--mode-line-counters))))
