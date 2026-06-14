;; Minimal init for recording the shexc-ts-mode demo with VHS.
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(load-theme 'modus-vivendi t)

(add-to-list 'treesit-extra-load-path (expand-file-name "~/.emacs.d/tree-sitter/"))
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(require 'shexc-ts-mode)

;; Make flymake re-check quickly for the demo.
(setq flymake-no-changes-timeout 0.2)

(setq-default mode-line-format
              '("  " mode-name "  " (:eval (flymake--mode-line-counters))))

;; Start with no shapes/predicates highlighted, so the demo can turn each
;; highlight-reachable category on one at a time and show the effect.
(setq shexc-ts-mode-highlight-reachable-include-current nil
      shexc-ts-mode-highlight-reachable-include-current-predicates nil
      shexc-ts-mode-highlight-reachable-include-non-extended nil
      shexc-ts-mode-highlight-reachable-include-non-extended-predicates nil
      shexc-ts-mode-highlight-reachable-include-extended nil
      shexc-ts-mode-highlight-reachable-include-extended-predicates nil)
