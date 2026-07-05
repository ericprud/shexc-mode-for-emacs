;; Minimal init for recording the cross-buffer flymake demo with VHS
;; (schema/data buffers linked via `shex-manifest-browser'/`shex-
;; validate' -- editing the schema alone refreshes the data buffer's
;; own diagnostics, per `shex-validate--register-dependent'/`--notify-
;; dependents').
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(load-theme 'modus-vivendi t)

(add-to-list 'treesit-extra-load-path (expand-file-name "~/.emacs.d/tree-sitter/"))
;; Emacs's own bundled `transient' predates the `:info' pseudo-suffix
;; the feature menu (`C-c C-c') uses -- `package-initialize' puts a
;; newer, MELPA-installed `transient' ahead of it on `load-path'
;; (confirmed empirically: without this, `shexc-ts-mode' itself fails
;; to load with "Eager macro-expansion failure: Need command, got
;; ':info'", which then also takes `shex-manifest-browser' down with
;; it -- see `try-packages.el', which needs this same fix).
(require 'package)
(package-initialize)
(let ((repo (expand-file-name "../.." (file-name-directory load-file-name))))
  (add-to-list 'load-path repo)
  ;; A relative rudof path is resolved against THIS file's own
  ;; directory (computed once, right now) -- not whatever buffer is
  ;; current whenever `shex-validate' first loads the module, which
  ;; can be much later and is otherwise unpredictable; adjust this if
  ;; your own `rudof' checkout isn't a sibling of REPO's parent.
  (setq shex-validate-rudof-module-path
        (expand-file-name "../rudof/target/release/libemacs_rudof.so" repo)))
(require 'shexc-ts-mode)
(require 'turtle-ts-mode)
(require 'shex-validate)
(require 'shex-manifest-browser)

;; Make flymake re-check quickly for the demo.
(setq flymake-no-changes-timeout 0.2)

;; Spatial window navigation -- `other-window' (`C-x o') cycles through
;; all FOUR windows the demo ends up with (manifest table, schema,
;; data, AND `shex-validate-show-result-shapemap''s own bottom side
;; window), in an order that isn't worth depending on; `windmove' picks
;; the window in a given direction instead, unambiguous regardless of
;; how many other windows exist. Bound to `C-<left>'/`C-<right>'
;; (not the more usual `M-<left>'/`M-<right>') because VHS's own
;; `Alt+<key>' tape syntax doesn't support arrow keys at all, only
;; `Ctrl+<key>' does.
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)

(setq-default mode-line-format
              '("  " mode-name "  " (:eval (flymake--mode-line-counters))))
