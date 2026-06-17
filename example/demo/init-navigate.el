;; Minimal init for recording the shexc-ts-mode xref-navigation demo with VHS.
(setq inhibit-startup-screen t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(load-theme 'modus-vivendi t)

(add-to-list 'treesit-extra-load-path (expand-file-name "~/.emacs.d/tree-sitter/"))
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))
(require 'shexc-ts-mode)

;; `xref-find-definitions'/`xref-go-back' (M-./M-,) leave no visible trace of
;; which key was pressed, which makes a silent recording hard to follow.
;; Rather than pull in the (non-built-in) `keycast' package just for this
;; recording, echo the chord and command name ourselves whenever one of them
;; runs, and pause briefly so it's readable in the gif.
(defvar shexc-demo--keycast-alist
  '((xref-find-definitions . "M-.")
    (xref-go-back . "M-,")))

(defun shexc-demo--keycast ()
  (when-let* ((key (alist-get this-command shexc-demo--keycast-alist)))
    (message "  %s  ->  %s" key this-command)
    (sit-for 0.8)))

(add-hook 'post-command-hook #'shexc-demo--keycast)
