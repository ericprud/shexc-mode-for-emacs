# shexc-mode-for-emacs
This is an emacs major mode for editing ShExC documents
originally created as N3-mode by Hugo Haas and Dave Pawson.

Put shexc-mode.el somewhere emacs can find it (e.g. ~/.emacs.d/vendor/)
And add the following to your .emacs file:

;;
;; shexc mode
;;

(add-to-list 'load-path "{path}/shexc-mode.el")
(autoload 'shexc-mode "shexc-mode" "Major mode for OWL or SHEXC files" t)

;; Turn on font lock when in shexc mode
(add-hook 'shexc-mode-hook
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.shexc" . shexc-mode)
        '("\\.shex" . shexc-mode))
       auto-mode-alist))

;; Replace {path} with the full path to shexc-mode.el on your system.

;; If you want to make it load just a little faster;
;; C-x f shexc-mode.el
;; M-x byte-compile-file shexc-mode.el
