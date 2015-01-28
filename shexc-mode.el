;;; shexc-mode.el --- mode for ShExC

;; Copyright (c) 2003-2007 Hugo Haas <hugo@larve.net>
;; re-worked and re-published by kurtjx (c) 2010 <kurtjx@gmail.com>
;; repurposed for ShExC (c) 2015 <eric@w3.org>

;; For documentation on ShExC, see:
;; https://www.w3.org/2014/03/ShEx-subm/Primer

;;; Comentary:

;; Goals:
;; - sytax highlighting
;; - completion
;; - indentation

;; What it does now:
;; - Syntax highlighting
;; - comment/uncomment block with M-;

;;; Code:

;; the command to comment/uncomment text
(defun shexc-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
     (comment-dwim arg)))

(let* (
       (HEX		"[0-9A-Fa-f]")
       (UCHAR		(format "\\(?:\\\\u%s%s%s%s\\|\\\\U%s%s%s%s%s%s%s%s\\)" HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX HEX))
       (IRIREF		(format "<\\(?:[^\x0\ -\x20\ <>\"{}|^`\\\\]\\|%s\\)*>" UCHAR)) ; #x00=NULL #01-#x1F=control codes #x20=space
       (LANGTAG		"@[a-zA-Z]+\\(?:'-'[a-zA-Z0-9]+\\)*")
       (INTEGER		"[+-]?[0-9]+")
       (DECIMAL		"[+-]?[0-9]*\\.[0-9]+")
       (EXPONENT	"[eE][+-]?[0-9]+")
       (DOUBLE		(format "[+-]?\\(?:[0-9]+\\.[0-9]*%s\\|\\.[0-9]+%s|[0-9]+%s" EXPONENT EXPONENT EXPONENT))
       (ECHAR		"\\(?:\\\\[tbnrf\"'\\\\]\\)")
       (STRING_LITERAL1	(format "'\\(?:[^\x27\ \x5C\ \xA\ \xD\ ]\\|%s\\|%s\\)*'" ECHAR UCHAR)) ; #x27=' #x5C=\ #xA=new line #xD=carriage return
       (STRING_LITERAL2	(format "\"\\(?:[^\x22\ \x5C\ \xA\ \xD\ ]\\|%s\\|%s\\)*\"" ECHAR UCHAR)) ; #x22=" #x5C=\ #xA=new line #xD=carriage return
       (STRING_LITERAL_LONG1	(format "'''\\(?:\\(?:'\\|''\\)?\\([^'\\\\]\\|%s\\|%s\\)\\)*'''" ECHAR UCHAR))
       (STRING_LITERAL_LONG2	(format "\"\"\"\\(?:\\(?:\"\\|\"\"\\)?\\(?:[^\"\\\\]\\|%s\\|%s\\)\\)*\"\"\"" ECHAR UCHAR))

       (WS		"[\x20\ \x9\ \xD\ \xA\ ]") ; #x20=space #x9=character tabulation #xD=carriage return #xA=new line
       (ANON		(format "\\(?:\\[%s*\\]\\)" WS))
       ; @@ disabled \x10000\ -\x2FFFF\ in PN_CHARS_BASE; slowed emacs too much.
       ;(PN_CHARS_BASE	"\\(?:[A-Za-z\x00C0\ -\x00D6\ \x00D8\ -\x00F6\ \x00F8\ -\x02FF\ \x0370\ -\x037D\ \x037F\ -\x1FFF\ \x200C\ -\x200D\ \x2070\ -\x218F\ \x2C00\ -\x2FEF\ \x3001\ -\xD7FF\ \xF900\ -\xFDCF\ \xFDF0\ -\xFFFD\ \x10000\ -\xEFFFF\ ]\\)")
       (PN_CHARS_BASE	"\\(?:[A-Za-z\x00C0\ -\x00D6\ \x00D8\ -\x00F6\ \x00F8\ -\x02FF\ \x0370\ -\x037D\ \x037F\ -\x1FFF\ \x200C\ -\x200D\ \x2070\ -\x218F\ \x2C00\ -\x2FEF\ \x3001\ -\xD7FF\ \xF900\ -\xFDCF\ \xFDF0\ -\xFFFD\ ]\\)")
       (PN_CHARS_U	(format "\\(?:%s\\|_\\)" PN_CHARS_BASE))
       (PN_CHARS	(format "\\(?:%s\\|-\\|[0-9]\\|\x00B7\ \\|[\x0300\ -\x036F\ ]\\|[\x203F\ -\x2040\ ]\\)" PN_CHARS_U))
       ;(BLANK_NODE_LABEL	(format "_:\(?:%s\\|[0-9]\\)\\(?:\\(?:%s\\|\\.\\)*%s\\)?" PN_CHARS_U PN_CHARS PN_CHARS))
       (BLANK_NODE_LABEL	(format "\\(?:_:\\(?:%s\\|[0-9]\\)\\(?:\\(?:%s\\|\\.\\)*%s\\)?\\)" PN_CHARS_U PN_CHARS PN_CHARS))
       (PN_PREFIX	(format "\\(?:%s\\(?:\\(?:%s\\|\\.\\)*%s\\)?\\)" PN_CHARS_BASE PN_CHARS PN_CHARS))
       (PNAME_NS	(format "\\(?:%s?:\\)" PN_PREFIX))
       (PN_LOCAL_ESC	"\\(?:\\\\[_~.-!$&'()*+,;=/?#@%]\\)")
       (PERCENT 	(format "\\(?:%%%s%s\\)" HEX HEX))
       (PLX		(format "\\(?:%s\\|%s\\)" PERCENT PN_LOCAL_ESC))
       (PN_LOCAL	(format "\\(?:\\(?:%s\\|:\\|[0-9]\\|%s\\)\\(?:\\(?:%s\\|'.'\\|':'\\|%s\\)*\\(?:%s\\|':'\\|%s\\)\\)?\\)" PN_CHARS_U PLX PN_CHARS PLX PN_CHARS PLX))
       (PNAME_LN	(format "%s%s" PNAME_NS PN_LOCAL))
       (PrefixedName	(format "\\(?:%s\\|%s\\)" PNAME_LN PNAME_NS))
       (BlankNode	(format "\\(?:%s\\|%s\\)" BLANK_NODE_LABEL ANON))
       (String		(format "\\(?:%s\\|%s\\|%s\\|%s\\)" STRING_LITERAL1 STRING_LITERAL2 STRING_LITERAL_LONG1 STRING_LITERAL_LONG2))
       (CODE		(format "%%%s?{\\(?:[^%%\\\\]\\|\\\\%%\\)*%%}" PN_PREFIX))
       (comment		"\\(?:#[^\r\n]*[\r\n]\\)")
       (SP		(format "\\(?:\\(?:[ \t\r\n]\\|%s\\)*\\)" comment)) ; "SP" scans better than "skip"
       )
  (setq shexc-highlights
; overly-simlified building blocks, mostly from <http://www.w3.org/TR/turtle/#terminals>:
;   PNAME_NS: \\(?:[a-zA-Z0-9_-]\\)*:
;   PN_LOCAL: \\(?:[:a-zA-Z0-9_-]\\|%[0-9a-fA-F][0-9a-fA-F]\\|\\\\[_~.-!$&'()*+,;=/?#@%]\\)*
;   semantic action label: %\\(?:[a-zA-Z0-9_-]*\\)
	(list
	 (list "\\(prefix\\|PREFIX\\|start\\|START\\)\\>" 1 font-lock-keyword-face t) ; how to do case insensitive?
	 (list "\\([.?*+&]\\)" 1 font-lock-keyword-face t)
	 (list "\\<\\(a\\)\\>" 1 font-lock-keyword-face t)
         ;(list "\\(\\(?:[a-zA-Z0-9_]+\\):\\)" 1 font-lock-type-face t)
	 ;(list ":\\(\\(?:[a-zA-Z0-9_]+\\)\\)[ ;.)?+*,#\n]" 1 font-lock-constant-face t)

	 (list (format "\\(%s\\)%s" PNAME_NS PN_LOCAL) 1 font-lock-type-face t) ; capture (PNAME_NS)PN_LOCAL
	 (list (format "%s\\(%s\\)" PNAME_NS PN_LOCAL) 1 font-lock-constant-face t) ; capture PNAME_NS(PN_LOCAL)

	 (list (format "\\(%s\\)" IRIREF) 1 font-lock-function-name-face t)
	 (list (format "\\(%s\\)" BlankNode) 1 font-lock-function-name-face t)

         ; @SP*<Shape> | &SP*<Shape>
	 (list (format "\\([@&]%s%s\\)" SP IRIREF) 1 font-lock-variable-name-face t)
	 ;(list (format "\\([@&]\\(?:[ \n\t]*\\|%s\\)*%s\\)" comment IRIREF) 1 font-lock-variable-name-face t)
         ; @SP*my:Shape | &SP*my:Shape
	 (list (format "\\([@&]%s%s\\)" SP PrefixedName) 1 font-lock-variable-name-face t)
         ; <Shape>SP*{...}
	 (list (format "\\(%s\\)%s{" IRIREF SP) 1 font-lock-variable-name-face t)
         ; my:ShapeSP*{...}
	 (list (format "\\(%s\\)%s{" PrefixedName SP) 1 font-lock-variable-name-face t)
         ; start=SP*<Shape>
	 (list (format "start%s=%s\\(%s\\)" SP SP IRIREF) 1 font-lock-variable-name-face t)
         ; start=SP*my:Shape
	 (list (format "start%s=%s\\(%s\\)" SP SP PrefixedName) 1 font-lock-variable-name-face t)

	 ; @@ doesn't work for long lines
	 ;(list "\\(\\\".*?\\\"\\)" 1 font-lock-string-face t)
	 (list (format "\\(%s\\)" String) 1 font-lock-string-face t) ; @@ There's some other string stuff going on somewhere
         ; Bug: some trailing characters are highlighted; restricting comments regexp
         ; ("\\(#.*\\)" 1 font-lock-comment-face t)
	 ;(list "^\\s-*\\(#.*\\)" 1 font-lock-comment-face t)
	 (list (format "^\\(%s\\)" comment) 1 font-lock-comment-face t)

         ; semantic actions
	 (list (format "\\(%s\\)" CODE) 1 font-lock-preprocessor-face t)
	 )
	)
  (format "\\([@&]%s%s\\)" SP IRIREF)
  )

;;(define-generic-mode 'shexc-mode
(define-derived-mode shexc-mode fundamental-mode
  ;; setup tab key not working :/
  ;;(setq c-basic-offset 4)

  ;; syntax highlighting
  (setq font-lock-defaults '(shexc-highlights))

  ;; modify the keymap M-; comments/uncomments region
  (define-key shexc-mode-map [remap comment-dwim] 'shexc-comment-dwim)
  ;; comments: "# ..."
  (modify-syntax-entry ?# "< b" shexc-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" shexc-mode-syntax-table)

  ;; description
  "Mode for Notation 3 documents."
)

