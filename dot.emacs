; -*- mode: Emacs-Lisp;-*-
; $Id$

; Personal information
(setq user-mail-address "toki@freedom.ne.jp")
(setq user-full-name "TOKI Yoshinori")

; Local emacs-lisp library path
(setq load-path
      (append (list (expand-file-name "~/elisp/lib")
		    (expand-file-name "~/elisp/work"))
	      load-path))

; Japanese environment
(set-language-environment 'Japanese)
(set-default-coding-systems 'japanese-iso-8bit)
(set-terminal-coding-system 'japanese-iso-8bit)

; Info directories
(setq Info-default-directory-list
      '("/usr/share/info"
	"/usr/local/info"
	"/usr/X11R6/info"))

; User key bindings
(load "term/bobcat")
(global-set-key "\C-\\" 'help-command)
(global-set-key "\C-\\\C-\\" 'help-for-help)
(global-set-key "\M-g" 'goto-line)

; Mode line information
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
; (setq display-time-format "%y/%m/%d (%a) %h:%m")
(display-time)
(line-number-mode t)
(column-number-mode t)

; Replacing
(defun select-query-replace (enable-regexp)
  (interactive "P")
  (let ((args (query-replace-read-args
	       (if enable-regexp "Query replace regexp" "Query replace")
	       (if enable-regexp t nil))))
    (if enable-regexp
	(query-replace-regexp (nth 0 args) (nth 1 args))
      (query-replace (nth 0 args) (nth 1 args)))))
(global-set-key "\M-%" 'select-query-replace)

; Find file
(defun select-find-file (enable-hexl)
  (interactive "P")
  (if enable-hexl
      (funcall (function hexl-find-file)
	       (read-file-name "Filename: " nil nil t))
    (funcall (function find-file)
	     (read-file-name "Find file: " nil nil nil))))
(global-set-key "\C-x\C-f" 'select-find-file)

; Buffer switching
(defadvice switch-to-buffer (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer: " (other-buffer) t))))
(defadvice switch-to-buffer-other-window (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer in other window: " (other-buffer) t))))
(defadvice switch-to-buffer-other-frame (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer in other frame: " (other-buffer) t))))

; Window
(defun other-window-one-step (previous)
  (interactive "P")
  (if previous
      (other-window -1)
    (other-window 1)))
(global-set-key "\C-^" 'other-window-one-step)
(setq truncate-partial-width-windows nil)

; No new lines
(setq next-line-add-newlines nil)

; Timestamp
(defun insert-timestamp ()
  (interactive)
  (insert (current-time-string)))

; Emacsclient
(server-start)

; Bold face
(if window-system
    (condition-case nil
	(progn
	  (make-face-bold 'bold)
	  (make-face-bold 'bold-italic))
      (error nil)))

; Font lock mode
(custom-declare-face
 'font-lock-builtin-face
 '((((class grayscale) (background light)) (:foreground "lightgray" :bold t))
   (((class grayscale) (background dark)) (:foreground "dimgray" :bold t))
   (((class color) (background light)) (:foreground "seagreen"))
   (((class color) (background dark)) (:foreground "darkolivegreen"))
   (t (:bold t)))
 "font lock mode face used to highlight builtins."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'font-lock-string-face
 '((((class grayscale) (background light)) (:foreground "dimgray" :italic t))
   (((class grayscale) (background dark)) (:foreground "lightgray" :italic t))
   (((class color) (background light)) (:foreground "gray40"))
   (((class color) (background dark)) (:foreground "lightsalmon"))
   (t (:italic t)))
 "font lock mode face used to highlight strings."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'font-lock-variable-name-face
 '((((class grayscale) (background light))
    (:foreground "gray90" :bold t :italic t))
   (((class grayscale) (background dark))
    (:foreground "dimgray" :bold t :italic t))
   (((class color) (background light)) (:foreground "brown"))
   (((class color) (background dark)) (:foreground "lightgoldenrod"))
   (t (:bold t :italic t)))
 "font lock mode face used to highlight variable names."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'info-node
 '((((class grayscale) (background light))
    (:foreground "black" :bold t))
   (((class grayscale) (background dark))
    (:foreground "white" :bold t))
   (((class color) (background light)) (:foreground "purple" :bold t))
   (((class color) (background dark)) (:foreground "plum1" :bold t))
   (t (:bold t)))
 "info mode face used to highlight node."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'info-xref
 '((((class grayscale) (background light))
    (:foreground "black" :bold t))
   (((class grayscale) (background dark))
    (:foreground "white" :bold t))
   (((class color) (background light)) (:foreground "blue" :bold t))
   (((class color) (background dark)) (:foreground "cyan" :bold t))
   (t (:bold t)))
 "info mode face used to highlight xref."
 :group 'font-lock-highlighting-faces)
(if (not (getenv "xmono"))
    ; xmono is monochrome x server flag.
    ; see `.xsession' file.
    (global-font-lock-mode t))

; HTML mode
(setq auto-mode-alist
      (append '(("\\.rhtml$" . html-mode)) auto-mode-alist))
(eval-after-load "sgml-mode"
  '(setq html-tag-alist
	 (let* ((1-7 '(("1") ("2") ("3") ("4") ("5") ("6") ("7")))
		(1-9 '(,@1-7 ("8") ("9")))
		(align '(("align" ("left") ("center") ("right"))))
		(valign '(("top") ("middle") ("bottom") ("baseline")))
		(rel '(("next") ("previous") ("parent") ("subdocument") ("made")))
		(href '("href" ("ftp:") ("file:") ("finger:") ("gopher:") ("http:")
			("mailto:") ("news:") ("rlogin:") ("telnet:") ("tn3270:")
			("wais:") ("/cgi-bin/")))
		(name '("name"))
		(link `(,href
			("rel" ,@rel)
			("rev" ,@rel)
			("title")))
		(list '((nil \n ( "List item: "
				  "<li>" str \n))))
		(cell `(,align
			("valign" ,@valign)
			("colspan" ,@1-9)
			("rowspan" ,@1-9)
			("nowrap" t))))
	   ;; put ,-expressions first, else byte-compile chokes (as of V19.29)
	   ;; and like this it's more efficient anyway
	   `(("a" ,name ,@link)
	     ("base" t ,@href)
	     ("dir" ,@list)
	     ("font" nil "size" ("-1") ("+1") ("-2") ("+2") ,@1-7)
	     ("form" (\n _ \n "<input type=\"submit\" value=\"\">")
	      ("action" ,@(cdr href)) ("method" ("get") ("post")))
	     ("h1" ,@align)
	     ("h2" ,@align)
	     ("h3" ,@align)
	     ("h4" ,@align)
	     ("h5" ,@align)
	     ("h6" ,@align)
	     ("hr" t ("size" ,@1-9) ("width") ("noshade" t) ,@align)
	     ("img" t ("align" ,@valign ("texttop") ("absmiddle") ("absbottom"))
	      ("src") ("alt") ("width" "1") ("height" "1")
	      ("border" "1") ("vspace" "1") ("hspace" "1") ("ismap" t))
	     ("input" t ("size" ,@1-9) ("maxlength" ,@1-9) ("checked" t) ,name
	      ("type" ("text") ("password") ("checkbox") ("radio")
	       ("submit") ("reset"))
	      ("value"))
	     ("link" t ,@link)
	     ("menu" ,@list)
	     ("ol" ,@list ("type" ("A") ("a") ("I") ("i") ("1")))
	     ("p" ,@align)
	     ("select" (nil \n
			    ("Text: "
			     "<option>" str \n))
	      ,name ("size" ,@1-9) ("multiple" t))
	     ("table" (nil \n
			   ((completing-read "Cell kind: " '(("td") ("th"))
					     nil t "t")
			    "<tr><" str ?> _ \n))
	      ("border" t ,@1-9) ("width" "10") ("cellpadding"))
	     ("td" ,@cell)
	     ("textarea" ,name ("rows" ,@1-9) ("cols" ,@1-9))
	     ("th" ,@cell)
	     ("ul" ,@list ("type" ("disc") ("circle") ("square")))

	     ,@sgml-tag-alist

	     ("abbrev")
	     ("acronym")
	     ("address")
	     ("array" (nil \n
			   ("Item: " "<item>" str \n))
	      "align")
	     ("au")
	     ("b")
	     ("big")
	     ("blink")
	     ("blockquote" \n)
	     ("body" \n ("background" ".gif") ("bgcolor" "#") ("text" "#")
	      ("link" "#") ("alink" "#") ("vlink" "#"))
	     ("box" (nil _ "<over>" _))
	     ("br" t ("clear" ("left") ("right")))
	     ("caption" ("valign" ("top") ("bottom")))
	     ("center" \n)
	     ("cite")
	     ("code" \n)
	     ("dd")
	     ("del")
	     ("dfn")
	     ("dl" (nil \n
			( "Term: "
			  "<dt>" str "</dt><dd></dd>" _ \n)))
	     ("dt")
	     ("em")
					;("fn" "id" "fn")  ; ???
	     ("head" \n)
	     ("html" (\n
		      "<head>\n"
		      "<title>" (setq str (read-input "Title: ")) "</title>\n"
		      "</head>\n"
		      "<body>\n<h1>" str "</h1>\n" _
		      "\n<address>\n<a href=\"mailto:"
		      user-mail-address
		      "\">" (user-full-name) "</a>\n</address>\n"
		      "</body>"
		      ))
	     ("i")
	     ("ins")
	     ("isindex" t ("action") ("prompt"))
	     ("kbd")
	     ("lang")
	     ("li")
	     ("math" \n)
	     ("nobr")
	     ("option" t ("value") ("label") ("selected" t))
	     ("over" t)
	     ("person")
	     ("pre" \n)
	     ("q")
	     ("rev")
	     ("s")
	     ("samp")
	     ("small")
	     ("strong")
	     ("sub")
	     ("sup")
	     ("title")
	     ("tr" t)
	     ("tt")
	     ("u")
	     ("var")
	     ("wbr" t)))))

; XML mode
(setq auto-mode-alist
      (append '(("\\.xml\\(\\.[^\\.]+\\)?$" . sgml-mode)
		("\\.xsl\\(\\.[^\\.]+\\)?$" . sgml-mode)) auto-mode-alist))

; Java mode
(setq auto-mode-alist
      (append '(("\\.java\\(\\.[^\\.]+\\)?$" . java-mode)) auto-mode-alist))

; C & C++ mode customization
(add-hook
 'c-mode-common-hook
 (function
  (lambda ()
    (setq c-basic-offset 2)
    (c-set-offset 'substatement 0))))

; Auto compression
(auto-compression-mode t)

; Perl mode indent customization
(setq perl-indent-level                2)
(setq perl-continued-statement-offset  2)
(setq perl-continued-brace-offset     -2)
(setq perl-brace-offset                0)
(setq perl-brace-imaginary-offset      0)
(setq perl-label-offset                0)

; LaTeX mode
(setq tex-default-mode 'latex-mode)
(autoload 'latex-label-insert "latex-label"
  "insertion of a latex label." t)
(add-hook
 'latex-mode-hook
 (function
  (lambda ()
    (define-key tex-mode-map "\C-cl" 'latex-label-insert))))

; Exciting cite utility
(autoload 'xcite "xcite" "exciting cite" t)
(autoload 'xcite-yank-cur-msg "xcite" "exciting cite" t)
(global-set-key "\C-cc" 'xcite)
(setq xcite:insert-header-function
      (function xcite-toki-header))
(defun xcite-toki-header ()
  (concat
   (if date
       (format "{Date} %s\n" date))
   (if subject
       (format "{Subject} %s\n" subject))
   (if msgid
       (format "{Message ID} %s\n" msgid))
   (if id
       (format "%s wrote...\n"
	       (cond
		((or (string-match "^toki@freedom\\.ne\\.jp$" id)
		     (string-match "^toki@.*phys\\(\\.sci\\)?\\.kobe-u\\.ac\\.jp$" id))
		 (format "自分 <%s>" id))
		(handle
		 (format "%s <%s>" handle id))
		(t id))))))

; Ruby mode
(autoload 'ruby-mode "ruby-mode"
  "mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)
		("\\.cgi$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist
      (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

; Interactive ruby
(autoload 'run-ruby "inf-ruby"
  "run an inferior ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby"
  "set local key defs for inf-ruby in ruby-mode")
(add-hook
 'ruby-mode-hook
 (function
  (lambda ()
    (inf-ruby-keys))))

; Ruby debugger
(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)

; RD (ruby document) mode
(autoload 'rd-mode "rd-mode" "major mode for ruby document formatter rd" t)
(setq auto-mode-alist
      (append '(("\\.rd$" . rd-mode)
		("\\.rd\\.[A-Za-z]*$" . rd-mode))
	      auto-mode-alist))

; Man
(setq manual-program "jman")
(setq manual-program "jman")

; Comparing files
(setq diff-switches "-u")

; ; Japanese elisp info
; (autoload 'elisp-info-describe-function "elisp-info"
;   "alternative describe-function" t nil)
; (autoload 'elisp-info-describe-variable "elisp-info"
;   "alternative describe-variable" t nil)
; (autoload 'elisp-info-lookup-index "elisp-info"
;   "alternative index-info" t nil)
; (eval-after-load
;     "help.el"
;   (progn
;     (define-key help-map "f" 'elisp-info-describe-function)
;     (define-key help-map "d" 'elisp-info-describe-function)
;     (define-key help-map "d" 'describe-function)
;     (define-key help-map "v" 'elisp-info-describe-variable)
;     (define-key help-map "v" 'describe-variable)))
; (add-hook
;  'info-mode-hook
;  (function
;   (lambda ()
;     (define-key info-mode-map "i" 'elisp-info-lookup-index)
;     (define-key info-mode-map "i" 'info-index))))

; PAW - kumac-mode
(autoload 'kumac-mode "kumac-mode" "mode for editing kumac files." t)
(setq auto-mode-alist
      (append '(("\\.kumac$" . 'kumac-mode)) auto-mode-alist))

; Verilog-HDL mode
(setq use-verilog-mode t)
;(load "color-def")
;(load "verilog-color")
(autoload 'verilog-mode "verilog-mode" "verilog mode" t )
(setq auto-mode-alist
      (append '(("\\.v\\'" . verilog-mode)
		("\\.tv\\'" . verilog-mode)
		("\\.dv\\'" . verilog-mode)
		("\\.vlg\\'" . verilog-mode)
		("\\.vei\\'" . verilog-mode)) auto-mode-alist))
(setq verilog-indent-level             2)
(setq verilog-indent-level-module      2)
(setq verilog-indent-level-declaration 2)
(setq verilog-indent-level-behavorial  2)
(setq verilog-cexp-indent              1)
(setq verilog-case-indent              2)
(setq verilog-minimum-comment-distance 40)
(setq verilog-auto-newline nil)
(setq verilog-auto-indent-on-newline nil)
(setq verilog-tab-always-indent t)
(setq verilog-indent-begin-after-if nil)
(setq verilog-auto-endcomments nil)
(setq verilog-auto-lineup `(all))

; Shell script mode
(setq sh-indentation 2)

; Shell command completion
(autoload 'shell-command-with-completion
      "shell-command" "alternate shell-command" t nil)
(define-key global-map "\e!" 'shell-command-with-completion)
(autoload 'shell-command-with-completion-on-region
  "shell-command" "alternate shell-command-on-region" t nil)
(define-key global-map "\e|" 'shell-command-with-completion-on-region)

; Fetchmail
(autoload 'fetchmail "fetchmail" nil t)
(setq fetchmail-default-server "freedom")
(setq fetchmail-server-option-alist
      '(("sv01.phys.sci.kobe-u.ac.jp" "--check")))
(setq fetchmail-server-alias-alist
      '(("homesrv"  . "babayaga.plutonian.ne.jp")
	("freedom"  . "mail.freedom.ne.jp")
	("kobephys" . "sv01.phys.sci.kobe-u.ac.jp")))

; for Bookmark
(setq bookmark-search-size 32)

; Grep
(setq grep-command "egrep -ne ")

; Parenthesis
(show-paren-mode t)

; Mailcrypt
(load "mailcrypt")
(mc-setversion "2.6")

; FLIM
(setq eword-max-size-to-decode (* 64 1024))
(setq eword-lexical-analyzer		; http://lists.airs.net/wl/archive/199909/msg00009.html
      '(;eword-analyze-quoted-string
        eword-analyze-domain-literal
        eword-analyze-comment
        eword-analyze-spaces
        eword-analyze-special
        eword-analyze-encoded-word
        eword-analyze-atom))
(eval-after-load "mime"			; http://lists.airs.net/wl/archive/199909/msg00031.html
  '(defadvice mime-entity-filename (around mime-decode activate)
     ad-do-it
     (and ad-return-value 
	  (setq ad-return-value (eword-decode-string ad-return-value)))))

; SEMI
; (load "mime-setup")
(setq mime-edit-split-message nil)

; Wanderlust
(setq wl-plugged nil)			; OFFLINE mode
(autoload 'wl "wl" "wanderlust" t)
(autoload 'wl-draft "wl" "write draft with wanderlust." t)

; SKK
(autoload 'skk-mode "skk" nil t)
(autoload 'skk-auto-fill-mode "skk" nil t)
(autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)
(global-set-key "\C-x\C-j" (function skk-mode))
(global-set-key "\C-xj" '(function skk-auto-fill-mode))
(add-hook 'isearch-mode-hook (function skk-isearch-mode-setup))
(add-hook 'isearch-mode-end-hook (function skk-isearch-mode-cleanup))
(setq skk-large-jisyo "/usr/local/share/skk/SKK-JISYO.L")
(setq skk-rom-kana-rule-list
      '(("hh" "h"
	 ("ッ" . "っ"))
	("mm" "m"
	 ("ン" . "ん"))
	; 記号の追加
	("!" nil "！")))

; SDIC-mode
(autoload 'sdic-describe-word "sdic"
  "" t nil)
(autoload 'sdic-describe-word-at-point "sdic"
  "" t nil)
(global-set-key "\C-xw" 'sdic-describe-word)
(global-set-key "\C-xW" 'sdic-describe-word-at-point)
(setq sdic-eiwa-dictionary-list
      '((sdicf-client "/usr/local/share/dict/gene.sdic.gz"
		      (title "GENE")
		      (strategy direct))
	(sdicf-client "/usr/local/share/dict/eedict.sdic.gz"
		      (title "EEDICT")
		      (strategy direct))))
(setq sdic-waei-dictionary-list
      '((sdicf-client "/usr/local/share/dict/jedict.sdic.gz"
		      (title "JEDICT")
		      (strategy direct))
	(sdicf-client "/usr/local/share/dict/jgene.sdic.gz"
		      (title "JGENE")
		      (strategy direct))))

; Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-mail-domain-list '("mail.freedom.ne.jp"))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
