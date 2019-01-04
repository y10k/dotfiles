; -*- mode: Emacs-Lisp;-*-

; HOME directory
(cd (expand-file-name "~"))

; Personal information
(setq user-mail-address "toki@freedom.ne.jp")
(setq user-full-name "TOKI Yoshinori")

; Local emacs-lisp library path
(setq load-path
      (append (list (expand-file-name "~/elisp/work")
		    (expand-file-name "~/elisp/patch")
		    (expand-file-name "~/elisp/lib"))
	      load-path))

; Emacs Lisp Package Archive
; install packages: markdown-mode
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; Japanese environment
(set-language-environment 'Japanese)
(cond
 ; Coding sytem
 ((eq system-type 'windows-nt)
  (set-default-coding-systems 'japanese-shift-jis)
  (set-terminal-coding-system 'japanese-shift-jis))
 (t
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)))

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
 '((((type tty) (class color)) (:foreground "green"))
   (((class grayscale) (background light)) (:foreground "dimgray" :italic t))
   (((class grayscale) (background dark)) (:foreground "lightgray" :italic t))
   (((class color) (background light)) (:foreground "gray40"))
   (((class color) (background dark)) (:foreground "lightsalmon"))
   (t (:italic t)))
 "font lock mode face used to highlight strings."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'font-lock-variable-name-face
 '((((class grayscale) (background light)) (:foreground "gray90" :bold t :italic t))
   (((class grayscale) (background dark)) (:foreground "dimgray" :bold t :italic t))
   (((class color) (background light)) (:foreground "brown"))
   (((class color) (background dark)) (:foreground "lightgoldenrod"))
   (t (:bold t :italic t)))
 "font lock mode face used to highlight variable names."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'info-node
 '((((class grayscale) (background light)) (:foreground "black" :bold t))
   (((class grayscale) (background dark)) (:foreground "white" :bold t))
   (((class color) (background light)) (:foreground "purple" :bold t))
   (((class color) (background dark)) (:foreground "plum1" :bold t))
   (t (:bold t)))
 "info mode face used to highlight node."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'info-xref
 '((((class grayscale) (background light)) (:foreground "black" :bold t))
   (((class grayscale) (background dark)) (:foreground "white" :bold t))
   (((class color) (background light)) (:foreground "blue" :bold t))
   (((class color) (background dark)) (:foreground "cyan" :bold t))
   (t (:bold t)))
 "info mode face used to highlight xref."
 :group 'font-lock-highlighting-faces)
(custom-declare-face
 'sh-heredoc-face
 '((((class color) (background light)) (:foreground "sea green"))
   (((class color) (background dark)) (:foreground "yellow" :bold t))
   (t (:bold t)))
 "Face to show a here-document"
 :group 'sh-indentation)
(custom-declare-face
  'diff-context-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class color) (background light)) (:foreground "grey50"))
    (((class color) (background dark)) (:foreground "grey70"))
    (t ))
  "`diff-mode' face used to highlight context and other side-information."
  :group 'diff-mode)
(custom-declare-face
 'comint-highlight-prompt
 '((((background dark)) (:foreground "cyan"))
   (t (:foreground "cyan")))
 "Face to use to highlight prompt when `comint-highlight-prompt' is non-nil."
 :group 'comint)
(custom-declare-face
 'log-view-message-face
 '((((class color) (background light)) (:background "magenta"))
   (t (:bold t)))
 "Face for the message header line in `log-view-mode'."
 :group 'log-view)
(global-font-lock-mode t)

; alpha
(if (and (eq window-system 'x)
	 (>= emacs-major-version 23))
    (progn
      (setq initial-frame-alist
	    '((width . 180)
	      (height . 55)
	      (cursor-color . "Green")
	      (foreground-color . "White")
	      (background-color . "Black")
	      (alpha . (75 50 50 50))
	      (font . "Takaoゴシック-17")
;	      (font . "DejaVu Sans Mono-15")
	      ))
      (setq default-frame-alist initial-frame-alist)))

; for Meadow at MS-Windows
(if (eq window-system 'w32)
    (progn
      (setq initial-frame-alist
	    '((width . 100)
	      (height . 42)
	      (cursor-color . "Navy")
	      (foreground-color . "Black")
	      (background-color . "OldLace")
	      (alpha . (90 70 70 70))))
      (cond
       ((string-match "^varcolac" (downcase (system-name)))
	(setcdr (assq 'width initial-frame-alist) 120)
	(setcdr (assq 'height initial-frame-alist) 56))
       ((string-match "^cherry-blossom" (downcase (system-name)))
	(setcdr (assq 'width initial-frame-alist) 110)
	(setcdr (assq 'height initial-frame-alist) 50)))
      (setq default-frame-alist initial-frame-alist)))

; Shell mode
(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-output t)

; Info directories
(setq Info-default-directory-list
      (mapcar
       (lambda (path) (expand-file-name path))
       '("/usr/share/info" "/usr/local/info" "/usr/X11R6/info")))
(cond
 ((eq system-type 'windows-nt)
  (setq Info-directory-list
	(mapcar
	 (lambda (path) (expand-file-name path))
	 '("/usr/local/Meadow/1.14/info" "/usr/local/info")))))

; User key bindings
(load "term/bobcat")
(when (fboundp 'terminal-init-bobcat)
  (terminal-init-bobcat))
(if (boundp 'minibuffer-local-filename-completion-map)
    (progn
      (define-key minibuffer-local-filename-completion-map " "
	'minibuffer-complete-word)
      (define-key minibuffer-local-must-match-filename-map " "
	'minibuffer-complete-word)))
(global-set-key "\C-\\" 'help-command)
(global-set-key "\C-\\\C-\\" 'help-for-help)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-h" 'delete-backward-char)

; Frame title
(setq frame-title-format
      '(multiple-frames ("%b - " invocation-name "@" system-name)
			("" invocation-name "@" system-name)))

; Mode line information
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
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
(iswitchb-mode t)
(global-set-key "\C-x\C-b" 'ibuffer)
(defadvice switch-to-buffer (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer: " (other-buffer) t))))
(defadvice switch-to-buffer-other-window (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer in other window: " (other-buffer) t))))
(defadvice switch-to-buffer-other-frame (before strict-buffer-name activate)
  (interactive (list (read-buffer "Switch to buffer in other frame: " (other-buffer) t))))
(defun make-buffer (buffer-name)
  (interactive (list (read-buffer "New buffer: " nil nil)))
  (switch-to-buffer (get-buffer-create buffer-name)))
(defun make-buffer-other-window (buffer-name)
  (interactive (list (read-buffer "New buffer: " nil nil)))
  (switch-to-buffer-other-window (get-buffer-create buffer-name)))
(defun make-buffer-other-frame (buffer-name)
  (interactive (list (read-buffer "New buffer: " nil nil)))
  (switch-to-buffer-other-frame (get-buffer-create buffer-name)))
(defun duplicate-buffer (base-buffer-name)
  (let ((base-buffer (get-buffer base-buffer-name)))
    (let ((default-major-mode (cdr (assq 'major-mode
					 (buffer-local-variables base-buffer))))
	  (copy-buffer (make-indirect-buffer base-buffer
					     (generate-new-buffer-name
					      (concat "*" base-buffer-name " (copy)*")))))
      (set-buffer-major-mode copy-buffer)
      copy-buffer)))
(defun copy-buffer (base-buffer-name)
  (interactive (list (buffer-name)))
  (switch-to-buffer (duplicate-buffer base-buffer-name)))
(defun copy-buffer-other-window (base-buffer-name)
  (interactive (list (buffer-name)))
  (switch-to-buffer-other-window (duplicate-buffer base-buffer-name)))
(defun copy-buffer-other-frame (base-buffer-name)
  (interactive (list (buffer-name)))
  (switch-to-buffer-other-frame (duplicate-buffer base-buffer-name)))

; Window switching
(defun other-window-one-step (previous)
  (interactive "P")
  (if previous
      (other-window -1)
    (other-window 1)))
(global-set-key "\M-o" 'other-window-one-step)
(global-set-key "\C-^" 'other-window-one-step)
(setq truncate-partial-width-windows nil)

; No new lines
(setq next-line-add-newlines nil)

; Timestamp
(defun insert-timestamp ()
  (interactive)
  (insert (current-time-string)))
(defun insert-current-date()
  (interactive)
  (insert (format-time-string "%Y-%m-%d (%a)" (current-time))))
(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%H:%M:%S" (current-time))))
(global-set-key "\C-c;" 'insert-current-date)
(global-set-key "\C-c:" 'insert-current-time)

; HTML mode
(setq auto-mode-alist
      (append '(("\\.rhtml$" . html-mode)
		("\\.xhtml$" . sgml-mode)) auto-mode-alist))
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
(require 'ruby-test-unit)
(setq ruby-test-unit-runner-options "--no-use-color") ; for test-unit on ruby-2.2.0 or later.
(setq ruby-program-name
      (concat "ruby " (expand-file-name "/usr/local/bin/irb") " --inf-ruby-mode"))

; Major mode for RDoc editing
(autoload 'rdoc-mode "rdoc-mode" "Major mode for RD editing." t)
(setq auto-mode-alist
      (append '(("\\.rd$" . rdoc-mode)
		("\\.rd\\.[A-Za-z]*$" . rdoc-mode))
	      auto-mode-alist))

; Comparing files
(setq diff-switches "-u")

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
(setq sh-basic-offset 2)

;; ; Shell command completion
;; (autoload 'shell-command-with-completion
;;       "shell-command" "alternate shell-command" t nil)
;; (define-key global-map "\e!" 'shell-command-with-completion)
;; (autoload 'shell-command-with-completion-on-region
;;   "shell-command" "alternate shell-command-on-region" t nil)
;; (define-key global-map "\e|" 'shell-command-with-completion-on-region)

; Fetchmail
(autoload 'fetchmail "fetchmail" nil t)
(setq fetchmail-default-server "freedom")
(setq fetchmail-server-option-alist
      '(("sv01.phys.sci.kobe-u.ac.jp" "--check")))
(setq fetchmail-server-alias-alist
      '(("homesrv"  . "babayaga.plutonia.ne.jp")
	("freedom"  . "mail.freedom.ne.jp")
	("kobephys" . "sv01.phys.sci.kobe-u.ac.jp")))

; for Bookmark
(setq bookmark-search-size 32)

; Grep
(setq grep-command "egrep -ne ")

; Parenthesis
(show-paren-mode t)

; FLIM
(setq mime-field-decoding-max-size (* 64 1024))
(setq mime-header-lexical-analyzer	; http://lists.airs.net/wl/archive/199909/msg00009.html
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

; SKK
(autoload 'skk-mode "skk" nil t)
(autoload 'skk-auto-fill-mode "skk" nil t)
(autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)
(global-set-key "\C-x\C-j" (function skk-mode))
(global-set-key "\C-xj" '(function skk-auto-fill-mode))
(add-hook 'isearch-mode-hook (function skk-isearch-mode-setup))
(add-hook 'isearch-mode-end-hook (function skk-isearch-mode-cleanup))
(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
(if (eq system-type 'windows-nt)
    (setq skk-large-jisyo (concat (getenv "SystemDrive") skk-large-jisyo)))
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
      (mapcar
       (lambda (sdic-dictionary)
	 (setcar (cdr sdic-dictionary)
		 (expand-file-name (cadr sdic-dictionary)))
	 sdic-dictionary)
       '((sdicf-client "~/dict/gene.sdic.gz"
		       (title "GENE")
		       (strategy direct))
	 (sdicf-client "~/dict/eedict.sdic.gz"
		       (title "EEDICT")
		       (strategy direct)))))
(setq sdic-waei-dictionary-list
      (mapcar
       (lambda (sdic-dictionary)
	 (setcar (cdr sdic-dictionary)
		 (expand-file-name (cadr sdic-dictionary)))
	 sdic-dictionary)
       '((sdicf-client "~/dict/jedict.sdic.gz"
		       (title "JEDICT")
		       (strategy direct))
	 (sdicf-client "~/dict/jgene.sdic.gz"
		       (title "JGENE")
		       (strategy direct)))))

; Use unzip on zip mode
(setq archive-zip-use-pkzip nil)

; WWW browser
(setq w3m-coding-system
      (cond
       ((eq system-type 'windows-nt) 'shift_jis-dos)
       (t 'utf-8)))
(setq browse-url-browser-function 'browse-url-mozilla)
(setq mime-setup-enable-inline-html nil)
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(autoload 'w3m-find-file "w3m" "w3m Interface function for local file." t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(eval-after-load "w3m"
  '(defadvice w3m-reload-this-page (around no-cache activate)
     (let ((w3m-command-arguments
	    (append w3m-command-arguments
		    '("-header" "Pragma: no-cache"
		      "-header" "Cache-Control: no-cache"))))
       ad-do-it)))
(global-set-key "\C-xm" 'browse-url-at-point)
(setq shimbun-asahi-url "http://www.asahi.com/")
(setq shimbun-asahi-html-url "http://www.asahi.com/")
(custom-declare-face
 'w3m-form-face
 '((((class color) (background light)) (:foreground "red" :underline t))
   (((class color) (background dark)) (:foreground "red" :underline t))
   (t (:underline t)))
 "*Face to fontify forms."
 :group 'w3m-face)

; EWB mode
(autoload 'ewb-mode "ewb-mode" "" t)
(setq auto-mode-alist
      (append '(("\\.ewb$" . ewb-mode))
	      auto-mode-alist))

; disable Tool Bar
; Xresource => Emacs.toolBar: 0
(if (>= emacs-major-version 21)
    (tool-bar-mode 0))

; Mouse Wheel mode
(if (>= emacs-major-version 21)
    (mouse-wheel-mode 1))

;; ; Navi2ch
;; (require 'navi2ch)
;; (setq navi2ch-list-bbstable-url "http://menu.2ch.net/bbsmenu.html")
;; (if (eq system-type 'windows-nt)
;;     (setq navi2ch-directory "//cernobog/toki/.navi2ch"))

; HOME directory
(if (eq system-type 'windows-nt)
    (cd (expand-file-name "~")))

; patch for ediff
;; (if (and (featurep 'meadow)
;; 	 (string-match "Meadow-1\.15" (Meadow-version)))
    (progn
      (eval-after-load "ediff-init"
	'(defadvice ediff-window-display-p (after disable-window-display activate)
	   (setq ad-return-value nil)))
      (if (eq window-system 'w32)
	  (setq ediff-force-faces t)))
;;   )

; Big Brother Database
(setq bbdb-file
      (if (eq system-type 'windows-nt)
	  "//cernobog/toki/.bbdb"
	"~/.bbdb"))

; mini buffer
(if (eq emacs-major-version 21)
    (setq resize-mini-windows nil))

; for JavaScript
(autoload 'ecmascript-mode "ecmascript-mode"
  "Major mode for editing ECMAScript code." t)
(setq auto-mode-alist
      (append '(("\\.js$" . ecmascript-mode)) auto-mode-alist))

; for dired
(add-hook 'dired-load-hook
	  (lambda ()
	    (define-key dired-mode-map "W" 'browse-url-of-dired-file)))

; for subversion
;(require 'psvn)
;(add-to-list 'vc-handled-backends 'SVN)

; for python
(setq python-indent 2)

; for VBScript
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" . 
				 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-mode-indent 2)

; for git
;(require 'git)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)
(add-to-list 'vc-handled-backends 'GIT)
(if (eq system-type 'windows-nt) (setq git-cmd "git.exe"))

; Window
(setq split-width-threshold 300)

; no menu in CUI
(if (not window-system)
    (menu-bar-mode 0))

; memo
(defun change-log-markdown-open ()
  "open change-log.md"
  (interactive)
  (find-file "/mnt/c/Users/toki/OneDrive/ドキュメント/change-log.md")
  (goto-char (point-max)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-try-passive-mode t)
 '(package-selected-packages (quote (inf-ruby yari yaml-mode markdown-mode))))
