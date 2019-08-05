; -*- mode: emacs-lisp;-*-
; $Id$

;;;
;;; Wanderlust
;;;

; Win32 HOME network settings
(cond
 ((and (eq window-system 'w32)
       (equal (system-name) "CERNOBOG"))
  (setq elmo-msgdb-dir "//babayaga/toki/.elmo")
  (setq elmo-localdir-folder-path "//babayaga/toki/Mail")))

; Folder
(setq wl-strict-diff-folders '("inbox" "toki"))
(setq wl-auto-check-folder-list '("inbox" "toki"))
(setq wl-auto-uncheck-folder-list '("."))
(setq wl-default-folder "+inbox")
(setq wl-default-spec "+")
(setq wl-stay-folder-window t)

; Server
(setq elmo-default-nntp-server "news.neweb.ne.jp")
(setq elmo-default-pop3-server "mail.freedom.ne.jp")
(setq wl-smtp-posting-server "mail.freedom.ne.jp")
(setq wl-draft-send-mail-func 'wl-draft-send-mail-with-pop-before-smtp)

; Offline mode
(setq wl-plugged nil)
(add-hook 'wl-make-plugged-hook
	  (function
	   (lambda ()
	     (elmo-set-plugged t "babayaga" 110))))

; Message
(setq elmo-msgdb-extra-fields '("X-ML-Name" "Newsgroups"))
(setq elmo-archive-treat-file t)
(setq wl-alias-file "~/.aliases")
(setq wl-summary-important-above 0)
(setq wl-summary-temp-above 1000)
(setq wl-message-id-domain "mail.freedom.ne.jp")
(setq wl-summary-auto-refile-skip-marks ())
(setq wl-refile-rule-alist
      '(; 予定
	("Subject"
	 ("^\\[予定\\]" . "+schedule"))
	; 日記ログ
	("From"
	 ("^DIARY Archive CGI (toki@imopc7"          . "+diary/local")
	 ("^DIARY Archive CGI (toki@ppbdbs01"        . "+diary/local")
	 ("^DIARY Archive CGI (nobody@\\(web\\|www\\)\\.freedom" . "+diary/freedom"))
	; 計算機
	("Subject"
	 ("cernobog" . "+admin/cernobog")
	 ("babayaga" . "+admin/babayaga")
	 ("root"     . "+admin/root"))
	("To"
	 ("root@cernobog" . "+admin/cernobog")
	 ("root@babayaga" . "+admin/babayaga")
	 ("root"          . "+admin/root"))
	; Ruby
	("X-ML-Name"
	 ("ruby-list" . "+ruby/list"))
	; まぐまぐ
	("Subject"
	 ("^\\[Weekly Mag2"                         . "+mag2/weekly")
	 ("^\\[\\(電脳通情報部\\|明日を創るもの\\)" . "+mag2/id.0000003443")
	 ("^\\[今週の○○"                          . "+mag2/id.0000004784")
	 ("^たった一人の情報システム課"             . "+mag2/id.0000016004"))
	("From"
	 ("mag2" . "+mag2"))))
(setq wl-refile-rule-alist
      (append wl-refile-rule-alist
	      ((lambda (rule-src-list)
		 (apply (function append) 
			(mapcar
			 (lambda (rule-src)
			   (apply (lambda (field-list pattern-list folder)
				    (mapcar
				     (lambda (field)
				       (cons field
					     (mapcar
					      (lambda (pattern)
						(cons pattern folder))
					      pattern-list)))
				     field-list))
				  rule-src))
			 rule-src-list)))
	       '(
		 ; BESS
		 (("To" "Cc" "From") ("bess-japan") "+kobe/bess/japan")
		 (("To" "Cc" "From") ("bess-workers") "+kobe/bess/workers")
		 (("To" "Cc" "From") ("chousan" "anraku" "imori") "+kobe/bess/fadcif")
		 ; フリーダム
		 (("To" "Cc" "From") ("info@freedom.ne.jp") "+freedom")
		 ; JLC
		 (("To" "Cc" "From") ("jlc") "+kobe/jlc")
		 ; 神戸
		 (("To" "Cc" "From") ("srvadm") "+kobe/computer/srvadm")
		 (("To" "Cc" "From") ("pp-bulletin") "+kobe/pp-bulletin")
		 (("To" "Cc" "From") ("m1@astro") "+kobe/m1")
		 (("To" "Cc" "From") ("m2@astro") "+kobe/m2")
		 ))))

; Expire
(setq wl-expire-use-log t)
(setq wl-summary-expire-reserve-marks
      '(; "$"
	"N" "U" "!"))
(setq wl-expire-alist
      '(("^\\+send$"                 (date 7)         trash)
	("^\\+trash$"                (number 100 130) remove)
	("^\\+ruby/list$"            (number 100 130) wl-expire-archive-date)
	("^\\+mag2/weekly$"          (number 100 130) wl-expire-archive-date)
	("^\\+mag2/id\\.0000003443$" (number 100 130) wl-expire-archive-date)
	("^\\+diary/freedom$"        (number 100 130) wl-expire-archive-date)
	("^\\+freedom$"              (number 100 130) wl-expire-archive-date)
	("^\\+admin/root$"           (number 100 130) wl-expire-archive-date)
	("^\\+admin/mail$"           (number 100 130) wl-expire-archive-date)
	("^\\+admin/babayaga$"       (number 100 130) wl-expire-archive-date)
	("^\\+admin/cernobog$"       (number 100 130) wl-expire-archive-date)
	("^\\+admin/root$"           (number 100 130) wl-expire-archive-date)
	))
(add-hook
 'wl-summary-prepared-pre-hook
 (function
  (lambda ()
    (if (or (string-match "^%" wl-summary-buffer-folder-name)
	    (string-match "^-" wl-summary-buffer-folder-name)
	    (string-match "^&" wl-summary-buffer-folder-name))
	(if wl-plugged (wl-summary-expire))
      (wl-summary-expire)))))

; Draft
(setq wl-interactive-send t)
(setq wl-user-mail-address-list
      '("toki@freedom.ne.jp"
	"toki@phys.sci.kobe-u.ac.jp"
	"toki@hep.phys.sci.kobe-u.ac.jp"
	"toki@sv01.phys.sci.kobe-u.ac.jp"
	"toki@icepp.s.u-tokyo.ac.jp"
	"toki@imopc7.icepp.s.u-tokyo.ac.jp"))
(setq wl-from "土岐 仁謙 (TOKI Yoshinori) <toki@freedom.ne.jp>")
(setq wl-fcc "+send")
(setq wl-draft-reply-without-argument-list
      '(("Followup-To" . (nil nil ("Followup-To")))
	("Mail-Followup-To" . (("Mail-Followup-To") nil ("Newsgroups")))
	(("X-ML-Name" "Reply-To") . (("Reply-To") nil nil))
	("From" . (("From") ("To" "Cc") ("Newsgroups")))))
(setq wl-draft-always-delete-myself t)
(setq wl-draft-config-alist
      '((t
	 ("X-PGP-Fingerprint" . "D0 A8 90 AB 73 F8 34 FE  CE CA DB BF 01 30 C0 35"))
	))
(setq wl-template-alist
      '(("sig:phys"
	 (bottom .
"------------------------------------------------------------
とある酪農場に助言を求められた物理学者の説明。
彼は黒板にマルを一つ書き「まず、牛を球と仮定します…」
土岐 仁謙 (TOKI Yoshinori) <toki@freedom.ne.jp>
"))
	("sig:ed5"
	 (bottom .
"--------------------------------------------------
「心無くして、技奮わず。技無くして、心届かず。」
土岐 仁謙 (TOKI Yoshinori) <toki@freedom.ne.jp>
"))
	("sig:grappler"
	 (bottom .
"--------------------------------------------------
「現実ってこうなんだ。心が捻られそうだ。」
土岐 仁謙 (http://www.freedom.ne.jp/toki/)
"))
	("sig:chernobog"
	(bottom .
"--------------------------------------------------
土岐 仁謙 (http://www.freedom.ne.jp/toki/)
Cop, cop, kopocam! Va! Sagana! Sagana! Va!
"))
	))
(add-hook
 'wl-mail-setup-hook
 (function
  (lambda ()
    (set-buffer-file-coding-system 'iso-2022-jp-unix))))
(add-hook
 'wl-mail-setup-hook
 (function
  (lambda ()
    (define-key wl-draft-mode-map "\C-c\C-y" 'xcite-yank-cur-msg))))

; IM
(defun TT:wl-inc-mail ()
  (interactive)
  (message "Incing ... ")
  (call-process "imget" nil nil nil)
  (if (and (boundp 'wl-summary-buffer-folder-name)
           (eq wl-summary-buffer-folder-name wl-default-folder))
      (wl-summary-sync-force-update)
    (wl-summary-goto-folder-subr wl-default-folder 'force-update nil nil)))
(add-hook
 'wl-folder-mode-hook
 (function
  (lambda ()
    (define-key wl-folder-mode-map  "\M-i" 'TT:wl-inc-mail))))
(add-hook
 'wl-summary-mode-hook
 (function
  (lambda ()
    (define-key wl-summary-mode-map "\M-i" 'TT:wl-inc-mail))))

; Color
(set-face-foreground 'wl-highlight-message-cited-text-2 "DeepPink")
(set-face-foreground 'wl-highlight-summary-new-face "Magenta")
