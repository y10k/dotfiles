; -*- mode: emacs-lisp;-*-
; $Id$

;;;
;;; Wanderlust
;;;

; Win32 HOME network settings
(cond
 ((and (eq window-system 'w32)
       (or (string-match "^[Cc][Ee][Rr][Nn][Oo][Bb][Oo][Gg]" (system-name))
	   (string-match "^[Vv][Aa][Rr][Cc][Oo][Ll][Aa][Cc]" (system-name))))
  (setq elmo-msgdb-directory "//cernobog/toki/.elmo")
  (setq elmo-localdir-folder-path "//cernobog/toki/Mail")))

; Folder
(setq wl-strict-diff-folders '("^\\+inbox$" "@.*\\.plutonian\\.ne\\.jp" "@mail\\.freedom\\.ne\\.jp"))
(setq wl-auto-check-folder-list '("^\\+inbox$" "@.*\\.plutonian\\.ne\\.jp"))
(setq wl-auto-uncheck-folder-list '("."))
(setq wl-default-folder "+inbox")
(setq wl-default-spec "%")
(setq wl-stay-folder-window t)

; Server
(setq elmo-pop3-default-server "mail.freedom.ne.jp")
(setq elmo-imap4-default-server "cernobog.plutonian.ne.jp")
(setq elmo-imap4-default-authenticate-type 'cram-md5)
(setq elmo-nntp-default-server "news.edit.ne.jp")
(setq wl-smtp-posting-server "mail.freedom.ne.jp")
(setq wl-draft-send-mail-function 'wl-draft-send-mail-with-pop-before-smtp)

; Local Domain
(setq wl-local-domain "plutonia.ne.jp")

; Offline mode
(setq wl-plugged nil)
(if (or (string-match "^[Cc][Ee][Rr][Nn][Oo][Bb][Oo][Gg]" (system-name))
	(string-match "^[Vv][Aa][Rr][Cc][Oo][Ll][Aa][Cc]" (system-name))
	(string-match "^[Bb][Aa][Bb][Aa][Yy][Aa][Gg][Aa]" (system-name)))
    (add-hook 'wl-make-plugged-hook
	      (function
	       (lambda ()
		 (elmo-set-plugged t "mail.freedom.ne.jp" 110)
		 (elmo-set-plugged t "mx.edit.ne.jp" 110)
		 (elmo-set-plugged t "babayaga.plutonian.ne.jp" 110)
		 (elmo-set-plugged t "cernobog.plutonian.ne.jp" 110)
		 (elmo-set-plugged t "cernobog.plutonian.ne.jp" 143)
		 (elmo-set-plugged t "imap.gmail.com" 993 'ssl)
		 (elmo-set-plugged t "news.edit.ne.jp" 119)
		 (elmo-set-plugged t "shimbun")))))

; Message
(setq elmo-msgdb-extra-fields '("X-ML-Name" "Newsgroups"))
(setq elmo-archive-treat-file t)
(setq wl-alias-file "~/.aliases")
(setq wl-summary-important-above 0)
(setq wl-summary-target-above 1000)
(setq wl-message-id-domain "mail.freedom.ne.jp")
(setq wl-summary-auto-refile-skip-marks ())
(setq wl-refile-rule-alist
      '(; Ruby
	("X-ML-Name"
	 ("ruby-list" . "%INBOX.ruby.list")
	 ("rubyunit"  . "%INBOX.ruby.unit")
	 ("rubybook"  . "%INBOX.ruby.unit"))
	; ひびとも
	("Subject"
	 ("ひびとも通信" . "%INBOX.hibitomo"))
	; まぐまぐ
	("Subject"
	 ("^\\[Weekly Mag2"                         . "%INBOX.mag2.weekly")
	 ("^\\[\\(電脳通情報部\\|明日を創るもの\\|もっとわかる＠ＩＴ\\)"
	                                            . "%INBOX.mag2.id_0000003443")
	 ("^\\[今週の○○"                          . "%INBOX.mag2.id_0000004784")
	 ("^たった一人の情報システム課"             . "%INBOX.mag2.id_0000016004"))
	("From"
	 ("mag2" . "%INBOX.mag2"))
	; 日記ログ
	("From"
	 ("^DIARY Archive CGI (nobody@\\(web\\|www\\)\\.freedom" . "%INBOX.diary"))
	; 計算機
	("From"
	 ("Cron Daemon" . "%INBOX.admin.cron"))
	("Subject"
	 ("cernobog" . "%INBOX.admin.cernobog")
	 ("babayaga" . "%INBOX.admin.babayaga")
	 ("root"     . "%INBOX.admin.root"))
	("To"
	 ("root@cernobog" . "%INBOX.admin.cernobog")
	 ("root@babayaga" . "%INBOX.admin.babayaga")
	 ("root"          . "%INBOX.admin.root"))))
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
		 ; フリーダム
		 (("To" "Cc" "From") ("info@freedom.ne.jp") "%INBOX.freedom")
		 ; TSUTAYA
		 (("To" "Cc" "From") ("@tsutaya\\.co\\.jp") "%INBOX.tsutaya")
		 ; NTT
		 (("To" "Cc" "From") ("ntt\\.co\\.jp") "%INBOX.ntt")
		 ))))

; Expire
(setq wl-expire-use-log t)
(setq wl-summary-expire-reserve-marks
      '(; "$"
	"N" "U" "!"))
;; (setq wl-expire-alist
;;       '(("^+send$"                 (date 7)         trash)
;; 	("^+trash$"                (number 100 130) remove)
;; 	("^+ruby/list$"            (number 100 130) wl-expire-archive-date)
;; 	("^+mag2/weekly$"          (number 100 130) wl-expire-archive-date)
;; 	("^+mag2/id\\.0000003443$" (number 100 130) wl-expire-archive-date)
;; 	("^+diary/freedom$"        (number 100 130) wl-expire-archive-date)
;; 	("^+freedom$"              (number 100 130) wl-expire-archive-date)
;; 	("^+admin/root$"           (number 100 130) wl-expire-archive-date)
;; 	("^+admin/mail$"           (number 100 130) wl-expire-archive-date)
;; 	("^+admin/babayaga$"       (number 100 130) wl-expire-archive-date)
;; 	("^+admin/cernobog$"       (number 100 130) wl-expire-archive-date)
;; 	("^+admin/root$"           (number 100 130) wl-expire-archive-date)
;; 	))
; (if (and
;      (equal system-name "cernobog.plutonian.ne.jp")
;      (eq system-type 'berkeley-unix))
;     (add-hook
;      'wl-summary-prepared-pre-hook
;      (function
;       (lambda ()
; 	(cond
; 	 (t
; 	  (wl-summary-expire)))))))

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
	 ("X-GnuPG-Fingerprint" . "2968 565F 0550 57D3 1AF8  E03F 520A 03B6 FAC1 4744"))
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

;; ; X-Face
;; (cond ((featurep 'xemacs)
;;        ;;
;;        )
;;       ((= 21 emacs-major-version)
;;        (setq wl-highlight-x-face-function 'x-face-decode-message-header)
;;        (define-key wl-summary-mode-map "\C-x4s" 'x-face-save)
;;        (define-key wl-draft-mode-map "\C-x4i" 'x-face-insert)
;;        ;; "\M-t" key is reserved for wl command.
;;        (define-key wl-draft-mode-map "\M-\C-t" 'x-face-show)
;;        ;;
;;        ;; If a file name has no directory component, it should be
;;        ;; found in the directory which is specified by the option
;;        ;; `x-face-image-file-directory'.
;;        (setq x-face-default-xbm-file "YourFace.xbm")
;;        ;;
;;        (add-hook 'wl-mail-setup-hook 'x-face-insert)
;;        ;; If you use `wl-draft-insert-x-face-field' instead of
;;        ;; `x-face-insert' for inserting an X-Face, you can highlight
;;        ;; it as an image with the setting of the following hook:
;;        (add-hook 'wl-draft-insert-x-face-field-hook
;; 		 (lambda nil
;; 		   (x-face-insert wl-x-face-file)))
;;        )
;;       (t
;;        ;;
;;        ))


;; ; IM
;; (defun TT:wl-inc-mail ()
;;   (interactive)
;;   (message "Incing ... ")
;;   (call-process "imget" nil nil nil)
;;   (if (and (boundp 'wl-summary-buffer-folder-name)
;;            (eq wl-summary-buffer-folder-name wl-default-folder))
;;       (wl-summary-sync-force-update)
;;     (wl-summary-goto-folder-subr wl-default-folder 'force-update nil nil)))
;; (add-hook
;;  'wl-folder-mode-hook
;;  (function
;;   (lambda ()
;;     (define-key wl-folder-mode-map  "\M-i" 'TT:wl-inc-mail))))
;; (add-hook
;;  'wl-summary-mode-hook
;;  (function
;;   (lambda ()
;;     (define-key wl-summary-mode-map "\M-i" 'TT:wl-inc-mail))))

; Color
(set-face-foreground 'wl-highlight-message-cited-text-2 "DeepPink")
(set-face-foreground 'wl-highlight-summary-new-face "Magenta")
