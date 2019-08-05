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
(setq wl-default-spec "%")
(setq wl-stay-folder-window t)

; Server
(setq elmo-default-nntp-server "news7.dion.ne.jp")
(setq elmo-default-pop3-server "mail.freedom.ne.jp")
(setq elmo-default-imap4-server "mail.plutonian.ne.jp")
(setq wl-smtp-posting-server "mail.freedom.ne.jp")
(setq wl-draft-send-mail-func 'wl-draft-send-mail-with-pop-before-smtp)

; Offline mode
(setq wl-plugged nil)
(if (equal (system-name) "cernobog.plutonian.ne.jp")
    (add-hook 'wl-make-plugged-hook
	      (function (lambda () (elmo-set-plugged t "mail.plutonian.ne.jp" 143)))))

; Message
(setq elmo-msgdb-extra-fields '("X-ML-Name" "Newsgroups"))
(setq elmo-archive-treat-file t)
(setq wl-alias-file "~/.aliases")
(setq wl-summary-important-above 0)
(setq wl-summary-temp-above 1000)
(setq wl-message-id-domain "mail.freedom.ne.jp")
(setq wl-summary-auto-refile-skip-marks ())
(setq wl-refile-rule-alist
      '(; ͽ��
	("Subject"
	 ("^\\[ͽ��\\]" . "%#mh/schedule"))
	; ������
	("From"
	 ("^DIARY Archive CGI (toki@imopc7"          . "%#mh/diary/local")
	 ("^DIARY Archive CGI (toki@ppbdbs01"        . "%#mh/diary/local")
	 ("^DIARY Archive CGI (nobody@\\(web\\|www\\)\\.freedom" . "%#mh/diary/freedom"))
	; �׻���
	("Subject"
	 ("cernobog" . "%#mh/admin/cernobog")
	 ("babayaga" . "%#mh/admin/babayaga")
	 ("root"     . "%#mh/admin/root"))
	("To"
	 ("root@cernobog" . "%#mh/admin/cernobog")
	 ("root@babayaga" . "%#mh/admin/babayaga")
	 ("root"          . "%#mh/admin/root"))
	; Ruby
	("X-ML-Name"
	 ("ruby-list" . "%#mh/ruby/list"))
	; �ޤ��ޤ�
	("Subject"
	 ("^\\[Weekly Mag2"                         . "%#mh/mag2/weekly")
	 ("^\\[\\(��Ǿ�̾�����\\|�������Ϥ���\\)" . "%#mh/mag2/id.0000003443")
	 ("^\\[�����Ρ���"                          . "%#mh/mag2/id.0000004784")
	 ("^���ä���ͤξ��󥷥��ƥ��"             . "%#mh/mag2/id.0000016004"))
	("From"
	 ("mag2" . "%#mh/mag2"))))
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
		 (("To" "Cc" "From") ("bess-japan") "%#mh/kobe/bess/japan")
		 (("To" "Cc" "From") ("bess-workers") "%#mh/kobe/bess/workers")
		 (("To" "Cc" "From") ("chousan" "anraku" "imori") "%#mh/kobe/bess/fadcif")
		 ; �ե꡼����
		 (("To" "Cc" "From") ("info@freedom.ne.jp") "%#mh/freedom")
		 ; JLC
		 (("To" "Cc" "From") ("jlc") "%#mh/kobe/jlc")
		 ; ����
		 (("To" "Cc" "From") ("srvadm") "%#mh/kobe/computer/srvadm")
		 (("To" "Cc" "From") ("pp-bulletin") "%#mh/kobe/pp-bulletin")
		 (("To" "Cc" "From") ("m1@astro") "%#mh/kobe/m1")
		 (("To" "Cc" "From") ("m2@astro") "%#mh/kobe/m2")
		 ))))

; Expire
(setq wl-expire-use-log t)
(setq wl-summary-expire-reserve-marks
      '(; "$"
	"N" "U" "!"))
(setq wl-expire-alist
      '(("^%#mh/send$"                 (date 7)         trash)
	("^%#mh/trash$"                (number 100 130) remove)
	("^%#mh/ruby/list$"            (number 100 130) wl-expire-archive-date)
	("^%#mh/mag2/weekly$"          (number 100 130) wl-expire-archive-date)
	("^%#mh/mag2/id\\.0000003443$" (number 100 130) wl-expire-archive-date)
	("^%#mh/diary/freedom$"        (number 100 130) wl-expire-archive-date)
	("^%#mh/freedom$"              (number 100 130) wl-expire-archive-date)
	("^%#mh/admin/root$"           (number 100 130) wl-expire-archive-date)
	("^%#mh/admin/mail$"           (number 100 130) wl-expire-archive-date)
	("^%#mh/admin/babayaga$"       (number 100 130) wl-expire-archive-date)
	("^%#mh/admin/cernobog$"       (number 100 130) wl-expire-archive-date)
	("^%#mh/admin/root$"           (number 100 130) wl-expire-archive-date)
        ))
(if (and
     (equal system-name "cernobog.plutonian.ne.jp")
     (eq system-type 'berkeley-unix))
    (add-hook
     'wl-summary-prepared-pre-hook
     (function
      (lambda ()
	(cond
	 (t
	  (wl-summary-expire)))))))

; Draft
(setq wl-interactive-send t)
(setq wl-user-mail-address-list
      '("toki@freedom.ne.jp"
	"toki@phys.sci.kobe-u.ac.jp"
	"toki@hep.phys.sci.kobe-u.ac.jp"
	"toki@sv01.phys.sci.kobe-u.ac.jp"
	"toki@icepp.s.u-tokyo.ac.jp"
	"toki@imopc7.icepp.s.u-tokyo.ac.jp"))
(setq wl-from "�ڴ� �θ� (TOKI Yoshinori) <toki@freedom.ne.jp>")
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
�Ȥ���������˽��������줿ʪ���ؼԤ�������
��Ϲ��Ĥ˥ޥ���Ľ񤭡֤ޤ�������Ȳ��ꤷ�ޤ��ġ�
�ڴ� �θ� (TOKI Yoshinori) <toki@freedom.ne.jp>
"))
	("sig:ed5"
	 (bottom .
"--------------------------------------------------
�ֿ�̵�����ơ���ʳ�鷺����̵�����ơ����Ϥ�������
�ڴ� �θ� (TOKI Yoshinori) <toki@freedom.ne.jp>
"))
	("sig:grappler"
	 (bottom .
"--------------------------------------------------
�ָ��¤äƤ����ʤ��������Ǳ��줽��������
�ڴ� �θ� (http://www.freedom.ne.jp/toki/)
"))
	("sig:chernobog"
	(bottom .
"--------------------------------------------------
�ڴ� �θ� (http://www.freedom.ne.jp/toki/)
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
