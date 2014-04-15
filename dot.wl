; -*- mode: emacs-lisp; coding: utf-8 -*-

;;;
;;; Wanderlust
;;;

; SSL
(setq ssl-program-name "openssl")
(setq ssl-program-arguments
  '("s_client"
    "-quiet"
    "-host" host
    "-port" service
    "-verify" (int-to-string ssl-certificate-verification-policy)
    "-CApath" ssl-certificate-directory
    ))

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
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-port 993)
(setq elmo-imap4-default-stream-type 'ssl)
(setq elmo-imap4-default-user "yr10ki@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-use-modified-utf7 t)
(setq elmo-nntp-default-server "news.edit.ne.jp")
(setq wl-smtp-posting-server "mail.freedom.ne.jp")
(setq wl-smtp-posting-port 587)
(setq wl-smtp-posting-user "toki")
(setq wl-smtp-authenticate-type "plain")

; Local Domain
(setq wl-local-domain "wl-message-id-domain")

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

; Expire
(setq wl-expire-use-log t)
(setq wl-summary-expire-reserve-marks
      '(; "$"
	"N" "U" "!"))

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
(setq wl-template-alist
      '(("sig:japanese"
	 ("From" . wl-from)
	 (bottom .
"-
土岐 仁謙 (TOKI Yoshinori) <toki@freedom.ne.jp>
"))
	("sig:english"
	 ("From" . "Yoshinori Toki <toki@freedom.ne.jp>")
	 (bottom .
"-
Yoshinori Toki <toki@freedom.ne.jp>
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

; Color
(set-face-foreground 'wl-highlight-message-cited-text-2 "DeepPink")
(set-face-foreground 'wl-highlight-summary-new-face "Magenta")
