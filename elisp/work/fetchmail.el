;;; $Id$
;;; fetchmail.el --- Emacs から fetchmail を起動する。

(defvar fetchmail-use-passwd nil)
(defvar fetchmail-notify-beep t)

(defvar fetchmail-passwd nil)
(defvar fetchmail-protocol "POP3")
(defvar fetchmail-username (user-login-name))
(defvar fetchmail-mailserver "localhost")

(defvar fetchmail-process-name "fetchmail")
(defvar fetchmail-buffer-name "*fetchmail*")

(defun fetchmail (query-passwd)
  (interactive "P")
  ; 二重起動のチェック
  (if (get-process fetchmail-process-name)
      (error "Fetchmail is running."))
  ; パスワードの入力
  (if fetchmail-use-passwd
      (if (or (not fetchmail-passwd) query-passwd)
	  (setq fetchmail-passwd (read-passwd "Fetchmail password: "))))
  ; Fetchmail バッファの初期化
  (save-excursion
    (get-buffer-create fetchmail-buffer-name)
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert "<<< fetchmail >>>\n"))
  (let ((process-connection-type t)
	fetchmail-process)
    (setq fetchmail-process
	  ; Fetchmail の起動
	  (start-process fetchmail-process-name fetchmail-buffer-name
			 "fetchmail"
			 "--protocol" fetchmail-protocol
			 "--username" fetchmail-username
			 fetchmail-mailserver))
    (set-process-sentinel
     fetchmail-process
     ; Fetchmail 終了の通知
     (lambda (process event)
       (cond
	((string-match "finished" event)
	 (message "You have mail.")
	 (if fetchmail-notify-beep (beep))
	 (force-mode-line-update))
	((string-match "exited" event)
	 (if (= 1 (process-exit-status process))
	     (message "You have no mail.")
	   (message "Fetchmail failure!"))
	 (if fetchmail-notify-beep (beep))))))
    (if fetchmail-use-passwd
	; Fetchmail にパスワードを入力する
	(progn
	  (catch 'query
	    ; パスワードプロンプトの表示を待つ
	    (while t
	      (save-excursion
		(set-buffer fetchmail-buffer-name)
		(goto-char (process-mark fetchmail-process))
		(beginning-of-line)
		(if (string-match "^Enter password"
				  (buffer-substring
				   (point)
				   (process-mark fetchmail-process)))
		    (throw 'query nil)))
	      (sleep-for 0.1)))
	  (process-send-string
	   (process-name fetchmail-process) fetchmail-passwd)
	  (process-send-eof
	   (process-name fetchmail-process))))))
