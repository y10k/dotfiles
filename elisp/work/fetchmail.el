;;; fetchmail.el --- Emacs から fetchmail を起動する。
;;; $Id$
;;
;; <<< 作者 >>>
;; 土岐 仁謙 <toki@freedom.ne.jp>
;;
;; <<< URL >>>
;; http://www.freedom.ne.jp/toki/elisp/fetchmail.el
;;
;; <<< 使用法 >>>
;; まずfetchmail.elをload-pathの通ったディレクトリに置いて.emacsに
;;   (autoload 'fetchmail "fetchmail" nil t)
;; というコードを追加し、そして次に
;; fetchmail-server-param-alist変数の設定をします。
;; このとき注意しないといけないのがomit-passwdパラメータの設定で、
;; ~/.fetchmailrcでパスワードの設定をしているときは必ずtを設定して下さい。
;; ~/.fetchmailrc の設定とomit-passwdの設定が食い違っていると
;; fetchmail.elは正常に動作しないので、気をつけてください。
;; 後は好みに応じて
;;   fetchmail-default-server
;;   fetchmail-preprocess-hook
;;   fetchmail-postprocess-hook
;;   fetchmail-notify-beep
;;   fetchmail-window
;;   fetchmail-window-time-format
;;   fetchmail-window-height-ratio
;;   fetchmail-window-height-lower-limit
;;   fetchmail-window-height-upper-limit
;; これらの変数の値を適当に設定してください。
;; 面倒ならデフォルト値のままでも構いません。
;; 後はM-x fetchmailを実行するとfetchmailが起動します。
;; omit-passwdをnilに設定しているかあるいは設定していなければ、
;; 最初に一度だけパスワードを問い合わせて記憶し、
;; 二回目の実行からは記憶したパスワードを使用します。
;; このときパスワードはfetchmail-server-passwd-alist変数に記憶され、
;; Emacs Lispに慣れた人なら簡単に取り出せてしまうので、
;; 端末の前を離れるときは注意してください。
;;

(defvar fetchmail-default-server nil
  "デフォルトのサーバ。")

(defvar fetchmail-server-param-alist '(("localhost" . ()))
  "サーバのパラメータを設定する連想リスト。
例: '((\"hepsun2.phys.sci.kobe-u.ac.jp\" .
       ((protocol . \"imap\")))
      (\"phys03.phys.sci.kobe-u.ac.jp\" .
       ((omit-passwd . t)
        (protocol . \"apop\"))))")

(defvar fetchmail-server-alias-alist ()
  "サーバの別名を設定する連想リスト。
例: '((\"KOBEHEP\" . \"hepsun2.phys.sci.kobe-u.ac.jp\")
      (\"KOBEPHYS\" . \"phys03.phys.sci.kobe-u.ac.jp\"))")

(defvar fetchmail-preprocess-hook ()
  "fetchmail の前処理を登録するフック。")

(defvar fetchmail-postprocess-hook ()
  "fetchmail の後処理を登録するフック。")

(defvar fetchmail-param-func-alist
  '((omit-passwd . fetchmail-param-omit-passwd)
    (check       . fetchmail-param-check)
    (username    . fetchmail-param-username)
    (protocol    . fetchmail-param-protocol)
    (port        . fetchmail-param-port)
    (timeout     . fetchmail-param-timeout)
    (folder      . fetchmail-param-folder)
    (keep        . fetchmail-param-keep)
    (flush       . fetchmail-param-flush))
  "パラメータのシンボルをキーに持ちパラメータの値を解析する関数のシンボルを
値に持つ連想リスト。fetchmail-server-param-alist関数のパラメータの解析に
使われる。")

(defvar fetchmail-server-passwd-alist ()
  "サーバのパスワードを保存する連想リスト。")

(defvar fetchmail-notify-beep t
  "この変数が真のときfetchmailが終了したことをbeep音で知らせる。")

(defvar fetchmail-window t
  "この変数が真のときfetchmailバッファをウィンドウで開く。")

(defvar fetchmail-window-time-format " [%a %b %e %T %Y]"
  "Fetchmailを起動した時刻を表示する書式。
この変数にnilを設定すると時刻を表示しない。")

(defvar fetchmail-window-height-ratio 0.15
  "Fetchmailウィンドウの高さの割合。")

(defvar fetchmail-window-height-lower-limit 5
  "Fetchmailウィンドウの高さの下限。")

(defvar fetchmail-window-height-upper-limit 10
  "Fetchmailウィンドウの高さの上限。")

(defvar fetchmail-last-server nil
  "最後に使われたサーバの名前が入っている。
fetchmail-start関数が自動的に設定するので、ユーザが設定してはいけない。")

(defvar fetchmail-process-name "fetchmail"
  "Fetchmailプロセスの名前。")

(defvar fetchmail-buffer-name "*fetchmail*"
  "Fetchmailバッファの名前。")

(defvar fetchmail-running nil
  "Fetchmailが動作中であることを表わすマイナーモード変数")
(unless (assq 'fetchmail-running minor-mode-alist)
  (setq minor-mode-alist
	(cons '(fetchmail-running " Fetching mail...")
	      minor-mode-alist)))

(defun fetchmail-set-passwd (fetchmail-server fetchmail-passwd)
  "fetchmail-server-passwd-alistにパスワードを設定する。"
  (let ((fetchmail-server-passwd-pair
	 (assoc fetchmail-server
		fetchmail-server-passwd-alist)))
    (if fetchmail-server-passwd-pair
	(setcdr fetchmail-server-passwd-pair
		fetchmail-passwd)
      (setq fetchmail-server-passwd-alist
	    (cons (cons fetchmail-server fetchmail-passwd)
		  fetchmail-server-passwd-alist)))))

(defun fetchmail-get-passwd (fetchmail-server)
  "fetchmail-server-passwd-alistからパスワードを得る。"
  (cdr (assoc fetchmail-server
	      fetchmail-server-passwd-alist)))

(defun fetchmail-clear-passwd (fetchmail-server)
  "fetchmail-server-passwd-alistからパスワードを削除する。"
  (fetchmail-set-passwd fetchmail-server nil))

(defun fetchmail-get-server-param (fetchmail-server fetchmail-param)
  "fetchmail-server-param-alistからサーバのパラメータを得る。"
  (cdr (assq fetchmail-param
	     (cdr (assoc fetchmail-server
			 fetchmail-server-param-alist)))))

(defun fetchmail-query-passwd (fetchmail-server)
  "サーバのパスワードを設定する。"
  (unless (fetchmail-get-passwd fetchmail-server)
    (fetchmail-set-passwd fetchmail-server
			  (base64-encode-string
			   (read-passwd (format "Password for %s: "
						fetchmail-server)) nil))))

(defun fetchmail-param-omit-passwd (fetchmail-server omit-passwd)
  "パスワードの入力を省略する。"
  (setq fetchmail-query-passwd nil)
  ())

(defun fetchmail-param-check (fetchmail-server check)
  "Checkオプションのリストを作る。"
  (if check (list "--check")))

(defun fetchmail-param-username (fetchmail-server username)
  "Usernameオプションのリストを作る。"
  (if username (list "--username" username)))

(defun fetchmail-param-protocol (fetchmail-server protocol)
  "Protocolオプションのリストを作る。"
  (if protocol (list "--protocol" protocol)))

(defun fetchmail-param-port (fetchmail-server port)
  "Portオプションのリストを作る。"
  (if port (list "--port" (number-to-string port))))

(defun fetchmail-param-timeout (fetchmail-server timeout)
  "Timeoutオプションのリストを作る。"
  (if timeout (list "--timeout" (number-to-string timeout))))

(defun fetchmail-param-folder (fetchmail-server folder)
  "Folderオプションのリストを作る。"
  (if folder (list "--folder" folder)))

(defun fetchmail-param-keep (fetchmail-server keep)
  "Keepオプションのリストを作る。"
  (if keep (list "--keep")))

(defun fetchmail-param-flush (fetchmail-server flush)
  "Flushオプションのリストを作る。"
  (if flush (list "--flush")))

(defun fetchmail-param-funcall (fetchmail-server func-name func-arg)
  "パラメータに対応する変換関数を呼び出す。"
  (funcall (cdr (assq func-name fetchmail-param-func-alist))
	   fetchmail-server func-arg))

(defun fetchmail-make-option-list (fetchmail-server fetchmail-param-alist)
  "パラメータからfetchmailのオプションのリストを作る。"
  (apply (function append)
	 (mapcar
	  (lambda (param-pair)
	    (fetchmail-param-funcall fetchmail-server
				     (car param-pair) (cdr param-pair)))
	  fetchmail-param-alist)))

(defun fetchmail-make-server-alist (fetchmail-server-param-alist)
  "Fetchmailサーバの連想リストを作る。"
  (let ((count 0))
    (mapcar
     (lambda (server-param-pair)
       (setq count (1+ count))
       (cons (car server-param-pair)
	     count))
     fetchmail-server-param-alist)))

(defun fetchmail-query-server ()
  "Fetchmailのサーバをミニバッファで選択する。"
  (let ((fetchmail-server
	 (completing-read "Fetchmail server: "
			  (fetchmail-make-server-alist
			   fetchmail-server-param-alist))))
    (if (and fetchmail-server
	     (> (length fetchmail-server) 0))
	fetchmail-server
      nil)))

(defun fetchmail-get-server-name (fetchmail-server-name-or-alias)
  "Fetchmailのサーバの別名を解決する。"
  (or (cdr (assoc fetchmail-server-name-or-alias fetchmail-server-alias-alist))
      fetchmail-server-name-or-alias))

(defun fetchmail-buffer-p ()
  "Fetchmailバッファが開いているかどうかを確認する。"
  (if (get-buffer fetchmail-buffer-name)
      t
    nil))

(defun fetchmail-make-buffer ()
  "Fetchmailバッファを作る。"
  (let ((default-major-mode 'fetchmail-mode))
    (set-buffer-major-mode
     (get-buffer-create fetchmail-buffer-name))))

(defun fetchmail-insert-buffer (msg)
  "Fetchmailバッファの最後にメッセージを挿入する。"
  (save-excursion
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail-window-p (&optional all-frames)
  "Fetchmailウィンドウが開いているかどうかを確認する。"
  (if (get-buffer-window fetchmail-buffer-name all-frames)
      t
    nil))

(defun fetchmail-open-window ()
  "Fetchmailバッファをウィンドウで開く。"
  (unless (equal fetchmail-buffer-name (buffer-name))
    (set-window-buffer
     (split-window (selected-window) 
		   (- (window-height)
		      (max fetchmail-window-height-lower-limit
			   (min fetchmail-window-height-upper-limit
				(round
				 (* (window-height)
				    fetchmail-window-height-ratio))))
		      1))
     fetchmail-buffer-name)))

(defun fetchmail-close-window ()
  "Fetchmailバッファのウィンドウを閉じる。"
  (interactive)
  (if (fetchmail-window-p t)
      (progn
	(delete-windows-on (get-buffer fetchmail-buffer-name))
	(bury-buffer fetchmail-buffer-name))))

(defun fetchmail-run (fetchmail-server fetchmail-option-list)
  "Fetchmailを起動してそのプロセスを返す。"
  (fetchmail-insert-buffer
   (concat "<<< fetchmail"
	   (if fetchmail-window-time-format
	       (format-time-string fetchmail-window-time-format
				   (current-time))
	     "")
	   " >>>\n"))
  (let ((process-connection-type t)
	(fetchmail-run-list (append (list "fetchmail")
				    fetchmail-option-list
				    (list (fetchmail-get-server-name fetchmail-server)))))
    (fetchmail-insert-buffer (concat (mapconcat
				      (lambda (param) param)
				      fetchmail-run-list " ") "\n"))
    (apply (function start-process)
	   fetchmail-process-name
	   fetchmail-buffer-name
	   fetchmail-run-list)))

(defun fetchmail-enter-passwd (fetchmail-process fetchmail-passwd)
  "Fetchmailプロセスにパスワードを入力する。"
  (catch 'passwd-entered
    (while t
      (save-excursion
	(set-buffer fetchmail-buffer-name)
	(goto-char (process-mark fetchmail-process))
	(beginning-of-line)
	(if (string-match "Enter password"
			  (buffer-substring (point)
					    (process-mark fetchmail-process)))
	    (throw 'passwd-entered nil)))
      (unless (process-status fetchmail-process)
	(error "Fetchmail exited in entering password."))
      (sleep-for 0.1)))
  (process-send-string (process-name fetchmail-process)
		       fetchmail-passwd)
  (process-send-eof (process-name fetchmail-process)))

(defun fetchmail-start (fetchmail-server fetchmail-option-list)
  "Fetchmailを一つのサーバに対して起動する。"
  (if (get-process fetchmail-process-name)
      (error "Fetchmail is already running."))
  (run-hooks 'fetchmail-preprocess-hook)
  (let ((fetchmail-process
	 (fetchmail-run fetchmail-server
			fetchmail-option-list)))
    (if fetchmail-query-passwd
	(fetchmail-enter-passwd fetchmail-process
				(base64-decode-string
				 (fetchmail-get-passwd fetchmail-server))))
    (setq fetchmail-running t)
    (force-mode-line-update)
    (setq fetchmail-last-server fetchmail-server)
    (set-process-sentinel fetchmail-process 'fetchmail-finish)))

(defun fetchmail-finish (fetchmail-process event)
  "Fetchmailプロセス終了時の後始末をする。"
  (let ((fetchmail-exit-status
	 (cond
	  ((string-match "finished" event)
	   'mail)
	  ((string-match "exited" event)
	   (if (= 1 (process-exit-status fetchmail-process))
	       'nomail
	     (fetchmail-clear-passwd fetchmail-last-server)
	     'failure)))))
    (if fetchmail-exit-status
	(progn
	  (setq fetchmail-running nil)
	  (force-mode-line-update)
	  (let ((fetchmail-message
		 (cond
		  ((eq 'mail fetchmail-exit-status)
		   "You have mail.")
		  ((eq 'nomail fetchmail-exit-status)
		   "You have no mail.")
		  ((eq 'failure fetchmail-exit-status)
		   "Failed to fetchmail.")
		  (t
		   (error "Invalid fetchmail-exit-status")))))
	    (run-hooks 'fetchmail-postprocess-hook)
	    (fetchmail-insert-buffer (concat fetchmail-message "\n"))
	    (unless (fetchmail-window-p)
	      (message fetchmail-message))
	    (if fetchmail-notify-beep (beep)))))))

(defun fetchmail (fetchmail-server)
  "Fetchmailを起動する。引数を与えるかfetchmail-default-serverが
設定されていないときは、ミニバッファで複数のサーバから選択する。"
  (interactive "P")
  (unless (stringp fetchmail-server)
    (setq fetchmail-server
	  (cond
	   (fetchmail-server
	    (fetchmail-query-server))
	   (fetchmail-default-server
	    fetchmail-default-server)
	   ((= 1 (length fetchmail-server-param-alist))
	    (car (car fetchmail-server-param-alist)))
	   (t
	    (fetchmail-query-server)))))
  (unless fetchmail-server
    (error "Not selected fetchmail server."))
  (let ((fetchmail-query-passwd t))
    (let ((fetchmail-option-list
	   (fetchmail-make-option-list fetchmail-server
				       (cdr (assoc fetchmail-server
						   fetchmail-server-param-alist)))))
      (if fetchmail-query-passwd
	  (fetchmail-query-passwd fetchmail-server))
      (unless (fetchmail-buffer-p)
	(fetchmail-make-buffer))
      (if (and fetchmail-window
	       (not (fetchmail-window-p t)))
	  (fetchmail-open-window))
      (fetchmail-start fetchmail-server fetchmail-option-list))))

(defun fetchmail-mode ()
  "Fetchmailバッファ用のモード。"
  (interactive)
  (setq major-mode 'fetchmail-mode)
  (setq mode-name "Fetchmail")
  (setq fetchmail-mode-map (make-keymap))
  (define-key fetchmail-mode-map "\C-cx" 'fetchmail)
  (define-key fetchmail-mode-map "\C-cq" 'fetchmail-close-window)
  (use-local-map fetchmail-mode-map))
