;;; $Id$
;;; fetchmail.el --- Emacs から fetchmail を起動する。

(defvar fetchmail-default-server nil
  "デフォルトのサーバ。")

(defvar fetchmail-server-param-alist '(("localhost" . ()))
  "サーバのパラメータを設定する連想リスト。
例: '((\"mail.freedom.ne.jp\" .
       ((query-passwd . t)
        (protocol . \"pop3\")))
      (\"hepsun2.phys.sci.kobe-u.ac.jp\" .
       ((query-passwd . t)
        (protocol . \"imap\")))
      (\"phys03.phys.sci.kobe-u.ac.jp\" .
       ((query-passwd . t)
        (protocol . \"apop\"))))")

(defvar fetchmail-preprocess-hook ()
  "fetchmail の前処理を登録するフック。")

(defvar fetchmail-postprocess-hook ()
  "fetchmail の後処理を登録するフック。")

(defvar fetchmail-param-func-alist
  '((query-passwd . fetchmail-param-query-passwd)
    (check        . fetchmail-param-check)
    (username     . fetchmail-param-username)
    (protocol     . fetchmail-param-protocol)
    (port         . fetchmail-param-port)
    (timeout      . fetchmail-param-timeout)
    (folder       . fetchmail-param-folder)
    (keep         . fetchmail-param-keep)
    (flush        . fetchmail-param-flush))
  "パラメータのシンボルをキーに持ちパラメータの値を解析する関数のシンボルを
値に持つ連想リスト。fetchmail-server-param-alist のパラメータの解析に
使われる。")

(defvar fetchmail-passwd-alist ()
  "サーバのパスワードを保存する連想リスト。")

(defvar fetchmail-notify-beep t
  "この変数が真のとき fetchmail が終了したことを beep 音で知らせる。")

(defvar fetchmail-window t
  "この変数が真のとき fetchmail バッファをウィンドウで開く。")

(defvar fetchmail-window-height-ratio 0.15
  "fetchmail ウィンドウの高さの割合。")

(defvar fetchmail-window-height-limit 10
  "fetchmail ウィンドウの高さの最大値。")

(defvar fetchmail-last-server nil
  "最後に使われたサーバの名前が入っている。
fetchmail-start が自動的に設定するので、ユーザが設定してはいけない。")

(defvar fetchmail-exit-func nil
  "fetchmail 終了時に呼ばれる関数を登録する。
fetchmail-start が自動的に設定するので、ユーザが設定してはいけない。")

(defvar fetchmail-exit-status nil
  "fetchmail 終了時の状態が設定される。
状態はシンボル値で mail, nomail, failure の三種類ある。
fetchmail-start が自動的に設定するので、ユーザが設定してはいけない。")

(defvar fetchmail-process-name "fetchmail"
  "fetchmail プロセスの名前。")

(defvar fetchmail-buffer-name "*fetchmail*"
  "fetchmail バッファの名前。")

(defvar fetchmail-running nil
  "fetchmail が動作中であることを表わすマイナーモード変数")
(if (not (assq 'fetchmail-running minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(fetchmail-running " Fetching mail...")
		minor-mode-alist)))

(defun fetchmail-set-passwd (fetchmail-server passwd)
  "fetchmail-passwd-alist にパスワードを設定する。"
  (let ((pair (assoc fetchmail-server
		     fetchmail-passwd-alist)))
    (if pair
	(setcdr pair passwd)
      (setq pair (list nil))
      (setcar pair fetchmail-server)
      (setcdr pair passwd)
      (setq fetchmail-passwd-alist
	    (cons pair fetchmail-passwd-alist)))))

(defun fetchmail-get-passwd (fetchmail-server)
  "fetchmail-passwd-alist からパスワードを得る。"
  (cdr (assoc fetchmail-server fetchmail-passwd-alist)))

(defun fetchmail-clear-passwd (fetchmail-server)
  "fetchmail-passwd-alist からパスワードを削除する。"
  (fetchmail-set-passwd fetchmail-server nil))

(defun fetchmail-get-server-param (fetchmail-server fetchmail-param)
  "fetchmail-server-param-alist からサーバのパラメータを得る。"
  (cdr (assq fetchmail-param
	     (cdr (assoc fetchmail-server
			 fetchmail-server-param-alist)))))

(defun fetchmail-param-query-passwd (fetchmail-server query-passwd)
  "サーバのパスワードを設定する。"
  (if query-passwd
      (let ((passwd (fetchmail-get-passwd fetchmail-server)))
	(if (not passwd)
	    (fetchmail-set-passwd
	     fetchmail-server
	     (read-passwd (format "Password for %s: "
				  fetchmail-server))))))
  nil)

(defun fetchmail-param-check (fetchmail-server check)
  "check オプションのリストを作る。"
  (if check (list "-c")))

(defun fetchmail-param-username (fetchmail-server username)
  "username オプションのリストを作る。"
  (if username (list "-u" username)))

(defun fetchmail-param-protocol (fetchmail-server protocol)
  "protocol オプションのリストを作る。"
  (if protocol (list "-p" protocol)))

(defun fetchmail-param-port (fetchmail-server port)
  "port オプションのリストを作る。"
  (if port (list "-P" (number-to-string port))))

(defun fetchmail-param-timeout (fetchmail-server timeout)
  "timeout オプションのリストを作る。"
  (if timeout (list "-t" (number-to-string timeout))))

(defun fetchmail-param-folder (fetchmail-server folder)
  "folder オプションのリストを作る。"
  (if folder (list "-r" folder)))

(defun fetchmail-param-keep (fetchmail-server keep)
  "keep オプションのリストを作る。"
  (if keep (list "-k")))

(defun fetchmail-param-flush (fetchmail-server flush)
  "flush オプションのリストを作る。"
  (if flush (list "-F")))

(defun fetchmail-param-funcall (fetchmail-server key value)
  "パラメータに対応する変換関数を呼び出す。"
  (funcall (cdr (assq key fetchmail-param-func-alist))
	   fetchmail-server value))

(defun fetchmail-make-option-list (fetchmail-server fetchmail-param-alist)
  "パラメータから fetchmail のオプションのリストを作る。"
  (if fetchmail-param-alist
      (let ((option-list
	     (fetchmail-param-funcall fetchmail-server
				      (car (car fetchmail-param-alist))
				      (cdr (car fetchmail-param-alist)))))
	(if (cdr fetchmail-param-alist)
	    (append option-list
		    (fetchmail-make-option-list fetchmail-server
						(cdr fetchmail-param-alist)))
	  option-list))))

(defun fetchmail-make-server-alist (server-param-alist num)
  "fetchmail サーバの連想リストを作る。"
  (if server-param-alist
      (let ((server-node
	     (list (car (car server-param-alist)) num)))
	(if (cdr server-param-alist)
	    (cons server-node
		  (fetchmail-make-server-alist
		   (cdr server-param-alist) (+ 1 num)))
	  (list server-node)))))

(defun fetchmail-query-server ()
  "fetchmail のサーバをミニバッファで選択する。"
  (completing-read
   "Fetchmail server: "
   (fetchmail-make-server-alist fetchmail-server-param-alist 1)
   nil t))

(defun fetchmail-make-buffer ()
  "fetchmail バッファを作る。"
  (let ((default-major-mode 'fetchmail-mode))
    (set-buffer-major-mode
     (get-buffer-create fetchmail-buffer-name))))

(defun fetchmail-open-window ()
  "fetchmail バッファをウィンドウで開く。"
  (if (not (equal fetchmail-buffer-name (buffer-name)))
      (set-window-buffer
       (split-window (selected-window) 
		     (- (window-height)
			(min fetchmail-window-height-limit
			     (round
			      (* (window-height)
				 fetchmail-window-height-ratio)))
			1))
       fetchmail-buffer-name)))

(defun fetchmail-close-window ()
  "fetchmail バッファのウィンドウを閉じる。"
  (interactive)
  (if (get-buffer-window fetchmail-buffer-name)
      (progn
	(delete-windows-on (get-buffer fetchmail-buffer-name))
	(bury-buffer fetchmail-buffer-name))))

(defun fetchmail-insert-buffer (msg)
  "fetchmail バッファの最後にメッセージを挿入する。"
  (save-excursion
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail-list-to-string (args)
  "リストを文字列に変換する。"
  (if (cdr args)
      (concat (car args) " "
	      (fetchmail-list-to-string (cdr args)))
    (car args)))

(defun fetchmail-run (fetchmail-server fetchmail-param-alist)
  "fetchmail を起動してそのプロセスを返す。"
  (fetchmail-insert-buffer "<<< fetchmail >>>\n")
  (let ((process-connection-type t)
	(fetchmail-run-list
	 (append (list "fetchmail")
		 (fetchmail-make-option-list fetchmail-server
					     fetchmail-param-alist)
		 (list fetchmail-server))))
    (fetchmail-insert-buffer
     (concat (fetchmail-list-to-string fetchmail-run-list) "\n"))
    (apply 'start-process
	   fetchmail-process-name
	   fetchmail-buffer-name
	   fetchmail-run-list)))

(defun fetchmail-enter-passwd (fetchmail-process)
  "fetchmail プロセスにパスワードを入力する。"
  (catch 'passwd-entered
    (while t
      (save-excursion
	(set-buffer fetchmail-buffer-name)
	(goto-char (process-mark fetchmail-process))
	(beginning-of-line)
	(if (string-match
	     "Enter password"
	     (buffer-substring (point)
			       (process-mark fetchmail-process)))
	    (throw 'passwd-entered nil)))
      (if (eq 'exit
	      (process-status (process-name fetchmail-process)))
	  (error "Fetchmail abort."))
      (sleep-for 0.1)))
  (process-send-string (process-name fetchmail-process)
		       (fetchmail-get-passwd fetchmail-server))
  (process-send-eof (process-name fetchmail-process)))

(defun fetchmail-finish (fetchmail-process event)
  "fetchmail プロセス終了時の後始末をする。"
  (if (setq fetchmail-exit-status
	    (cond
	     ((string-match "finished" event) 'mail)
	     ((string-match "exited" event)
	      (if (= 1 (process-exit-status fetchmail-process)) 'nomail
		(fetchmail-clear-passwd fetchmail-last-server) 'failure))))
      (progn
	(setq fetchmail-running nil)
	(force-mode-line-update)
	(run-hooks 'fetchmail-postprocess-hook)
	(if fetchmail-exit-func
	    (funcall fetchmail-exit-func
		     fetchmail-last-server fetchmail-exit-status)))))

(defun fetchmail-start
  (fetchmail-server
   fetchmail-param-alist &optional exit-func)
  "fetchmail を一つのサーバに対して起動する。
exit-func が与えられたときは、fetchmail が終了したときに
サーバ名と fetchmail の終了状態が渡されて呼ばれる。
fetchmail の終了状態は 'mail, 'nomail, 'failure の三種類。"
  (if (get-process fetchmail-process-name)
      (error "Fetchmail is running."))
  (run-hooks 'fetchmail-preprocess-hook)
  (let ((fetchmail-process (fetchmail-run fetchmail-server
					  fetchmail-param-alist)))
    (if (fetchmail-get-server-param fetchmail-server 'query-passwd)
	(fetchmail-enter-passwd fetchmail-process))
    (setq fetchmail-running t)
    (force-mode-line-update)
    (setq fetchmail-last-server fetchmail-server)
    (setq fetchmail-exit-func exit-func)
    (set-process-sentinel fetchmail-process 'fetchmail-finish)))

(defun fetchmail (query-server)
  "fetchmail を起動する。引数を与えるか fetchmail-default-server が
設定されていないときは、ミニバッファで複数のサーバから選択する。"
  (interactive "P")
  (let (fetchmail-server)
    (setq fetchmail-server
	  (cond
	   (query-server (fetchmail-query-server))
	   (fetchmail-default-server fetchmail-default-server)
	   ((= 1 (length fetchmail-server-param-alist))
	    (car (car fetchmail-server-param-alist)))
	   ((= 0 (length fetchmail-server-param-alist)) nil)
	   (t (fetchmail-query-server))))
    (if (or (not fetchmail-server) (= 0 (length fetchmail-server)))
	(error "Not selected fetchmail server."))
    (if (not (get-buffer fetchmail-buffer-name))
	(fetchmail-make-buffer))
    (if (and fetchmail-window
	     (not (get-buffer-window fetchmail-buffer-name)))
	(fetchmail-open-window))
    (let ((server-param-alist
	   (assoc fetchmail-server fetchmail-server-param-alist)))
      (fetchmail-start
       (car server-param-alist)
       (cdr server-param-alist)
       (lambda (fetchmail-server fetchmail-exit-status)
	 (let ((fetchmail-message
		(cond
		 ((eq 'mail fetchmail-exit-status)
		  "You have mail.")
		 ((eq 'nomail fetchmail-exit-status)
		  "You have no mail.")
		 ((eq 'failure fetchmail-exit-status)
		  "Failed to fetchmail."))))
	   (fetchmail-insert-buffer (concat fetchmail-message "\n"))
	   (if (not (get-buffer-window fetchmail-buffer-name))
	       (message fetchmail-message))
	   (if fetchmail-notify-beep (beep))))))))

(defun fetchmail-mode ()
  "fetchmail バッファ用のモード。"
  (interactive)
  (setq major-mode 'fetchmail-mode)
  (setq mode-name "Fetchmail")
  (setq fetchmail-mode-map (make-keymap))
  (define-key fetchmail-mode-map "\C-cx" 'fetchmail)
  (define-key fetchmail-mode-map "\C-cq" 'fetchmail-close-window)
  (use-local-map fetchmail-mode-map))
