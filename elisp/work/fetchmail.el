;;; $Id$
;;; fetchmail.el --- Emacs から fetchmail を起動する。

(defvar fetchmail-default-server nil
  "デフォルトのサーバ。")

(defvar fetchmail-server-param-alist '(("localhost" . ()))
  "サーバ名の文字列をキーに持ちパラメータの連想リストを値に持つ連想リスト。")

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
  "パラメータのシンボルをキーに持ちパラメータの値を解析する関数を
値に持つ連想リスト。fetchmail-server-param-alist のパラメータの解析に
使われる。")

(defvar fetchmail-passwd-alist ()
  "サーバ名をキーに持ちパスワードを値にもつ連想リスト。")

(defvar fetchmail-notify-beep t
  "この変数が真のとき fetchmail が終了したことを beep 音で知らせる。")

(defvar fetchmail-window t
  "この変数が真のとき fetchmail バッファをウィンドウで開く。")

(defvar fetchmail-window-height 10
  "fetchmail ウィンドウの高さ。")

(defvar fetchmail-last-server nil
  "最も最近使われたサーバの名前が入っている。
fetchmail-start が自動的に設定するので、ユーザが設定してはいけない。")

(defvar fetchmail-exit-func nil
  "fetchmail 終了時に呼ばれる関数を登録する。
fetchmail-start が自動的に設定するので、ユーザが設定してはいけない。")

(defvar fetchmail-exit-status nil
  "fetchmail 終了時の状態が設定される。
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

(defun fetchmail-set-passwd
  (server passwd)
  "fetchmail-passwd-alist にパスワードを設定する。"
  (let ((pair (assoc server fetchmail-passwd-alist)))
    (if pair
	(setcdr pair passwd)
      (setq pair (list nil))
      (setcar pair server)
      (setcdr pair passwd)
      (setq fetchmail-passwd-alist
	    (cons pair fetchmail-passwd-alist)))))

(defun fetchmail-get-passwd (server)
  "fetchmail-passwd-alist からパスワードを得る。"
  (cdr (assoc server fetchmail-passwd-alist)))

(defun fetchmail-clear-passwd (server)
  "fetchmail-passwd-alist からパスワードを削除する。"
  (fetchmail-set-passwd server nil))

(defun fetchmail-get-server-param (server param)
  "fetchmail-server-param-alist からサーバのパラメータを得る。"
  (cdr (assq param
	     (cdr (assoc server fetchmail-server-param-alist)))))

(defun fetchmail-param-check (server check)
  "check オプションのリストを作る。"
  (if check
      (list "--check")))

(defun fetchmail-param-query-passwd (server query-passwd)
  "サーバのパスワードを設定する。"
  (if query-passwd
      (let (passwd)
	(setq passwd (fetchmail-get-passwd server))
	(if (not passwd)
	    (fetchmail-set-passwd
	     server
	     (read-passwd (format "Password for %s: " server)))))) nil)

(defun fetchmail-param-username (server username)
  "username オプションのリストを作る。"
  (if username
      (list "--username" username)))

(defun fetchmail-param-protocol (server protocol)
  "protocol オプションのリストを作る。"
  (if protocol
      (list "--protocol" protocol)))

(defun fetchmail-param-port (server port)
  "port オプションのリストを作る。"
  (if port
      (list "--port" (number-to-string port))))

(defun fetchmail-param-timeout (server timeout)
  "timeout オプションのリストを作る。"
  (if timeout
      (list "--timeout" (number-to-string timeout))))

(defun fetchmail-param-folder (server folder)
  "folder オプションのリストを作る。"
  (if folder
      (list "--folder" folder)))

(defun fetchmail-param-keep (server keep)
  "keep オプションのリストを作る。"
  (if keep
      (list "--keep")))

(defun fetchmail-param-flush (server flush)
  "flush オプションのリストを作る。"
  (if flush
      (list "--flush")))

(defun fetchmail-param-funcall (server key value)
  "パラメータに対応する変換関数を呼び出す。"
  (funcall
   (cdr (assq key fetchmail-param-func-alist))
   server value))

(defun fetchmail-make-option-list (server param-alist)
  "パラメータから fetchmail のオプションのリストを作る。"
  (let ((option-list ()))
    (while param-alist
      (let ((key (car (car param-alist)))
	    (value (cdr (car param-alist))))
	(setq option-list
	      (append option-list
		      (fetchmail-param-funcall server key value))))
      (setq param-alist (cdr param-alist)))
    option-list))

(defun fetchmail-start (server param-alist &optional exit-func)
  "fetchmail を一つのサーバに対して起動する。
exit-func が与えられたときは、fetchmail が終了したときに
サーバ名と fetchmail の終了状態が渡されて呼ばれる。
fetchmail の終了状態は 'mail, 'nomail, 'failure の三種類。"
  ; 二重起動のチェック
  (if (get-process fetchmail-process-name)
      (error "Fetchmail is running."))
  (let ((process-connection-type t) fetchmail-process)
    (setq fetchmail-process
	  ; fetchmail の起動
	  (apply 'start-process
		 fetchmail-process-name
		 fetchmail-buffer-name
		 "fetchmail"
		 (append (fetchmail-make-option-list server param-alist)
			 (list server))))
    (if (fetchmail-get-server-param server 'query-passwd)
	; fetchmail にパスワードを入力する 
	(progn
	  (catch 'query-passwd
	    (while t
	      (save-excursion
		(set-buffer fetchmail-buffer-name)
		(goto-char (process-mark fetchmail-process))
		(beginning-of-line)
		(if (string-match
		     "Enter password"
		     (buffer-substring (point)
				       (process-mark fetchmail-process)))
		    (throw 'query-passwd nil)))
	      (if (eq 'exit
		      (process-status (process-name fetchmail-process)))
		  (error "Fetchmail abort."))
	      (sleep-for 0.1)))
	  (process-send-string (process-name fetchmail-process)
			       (fetchmail-get-passwd server))
	  (process-send-eof (process-name fetchmail-process))
	  (save-excursion
	    (set-buffer fetchmail-buffer-name)
	    (goto-char (process-mark fetchmail-process)))))
    (setq fetchmail-running t)
    (force-mode-line-update)
    (setq fetchmail-last-server server)
    (setq fetchmail-exit-func exit-func)
    (set-process-sentinel
     fetchmail-process
     (lambda (process event)
       ; fetchmail 終了時の処理
       (if (setq fetchmail-exit-status
		 (cond
		  ((string-match "finished" event) 'mail)
		  ((string-match "exited" event)
		   (if (= 1 (process-exit-status process))
		       'nomail
		     (fetchmail-clear-passwd fetchmail-last-server)
		     'failure))))
	   (progn
	     (setq fetchmail-running nil)
	     (force-mode-line-update)
	     (if fetchmail-exit-func
		 (funcall fetchmail-exit-func
			  fetchmail-last-server fetchmail-exit-status))))))))

(defun fetchmail-query-server ()
  "fetchmail のサーバをミニバッファで選択する。"
  (completing-read
   "Fetchmail server: "
   (let ((server-list fetchmail-server-param-alist)
	 (collection ())
	 (i 0))
     (while server-list
       (setq i (+ i 1))
       (setq collection
	     (append collection
		     (list (list (car (car server-list)) i))))
       (setq server-list (cdr server-list)))
     collection)
   nil t))

(defun fetchmail-open-window ()
  "fetchmail バッファをウィンドウで開く。"
  (if (not (equal fetchmail-buffer-name (buffer-name)))
      (set-window-buffer
       (split-window (selected-window) 
		     (- (window-height) fetchmail-window-height 1))
       fetchmail-buffer-name)))

(defun fetchmail-insert-buffer (msg)
  "fetchmail バッファの最後にメッセージを挿入する。"
  (save-excursion
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail (query-server)
  (interactive "P")
  "fetchmail を起動する。引数を与えるか fetchmail-default-server が
設定されていないときは、ミニバッファで複数のサーバから選択する。"
  (let (server)
    ; サーバの選択
    (setq server
	  (if query-server
	      (fetchmail-query-server)
	    (if fetchmail-default-server
		fetchmail-default-server
	      (fetchmail-query-server))))
    (if (= 0 (length server))
	(error "Not selected fetchmail server."))
    ; fetchmail バッファの初期化
    (get-buffer-create fetchmail-buffer-name)
    (fetchmail-insert-buffer "<<< fetchmail >>>\n")
    (if (and fetchmail-window
	     (not (get-buffer-window fetchmail-buffer-name)))
	(fetchmail-open-window))
    ; fetchmail を起動
    (let ((server-param-alist
	   (assoc server fetchmail-server-param-alist)))
      (fetchmail-start
       (car server-param-alist) (cdr server-param-alist)
       (lambda (server exit-status)
	 (cond
	  ((eq 'mail exit-status)
	   (fetchmail-insert-buffer "You have mail.\n")
	   (if (not (get-buffer-window fetchmail-buffer-name))
	       (message "You have mail.")))
	  ((eq 'nomail exit-status)
	   (fetchmail-insert-buffer "You have no mail.\n")
	   (if (not (get-buffer-window fetchmail-buffer-name))
	       (message "You have no mail.")))
	  ((eq 'failure)
	   (fetchmail-insert-buffer "Failed to fetchmail.\n")
	   (error "Failed to fetchmail.")))
	 (if fetchmail-notify-beep (beep)))))))
