;;; Fetchmail.el --- Emacs から fetchmail を起動する。
;;; $Id$
;;
;; <<< 作者 >>>
;; 土岐 仁謙 <toki@freedom.ne.jp>
;;
;; <<< URL >>>
;; http://www.freedom.ne.jp/toki/elisp/fetchmail.el
;;
;; <<< 注意 >>>
;; [2000-09-10]
;; カスタマイズの変数が一部変更されました。
;; 次の変数は廃止になったので注意してください。
;;   fetchmail-server-param-alist
;;   fetchmail-param-func-alist
;;
;; <<< 使用法 >>>
;; まずfetchmail.elをload-pathの通ったディレクトリに置いて.emacsに
;;   (autoload 'fetchmail "fetchmail" nil t)
;; というコードを追加します。
;; 次に変数fetchmail-server-omit-passwd-listにfetchmailに.fetchmailrcで
;; パスワードを設定してあるサーバ名を設定します。
;; 変数fetchmail-server-omit-passwd-listにサーバ名を追加すると
;; fetchmailを起動するときにパスワードを尋ねなくなります。
;; 逆に変数fetchmail-server-omit-passwd-listのサーバ名を削除して
;; .fetchmammilrcからパスワードのエントリを削除すると、
;; fetchmailを起動するとき最初の一回だけパスワードを尋ねて
;; それ以降はEmacsを終了するまでパスワードを記憶します。
;; 変数fetchmail-server-omit-passwd-listと.fetchmailrcの設定が
;; 矛盾しているとうまく動作しないので注意してください。
;; 後は好みに応じて
;;   fetchmail-default-server
;;   fetchmail-server-option-alist
;;   fetchmail-server-alias-alist
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
;; サーバが変数fetchmail-server-omit-passwd-listに登録されていなければ
;; 最初に一度だけパスワードを問い合わせて記憶し、二回目の実行からは
;; 記憶したパスワードを使用します。
;; このときパスワードはfetchmail-server-passwd-alist変数に記憶され、
;; Emacs Lispに慣れた人なら簡単に取り出せてしまうので
;; 端末の前を離れるときは注意してください。
;;

(defvar fetchmail-default-server nil
  "デフォルトのサーバ。")

(defvar fetchmail-server-omit-passwd-list ()
  "パスワードの入力を省略するサーバ名のリスト。
例: '(\"hepsun2.phys.sci.kobe-u.ac.jp\"
      \"phys03.phys.sci.kobe-u.ac.jp\")")

(defvar fetchmail-server-option-alist ()
  "サーバ毎のオプションを設定する連想リスト。
例: '((default \"--protocol\" \"--pop3\" \"--fetchall\") ; デフォルト
      (\"hepsun2.phys.sci.kobe-u.ac.jp\"
       \"--protocol\" \"apop\" \"--user\" \"foo\" \"--fetchall\")
      (\"phys03.phys.sci.kobe-u.ac.jp\"
       \"--protocol\" \"apop\" \"--user\" \"bar\" \"--keep\" \"--no-flush\" \"--uidl\"))")

(defvar fetchmail-server-alias-alist ()
  "サーバの別名を設定する連想リスト。
例: '((\"KOBEHEP\" . \"hepsun2.phys.sci.kobe-u.ac.jp\")
      (\"KOBEPHYS\" . \"phys03.phys.sci.kobe-u.ac.jp\"))")

(defvar fetchmail-server-passwd-alist ()
  "サーバのパスワードを保存する連想リスト。")

(defvar fetchmail-preprocess-hook ()
  "Fetchmailの前処理を登録するフック。")

(defvar fetchmail-postprocess-hook ()
  "Fetchmailの後処理を登録するフック。")

(defvar fetchmail-notify-beep t
  "この変数が真のときfetchmailが終了したことをbeep音で知らせる。")

(defvar fetchmail-window t
  "この変数が真のときfetchmailバッファをウィンドウで開く。")

(defvar fetchmail-window-time-format " [%a %b %e %T %Y]"
  "Fetchmailを起動した時刻を表示する書式。
この変数にnilを設定すると時刻を表示しない。")

(defvar fetchmail-window-height-ratio 0.15
  "Fetchmailウィンドウの高さの比率。")

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

(defvar fetchmail-mode-map nil
  "Fetchmailメジャーモードのキーマップ。")
(unless fetchmail-mode-map
  (setq fetchmail-mode-map (make-sparse-keymap))
  (define-key fetchmail-mode-map "\C-cx" 'fetchmail)
  (define-key fetchmail-mode-map "\C-cq" 'fetchmail-close-window))

(defvar fetchmail-mode-hook nil
  "Fetchmailメジャーモードのフック。")

(defun fetchmail-get-server-name (fetchmail-server-name-or-alias)
  "Fetchmailのサーバの別名を解決する。"
  (or (cdr (assoc fetchmail-server-name-or-alias fetchmail-server-alias-alist))
      fetchmail-server-name-or-alias))

(defun fetchmail-get-option-list (fetchmail-server)
  "fetchmailのオプションのリストを返す。"
  (or (cdr (assoc (fetchmail-get-server-name fetchmail-server)
		  fetchmail-server-option-alist))
      (cdr (assq 'default fetchmail-server-option-alist))))

(defun fetchmail-make-server-alist ()
  "Fetchmailサーバの連想リストを作る。"
  (let ((fetchmail-server-list ()))
    (if fetchmail-default-server
	(setq fetchmail-server-list
	      (cons fetchmail-default-server fetchmail-server-list)))
    (if fetchmail-server-omit-passwd-list
	(setq fetchmail-server-list
	      (append fetchmail-server-list
		      fetchmail-server-omit-passwd-list)))
    (let (fetchmail-server-alist
	  (fetchmail-server-alist-list (list fetchmail-server-option-alist
					     fetchmail-server-alias-alist)))
      (while fetchmail-server-alist-list
	(setq fetchmail-server-alist
	      (car fetchmail-server-alist-list))
	(while fetchmail-server-alist
	  (if (stringp (caar fetchmail-server-alist))
	      (let ((fetchmail-server-couple-list (list (caar fetchmail-server-alist)
							(fetchmail-get-server-name
							 (caar fetchmail-server-alist)))))
		(while fetchmail-server-couple-list
		  (if (not (member (car fetchmail-server-couple-list)
				   fetchmail-server-list))
		      (setq fetchmail-server-list
			    (cons (car fetchmail-server-couple-list)
				  fetchmail-server-list)))
		  (setq fetchmail-server-couple-list
			(cdr fetchmail-server-couple-list)))))
	  (setq fetchmail-server-alist
		(cdr fetchmail-server-alist)))
	(setq fetchmail-server-alist-list
	      (cdr fetchmail-server-alist-list))))
    (let ((count 0))
      (mapcar
       (lambda (fetchmail-server)
	 (cons fetchmail-server
	       (setq count (1+ count))))
       fetchmail-server-list))))

(defun fetchmail-query-server ()
  "Fetchmailのサーバをミニバッファで選択する。"
  (let ((fetchmail-server
	 (completing-read "Fetchmail server: "
			  (fetchmail-make-server-alist)
			  nil nil nil nil fetchmail-default-server)))
    (if (and fetchmail-server
	     (> (length fetchmail-server) 0))
	fetchmail-server
      nil)))

(defun fetchmail-set-passwd (fetchmail-server fetchmail-passwd)
  "fetchmail-server-passwd-alistにパスワードを設定する。"
  (let ((fetchmail-server-passwd-pair
	 (assoc (fetchmail-get-server-name fetchmail-server)
		fetchmail-server-passwd-alist)))
    (let ((fetchmail-old-passwd (cdr fetchmail-server-passwd-pair)))
      (if fetchmail-server-passwd-pair
	  (setcdr fetchmail-server-passwd-pair
		  fetchmail-passwd)
	(setq fetchmail-server-passwd-alist
	      (cons (cons (fetchmail-get-server-name fetchmail-server)
			  fetchmail-passwd)
		    fetchmail-server-passwd-alist)))
      fetchmail-old-passwd)))

(defun fetchmail-get-passwd (fetchmail-server)
  "fetchmail-server-passwd-alistからパスワードを取り出す。"
  (cdr (assoc (fetchmail-get-server-name fetchmail-server)
	      fetchmail-server-passwd-alist)))

(defun fetchmail-clear-passwd (fetchmail-server)
  "fetchmail-server-passwd-alistからパスワードを削除する。"
  (fetchmail-set-passwd (fetchmail-get-server-name fetchmail-server) nil))

(defun fetchmail-query-passwd-p (fetchmail-server)
  (not (member (fetchmail-get-server-name fetchmail-server)
	       fetchmail-server-omit-passwd-list)))

(defun fetchmail-query-passwd (fetchmail-server)
  "サーバのパスワードを設定する。"
  (unless (fetchmail-get-passwd fetchmail-server)
    (fetchmail-set-passwd (fetchmail-get-server-name fetchmail-server)
			  (base64-encode-string
			   (read-passwd (format "Password for %s: "
						fetchmail-server)) nil))))

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
  (save-selected-window
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail-window-p (&optional all-frames)
  "Fetchmailウィンドウが開いているかどうかを確認する。"
  (if (get-buffer-window fetchmail-buffer-name all-frames)
      t
    nil))

(defun fetchmail-open-window ()
  "Fetchmailバッファのウィンドウを開く。"
  (interactive)
  (unless (fetchmail-window-p t)
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

(defun fetchmail-start-process (fetchmail-server)
  "Fetchmailを起動してそのプロセスを返す。"
  (fetchmail-insert-buffer
   (concat "<<< fetchmail"
	   (if fetchmail-window-time-format
	       (format-time-string fetchmail-window-time-format
				   (current-time)))
	   " >>>\n"))
  (let ((process-connection-type t)
	(fetchmail-process-list (append (list "fetchmail")
					(fetchmail-get-option-list fetchmail-server)
					(list (fetchmail-get-server-name fetchmail-server)))))
    (fetchmail-insert-buffer (concat (mapconcat
				      (lambda (param) param)
				      fetchmail-process-list " ") "\n"))
    (apply (function start-process)
	   fetchmail-process-name
	   fetchmail-buffer-name
	   fetchmail-process-list)))

(defun fetchmail-enter-passwd (fetchmail-process fetchmail-passwd)
  "Fetchmailプロセスにパスワードを入力する。"
  (catch 'passwd-entered
    (while t
      (sleep-for 0.1)
      (save-selected-window
	(set-buffer fetchmail-buffer-name)
	(goto-char (process-mark fetchmail-process))
	(beginning-of-line)
	(if (string-match "Enter password"
			  (buffer-substring (point)
					    (process-mark fetchmail-process)))
	    (throw 'passwd-entered nil)))
      (if (or (not (process-status fetchmail-process))
	      (not (eq (process-status fetchmail-process) 'run)))
	(error "Fetchmail aborted in entering password."))))
  (process-send-string fetchmail-process fetchmail-passwd)
  (process-send-eof fetchmail-process))

(defun fetchmail-start (fetchmail-server)
  "Fetchmailを一つのサーバに対して起動する。"
  (if (get-process fetchmail-process-name)
      (error "Fetchmail is already running."))
  (run-hooks 'fetchmail-preprocess-hook)
  (let ((fetchmail-process (fetchmail-start-process fetchmail-server)))
    (setq fetchmail-running t)
    (force-mode-line-update)
    (setq fetchmail-last-server fetchmail-server)
    (set-process-sentinel fetchmail-process
			  (function fetchmail-finish))
    (if (fetchmail-query-passwd-p fetchmail-server)
	(fetchmail-enter-passwd fetchmail-process
				(base64-decode-string
				 (fetchmail-get-passwd fetchmail-server))))))

(defun fetchmail-finish (fetchmail-process event)
  "Fetchmailプロセス終了時の後始末をする。"
  (let ((fetchmail-exit-status
	 (cond
	  ((string-match "finished" event) 'mail)
	  ((string-match "exited" event)
	   (if (= 1 (process-exit-status fetchmail-process))
	       'nomail
	     'failure))
	  (t 'failure))))
    (if (eq fetchmail-exit-status 'failure)
	(fetchmail-clear-passwd fetchmail-last-server))
    (setq fetchmail-running nil)
    (force-mode-line-update)
    (let ((fetchmail-message
	   (cond
	    ((eq 'mail fetchmail-exit-status)
	     "You have mail.")
	    ((eq 'nomail fetchmail-exit-status)
	     "You have no mail.")
	    ((eq 'failure fetchmail-exit-status)
	     "Failed on fetchmail.")
	    (t
	     (error "Invalid fetchmail-exit-status")))))
      (if (eq 'mail fetchmail-exit-status)
	  (run-hooks 'fetchmail-postprocess-hook))
      (fetchmail-insert-buffer (concat fetchmail-message "\n"))
      (unless (fetchmail-window-p)
	(message fetchmail-message))
      (if fetchmail-notify-beep (beep)))))

(defun fetchmail (fetchmail-server)
  "Fetchmailを起動する。引数を与えるかfetchmail-default-serverが
設定されていないときは、ミニバッファで複数のサーバから選択する。"
  (interactive "P")
  (if (and (boundp 'fetchmail-server-param-alist)
	   fetchmail-server-param-alist)
      (error "obsolete variable: fetchmail-server-param-alist"))
  (if (and (boundp 'fetchmail-param-func-alist)
	   fetchmail-param-func-alist)
      (error "obsolete variable: fetchmail-param-func-alist"))
  (unless (stringp fetchmail-server)
    (setq fetchmail-server
	  (cond
	   (fetchmail-server (fetchmail-query-server))
	   (fetchmail-default-server fetchmail-default-server)
	   ((= 1 (length fetchmail-server-param-alist))
	    (car (car fetchmail-server-param-alist)))
	   (t (fetchmail-query-server)))))
  (unless fetchmail-server
    (error "Not selected fetchmail server."))
  (if (fetchmail-query-passwd-p fetchmail-server)
    (fetchmail-query-passwd fetchmail-server))
  (unless (fetchmail-buffer-p)
    (fetchmail-make-buffer))
  (if fetchmail-window
      (fetchmail-open-window))
  (fetchmail-start fetchmail-server))

(defun fetchmail-mode ()
  "Fetchmailバッファ用のメジャーモード。"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fetchmail-mode)
  (setq mode-name "Fetchmail")
  (use-local-map fetchmail-mode-map)
  (run-hooks 'fetchmail-mode-hook))

(provide 'fetchmail)
