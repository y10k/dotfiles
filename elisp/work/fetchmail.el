;;; $Id$
;;; fetchmail.el --- Emacs ���� fetchmail ��ư���롣

(defvar fetchmail-default-server nil
  "�ǥե���ȤΥ����С�")

(defvar fetchmail-server-param-alist '(("localhost" . ()))
  "�����ФΥѥ�᡼�������ꤹ��Ϣ�ۥꥹ�ȡ�
��: '((\"mail.freedom.ne.jp\" .
       ((query-passwd . t)
        (protocol . \"pop3\")))
      (\"hepsun2.phys.sci.kobe-u.ac.jp\" .
       ((query-passwd . t)
        (protocol . \"imap\")))
      (\"phys03.phys.sci.kobe-u.ac.jp\" .
       ((query-passwd . t)
        (protocol . \"apop\"))))")

(defvar fetchmail-preprocess-hook ()
  "fetchmail ������������Ͽ����եå���")

(defvar fetchmail-postprocess-hook ()
  "fetchmail �θ��������Ͽ����եå���")

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
  "�ѥ�᡼���Υ���ܥ�򥭡��˻����ѥ�᡼�����ͤ���Ϥ���ؿ��Υ���ܥ��
�ͤ˻���Ϣ�ۥꥹ�ȡ�fetchmail-server-param-alist �Υѥ�᡼���β��Ϥ�
�Ȥ��롣")

(defvar fetchmail-passwd-alist ()
  "�����ФΥѥ���ɤ���¸����Ϣ�ۥꥹ�ȡ�")

(defvar fetchmail-notify-beep t
  "�����ѿ������ΤȤ� fetchmail ����λ�������Ȥ� beep �����Τ餻�롣")

(defvar fetchmail-window t
  "�����ѿ������ΤȤ� fetchmail �Хåե��򥦥���ɥ��ǳ�����")

(defvar fetchmail-window-height-ratio 0.15
  "fetchmail ������ɥ��ι⤵�γ�硣")

(defvar fetchmail-window-height-limit 10
  "fetchmail ������ɥ��ι⤵�κ����͡�")

(defvar fetchmail-last-server nil
  "�Ǹ�˻Ȥ�줿�����Ф�̾�������äƤ��롣
fetchmail-start ����ưŪ�����ꤹ��Τǡ��桼�������ꤷ�ƤϤ����ʤ���")

(defvar fetchmail-exit-func nil
  "fetchmail ��λ���˸ƤФ��ؿ�����Ͽ���롣
fetchmail-start ����ưŪ�����ꤹ��Τǡ��桼�������ꤷ�ƤϤ����ʤ���")

(defvar fetchmail-exit-status nil
  "fetchmail ��λ���ξ��֤����ꤵ��롣
���֤ϥ���ܥ��ͤ� mail, nomail, failure �λ����ढ�롣
fetchmail-start ����ưŪ�����ꤹ��Τǡ��桼�������ꤷ�ƤϤ����ʤ���")

(defvar fetchmail-process-name "fetchmail"
  "fetchmail �ץ�����̾����")

(defvar fetchmail-buffer-name "*fetchmail*"
  "fetchmail �Хåե���̾����")

(defvar fetchmail-running nil
  "fetchmail ��ư����Ǥ��뤳�Ȥ�ɽ�魯�ޥ��ʡ��⡼���ѿ�")
(if (not (assq 'fetchmail-running minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(fetchmail-running " Fetching mail...")
		minor-mode-alist)))

(defun fetchmail-set-passwd (fetchmail-server passwd)
  "fetchmail-passwd-alist �˥ѥ���ɤ����ꤹ�롣"
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
  "fetchmail-passwd-alist ����ѥ���ɤ����롣"
  (cdr (assoc fetchmail-server fetchmail-passwd-alist)))

(defun fetchmail-clear-passwd (fetchmail-server)
  "fetchmail-passwd-alist ����ѥ���ɤ������롣"
  (fetchmail-set-passwd fetchmail-server nil))

(defun fetchmail-get-server-param (fetchmail-server fetchmail-param)
  "fetchmail-server-param-alist ���饵���ФΥѥ�᡼�������롣"
  (cdr (assq fetchmail-param
	     (cdr (assoc fetchmail-server
			 fetchmail-server-param-alist)))))

(defun fetchmail-param-query-passwd (fetchmail-server query-passwd)
  "�����ФΥѥ���ɤ����ꤹ�롣"
  (if query-passwd
      (let ((passwd (fetchmail-get-passwd fetchmail-server)))
	(if (not passwd)
	    (fetchmail-set-passwd
	     fetchmail-server
	     (read-passwd (format "Password for %s: "
				  fetchmail-server))))))
  nil)

(defun fetchmail-param-check (fetchmail-server check)
  "check ���ץ����Υꥹ�Ȥ��롣"
  (if check (list "-c")))

(defun fetchmail-param-username (fetchmail-server username)
  "username ���ץ����Υꥹ�Ȥ��롣"
  (if username (list "-u" username)))

(defun fetchmail-param-protocol (fetchmail-server protocol)
  "protocol ���ץ����Υꥹ�Ȥ��롣"
  (if protocol (list "-p" protocol)))

(defun fetchmail-param-port (fetchmail-server port)
  "port ���ץ����Υꥹ�Ȥ��롣"
  (if port (list "-P" (number-to-string port))))

(defun fetchmail-param-timeout (fetchmail-server timeout)
  "timeout ���ץ����Υꥹ�Ȥ��롣"
  (if timeout (list "-t" (number-to-string timeout))))

(defun fetchmail-param-folder (fetchmail-server folder)
  "folder ���ץ����Υꥹ�Ȥ��롣"
  (if folder (list "-r" folder)))

(defun fetchmail-param-keep (fetchmail-server keep)
  "keep ���ץ����Υꥹ�Ȥ��롣"
  (if keep (list "-k")))

(defun fetchmail-param-flush (fetchmail-server flush)
  "flush ���ץ����Υꥹ�Ȥ��롣"
  (if flush (list "-F")))

(defun fetchmail-param-funcall (fetchmail-server key value)
  "�ѥ�᡼�����б������Ѵ��ؿ���ƤӽФ���"
  (funcall (cdr (assq key fetchmail-param-func-alist))
	   fetchmail-server value))

(defun fetchmail-make-option-list (fetchmail-server fetchmail-param-alist)
  "�ѥ�᡼������ fetchmail �Υ��ץ����Υꥹ�Ȥ��롣"
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
  "fetchmail �����Ф�Ϣ�ۥꥹ�Ȥ��롣"
  (if server-param-alist
      (let ((server-node
	     (list (car (car server-param-alist)) num)))
	(if (cdr server-param-alist)
	    (cons server-node
		  (fetchmail-make-server-alist
		   (cdr server-param-alist) (+ 1 num)))
	  (list server-node)))))

(defun fetchmail-query-server ()
  "fetchmail �Υ����Ф�ߥ˥Хåե������򤹤롣"
  (completing-read
   "Fetchmail server: "
   (fetchmail-make-server-alist fetchmail-server-param-alist 1)
   nil t))

(defun fetchmail-make-buffer ()
  "fetchmail �Хåե����롣"
  (let ((default-major-mode 'fetchmail-mode))
    (set-buffer-major-mode
     (get-buffer-create fetchmail-buffer-name))))

(defun fetchmail-open-window ()
  "fetchmail �Хåե��򥦥���ɥ��ǳ�����"
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
  "fetchmail �Хåե��Υ�����ɥ����Ĥ��롣"
  (interactive)
  (if (get-buffer-window fetchmail-buffer-name)
      (progn
	(delete-windows-on (get-buffer fetchmail-buffer-name))
	(bury-buffer fetchmail-buffer-name))))

(defun fetchmail-insert-buffer (msg)
  "fetchmail �Хåե��κǸ�˥�å��������������롣"
  (save-excursion
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail-list-to-string (args)
  "�ꥹ�Ȥ�ʸ������Ѵ����롣"
  (if (cdr args)
      (concat (car args) " "
	      (fetchmail-list-to-string (cdr args)))
    (car args)))

(defun fetchmail-run (fetchmail-server fetchmail-param-alist)
  "fetchmail ��ư���Ƥ��Υץ������֤���"
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
  "fetchmail �ץ����˥ѥ���ɤ����Ϥ��롣"
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
  "fetchmail �ץ�����λ���θ�����򤹤롣"
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
  "fetchmail ���ĤΥ����Ф��Ф��Ƶ�ư���롣
exit-func ��Ϳ����줿�Ȥ��ϡ�fetchmail ����λ�����Ȥ���
������̾�� fetchmail �ν�λ���֤��Ϥ���ƸƤФ�롣
fetchmail �ν�λ���֤� 'mail, 'nomail, 'failure �λ����ࡣ"
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
  "fetchmail ��ư���롣������Ϳ���뤫 fetchmail-default-server ��
���ꤵ��Ƥ��ʤ��Ȥ��ϡ��ߥ˥Хåե���ʣ���Υ����Ф������򤹤롣"
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
  "fetchmail �Хåե��ѤΥ⡼�ɡ�"
  (interactive)
  (setq major-mode 'fetchmail-mode)
  (setq mode-name "Fetchmail")
  (setq fetchmail-mode-map (make-keymap))
  (define-key fetchmail-mode-map "\C-cx" 'fetchmail)
  (define-key fetchmail-mode-map "\C-cq" 'fetchmail-close-window)
  (use-local-map fetchmail-mode-map))
