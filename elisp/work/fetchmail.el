;;; $Id$
;;; fetchmail.el --- Emacs ���� fetchmail ��ư���롣

(defvar fetchmail-default-server nil
  "�ǥե���ȤΥ����С�")

(defvar fetchmail-server-param-alist '(("localhost" . ()))
  "������̾��ʸ����򥭡��˻����ѥ�᡼����Ϣ�ۥꥹ�Ȥ��ͤ˻���Ϣ�ۥꥹ�ȡ�")

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
  "�ѥ�᡼���Υ���ܥ�򥭡��˻����ѥ�᡼�����ͤ���Ϥ���ؿ���
�ͤ˻���Ϣ�ۥꥹ�ȡ�fetchmail-server-param-alist �Υѥ�᡼���β��Ϥ�
�Ȥ��롣")

(defvar fetchmail-passwd-alist ()
  "������̾�򥭡��˻����ѥ���ɤ��ͤˤ��Ϣ�ۥꥹ�ȡ�")

(defvar fetchmail-notify-beep t
  "�����ѿ������ΤȤ� fetchmail ����λ�������Ȥ� beep �����Τ餻�롣")

(defvar fetchmail-window t
  "�����ѿ������ΤȤ� fetchmail �Хåե��򥦥���ɥ��ǳ�����")

(defvar fetchmail-window-height 10
  "fetchmail ������ɥ��ι⤵��")

(defvar fetchmail-last-server nil
  "�Ǥ�Ƕ�Ȥ�줿�����Ф�̾�������äƤ��롣
fetchmail-start ����ưŪ�����ꤹ��Τǡ��桼�������ꤷ�ƤϤ����ʤ���")

(defvar fetchmail-exit-func nil
  "fetchmail ��λ���˸ƤФ��ؿ�����Ͽ���롣
fetchmail-start ����ưŪ�����ꤹ��Τǡ��桼�������ꤷ�ƤϤ����ʤ���")

(defvar fetchmail-exit-status nil
  "fetchmail ��λ���ξ��֤����ꤵ��롣
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

(defun fetchmail-set-passwd
  (server passwd)
  "fetchmail-passwd-alist �˥ѥ���ɤ����ꤹ�롣"
  (let ((pair (assoc server fetchmail-passwd-alist)))
    (if pair
	(setcdr pair passwd)
      (setq pair (list nil))
      (setcar pair server)
      (setcdr pair passwd)
      (setq fetchmail-passwd-alist
	    (cons pair fetchmail-passwd-alist)))))

(defun fetchmail-get-passwd (server)
  "fetchmail-passwd-alist ����ѥ���ɤ����롣"
  (cdr (assoc server fetchmail-passwd-alist)))

(defun fetchmail-clear-passwd (server)
  "fetchmail-passwd-alist ����ѥ���ɤ������롣"
  (fetchmail-set-passwd server nil))

(defun fetchmail-get-server-param (server param)
  "fetchmail-server-param-alist ���饵���ФΥѥ�᡼�������롣"
  (cdr (assq param
	     (cdr (assoc server fetchmail-server-param-alist)))))

(defun fetchmail-param-check (server check)
  "check ���ץ����Υꥹ�Ȥ��롣"
  (if check
      (list "--check")))

(defun fetchmail-param-query-passwd (server query-passwd)
  "�����ФΥѥ���ɤ����ꤹ�롣"
  (if query-passwd
      (let (passwd)
	(setq passwd (fetchmail-get-passwd server))
	(if (not passwd)
	    (fetchmail-set-passwd
	     server
	     (read-passwd (format "Password for %s: " server)))))) nil)

(defun fetchmail-param-username (server username)
  "username ���ץ����Υꥹ�Ȥ��롣"
  (if username
      (list "--username" username)))

(defun fetchmail-param-protocol (server protocol)
  "protocol ���ץ����Υꥹ�Ȥ��롣"
  (if protocol
      (list "--protocol" protocol)))

(defun fetchmail-param-port (server port)
  "port ���ץ����Υꥹ�Ȥ��롣"
  (if port
      (list "--port" (number-to-string port))))

(defun fetchmail-param-timeout (server timeout)
  "timeout ���ץ����Υꥹ�Ȥ��롣"
  (if timeout
      (list "--timeout" (number-to-string timeout))))

(defun fetchmail-param-folder (server folder)
  "folder ���ץ����Υꥹ�Ȥ��롣"
  (if folder
      (list "--folder" folder)))

(defun fetchmail-param-keep (server keep)
  "keep ���ץ����Υꥹ�Ȥ��롣"
  (if keep
      (list "--keep")))

(defun fetchmail-param-flush (server flush)
  "flush ���ץ����Υꥹ�Ȥ��롣"
  (if flush
      (list "--flush")))

(defun fetchmail-param-funcall (server key value)
  "�ѥ�᡼�����б������Ѵ��ؿ���ƤӽФ���"
  (funcall
   (cdr (assq key fetchmail-param-func-alist))
   server value))

(defun fetchmail-make-option-list (server param-alist)
  "�ѥ�᡼������ fetchmail �Υ��ץ����Υꥹ�Ȥ��롣"
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
  "fetchmail ���ĤΥ����Ф��Ф��Ƶ�ư���롣
exit-func ��Ϳ����줿�Ȥ��ϡ�fetchmail ����λ�����Ȥ���
������̾�� fetchmail �ν�λ���֤��Ϥ���ƸƤФ�롣
fetchmail �ν�λ���֤� 'mail, 'nomail, 'failure �λ����ࡣ"
  ; ��ŵ�ư�Υ����å�
  (if (get-process fetchmail-process-name)
      (error "Fetchmail is running."))
  (let ((process-connection-type t) fetchmail-process)
    (setq fetchmail-process
	  ; fetchmail �ε�ư
	  (apply 'start-process
		 fetchmail-process-name
		 fetchmail-buffer-name
		 "fetchmail"
		 (append (fetchmail-make-option-list server param-alist)
			 (list server))))
    (if (fetchmail-get-server-param server 'query-passwd)
	; fetchmail �˥ѥ���ɤ����Ϥ��� 
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
       ; fetchmail ��λ���ν���
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
  "fetchmail �Υ����Ф�ߥ˥Хåե������򤹤롣"
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
  "fetchmail �Хåե��򥦥���ɥ��ǳ�����"
  (if (not (equal fetchmail-buffer-name (buffer-name)))
      (set-window-buffer
       (split-window (selected-window) 
		     (- (window-height) fetchmail-window-height 1))
       fetchmail-buffer-name)))

(defun fetchmail-insert-buffer (msg)
  "fetchmail �Хåե��κǸ�˥�å��������������롣"
  (save-excursion
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail (query-server)
  (interactive "P")
  "fetchmail ��ư���롣������Ϳ���뤫 fetchmail-default-server ��
���ꤵ��Ƥ��ʤ��Ȥ��ϡ��ߥ˥Хåե���ʣ���Υ����Ф������򤹤롣"
  (let (server)
    ; �����Ф�����
    (setq server
	  (if query-server
	      (fetchmail-query-server)
	    (if fetchmail-default-server
		fetchmail-default-server
	      (fetchmail-query-server))))
    (if (= 0 (length server))
	(error "Not selected fetchmail server."))
    ; fetchmail �Хåե��ν����
    (get-buffer-create fetchmail-buffer-name)
    (fetchmail-insert-buffer "<<< fetchmail >>>\n")
    (if (and fetchmail-window
	     (not (get-buffer-window fetchmail-buffer-name)))
	(fetchmail-open-window))
    ; fetchmail ��ư
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
