;;; fetchmail.el --- Emacs ���� fetchmail ��ư���롣
;;; $Id$
;;
;; <<< ��� >>>
;; �ڴ� �θ� <toki@freedom.ne.jp>
;;
;; <<< URL >>>
;; http://www.freedom.ne.jp/toki/elisp/fetchmail.el
;;
;; <<< ����ˡ >>>
;; �ޤ�fetchmail.el��load-path���̤ä��ǥ��쥯�ȥ���֤���.emacs��
;;   (autoload 'fetchmail "fetchmail" nil t)
;; �Ȥ��������ɤ��ɲä��������Ƽ���
;; fetchmail-server-param-alist�ѿ�������򤷤ޤ���
;; ���ΤȤ���դ��ʤ��Ȥ����ʤ��Τ�omit-passwd�ѥ�᡼��������ǡ�
;; ~/.fetchmailrc�ǥѥ���ɤ�����򤷤Ƥ���Ȥ���ɬ��t�����ꤷ�Ʋ�������
;; ~/.fetchmailrc �������omit-passwd�����꤬������äƤ����
;; fetchmail.el�������ư��ʤ��Τǡ�����Ĥ��Ƥ���������
;; ��Ϲ��ߤ˱�����
;;   fetchmail-default-server
;;   fetchmail-preprocess-hook
;;   fetchmail-postprocess-hook
;;   fetchmail-notify-beep
;;   fetchmail-window
;;   fetchmail-window-time-format
;;   fetchmail-window-height-ratio
;;   fetchmail-window-height-lower-limit
;;   fetchmail-window-height-upper-limit
;; �������ѿ����ͤ�Ŭ�������ꤷ�Ƥ���������
;; ���ݤʤ�ǥե�����ͤΤޤޤǤ⹽���ޤ���
;; ���M-x fetchmail��¹Ԥ����fetchmail����ư���ޤ���
;; omit-passwd��nil�����ꤷ�Ƥ��뤫���뤤�����ꤷ�Ƥ��ʤ���С�
;; �ǽ�˰��٤����ѥ���ɤ��䤤��碌�Ƶ�������
;; ����ܤμ¹Ԥ���ϵ��������ѥ���ɤ���Ѥ��ޤ���
;; ���ΤȤ��ѥ���ɤ�fetchmail-server-passwd-alist�ѿ��˵������졢
;; Emacs Lisp�˴��줿�ͤʤ��ñ�˼��Ф��Ƥ��ޤ��Τǡ�
;; ü��������Υ���Ȥ�����դ��Ƥ���������
;;

(defvar fetchmail-default-server nil
  "�ǥե���ȤΥ����С�")

(defvar fetchmail-server-param-alist '(("localhost" . ()))
  "�����ФΥѥ�᡼�������ꤹ��Ϣ�ۥꥹ�ȡ�
��: '((\"hepsun2.phys.sci.kobe-u.ac.jp\" .
       ((protocol . \"imap\")))
      (\"phys03.phys.sci.kobe-u.ac.jp\" .
       ((omit-passwd . t)
        (protocol . \"apop\"))))")

(defvar fetchmail-server-alias-alist ()
  "�����Ф���̾�����ꤹ��Ϣ�ۥꥹ�ȡ�
��: '((\"KOBEHEP\" . \"hepsun2.phys.sci.kobe-u.ac.jp\")
      (\"KOBEPHYS\" . \"phys03.phys.sci.kobe-u.ac.jp\"))")

(defvar fetchmail-preprocess-hook ()
  "fetchmail ������������Ͽ����եå���")

(defvar fetchmail-postprocess-hook ()
  "fetchmail �θ��������Ͽ����եå���")

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
  "�ѥ�᡼���Υ���ܥ�򥭡��˻����ѥ�᡼�����ͤ���Ϥ���ؿ��Υ���ܥ��
�ͤ˻���Ϣ�ۥꥹ�ȡ�fetchmail-server-param-alist�ؿ��Υѥ�᡼���β��Ϥ�
�Ȥ��롣")

(defvar fetchmail-server-passwd-alist ()
  "�����ФΥѥ���ɤ���¸����Ϣ�ۥꥹ�ȡ�")

(defvar fetchmail-notify-beep t
  "�����ѿ������ΤȤ�fetchmail����λ�������Ȥ�beep�����Τ餻�롣")

(defvar fetchmail-window t
  "�����ѿ������ΤȤ�fetchmail�Хåե��򥦥���ɥ��ǳ�����")

(defvar fetchmail-window-time-format " [%a %b %e %T %Y]"
  "Fetchmail��ư���������ɽ������񼰡�
�����ѿ���nil�����ꤹ��Ȼ����ɽ�����ʤ���")

(defvar fetchmail-window-height-ratio 0.15
  "Fetchmail������ɥ��ι⤵�γ�硣")

(defvar fetchmail-window-height-lower-limit 5
  "Fetchmail������ɥ��ι⤵�β��¡�")

(defvar fetchmail-window-height-upper-limit 10
  "Fetchmail������ɥ��ι⤵�ξ�¡�")

(defvar fetchmail-last-server nil
  "�Ǹ�˻Ȥ�줿�����Ф�̾�������äƤ��롣
fetchmail-start�ؿ�����ưŪ�����ꤹ��Τǡ��桼�������ꤷ�ƤϤ����ʤ���")

(defvar fetchmail-process-name "fetchmail"
  "Fetchmail�ץ�����̾����")

(defvar fetchmail-buffer-name "*fetchmail*"
  "Fetchmail�Хåե���̾����")

(defvar fetchmail-running nil
  "Fetchmail��ư����Ǥ��뤳�Ȥ�ɽ�魯�ޥ��ʡ��⡼���ѿ�")
(unless (assq 'fetchmail-running minor-mode-alist)
  (setq minor-mode-alist
	(cons '(fetchmail-running " Fetching mail...")
	      minor-mode-alist)))

(defun fetchmail-set-passwd (fetchmail-server fetchmail-passwd)
  "fetchmail-server-passwd-alist�˥ѥ���ɤ����ꤹ�롣"
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
  "fetchmail-server-passwd-alist����ѥ���ɤ����롣"
  (cdr (assoc fetchmail-server
	      fetchmail-server-passwd-alist)))

(defun fetchmail-clear-passwd (fetchmail-server)
  "fetchmail-server-passwd-alist����ѥ���ɤ������롣"
  (fetchmail-set-passwd fetchmail-server nil))

(defun fetchmail-get-server-param (fetchmail-server fetchmail-param)
  "fetchmail-server-param-alist���饵���ФΥѥ�᡼�������롣"
  (cdr (assq fetchmail-param
	     (cdr (assoc fetchmail-server
			 fetchmail-server-param-alist)))))

(defun fetchmail-query-passwd (fetchmail-server)
  "�����ФΥѥ���ɤ����ꤹ�롣"
  (unless (fetchmail-get-passwd fetchmail-server)
    (fetchmail-set-passwd fetchmail-server
			  (base64-encode-string
			   (read-passwd (format "Password for %s: "
						fetchmail-server)) nil))))

(defun fetchmail-param-omit-passwd (fetchmail-server omit-passwd)
  "�ѥ���ɤ����Ϥ��ά���롣"
  (setq fetchmail-query-passwd nil)
  ())

(defun fetchmail-param-check (fetchmail-server check)
  "Check���ץ����Υꥹ�Ȥ��롣"
  (if check (list "--check")))

(defun fetchmail-param-username (fetchmail-server username)
  "Username���ץ����Υꥹ�Ȥ��롣"
  (if username (list "--username" username)))

(defun fetchmail-param-protocol (fetchmail-server protocol)
  "Protocol���ץ����Υꥹ�Ȥ��롣"
  (if protocol (list "--protocol" protocol)))

(defun fetchmail-param-port (fetchmail-server port)
  "Port���ץ����Υꥹ�Ȥ��롣"
  (if port (list "--port" (number-to-string port))))

(defun fetchmail-param-timeout (fetchmail-server timeout)
  "Timeout���ץ����Υꥹ�Ȥ��롣"
  (if timeout (list "--timeout" (number-to-string timeout))))

(defun fetchmail-param-folder (fetchmail-server folder)
  "Folder���ץ����Υꥹ�Ȥ��롣"
  (if folder (list "--folder" folder)))

(defun fetchmail-param-keep (fetchmail-server keep)
  "Keep���ץ����Υꥹ�Ȥ��롣"
  (if keep (list "--keep")))

(defun fetchmail-param-flush (fetchmail-server flush)
  "Flush���ץ����Υꥹ�Ȥ��롣"
  (if flush (list "--flush")))

(defun fetchmail-param-funcall (fetchmail-server func-name func-arg)
  "�ѥ�᡼�����б������Ѵ��ؿ���ƤӽФ���"
  (funcall (cdr (assq func-name fetchmail-param-func-alist))
	   fetchmail-server func-arg))

(defun fetchmail-make-option-list (fetchmail-server fetchmail-param-alist)
  "�ѥ�᡼������fetchmail�Υ��ץ����Υꥹ�Ȥ��롣"
  (apply (function append)
	 (mapcar
	  (lambda (param-pair)
	    (fetchmail-param-funcall fetchmail-server
				     (car param-pair) (cdr param-pair)))
	  fetchmail-param-alist)))

(defun fetchmail-make-server-alist (fetchmail-server-param-alist)
  "Fetchmail�����Ф�Ϣ�ۥꥹ�Ȥ��롣"
  (let ((count 0))
    (mapcar
     (lambda (server-param-pair)
       (setq count (1+ count))
       (cons (car server-param-pair)
	     count))
     fetchmail-server-param-alist)))

(defun fetchmail-query-server ()
  "Fetchmail�Υ����Ф�ߥ˥Хåե������򤹤롣"
  (let ((fetchmail-server
	 (completing-read "Fetchmail server: "
			  (fetchmail-make-server-alist
			   fetchmail-server-param-alist))))
    (if (and fetchmail-server
	     (> (length fetchmail-server) 0))
	fetchmail-server
      nil)))

(defun fetchmail-get-server-name (fetchmail-server-name-or-alias)
  "Fetchmail�Υ����Ф���̾���褹�롣"
  (or (cdr (assoc fetchmail-server-name-or-alias fetchmail-server-alias-alist))
      fetchmail-server-name-or-alias))

(defun fetchmail-buffer-p ()
  "Fetchmail�Хåե��������Ƥ��뤫�ɤ������ǧ���롣"
  (if (get-buffer fetchmail-buffer-name)
      t
    nil))

(defun fetchmail-make-buffer ()
  "Fetchmail�Хåե����롣"
  (let ((default-major-mode 'fetchmail-mode))
    (set-buffer-major-mode
     (get-buffer-create fetchmail-buffer-name))))

(defun fetchmail-insert-buffer (msg)
  "Fetchmail�Хåե��κǸ�˥�å��������������롣"
  (save-excursion
    (set-buffer fetchmail-buffer-name)
    (goto-char (point-max))
    (insert-before-markers msg)))

(defun fetchmail-window-p (&optional all-frames)
  "Fetchmail������ɥ��������Ƥ��뤫�ɤ������ǧ���롣"
  (if (get-buffer-window fetchmail-buffer-name all-frames)
      t
    nil))

(defun fetchmail-open-window ()
  "Fetchmail�Хåե��򥦥���ɥ��ǳ�����"
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
  "Fetchmail�Хåե��Υ�����ɥ����Ĥ��롣"
  (interactive)
  (if (fetchmail-window-p t)
      (progn
	(delete-windows-on (get-buffer fetchmail-buffer-name))
	(bury-buffer fetchmail-buffer-name))))

(defun fetchmail-run (fetchmail-server fetchmail-option-list)
  "Fetchmail��ư���Ƥ��Υץ������֤���"
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
  "Fetchmail�ץ����˥ѥ���ɤ����Ϥ��롣"
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
  "Fetchmail���ĤΥ����Ф��Ф��Ƶ�ư���롣"
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
  "Fetchmail�ץ�����λ���θ�����򤹤롣"
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
  "Fetchmail��ư���롣������Ϳ���뤫fetchmail-default-server��
���ꤵ��Ƥ��ʤ��Ȥ��ϡ��ߥ˥Хåե���ʣ���Υ����Ф������򤹤롣"
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
  "Fetchmail�Хåե��ѤΥ⡼�ɡ�"
  (interactive)
  (setq major-mode 'fetchmail-mode)
  (setq mode-name "Fetchmail")
  (setq fetchmail-mode-map (make-keymap))
  (define-key fetchmail-mode-map "\C-cx" 'fetchmail)
  (define-key fetchmail-mode-map "\C-cq" 'fetchmail-close-window)
  (use-local-map fetchmail-mode-map))
