;;; LaTeX label insertion
;;; $Id$
;;
;; <<< Author >>>
;; Yoshinori Toki <toki@freedom.ne.jp>
;;
;; <<< URL >>>
;; http://www.freedom.ne.jp/toki/elisp/latex-label.el
;;
;; <<< Installation >>>
;; (autoload 'latex-label-insert "latex-label"
;;   "Insertion of a LaTeX label." t)
;; (add-hook
;;  'latex-mode-hook
;;  (function
;;   (lambda ()
;;     (define-key tex-mode-map "\C-cl" 'latex-label-insert))))
;;

(defvar latex-label-history ()
  "LaTeX label completion minibuffer history.")

(defun latex-label-search ()
  "Search a next LaTeX label in current buffer."
  (let ((begin-of-label (search-forward "\\label{" nil t)))
    (if begin-of-label
	(let ((latex-label-source
	       (buffer-substring-no-properties begin-of-label
					       (progn
						 (end-of-line) (point)))))
	  (let ((point-of-tail
		 (string-match "}" latex-label-source)))
	    (if point-of-tail
		(substring latex-label-source
			   0 point-of-tail)))))))

(defun latex-label-make-alist ()
  "Make a associated list of LaTeX labels in current buffer."
  (let ((count 0)
	(latex-label nil)
	(latex-label-list ()))
    (while (setq latex-label (latex-label-search))
      (setq latex-label-list
	    (cons (cons latex-label count)
		  latex-label-list))
      (setq count (1+ count)))
    (reverse latex-label-list)))

(defun latex-label-insert (query-buffer)
  "Insertion of a LaTeX label."
  (interactive "P")
  (let ((latex-label-buffer
	 (if query-buffer
	     (read-buffer "LaTeX label search buffer: "
			  (buffer-name (current-buffer)) t)
	   (buffer-name (current-buffer)))))
    (let ((latex-label-alist
	   (save-excursion
	     (set-buffer latex-label-buffer)
	     (goto-char (point-min))
	     (latex-label-make-alist))))
      (unless latex-label-alist
	(error (format "Not found a LaTeX label in a buffer: %s"
		       latex-label-buffer)))
      (insert (completing-read "LaTeX label: "
			       latex-label-alist nil t nil
			       'latex-label-history)))))
