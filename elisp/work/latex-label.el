;;; LaTeX label insertion
;;; $Id$

(defvar latex-label-history ()
  "LaTeX label completion minibuffer history.")

(defun latex-label-search ()
  "Search a next LaTeX label in current buffer."
  (let ((start-point (point))
	(begin-of-label (search-forward "\\label{" nil t))
	(end-of-label (search-forward "}" nil t)))
    (if (and begin-of-label end-of-label
	     (/= start-point begin-of-label))
	(buffer-substring-no-properties
	 begin-of-label (1- end-of-label)))))

(defun latex-label-make-alist (&optional num)
  "Make a LaTeX label associated list of current buffer."
  (if (not num)
      (setq num 0))
  (let ((latex-label (latex-label-search)))
    (if latex-label
	(cons (list latex-label num)
	      (latex-label-make-alist (1+ num))))))

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
      (if (not latex-label-alist)
	  (error (format "Not found a LaTeX label in a buffer: %s."
			 latex-label-buffer)))
      (insert
       (completing-read "LaTeX label: "
			latex-label-alist nil t nil
			'latex-label-history)))))
