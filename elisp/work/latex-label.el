;;; Refer LaTeX Label
;;; Copyright (C) Yoshinori Toki in 1997
;;; $Id$

(defvar latex-label-completion-minibuffer-history ()
  "LaTeX label completion minibuffer history.")

(defun search-latex-label ()
  "Search a next LaTeX label in current buffer."
  (let (start-point begin-of-label end-of-label)
    (setq start-point (point))
    (search-forward "\\label{" (point-max) t)
    (setq begin-of-label (point))
    (search-forward "}" (point-max) t)
    (setq end-of-label (- (point) 1))
    (if (not (= start-point begin-of-label))
      (buffer-substring begin-of-label end-of-label) nil)))

(defun make-latex-label-alist ()
  "Make a LaTeX label associated list of current buffer."
  (let (idx latex-label latex-label-alist)
    (setq latex-label-alist ())
    (save-excursion
      (goto-char (point-min))
      (setq idx 0)
      (while (setq latex-label (search-latex-label))
	(setq idx (+ idx 1))
	(setq latex-label-alist
	      (cons (cons latex-label idx) latex-label-alist))))
    latex-label-alist))

(defun latex-label ()
  "Completion of a LaTeX label in current buffer."
  (interactive)
  (let (latex-label-alist)
    (setq latex-label-alist (make-latex-label-alist))
    (if (not latex-label-alist)
	(progn (error "Not found LaTeX labels in current buffer.")))
    (insert
     (completing-read "LaTeX label: " latex-label-alist nil t nil
		      'latex-label-completion-minibuffer-history))))

(defun latex-label-buffer ()
  "Completion of a LaTeX label in a buffer."
  (interactive)
  (let (scan-buffer-name latex-label-alist)
    (setq scan-buffer-name (read-buffer "Search buffer: " nil t))
    (if scan-buffer-name
      (progn
	(setq latex-label-alist ())
	(save-excursion
	  (set-buffer scan-buffer-name)
	  (setq latex-label-alist (make-latex-label-alist)))
	(if (not latex-label-alist)
	  (progn (error "Not found LaTeX labels in buffer: %s."
			scan-buffer-name)))
	(insert
	 (completing-read "LaTeX label: " latex-label-alist nil t nil
			  'latex-label-completion-minibuffer-history))))))
