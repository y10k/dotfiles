;;
;;  ewb-mode.el
;;
;;  written by Minero Aoki
;;

(require 'derived)


(define-derived-mode ewb-mode text-mode "ewb"
  "Major mode for ewb."
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "$")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ewb-next-line (n)
  "move down to bottom in window"
  (interactive "p")
  (ver-move n))

(defun ewb-prev-line (n)
  "move up to top in window"
  (interactive "p")
  (ver-move (- n)))

(defun ver-move (n)
  "move vertical in window"
  (let ((cc (current-column-w)))
    (vertical-motion n)
    (move-to-column (+ (current-column) cc))))

(defun ewb-beginning-of-line ()
  "move to beginning of line"
  (interactive)
  (move-to-column (- (current-column) (current-column-w))))

(defun ewb-end-of-line ()
  "move to end of line"
  (interactive)
  (let ((c (save-excursion
	     (vertical-motion 1)
	     (backward-char 1)
	     (current-column))))
    (move-to-column c)))

(defun current-column-w ()
  (- (current-column)
     (save-excursion (vertical-motion 0) (current-column))))

(defun ewb-next-paragraph (n)
  "move to next ewb paragraph"
  (interactive "p")
  (next-line n))

(defun ewb-prev-paragraph (n)
  "move to prev ewb paragraph"
  (interactive "p")
  (previous-line n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ewb-new-paragraph ()
  "begin new paragraph and insert space"
  (interactive)
  (enter-new-paragraph)
  (insert "@"))

(defun ewb-insert-tt ()
  "insert //tt"
  (interactive)
  (insert-and-back "//tt{" "//}"))

(defun ewb-insert-cb ()
  "insert //cb"
  (interactive)
  (enter-new-paragraph)
  (insert-and-back "\n//cb{\n" "\n//}\n"))

(defun ewb-insert-lst1 ()
  "insert //lst1"
  (interactive)
  (skk-mode-off)
  (enter-new-paragraph)
  (insert-and-back "\n//lst1{\n" "\n//}\n"))

(defun ewb-insert-lst2 ()
  "insert //lst2"
  (interactive)
  (skk-mode-off)
  (enter-new-paragraph)
  (insert-and-back "\n//l " "\n//lst2{\n\n//}\n"))

(defun ewb-insert-ky ()
  "insert //ky"
  (interactive)
  (enter-new-paragraph)
  (insert-and-back "//ky{\n//ky5" "\n//}\n"))

(defun ewb-insert-k1 ()
  "insert //k1"
  (interactive)
  (enter-new-paragraph)
  (insert-and-back "\n//k1{\nœ//|" "\n//}\n"))

(defun ewb-insert-k2 ()
  "insert //k2"
  (interactive)
  (enter-new-paragraph)
  (insert-and-back "\n//k2{\n1.//|" "\n//}\n"))

(defun ewb-insert-i ()
  "insert //i"
  (interactive)
  (enter-new-paragraph)
  (insert "\n//i "))

(defun ewb-insert-ii ()
  "insert //ii"
  (interactive)
  (enter-new-paragraph)
  (insert "\n//ii "))

(defun ewb-insert-iii ()
  "insert //iii"
  (interactive)
  (enter-new-paragraph)
  (insert "\n//iii "))

(defun ewb-insert-iiii ()
  "insert //iiii"
  (interactive)
  (enter-new-paragraph)
  (insert "\n//iiii "))

(defun ewb-insert-iiiii ()
  "insert //iiiii"
  (interactive)
  (enter-new-paragraph)
  (insert "\n//iiiii "))

(defun insert-and-back (beg end)
  (insert beg end)
  (backward-char (length end)))

(defun enter-new-paragraph ()
  (or (looking-at "^")
      (insert "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key ewb-mode-map "\C-n" 'ewb-next-line)
(define-key ewb-mode-map "\C-p" 'ewb-prev-line)
(define-key ewb-mode-map "\C-a" 'ewb-beginning-of-line)
(define-key ewb-mode-map "\C-e" 'ewb-end-of-line)
(define-key ewb-mode-map "\M-a" 'ewb-prev-paragraph)
(define-key ewb-mode-map "\M-e" 'ewb-next-paragraph)

(define-key ewb-mode-map "\C-t" 'ewb-insert-tt)
(define-key ewb-mode-map "\C-c\C-c" 'ewb-insert-cb)
(define-key ewb-mode-map "\C-c\C-l" 'ewb-insert-lst1)
(define-key ewb-mode-map "\C-c\C-i" 'ewb-insert-lst2)
(define-key ewb-mode-map "\C-c\C-u" 'ewb-insert-k1)  ; unordered
(define-key ewb-mode-map "\C-c\C-o" 'ewb-insert-k2)  ; ordered
(define-key ewb-mode-map "\C-c\C-k" 'ewb-insert-ky)
(define-key ewb-mode-map "\C-c1" 'ewb-insert-i)
(define-key ewb-mode-map "\C-c2" 'ewb-insert-ii)
(define-key ewb-mode-map "\C-c3" 'ewb-insert-iii)
(define-key ewb-mode-map "\C-c4" 'ewb-insert-iiii)
(define-key ewb-mode-map "\C-c5" 'ewb-insert-iiiii)
(define-key ewb-mode-map "\C-c\C-j" 'ewb-new-paragraph)
(define-key ewb-mode-map "\C-c " 'ewb-new-paragraph)

(define-key ewb-mode-map "\M-q" nil)

(provide 'ewb-mode)
