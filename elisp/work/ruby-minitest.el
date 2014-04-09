;;; ruby-minitest.el --- Emacs からコンパイルモードでMiniTestのテストケースを実行する。

(defun ruby-minitest-get-test-file-name ()
  "カレントバッファで開いているテストファイルの名前を返す。"
  (let ((file-name (buffer-file-name)))
    (if (string-match "test_.*\\.rb$" file-name)
        file-name)))

(defun ruby-minitest-get-point-at-beginning-of-line ()
  "ポイントをカレントバッファの行頭に移動してポイント値を返す。"
  (beginning-of-line)
  (point))

(defun ruby-minitest-get-point-at-end-of-line ()
  "ポイントをカレントバッファの行末に移動してポイント値を返す。"
  (end-of-line)
  (point))

(defun ruby-minitest-get-line ()
  "カレントバッファのポイントの1行を文字列で返す。"
  (buffer-substring-no-properties
   (ruby-minitest-get-point-at-beginning-of-line)
   (ruby-minitest-get-point-at-end-of-line)))

(defun ruby-minitest-get-test-method-name (line)
  "Ruby MiniTestのテストメソッドの名前を文字列から取り出して返す。"
  (let ((case-fold-search nil))
    (if (string-match "def[ \t]+\\(test_[A-Za-z0-9_]+\\??\\)" line)
        (match-string 1 line))))

(defun ruby-minitest-get-command-string (test-file-name test-method-name)
  "Ruby MiniTestを実行するコマンドを文字列で返す。"
  (concat "bundle exec ruby " test-file-name " -n" test-method-name))

(defun ruby-minitest-get-command-string-with-ruby-options (test-file-name test-method-name ruby-options)
  "Ruby MiniTestを実行するコマンドを文字列で返す。"
  (concat "bundle exec ruby " ruby-options " " test-file-name " -n" test-method-name))

(defun ruby-minitest-run-test-method (ruby-debug-option-p)
  "run test method of Ruby MiniTest at compilation mode."
  (interactive "P")
  (let ((test-file-name (ruby-minitest-get-test-file-name))
        (test-method-name (ruby-minitest-get-test-method-name (ruby-minitest-get-line))))
    (if (and test-file-name test-method-name)
        (let ((command-string
               (if ruby-debug-option-p
                   (ruby-minitest-get-command-string-with-ruby-options test-file-name test-method-name "-d")
                 (ruby-minitest-get-command-string test-file-name test-method-name))))
          (compile command-string)))))

; Local Variables:
; mode: Emacs-Lisp
; indent-tabs-mode: nil
; End:
