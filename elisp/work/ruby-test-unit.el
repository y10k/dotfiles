;;; ruby-unit-test.el --- Emacs からコンパイルモードでTest::Unitのテストケースを実行する。

(defvar ruby-unit-test-runner-options nil
  "TestRunnerのオプションを設定する。")

(defun ruby-unit-test-get-test-file-name ()
  "カレントバッファで開いているテストファイルの名前を返す。"
  (let ((file-name (buffer-file-name)))
    (if file-name
        (if (string-match ".*\\.[Rr][Bb]$" file-name)
            file-name))))

(defun ruby-unit-test-get-point-at-beginning-of-line ()
  "ポイントをカレントバッファの行頭に移動してポイント値を返す。"
  (beginning-of-line)
  (point))

(defun ruby-unit-test-get-point-at-end-of-line ()
  "ポイントをカレントバッファの行末に移動してポイント値を返す。"
  (end-of-line)
  (point))

(defun ruby-unit-test-get-line ()
  "カレントバッファのポイントの1行を文字列で返す。"
  (buffer-substring-no-properties
   (ruby-unit-test-get-point-at-beginning-of-line)
   (ruby-unit-test-get-point-at-end-of-line)))

(defun ruby-unit-test-get-test-method-name (line)
  "Ruby Test::Unitのテストメソッドの名前を文字列から取り出して返す。"
  (let ((case-fold-search nil))
    (if (string-match "def[ \t]+\\(test_[A-Za-z0-9_]+\\??\\)" line)
        (match-string 1 line))))

(defun ruby-unit-test-get-test-method-command-string (test-file-name test-method-name &optional test-options ruby-options)
  "Ruby Test::Unitを実行するコマンドを文字列で返す。"
  (concat "bundle exec ruby "
          (if ruby-options (concat ruby-options " ") "")
          test-file-name
          (if test-options (concat " " test-options) "")
          " -n" test-method-name))

(defun ruby-unit-test-run-test-method (ruby-debug-option-p)
  "run test method of Ruby Test::Unit at compilation mode."
  (interactive "P")
  (save-excursion
    (let ((test-file-name (ruby-unit-test-get-test-file-name))
          (test-method-name (ruby-unit-test-get-test-method-name (ruby-unit-test-get-line))))
      (if (and test-file-name test-method-name)
          (let ((command-string
                 (if ruby-debug-option-p
                     (ruby-unit-test-get-test-method-command-string test-file-name test-method-name ruby-unit-test-runner-options "-d")
                   (ruby-unit-test-get-test-method-command-string test-file-name test-method-name ruby-unit-test-runner-options))))
            (compile command-string))
        (message "Not found a Ruby Test::Unit method here.")))))

; Local Variables:
; mode: Emacs-Lisp
; indent-tabs-mode: nil
; End:
