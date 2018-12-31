;;; ruby-unit-test.el --- Emacs からコンパイルモードでTest::Unitのテストケースを実行する。

(require 'compile)

(defvar ruby-unit-test-ruby-command "bundle exec ruby"
  "Rubyを実行するコマンドを設定する。")

(defvar ruby-unit-test-rake-test-command "bundle exec rake test"
  "Rakeでtestタスクを実行するコマンドを設定する。")

(defvar ruby-unit-test-runner-options nil
  "TestRunnerのオプションを設定する。")

(defvar ruby-unit-test-method-definition-regexp
  '((pattern . "\\(^\\|\\s-\\)def\\s-+\\(test_[^ \t(){}?!|]+[?!]?\\)")
    (name-pos . 2)))

(defvar ruby-unit-test-class-definition-regexp
  '((pattern . "\\(^\\|\\s-\\)class\\s-+\\([^ \t]+\\)\\s-*<\\s-*Test::Unit::TestCase")
    (name-pos . 2)))

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

(defun ruby-unit-test-goto-test-method-definition ()
  "メソッド定義の行へ移動する。"
  (end-of-line)                         ;カレント行を検索対象に含めるため
  (let ((case-fold-search nil))
    (re-search-backward (cdr (assq 'pattern ruby-unit-test-method-definition-regexp)) nil t)))

(defun ruby-unit-test-goto-test-class-definition ()
  "クラス定義の行へ移動する。"
  (end-of-line)                         ;カレント行を検索対象に含めるため
  (let ((case-fold-search nil))
    (re-search-backward (cdr (assq 'pattern ruby-unit-test-class-definition-regexp)) nil t)))

(defun ruby-unit-test-get-test-method-name (line)
  "Ruby Test::Unitのテストメソッドの名前を文字列から取り出して返す。"
  (let ((case-fold-search nil))
    (if (string-match (cdr (assq 'pattern ruby-unit-test-method-definition-regexp)) line)
        (match-string (cdr (assq 'name-pos ruby-unit-test-method-definition-regexp)) line))))

(defun ruby-unit-test-get-test-class-name (line)
  "Ruby Test::Unitのテストクラスの名前を文字列から取り出して返す。"
  (let ((case-fold-search nil))
    (if (string-match (cdr (assq 'pattern ruby-unit-test-class-definition-regexp)) line)
        (match-string (cdr (assq 'name-pos ruby-unit-test-class-definition-regexp)) line))))

(defun ruby-unit-test-get-test-file-command-string (test-file-name &optional test-options ruby-options)
  "Ruby Test::Unitのテストファイルを実行するコマンドを文字列で返す。"
  (concat ruby-unit-test-ruby-command " "
          (if ruby-options (concat ruby-options " ") "")
          (shell-quote-argument test-file-name)
          (if test-options (concat " " test-options) "")))

(defun ruby-unit-test-get-test-class-command-string (test-file-name test-class-name &optional test-options ruby-options)
  "Ruby Test::Unitのテストクラスを実行するコマンドを文字列で返す。"
  (concat (ruby-unit-test-get-test-file-command-string test-file-name
                                                       test-options
                                                       ruby-options)
          " "
          (shell-quote-argument (concat "-t/\\b" test-class-name "\\b/"))))

(defun ruby-unit-test-get-test-method-command-string (test-file-name test-class-name test-method-name &optional test-options ruby-options)
  "Ruby Test::Unitのテストメソッドを実行するコマンドを文字列で返す。"
  (concat (ruby-unit-test-get-test-class-command-string test-file-name
                                                        test-class-name
                                                        test-options
                                                        ruby-options)
          " "
          (shell-quote-argument (concat "-n" test-method-name))))

(defun ruby-unit-test-run-test-method (ruby-debug-option-p)
  "run test method of Ruby Test::Unit at compilation mode."
  (interactive "P")
  (save-excursion
    (let ((test-file-name (ruby-unit-test-get-test-file-name)))
      (if test-file-name
          (if (ruby-unit-test-goto-test-method-definition)
              (let ((test-method-name (ruby-unit-test-get-test-method-name (ruby-unit-test-get-line))))
                (if (ruby-unit-test-goto-test-class-definition)
                    (let ((test-class-name (ruby-unit-test-get-test-class-name (ruby-unit-test-get-line))))
                      (let ((command-string
                             (if ruby-debug-option-p
                                 (ruby-unit-test-get-test-method-command-string test-file-name
                                                                                test-class-name
                                                                                test-method-name
                                                                                ruby-unit-test-runner-options
                                                                                "-d")
                               (ruby-unit-test-get-test-method-command-string test-file-name
                                                                              test-class-name
                                                                              test-method-name
                                                                              ruby-unit-test-runner-options))))
                        (compile command-string)))
                  (message "Not found a Ruby Test::Unit test-case class.")))
            (message "Not found a Ruby Test::Unit method."))
        (message "Not a ruby script file.")))))

(defun ruby-unit-test-run-test-class (ruby-debug-option-p)
  "run test class of Ruby Test::Unit at compilation mode."
  (interactive "P")
  (save-excursion
    (let ((test-file-name (ruby-unit-test-get-test-file-name)))
      (if test-file-name
          (if (ruby-unit-test-goto-test-class-definition)
              (let ((test-class-name (ruby-unit-test-get-test-class-name (ruby-unit-test-get-line))))
                (let ((command-string
                       (if ruby-debug-option-p
                           (ruby-unit-test-get-test-class-command-string test-file-name
                                                                         test-class-name
                                                                         ruby-unit-test-runner-options
                                                                         "-d")
                         (ruby-unit-test-get-test-class-command-string test-file-name
                                                                       test-class-name
                                                                       ruby-unit-test-runner-options))))
                  (compile command-string)))
            (message "Not found a Ruby Test::Unit test-case class."))
        (message "Not a ruby script file.")))))

(defun ruby-unit-test-run-test-file (ruby-debug-option-p)
  "run test file of Ruby Test::Unit at compilation mode."
  (interactive "P")
  (save-excursion
    (let ((test-file-name (ruby-unit-test-get-test-file-name)))
      (if test-file-name
          (let ((command-string
                 (if ruby-debug-option-p
                     (ruby-unit-test-get-test-file-command-string test-file-name
                                                                  ruby-unit-test-runner-options
                                                                  "-d")
                   (ruby-unit-test-get-test-file-command-string test-file-name
                                                                ruby-unit-test-runner-options))))
            (compile command-string))
        (message "Not a ruby script file.")))))

(defun ruby-unit-test-run-rake-test ()
  "run test task of Rake at compilation mode."
  (interactive)
  (compile (concat ruby-unit-test-rake-test-command
                   (if ruby-unit-test-runner-options
                       (concat " " (shell-quote-argument (concat "TESTOPTS=" ruby-unit-test-runner-options)))
                     ""))))

(provide 'ruby-test-unit)

; Local Variables:
; mode: Emacs-Lisp
; indent-tabs-mode: nil
; End:
