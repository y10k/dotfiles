;;----------------------------------------------------------------------
;; カラフル Verilog モード
;;----------------------------------------------------------------------
;; 説明:
;;   Verilog-HDL を emacs(mule)上で記述する時に hilit19 を利用して
;;   キーワードに色をつけるプログラムです
;;
;; 補足:
;;  ・本プログラムを使用するには、color-def.el も必要となります。
;;     (→ http://www.pa.airnte.ne.jp/tsuruta/programs/color-def.el)
;;  ・verilog-mode.el との併用も可能です。
;;     (→ ftp://ftp.siliconlogic.com/pub/comp.lang.verilog/verilog-mode.Z)
;;
;; 「.emacs」の設定:
;;    (setq load-path (cons ("プログラムの置場所") load-path))
;;    (load "color-def")
;;    (load "verilog-color")
;;    ※ verilog-mode.el を使用する場合は (load "verilog") の前に
;;       (setq use-verilog-mode t) を加えて下さい
;;
;; 操作:
;;   [ESC]-[z]        : auto-indent の切り替え(verilog-mode)
;;----------------------------------------------------------------------
;;                        Copyright (C) 1995 - 1997 TSURUTA Mitsutoshi
;;----------------------------------------------------------------------
(defvar use-verilog-mode nil "usr verilog mode")

(cond ((null use-verilog-mode)
       (defun verilog-mode ()
         "Verilog-mode のダミー、Verilog の予約語に色を付けたい時に使う。"
         (interactive)
         (kill-all-local-variables)
         (setq major-mode 'verilog-mode)
         (setq mode-name "Verilog")))
      (t
       (autoload 'verilog-mode "verilog-mode" nil t)))
       
(setq auto-mode-alist 
  (append (list (cons "\\.v$" 'verilog-mode)
                (cons "\\.bhv$" 'verilog-mode)
                (cons "\\.rtl.*$" 'verilog-mode)
                (cons "testfixture.*" 'verilog-mode)
                (cons "\\.verilog$" 'verilog-mode))
          auto-mode-alist))

;(setq verilog-mode-hook 
;  '(lambda ()
;     (progn
;       (setq verilog-tab-always-indent t  ; [tab]キーをインデント用にするか
;             verilog-auto-newline t       ; 自動的に改行をするか
;             verilog-auto-endcomments nil ; endXXX の後ろに自動的にコメントを
;                                          ; 入れるか（↓こんな感じ）
;                                          ; function [a:b] XXX;
;                                          ; endfunction /* XXX */
;             verilog-indent-level 2       ; インデント深さ
;             verilog-continued-expr 2     ; ２行にわたる時に次の行のインデント
;             verilog-label-offset 0       ; ラベルのインデントレベル
;             verilog-case-offset 2        ; case 文のインデントレベル
;             )
;       (define-key verilog-mode-map "\M-z" 'change-verilog-auto-newline))))

(defun change-verilog-auto-newline ()
  "自動的に改行をするかしないかのスイッチ
   [ESC][z] で、切り替わる。"
  (interactive)
  (if (null verilog-auto-newline)
    (message "verilog-auto-newline mode : ON")
    (message "verilog-auto-newline mode : OFF"))
  (setq verilog-auto-newline (not verilog-auto-newline)))

(cond
  (window-system
    (hilit-set-mode-patterns
      'verilog-mode
      '(("//.*$" nil comment)
        ("/\\*.*" "\\*/" comment)
        ("`define" ".$" define)
        ("`include" ".$" define)
        ("`ifdef" ".$" define) ("`else" ".$" define) ("`endif" ".$" define)
        ("\\<\\(module\\|endmodule\\|function\\|endfunction\\)\\>" nil defun)
	("\\<\\(task\\|endtask\\|fork\\|join\\)\\>" nil defun)
        ("\\<\\(always\\|initial\\|assign\\|deassign\\|parameter\\)\\>" 1 keyword)
        ("\\<\\(posedge\\|negedge\\|or\\)\\>" 1 navy)
	("\\<\\(if\\|else\\|case\\|casez\\|casex\\|endcase\\)\\>" 1 keyword)
        ("\\<\\(begin\\|end\\|for\\|while\\|repeat\\|wait\\)\\>" 1 keyword)
        ("\\(\@\\|\#\\)" 1 keyword)
        ("\\<\\(input\\|output\\|inout\\|reg\\|wire\\)\\>" nil include)
        ("\\<\\(integer\\|real\\|time\\)\\>" nil include)
        ("\\(\$finish\\|\$stop\\)" nil glob-struct)
        ("\\(\$display\\|\$write\\|$monitor\\)" nil glob-struct)
        ("\\(\$random\\|\$readmemb\\|\$readmemh\\)" nil glob-struct)
        ("\\(\+\\|\-\\|\*\\|\/\\|\%\\)" 1 msg-quote)
        ("\\(\~\\|\&\\|\|\\|\\^\\|\!\\|\=\\|\<\\|\>\\)" nil msg-quote)
        ("[0-9]\*\'b[01zxZX_?]\*" nil DarkOrange)
        ("[0-9]\*\'h[0-9A-Fa-fzxZX_]\*" nil DarkOrange)
        ("[0-9]\*\'o[0-7zxZX_]\*" nil DarkOrange)
        ("[0-9]\*\'d[0-9zxZX_]\*" nil DarkOrange)
        ("\\<[0-9]\+\\>" nil OrangeRed)
        ("`[a-zA-Z0-9]\*" nil darkgreen)
        ))))
