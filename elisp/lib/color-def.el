;;
;;  hilit19 の カラー表の定義の追加
;;
(cond (window-system
       (require 'hilit19)

       (setq hilit-default-face-table 
             (append hilit-default-face-table
                     '(
                       (red        red        red     nil)
                       (orange     orange     orange  nil)
                       (yellow     yellow     yellow  nil)
                       (green      green      green   nil)
                       (forestgreen forestgreen forestgreen nil)
                       (darkgreen darkgreen darkgreen nil)
                       (cyan4 cyan4 cyan4 nil)
                       (blue       bule       bule    nil)
                       (purple     purple     purple  nil)
                       (pink       pink       pink    nil)
                       (black      black      black   nil)
                       (gray       gray45     gray45  nil)
                       (navy       navy       navy    nil)
                       (brown      brown      brown   nil)
                       (magenta4   magenta4   magenta4 nil)
                       (OrangeRed OrangeRed   OrangeRed nil)
                       (tomato     tomato     tomato  nil)
                       (DarkOrange DarkOrange DarkOrange nil)
                       (DeepPink   DeepPink   DeepPink  nil)
                       (firebrick firebrick   firebrick nil)
                       (DeepSkyBlue4 DeepSkyBlue4 DeepSkyBlue4 nil)
                       (DarkOliveGreen DarkOliveGreen DarkOliveGreen nil)
                       (bold       bold       bold    nil)))) ))
