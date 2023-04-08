(setq TeX-fold-type-list '(env macro comment)
      TeX-fold-env-spec-list '(("[comment]" ("comment")) ("[proof]" ("proof")))
      LaTeX-fold-env-spec-list '(("frame" ("frame")))
      TeX-fold-macro-spec-list
      '(("[f]" ("footnote" "marginpar"))
        ("[c]" ("cite"))
        ("[l]" ("label"))
        ("[r]" ("ref" "pageref" "eqref"))
        ("[i]" ("index" "glossary"))
        ("[1]:||*" ("item"))
        ("..." ("dots"))
        ("(C)" ("copyright"))
        ("(R)" ("textregistered"))
        ("TM" ("texttrademark"))
        (1 ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup"))))

(setq LaTeX-font-list
      '((?m "\\textmc{" "}" "\\mathmc{" "}")
        (?g "\\textgt{" "}" "\\mathgt{" "}")
        (?e "\\en{" "}")
        (?c "\\cn{" "}")
        (?4 "$" "$")
        (1 "" "" "\\mathcal{" "}")
        (2 "\\textbf{" "}" "\\mathbf{" "}")
        (3 "\\textsc{" "}")
        (5 "\\emph{" "}")
        (6 "\\textsf{" "}" "\\mathsf{" "}")
        (9 "\\textit{" "}" "\\mathit{" "}")
        (12 "\\textulc{" "}")
        (13 "\\textmd{" "}")
        (14 "\\textnormal{" "}" "\\mathnormal{" "}")
        (18 "\\textrm{" "}" "\\mathrm{" "}")
        (19 "\\textsl{" "}" "\\mathbb{" "}")
        (20 "\\texttt{" "}" "\\mathtt{" "}")
        (21 "\\textup{" "}")
        (23 "\\textsw{" "}")
        (4 "" "" t)))

(setq preview-default-option-list
      '("displaymath" "floats" "graphics" "textmath" "footnotes") ;; 执行预览的环境
      preview-preserve-counters t ;; 保留数学公式编号
      preview-pdf-color-adjust-method 'compatible) ;; 预览图片使用Emacs主题背景色

(setq  reftex-label-alist ;; 交叉引用的自定义类型
       '(("theorem" ?t "thm:" nil nil ("Theorem" "定理"))
         ("lemma" ?a "lem:" nil nil ("Lemma")))
       reftex-ref-macro-prompt nil ;; ~cte<tab>~ 后不提示类型
       reftex-ref-style-default-list '("Default")) ;; 默认引用风格

(require 'tex-mode) ;; 载入 tex--prettify-symbols-alist 变量
(defun my/more-prettified-symbols ()
  (mapc (lambda (pair) (delete pair tex--prettify-symbols-alist))
        '(("\\supset" . 8835)))
  (mapc (lambda (pair) (cl-pushnew pair tex--prettify-symbols-alist))
        '(("\\Z" . 8484)
          ("\\Q" . 8474)
          ("\\N" . 8469)
          ("\\R" . 8477)
          ("\\eps" . 949)
          ("\\inf" . #x22C0) 
          ("\\sup". #x22C1)
          ("\\ONE" . #x1D7D9)
          ("\\mathbb{S}" . #x1D54A)
          ("\\PP" . #x2119)
          ("\\Ps" . #x1D5AF )
          ("\\Pp" . #x1D40F)
          ("\\E" . #x1D5A4)
          ("\\Ee" . #x1D404)
          ("\\EE" . #x1D53C )
          ("\\Fc" . #x2131)
          ("\\Nc" . #x1D4A9))))
(my/more-prettified-symbols) ;; 读入自定义 prettify 符号
