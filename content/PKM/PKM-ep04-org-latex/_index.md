+++
title = "从零搭建个人知识库: Org 笔记中高效写 LaTeX"
draft = false
weight = 40
+++

## 前言 {#前言}

在 Org-mode 中获得和 LaTeX 模式 **一致** 的 LaTeX 代码体验:

-   使用 CDLaTeX
-   语法高亮
-   数学符号 `prettify`
-   括号自动匹配
-   快速公式预览
-   LaTeX 导言区 （preamble) 共享


## 默认设置 {#默认设置}

-   输入数学公式
    -   行内公式: `\(...\)`, `$...$`
    -   行间公式 `\[...\]`, `$$...$$`, `\begin{XXX}...\end{XXX}`
-   在行间数学公式中用 <kbd>C-c '</kbd> 可进入 LaTeX 编辑模式. (有些版本的 `org-mode` 的 LaTeX 编辑模式是采用 `latex-mode`)
    ```elisp
    ;; 你自己的 LaTeX 模式设置
    (defun my/latex-hook ()
      (turn-on-cdlatex)
      (turn-on-reftex))

    (add-hook 'LaTeX-mode-hook 'my/latex-hook)
    (add-hook 'latex-mode-hook 'my/latex-hook)
    ```
-   数学公式预览
    -   预览当前位置/当前选中区域 <kbd>C-c C-x C-l</kbd>
    -   取消预览: <kbd>C-u</kbd> + <kbd>C-c C-x C-l</kbd>


## 打开 CDLaTeX 模式 {#打开-cdlatex-模式}

Org-mode 有一个专属的 `org-cdlatex-mode`, 打开后可以使用 CDLaTeX 模式中定义的所有快捷键命令. 关于 CDLaTeX可以参考

-   [【省时省力写LaTeX】cdlatex中快速输入数学符号和字体与自定义设置](https://www.bilibili.com/video/BV1qa4y1u7Cd)
-   [【省时省力写LaTeX】Tab补全快速插入LaTeX代码](https://www.bilibili.com/video/BV1Rb421J7eS)

<!--listend-->

```elisp
(add-hook 'org-mode-hook #'org-cdlatex-mode) ;; 在 org-mode 中使用 cdlatex
```

主要区别:

-   <kbd>Tab</kbd> 要兼容 `org-mode` 中的展开/折叠功能
    -   环境补全只能用在行首, 如 `equ` + <kbd>Tab</kbd> = `\begin{equation}\end{equation}`
    -   数学环境中 <kbd>Tab</kbd> 的功能与 LaTeX 一致, 如 `qq` + <kbd>Tab</kbd> = `\quad`
    -   其余情形 <kbd>Tab</kbd> 执行 Org-mode 的展开/折叠
-   反引号 <kbd>`</kbd> 输入数学符号可以在非数学环境中使用 (但是符号不能被预览, 只能 prettify).
-   `$`, `(` 等不再自动匹配.


## 语法高亮与美化 {#语法高亮与美化}

```elisp
(setq org-highlight-latex-and-related '(native latex entities)) ;; LaTeX 语法高亮设置
(setq org-pretty-entities t) ;; LaTeX 代码的 prettify
(setq org-pretty-entities-include-sub-superscripts nil) ;; 不隐藏 LaTeX 的上下标更容易编辑
```

-   `org-pretty-entities` 和 LaTeX 模式中 `prettify-symbol-mode` 共享变量 `tex--prettify-symbols-alist` 的设置
-   设置 `tex--prettify-symbols-alist` 可参考 [【Emacs+LaTeX教程】如何优雅地预览数学公式](https://www.bilibili.com/video/BV1tv4y1V7xY/)


## 括号和 `$` 匹配问题 {#括号和-匹配问题}

其实也可以用 `cdlatex-mode` 替代 `org-cdlatex-mode`, 但是确实有一些副作用.

折衷的解决方法: 复写 `cdlatex-mode` 中的括号和 `$` 匹配函数.

```elisp
 ;; org-cdlatex-mode 中使用 cdlatex 的自动匹配括号, 并把 $...$ 换成 \( ... \)
 (defun my/insert-inline-OCDL ()
   (interactive)
   (insert "\\(") ;; 把 "\\(" 和 "\\)" 替换成 "$" 就能实现输入成对 "$" 的功能.
   (save-excursion (insert "\\)" )))
(defun my/insert-bra-OCDL ()
   (interactive)
   (insert "(")
   (save-excursion (insert ")" )))
 (defun my/insert-sq-bra-OCDL ()
   (interactive)
   (insert "[")
   (save-excursion (insert "]" )))
 (defun my/insert-curly-bra-OCDL ()
   (interactive)
   (insert "{")
   (save-excursion (insert "}" )))

 (define-key org-cdlatex-mode-map (kbd "$") 'my/insert-inline-OCDL)
 (define-key org-cdlatex-mode-map (kbd "(") 'my/insert-bra-OCDL)
 (define-key org-cdlatex-mode-map (kbd "[") 'my/insert-square-bra-OCDL)
 (define-key org-cdlatex-mode-map (kbd "{") 'my/insert-curly-bra-OCDL)
```

这里还做了一个优化: 把 <kbd>$</kbd> 绑定成输入 `\(?\)`, 原因有二:

-   用 `$` 输入行内公式, 要求公式内紧临 `$` 的字符不能是空格, 否则不能识别为数学环境. 识别失败又会导致语法高亮、 `cdlatex-mode` 不能触发等问题
-   用 `$` 输入行内公式, 后面必须是空格. 这和 LaTeX 模式下输入习惯不同.

用 `\(?\)` 替代就没有以上所有问题.


## 公式预览 {#公式预览}

基本命令

-   <kbd>C-c C-x C-l</kbd>: 预览/取消预览当前光标所在数学公式, 或预览当前章节所有数学公式
-   <kbd>C-u</kbd> + <kbd>C-c C-x C-l</kbd>: **取消** 预览当前 **章节** 所有数学公式
-   <kbd>C-u C-u</kbd> + <kbd>C-c C-x C-l</kbd>: 预览当前 **缓冲区** 所有数学公式
-   <kbd>C-u C-u C-u</kbd> + <kbd>C-c C-x C-l</kbd>: **取消** 预览当前 **缓冲区** 所有数学公式.

预览图片大小调整

```elisp
(setq my/latex-preview-scale 2) ;; 一般来说这里的 scale 约等于 set-face-attribute 中的 :height /100
(setq org-format-latex-options
      `(:foreground default :background default :scale ,my/latex-preview-scale :html-foreground "Black" :html-background "Transparent" :html-scale ,my/latex-preview-scale :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))) ;; 增大公式预览的图片大小
```


### 快速编译 `org-preview` {#快速编译-org-preview}

`org-preview` 是一个非正式的包, 极大 **提高** 了数学公式预览速度 (异步编译, 有望加入未来版本的 Org).

```elisp
;; 快速编译数学公式
(use-package org-preview
  :load-path "lisp/" ; 需要手动从网盘或 https://github.com/karthink/org-preview/ 下载 org-preview.el 文件, 并置于 ~/.emacs.d/lisp/ 文件夹下
  ;; straight 用户用下一行取代上一行
  ;; :straight (:host github :repo "karthink/org-preview")
  :hook (org-mode . org-preview-mode))
```


### 自动预览 `org-fragtog` {#自动预览-org-fragtog}

-   光标移出公式, 自动编译
-   光标移入公式, 自动展开

<!--listend-->

```elisp
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))
```


## LaTeX 导言区共享 {#latex-导言区共享}

Org-mode 如何用上自定义的 LaTeX 宏命令?


### 方法一: 使用 `#+LATEX_HEADER:` 关键字 {#方法一-使用-plus-latex-header-关键字}

在 Org 文件开头加入 =
   `#+LATEX_HEADER: \newcommand{\R}{\mathbb{R}}`
就可以在整个文件中使用 `\mathbb{R}`


### 方法二: 使用 `#+SETUPFILE:` 关键字 {#方法二-使用-plus-setupfile-关键字}

可以创建一个叫 `latex-preamble.org` 放在 Org-roam 目录下, 然后给所有的笔记文件开头 (可以放在笔记模板里) 加上
    `#+SETUPFILE: ./latex-preamble.org`
然后把所有要用的导言区命令每一行加上 `#+LATEX_HEARDER:` 放在 `latex-preamble.org` 文件中.


### 方法三: 通过 `.sty` 文件与 LaTeX 文件共享自定义命令 {#方法三-通过-dot-sty-文件与-latex-文件共享自定义命令}

-   把常用的导言区命令放在 `~/texmf/tex/latex/` 目录下的 `mysymbol.sty` 文件中
-   添加设置
    ```elisp
    ;; 在 ~/texmf/tex/latex/ 下的 .sty 文件
    (setq org-latex-packages-alist '(("" "mysymbol" t)))
    ```

原理:

-   `~/texmf` 是正常情况下 `$TEXMF` 的目录. 可以通过命令行 `kpsewhich --var-value TEXMF` 确认
-   `$TEXMF/tex/latex/` 目录下的 `.sty` 的文件可以被 `\usepackage` 引用.
-   上述 Emacs 设置相当于 Org 编译 LaTeX 公式时默认加载 `\usepackage{mysymbol}` 命令; 而在 LaTeX 文件中也可以使用同样的命令.


## 如何导出 Org 笔记 {#如何导出-org-笔记}

-   <kbd>C-c C-e h o</kbd> 导出成 HTML 并打开
-   <kbd>C-c C-e l o</kbd> 导出成 PDF 并打开.


## 总结 {#总结}

Org-mode 和 CDLaTeX 的强强联合

-   Org 笔记的灵活性
-   Org-roam 双链笔记工作流程
-   CDLaTeX 的流畅数学公式体验

代码汇总

```elisp
;; 你自己的 LaTeX 模式设置
(defun my/latex-hook ()
  (turn-on-cdlatex)
  (turn-on-reftex))

(add-hook 'LaTeX-mode-hook 'my/latex-hook)
(add-hook 'latex-mode-hook 'my/latex-hook)
(add-hook 'org-mode-hook #'org-cdlatex-mode) ;; 在 org-mode 中使用 cdlatex

(setq org-highlight-latex-and-related '(native latex entities)) ;; LaTeX 语法高亮设置
(setq org-pretty-entities t) ;; LaTeX 代码的 prettify
(setq org-pretty-entities-include-sub-superscripts nil) ;; 不隐藏 LaTeX 的上下标更容易编辑


;; org-cdlatex-mode 中使用 cdlatex 的自动匹配括号, 并把 $...$ 换成 \( ... \)
(defun my/insert-inline-OCDL ()
  (interactive)
  (insert "\\(") ;; 把 "\\(" 和 "\\)" 替换成 "$" 就能实现输入成对 "$" 的功能.
  (save-excursion (insert "\\)" )))
(defun my/insert-bra-OCDL ()
  (interactive)
  (insert "(")
  (save-excursion (insert ")" )))
(defun my/insert-sq-bra-OCDL ()
  (interactive)
  (insert "[")
  (save-excursion (insert "]" )))
(defun my/insert-curly-bra-OCDL ()
  (interactive)
  (insert "{")
  (save-excursion (insert "}" )))

(define-key org-cdlatex-mode-map (kbd "$") 'my/insert-inline-OCDL)
(define-key org-cdlatex-mode-map (kbd "(") 'my/insert-bra-OCDL)
(define-key org-cdlatex-mode-map (kbd "[") 'my/insert-square-bra-OCDL)
(define-key org-cdlatex-mode-map (kbd "{") 'my/insert-curly-bra-OCDL)

(setq my/latex-preview-scale 2) ;; 一般来说这里的 scale 约等于 set-face-attribute 中的 :height /100
(setq org-format-latex-options
      `(:foreground default :background default :scale ,my/latex-preview-scale :html-foreground "Black" :html-background "Transparent" :html-scale ,my/latex-preview-scale :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))) ;; 增大公式预览的图片大小
;; 快速编译数学公式
(use-package org-preview
  :load-path "lisp/" ; 需要手动从网盘或 https://github.com/karthink/org-preview/ 下载 org-preview.el 文件, 并置于 ~/.emacs.d/lisp/ 文件夹下
  ;; straight 用户用下一行取代上一行
  ;; :straight (:host github :repo "karthink/org-preview")
  :hook (org-mode . org-preview-mode))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; 在 ~/texmf/tex/latex/ 下的 .sty 文件
(setq org-latex-packages-alist '(("" "mysymbol" t)))
```
