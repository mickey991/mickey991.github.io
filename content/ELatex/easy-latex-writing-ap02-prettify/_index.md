+++
title = "如何优雅地预览公式"
draft = false
weight = 220
+++

## 所见即所得的实现方式 {#所见即所得的实现方式}

文本编辑中的两个要素

-   文本本身
-   文本的格式


### pdf 文件预览: 正向与逆向搜索 {#pdf-文件预览-正向与逆向搜索}

[【Emacs+LaTeX教程】Emacs最强内置pdf阅读功能pdf-tools简介](https://www.bilibili.com/video/BV1pg4y1s7Z9/)
缺点

-   需要大屏幕
-   如果编译错误就无法预览


### 使用 `preview-latex` {#使用-preview-latex}

[【教程】LaTeX+Emacs从零开始2-6节：所见即所得之Preview-latex](https://www.bilibili.com/video/BV1H4411a7fD/)
缺点

-   需要手动执行编译: 常用键 <kbd>C-c C-p C-p</kbd>
-   代码的可读性不强


### 使用 `prettify-symbols-mode` {#使用-prettify-symbols-mode}

优点

-   不需要手动触发
-   没有编译过程, 不会报错
-   提高了代码的可读性


## 基本设置 {#基本设置}

版本要求

-   Emacs &gt;= 25
-   AucTex &gt;= 13.1.10 (可通过 <kbd>M-x package-list-package</kbd> 中查找 `auctex` 查看)


### 临时打开 {#临时打开}

<kbd>M-x prettify-symbols-mode</kbd>


### `init.el` 文件设置 {#init-dot-el-文件设置}

```elisp
  (defun my-latex-hook ()
    (prettify-symbols-mode t))
  (add-hook 'LaTeX-mode-hook 'my-latex-hook)
```


### 字体设置 {#字体设置}

保证 Unicode 数学符号可以正确显示

```elisp
  (set-fontset-font "fontset-default" 'mathematical "Cambria Math")
```


### 自动展开 {#自动展开}

设置自动展开光标附近的宏命令.

```elisp
  (setq prettify-symbols-unprettify-at-point t)
```

tips: 如果只想删除刚输入的一个宏命令, 最快的方法是用 <kbd>C-/</kbd> 撤消, 而不是一个个字符删除.


## 加入自己的符号 {#加入自己的符号}

```elisp
  (require 'tex-mode)
  (defun my/more-prettified-symbols ()
    (mapc (lambda (pair) (cl-pushnew pair tex--prettify-symbols-alist))
          '(("\\Z" . 8484) ;; 大多数人在latex中会用 \Z, \Q, \N, \R 表示数域
            ("\\Q" . 8474)
            ("\\N" . 8469)
            ("\\R" . 8477)
            ("\\eps" . 949)
            ("\\ONE" . #x1D7D9)
            ("\\mathbb{S}" . #x1D54A)
            ("\\PP" . #x2119) ;; 个人需要, 经常要使用P和E的数学字体
            ("\\P" . #x1D5AF )
            ("\\Pp" . #x1D40F)
            ("\\E" . #x1D5A4)
            ("\\Ee" . #x1D404)
            ("\\EE" . #x1D53C )
            ("\\Fc" . #x2131)
            ("\\Nc" . #x1D4A9))))
  (my/more-prettified-symbols)
```

将 <kbd>("&lt;latex 宏命令&gt;" . &lt;unicode 编码&gt;)</kbd> 加入列表中

-   latex 宏命令一般以 "`\\`" 开头, 表示一个普通的 "`\`".
-   unicode 编码以 "`#x`" 表示是 16 进制数字, 否则就是 10 进制
-   常用编码表: <https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode>

个人加入编码的原则

-   原列表中没有的编码
-   像 `\N` , `\Z` 等大多数人使用的宏命令, 这样可以减少与他人合作的障碍
-   进一步简化自己的常用命令, 像上面的各种 `E`, `P`.


## LaTeX 相关设置汇总 {#latex-相关设置汇总}

```elisp
  ;; 以下为LaTeX mode相关设置
  (setq-default TeX-master nil) ;; 编译时问询主文件名称
  (setq TeX-parse-selt t) ;; 对新文件自动解析(usepackage, bibliograph, newtheorem等信息)
  ;; PDF正向搜索相关设置
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)

  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用pdf-tools 打开 pdf
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer) ;; 在完成编译后刷新 pdf 文件

  ;; 打开TeX文件时应该加载的mode/执行的命令
  (defun my-latex-hook ()
    (turn-on-cdlatex) ;; 加载cdlatex
    (outline-minor-mode) ;; 加载outline mode
    (prettify-symbols-mode t)
    (turn-on-reftex)  ;; 加载reftex
    (outline-hide-body)) ;; 打开文件时只显示章节标题

  (add-hook 'LaTeX-mode-hook 'my-latex-hook)

  (setq prettify-symbols-unprettify-at-point t)
  (set-fontset-font "fontset-default" 'mathematical "Cambria Math")

  (require 'tex-mode)
  (defun my/more-prettified-symbols ()
    (mapc (lambda (pair) (cl-pushnew pair tex--prettify-symbols-alist))
          '(("\\Z" . 8484) ;; 大多数人在latex中会用 \Z, \Q, \N, \R 表示数域
            ("\\Q" . 8474)
            ("\\N" . 8469)
            ("\\R" . 8477)
            ("\\eps" . 949)
            ("\\ONE" . #x1D7D9)
            ("\\mathbb{S}" . #x1D54A)
            ("\\PP" . #x2119) ;; 个人需要, 经常要使用P和E的数学字体
            ("\\P" . #x1D5AF )
            ("\\Pp" . #x1D40F)
            ("\\E" . #x1D5A4)
            ("\\Ee" . #x1D404)
            ("\\EE" . #x1D53C )
            ("\\Fc" . #x2131)
            ("\\Nc" . #x1D4A9))))
  (my/more-prettified-symbols)
```
