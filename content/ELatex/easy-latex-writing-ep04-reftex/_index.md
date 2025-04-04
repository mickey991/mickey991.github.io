+++
title = "RefTeX 管理交叉引用"
draft = false
weight = 40
+++

## RefTeX 解决的痛点 {#reftex-解决的痛点}

你是否遇到过以下困扰：

-   标签数量过多，不知该如何命名？
-   标签过短担心重复，过长又难以记忆？
-   在引用时抓狂不已：在 tex 文件中翻来覆去，只为找到公式并复制标签到引用位置？

RefTeX 让你轻松创建、引用和管理标签，随心所欲，毫无压力。


## 插入标签 {#插入标签}

用 <kbd>M-x</kbd> + `reftex-label`, 或者 `lbl` + <kbd>Tab</kbd> (CDLaTeX 的命令补全) 均可以生成形如 `\label{type:XXXX}` 的标签.

标签的内容根据标签的类型, 有以下的方式决定.

-   自动生成的数字标签. 这适用于公式环境 `\label{eq:NNN}` 或者列表环境 `\label{it:NNN}`, 这里 `NNN` 表示自动生成的数字. 它是 RefTeX 在当前文档中找到的可以使用的最小数字.
    -   在写数学证明的过程中, 很多需要引用的公式只是在证明过程中引用, 这时自动编号就剩去了想标签名字的烦恼.
    -   我常配合 <kbd>C-u C-c C-e</kbd> 改变环境使用, 把从无标签公式变有标签公式.
-   根据上下文生成标签: 章节. 例如, `\section{First Second Third}` 自动生成 `\label{sec:first-second-third}`
-   如果以上方法均不适用, 则可以手动输入标签.


## 引用标签 {#引用标签}

用 <kbd>M-x</kbd> + `reftex-reference`,  <kbd>C-c [</kbd> 或者 `ref` + <kbd>Tab</kbd> (CDLaTeX 命令补全) 均可以触发引用交叉标签.

-   第一步: 选择引用的宏命令, 如 `\ref`, `\pageref` 等
-   第二步: 选择标签类型, 默认类型包括 (<kbd>?</kbd> 查看帮助):
    -   `e`: 公式标签 (`eq:`), 包括 `equation`, 以及 `align`, `gather` 等 `amsmath` 定义的数学公式环境
    -   `i`: 列表标签 (`it:`), 列表环境 `item` 的标签
    -   `s`: 章节标签 (`sec:`), `f`: 图片标签 (`fig:`), 等等
    -   <kbd>SPC</kbd> 可以不指定标签类型
-   第三步: 进入标签选择界面 (<kbd>?</kbd> 查看帮助).
    -   移动光标
        -   上下: <kbd>p</kbd> / <kbd>n</kbd>, <kbd>Up</kbd> / <kbd>Down</kbd>
        -   上一个/下一个章节标题: <kbd>C-c C-p</kbd> / <kbd>C-c C-n</kbd>
        -   跳到第 N 个 section: <kbd>N z</kbd>
    -   上下文: <kbd>SPC</kbd>
    -   刷新标签列表: <kbd>r</kbd>
    -   插入单个标签: <kbd>RET</kbd> 插入当前标签
    -   插入多个标签: <kbd>m</kbd> (<kbd>+</kbd>, <kbd>-</kbd> ) 进行标记, 再用 <kbd>RET</kbd> 或  <kbd>a</kbd> 插入. <kbd>a</kbd> 可以把标签插入到同一个 `\ref{}` 命令中
    -   搜索标签: <kbd>Tab</kbd>


## 基本设置 {#基本设置}


### 开启 RefTeX {#开启-reftex}

包含在我们一开始的 AucTeX 配置代码中:

```elisp
(defun my/latex-hook ()
  (turn-on-cdlatex)
  (turn-on-reftex))

(add-hook 'LaTeX-mode-hook 'my/latex-hook)
```


### 关闭引用宏命令的询问 {#关闭引用宏命令的询问}

默认使用 `\ref{}` 格式,  `ref` + <kbd>Tab</kbd> 直接进入标签选择

```elisp
(setq reftex-ref-macro-prompt nil)
```

在标签选择界面, 可以用 <kbd>v</kbd> 和 <kbd>V</kbd> 去改变引用格式.


### magic word 功能 {#magic-word-功能}

`reftex-reference` 会识别光标前的文字自动选择标签类型. 例如, `equation` 后插入标签会默认类型为 `e`

可以由变量 `reftex-guess-label-type` 控制

```elisp
(setq reftex-guess-label-type t) ; 默认值
```


### 自定义标签类型和 magic word {#自定义标签类型和-magic-word}

可以通过 <kbd>M-x</kbd> `customize-variable` 界面修改 `reftex-label-alist`


#### 例 1: 新增标签类型 `t`,  绑定 `theorem` 环境, 标签以 `thm:` 开头 {#例-1-新增标签类型-t-绑定-theorem-环境-标签以-thm-开头}

-   Environment or `\macro`: `theorem`
-   Type specification: `t`
-   Label prefix string: `thm:`
-   magic word: `Theorem`, `定理`

当我们选择某个标签类型如 `t` 时

-   (默认) 若 `reftex-trust-label-prefix` 为 `nil`, 只会用环境名 `theorem` 来决定标签类型
-   若 `reftex-trust-label-prefix` 为 `t`, 也会用 `thm:` 来识别标签类型.

<!--listend-->

```elisp
(setq reftex-trust-label-prefix t)
```


#### 例 2: 给 `e` 类型加入更多的 magic word {#例-2-给-e-类型加入更多的-magic-word}

-   Type specification: `e`
-   magic word: `公式`
-   其它 **留空**


### 生成标签的方式 {#生成标签的方式}

可以通过 <kbd>M-x</kbd> `customize-variable` 界面修改 `reftex-insert-label-flag`

-   Derive label from context: 使用上下文生成标签的类型. 比如我们可以把 `t` 加进去.
-   Prompt for label string: 插入前在小缓冲区提示.

如果一个类型没有在两个列表中出现, 则使用数字作为标签.


### 自动打开跟随模式 {#自动打开跟随模式}

```elisp
(setq reftex-label-menu-flags '(t t nil nil t nil t nil)) ; 在标签选择界面
```


## 代码总结 {#代码总结}

```elisp
;; 为 LaTeX 模式加载 RefTeX
(defun my/latex-hook ()
  (turn-on-cdlatex)
  (turn-on-reftex))

(add-hook 'LaTeX-mode-hook 'my/latex-hook)

(setq reftex-ref-macro-prompt nil)
(setq reftex-guess-label-type t) ; 默认值
(setq reftex-trust-label-prefix t)
(setq reftex-label-menu-flags '(t t nil nil t nil t nil)) ; 标签选择界面跟随界面
;; 以及通过 customize-variable 对 reftex-label-alist 和 reftex-insert-label-flag 的设置
```
