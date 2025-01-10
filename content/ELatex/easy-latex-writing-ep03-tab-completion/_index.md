+++
title = "Tab 补全快速插入 LaTeX 代码"
draft = false
weight = 30
+++

大家好, 我是小米. 本期我们将介绍如何在 CDLaTeX 中用 `Tab` 补全命令快速地输入复杂的宏命令和环境模板.


## Tab 补全插入宏命令 {#tab-补全插入宏命令}

补全原理很简单, 用几个字母组合加 <kbd>Tab</kbd> 生成一些复杂的命令. 例如, `fr` + <kbd>Tab</kbd> 就会生成 `\frac{}{}`, 这里光标会停留在第一个括号内; 在第一个括号内完成输入后, 按 <kbd>Tab</kbd> 光标就会跳到下一个括号中. 因此, 输入一个常见的分数 `\frac{1}{2}` 只需要输入 <kbd>f</kbd> + <kbd>r</kbd> + <kbd>Tab</kbd> + <kbd>1</kbd> + <kbd>Tab</kbd> + <kbd>2</kbd>.


### 内置命令举例 {#内置命令举例}

CDLaTeX 内置了一些可补全的命令, 可以在 `cdlatex-command-alist-default` 变量中查看 (<kbd>C-h v</kbd>). 我们举一些例子 (以下 `?` 所在位置表示补全后光标停留的位置.)

-   分数 `fr` + <kbd>Tab</kbd> = `\frac{?}{}`, 根号 `sq` + <kbd>Tab</kbd> = `\sqrt{?}`
-   空格 `qq` + <kbd>Tab</kbd> = `\quad`, 大空格 `qqq` + <kbd>Tab</kbd> = `\qquad`
-   括号 `lr(` + <kbd>Tab</kbd> = `\left(?\right)`, `lr[` = `\left[?\right]`
-   章节标题 `sn` + <kbd>Tab</kbd> = `\section{?}`, `ss` + <kbd>Tab</kbd> = `\subsection{?}`, `sss` + <kbd>Tab</kbd> = `\subsubsubsection{?}`


### 一些自定义例子 {#一些自定义例子}

-   `te` + <kbd>Tab</kbd> = `\text{}`
-   `se` + <kbd>Tab</kbd> = `\{ \}` (set)
-   `st` + <kbd>Tab</kbd> = `\stackover{}{}`
-   `hl` + <kbd>Tab</kbd> = `\hline`, `hhl` + <kbd>Tab</kbd> = `\\ \hline` (表格中常用)
-   `big(` + <kbd>Tab</kbd> = `\big(?\big)`, `Big(` + <kbd>Tab</kbd> = `\Big(?\Big)`, `bigg(` + <kbd>Tab</kbd> = `\bigg(?\bigg)` (`\big`, `\Big`, `\bigg` 等是 `amsmath` 中调整括号大小的命令)
-   `lr<` + <kbd>Tab</kbd> = `\langle?\rangle`, 一对尖括号 \\(\langle  \rangle\\).

显然, 这里的关键字选择都是用命令中最开始的两到三个字母, 这样非常好记, 也很容易使用.


## Tab 补全环境模板 {#tab-补全环境模板}

大家可以看到这里的 `Tab` 补全其实就是一个替换字符串的过程. 当然字符串中也可以包括换行, 因此同样的机制也可以输入形如 `\begin{XXX} ... \end{XXX}` 的环境.


### 内置命令举例 {#内置命令举例}


#### `equation` 环境 {#equation-环境}

`equ` + <kbd>Tab</kbd> 插入如下模板:

其中, `\label{eq:XXX}` 是 `CDLaTeX` 调用 `reftex` 自动生成的数字标签.

类似的数学公式环境还有如

-   `ali` + <kbd>Tab</kbd> 插入 `align` 环境 (自动生成标签), `ali*` + <kbd>Tab</kbd> 插入 `align*` 环境 (无标签)
-   `gat` + <kbd>Tab</kbd> 插入 `gather` 环境 (自动生成标签), `gat*` + <kbd>Tab</kbd> 插入 `gather*` 环境 (无标签)


#### 列表环境 {#列表环境}

`enu` + <kbd>Tab</kbd> 插入

此时, 在 `enumerate` 环境中:

-   `it` + <kbd>Tab</kbd> = `\item`
-   <kbd>C-&lt;enter&gt;</kbd> 会换行并生成 `\item`

这里, `enu` + <kbd>Tab</kbd> 等同于用 `cdlatex-environment` (<kbd>C-c {</kbd> ) 插入 `enumerate` 环境

类似的还有

-   `ite` + <kbd>Tab</kbd> 插入 `itemize` 环境
-   `fg` + <kbd>Tab</kbd> 插入 `figure` 环境


## 自定义补全命令 {#自定义补全命令}

现在我们介绍如何自定义你自己需要的补全命令. 默认的补全命令都在 `cdlatex-command-alist-default` 中, 而现有的所有命令, 包括内置的和自定义的, 都可以通过 <kbd>C-c ?</kbd> 查看.

在用 <kbd>C-c ?</kbd> 查看时, 我们会在最右一列看到 `TEXT` 和 `MATH` 关键字:

-   `MATH` 关键字表示补全可以在 **数学环境** 中触发
-   `TEXT` 关键字表示补全可以在 **文本环境** 中触发

加入自定义新的补全命令通过修改变量 `cdlatex-command-alist`. 方法是调用 <kbd>M-x</kbd> `customize-variable =, 然后输入变量名 =cdlatex-command-alist`.


### 带参数的宏命令 {#带参数的宏命令}

例子: `te` + <kbd>Tab</kbd> 输入 `\text{?}` (光标停在括号内). 我们需要填入如下参数

-   keyword: `te`
-   Docstring: 随便填, 只是用于说明的解释性文字, 例如 `insert \text{}`
-   Replacement: `\text{?}` (`?` 表示光标停留的位置)
-   Hook: `cdlatex-position-cursor` (如果需要指定光标则必填!)
-   Argument: `nil` (这是上面 hook 的参数)
-   Text Mode: `nil`, Math mode: `t`

保存设置 (`Apply and Save`) 之后, 在已经打开的 `tex` 文件中用 <kbd>C-c C-n</kbd> 可以刷新设置, 就可以开始使用了.


### 插入匹配的括号 {#插入匹配的括号}

例子: `big{` + <kbd>Tab</kbd> 插入 `\big\{? \big\}`

-   keyword: `big{`
-   Docstring: `insert \big\{? \big\}`
-   Replacement: `\big\{? \big\`
-   Hook: `cdlatex-position-cursor`
-   Argument: `nil`
-   Text Mode: `nil`, Math mode: `t`

这里有两个细节. 第一是我们在 `?` 后面手动多加了一个空格, 这里因为在 LaTeX 编辑模式下, 按 <kbd>Tab</kbd> 会自动跳到一个空格位置, 因此我们尽量用空格把代码分隔开来, 便于以后的修改; 既然如此, 我们干脆在模板中加入这个空格.

第二个细节时我们的替换字符串最后少了一个 `}`. 这是因为 `CDLaTeX` 中默认会自动匹配输入一对括号 `{}`. 因此我们只需要补全除了右花括号 `}` 以外的部分就可以.
`CDLaTeX` 中自动匹配的括号可以通过 `cdlatex-paired-parens` 设置, 只针对 `$([{<|` 6个字符. 我一般会自动匹配
`$([{` . 这里大家只需要注意你在 `cdlatex-command-alist` 中的设置与 `cdlatex-paired-parens` 保持一致就可以了.


### 插入环境 {#插入环境}

例子: `case` + <kbd>Tab</kbd> 插入

-   keyword: `case`
-   Docstring: `insert \begin{cases} \end{cases}`
-   Replacement: 输入框内用 <kbd>C-j</kbd> 换行, 然后正常输入需要替换的文本即可
-   Hook: `cdlatex-position-cursor`
-   Argument: `nil`
-   Text Mode: `nil`, Math mode: `t`


### 插入环境 II {#插入环境-ii}

插入环境除了直接在 `cdlatex-command-alist` 的 `Replacement` 中写入环境模板以外, 还可以通过调用函数 `cdlatex-environment` 的方式实现.
在 LaTeX 编辑模式中, 有两种用环境名插入环境的方法

-   <kbd>M-x</kbd> `LaTeX-environment` (<kbd>C-c C-e</kbd>) + `description`: 这会调用 `AucTeX` 的环境模板
-   <kbd>M-x</kbd> `cdlatex-environment` (<kbd>C-c {</kbd> ) + `description`: 这会调用 `CDLaTeX` 的环境模板.

两种模板略有不同. 这第二种插入环境的方法就是用 <kbd>Tab</kbd> 补全触发第二个命令.

例子: `des` + <kbd>Tab</kbd> 插入 `description` 环境

-   keyword: `des`
-   Docstring: `insert \begin{description} \end{description}`
-   Replacement: `nil`
-   Hook: `cdlatex-environment`
-   Argument: `("description")`
-   Text Mode: `t`, Math mode: `nil`

这里需要注意的是我们用了一个不同的 `hook`! 所插入的模板是由 `cdlatex-env-alist`, `cdlatex-env-alist-default` 控制的.

使用这种方式插入环境的好处:

-   支持自动插入标签: `AUTOLABEL` 关键字 (`equ` + <kbd>Tab</kbd> 生成带标签的环境的实现方式)
-   支持多行环境的 `item` 模板 (<kbd>C-&lt;enter&gt;</kbd> 触发)

不过, 在一般情况下, 第一种方法直接把环境模板写进 `cdlatex-command-alist` 也能实现大部分的功能了.


## 我的一些设置分享 {#我的一些设置分享}

我的 `cdlatex-command-alist` 变量, 仅做抛砖引玉之用.

```elisp
  (setq cdlatex-command-alist
        '(("eq" "insert pairs of \\[ \\]" "\\[ ? \\]" cdlatex-position-cursor nil t t)
          ("Big(" "insert Big ()" "\\Big( ? \\Big" cdlatex-position-cursor nil nil t)
          ("Big[" "insert Big[" "\\Big[ ? \\Big" cdlatex-position-cursor nil nil t)
          ("Big\\|" "insert Big \\|" "\\Big\\| ? \\Big\\|" cdlatex-position-cursor nil nil t)
          ("Big{" "insert Big{}" "\\Big\\{ ? \\Big\\" cdlatex-position-cursor nil nil t)
          ("Big|" "insert Big|" "\\Big| ? \\Big|" cdlatex-position-cursor nil nil t)
          ("aali" "insert equation" "\\left\\{\\begin{aligned}\n? \n\\end{aligned}\\right." cdlatex-position-cursor nil nil t)
          ("alb" "Insert beamer alert block with overlay" "\\begin{alertblock}<+->{ ? } \n\n\\end{alertblock}" cdlatex-position-cursor nil t nil)
          ("alb*" "Insert beamer alert block without overlay" "\\begin{alertblock}{ ? } \n\n\\end{alertblock}" cdlatex-position-cursor nil t nil)
          ("big(" "insert big ()" "\\big( ? \\big" cdlatex-position-cursor nil nil t)
          ("big[" "insert big []" "\\big[ ? \\big" cdlatex-position-cursor nil nil t)
          ("big\\|" "insert big \\|" "\\big\\| ? \\big\\|" cdlatex-position-cursor nil nil t)
          ("bigg(" "insert bigg()" "\\bigg( ? \\bigg" cdlatex-position-cursor nil nil t)
          ("bigg[" "insert bigg[" "\\bigg[ ? \\bigg" cdlatex-position-cursor nil nil t)
          ("bigg\\|" "insert bigg\\|" "\\bigg\\| ? \\bigg\\|" cdlatex-position-cursor nil nil t)
          ("bigg{" "insert bigg{}" "\\bigg\\{ ? \\bigg\\" cdlatex-position-cursor nil nil t)
          ("bigg|" "insert bigg|" "\\bigg| ? \\bigg|" cdlatex-position-cursor nil nil t)
          ("big{" "insert big {}" "\\big\\{ ? \\big\\" cdlatex-position-cursor nil nil t)
          ("big|" "insert big|" "\\big| ? \\big|" cdlatex-position-cursor nil nil t)
          ("blo" "Insert beamer block with overlay" "\\begin{block}<+->{ ? } \n\n\\end{block}" cdlatex-position-cursor nil t nil)
          ("blo*" "Insert beamer block WITHOUT overlay" "\\begin{block}{ ? } \n\n\\end{block}" cdlatex-position-cursor nil t nil)
          ("bn" "binomial" "\\binom{?}{}" cdlatex-position-cursor nil nil t)
          ("capl" "insert \\bigcap\\limits_{}^{}" "\\bigcap\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("case" "insert cases" "\\begin{cases}\n? & \\\\\n &\n\\end{cases}" cdlatex-position-cursor nil nil t)
          ("cd" "insert cdots" "\\cdots" nil nil t t)
          ("cupl" "insert \\bigcup\\limits_{}^{}" "\\bigcup\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("dd" "insert ddots" "\\ddots" nil nil t t)
          ("def" "insert definition env" "" cdlatex-environment ("definition") t nil)
          ("des" "insert description" "" cdlatex-environment ("description") t nil)
          ("enu*" "insert enu" "\\begin{enumerate}\n\\item ?\n\\end{enumerate}" cdlatex-position-cursor nil t nil)
          ("equ*" "insert unlabel equation" "" cdlatex-environment ("equation*") t nil)
          ("exb" "Insert beamer example block with overlay" "\\begin{exampleblock}<+->{ ? } \n\n\\end{exampleblock}" cdlatex-position-cursor nil t nil)
          ("exb*" "Insert beamer example block without overlay" "\\begin{exampleblock}{ ? } \n\n\\end{exampleblock}" cdlatex-position-cursor nil t nil)
          ("exe" "Insert exercise" "\\begin{exercise}\n? \n\\end{exercise}" cdlatex-position-cursor nil t nil)
          ("fra" "insert frame (for beamer)" "" cdlatex-environment ("frame") t nil)
          ("hhl" "insert \\ \\hline" "\\\\ \\hline" ignore nil t nil)
          ("hl" "insert \\hline" "\\hline" ignore nil t nil)
          ("ipenu" "insert in paragraph enumerate" "" cdlatex-environment ("inparaenum") t nil)
          ("ipite" "insert in paragraph itemize" "" cdlatex-environment ("inparaitem") t nil)
          ("it" "insert \\item" "\\item?" cdlatex-position-cursor nil t nil)
          ("ld" "insert ldots" "\\ldots" nil nil t t)
          ("lem" "insert lemma env" "" cdlatex-environment ("lemma") t nil)
          ("liml" "insert \\lim\\limits_{}" "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)
          ("lr<" "insert bra-ket" "\\langle ? \\rangle" cdlatex-position-cursor nil nil t)
          ("myenu" "insert in my enumerate for beamer" "" cdlatex-environment ("myenumerate") t nil)
          ("myite" "insert in my itemize for beamer" "" cdlatex-environment ("myitemize") t nil)
          ("ons" "" "\\onslide<?>{ }" cdlatex-position-cursor nil t t)
          ("pa" "insert pause" "\\pause" ignore nil t nil)
          ("pro" "insert proof env" "" cdlatex-environment ("proof") t nil)
          ("prodl" "insert \\prod\\limits_{}^{}" " \\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
          ("prop" "insert proposition" "" cdlatex-environment ("proposition") t nil)
          ("se" "insert \\{\\}" "\\{ ? \\}" cdlatex-position-cursor nil nil t)
          ("spl" "insert split" "" cdlatex-environment ("split") nil t)
          ("st" "stackrel" "\\stackrel{?}{}" cdlatex-position-cursor nil nil t)
          ("te" "insert text" "\\text{?}" cdlatex-position-cursor nil nil t)
          ("thm" "insert theorem env" "" cdlatex-environment ("theorem") t nil)
          ("vd" "insert vdots" "\\vdots" nil nil t t)))
```
