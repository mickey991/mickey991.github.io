+++
title = "CDLaTeX 中快速输入数学符号和字体与自定义设置"
draft = false
+++

大家好, 我是小米, 欢迎大家来到我的省时省力写 LaTeX 系列. 本期我们开始介绍 Emacs 的 CDLaTeX 插件. 这次讲解如何使用 CDLaTeX 快速插入数学字母, 符号和字体的功能, 以及如何自定义新的快捷键.


## AucTeX 和 CDLaTeX 基本设置 {#auctex-和-cdlatex-基本设置}

Emacs 中的 LaTeX 编辑主要是依赖 [AucTeX](https://www.gnu.org/s/auctex) 和 [CDLaTeX](https://github.com/cdominik/cdlatex) 这两个插件. AucTeX 提供了编辑 LaTeX 的基本功能, 而 CDLaTeX 主要提供了大量简化和易设置的输入方式.  为了安装并在 LaTeX 编辑时启用这两个插件, 我们需要在 `init.el` 中加入代码:

```elisp
(defun my/latex-hook ()
  (turn-on-cdlatex)
  (turn-on-reftex))

(use-package tex
  :ensure auctex
  ;; 若使用 straight, 注释前一行, 并取消下一行注释:
  ;; :straight auctex
  :custom
  (TeX-parse-self t) ; 自动解析 tex 文件
  (TeX-PDF-mode t)
  (TeX-DVI-via-PDFTeX t)
  :config
  (setq-default TeX-master t) ; 默认询问主文件
  (add-hook 'LaTeX-mode-hook 'my/latex-hook)) ; 加载LaTeX模式钩子

(use-package cdlatex
  :after tex ; 保证 cdlatex 在 auctex 之后加载
  :load-path "lisp/" ; 需要手动从网盘或 https://github.com/cdominik/cdlatex/blob/master/cdlatex.el 下载 cdlatex.el 文件, 并置于 ~/.emacs.d/lisp/ 文件夹下
  ;; 若使用 straight, 注释前一行, 并取消下一行注释:
  ;; :straight (:host github :repo "cdominik/cdlatex" )
  )
```

使用 `straight.el` 的用户需要根据注释内容适当调整. 在 `(use-package cdlatex ...)` 中我们指定了 `:after tex`, 是为了保证 `cdlateX` 在 `auctex` 之后加载. 把 `cdlatex` 的 `use-package` 代码块置于 `auctex` 之后也实现了相同效果; 而加了这一行后, 代码块次序可以随意调整.

`LaTeX-mode-hook` 是我们打开 LaTeX 文件时需要加载的设置, 这里我们定义了一个新的函数 `my/latex-hook` (名字可随意), 这样方便我们日后加入更多的功能. 函数的第一行 `(turn-on-cdlatex)` 就是打开 `tex` 文件时加载 `cdlatex-mode` 的命令.

成功设置后, 当我们打开 `tex` 文件时, 大家应当可以看到模式栏中的 `LaTeX/P` 和 `CDL`, 就表示加载了 `aucteX` 和 `cdlatex`. 通过 <kbd>C-h m</kbd> (<kbd>m</kbd> 表示 mode) 可以查看当前加载的所有主要模式和次要模式.


## 数学符号输入 {#数学符号输入}

这里的数学符号也包括各种非拉丁字母如 `\alpha`, `\aleph` 等. 输入方法是用反引号 (<kbd>Tab</kbd> 上方) 加另一个键组成的快捷键输入.


### 插入希腊字母 {#插入希腊字母}

希腊字母可以用 <kbd>\`</kbd> + 对应拉丁字母插入, 包括大小写. 例如

-   <kbd>\`</kbd> + <kbd>a</kbd>: `\alpha`
-   <kbd>\`</kbd> + <kbd>b</kbd>: `\beta`
-   <kbd>\`</kbd> + <kbd>g</kbd>: `\gamma`
-   <kbd>\`</kbd> + <kbd>G</kbd>: `\Gamma`
-   <kbd>\`</kbd> + <kbd>S</kbd>: `\Sigma`

如果你不熟悉希腊字母对应的拉丁字母, 没有关系, 只要在按下 <kbd>\`</kbd> 后稍稍停顿, 就会弹出一个提示界面.
大家刚开始使用时可以多查看这个提示界面.


### 数学符号 {#数学符号}

大家在提示界面可以看到, 除了希腊字母以外, 我们还可以用同样的方法快速插入数学符号. CDLaTeX 预置了很多好记的默认设置. 例如, <kbd>\`</kbd> + <kbd>8</kbd> 插入 `\infty`, 因为数字8放平就是无穷, 又如, <kbd>\`</kbd> + <kbd>*</kbd> 插入 `\times` 乘号, <kbd>\`</kbd> + <kbd>+</kbd> 插入 `\cup` (并集), <kbd>\`</kbd> + <kbd>&gt;</kbd> 插入 `\rightarrow` (右箭头) 等.


### 第二和第三层目录 {#第二和第三层目录}

`CDLaTeX` 中连续按下两次反引号 <kbd>\`</kbd> 可以打开第二层目录. 第二层通常用于希腊字母的变体, 如

-   <kbd>\`e</kbd> 插入 `\epsilon`, <kbd>\`\`e</kbd> 插入 `\varepsilon`
-   <kbd>\`r</kbd> 插入 `\rho`, <kbd>\`\`r</kbd> 插入 `\varrho`

又或者是一些类似的符号, 如

-   <kbd>\`&gt;</kbd> 插入 `\rightarrow`, <kbd>\`\`&gt;</kbd> 插入 `\longrightarrow`

或者是多个符号最直观的快捷键相同, 但是频率最高的放在第一层, 频率低的放在第二层, 如

-   <kbd>\`d</kbd> 插入 `\delta`, <kbd>\`\`d</kbd> 插入 `\partial` (求偏导符号)

这个目录还有第3层, 这里绑定的快捷键就更少了. 默认的是一些数学函数的符号, 如 `\sin`, `\exp` 等


### 如何插入 LaTeX 左双引号 `` `` `` {#如何插入-latex-左双引号}

反引号在 LaTeX 中写作几乎不会用到, 除了用于左双引号 <kbd>\`\`</kbd> (laTeX 的右双引号是 `''` ). 这很好解决: 在 AucTeX 默认设置下, 第一个输入的双引号 <kbd>"</kbd> 会自动转换成为 <kbd>\`\`</kbd> 插入, 第二个输入的双引号 <kbd>"</kbd> 会转换为 <kbd>\'\'</kbd> . 例如, <kbd>"word"</kbd> 将插入 <kbd>``word\'\'</kbd>.

当然, 你也可以把反引号修改成其它的键, 但是既然无须担心双引号输入的问题, 我觉得改的意义不大. 反引号已经是很好的选择.


## 自定义数学符号快捷键 {#自定义数学符号快捷键}

Emacs 的最大优势就是我们可以自由地设置. 前面反引号 <kbd>\`</kbd> 触发的快捷输入, 我们也可以添加自己需要的符号或调整已有的设置.

这里的所有设置保存在一个叫 `cdlatex-math-symbol-alist` 的变量中. 我们接下来讲解在 Emacs 如何设置一个变量, 保存设置以及加载设置. 这对其它的变量也是一样.


### 打开设置界面 {#打开设置界面}

虽然所有的变量设置都可以通过 `init.el` 里面的 `(setq ...)` 语句完成, 对于 `cdlatex-math-symbol-alist` 这种结构非常复杂的变量, 新手还是建议用 Emacs 自带的设置界面.

打开一个变量的设置界面主要有两种方式 (以 `cdlatex-math-symbol-alist` 为例)

1.  通过 `customize-variable` 命令:
    <kbd>M-x</kbd> `customize-variable`  <kbd>RET</kbd> <kbd>M-x</kbd> `cdlatex-math-symbol-alist`
2.  从变量的帮助界面进入设置界面:
    <kbd>C-h v</kbd> `cdlatex-math-symbol-alist`  并点击 `customize`


### 设置实例 {#设置实例}

我们想调换 <kbd>\`e</kbd> 和 <kbd>\`\`e</kbd> 原本的快捷键设置, 即实现如下效果:  <kbd>\`e</kbd> 插入 `\varepsilon`, <kbd>\`\`e</kbd> 插入 `\epsilon`. (这么做的原因是 `\varepsilon` 更常用).

1.  打开 `cdlatex-math-symbol-alist` 的设置界面
2.  点击 <kbd>INS</kbd> 插入一个新条目
3.  在 `character` 后输入 `e`
4.  在 `Repeat` 后按 <kbd>INS</kbd>, 新插入的一行输入 `\varepsilon`
5.  在 `Repeat` 后按 <kbd>INS</kbd>, 新插入的一行输入 `\epsilon`

这就完成了基本设置. 如果大家想绑定 <kbd>\`\`\`e</kbd> 和 <kbd>\`\`\`\`e</kbd> 等, 只需要再加入新的行以及你需要的 LaTeX 宏命令即可.

这里因为 <kbd>\`e</kbd> 已经在 `CDLaTeX` 的默认设置中, 所以我们是覆盖了原有设置. 你可以在一开始的按下 <kbd>\`</kbd> 的提示界面中看到默认设置, 或者通过查看变量 `cdlatex-math-symbol-alist-default`.


### 保存与加载设置 {#保存与加载设置}

设置完毕我们会点击 `Apply and Save`.

-   `Apply`: 改变了当前 `cdlatex-math-symbol-alist` 的値, 重启 Emacs 后失效
-   `Save`: 保存设置, 重启后仍生效.

但是已经打开的 `tex` 文件是看不到更新的设置的. 想要重新加载 `CDLaTeX` 的设置. 这有3种方法:

1.  重启 Emacs
2.  一个是打开新的 `tex` 文件
3.  在原来的 `.tex` 文件缓冲区, 按下 <kbd>C-c C-n</kbd>.

第三种方法可以刷新 LaTeX 模式设置, 也适用于其它与 `cdlatex` 的设置. 此时, 大家按下反引号 <kbd>\`</kbd> 就可以看到更新后的列表了.


### 怎么选择快捷键 {#怎么选择快捷键}

原则上这个机制可以插入任意的数学表达式, 如 `\stackrel{\mathrm{a.s.}}{=`}=, 但是建议只绑定原子化的数学符号. 复杂的表达式更适合用 CDLaTeX 的命令补全功能. (参考 [Tab 补全快速插入LaTeX代码]({{< relref "easy-latex-writing-ep03-tab-completion" >}}))

快捷键要易记, 直观, 凭你的第一感觉就能找到. 否则不能提高输入速度. 大家也可以查看默认的设置寻找灵感. 反例就是把左箭头 `\leftarrow` 绑到 <kbd>\`&gt;</kbd> 上.

如果一个键上绑定了多层快捷键, 要考虑不同命令使用的频率, 把最常用的放在第一层, 次常用的放在第二层, 依此类推. 像上面的 `\epsilon` 和 `\varepsilon` 的例子.

你也可以绑定自己定义的宏命令. 例如, 我的 <kbd>\`e</kbd> 绑定的是 `\eps`, 而在我的 LaTeX 文档引言区中会定义 `\newcommand{\eps}{\varepsilon}`. 这样的好处可以提高代码的可读性, 方便交流. 毕竟你的导师, 你的合作者未必用 Emacs, 长长的 `\varepsilon` 会让人眼花. 但是我输入时想到的是希腊字母 epsilon 就应该用 <kbd>\`e</kbd> 输入.

这里有很大的发挥空间, 因为第二层和第三层基本都是空的, 每个键还分大小写, 可以自由设置100多个快捷键. 所以尽情发挥吧.


## 数学字体修饰 {#数学字体修饰}


### 数学字体 {#数学字体}

CDLaTeX 还可以快速插入不同的数学字体, 像 `\mathrm{}`, `\mathbf{}` 等等.
例如, 我们常常用粗体 R 表示实数域, 也就是 `\mathbf{R}`. 我们可以按3个键完成输入: <kbd>R</kbd> + <kbd>\'</kbd> + <kbd>b</kbd>

-   <kbd>R</kbd>: 输入字母 R
-   <kbd>\'</kbd> (单引号): 打开数学字体列表. 作用相当于前面的 <kbd>\`</kbd>
-   <kbd>b</kbd>: 在字母 R 外面插入表示粗体的 LaTeX 宏命令 `\mathbf{}`

按单引号 <kbd>\'</kbd> 默认会改变前一个字母的字体, 也包括希腊字母, 但只是前面一个字母. 例:

-   <kbd>\`a\'b</kbd> 插入 `\mathbf{\alpha}`
-   <kbd>ab\'b</kbd> 插入 `a\mathbf{b}`.

如果需要改变多个字母的字体可以先选择字体, 再输入文本. 这就是第二种方法. 但是输入单引号时前面要是空格或者 `$`, `{` 这种功能性字符. 例:

-   <kbd>$\'babc</kbd> 插入 `$\mathbf{abc}$`.
-   <kbd>$a\'bc</kbd> 插入 `$\mathbf{a}c$`.

可以用于改变多个字符的字体.


### 其它修饰 {#其它修饰}

这种插入方式也可以推广到一切 LaTeX 宏命令 + 一对花括号内一段文本的结构. 除了像 `\mathbf{}`, `\mathrm{}` 这种数学字体, 还可以输入

-   数学公式中对字母的其它修饰, 如

    -   <kbd>\'&gt;</kbd> 插入 `\vec{}`
    -   <kbd>\'^</kbd> 插入 `\hat{}`
    -   <kbd>\'-</kbd> 插入 `\bar{}`

    这里默认的快捷捷非常直观, 大家也可以按下单引号 `'` 稍等以查看提示界面.
-   非数学公式中的文本字体, 如
    -   <kbd>\'b</kbd> 插入 `\textbf{}`
    -   <kbd>\'i</kbd> 插入 `\textit{}`

这里同样的 <kbd>\'b</kbd>, 用在数学公式内就是 `\mathbf{}`, 用在文本中就是 `\textbf{}`. CDLaTeX 会自动检测当前环境是否为数学环境.


### 嵌套修饰 {#嵌套修饰}

触发字体修饰的第三种方法是选先高亮选中一段文本, 再选择修饰. 例如, 选中数学环境外的 `blabla`, 然后按 <kbd>'b</kbd>, 则 `blabla` 会变成 `\textbf{blabla}`. 如果 `blabla` 在数学环境内, 则变成 `\mathbf{blabla}`

第一种方法只能修饰一个字母, 所以嵌套修饰只能使用第二种或第三种方法. 例:

-   <kbd>\'-\'bR</kbd> 插入 `\bar{\mathbf{R}}`.
-   <kbd>R\'b</kbd> 插入 `\mathbf{R}`, 然后高亮选中按下 <kbd>\'-</kbd>, 变成 `\bar{\mathbf{R}}`


## 自定义字体修饰 {#自定义字体修饰}

这里需要设置的变量是 `cdlatex-math-modify-alist`. 打开设置界面的方法和前面一样, 输入
<kbd>M-x</kbd> `customize-variable` <kbd>RET</kbd> `cdlatex-math-modify-alist`

现在我们举一个例子. 假设我们想用 <kbd>\'t</kbd> 在数学公式中插入空心粗体 `\mathbb{}`. 操作如下

-   打开 `cdlatex-math-modify-alist` 设置界面
-   点击 <kbd>INS</kbd> 新建一个条目
-   第一行 `character` 输入 <kbd>t</kbd>
-   第二行: `\mathbb`
-   第三行: 保持空白, 因为文本模式下没有空心粗体, 或者输入 `\text`, 这是 `CDLaTeX` 的默认设置.
-   第四行: `Type` 改成 `command`. 两种方式几乎等价但是 `command` 现在更常用.
-   第五, 第六行: 不变

我们修改完之后, 按 `Apply and Save` 保存, 然后在 `tex` 文件缓冲区中用 <kbd>C-c C-n</kbd> 刷新设置, 这样我们在数学环境中按下 <kbd>\'</kbd> 就能插入空心粗体 `\mathbb{}` 了.


## `customize-variable` 设置保存位置 {#customize-variable-设置保存位置}

我们的 `init.el` 设置里面有这样两行:

```elisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; .....
;; .....
(if (file-exists-p custom-file) (load-file custom-file))
```

这样 Emacs 会把通过 `customize-variable` 设置的变量保存在我们自定义的 `custom.el` 的文件中. 内容大概像这样:

```elisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cdlatex-math-modify-alist '((116 "\\mathbb" "" t nil nil)))
 '(cdlatex-math-symbol-alist '((101 ("\\varepsilon" "\\epsilon")))))
;; ......
```

这里包含了我们前面对 `cdlatex-math-modify-alist` 和 `cdlatex-math-symbol-alist` 的设置.

如果没有特别的设置, `customize-variable` 设置的变量默认会由 Emacs 保存到 `init.el` 文件的最后. 我们的设置可以区分自己的设置和 Emacs 保存的设置.

当然, 你也可以手动把 `custom-set-variables` 中的内容用 `(setq ...)` 语句写在你的 `init.el` 当中, 尤其可以放在相应插件的 `use-package` 代码块中. 这样的好处是方便单独管理每个插件的设置, 并且利用 `use-package` 的延迟加载功能加快打开 Emacs 的时间. 当我们的 CDLaTeX 设置很长的时候, 这样做可以把 Emacs 的启动时间从10多秒减少到1秒以下. 大家可以在熟悉了 Emacs 的设置后再做尝试, 新手不推荐这么做.


## 总结 {#总结}

Emacs 中的 CDLaTeX 插件利用反引号 <kbd>\`</kbd> 和单引号 <kbd>\'</kbd> 开始的快捷键可以快速插入数学字母, 符号和字体. 我们可以通过设置 `cdlatex-math-symbol-alist` 和 `cdlatex-math-modify-alist` 这两个变量修改和增加自己喜欢的快捷键.

在下期视频中我们将介绍 CDLaTeX 中 <kbd>Tab</kbd> 的命令/模板补全功能. 它可以帮助我们输入一些更复杂的宏命令, 或者插入环境模板等等.
