+++
title = "省时省力写 LaTeX 系列"
draft = false
+++

-   引子: 五分钟说服你用 Emacs 写 LaTeX ([视频](https://www.bilibili.com/video/BV1Xk4y1a7Gp/))
-   第1节: [Emacs 快速配置与插件管理]({{< relref "easy-latex-writing-ep01-basic-setup-and-package-management" >}}) ([视频](https://www.bilibili.com/video/BV1nm4y117gn/))
-   第2节: [cdlatex 中快速输入数学符号和字体与自定义设置]({{< relref "easy-latex-writing-ep02-math-symbol-and-modify" >}}) ([视频](https://www.bilibili.com/video/BV1qa4y1u7Cd/))
-   第3节: [Tab 补全快速插入LaTeX代码]({{< relref "easy-latex-writing-ep03-tab-completion" >}}) ([视频](https://www.bilibili.com/video/BV1Rb421J7eS))
-   附录1: [Emacs最强内置pdf阅读器 pdf-tools 简介]({{< relref "easy-latex-writing-ap01-pdf-tools" >}}) ([视频](https://www.bilibili.com/video/BV1pg4y1s7Z9/))
-   附录2: [如何优雅地预览公式]({{< relref "easy-latex-writing-ap02-prettify" >}}) ([视频](https://www.bilibili.com/video/BV1tv4y1V7xY/))


## Emacs 快速配置与插件管理 {#easy-latex-writing-ep01-basic-setup-and-package-management}

本小节将介绍如何快速配置一个用于 LaTeX 写作的 [Emacs](https://www.gnu.org/s/emacs/), 包括软件安装和插件管理, 以及 Emacs 的一些必要的入门知识. 最后将推荐两组非常实用的插件: 第一组插件 [Which-key](https://github.com/justbur/emacs-which-key) + [Keycast](https://github.com/tarsius/keycast) + [Helpful](https://github.com/Wilfred/helpful) 将帮助我们快速熟悉 Emacs 的操作和概念, 第二组插件 [Vertico](https://github.com/minad/vertico) + [Marginalia](https://github.com/minad/marginalia) + [Orderless](https://github.com/oantolin/orderless) 将有效提升我们在小缓冲区的补全体验, 并附上 [AucTeX](https://www.gnu.org/s/auctex) 和 [CDLaTeX](https://github.com/cdominik/cdlatex) 的基本设置.


### Emacs 安装 {#emacs-安装}

Emacs 可以运行在 Windows, Linux, MacOS 上, 也可以通过 `termux` 运行在 Android 手机或平板上. 这里我们只介绍电脑系统上的 Emacs 安装.

在本文撰写时 (2023.4), Emacs 的最新正式版本为 28.2, 最近的测试版本为 29.090. 下面介绍的安装方法针对的是正式版. (更新: 2023.8 已经可以下载 Emacs 29 正式版.)

在多数平台上, 我们是利用合适的开源软件管理器来安装 Emacs. 在 Linux 中这是自带的, 在 Windows 和 MacOS 中大家需要自行安装. 我们下面详细说明.


#### Linux {#linux}

Linux 系统中可以用自带的软件管理器安装 Emacs. 例如, 在 Ubuntu 中使用 `apt` 安装 Emacs, 只需要在命令行中输入

```shell
sudo apt install emacs
```


#### MacOS {#macos}

MacOS 中需要先安装 [Homebrew](https://brew.sh/). 方法是在命令行中输入

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

当 Homebrew 安装成功后会有提示, 然后就可以使用 Homebrew 安装 Emacs 了: 在命令行中输入

```shell
brew install emacs
```


#### Windows {#windows}

Windows 下安装 Emacs 有两种常见方法. 第一种是安装 MSYS2 ([下载链接](https://www.msys2.org/)). 安装后打开 `C:/msys64/mingw64.exe` (`C:/msys64/` 是 MSYS2 的默认安装目录, 根据实际情况调整). 在命令行中输入

```shell
pacman -S mingw-w64-x86_64-emacs
```

通过 MSYS2 安装的一个好处是我们可以通过 Pacman 管理 Emacs 的更新. 另一个好处是在上面还可以很方便安装其它开源软件, 例如 Git, Epdfinfo 等. Epdfinfo 是在 Windows 下使用 Emacs 的 [PDF-tools](https://github.com/vedang/pdf-tools) 插件的必需软件 ([BV1pg4y1s7Z9](https://www.bilibili.com/video/BV1pg4y1s7Z9/)).

另一种安装方法是直接从[官网](http://ftp.gnu.org/gnu/emacs/windows/emacs-28/)上下载安装包. Emacs 28 的安装包已经优化了不少, 会自动把程序安装至 `C:/Program Files/emacs` 目录下, 并附带卸载程序. 通过安装包安装的 Emacs 需要我们手动更新.


### 安装 Emacs 后的额外设置 {#安装-emacs-后的额外设置}


#### Ctrl 键设置 {#ctrl-键设置}

安装完 Emacs 之后, 我 **强烈建议** 大家交换 <kbd>Caps Lock</kbd>  与 <kbd>Left Ctrl</kbd>. Emacs 常常使用以 <kbd>Ctrl</kbd> 开始的快捷键, 因此把 <kbd>Ctrl</kbd> 与不常用的大写锁定 <kbd>Caps Lock</kbd> 交换是每个 Emacs 使用者对电脑 做的第一件事. <kbd>Ctrl</kbd> 键的广泛使用是因为在 Emacs 诞生之初, 当时通用的键盘 <kbd>Ctrl</kbd> 确实在当今的 <kbd>Caps Lock</kbd> 位置上. 再啰嗦一句: 交换 <kbd>Caps lock</kbd> 与 <kbd>Ctrl</kbd> 绝不是一件可有可无的事情, 它在我们日常使用 Emacs 中真的非常重要! 大家千万不要怕麻烦.

交换 <kbd>Ctrl</kbd> 与 <kbd>Caps Lock</kbd> 的方法在不同系统上也不一样.

<!--list-separator-->

-  Windows

    Windows 中更改键位可以通过注册表或者最新的 `PowerToys` 软件.

    <!--list-separator-->

    -  注册表方法 (适用于Win 10 以前)

        方法如下:

        -   <kbd>Win</kbd> + <kbd>r</kbd> 并输入 `regedit` 打开注册表
        -   找到目录 `[HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]`
        -   新建 `Scancode` 文件, 把内容修改为
            ```nil
            00 00 00 00 00 00 00 00
            03 00 00 00 1d 00 3a 00
            3a 00 1d 00 00 00 00 00
            ```
        -   保存 `Scancode` 文件并重启电脑

    <!--list-separator-->

    -  PowerToys (适用于 Win 10, Win 11)

        -   将系统中 "微软商店" 更新到最新版本 (Win 10 不更新可能会找不到 PowerToys)
        -   在 "微软商店"= 中搜索 `PowerToys` 并安装
        -   在 PowerToys 中找到键位设置, 并交换 <kbd>Caps Lock</kbd> 和 <kbd>Left Control</kbd>

<!--list-separator-->

-  Ubuntu 及其它 Linux 系统

    在 Ubuntu 下, 可以安装 Gnome-tweaks:

    ```shell
    sudo apt install gnome-tweaks
    ```

    然后打开 Gnome-tweaks 的键盘设置, 在高级选项里有关于 <kbd>Ctrl</kbd> 键的设置. 你不仅仅可以交换它与 <kbd>Caps Lock</kbd>, 也可以进行许多别的设置.

    又或者, 在很多 Linux 系统的命令行下输入

    ```shell
    setxkbmap -option ctrl:swapcaps
    ```

    也可以交换 <kbd>Ctrl</kbd> 与 <kbd>Caps Lock</kbd> .

<!--list-separator-->

-  MacOS

    在 MacOS 中, 大家可以在 `system` -&gt; `keyboard` -&gt; `functional keys` 中调整所有功能键的键位.


#### 家目录与系统路径 {#家目录与系统路径}

剩下两个设置只有 Windows 用户需要进行.

第一是把 Emacs 的家目录, 即 Emacs 中通过 `~` 访问的目录, 改成 `C:/Users/<用户名>/`. 默认的家目录是 `C:/Users/<用户名>/AppData/Roaming/`. 从这个目录出发不方便我们访问像 "我的文档" 这种常用文件夹, 所以我们需要手动修改家目录为 `C:/Users/<用户名>/`, 与 Linux 和 MacOS 的使用习惯保持一致.

修改家目录的方法是在环境变量的设置中 (可以在 Windows 搜索栏中搜索 `Edit system variables` 打开), 增加一个用户的环境变量 `HOME`, 把它设置为 `C:/Users/<用户名>/`.

第二是保证你的 Emacs 安装目录在系统变量 `PATH` 上. 如果不在, 还是在同一个界面, 把包含你 `emacs.exe` 的文件夹路径手动添加到 `PATH` 变量中.


### Emacs 基本知识讲解与必知快捷键 {#emacs-基本知识讲解与必知快捷键}

作为 Emacs 新手, 大家需要理解的一个核心概念就是命令. 在 Emacs 中所有的操作, 无论简单复杂都是命令.
Emacs 这个单词就来自于 macro, 即宏命令. 比如说我们想执行打开一个文件的操作, 有3种方式等价的操作方式:

1.  在菜单栏里选择 `file` -&gt; `visit new file`, 然后和普通的软件一样选择你要打开的文件.
2.  执行 `open-file` 命令. 方法是按下 <kbd>M-x</kbd> (<kbd>M</kbd> = <kbd>Alt</kbd>), 然后在最下面的小缓冲区输入 `open-file`, 然后输入文件名.
3.  按下 <kbd>C-x C-f</kbd> 快捷键, 并输入文件名.

Emacs 中有许多有用的命令. 你未来也可以自己通过 Elisp 语言编写自己命令. Emacs 把其中最常用的命令都绑定了快捷键, 用户自己也可以设置自己的快捷键. 当我们用熟了之后, 很多快捷键就会像打字一样形成肌肉记忆. 作为新手, 我们有很多键盘的快捷操作可以用鼠标代替, 因此你不必急于一下子掌握全部 Emacs 的快捷键.

但是, 仍有一些快捷键是大家最好尽快熟悉的. 下面这张表我给大家总结了新手必知的几个快捷键.
在表中, <kbd>C</kbd> 表示 <kbd>Ctrl</kbd>, <kbd>M</kbd> 表示 <kbd>Alt</kbd>. 这也是 Emacs 快捷键通用写法. 最右边一列是快捷键对应的命令名, 也就是第一列所有的快捷键都等价于 <kbd>M-x</kbd> 加上第三列.

| 快捷键               | 操作         | 命令名                           |
|-------------------|------------|-------------------------------|
| <kbd>C-g</kbd>       | 中止当前一!切!命!令! | `keyboard-quit`                  |
| <kbd>C-/</kbd>       | 撤销命令     | `undo`                           |
| 文件操作             |              |                                  |
| <kbd>C-x C-f</kbd>   | 打开文件     | `find-file`                      |
| <kbd>C-x C-s</kbd>   | 保存文件     | `save-buffer`                    |
| <kbd>C-x b</kbd>     | 切换文件 (缓冲区) | `switch-to-buffer`               |
| <kbd>C-x 1</kbd>     | 关闭其它窗口 | `delete-other-window`            |
| <kbd>C-h f/v/k</kbd> | 查询命令/变量/快捷键 | `describe-function/variable/key` |
| 文本处理             |              |                                  |
| <kbd>M-x</kbd>       | 复制         | `kill-ring-save`                 |
| <kbd>C-w</kbd>       | 剪切         | `kill-region`                    |
| <kbd>C-y</kbd>       | 粘贴         | `yank`                           |


#### 中止命令与撤销命令 {#中止命令与撤销命令}

在 Emacs 中发生误操作时, 你需要知道如何中止与撤销命令. 当你的快捷键输入一半想反悔时 (是的, Emacs 的快捷键可以很长!), 可以使用 <kbd>C-g</kbd> 重新来输入, 又或者 Emacs 在执行命令时卡住了, 你可以通过 <kbd>C-g</kbd> 来让它恢复正常.

如果你需要撤回上一条命令, 则需要使用 <kbd>C-/</kbd>. 但值得注意的是, 撤回撤回命令的命令也是同一个键; 这偶尔会让人抓狂.


#### 文件与窗口相关命令 {#文件与窗口相关命令}

下面我们介绍 Emacs 中最基础的几个管理界面的快捷键.

首先是打开文件, <kbd>C-x C-f</kbd>, 命令名是 `find-file`. 这里的 `find` 隐含 Emacs 会根据不同情况执行不同操作: 若文件存在, 则是普通的打开文件; 若文件不存在, 则是打开一个新文件.

第二个是保存文件, <kbd>C-x C-s</kbd>, 对应 `save-buffer`, 即把当前缓冲区 (更新后) 的内容写进文件里.

大多数情况将缓冲区 (buffer) 等同于文件不会影响你的 Emacs 使用. 这里简单讲讲它们的不同. 文件存在于电脑硬盘上, 而 Emacs 的缓冲区只显示文件内容. 当你把文件内容读入缓冲区以后, 又在 Emacs 外修改了文件的内容, 缓冲区中的内容并不会改变, 除非你明确指示 Emacs 重新读取. 而在 Windows 中, 一个文件同时只能被一个 Windows 程序打开. Emacs 的缓冲区也不一定对应着文件, 在模式栏大家可以看到当前缓冲区的名字. 名字被两个 `*` 号包含的一般是非文件的缓冲区, 例如 `*Message*` 用于显示 Emacs 给用户的信息, 编译 LaTeX 时 `*Output*` 会存放编译输出结果等.

第三个命令是切换缓冲区/文件, <kbd>C-x b</kbd>, 对应 `switch-to-buffer`. 执行后在最下方的小缓冲区会提示输入你想要切换的缓冲区名字, 默认是上一个显示的缓冲区, 直接回车就行.

在 Emacs 中同时显示多个缓冲区的方法是打开多个窗口 (window), 然后在每个窗口中显示一个缓冲区. 有时 Emacs 自动创建新的窗口, 例如展示帮助信息时. 新手最常用的操作是保留当前光标所在窗口, 而关掉其它所有窗口. 这可以通过, <kbd>C-x 1</kbd>, 即 `delete-other-window` 实现. 我们可以用鼠标辅助我们在不同窗口间切换.


#### 帮助命令 {#帮助命令}

Emacs 中查询帮助信息的快捷键是 <kbd>C-h &lt;字母&gt;</kbd>. 常用的有 <kbd>C-h f</kbd>, 查询命令, <kbd>C-h v</kbd>, 查询变量, 以及 <kbd>C-h k</kbd>, 查询快捷键. 通常 <kbd>C-h</kbd> 命令会自动创建新的窗口显示帮助信息.  我们可以先把光标移到我们工作的缓冲区, 然后用 <kbd>C-x 1</kbd> 关闭掉帮助信息窗口. 注意此时帮助信息的缓冲区并没有关闭, 重新显示可以通过 <kbd>C-x b</kbd> 并查找以 `*help*` 命名的缓冲区.


#### 复制/剪切/粘贴 {#复制-剪切-粘贴}

Emacs 有自己一套复制/剪切/粘贴的快捷键: <kbd>M-w</kbd> / <kbd>C-w</kbd> / <kbd>C-y</kbd>. 这和一般程序的 <kbd>C-c</kbd> / <kbd>C-x</kbd> / <kbd>C-v</kbd> 不同, 需要大家习惯. 所有复制或剪切的内容都会进入一个叫 `kill-ring` 的地方, 它相当于一个剪粘版的历史记录. 粘贴快捷键 <kbd>C-y</kbd> 会粘贴最近一条记录, 如果你想访问之前的记录, 可以紧跟着 <kbd>C-y</kbd> 再按下一次或多次 <kbd>M-y</kbd>.


### Emacs 插件管理 {#emacs-插件管理}

接下来我们介绍如何更好地管理 Emacs 插件. Emacs 插件也叫 Emacs 包 (package). 插件可以给我们带来更多的功能, 是 Emacs 使用中不可缺少的一环. 插件的安装和设置与其它的 Emacs 设置一样, 都放在 Emacs 的启动文件 `~/.emacs.d/init.el` 中. 关于插件安装与设置, 我推荐大家使用现在常用的 `use-package` 语法, 它的语法更简洁, 还可以很方便地自动安装插件.

Emacs 中下载新的插件可以通过不同的方式 (这也是由某些插件提供的). 常用的有两种, 一种是用内置的 `package.el`, 这个插件名字就叫 `package.el`, `.el` 后缀来自于 Emacs 的编程语言 Elisp.
第二种是用 [Straight](https://github.com/radian-software/straight.el). `package.el` 会从官方的插件库 (ELPA, MELPA) 或镜像网站上下载新插件, 而 Straight 用下载插件的源代码并编译, 一般是利用 Git 从 Github 上下载. 为了使用 Straight, 你需要系统上已经安装了 Git 程序, 并且能正常地访问 `github.com`. 以下我们介绍两种安装方式如何设置.

我们在 `package.el` 和 Straight 的设置示例中都手动检查并安装了 `use-package`. Emacs 29 后 `use-package` 已经是内置插件, 相关代码可以省去.


#### `package.el` 设置示例 {#package-dot-el-设置示例}

以下我们提供了 `package.el` 的一个设置示例. 大家需要把如下代码放入设置文件 `./.emacs.d/init.el` 中.

```elisp
;; -*- lexical-binding: t; -*-
;; 静态作用域声明必须放在首行
;; 把 Emacs 自动添加的代码放到 custom.el 中
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;========================================
;; 使用 package.el 设置 Emacs 插件管理
;;========================================
(require 'package) ; 加载 package.el
(setq package-check-signature nil) ; 如果检查签名有问题可以加入这一行
;; 添加仓库位置
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; 国内用户也可以使用清华的镜像网站. 用下面的代码代替上面两行
;; (setq package-archives
;;       '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
;;         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; 刷新插件列表
(unless package-archive-contents
  (package-refresh-contents))
;; 自动安装 use-package. 在Emacs 29中已内置故可省略
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; 自动安装所有使用 use-package 声明的插件
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;;========================================
;; Emacs 插件管理设置完毕
;;========================================


;;========================================
;; 这段代码放在最后, 加载 Emacs 自动设置的变量
(if (file-exists-p custom-file) (load-file custom-file))
;;========================================
```

这段代码的第一部分启用了 `package.el`, 然后通过 `package-archives` 变量设置了下载插件的网址. 在国内也可以使用清华的软件源. 接下来 `package-refresh-contents` 刷新了插件列表. 然后我们自动检测 `use-package` 是否安装, 如果没有安装则自动下载安装. 最后, 我们设置了 `use-package-always-ensure` 变量为 `t`, 这样以后我们所有用 `use-package` 声明的插件都会自动安装.


#### `straight.el` 设置示例 {#straight-dot-el-设置示例}

`straight` 需要用 `git` 从 `github` 等网站上下载源码. 请再三确认 `git` 在系统路径上 (尤其是用 `msys2` 安装的 Windows 用户).

因为 `straight.el` 与 `package.el` 难以共存, 所以我们必须早早手动禁用内置的 `package.el`. 这必须修改一个我们平时很少用的文件 `.emacs.d/early-init.el`. 我们需要在 `early-init.el` 中加入

```elisp
;; 在执行 init.el 前禁用 package.el
(setq package-enable-at-startup nil)
```

接下来, 我们需要在 `init.el` 中加入以下代码:

```elisp
;; -*- lexical-binding: t; -*-
;;========================================
;; 把 Emacs 自动添加的代码放到 custom.el 中
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; 使用 straight.el 设置 Emacs 插件管理
;;========================================
(defvar bootstrap-version)
;; 修复 Emacs 29 修改了 native-compile 相关变量导致的 bug
(unless (version<= emacs-version "28.2")
  (setq straight-repository-branch "develop"))
;; 以下代码从 straight.el 主页 https://github.com/radian-software/straight.el 上复制
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; 用 straight.el 安装 use-package 声明的插件
(setq straight-use-package-by-default t) ; 自动安装所有插件, 相当于加入 :straight t
;;========================================
;; Emacs 插件管理设置完毕
;;========================================

;;========================================
;; 这段代码放在最后, 加载 Emacs 自动设置的变量
(if (file-exists-p custom-file) (load-file custom-file))
;;========================================
```

这里大部分的代码是 `straight.el` 的 [Github主页](https://github.com/radian-software/straight.el)上提供的下载与安装 `straight` 的代码, 然后再用 `straight` 安装 `use-package`. 最后我用把 `straight-use-package-by-default` 变量设为 `t`, 这是在使用 `straight` 时进行插件自动安装的设置.


### 推荐插件 {#推荐插件}

下面我给大家推荐两组非常实用的插件. 在进行好 `package.el` 或者 `straight.el` 的设置后 (注意: 不能同时使用), 安装与设置插件只需要把相关的 `use-package` 代码块复制到 `init.el` 即可. 而且在两个体系下的代码块基本是通用的.

我们之前也都设置了自动安装插件. 当你第一次执行 `init.el` 时 (通常是第一次重启 Emacs 的时候), Emacs 会自动检测你在 `init.el` 中声明的插件是否已经安装, 若没有则通过指定的方法 (`package.el` 或 `straight.el`) 自动下载安装. 如果大家在一台新的机器上使用 Emacs, 把 `init.el` 文件复制到新机器上就可以直接获得一模一样的使用体验!

你也可以在修改完 `init.el` 后, 执行 <kbd>M-x</kbd> `eval-buffer` 命令手动加载新加的 `use-package` 代码块.

在复制代码块中最常见的问题是某个地方在复制的过程中漏了括号. 大家已经发现 elisp 语言中括号是必须配对的. 我们可以在修改 `init.el` 后手动的用 <kbd>M-x</kbd> `match-paren` 检查括号是否匹配. 如果有不匹配的括号, 那么光标就会跳过没有匹配成功的括号上, 否则这个命令不会用任何效果.


#### 插件组合1: 更多的帮助信息 {#插件组合1-更多的帮助信息}

我们首先介绍 [Which-key](https://github.com/justbur/emacs-which-key) + [Keycast](https://github.com/tarsius/keycast) + [Helpful](https://github.com/Wilfred/helpful). 安装代码如下

```elisp
(use-package which-key
  :custom (which-key-idle-delay 0.5) ; 延迟时间, 以秒为单位
  :config (which-key-mode)) ; 启用 which-key 模式

(use-package keycast
  :config (keycast-header-line-mode 1)) ; 在标题显示

(use-package helpful
  :bind
  ;; 重新定向 C-h 开始的命令
  (([remap describe-function] . #'helpful-callable)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-key] . #'helpful-key)
   ([remap describe-command] . #'helpful-command)
   ([remap describe-symbol] . #'helpful-symbol)
   ("C-h C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)))
```

`which-key` 可以在按下快捷键的时候自动提示你接下来可能的快捷键. 比如按下 <kbd>C-h</kbd>, 就会提示接下来你按 <kbd>v</kbd>, <kbd>f</kbd>, <kbd>k</kbd> 等可以查看哪种类型的帮助. 如果把鼠标悬停在选项上也会在浮窗中显示对应命令的帮助.

`keycast` 则会显示当前你使用的快捷键及对应的命令名. 它有4种显示的位置. 代码中我们选择在 `headrer-line` , 也就是 Emacs 窗口的最上方显示. 如果大家想在别的地方显示, 比如模式栏, 可以把 `(keycast-header-line-mode 1)` 改成 `(keycast-mode-line-mode 1)`.

这两个插件可以帮助新手快速熟悉 Emacs 的快捷键和命令. 对于老用户来说, 也可以帮你快速熟悉新安装的插件. 我自己平时也是常开的.

`helpful` 则优化了帮助界面的信息显示, 包括更多有用的信息和高亮.


#### 插件组合2: 更好的补全界面 {#插件组合2-更好的补全界面}

在 Emacs 中输入命令或打开文件, 切换缓冲区等等都会用到小缓冲区补全. 第二组插件 [Vertico](https://github.com/minad/vertico) + [Marginalia](https://github.com/minad/marginalia) + [Orderless](https://github.com/oantolin/orderless) 是针对小缓冲区补全的. 代码如下

```elisp
(use-package vertico ; 竖式展开小缓冲区
  :custom (verticle-cycle t)
  :config (vertico-mode))

(use-package marginalia ; 更多信息
  :config (marginalia-mode))

(use-package orderless ; 乱序补全
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
```

`vertico` 把每个补全选项放在单独的一行, 配合 `marginalia` 会在每个选项的右边加入更多相关信息.

在小缓冲区中输入时, 我们可以按 <kbd>Tab</kbd> 补全当前的输入. 加入 `vertico` 之后, 我们可以用 <kbd>C-n</kbd> 和 <kbd>C-p</kbd> 或者上下移动键来选择不同的补全选项. <kbd>C-n</kbd> 和 <kbd>C-p</kbd> 也是 Emacs 中上下移动光标的快捷键.

最后的 `orderless` 允许我们在小缓冲区补全时忽略单词的顺序. 例如, 如果我们输入 <kbd>M-x</kbd>, 想要匹配 `find-file` 命令, 在默认情况下必须先输入 `find`, 再输入 `file` 才能找到 `find-file`. 如果你用了 `orderless`, 则可以通过 `file find` 找到, 或者部分的单词 `fil fin` + ~Tab~= 找到.


### 基本的 `CDLaTeX` + `AucTeX` 设置 {#基本的-cdlatex-plus-auctex-设置}

最后我们提供一个简单可用的 `CDLaTeX` 和 `AucTeX` 设置. 大家只要把这段代码复制进 `init.el` 就可以实现视频[五分钟说服你用Emacs写LaTeX](https://www.bilibili.com/video/BV1Xk4y1a7Gp/)中的大部分功能.

```elisp
(defun my/latex-hook ()
  (turn-on-cdlatex)
  (turn-on-reftex))
(use-package cdlatex
  :load-path "lisp/" ; 需要手动从网盘或 https://github.com/cdominik/cdlatex/blob/master/cdlatex.el 下载 cdlatex.el 文件, 并置于 ~/.emacs.d/lisp/ 文件夹下
  ;; 若使用 straight, 注释前一行, 并取消下一行注释:
  ;; :straight (:host github :repo "cdominik/cdlatex" )
  )
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
  (add-hook 'LaTeX-mode-hook 'my-latex-hook)) ; 加载LaTeX模式设置
```

这个基本设置不一定能实现 PDF 正向或反向搜索, 因为这取决于操作系统与 PDF 阅读器. 如果你想在不同平台上使用 Emacs 并获得统一的 PDF 体验, 可以考虑使用 PDF-tools (见视频 [BV1pg4y1s7Z9](https://www.bilibili.com/video/BV1pg4y1s7Z9/)).

关于 CDLaTeX 的安装, 要注意的是, 它并不在任何的软件源时. 如果使用 `package.el`, 你需要手动下载这个文件 ([Github链接](https://github.com/cdominik/cdlatex/blob/master/cdlatex.el) 或者我网盘里的备份), 并用 <kbd>:load-path</kbd> 关键字指定文件的目录. 如果是 Straight, 则需要我们指定 Github 仓库的地址 `cdominik/cdlatex`.

AucTeX 是通过 `(use-package tex)` 激活的. 因为包名不统一的问题, 我们要额外加入 `:ensure auctex` 或 `:straight auctex`. 其实 Emacs 已经内置了 AucTeX, 但不一定是最新版本, 我们这里的 `use-package` 则会把它更新到最新版.


### 有用的链接 {#有用的链接}

-   Emacs 官网: <https://www.gnu.org/software/emacs/>
-   我的坚果云分享: <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0E>
-   我的 Emacs 设置: <https://gitee.com/mickey991/emacs-config.git>


## CDLaTeX 中快速输入数学符号和字体与自定义设置 {#easy-latex-writing-ep02-math-symbol-and-modify}

大家好, 我是小米, 欢迎大家来到我的省时省力写 LaTeX 系列. 本期我们开始介绍 Emacs 的 CDLaTeX 插件. 这次讲解如何使用 CDLaTeX 快速插入数学字母, 符号和字体的功能, 以及如何自定义新的快捷键.


### AucTeX 和 CDLaTeX 基本设置 {#auctex-和-cdlatex-基本设置}

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


### 数学符号输入 {#数学符号输入}

这里的数学符号也包括各种非拉丁字母如 `\alpha`, `\aleph` 等. 输入方法是用反引号 (<kbd>Tab</kbd> 上方) 加另一个键组成的快捷键输入.


#### 插入希腊字母 {#插入希腊字母}

希腊字母可以用 <kbd>\`</kbd> + 对应拉丁字母插入, 包括大小写. 例如

-   <kbd>\`</kbd> + <kbd>a</kbd>: `\alpha`
-   <kbd>\`</kbd> + <kbd>b</kbd>: `\beta`
-   <kbd>\`</kbd> + <kbd>g</kbd>: `\gamma`
-   <kbd>\`</kbd> + <kbd>G</kbd>: `\Gamma`
-   <kbd>\`</kbd> + <kbd>S</kbd>: `\Sigma`

如果你不熟悉希腊字母对应的拉丁字母, 没有关系, 只要在按下 <kbd>\`</kbd> 后稍稍停顿, 就会弹出一个提示界面.
大家刚开始使用时可以多查看这个提示界面.


#### 数学符号 {#数学符号}

大家在提示界面可以看到, 除了希腊字母以外, 我们还可以用同样的方法快速插入数学符号. CDLaTeX 预置了很多好记的默认设置. 例如, <kbd>\`</kbd> + <kbd>8</kbd> 插入 `\infty`, 因为数字8放平就是无穷, 又如, <kbd>\`</kbd> + <kbd>*</kbd> 插入 `\times` 乘号, <kbd>\`</kbd> + <kbd>+</kbd> 插入 `\cup` (并集), <kbd>\`</kbd> + <kbd>&gt;</kbd> 插入 `\rightarrow` (右箭头) 等.


#### 第二和第三层目录 {#第二和第三层目录}

`CDLaTeX` 中连续按下两次反引号 <kbd>\`</kbd> 可以打开第二层目录. 第二层通常用于希腊字母的变体, 如

-   <kbd>\`e</kbd> 插入 `\epsilon`, <kbd>\`\`e</kbd> 插入 `\varepsilon`
-   <kbd>\`r</kbd> 插入 `\rho`, <kbd>\`\`r</kbd> 插入 `\varrho`

又或者是一些类似的符号, 如

-   <kbd>\`&gt;</kbd> 插入 `\rightarrow`, <kbd>\`\`&gt;</kbd> 插入 `\longrightarrow`

或者是多个符号最直观的快捷键相同, 但是频率最高的放在第一层, 频率低的放在第二层, 如

-   <kbd>\`d</kbd> 插入 `\delta`, <kbd>\`\`d</kbd> 插入 `\partial` (求偏导符号)

这个目录还有第3层, 这里绑定的快捷键就更少了. 默认的是一些数学函数的符号, 如 `\sin`, `\exp` 等


#### 如何插入 LaTeX 左双引号 `` `` `` {#如何插入-latex-左双引号}

反引号在 LaTeX 中写作几乎不会用到, 除了用于左双引号 <kbd>\`\`</kbd> (laTeX 的右双引号是 `''` ). 这很好解决: 在 AucTeX 默认设置下, 第一个输入的双引号 <kbd>"</kbd> 会自动转换成为 <kbd>\`\`</kbd> 插入, 第二个输入的双引号 <kbd>"</kbd> 会转换为 <kbd>\'\'</kbd> . 例如, <kbd>"word"</kbd> 将插入 <kbd>``word\'\'</kbd>.

当然, 你也可以把反引号修改成其它的键, 但是既然无须担心双引号输入的问题, 我觉得改的意义不大. 反引号已经是很好的选择.


### 自定义数学符号快捷键 {#自定义数学符号快捷键}

Emacs 的最大优势就是我们可以自由地设置. 前面反引号 <kbd>\`</kbd> 触发的快捷输入, 我们也可以添加自己需要的符号或调整已有的设置.

这里的所有设置保存在一个叫 `cdlatex-math-symbol-alist` 的变量中. 我们接下来讲解在 Emacs 如何设置一个变量, 保存设置以及加载设置. 这对其它的变量也是一样.


#### 打开设置界面 {#打开设置界面}

虽然所有的变量设置都可以通过 `init.el` 里面的 `(setq ...)` 语句完成, 对于 `cdlatex-math-symbol-alist` 这种结构非常复杂的变量, 新手还是建议用 Emacs 自带的设置界面.

打开一个变量的设置界面主要有两种方式 (以 `cdlatex-math-symbol-alist` 为例)

1.  通过 `customize-variable` 命令:
    <kbd>M-x</kbd> `customize-variable`  <kbd>RET</kbd> <kbd>M-x</kbd> `cdlatex-math-symbol-alist`
2.  从变量的帮助界面进入设置界面:
    <kbd>C-h v</kbd> `cdlatex-math-symbol-alist`  并点击 `customize`


#### 设置实例 {#设置实例}

我们想调换 <kbd>\`e</kbd> 和 <kbd>\`\`e</kbd> 原本的快捷键设置, 即实现如下效果:  <kbd>\`e</kbd> 插入 `\varepsilon`, <kbd>\`\`e</kbd> 插入 `\epsilon`. (这么做的原因是 `\varepsilon` 更常用).

1.  打开 `cdlatex-math-symbol-alist` 的设置界面
2.  点击 <kbd>INS</kbd> 插入一个新条目
3.  在 `character` 后输入 `e`
4.  在 `Repeat` 后按 <kbd>INS</kbd>, 新插入的一行输入 `\varepsilon`
5.  在 `Repeat` 后按 <kbd>INS</kbd>, 新插入的一行输入 `\epsilon`

这就完成了基本设置. 如果大家想绑定 <kbd>\`\`\`e</kbd> 和 <kbd>\`\`\`\`e</kbd> 等, 只需要再加入新的行以及你需要的 LaTeX 宏命令即可.

这里因为 <kbd>\`e</kbd> 已经在 `CDLaTeX` 的默认设置中, 所以我们是覆盖了原有设置. 你可以在一开始的按下 <kbd>\`</kbd> 的提示界面中看到默认设置, 或者通过查看变量 `cdlatex-math-symbol-alist-default`.


#### 保存与加载设置 {#保存与加载设置}

设置完毕我们会点击 `Apply and Save`.

-   `Apply`: 改变了当前 `cdlatex-math-symbol-alist` 的値, 重启 Emacs 后失效
-   `Save`: 保存设置, 重启后仍生效.

但是已经打开的 `tex` 文件是看不到更新的设置的. 想要重新加载 `CDLaTeX` 的设置. 这有3种方法:

1.  重启 Emacs
2.  一个是打开新的 `tex` 文件
3.  在原来的 `.tex` 文件缓冲区, 按下 <kbd>C-c C-n</kbd>.

第三种方法可以刷新 LaTeX 模式设置, 也适用于其它与 `cdlatex` 的设置. 此时, 大家按下反引号 <kbd>\`</kbd> 就可以看到更新后的列表了.


#### 怎么选择快捷键 {#怎么选择快捷键}

原则上这个机制可以插入任意的数学表达式, 如 `\stackrel{\mathrm{a.s.}}{=`}=, 但是建议只绑定原子化的数学符号. 复杂的表达式更适合用 CDLaTeX 的命令补全功能. (参考 [Tab 补全快速插入LaTeX代码]({{< relref "easy-latex-writing-ep03-tab-completion" >}}))

快捷键要易记, 直观, 凭你的第一感觉就能找到. 否则不能提高输入速度. 大家也可以查看默认的设置寻找灵感. 反例就是把左箭头 `\leftarrow` 绑到 <kbd>\`&gt;</kbd> 上.

如果一个键上绑定了多层快捷键, 要考虑不同命令使用的频率, 把最常用的放在第一层, 次常用的放在第二层, 依此类推. 像上面的 `\epsilon` 和 `\varepsilon` 的例子.

你也可以绑定自己定义的宏命令. 例如, 我的 <kbd>\`e</kbd> 绑定的是 `\eps`, 而在我的 LaTeX 文档引言区中会定义 `\newcommand{\eps}{\varepsilon}`. 这样的好处可以提高代码的可读性, 方便交流. 毕竟你的导师, 你的合作者未必用 Emacs, 长长的 `\varepsilon` 会让人眼花. 但是我输入时想到的是希腊字母 epsilon 就应该用 <kbd>\`e</kbd> 输入.

这里有很大的发挥空间, 因为第二层和第三层基本都是空的, 每个键还分大小写, 可以自由设置100多个快捷键. 所以尽情发挥吧.


### 数学字体修饰 {#数学字体修饰}


#### 数学字体 {#数学字体}

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


#### 其它修饰 {#其它修饰}

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


#### 嵌套修饰 {#嵌套修饰}

触发字体修饰的第三种方法是选先高亮选中一段文本, 再选择修饰. 例如, 选中数学环境外的 `blabla`, 然后按 <kbd>'b</kbd>, 则 `blabla` 会变成 `\textbf{blabla}`. 如果 `blabla` 在数学环境内, 则变成 `\mathbf{blabla}`

第一种方法只能修饰一个字母, 所以嵌套修饰只能使用第二种或第三种方法. 例:

-   <kbd>\'-\'bR</kbd> 插入 `\bar{\mathbf{R}}`.
-   <kbd>R\'b</kbd> 插入 `\mathbf{R}`, 然后高亮选中按下 <kbd>\'-</kbd>, 变成 `\bar{\mathbf{R}}`


### 自定义字体修饰 {#自定义字体修饰}

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


### `customize-variable` 设置保存位置 {#customize-variable-设置保存位置}

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


### 总结 {#总结}

Emacs 中的 CDLaTeX 插件利用反引号 <kbd>\`</kbd> 和单引号 <kbd>\'</kbd> 开始的快捷键可以快速插入数学字母, 符号和字体. 我们可以通过设置 `cdlatex-math-symbol-alist` 和 `cdlatex-math-modify-alist` 这两个变量修改和增加自己喜欢的快捷键.

在下期视频中我们将介绍 CDLaTeX 中 <kbd>Tab</kbd> 的命令/模板补全功能. 它可以帮助我们输入一些更复杂的宏命令, 或者插入环境模板等等.


## Tab 补全快速插入 LaTeX 代码 {#easy-latex-writing-ep03-tab-completion}

大家好, 我是小米. 本期我们将介绍如何在 CDLaTeX 中用 `Tab` 补全命令快速地输入复杂的宏命令和环境模板.


### Tab 补全插入宏命令 {#tab-补全插入宏命令}

补全原理很简单, 用几个字母组合加 <kbd>Tab</kbd> 生成一些复杂的命令. 例如, `fr` + <kbd>Tab</kbd> 就会生成 `\frac{}{}`, 这里光标会停留在第一个括号内; 在第一个括号内完成输入后, 按 <kbd>Tab</kbd> 光标就会跳到下一个括号中. 因此, 输入一个常见的分数 `\frac{1}{2}` 只需要输入 <kbd>f</kbd> + <kbd>r</kbd> + <kbd>Tab</kbd> + <kbd>1</kbd> + <kbd>Tab</kbd> + <kbd>2</kbd>.


#### 内置命令举例 {#内置命令举例}

CDLaTeX 内置了一些可补全的命令, 可以在 `cdlatex-command-alist-default` 变量中查看 (<kbd>C-h v</kbd>). 我们举一些例子 (以下 `?` 所在位置表示补全后光标停留的位置.)

-   分数 `fr` + <kbd>Tab</kbd> = `\frac{?}{}`, 根号 `sq` + <kbd>Tab</kbd> = `\sqrt{?}`
-   空格 `qq` + <kbd>Tab</kbd> = `\quad`, 大空格 `qqq` + <kbd>Tab</kbd> = `\qquad`
-   括号 `lr(` + <kbd>Tab</kbd> = `\left(?\right)`, `lr[` = `\left[?\right]`
-   章节标题 `sn` + <kbd>Tab</kbd> = `\section{?}`, `ss` + <kbd>Tab</kbd> = `\subsection{?}`, `sss` + <kbd>Tab</kbd> = `\subsubsubsection{?}`


#### 一些自定义例子 {#一些自定义例子}

-   `te` + <kbd>Tab</kbd> = `\text{}`
-   `se` + <kbd>Tab</kbd> = `\{ \}` (set)
-   `st` + <kbd>Tab</kbd> = `\stackover{}{}`
-   `hl` + <kbd>Tab</kbd> = `\hline`, `hhl` + <kbd>Tab</kbd> = `\\ \hline` (表格中常用)
-   `big(` + <kbd>Tab</kbd> = `\big(?\big)`, `Big(` + <kbd>Tab</kbd> = `\Big(?\Big)`, `bigg(` + <kbd>Tab</kbd> = `\bigg(?\bigg)` (`\big`, `\Big`, `\bigg` 等是 `amsmath` 中调整括号大小的命令)
-   `lr<` + <kbd>Tab</kbd> = `\langle?\rangle`, 一对尖括号 \\(\langle  \rangle\\).

显然, 这里的关键字选择都是用命令中最开始的两到三个字母, 这样非常好记, 也很容易使用.


### Tab 补全环境模板 {#tab-补全环境模板}

大家可以看到这里的 `Tab` 补全其实就是一个替换字符串的过程. 当然字符串中也可以包括换行, 因此同样的机制也可以输入形如 `\begin{XXX} ... \end{XXX}` 的环境.


#### 内置命令举例 {#内置命令举例}

<!--list-separator-->

-  `equation` 环境

    `equ` + <kbd>Tab</kbd> 插入如下模板:

    ```latex
    \begin{equation}
      \label{eq:NNN}
      ?
    \end{equation}
    ```

    其中, `\label{eq:XXX}` 是 `CDLaTeX` 调用 `reftex` 自动生成的数字标签.

    类似的数学公式环境还有如

    -   `ali` + <kbd>Tab</kbd> 插入 `align` 环境 (自动生成标签), `ali*` + <kbd>Tab</kbd> 插入 `align*` 环境 (无标签)
    -   `gat` + <kbd>Tab</kbd> 插入 `gather` 环境 (自动生成标签), `gat*` + <kbd>Tab</kbd> 插入 `gather*` 环境 (无标签)

<!--list-separator-->

-  列表环境

    `enu` + <kbd>Tab</kbd> 插入

    ```latex
    \begin{enumerate}
    \item
    \end{enumerate}
    ```

    此时, 在 `enumerate` 环境中:

    -   `it` + <kbd>Tab</kbd> = `\item`
    -   <kbd>C-&lt;enter&gt;</kbd> 会换行并生成 `\item`

    这里, `enu` + <kbd>Tab</kbd> 等同于用 `cdlatex-environment` (<kbd>C-c {</kbd> ) 插入 `enumerate` 环境

    类似的还有

    -   `ite` + <kbd>Tab</kbd> 插入 `itemize` 环境
    -   `fg` + <kbd>Tab</kbd> 插入 `figure` 环境


### 自定义补全命令 {#自定义补全命令}

现在我们介绍如何自定义你自己需要的补全命令. 默认的补全命令都在 `cdlatex-command-alist-default` 中, 而现有的所有命令, 包括内置的和自定义的, 都可以通过 <kbd>C-c ?</kbd> 查看.

在用 <kbd>C-c ?</kbd> 查看时, 我们会在最右一列看到 `TEXT` 和 `MATH` 关键字:

-   `MATH` 关键字表示补全可以在 **数学环境** 中触发
-   `TEXT` 关键字表示补全可以在 **文本环境** 中触发

加入自定义新的补全命令通过修改变量 `cdlatex-command-alist`. 方法是调用 <kbd>M-x</kbd> `customize-variable =, 然后输入变量名 =cdlatex-command-alist`.


#### 带参数的宏命令 {#带参数的宏命令}

例子: `te` + <kbd>Tab</kbd> 输入 `\text{?}` (光标停在括号内). 我们需要填入如下参数

-   keyword: `te`
-   Docstring: 随便填, 只是用于说明的解释性文字, 例如 `insert \text{}`
-   Replacement: `\text{?}` (`?` 表示光标停留的位置)
-   Hook: `cdlatex-position-cursor` (如果需要指定光标则必填!)
-   Argument: `nil` (这是上面 hook 的参数)
-   Text Mode: `nil`, Math mode: `t`

保存设置 (`Apply and Save`) 之后, 在已经打开的 `tex` 文件中用 <kbd>C-c C-n</kbd> 可以刷新设置, 就可以开始使用了.


#### 插入匹配的括号 {#插入匹配的括号}

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


#### 插入环境 {#插入环境}

例子: `case` + <kbd>Tab</kbd> 插入

```latex
\begin{cases}
? & \\
 &
\end{cases}
```

-   keyword: `case`
-   Docstring: `insert \begin{cases} \end{cases}`
-   Replacement: 输入框内用 <kbd>C-j</kbd> 换行, 然后正常输入需要替换的文本即可
-   Hook: `cdlatex-position-cursor`
-   Argument: `nil`
-   Text Mode: `nil`, Math mode: `t`


#### 插入环境 II {#插入环境-ii}

插入环境除了直接在 `cdlatex-command-alist` 的 `Replacement` 中写入环境模板以外, 还可以通过调用函数 `cdlatex-environment` 的方式实现.
在 LaTeX 编辑模式中, 有两种用环境名插入环境的方法

-   <kbd>M-x</kbd> `LaTeX-environment` (<kbd>C-c C-e</kbd>) + `description`: 这会调用 `AucTeX` 的环境模板
-   <kbd>M-x</kbd> `cdlatex-environment` (<kbd>C-c {</kbd> ) + `description`: 这会调用 `CDLaTeX` 的环境模板.

两种模板略有不同. 这第二种插入环境的方法就是用 <kbd>Tab</kbd> 补全触发第二个命令.

例子: `des` + <kbd>Tab</kbd> 插入 `description` 环境

```latex
\begin{description}
\item[?]
\end{description}
```

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


### 我的一些设置分享 {#我的一些设置分享}

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


## Emacs 最强内置 pdf阅读器 pdf-tools 简介 {#easy-latex-writing-ap01-pdf-tools}


### 使用 `pdf-tools` 的理由 {#使用-pdf-tools-的理由}

在用 Emacs 编写 LaTeX 文档的过程中, 你是否...

-   预览 pdf 需要来回在编辑器和 pdf 阅读器之间切换?
-   pdf 阅读器想实现一些新功能?
-   想给 pdf 阅读器的常用功能定义新的快捷键?

又或者, 你想用 Emacs 做读书笔记, 需要同时:

-   输入大量的数学符号
-   对 pdf 文件进行批注
-   同步 Emacs 笔记文件和 pdf 文件批注的位置

`pdf-tools` 可以完美实现这些目标.


### `pdf-tools` 的优点 {#pdf-tools-的优点}

与 `DocView` (Emacs 中内置的 pdf 阅读器) 比较

-   `DocView`: 不清晰, 阅读效果差, 读取速度慢
-   `pdf-tools`:
    -   速度快, 图片渲染效果好
    -   正常鼠标操作 + 大量 (可自定义) 快捷键


### 演示 {#演示}


#### 功能: {#功能}

-   基础的 pdf 阅读功能应有尽有, 包括超链接跳转和返回, 展开目录等
-   与 `auctex` 配合使用, 支持对编译后 pdf 进行正向/反向搜索
-   pdf 批注, 高亮, 下划线 (可保存在 pdf 文件上)


#### 使用场景 {#使用场景}

-   编写 `latex` 文档
-   配合 `org-noter` 在 pdf 上做读书笔记


### 安装流程 {#安装流程}

分为两部分


#### Emacs 包的安装 {#emacs-包的安装}

-   保证 `melpa-stable` 在 Emacs 包的列表中
    可以通过查看 `package-archives` 变量进行确认
    ```elisp
    (require 'package) ;; Emacs 包管理器
    (setq package-check-signature nil) ;; 如果有签名验证问题, 可以设置不检查签名
    (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                             ("melpa-stable" . "https://stable.melpa.org/packages/") ;; 下载 pdf-tools 只需要这个
                             ("melpa" . "https://melpa.org/packages/")
                             ("gnu" . "http://elpa.gnu.org/packages/")))
    ```
-   用 <kbd>M-x package-list-package</kbd> 打开 Emacs 包的列表
-   用 <kbd>C-s pdf-tools</kbd> 找到 `pdf-tools`
-   安装 `melpa-stable` 版本  (2023.3: `melpa` 版本仍有 bug)


#### `epdfinfo.exe` 的安装 {#epdfinfo-dot-exe-的安装}

`epdfinfo.exe` 及其它一些依赖文件 (例如 <kbd>libpopper-&lt;version&gt;.dll</kbd>) 可以帮助 Emacs 读取 pdf 文件

两种方法

-   把预编译好的文件直接放进 Emacs 的安装目录 (将上传一个可用的版本:   <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>)
-   利用 `msys2`


### 用 `msys2` 安装 `epdfinfo` {#用-msys2-安装-epdfinfo}


#### 什么是 `msys2`? {#什么是-msys2}

可以将许多开源程序本地化编译为 Windows 程序的平台

优点

-   软件管理和升级方便
-   Emacs 一些高阶功能依赖的不少开源程序都能在上面下载
-   其它可以安装的开源软件:
    `Git`, `Emacs`, `texlive`, `gcc`, `python` ...


#### 步骤 {#步骤}

-   到 `msys2` 官网上 <https://www.msys2.org/> 下载安装程序 `msys2-x86_x64-<date>.exe`. 默认安装目录为 `C:/msys64/`.
-   打开 `C:/msys64/` 下 `mingw64.exe`. 会弹出一个命令行终端
-   在命令行终端中输入
    ```sh
    pacman -S mingw-w64-x86_64-emacs-pdf-tools-server
    ```
    以上命令可以在[这里](https://packages.msys2.org/package/mingw-w64-x86_64-emacs-pdf-tools-server?repo=mingw64)找到.
-   确认并安装所有依赖包.
-   安装完成后, 你应该能在 `C:\msys64\mingw64\bin` 中找到 `epdfinfo.exe`.
-   将 `C:\msys64\mingw64\bin` 加入环境变量 `PATH`


### 基本配置 {#基本配置}


#### 启动 `pdf-tools` {#启动-pdf-tools}

在 `init.el` 文件中加入

```elisp
(pdf-tools-install)
```

如果想延迟启动 (如打开 pdf 文件后再启动, 节省 Emacs 启动时间), 可以用下面的代码替换

```elisp
(pdf-loader-install)
```


#### 配合 `AucTeX` 使用的配置 {#配合-auctex-使用的配置}

保持不变的设置

```elisp
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
(setq TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
(setq TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索
```

使用 `Sumatra PDF` 的配置

```elisp
(setq TeX-view-program-list
 '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o"))))
(assq-delete-all (quote output-pdf) TeX-view-program-selection)
(add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")
```

`pdf-tools` 的配置

```elisp
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用pdf-tools 打开 pdf
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer) ;; 在完成编译后刷新 pdf 文件
```


### 操作与个性化: 移动 {#操作与个性化-移动}

-   向下/上小滑动: 鼠标滚轮, <kbd>C-n</kbd> / <kbd>C-p</kbd>
-   向下/上大滑动: <kbd>&lt;space&gt;</kbd> / <kbd>S-&lt;space&gt;</kbd>
-   向后/前翻页: <kbd>n</kbd> / <kbd>p</kbd>

我的设置: 尽量把移动绑定在左手 (<kbd>awsd</kbd>), 空出右手进行鼠标操作.

```elisp
(define-key pdf-view-mode-map
  "d" 'pdf-view-next-page-command) ;; 向后翻页
(define-key pdf-view-mode-map
  "a" 'pdf-view-previous-page-command) ;; 向前翻页
(define-key pdf-view-mode-map
  "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
(define-key pdf-view-mode-map
  "w" 'pdf-view-scroll-down-or-previous-page) ;; 向上滑动
```


### 操作与个性化: 批注 {#操作与个性化-批注}

-   高亮: 右键菜单, 或 <kbd>C-C C-a h</kbd> (h=highlight)
-   直线下划线: 右键菜单, 或 <kbd>C-c C-a u</kbd> (u=underline)
-   波浪下划线: 右键菜单, 或 <kbd>C-c C-a s</kbd> (s=squiggly)
-   文字批注: 右键菜单, 或 <kbd>C-c C-a t</kbd> (t=text)
-   删除批注: 右键菜单, 或 <kbd>C-c C-a D</kbd> (d=delete)

我的设置:

```elisp
(require 'pdf-annot)
(define-key pdf-annot-minor-mode-map (kbd "C-a a") 'pdf-annot-add-highlight-markup-annotation) ;; 高亮
(define-key pdf-annot-minor-mode-map (kbd "C-a s") 'pdf-annot-add-squiggly-markup-annotation) ;; 波浪线
(define-key pdf-annot-minor-mode-map (kbd "C-a u") 'pdf-annot-add-underline-markup-annotation) ;; 下划线
(define-key pdf-annot-minor-mode-map (kbd "C-a d") 'pdf-annot-delete) ;; 删除
```


### 操作与个性化: 文档跳转 {#操作与个性化-文档跳转}

-   展示目录: <kbd>o</kbd>
    -   跳到目录位置: <kbd>&lt;enter&gt;</kbd> / <kbd>M-&lt;enter&gt;</kbd>
-   关闭目录: <kbd>q</kbd>
-   返回上一个位置: <kbd>l</kbd>
-   跳到下一个位置: <kbd>r</kbd>

这里重新绑定常用的返回功能 (小知识: 在 `Sumatra PDF` 里对应 <kbd>Alt-&lt;right&gt;</kbd>)

```elisp
(require 'pdf-history)
(define-key pdf-history-minor-mode-map "b" 'pdf-history-backward)
```


### 操作与个性化: 放缩 {#操作与个性化-放缩}

-   放大/缩小: <kbd>+</kbd> / <kbd>-</kbd>
-   放大到页宽/页高/屏幕: <kbd>W</kbd> / <kbd>H</kbd> / <kbd>P</kbd>
-   重置: <kbd>0</kbd>

打开 pdf 文件时自动放缩

```elisp
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 自动放大到页宽
```


### 其它可能出现的 bug {#其它可能出现的-bug}


#### 无法进行高亮/划线等 {#无法进行高亮-划线等}

这可能是安装了 2023 年后 `pdf-tools` 的版本导致的. 可以从 <kbd>M-x package-list-package</kbd> 界面中确认是从 `melpa-stable` 中安装的


#### 形同 `(invalid-function pdf-view-current-page)` 的错误信息 {#形同--invalid-function-pdf-view-current-page--的错误信息}

这是因为在 28.x 以后的 Emacs 版本中会开启本地化编译 (native compilation), 而 `pdf-tools` 中有一些语法过时了, 在本地化编译时会报错. 如果这个 bug 不解决的话, 不影响 `pdf-tools` 的使用, 但是会稍微降低 pdf 渲染的速度.

-   如何确认你的 Emacs 版本支持本地化编译

用 <kbd>C-h v &lt;enter&gt; system-configuration-options &lt;enter&gt;</kbd> 查询, 如果变量包含字段 `--with-native-compilation`, 则说明当前版本支持本地化编译

本地化编译后的文件会放在 `.emacs.d/eln-cache/` 中, 以 `.elc` 结尾.

-   解决方法

如果在上面的目录下已经产生了 `pdf-*.elc` 文件, 请先删除.

-   完全禁用本地化编译
    ```elisp
    (setq no-native-compile t)
    ```
-   只禁止 `pdf-tools` 的本地化编译
    ```elisp
    (setq native-comp-deferred-compilation-deny-list '(".*pdf.*"))
    ```


### 完整配置: {#完整配置}

```elisp
(pdf-tools-install)

(setq native-comp-deferred-compilation-deny-list '(".*pdf.*"))
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用pdf-tools 打开 pdf
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer) ;; 在完成编译后刷新 pdf 文件

(define-key pdf-view-mode-map "d" 'pdf-view-next-page-command) ;; 向后翻页
(define-key pdf-view-mode-map "a" 'pdf-view-previous-page-command) ;; 向前翻页
(define-key pdf-view-mode-map "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
(define-key pdf-view-mode-map "w" 'pdf-view-scroll-down-or-previous-page) ;; 向上滑动

(require 'pdf-annot)
(define-key pdf-annot-minor-mode-map (kbd "C-a a") 'pdf-annot-add-highlight-markup-annotation) ;; 高亮
(define-key pdf-annot-minor-mode-map (kbd "C-a s") 'pdf-annot-add-squiggly-markup-annotation) ;; 波浪线
(define-key pdf-annot-minor-mode-map (kbd "C-a u") 'pdf-annot-add-underline-markup-annotation) ;; 下划线
(define-key pdf-annot-minor-mode-map (kbd "C-a d") 'pdf-annot-delete) ;; 删除

(require 'pdf-history)
(define-key pdf-history-minor-mode-map "b" 'pdf-history-backward)

(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 自动放大到页宽
```


### 相关资源 {#相关资源}

-   `pdf-tools` 的 `Github` 仓库: <https://github.com/vedang/pdf-tools>
-   `msys2` 官网 <https://www.msys2.org/>
-   `epdfinfo.exe` 可用版本:  <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>


### 使用 `pdf-tools` 的理由 {#使用-pdf-tools-的理由}

在用 Emacs 编写 LaTeX 文档的过程中, 你是否...

-   预览 pdf 需要来回在编辑器和 pdf 阅读器之间切换?
-   pdf 阅读器想实现一些新功能?
-   想给 pdf 阅读器的常用功能定义新的快捷键?

又或者, 你想用 Emacs 做读书笔记, 需要同时:

-   输入大量的数学符号
-   对 pdf 文件进行批注
-   同步 Emacs 笔记文件和 pdf 文件批注的位置

`pdf-tools` 可以完美实现这些目标.


### `pdf-tools` 的优点 {#pdf-tools-的优点}

与 `DocView` (Emacs 中内置的 pdf 阅读器) 比较

-   `DocView`: 不清晰, 阅读效果差, 读取速度慢
-   `pdf-tools`:
    -   速度快, 图片渲染效果好
    -   正常鼠标操作 + 大量 (可自定义) 快捷键


### 演示 {#演示}


#### 功能: {#功能}

-   基础的 pdf 阅读功能应有尽有, 包括超链接跳转和返回, 展开目录等
-   与 `auctex` 配合使用, 支持对编译后 pdf 进行正向/反向搜索
-   pdf 批注, 高亮, 下划线 (可保存在 pdf 文件上)


#### 使用场景 {#使用场景}

-   编写 `latex` 文档
-   配合 `org-noter` 在 pdf 上做读书笔记


### 安装流程 {#安装流程}

分为两部分


#### Emacs 包的安装 {#emacs-包的安装}

-   保证 `melpa-stable` 在 Emacs 包的列表中
    可以通过查看 `package-archives` 变量进行确认
    ```elisp
    (require 'package) ;; Emacs 包管理器
    (setq package-check-signature nil) ;; 如果有签名验证问题, 可以设置不检查签名
    (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                             ("melpa-stable" . "https://stable.melpa.org/packages/") ;; 下载 pdf-tools 只需要这个
                             ("melpa" . "https://melpa.org/packages/")
                             ("gnu" . "http://elpa.gnu.org/packages/")))
    ```
-   用 <kbd>M-x package-list-package</kbd> 打开 Emacs 包的列表
-   用 <kbd>C-s pdf-tools</kbd> 找到 `pdf-tools`
-   安装 `melpa-stable` 版本  (2023.3: `melpa` 版本仍有 bug)


#### `epdfinfo.exe` 的安装 {#epdfinfo-dot-exe-的安装}

`epdfinfo.exe` 及其它一些依赖文件 (例如 <kbd>libpopper-&lt;version&gt;.dll</kbd>) 可以帮助 Emacs 读取 pdf 文件

两种方法

-   把预编译好的文件直接放进 Emacs 的安装目录 (将上传一个可用的版本:   <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>)
-   利用 `msys2`


### 用 `msys2` 安装 `epdfinfo` {#用-msys2-安装-epdfinfo}


#### 什么是 `msys2`? {#什么是-msys2}

可以将许多开源程序本地化编译为 Windows 程序的平台

优点

-   软件管理和升级方便
-   Emacs 一些高阶功能依赖的不少开源程序都能在上面下载
-   其它可以安装的开源软件:
    `Git`, `Emacs`, `texlive`, `gcc`, `python` ...


#### 步骤 {#步骤}

-   到 `msys2` 官网上 <https://www.msys2.org/> 下载安装程序 `msys2-x86_x64-<date>.exe`. 默认安装目录为 `C:/msys64/`.
-   打开 `C:/msys64/` 下 `mingw64.exe`. 会弹出一个命令行终端
-   在命令行终端中输入
    ```sh
    pacman -S mingw-w64-x86_64-emacs-pdf-tools-server
    ```
    以上命令可以在[这里](https://packages.msys2.org/package/mingw-w64-x86_64-emacs-pdf-tools-server?repo=mingw64)找到.
-   确认并安装所有依赖包.
-   安装完成后, 你应该能在 `C:\msys64\mingw64\bin` 中找到 `epdfinfo.exe`.
-   将 `C:\msys64\mingw64\bin` 加入环境变量 `PATH`


### 基本配置 {#基本配置}


#### 启动 `pdf-tools` {#启动-pdf-tools}

在 `init.el` 文件中加入

```elisp
(pdf-tools-install)
```

如果想延迟启动 (如打开 pdf 文件后再启动, 节省 Emacs 启动时间), 可以用下面的代码替换

```elisp
(pdf-loader-install)
```


#### 配合 `AucTeX` 使用的配置 {#配合-auctex-使用的配置}

保持不变的设置

```elisp
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
(setq TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
(setq TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索
```

使用 `Sumatra PDF` 的配置

```elisp
(setq TeX-view-program-list
 '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o"))))
(assq-delete-all (quote output-pdf) TeX-view-program-selection)
(add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")
```

`pdf-tools` 的配置

```elisp
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用pdf-tools 打开 pdf
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer) ;; 在完成编译后刷新 pdf 文件
```


### 操作与个性化: 移动 {#操作与个性化-移动}

-   向下/上小滑动: 鼠标滚轮, <kbd>C-n</kbd> / <kbd>C-p</kbd>
-   向下/上大滑动: <kbd>&lt;space&gt;</kbd> / <kbd>S-&lt;space&gt;</kbd>
-   向后/前翻页: <kbd>n</kbd> / <kbd>p</kbd>

我的设置: 尽量把移动绑定在左手 (<kbd>awsd</kbd>), 空出右手进行鼠标操作.

```elisp
(define-key pdf-view-mode-map
  "d" 'pdf-view-next-page-command) ;; 向后翻页
(define-key pdf-view-mode-map
  "a" 'pdf-view-previous-page-command) ;; 向前翻页
(define-key pdf-view-mode-map
  "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
(define-key pdf-view-mode-map
  "w" 'pdf-view-scroll-down-or-previous-page) ;; 向上滑动
```


### 操作与个性化: 批注 {#操作与个性化-批注}

-   高亮: 右键菜单, 或 <kbd>C-C C-a h</kbd> (h=highlight)
-   直线下划线: 右键菜单, 或 <kbd>C-c C-a u</kbd> (u=underline)
-   波浪下划线: 右键菜单, 或 <kbd>C-c C-a s</kbd> (s=squiggly)
-   文字批注: 右键菜单, 或 <kbd>C-c C-a t</kbd> (t=text)
-   删除批注: 右键菜单, 或 <kbd>C-c C-a D</kbd> (d=delete)

我的设置:

```elisp
(require 'pdf-annot)
(define-key pdf-annot-minor-mode-map (kbd "C-a a") 'pdf-annot-add-highlight-markup-annotation) ;; 高亮
(define-key pdf-annot-minor-mode-map (kbd "C-a s") 'pdf-annot-add-squiggly-markup-annotation) ;; 波浪线
(define-key pdf-annot-minor-mode-map (kbd "C-a u") 'pdf-annot-add-underline-markup-annotation) ;; 下划线
(define-key pdf-annot-minor-mode-map (kbd "C-a d") 'pdf-annot-delete) ;; 删除
```


### 操作与个性化: 文档跳转 {#操作与个性化-文档跳转}

-   展示目录: <kbd>o</kbd>
    -   跳到目录位置: <kbd>&lt;enter&gt;</kbd> / <kbd>M-&lt;enter&gt;</kbd>
-   关闭目录: <kbd>q</kbd>
-   返回上一个位置: <kbd>l</kbd>
-   跳到下一个位置: <kbd>r</kbd>

这里重新绑定常用的返回功能 (小知识: 在 `Sumatra PDF` 里对应 <kbd>Alt-&lt;right&gt;</kbd>)

```elisp
(require 'pdf-history)
(define-key pdf-history-minor-mode-map "b" 'pdf-history-backward)
```


### 操作与个性化: 放缩 {#操作与个性化-放缩}

-   放大/缩小: <kbd>+</kbd> / <kbd>-</kbd>
-   放大到页宽/页高/屏幕: <kbd>W</kbd> / <kbd>H</kbd> / <kbd>P</kbd>
-   重置: <kbd>0</kbd>

打开 pdf 文件时自动放缩

```elisp
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 自动放大到页宽
```


### 其它可能出现的 bug {#其它可能出现的-bug}


#### 无法进行高亮/划线等 {#无法进行高亮-划线等}

这可能是安装了 2023 年后 `pdf-tools` 的版本导致的. 可以从 <kbd>M-x package-list-package</kbd> 界面中确认是从 `melpa-stable` 中安装的


#### 形同 `(invalid-function pdf-view-current-page)` 的错误信息 {#形同--invalid-function-pdf-view-current-page--的错误信息}

这是因为在 28.x 以后的 Emacs 版本中会开启本地化编译 (native compilation), 而 `pdf-tools` 中有一些语法过时了, 在本地化编译时会报错. 如果这个 bug 不解决的话, 不影响 `pdf-tools` 的使用, 但是会稍微降低 pdf 渲染的速度.

-   如何确认你的 Emacs 版本支持本地化编译

用 <kbd>C-h v &lt;enter&gt; system-configuration-options &lt;enter&gt;</kbd> 查询, 如果变量包含字段 `--with-native-compilation`, 则说明当前版本支持本地化编译

本地化编译后的文件会放在 `.emacs.d/eln-cache/` 中, 以 `.elc` 结尾.

-   解决方法

如果在上面的目录下已经产生了 `pdf-*.elc` 文件, 请先删除.

-   完全禁用本地化编译
    ```elisp
    (setq no-native-compile t)
    ```
-   只禁止 `pdf-tools` 的本地化编译
    ```elisp
    (setq native-comp-deferred-compilation-deny-list '(".*pdf.*"))
    ```


### 完整配置: {#完整配置}

```elisp
(pdf-tools-install)

(setq native-comp-deferred-compilation-deny-list '(".*pdf.*"))
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 用pdf-tools 打开 pdf
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer) ;; 在完成编译后刷新 pdf 文件

(define-key pdf-view-mode-map "d" 'pdf-view-next-page-command) ;; 向后翻页
(define-key pdf-view-mode-map "a" 'pdf-view-previous-page-command) ;; 向前翻页
(define-key pdf-view-mode-map "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
(define-key pdf-view-mode-map "w" 'pdf-view-scroll-down-or-previous-page) ;; 向上滑动

(require 'pdf-annot)
(define-key pdf-annot-minor-mode-map (kbd "C-a a") 'pdf-annot-add-highlight-markup-annotation) ;; 高亮
(define-key pdf-annot-minor-mode-map (kbd "C-a s") 'pdf-annot-add-squiggly-markup-annotation) ;; 波浪线
(define-key pdf-annot-minor-mode-map (kbd "C-a u") 'pdf-annot-add-underline-markup-annotation) ;; 下划线
(define-key pdf-annot-minor-mode-map (kbd "C-a d") 'pdf-annot-delete) ;; 删除

(require 'pdf-history)
(define-key pdf-history-minor-mode-map "b" 'pdf-history-backward)

(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 自动放大到页宽
```


### 相关资源 {#相关资源}

-   `pdf-tools` 的 `Github` 仓库: <https://github.com/vedang/pdf-tools>
-   `msys2` 官网 <https://www.msys2.org/>
-   `epdfinfo.exe` 可用版本:  <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>


## 如何优雅地预览公式 {#easy-latex-writing-ap02-prettify}


### 所见即所得的实现方式 {#所见即所得的实现方式}

文本编辑中的两个要素

-   文本本身
-   文本的格式


#### pdf 文件预览: 正向与逆向搜索 {#pdf-文件预览-正向与逆向搜索}

[【Emacs+LaTeX教程】Emacs最强内置pdf阅读功能pdf-tools简介](https://www.bilibili.com/video/BV1pg4y1s7Z9/)
缺点

-   需要大屏幕
-   如果编译错误就无法预览


#### 使用 `preview-latex` {#使用-preview-latex}

[【教程】LaTeX+Emacs从零开始2-6节：所见即所得之Preview-latex](https://www.bilibili.com/video/BV1H4411a7fD/)
缺点

-   需要手动执行编译: 常用键 <kbd>C-c C-p C-p</kbd>
-   代码的可读性不强


#### 使用 `prettify-symbols-mode` {#使用-prettify-symbols-mode}

优点

-   不需要手动触发
-   没有编译过程, 不会报错
-   提高了代码的可读性


### 基本设置 {#基本设置}

版本要求

-   Emacs &gt;= 25
-   AucTex &gt;= 13.1.10 (可通过 <kbd>M-x package-list-package</kbd> 中查找 `auctex` 查看)


#### 临时打开 {#临时打开}

<kbd>M-x prettify-symbols-mode</kbd>


#### `init.el` 文件设置 {#init-dot-el-文件设置}

```elisp
(defun my-latex-hook ()
  (prettify-symbols-mode t))
(add-hook 'LaTeX-mode-hook 'my-latex-hook)
```


#### 字体设置 {#字体设置}

保证 Unicode 数学符号可以正确显示

```elisp
(set-fontset-font "fontset-default" 'mathematical "Cambria Math")
```


#### 自动展开 {#自动展开}

设置自动展开光标附近的宏命令.

```elisp
(setq prettify-symbols-unprettify-at-point t)
```

tips: 如果只想删除刚输入的一个宏命令, 最快的方法是用 <kbd>C-/</kbd> 撤消, 而不是一个个字符删除.


### 加入自己的符号 {#加入自己的符号}

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
-   unicode 编码以 "`#x`" 表示是16进制数字, 否则就是10进制
-   常用编码表: <https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode>

个人加入编码的原则

-   原列表中没有的编码
-   像 `\N` , `\Z` 等大多数人使用的宏命令, 这样可以减少与他人合作的障碍
-   进一步简化自己的常用命令, 像上面的各种 `E`, `P`.


### LaTeX 相关设置汇总 {#latex-相关设置汇总}

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
