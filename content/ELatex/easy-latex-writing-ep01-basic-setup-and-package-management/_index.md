+++
title = "Emacs 快速配置与插件管理"
draft = false
weight = 10
+++

本小节将介绍如何快速配置一个用于 LaTeX 写作的 [Emacs](https://www.gnu.org/s/emacs/), 包括软件安装和插件管理, 以及 Emacs 的一些必要的入门知识. 最后将推荐两组非常实用的插件: 第一组插件 [Which-key](https://github.com/justbur/emacs-which-key) + [Keycast](https://github.com/tarsius/keycast) + [Helpful](https://github.com/Wilfred/helpful) 将帮助我们快速熟悉 Emacs 的操作和概念, 第二组插件 [Vertico](https://github.com/minad/vertico) + [Marginalia](https://github.com/minad/marginalia) + [Orderless](https://github.com/oantolin/orderless) 将有效提升我们在小缓冲区的补全体验, 并附上 [AucTeX](https://www.gnu.org/s/auctex) 和 [CDLaTeX](https://github.com/cdominik/cdlatex) 的基本设置.


## Emacs 安装 {#emacs-安装}

Emacs 可以运行在 Windows, Linux, MacOS 上, 也可以通过 `termux` 运行在 Android 手机或平板上. 这里我们只介绍电脑系统上的 Emacs 安装.

在本文撰写时 (2023.4), Emacs 的最新正式版本为 28.2, 最近的测试版本为 29.090. 下面介绍的安装方法针对的是正式版. (更新: 2023.8 已经可以下载 Emacs 29 正式版.)

在多数平台上, 我们是利用合适的开源软件管理器来安装 Emacs. 在 Linux 中这是自带的, 在 Windows 和 MacOS 中大家需要自行安装. 我们下面详细说明.


### Linux {#linux}

Linux 系统中可以用自带的软件管理器安装 Emacs. 例如, 在 Ubuntu 中使用 `apt` 安装 Emacs, 只需要在命令行中输入

```shell
  sudo apt install emacs
```


### MacOS {#macos}

MacOS 中需要先安装 [Homebrew](https://brew.sh/). 方法是在命令行中输入

```shell
 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

当 Homebrew 安装成功后会有提示, 然后就可以使用 Homebrew 安装 Emacs 了: 在命令行中输入

```shell
 brew install emacs
```


### Windows {#windows}

Windows 下安装 Emacs 有两种常见方法. 第一种是安装 MSYS2 ([下载链接](https://www.msys2.org/)). 安装后打开 `C:/msys64/mingw64.exe` (`C:/msys64/` 是 MSYS2 的默认安装目录, 根据实际情况调整). 在命令行中输入

```shell
  pacman -S mingw-w64-x86_64-emacs
```

通过 MSYS2 安装的一个好处是我们可以通过 Pacman 管理 Emacs 的更新. 另一个好处是在上面还可以很方便安装其它开源软件, 例如 Git, Epdfinfo 等. Epdfinfo 是在 Windows 下使用 Emacs 的 [PDF-tools](https://github.com/vedang/pdf-tools) 插件的必需软件 ([BV1pg4y1s7Z9](https://www.bilibili.com/video/BV1pg4y1s7Z9/)).

另一种安装方法是直接从[官网](http://ftp.gnu.org/gnu/emacs/windows/emacs-28/)上下载安装包. Emacs 28 的安装包已经优化了不少, 会自动把程序安装至 `C:/Program Files/emacs` 目录下, 并附带卸载程序. 通过安装包安装的 Emacs 需要我们手动更新.


## 安装 Emacs 后的额外设置 {#安装-emacs-后的额外设置}


### Ctrl 键设置 {#ctrl-键设置}

安装完 Emacs 之后, 我 **强烈建议** 大家交换 <kbd>Caps Lock</kbd>  与 <kbd>Left Ctrl</kbd>. Emacs 常常使用以 <kbd>Ctrl</kbd> 开始的快捷键, 因此把 <kbd>Ctrl</kbd> 与不常用的大写锁定 <kbd>Caps Lock</kbd> 交换是每个 Emacs 使用者对电脑 做的第一件事. <kbd>Ctrl</kbd> 键的广泛使用是因为在 Emacs 诞生之初, 当时通用的键盘 <kbd>Ctrl</kbd> 确实在当今的 <kbd>Caps Lock</kbd> 位置上. 再啰嗦一句: 交换 <kbd>Caps lock</kbd> 与 <kbd>Ctrl</kbd> 绝不是一件可有可无的事情, 它在我们日常使用 Emacs 中真的非常重要! 大家千万不要怕麻烦.

交换 <kbd>Ctrl</kbd> 与 <kbd>Caps Lock</kbd> 的方法在不同系统上也不一样.


#### Windows {#windows}

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


#### Ubuntu 及其它 Linux 系统 {#ubuntu-及其它-linux-系统}

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


#### MacOS {#macos}

在 MacOS 中, 大家可以在 `system` -&gt; `keyboard` -&gt; `functional keys` 中调整所有功能键的键位.


### 家目录与系统路径 {#家目录与系统路径}

剩下两个设置只有 Windows 用户需要进行.

第一是把 Emacs 的家目录, 即 Emacs 中通过 `~` 访问的目录, 改成 `C:/Users/<用户名>/`. 默认的家目录是 `C:/Users/<用户名>/AppData/Roaming/`. 从这个目录出发不方便我们访问像 "我的文档" 这种常用文件夹, 所以我们需要手动修改家目录为 `C:/Users/<用户名>/`, 与 Linux 和 MacOS 的使用习惯保持一致.

修改家目录的方法是在环境变量的设置中 (可以在 Windows 搜索栏中搜索 `Edit system variables` 打开), 增加一个用户的环境变量 `HOME`, 把它设置为 `C:/Users/<用户名>/`.

第二是保证你的 Emacs 安装目录在系统变量 `PATH` 上. 如果不在, 还是在同一个界面, 把包含你 `emacs.exe` 的文件夹路径手动添加到 `PATH` 变量中.


## Emacs 基本知识讲解与必知快捷键 {#emacs-基本知识讲解与必知快捷键}

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


### 中止命令与撤销命令 {#中止命令与撤销命令}

在 Emacs 中发生误操作时, 你需要知道如何中止与撤销命令. 当你的快捷键输入一半想反悔时 (是的, Emacs 的快捷键可以很长!), 可以使用 <kbd>C-g</kbd> 重新来输入, 又或者 Emacs 在执行命令时卡住了, 你可以通过 <kbd>C-g</kbd> 来让它恢复正常.

如果你需要撤回上一条命令, 则需要使用 <kbd>C-/</kbd>. 但值得注意的是, 撤回撤回命令的命令也是同一个键; 这偶尔会让人抓狂.


### 文件与窗口相关命令 {#文件与窗口相关命令}

下面我们介绍 Emacs 中最基础的几个管理界面的快捷键.

首先是打开文件, <kbd>C-x C-f</kbd>, 命令名是 `find-file`. 这里的 `find` 隐含 Emacs 会根据不同情况执行不同操作: 若文件存在, 则是普通的打开文件; 若文件不存在, 则是打开一个新文件.

第二个是保存文件, <kbd>C-x C-s</kbd>, 对应 `save-buffer`, 即把当前缓冲区 (更新后) 的内容写进文件里.

大多数情况将缓冲区 (buffer) 等同于文件不会影响你的 Emacs 使用. 这里简单讲讲它们的不同. 文件存在于电脑硬盘上, 而 Emacs 的缓冲区只显示文件内容. 当你把文件内容读入缓冲区以后, 又在 Emacs 外修改了文件的内容, 缓冲区中的内容并不会改变, 除非你明确指示 Emacs 重新读取. 而在 Windows 中, 一个文件同时只能被一个 Windows 程序打开. Emacs 的缓冲区也不一定对应着文件, 在模式栏大家可以看到当前缓冲区的名字. 名字被两个 `*` 号包含的一般是非文件的缓冲区, 例如 `*Message*` 用于显示 Emacs 给用户的信息, 编译 LaTeX 时 `*Output*` 会存放编译输出结果等.

第三个命令是切换缓冲区/文件, <kbd>C-x b</kbd>, 对应 `switch-to-buffer`. 执行后在最下方的小缓冲区会提示输入你想要切换的缓冲区名字, 默认是上一个显示的缓冲区, 直接回车就行.

在 Emacs 中同时显示多个缓冲区的方法是打开多个窗口 (window), 然后在每个窗口中显示一个缓冲区. 有时 Emacs 自动创建新的窗口, 例如展示帮助信息时. 新手最常用的操作是保留当前光标所在窗口, 而关掉其它所有窗口. 这可以通过, <kbd>C-x 1</kbd>, 即 `delete-other-window` 实现. 我们可以用鼠标辅助我们在不同窗口间切换.


### 帮助命令 {#帮助命令}

Emacs 中查询帮助信息的快捷键是 <kbd>C-h &lt;字母&gt;</kbd>. 常用的有 <kbd>C-h f</kbd>, 查询命令, <kbd>C-h v</kbd>, 查询变量, 以及 <kbd>C-h k</kbd>, 查询快捷键. 通常 <kbd>C-h</kbd> 命令会自动创建新的窗口显示帮助信息.  我们可以先把光标移到我们工作的缓冲区, 然后用 <kbd>C-x 1</kbd> 关闭掉帮助信息窗口. 注意此时帮助信息的缓冲区并没有关闭, 重新显示可以通过 <kbd>C-x b</kbd> 并查找以 `*help*` 命名的缓冲区.


### 复制/剪切/粘贴 {#复制-剪切-粘贴}

Emacs 有自己一套复制/剪切/粘贴的快捷键: <kbd>M-w</kbd> / <kbd>C-w</kbd> / <kbd>C-y</kbd>. 这和一般程序的 <kbd>C-c</kbd> / <kbd>C-x</kbd> / <kbd>C-v</kbd> 不同, 需要大家习惯. 所有复制或剪切的内容都会进入一个叫 `kill-ring` 的地方, 它相当于一个剪粘版的历史记录. 粘贴快捷键 <kbd>C-y</kbd> 会粘贴最近一条记录, 如果你想访问之前的记录, 可以紧跟着 <kbd>C-y</kbd> 再按下一次或多次 <kbd>M-y</kbd>.


## Emacs 插件管理 {#emacs-插件管理}

接下来我们介绍如何更好地管理 Emacs 插件. Emacs 插件也叫 Emacs 包 (package). 插件可以给我们带来更多的功能, 是 Emacs 使用中不可缺少的一环. 插件的安装和设置与其它的 Emacs 设置一样, 都放在 Emacs 的启动文件 `~/.emacs.d/init.el` 中. 关于插件安装与设置, 我推荐大家使用现在常用的 `use-package` 语法, 它的语法更简洁, 还可以很方便地自动安装插件.

Emacs 中下载新的插件可以通过不同的方式 (这也是由某些插件提供的). 常用的有两种, 一种是用内置的 `package.el`, 这个插件名字就叫 `package.el`, `.el` 后缀来自于 Emacs 的编程语言 Elisp.
第二种是用 [Straight](https://github.com/radian-software/straight.el). `package.el` 会从官方的插件库 (ELPA, MELPA) 或镜像网站上下载新插件, 而 Straight 用下载插件的源代码并编译, 一般是利用 Git 从 Github 上下载. 为了使用 Straight, 你需要系统上已经安装了 Git 程序, 并且能正常地访问 `github.com`. 以下我们介绍两种安装方式如何设置.

我们在 `package.el` 和 Straight 的设置示例中都手动检查并安装了 `use-package`. Emacs 29 后 `use-package` 已经是内置插件, 相关代码可以省去.


### `package.el` 设置示例 {#package-dot-el-设置示例}

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


### `straight.el` 设置示例 {#straight-dot-el-设置示例}

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


## 推荐插件 {#推荐插件}

下面我给大家推荐两组非常实用的插件. 在进行好 `package.el` 或者 `straight.el` 的设置后 (注意: 不能同时使用), 安装与设置插件只需要把相关的 `use-package` 代码块复制到 `init.el` 即可. 而且在两个体系下的代码块基本是通用的.

我们之前也都设置了自动安装插件. 当你第一次执行 `init.el` 时 (通常是第一次重启 Emacs 的时候), Emacs 会自动检测你在 `init.el` 中声明的插件是否已经安装, 若没有则通过指定的方法 (`package.el` 或 `straight.el`) 自动下载安装. 如果大家在一台新的机器上使用 Emacs, 把 `init.el` 文件复制到新机器上就可以直接获得一模一样的使用体验!

你也可以在修改完 `init.el` 后, 执行 <kbd>M-x</kbd> `eval-buffer` 命令手动加载新加的 `use-package` 代码块.

在复制代码块中最常见的问题是某个地方在复制的过程中漏了括号. 大家已经发现 elisp 语言中括号是必须配对的. 我们可以在修改 `init.el` 后手动的用 <kbd>M-x</kbd> `match-paren` 检查括号是否匹配. 如果有不匹配的括号, 那么光标就会跳过没有匹配成功的括号上, 否则这个命令不会用任何效果.


### 插件组合1: 更多的帮助信息 {#插件组合1-更多的帮助信息}

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


### 插件组合2: 更好的补全界面 {#插件组合2-更好的补全界面}

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


## 基本的 `CDLaTeX` + `AucTeX` 设置 {#基本的-cdlatex-plus-auctex-设置}

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


## 有用的链接 {#有用的链接}

-   Emacs 官网: <https://www.gnu.org/software/emacs/>
-   我的坚果云分享: <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0E>
-   我的 Emacs 设置: <https://gitee.com/mickey991/emacs-config.git>
