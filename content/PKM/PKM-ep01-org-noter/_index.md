+++
title = "PDF 读书笔记插件 org-noter"
draft = false
weight = 20
+++

## org-noter 是什么? {#org-noter-是什么}

-   实现功能: 用一个 `.org` 笔记文件作为书本页边空间的延展, 记录任何信息, 包括截图, 数学公式, 代码等
-   `pdf` 窗口和 `org` 窗口双向同步
    -   在浏览 `pdf` 文件时, 笔记窗口光标会跟随到相应位置
    -   在笔记文件中移动光标, `pdf` 文件会跟随滚动


## 选择 org-noter 的理由 {#选择-org-noter-的理由}

... 或者选择 `org` 文件的理由

-   电子笔记: 无纸化, 便携, 可检索
-   纯文本格式, 易于保存和同步 (如用 `Git`)
-   基于 `org-mode` 的强大功能和生态
    -   诞生于 2003 年 (Markdown 诞生于 2004 年)
    -   `org-mode` 是 Emacs 中最强大的插件
    -   天生为笔记而生. 核心是"大纲式浏览" + "标记语言" + "任务管理"
    -   可以输出成 `markdown`, `html`, `latex`, `odt` 等多种格式
    -   本身支持 LaTeX 公式的编辑与预览


## 安装 {#安装}

-   `org-mode`: Emacs 自带, 可以通过 <kbd>M-x package-list-package</kbd> 更新到 `Melpa` 最新版
-   `pdf-tools`: 阅读 `pdf` 必备, 参考往期视频:
    [【Emacs+LaTeX教程】Emacs最强内置pdf阅读功能pdf-tools简介](https://www.bilibili.com/video/BV1pg4y1s7Z9/)
-   `org-noter`: <kbd>M-x package-list-package</kbd> 安装 `Melpa` 最新版
    -   2023年3月更新内容
        -   2维笔记定位
        -   `djvu`, `epub` 文件支持
        -   批注小标题优化
    -   如果安装了 `use-package`, 可将以下代码加入 `init.el` 自动安装
        ```elisp
        (use-package org-noter
          :ensure t )
        ```


## 基本操作 {#基本操作}


### 打开 `org-noter` {#打开-org-noter}

`org-noter` 打开需要执行 <kbd>M-x org-noter</kbd> 命令. 有两种方式

-   从 `pdf` 文件打开
    需要指定 `org` 文件名及路径. 这时会自动生成一个 `org` 文件, 并在 `org` 文件中保存 `pdf` 文件的路径名
-   从 `org` 文件打开
    这里 `org` 文件里须包含 `pdf` 文件的路径名, 一般来说是由第一种方式生成的 `org` 文件.

未来的计划: 结合 `org-roam` + `org-roam-bibtex` + `Zotero`,  我们可以生成一个包含 pdf 路径的 `org-noter` 读书笔记模板

-   作为 `org-noter` 的笔记文件使用
-   作为个人知识库一个节点被引用


### 大纲导入及 `org-mode` 基本操作 {#大纲导入及-org-mode-基本操作}

在 pdf 界面, <kbd>M-x org-noter-create-skeleton</kbd> 可以导入 `pdf` 大纲

**小技巧**: 在 `pdf-tools` 中, <kbd>o</kbd> / <kbd>q</kbd> 可以显示/关闭大纲.

`org-mode` 常用快捷键:

-   <kbd>&lt;tab&gt;</kbd> : 展开/折叠光标下标题, 进行如下循环:
    "只显示标题" -&gt; "显示子标题" -&gt; "显示子标题内容" -&gt; "只显示标题"
-   <kbd>&lt;shift&gt;-&lt;tab&gt;</kbd>: 同时展开/折叠所有标题
-   <kbd>M-&lt;left&gt;</kbd> / <kbd>M-&lt;right&gt;</kbd> : 当前标题升级/降级
-   <kbd>M-S-&lt;left&gt;</kbd> / <kbd>M-S-&lt;right&gt;</kbd>: 当前标题及其子标题升级/降级


### 插入笔记 {#插入笔记}

`org-noter` 通过记录页码及位置保持笔记文件与 `pdf` 文件的同步. 根据定位的精度, 在 `pdf` 界面, 有3种插入笔记方式

方法1
: 按 <kbd>i</kbd> 在当前页插入笔记 (page note)

方法2
: 按 <kbd>M-i</kbd> 在鼠标点击位置插入笔记 (precise note)

方法3
: 选中文字后, 按 <kbd>&lt;tab&gt;</kbd> / <kbd>M-i</kbd> 或 <kbd>i</kbd> 在选中文字位置插入笔记

在笔记中还可以使用 `CDLaTeX` 编辑数学公式, 以及用 `org-download` 插入截图. 请到教程的最后查看这些功能的基本设置.


### 位置同步 {#位置同步}

-   `pdf` -&gt; `org`: 自动
-   `org` -&gt; `pdf`: 手动
    -   <kbd>M-p</kbd> / <kbd>M-.</kbd> / <kbd>M-n</kbd>: 上一/当前/下一页笔记
    -   <kbd>C-M-p</kbd> / <kbd>C-M-.</kbd> / <kbd>C-M-n</kbd>: 上一/当前/下一条笔记
    -   以上命令也可以在 `pdf` 界面中使用


## 高级设置 {#高级设置}


### 默认笔记目录 {#默认笔记目录}

```elisp
(setq org-noter-notes-search-path '("your/path/to/note-directory/" "2nd-path"  "3rd-path"))
```

设置后, 从 pdf 文件中使用 <kbd>org-noter</kbd> 命令会自动在上述目录中寻找与文件名同名的 `.org` 笔记文件.

两种 `org-noter` 使用模式

-   (推荐) 笔记在不同的 `.org` 文件中
-   所有笔记在同一个 `.org` 文件中: 将上述变量设置为一个文件名
    ```elisp
    (setq org-noter-notes-search-path '("your/path/to/notes.org"))
    ```


### 自动保存上次位置 {#自动保存上次位置}

```elisp
(setq org-noter-auto-save-last-location t)
```


### 选中文字后插入笔记自动高亮 {#选中文字后插入笔记自动高亮}

```elisp
(setq org-noter-highlight-selected-text t)
```

这是全局设置. 如果想对某一条笔记临时启用或禁用, 可以使用 <kbd>C-u</kbd> 前缀.

例如: 当前高亮默认设置为 `t`, 但当前笔记不想高亮文字, 可以使用 <kbd>C-u M-e</kbd> 插入 `precise note`


### 长文本和短文本 {#长文本和短文本}

-   短文本默认标题为全文
-   长文本默认标题为 `Note for page XXX`

修改长/短文本标准:

```elisp
(setq org-noter-max-short-selected-text-length 20) ;; 默认为 80
```

修改短文本默认标题:

```elisp
(setq org-noter-default-heading-title "第 $p$ 页的笔记")
```


### 修改快捷键 {#修改快捷键}

```elisp
(global-set-key (kbd "C-c n n") 'org-noter) ;; 与 org-roam 配合
(define-key org-noter-doc-mode-map (kbd "e") 'org-noter-insert-note) ;; 加入左手键位
(define-key org-noter-doc-mode-map (kbd "M-e") 'org-noter-insert-precise-note) ;; 加入左手键位
```


### 代码汇总 {#代码汇总}

自定义设置的全部代码

```elisp
(setq org-noter-notes-search-path '("your/path/to/note-directory/")) ;; 默认笔记路径
(setq org-noter-auto-save-last-location t) ;; 自动保存上次阅读位置
(setq org-noter-max-short-selected-text-length 20) ;; 默认为 80
(setq org-noter-default-heading-title "第 $p$ 页的笔记") ;; 默认短标题格式
(global-set-key (kbd "C-c n n") 'org-noter) ;; 与 org-roam 配合
(define-key org-noter-doc-mode-map (kbd "e") 'org-noter-insert-note) ;; 加入左手键位
(define-key org-noter-doc-mode-map (kbd "M-e") 'org-noter-insert-precise-note) ;; 加入左手键位
```

将上述代码写在 `use-package` 代码块中:

```elisp
(use-package org-noter
  :ensure t
  :custom
  (org-noter-notes-search-path '("your/path/to/note-directory/")) ;; 默认笔记路径
  (org-noter-auto-save-last-location t) ;; 自动保存上次阅读位置
  (org-noter-max-short-selected-text-length 20) ;; 默认为 80
  (org-noter-default-heading-title "第 $p$ 页的笔记") ;; 默认短标题格式
  :bind
  (("C-c n n" . org-noter) ;; 与 org-roam 配合
   :map org-noter-doc-mode-map ;; 加入左手键位
   ("e" . org-noter-insert-note)
   ("M-e" . org-noter-insert-precise-note)))
```


## LaTeX 公式 {#latex-公式}

`org-mode` 中与 LaTex 有关的设置

```elisp
  (use-package org
    :defer t ;; 延迟加载
    :custom
    (org-highlight-latex-and-related '(native latex entities)) ;; LaTeX 高亮设置
    (org-pretty-entities t) ;; LaTeX 代码的 prettify
    (org-pretty-entities-include-sub-superscripts nil) ;; 不隐藏 LaTeX 的上下标更容易编辑
    (org-format-latex-options
     '(:foreground default :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))) ;; 增大公式预览的图片大小
    :config
    (add-hook 'org-mode-hook #'org-cdlatex-mode) ;; 打开 cdlatex
)
```

`CDLaTeX` 的设置可以参考我的视频

-   [【教程】 LaTeX+Emacs 从零开始2-2节: CDLaTeX简介](https://www.bilibili.com/video/BV134411v7jn/)
-   [【教程】 LaTeX+Emacs从零开始2-4节：cdLaTeX自定义设置](https://www.bilibili.com/video/BV1Z4411Y7F7/)

我会在另一期教程中详细介绍 `org-mode` 中 `LaTeX` 编辑的设置.


## 截图功能 {#截图功能}

需要借助 `org-download` 实现. 在这里只给出实现截图功能的设置代码, 以后的教程再详细讲解.

使用方法

-   使用系统截图工具 (`Windows` 下用 <kbd>&lt;Win&gt;+&lt;shift&gt;+s</kbd>) 将截图保存到剪贴板
-   用 <kbd>C-M-y</kbd> (绑定了 <kbd>org-download-clipboard</kbd> 函数) 将剪贴板中图片粘贴到 `org` 文件中.

安装方法

-   `Linux` / `MacOS` (未测试)
    用以下 `use-package` 代码安装及设置
    ```elisp
    (use-package org-download
      :ensure t ;; 自动从 melpa 上安装
      :defer t ;; 延迟加载
      :bind
      (:map org-mode-map
            ("C-M-y" . org-download-clipboard)) ;; 绑定从剪贴版粘贴截图的快捷键
      :custom
      (org-download-heading-lvl 1) ;; 用一级标题给截图文件命名
      :config
      (setq-default org-download-image-dir "./img")) ;; 用同级 ./img 目录放置截图文件
    ```
-   `Windows`
    -   需要安装 `ImageMagick`, 并保证 `magick.exe` 在 `PATH` 变量的路径中
        用 `msys2` 安装
        ```sh
        pacman -S mingw-w64-x86_64-imagemagick
        ```
    -   从[这里](https://github.com/abo-abo/org-download/blob/acc9622968cb4d0027cd3478c374191597c18ea4/org-download.el%20%E6%88%96)或网盘下载 `org-download.el` 文件, 置于你的 `.emacs.d` 文件夹合适的路径中 (如 `~/.emacs.d/lisp/`)
    -   使用以下 `use-package` 代码及设置
        ```elisp
        (use-package org-download
          :ensure async ;; 因为不是从melpa安装, 需要手动保证async安装
          :defer t ;; 延迟加载
          :load-path "~/.emacs.d/lisp/"
          :bind
          (:map org-mode-map
                ("C-M-y" . org-download-clipboard)) ;; 绑定从剪贴版粘贴截图的快捷键
          :custom
          (org-download-heading-lvl 1) ;; 用一级标题给截图文件命名
          :config
          (setq-default org-download-image-dir "./img")) ;; 用同级 ./img 目录放置截图文件
        ```
