+++
title = "Emacs 上搭建你的终生个人知识库"
draft = false
+++

-   第零节: [什么是双链笔记与 org-roam 初体验]({{< relref "PKM-ep00-intro-org-roam" >}}) ([视频](https://www.bilibili.com/video/BV1qV4y1Z7h9/))
-   第一节: [PDF 读书笔记插件 org-noter]({{< relref "PKM-ep01-org-noter" >}}) ([视频](https://www.bilibili.com/video/BV1Tc411s7Tu/))
-   第二节: [Zotero文献管理系统简介]({{< relref "PKM-ep02-zotero-intro" >}}) ([视频](https://www.bilibili.com/video/BV1Lc411J7gQ/))
-   第三节: [用org-roam管理Zotero文献笔记]({{< relref "PKM-ep03-zotero-org-noter-integration" >}}) ([视频](https://www.bilibili.com/video/BV1Mg4y1j75u/))


## 什么是双链笔记与 org-roam 初体验 {#PKM-ep00-intro-org-roam}


### 什么是笔记? {#什么是笔记}

笔记核心功能:

-   输入: 记录
-   输出: 信息的提取与归纳

笔记类型举例

-   课堂笔记
-   学习心得, 复习笔记
-   读书笔记
-   数学草稿
-   科研日志, 灵感
-   教程/食谱/日记等, 例如: "如何安装Emacs", "如何做红烧肉"

...


### 信息提取 {#信息提取}

从笔记高效地提取信息很难

-   纸质笔记
    -   优点: 简单
    -   缺点: 不能检索
-   电子笔记: `LaTeX`, `org`, `markdown` 等
    -   优点: 可全文搜索, 读取信息能力大大提高
    -   缺点: 思考的过程, 知识间的联系难以体现.


### 传统笔记的局限性 {#传统笔记的局限性}

-   自上而下的树状结构
    -   树状结构无处不在: 图书馆目录, 文件夹, 书本章节段落
-   缺陷
    -   输入: 初始分类难
    -   输出: 不利于发现联系
-   传统笔记的完美形式就是一本书
-   但是现代人遇到问题第一反应不是去翻书, 而是用搜索引擎或 ChatGPT.


### 理想的笔记模型 {#理想的笔记模型}

-   双链笔记: `原子化笔记` + `网状结构`
    -   原子化笔记: 将笔记拆分成独立, 完整的小条目
    -   网状结构: 用 `链接` 串连笔记, 自下而上生成结构
-   原型: 卡片盒笔记 (Zettlekasten)
    -   卢曼 (Luhmann, 1927-1988): 德国社会学家
    -   卢曼从1952年左右开始构建卡片盒笔记, 最后笔记包含9万余条目
    -   卢曼一生发表了50本著作与550篇论文
-   现代实现方式: 双链笔记/个人知识库软件 (2020年前后)
    -   `Roam Research`, `Obsidian`, `Logseq`, `Notion` ...
    -   在 Emacs 中: 基于 `org-roam` 的知识管理系统


### 为什么选择 `org-roam` {#为什么选择-org-roam}

... 而不是 ~~Roam Research~~, ~~Obsedian~~, ~~Logseq~~, ~~Notion~~, ...


#### 我个人的笔记需求 {#我个人的笔记需求}

-   简洁但强大的笔记功能, 包含 LaTeX 数学公式, 交叉引用
-   支持双链笔记
-   能与文献管理软件如 `Zotero` 整合
-   可视化界面; 易于导出为其它形式


#### 笔记软件比较 {#笔记软件比较}

-   `Roam Research`: 创建于2020.1., 双链+图形界面, 订阅制 (基础版 180＄/年)
-   `Obsidian`: 创建于 2020.3. 基于 `Markdown` 文件, 有强大插件生态, 基础版免费, 全平台使用
-   `Logseq`: 创建于 2020. 开源, 基于 `Markdown` 或 `org` 文件. 与 `Obsedian` 非常像
-   `org-mode` + `Emacs`
    -   本身强大的文本处理能力与笔记生态
    -   个性化设置
    -   与其它工作流程的整合
    -   底层结构是纯文本, 易于保存和导出
    -   开源软件, 永久免费, 强大的插件生态, 上限和自由度很高
    -   终生个人知识库: 平台也需要有强大的生命力, Emacs 有近40年历史


### 个人知识库的构建 {#个人知识库的构建}

-   核心: 笔记间的 `链接`
-   笔记类型
    -   灵感笔记 (fleeting note)
    -   文献笔记 (literature note)
    -   永久笔记 (pernament note)
-   自下而上生成结构: 一组产生密切联系的笔记自然而然形成一个主题, 一个主题可以形成一条新的永久笔记并与其它笔记产生联系. 主题的结合可以产生新的主题, 如此反复.
-   与个人 wiki 的区别:
    个人知识库不仅仅是知识的记录, 还有对知识的归纳和提炼 -- 这由我们建立的 `链接` 体现
-   参考资料
    -   <span class="underline">How to take smart notes</span>: Sönke Ahrens
    -   (中文版) [卡片笔记写作法](https://book.douban.com/subject/35503571/).


### 系列视频内容 {#系列视频内容}

-   本期视频
    -   安装与基本设置
    -   今天以后大家可以用 `org-roam` 记录学习 `org-roam` 的笔记
-   与 `Zotero` 文献管理整合 (`org-ref`, `helm-bibtex`, `org-roam-bibtex`)
-   与 `org-noter` 整合
-   笔记模板设置
-   `org` 笔记流程优化
    -   数学公式 (`org-cdlatex`, `org-preview`)
    -   自动补全 (`company`)
    -   中文输入法 (`pyim`)
    -   交叉引用 (`org-ref`)
    -   `org` 界面优化 (`org-modern`, `org-face` ...)
-   项目管理
-   任务管理 (`org-agenda`)
-   日记系统 (`org-roam-daily`)
-   可视化 (`org-roam-ui`)
-   导出设置

......


### org-roam 安装 {#org-roam-安装}

以Emacs 28为准

-   `gcc` 编译器: 用于编译 `emacsql-sqlite` (Emacs 29后应该不再需要)
    -   `Windows` 下可通过 `msys2` 安装 (推荐), 并保证在系统可执行文件目录下
        ```shell
        pacman -S mingw-w64-x86_64-gcc
        ```
    -   `Linux` 或 `MacOS` 下大概率系统自带
-   `org-roam` 插件
    -   用内置的 `package-el` 安装: <kbd>M-x package-install &lt;return&gt; org-roam &lt;return&gt;</kbd>
    -   `use-package` 模块中 <kbd>:ensure t</kbd> (见后面示例)
-   `org-roam-ui` 插件
    -   用内置的 `package-el` 安装: <kbd>M-x package-install &lt;return&gt; org-roam-ui &lt;return&gt;</kbd>
    -   `use-package` 模块中 <kbd>:ensure t</kbd> (见后面示例)
-   `emacsql-sqlite`
    -   安装后第一次加载 `org-roam`, Emacs 会提示正在编译 `emacsql-sqlite`, 请耐心等待.
    -   编译成功后系统中会找到类似 `.emacs.d/elpa/emacsql-sqlite-XXXXX/sqlite` 的目录


### 基本设置 {#基本设置}

-   笔记目录: 需要提前手动创建, 要易于同步;
-   日记设置: 目录与快捷键
-   快捷键: 使用任何一个将会启动 `org-roam`
    ```elisp
    (use-package org-roam
      :ensure t ;; 自动安装
      :custom
      (org-roam-directory "~/roam-notes/") ;; 默认笔记目录, 提前手动创建好
      (org-roam-dailies-directory "daily/") ;; 默认日记目录, 上一目录的相对路径
      (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
      :bind (("C-c n f" . org-roam-node-find)
             ;; 如果你的中文输入法会拦截非 ctrl 开头的快捷键, 也可考虑类似如下的设置
             ;; ("C-c C-n C-f" . org-roam-node-find)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ("C-c n l" . org-roam-buffer-toggle) ;; 显示后链窗口
             ("C-c n u" . org-roam-ui-mode)) ;; 浏览器中可视化
      :bind-keymap
      ("C-c n d" . org-roam-dailies-map) ;; 日记菜单
      :config
      (require 'org-roam-dailies)  ;; 启用日记功能
      (org-roam-db-autosync-mode)) ;; 启动时自动同步数据库

    (use-package org-roam-ui
      :ensure t ;; 自动安装
      :after org-roam
      :custom
      (org-roam-ui-sync-theme t) ;; 同步 Emacs 主题
      (org-roam-ui-follow t) ;; 笔记节点跟随
      (org-roam-ui-update-on-save t))
    ```


### 创建笔记及链接 {#创建笔记及链接}

创建链接是最基础也是最重要的功能!

-   <kbd>org-roam-capture</kbd> (快捷键 <kbd>C-c n c</kbd>)
    创建一条新笔记. 未来可以预设不同主题的默认模板. 每一条笔记就是一个 `org` 文件
-   <kbd>org-roam-find</kbd> (快捷键 <kbd>C-c n f</kbd>)
    通过关键词查找笔记并跳转
-   <kbd>org-roam-insert</kbd> (快捷键 <kbd>C-c n i</kbd>)
    插入一条笔记的链接
    -   在查找笔记时利用 <kbd>org-roam-find</kbd> 的界面
    -   若笔记不存在, 则利用 <kbd>org-roam-capture</kbd> 的界面创建笔记
-   删除笔记: 直接删除笔记文件即可, 如 <kbd>M-x delete-file</kbd>
-   `org-mode` 内链接跳转: 鼠标点击或 <kbd>C-c C-o</kbd> (<kbd>org-open-at-point</kbd>)
-   加入标签: 普通的标签可以看作一个特殊的笔记条目, 可以用 <kbd>org-roam-insert</kbd> 去插入一些以关键字为标题的笔记充当标签.

    一个通用惯例是在笔记头部用 `org` 的列表格式罗列标签, 如
    ```nil
    ​- tag :: <tag-1>, <tag-2>, <tag-3>
    ```

以后我们也可以把这一行加入笔记模板


### 优化 minibuffer 补全界面 {#优化-minibuffer-补全界面}

`vertico` + `orderless`

```elisp
(use-package vertico ;; 补全界面优化
  :ensure t
  :config
  (vertico-mode))
(use-package orderless ;; 无序搜索
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
```


### 同步 {#同步}

-   `org-roam` 的笔记文件是纯文本文件, 用任意网盘同步即可
    -   不同机器上要设置好 `org-roam-directory` 变量 (或者直接放在网盘目录下)
    -   也可以用 `git` 同步: 可控制文件类型, 版本控制
-   一般不需要同步数据库文件 `org-roam.db`.
    -   此数据库文件保存了链接信息, 一般保存在 `./emacs.d/` 目录下
    -   当笔记条目很多的时候, 这个文件会很大
    -   在不同的机器上可以根据 `org` 文件自动生成的 (<kbd>(org-roam-db-autosync-mode)</kbd>)
-   数据库更新
    -   自动更新: 通过 <kbd>(org-roam-db-autosync-mode)</kbd> 实现
    -   手动更新
        -   执行 <kbd>M-x org-roam-db-sync</kbd>
        -   `Windows` 下执行这一命令可能会出现 `Error ....... Selecting deleted buffer` 的错误信息. 只要 `emacs-sqlite` 已经正常安装, 无视这条错误信息并再次执行 <kbd>org-roam-db-sync</kbd> 即可.


### 结语 {#结语}


## PDF 读书笔记插件 org-noter {#PKM-ep01-org-noter}


### org-noter 是什么? {#org-noter-是什么}

-   实现功能: 用一个 `.org` 笔记文件作为书本页边空间的延展, 记录任何信息, 包括截图, 数学公式, 代码等
-   `pdf` 窗口和 `org` 窗口双向同步
    -   在浏览 `pdf` 文件时, 笔记窗口光标会跟随到相应位置
    -   在笔记文件中移动光标, `pdf` 文件会跟随滚动


### 选择 org-noter 的理由 {#选择-org-noter-的理由}

... 或者选择 `org` 文件的理由

-   电子笔记: 无纸化, 便携, 可检索
-   纯文本格式, 易于保存和同步 (如用 `Git`)
-   基于 `org-mode` 的强大功能和生态
    -   诞生于 2003 年 (Markdown 诞生于 2004 年)
    -   `org-mode` 是 Emacs 中最强大的插件
    -   天生为笔记而生. 核心是"大纲式浏览" + "标记语言" + "任务管理"
    -   可以输出成 `markdown`, `html`, `latex`, `odt` 等多种格式
    -   本身支持 LaTeX 公式的编辑与预览


### 安装 {#安装}

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


### 基本操作 {#基本操作}


#### 打开 `org-noter` {#打开-org-noter}

`org-noter` 打开需要执行 <kbd>M-x org-noter</kbd> 命令. 有两种方式

-   从 `pdf` 文件打开
    需要指定 `org` 文件名及路径. 这时会自动生成一个 `org` 文件, 并在 `org` 文件中保存 `pdf` 文件的路径名
-   从 `org` 文件打开
    这里 `org` 文件里须包含 `pdf` 文件的路径名, 一般来说是由第一种方式生成的 `org` 文件.

未来的计划: 结合 `org-roam` + `org-roam-bibtex` + `Zotero`,  我们可以生成一个包含 pdf 路径的 `org-noter` 读书笔记模板

-   作为 `org-noter` 的笔记文件使用
-   作为个人知识库一个节点被引用


#### 大纲导入及 `org-mode` 基本操作 {#大纲导入及-org-mode-基本操作}

在 pdf 界面, <kbd>M-x org-noter-create-skeleton</kbd> 可以导入 `pdf` 大纲

**小技巧**: 在 `pdf-tools` 中, <kbd>o</kbd> / <kbd>q</kbd> 可以显示/关闭大纲.

`org-mode` 常用快捷键:

-   <kbd>&lt;tab&gt;</kbd> : 展开/折叠光标下标题, 进行如下循环:
    "只显示标题" -&gt; "显示子标题" -&gt; "显示子标题内容" -&gt; "只显示标题"
-   <kbd>&lt;shift&gt;-&lt;tab&gt;</kbd>: 同时展开/折叠所有标题
-   <kbd>M-&lt;left&gt;</kbd> / <kbd>M-&lt;right&gt;</kbd> : 当前标题升级/降级
-   <kbd>M-S-&lt;left&gt;</kbd> / <kbd>M-S-&lt;right&gt;</kbd>: 当前标题及其子标题升级/降级


#### 插入笔记 {#插入笔记}

`org-noter` 通过记录页码及位置保持笔记文件与 `pdf` 文件的同步. 根据定位的精度, 在 `pdf` 界面, 有3种插入笔记方式

方法1
: 按 <kbd>i</kbd> 在当前页插入笔记 (page note)

方法2
: 按 <kbd>M-i</kbd> 在鼠标点击位置插入笔记 (precise note)

方法3
: 选中文字后, 按 <kbd>&lt;tab&gt;</kbd> / <kbd>M-i</kbd> 或 <kbd>i</kbd> 在选中文字位置插入笔记

在笔记中还可以使用 `CDLaTeX` 编辑数学公式, 以及用 `org-download` 插入截图. 请到教程的最后查看这些功能的基本设置.


#### 位置同步 {#位置同步}

-   `pdf` -&gt; `org`: 自动
-   `org` -&gt; `pdf`: 手动
    -   <kbd>M-p</kbd> / <kbd>M-.</kbd> / <kbd>M-n</kbd>: 上一/当前/下一页笔记
    -   <kbd>C-M-p</kbd> / <kbd>C-M-.</kbd> / <kbd>C-M-n</kbd>: 上一/当前/下一条笔记
    -   以上命令也可以在 `pdf` 界面中使用


### 高级设置 {#高级设置}


#### 默认笔记目录 {#默认笔记目录}

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


#### 自动保存上次位置 {#自动保存上次位置}

```elisp
(setq org-noter-auto-save-last-location t)
```


#### 选中文字后插入笔记自动高亮 {#选中文字后插入笔记自动高亮}

```elisp
(setq org-noter-highlight-selected-text t)
```

这是全局设置. 如果想对某一条笔记临时启用或禁用, 可以使用 <kbd>C-u</kbd> 前缀.

例如: 当前高亮默认设置为 `t`, 但当前笔记不想高亮文字, 可以使用 <kbd>C-u M-e</kbd> 插入 `precise note`


#### 长文本和短文本 {#长文本和短文本}

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


#### 修改快捷键 {#修改快捷键}

```elisp
(global-set-key (kbd "C-c n n") 'org-noter) ;; 与 org-roam 配合
(define-key org-noter-doc-mode-map (kbd "e") 'org-noter-insert-note) ;; 加入左手键位
(define-key org-noter-doc-mode-map (kbd "M-e") 'org-noter-insert-precise-note) ;; 加入左手键位
```


#### 代码汇总 {#代码汇总}

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


### LaTeX 公式 {#latex-公式}

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


### 截图功能 {#截图功能}

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


## Zotero文献管理系统简介 {#PKM-ep02-zotero-intro}


### 软件界面 {#软件界面}

-   `Zotero` 界面:
    `收藏夹` - `文献列表`  - `文献信息`
-   收藏夹功能
    -   同一文献可以属于不同收藏夹
    -   同一收藏夹内文献可以导出成 `.bib` 或其它文件
    -   小技巧: 按 <kbd>&lt;ctrl&gt;</kbd> 并点击文献, 左侧会高亮文献所在的收藏夹
-   文献可以有 `pdf` 文件作为附件.
    -   双击文献可以用 `Zotero` 内置 `pdf` 阅读器打开 `pdf` 文件
    -   内置阅读器支持高亮, 批注等
    -   也可以设置别的默认阅读器


### 向 Zotero 中加入文献 {#向-zotero-中加入文献}

方法1
: 利用浏览器的 `Zotero Connector` 插件 ([下载链接](https://www.zotero.org/download/))
    -   支持: `Edge`, `Chrome`, `FireFox`, `Safari`
    -   自动读取文献信息, 允许时自动下载 `pdf` 全文
    -   10年前很有用的功能: 网页快照, 先存再看

方法2
: 利用文件标识码, 如 `DOI`, `ISBN` 或者 `Arxiv ID` 等

方法3
: 从本地 `pdf` 文件创建. `Zotero` 会尝试获取文献信息


为保证参考文献信息准确, 建议多使用前两种方法

手动添加 `pdf` 附件: 适用于非官方手段获取的 `pdf` 文件

合并文献信息: `Duplicate Items`


### 功能小结 {#功能小结}

`Zotero` 核心功能

-   收集文献并整理文献信息
-   导出 `.bib` 等多种格式引用文件
-   管理文献 `pdf`

两个有用的插件

-   `Better BibTeX`:
    -   自定义 (更加统一的) 键名格式
    -   实时更新导出的 `.bib` 文件, 与 `Org-roam`, `Obsidian`, `Roam Research` 等笔记软件交互必备!
-   `Zotfile`:
    -   自动根据规则重命名 `pdf` 文件
    -   自动移动 `pdf` 文件至个人网盘同步


### 插件安装方法 {#插件安装方法}

-   在 <https://www.zotero.org/support/plugins> 上找到插件的 `.xpi` 文件并下载
-   打开 `Tools` -&gt; `Add Ons` 选择从文件进行安装
-   重启 `Zotero`


### `Better BibTeX` 设置 {#better-bibtex-设置}

设置界面在 `Edit` -&gt; `Preference` -&gt; `Better BibTeX` 下

-   引用键名格式
    -   我的设置: <kbd>authorsAlpha+year+shorttitle(3,3)</kbd>
        -   <kbd>authorsAlpha</kbd>: 采用 `bibtex` 中的 `alpha` 风格; 单作者取姓的前3字母, 2个以上取首字母
        -   <kbd>year</kbd>: 4位数字年份
        -   <kbd>shorttitle(m,n)</kbd>: 标题前 m 个单词, 其中前 n 个单词首字母大写
    -   在 <https://retorque.re/zotero-better-bibtex/citing/> 有更多设置
-   保证键名不重复: `keey citekey unique` -&gt; `across all libraries`
-   `.bib` 文件自动导出与更新
    -   选中收藏夹, 导出, 勾选自动更新
    -   更新频率设置: `Automatic Export` -&gt; `Automatic Export` -&gt; `On change`


### `ZotFile` 同步设置 {#zotfile-同步设置}

-   `Zotero` 登录后有 500M 的同步空间, 因此最好有第3方网盘同步 `pdf` 文件
    -   `Zotero` 帐号负责同步文献信息
    -   `ZotFlie` 负责用网盘同步 `pdf` 文件
    -   `Better BibTeX` 的键名无法同步, 但是相同规则的键名是一样的.
    -   在新机器上建议: 先设置好 `Better BibTeX`, 再登录 `Zotero` 帐号进行同步.

-   `Zotfile` 设置: `Tools` -&gt; `ZotFile Preference`
    -   同步目录: `General Settings` -&gt; `Custom Locations`
        -   使用个人网盘目录下的文件夹
        -   无须子文件夹, 因为子文件夹不方便搜索而且多余
    -   `pdf` 文件重命名规则: `Renaming Rules`
        -   建议使用 <kbd>%b</kbd>, 表示用 `bibtex` 键名命名 `pdf`
            -   方便查找
            -   大幅提高笔记软件交互效率
    -   自动重命名: `Advance Settings` -&gt; `Automatically rename new attachment` -&gt; `always rename`
        -   对于 `Zotero` 自动下载的 `pdf` 有效
        -   手动添加的 `pdf`, 右键点击并选择 `rename and move` 即可


### 下期内容 {#下期内容}

在 `org-roam` 笔记流程中整合 `Zotero`, 包括

-   `Emcas` 中读取 `Zotero` 文献数据库并进行引用
-   每一条引用的 `Zotero` 文献建立一则 `org-roam` 笔记
-   将上述笔记作为 `org-noter` 笔记文件, 在 `Emacs` 中阅读 `Zotero` 文献的 `pdf` 附件


## 用org-roam管理Zotero文献笔记 {#PKM-ep03-zotero-org-noter-integration}


### 实现功能 {#实现功能}

-   `Emcas` 中读取 `Zotero` 文献数据库并进行引用
-   每一条引用的 `Zotero` 文献建立一则 `org-roam` 笔记
-   将上述笔记作为 `org-noter` 笔记文件, 在 `Emacs` 中阅读 `Zotero` 文献的 `pdf` 附件


### 实现方法 {#实现方法}

-   `Zotero`
    -   用 `Better BibTeX` 导出文献数据至 `bib` 文件
    -   用 `ZotFile` 保存 PDF 文件 (一般是网盘文件夹)
-   `helm/ivy-bibtex`:
    -   将上面 `bib` 文件的信息读入 Emacs, 并提取文献的标题, 作者, 引用键名等
    -   提供搜索文献信息的界面
-   `org-roam`:
    准备一个符合 `org-noter` 的笔记格式的  `org-roam` 模板
-   `org-roam-bibtex` + `org-roam`
    -   利用提取的文献信息和上面的模板生成一则 `org-roam` 笔记
    -   其它一些操作, 如打开文献网址, 打开文献 `bibtex` 条目等
-   `org-noter` + `pdf-tools`
    打开 `org-roam` 笔记中的 PDF 文件并阅读, 做读书笔记.
    -   [【Emacs+LaTeX教程】Emacs最强内置pdf阅读器pdf-tools简介](https://www.bilibili.com/video/BV1pg4y1s7Z9/)
    -   [【从零搭建Emacs个人知识库】org-noter配置及使用](https://www.bilibili.com/video/BV1Tc411s7Tu/)


### 代码 {#代码}

-   `org-roam` 基本设置:

[【从零搭建Emacs个人知识库】什么是双链笔记与 org-roam 初体验](https://www.bilibili.com/video/BV1qV4y1Z7h9/)

-   `use-package` 和插件管理方法:
    [【省时省力写LaTeX】新手Emacs快速配置与插件管理](https://www.bilibili.com/video/BV1nm4y117gn/)

<!--listend-->

```elisp
;; 第一步: 告诉 Emacs 从哪里读取 Zotero 的信息
(setq zot_bib '("~/Nutstore/1/Nutstore/Zotero-Library/Better BibTeX Export/My Library.bib"
                "<另一个Zotero bib 文件>.bib") ; Zotero 用 Better BibTeX 导出的 .bib 文件. 可以是多个文件
      zot_pdf "~/Nutstore/1/Nutstore/Zotero-Library" ; Zotero 的 ZotFile 同步文件夹
      org_refs "~/repos/notes/ref/" ) ; 自定义的 org-roam 文献笔记目录. 我的 org-roam 根目录是 ~/repos/notes

;; 第二步: 让 helm-bibtex 读取 Zotero 的信息
(use-package helm-bibtex ; 这里也可以用 ivy-bibtex 替换 helm-bibtex
  :ensure t
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

;; 第三步: 让 org-roam-bibtex 使用 helm-bibtex 的信息, 并绑定 orb 的快捷键
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (("C-c n k" . orb-insert-link)
         ("C-c n a" . orb-note-action))
  :custom
  (orb-insert-interface 'helm-bibtex) ; 与上面 helm-bibtex/ivy-bibtex 的选择保持一致
  (orb-insert-link-description 'citekey) ; 默认是用标题, 但是论文的标题一般很长, 不适合作为笔记链接的名字
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))
```

第四步: 加入一个文献笔记的 `org-roam` 模板

```elisp
(use-package org-roam
  :ensure t ;; 自动安装
  :custom
  (org-roam-directory "~/repos/notes/") ;; 默认笔记目录, 提前手动创建好
  (org-roam-dailies-directory "daily/") ;; 默认日记目录, 上一目录的相对路径
  (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle) ;; 显示后链窗口
         ("C-c n u" . org-roam-ui-mode)) ;; 浏览器中可视化
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map) ;; 日记菜单
  :config
  (require 'org-roam-dailies)  ;; 启用日记功能

  ;;============= 新增内容 =================
  ;; 第四步: 用 org-roam 生成文献笔记,  放在 org-roam 的 use-package 代码块 :config 关键字之后
  ;; 下面的 (setq my/ref-template ...) 可以放到 use-package 代码块之外
  (setq my/ref-template
        (concat "#+FILETAGS: reading research \n"
                "- tags :: %^{keywords} \n"
                "* %^{title}\n"
                ":PROPERTIES:\n"
                ":Custom_ID: %^{citekey}\n"
                ":URL: %^{url}\n"
                ":AUTHOR: %^{author-or-editor}\n"
                ":NOTER_DOCUMENT: ~/Nutstore/1/Nutstore/Zotero-Library/%^{citekey}.pdf\n"
                ":NOTER_PAGE:\n"
                ":END:"))
  (add-to-list 'org-roam-capture-templates
               `("r" "Zotero 文献模板" plain ; 文献笔记模板
                 ,my/ref-template
                 :target
                 (file+head "ref/${citekey}.org" "#+title: ${title}\n")))
  ;;============= 新增内容结束 =================
  (org-roam-db-autosync-mode)) ;; 启动时自动同步数据库
```


### 模板详解 {#模板详解}

```elisp
(setq my/ref-template
      (concat "#+FILETAGS: reading research \n"
              "- tags :: %^{keywords} \n"
              "* %^{title}\n"
              ":PROPERTIES:\n"
              ":Custom_ID: %^{citekey}\n"
              ":URL: %^{url}\n"
              ":AUTHOR: %^{author-or-editor}\n"
              ":NOTER_DOCUMENT: ~/Nutstore/1/Nutstore/Zotero-Library/%^{citekey}.pdf\n"
              ":NOTER_PAGE:\n"
              ":END:"))
(add-to-list 'org-roam-capture-templates
             `("r" "Zotero 文献模板" plain ; 文献笔记模板
                ,my/ref-template
                :target
                (file+head "ref/${citekey}.org" "#+title: ${title}\n")))
```

-   <kbd>\n</kbd> 表示换行符 (一个短字符串就是一行)
-   需要手动保持一致的部分:
    -   <kbd>NOTER_DOCUMENT:</kbd> 后面接 PDF 文件目录与 `zot_pdf` 一致
    -   <kbd>file-head</kbd> 后面文献笔记默认文件夹与 `org_refs` 一致
-   <kbd>* %^{title}</kbd> 之后的内容是 `org-noter` 识别的关键字, 不建议修改
-   自由发挥的部分:
    -   <kbd>#+FILETAGS:</kbd>  `org-roam` 笔记标签, 会显示在 <kbd>C-c n f</kbd> 搜索界面并可搜索.  以空格分隔. 可以根据自己需要设置
    -   <kbd>- tags ::</kbd> 存放 Zotero 文献关键字
