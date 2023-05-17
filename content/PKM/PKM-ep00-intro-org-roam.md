+++
title = "什么是双链笔记与 org-roam 初体验"
draft = false
+++

## 什么是笔记? {#什么是笔记}

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


## 信息提取 {#信息提取}

从笔记高效地提取信息很难

-   纸质笔记
    -   优点: 简单
    -   缺点: 不能检索
-   电子笔记: `LaTeX`, `org`, `markdown` 等
    -   优点: 可全文搜索, 读取信息能力大大提高
    -   缺点: 思考的过程, 知识间的联系难以体现.


## 传统笔记的局限性 {#传统笔记的局限性}

-   自上而下的树状结构
    -   树状结构无处不在: 图书馆目录, 文件夹, 书本章节段落
-   缺陷
    -   输入: 初始分类难
    -   输出: 不利于发现联系
-   传统笔记的完美形式就是一本书
-   但是现代人遇到问题第一反应不是去翻书, 而是用搜索引擎或 ChatGPT.


## 理想的笔记模型 {#理想的笔记模型}

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


## 为什么选择 `org-roam` {#为什么选择-org-roam}

... 而不是 ~~Roam Research~~, ~~Obsedian~~, ~~Logseq~~, ~~Notion~~, ...


### 我个人的笔记需求 {#我个人的笔记需求}

-   简洁但强大的笔记功能, 包含 LaTeX 数学公式, 交叉引用
-   支持双链笔记
-   能与文献管理软件如 `Zotero` 整合
-   可视化界面; 易于导出为其它形式


### 笔记软件比较 {#笔记软件比较}

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


## 个人知识库的构建 {#个人知识库的构建}

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


## 系列视频内容 {#系列视频内容}

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


## org-roam 安装 {#org-roam-安装}

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


## 基本设置 {#基本设置}

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


## 创建笔记及链接 {#创建笔记及链接}

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


## 优化 minibuffer 补全界面 {#优化-minibuffer-补全界面}

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


## 同步 {#同步}

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


## 结语 {#结语}
