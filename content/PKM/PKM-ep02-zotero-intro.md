+++
title = "Zotero文献管理系统简介"
draft = false
+++

## 软件界面 {#软件界面}

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


## 向 Zotero 中加入文献 {#向-zotero-中加入文献}

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


## 功能小结 {#功能小结}

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


## 插件安装方法 {#插件安装方法}

-   在 <https://www.zotero.org/support/plugins> 上找到插件的 `.xpi` 文件并下载
-   打开 `Tools` -&gt; `Add Ons` 选择从文件进行安装
-   重启 `Zotero`


## `Better BibTeX` 设置 {#better-bibtex-设置}

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


## `ZotFile` 同步设置 {#zotfile-同步设置}

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


## 下期内容 {#下期内容}

在 `org-roam` 笔记流程中整合 `Zotero`, 包括

-   `Emcas` 中读取 `Zotero` 文献数据库并进行引用
-   每一条引用的 `Zotero` 文献建立一则 `org-roam` 笔记
-   将上述笔记作为 `org-noter` 笔记文件, 在 `Emacs` 中阅读 `Zotero` 文献的 `pdf` 附件
