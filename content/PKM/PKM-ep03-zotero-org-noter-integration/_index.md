+++
title = "用org-roam管理Zotero文献笔记"
draft = false
weight = 30
+++

## 实现功能 {#实现功能}

-   `Emcas` 中读取 `Zotero` 文献数据库并进行引用
-   每一条引用的 `Zotero` 文献建立一则 `org-roam` 笔记
-   将上述笔记作为 `org-noter` 笔记文件, 在 `Emacs` 中阅读 `Zotero` 文献的 `pdf` 附件


## 实现方法 {#实现方法}

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


## 代码 {#代码}

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


## 模板详解 {#模板详解}

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
