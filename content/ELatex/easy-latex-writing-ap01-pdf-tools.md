+++
title = "Emacs 最强内置 pdf阅读器 pdf-tools 简介"
draft = false
+++

## 使用 `pdf-tools` 的理由 {#使用-pdf-tools-的理由}

在用 Emacs 编写 LaTeX 文档的过程中, 你是否...

-   预览 pdf 需要来回在编辑器和 pdf 阅读器之间切换?
-   pdf 阅读器想实现一些新功能?
-   想给 pdf 阅读器的常用功能定义新的快捷键?

又或者, 你想用 Emacs 做读书笔记, 需要同时:

-   输入大量的数学符号
-   对 pdf 文件进行批注
-   同步 Emacs 笔记文件和 pdf 文件批注的位置

`pdf-tools` 可以完美实现这些目标.


## `pdf-tools` 的优点 {#pdf-tools-的优点}

与 `DocView` (Emacs 中内置的 pdf 阅读器) 比较

-   `DocView`: 不清晰, 阅读效果差, 读取速度慢
-   `pdf-tools`:
    -   速度快, 图片渲染效果好
    -   正常鼠标操作 + 大量 (可自定义) 快捷键


## 演示 {#演示}


### 功能: {#功能}

-   基础的 pdf 阅读功能应有尽有, 包括超链接跳转和返回, 展开目录等
-   与 `auctex` 配合使用, 支持对编译后 pdf 进行正向/反向搜索
-   pdf 批注, 高亮, 下划线 (可保存在 pdf 文件上)


### 使用场景 {#使用场景}

-   编写 `latex` 文档
-   配合 `org-noter` 在 pdf 上做读书笔记


## 安装流程 {#安装流程}

分为两部分


### Emacs 包的安装 {#emacs-包的安装}

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


### `epdfinfo.exe` 的安装 {#epdfinfo-dot-exe-的安装}

`epdfinfo.exe` 及其它一些依赖文件 (例如 <kbd>libpopper-&lt;version&gt;.dll</kbd>) 可以帮助 Emacs 读取 pdf 文件

两种方法

-   把预编译好的文件直接放进 Emacs 的安装目录 (将上传一个可用的版本:   <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>)
-   利用 `msys2`


## 用 `msys2` 安装 `epdfinfo` {#用-msys2-安装-epdfinfo}


### 什么是 `msys2`? {#什么是-msys2}

可以将许多开源程序本地化编译为 Windows 程序的平台

优点

-   软件管理和升级方便
-   Emacs 一些高阶功能依赖的不少开源程序都能在上面下载
-   其它可以安装的开源软件:
    `Git`, `Emacs`, `texlive`, `gcc`, `python` ...


### 步骤 {#步骤}

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


## 基本配置 {#基本配置}


### 启动 `pdf-tools` {#启动-pdf-tools}

在 `init.el` 文件中加入

```elisp
(pdf-tools-install)
```

如果想延迟启动 (如打开 pdf 文件后再启动, 节省 Emacs 启动时间), 可以用下面的代码替换

```elisp
(pdf-loader-install)
```


### 配合 `AucTeX` 使用的配置 {#配合-auctex-使用的配置}

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


## 操作与个性化: 移动 {#操作与个性化-移动}

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


## 操作与个性化: 批注 {#操作与个性化-批注}

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


## 操作与个性化: 文档跳转 {#操作与个性化-文档跳转}

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


## 操作与个性化: 放缩 {#操作与个性化-放缩}

-   放大/缩小: <kbd>+</kbd> / <kbd>-</kbd>
-   放大到页宽/页高/屏幕: <kbd>W</kbd> / <kbd>H</kbd> / <kbd>P</kbd>
-   重置: <kbd>0</kbd>

打开 pdf 文件时自动放缩

```elisp
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 自动放大到页宽
```


## 其它可能出现的 bug {#其它可能出现的-bug}


### 无法进行高亮/划线等 {#无法进行高亮-划线等}

这可能是安装了 2023 年后 `pdf-tools` 的版本导致的. 可以从 <kbd>M-x package-list-package</kbd> 界面中确认是从 `melpa-stable` 中安装的


### 形同 `(invalid-function pdf-view-current-page)` 的错误信息 {#形同--invalid-function-pdf-view-current-page--的错误信息}

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


## 完整配置: {#完整配置}

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


## 相关资源 {#相关资源}

-   `pdf-tools` 的 `Github` 仓库: <https://github.com/vedang/pdf-tools>
-   `msys2` 官网 <https://www.msys2.org/>
-   `epdfinfo.exe` 可用版本:  <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>


## 使用 `pdf-tools` 的理由 {#使用-pdf-tools-的理由}

在用 Emacs 编写 LaTeX 文档的过程中, 你是否...

-   预览 pdf 需要来回在编辑器和 pdf 阅读器之间切换?
-   pdf 阅读器想实现一些新功能?
-   想给 pdf 阅读器的常用功能定义新的快捷键?

又或者, 你想用 Emacs 做读书笔记, 需要同时:

-   输入大量的数学符号
-   对 pdf 文件进行批注
-   同步 Emacs 笔记文件和 pdf 文件批注的位置

`pdf-tools` 可以完美实现这些目标.


## `pdf-tools` 的优点 {#pdf-tools-的优点}

与 `DocView` (Emacs 中内置的 pdf 阅读器) 比较

-   `DocView`: 不清晰, 阅读效果差, 读取速度慢
-   `pdf-tools`:
    -   速度快, 图片渲染效果好
    -   正常鼠标操作 + 大量 (可自定义) 快捷键


## 演示 {#演示}


### 功能: {#功能}

-   基础的 pdf 阅读功能应有尽有, 包括超链接跳转和返回, 展开目录等
-   与 `auctex` 配合使用, 支持对编译后 pdf 进行正向/反向搜索
-   pdf 批注, 高亮, 下划线 (可保存在 pdf 文件上)


### 使用场景 {#使用场景}

-   编写 `latex` 文档
-   配合 `org-noter` 在 pdf 上做读书笔记


## 安装流程 {#安装流程}

分为两部分


### Emacs 包的安装 {#emacs-包的安装}

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


### `epdfinfo.exe` 的安装 {#epdfinfo-dot-exe-的安装}

`epdfinfo.exe` 及其它一些依赖文件 (例如 <kbd>libpopper-&lt;version&gt;.dll</kbd>) 可以帮助 Emacs 读取 pdf 文件

两种方法

-   把预编译好的文件直接放进 Emacs 的安装目录 (将上传一个可用的版本:   <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>)
-   利用 `msys2`


## 用 `msys2` 安装 `epdfinfo` {#用-msys2-安装-epdfinfo}


### 什么是 `msys2`? {#什么是-msys2}

可以将许多开源程序本地化编译为 Windows 程序的平台

优点

-   软件管理和升级方便
-   Emacs 一些高阶功能依赖的不少开源程序都能在上面下载
-   其它可以安装的开源软件:
    `Git`, `Emacs`, `texlive`, `gcc`, `python` ...


### 步骤 {#步骤}

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


## 基本配置 {#基本配置}


### 启动 `pdf-tools` {#启动-pdf-tools}

在 `init.el` 文件中加入

```elisp
(pdf-tools-install)
```

如果想延迟启动 (如打开 pdf 文件后再启动, 节省 Emacs 启动时间), 可以用下面的代码替换

```elisp
(pdf-loader-install)
```


### 配合 `AucTeX` 使用的配置 {#配合-auctex-使用的配置}

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


## 操作与个性化: 移动 {#操作与个性化-移动}

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


## 操作与个性化: 批注 {#操作与个性化-批注}

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


## 操作与个性化: 文档跳转 {#操作与个性化-文档跳转}

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


## 操作与个性化: 放缩 {#操作与个性化-放缩}

-   放大/缩小: <kbd>+</kbd> / <kbd>-</kbd>
-   放大到页宽/页高/屏幕: <kbd>W</kbd> / <kbd>H</kbd> / <kbd>P</kbd>
-   重置: <kbd>0</kbd>

打开 pdf 文件时自动放缩

```elisp
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 自动放大到页宽
```


## 其它可能出现的 bug {#其它可能出现的-bug}


### 无法进行高亮/划线等 {#无法进行高亮-划线等}

这可能是安装了 2023 年后 `pdf-tools` 的版本导致的. 可以从 <kbd>M-x package-list-package</kbd> 界面中确认是从 `melpa-stable` 中安装的


### 形同 `(invalid-function pdf-view-current-page)` 的错误信息 {#形同--invalid-function-pdf-view-current-page--的错误信息}

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


## 完整配置: {#完整配置}

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


## 相关资源 {#相关资源}

-   `pdf-tools` 的 `Github` 仓库: <https://github.com/vedang/pdf-tools>
-   `msys2` 官网 <https://www.msys2.org/>
-   `epdfinfo.exe` 可用版本:  <https://www.jianguoyun.com/p/DTiBwxMQ856tCxiflP0EIAA>
