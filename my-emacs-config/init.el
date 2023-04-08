;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1000 1000))

(defvar bootstrap-version)
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

(setq straight-use-package-by-default t) ;; 自动安装所有插件
(straight-use-package 'use-package) ;; 安装 use-package

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "emacs-config.org" user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(use-package no-littering
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))) ;; 设置自动保存文件路径

(use-package recentf
  :after no-littering
  :custom
  (recentf-exclude '(no-littering-var-directory
                     no-littering-etc-directory)) ;; 屏蔽临时文件
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :bind ("C-x C-r" . 'recentf-open-files)
  :config
  (recentf-mode 1))

(setq my/is-windows (eq system-type 'windows-nt))
(setq my/is-tablet (eq system-type 'gnu/linux))

(setq native-comp-deferred-compilation-deny-list '(".*pdf.*")) ;; 禁用 =pdf-tools= 有关文件的本地化编译

(prefer-coding-system 'utf-8) ;; 默认编码
(setq-default buffer-file-coding-system 'utf-8-unix) ;; 默认 EOL 设置

(if (eq system-type 'windows-nt)
    (progn
      (set-face-attribute 'default nil :font "Fira Mono" :height 160) ;; 默认字体及字号. Fira Mono 需要另行安装
      (set-fontset-font "fontset-default" 'han "Kaiti") ;; 中文默认字体
      (set-fontset-font "fontset-default" 'mathematical "Cambria Math") ;; 数学符号默认字体
      ))
(setq inhibit-compacting-font-caches t) ;; 汉字显示问题

(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono"  :height 150) ;; 固定间距字体. JetBrains Mono 需要另行安装
(set-face-attribute 'variable-pitch nil :font "Segoe Print" :height 160 :weight 'regular) ;; 可变间距字体

(defun my/pyim-probe-latex-mode ()
 "latex-mode 中的数学环境自动切换到英文输入."
 (and (eq major-mode 'latex-mode)
      (if (fboundp 'texmathp) (texmathp) nil)))

(use-package popup :defer t) ;; 选词框
(use-package pyim-wbdict :defer 2) ;; 五笔输入法
(use-package pyim 
  :defer 2
  :after pyim-wbdict
  :bind
  ("M-j" . 'pyim-convert-string-at-point)
  :config
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'wubi)
  (pyim-wbdict-v86-enable)
  (require 'popup)
  (setq pyim-page-tooltip 'popup)
  (setq-default pyim-punctuation-translate-p '(no));; 总是输入半角标点。
  (setq-default pyim-english-input-switch-functions
              '(pyim-probe-auto-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template
                pyim-probe-org-latex-mode
                my/pyim-probe-latex-mode))
  (setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation)))

(defun my/load-doom-theme (theme)
  "Disable active themes and load a Doom theme."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name)
                                                   (--select (string-prefix-p "doom-" it)))))))
  (ap/switch-theme theme)
  (set-face-foreground 'org-indent (face-background 'default)))

(defun my/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package modus-themes)
;; (setq modus-themes-common-palette-overrides
;;       `((bg-mode-line-active bg-red-subtle) 
;;         (fg-mode-line-active red-warmer)
;;         (border-mode-line-active unspecified) 
;;         (border-mode-line-inactive unspecified)
;;         (bg-region bg-lavender) 
;;         (fg-region unspecified)
;;         (fg-completion-match-0 fg-main)
;;         (fg-completion-match-1 fg-main)
;;         (fg-completion-match-2 fg-main)
;;         (fg-completion-match-3 fg-main)
;;         (bg-completion-match-0 bg-blue-intense)
;;         (bg-completion-match-1 bg-yellow-intense) 
;;         (bg-completion-match-2 bg-cyan-intense)
;;         (bg-completion-match-3 bg-red-intense)
;;         (comment yellow-cooler)
;;         (string green-cooler)
;;         (bg-paren-match bg-magenta-intense)
;;         ,@modus-themes-preset-overrides-intense))

;; (setq modus-themes-bold-construct t
;;       modus-themes-italic-construct t
;;       modus-themes-org-blocks 'tinted-background
;;       modus-themes-scale-headings t
;;       modus-themes-headings
;;       '((1 . (rainbow overline background 1.3))
;;         (2 . (rainbow background 1.2))
;;         (3 . (rainbow bold 1.15))
;;         (t . (semilight 1.1))))

;(load-theme 'modus-vivendi-tinted t)

(use-package all-the-icons
  :if (display-graphic-p)) ;; 需要另行安装字体
(use-package minions
  :hook doom-modeline-mode)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-unicode-fallback t)
  :config
  (custom-set-faces '(mode-line ((t (:height 0.85))))
                    '(mode-line-inactive ((t (:height 0.85))))))

(unless my/is-tablet
  (tool-bar-mode -1) ;; 禁用工具栏
  (scroll-bar-mode -1)) ;; 禁用滚动条
(menu-bar-mode -1) ;; 禁用菜单栏

(column-number-mode)
(visual-line-mode 1)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                pdf-view-mode-hook
                eww-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun my/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-word (- arg))))

(use-package vertico
  :defer 1
  :custom
  (verticle-cycle t)
  :config
  (vertico-mode)
  :bind (:map minibuffer-local-map
              ("M-h" .  my/minibuffer-backward-kill)))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :defer 1
  :config
  (marginalia-mode))

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package keycast
:after doom-modeline
:config
(setq keycast-mode-line-insert-after '(:eval (doom-modeline-format--main)))
(add-to-list 'global-mode-string '("" keycast-mode-line))
:hook
(doom-modeline-mode . keycast-mode-line-mode))

(use-package which-key
  :defer 2
  :diminish which-key-mode
  :custom (which-key-idle-delay 1)
  :config (which-key-mode))

(use-package helpful
  :defer 3
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-h x" . #'helpful-command)
   ("C-c C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)))

(use-package hydra
  :defer 5
  :init
  (require 'hydra))

(use-package evil :defer t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

(setq prettify-symbols-unprettify-at-point t) ;; 光标附近自动展开

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-in-previous-window)
        . ((mode . (org-mode helpful-mode help-mode)))))

(use-package popper
  :defer 3
  :bind (("C-M-'" . popper-toggle-latest)
         ("M-'" .  popper-cycle)
         ("C-M-;" . popper-toggle-type))
  :custom
  (popper-reference-buffers '("^\\*eshell\\*"
                              "^vterm"
                              help-mode
                              helpful-mode
                              compilation-mode
                              "\\*Messages\\*"
                              "Output\\*$"))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package ace-window
  :defer 2
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'global)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (ace-window-display-mode 1))

(use-package savehist
  :defer 2
  :config (savehist-mode))

(use-package super-save
  :defer 2
  :custom
  (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1))

(use-package saveplace
  :defer 2
  :config
  (save-place-mode 1))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(if (eq system-type 'windows-nt)
  (setq temporary-file-directory "~/AppData/Local/Temp/"))

(setq safe-local-variable-values
      '((code . utf-8)
        (eval setq-local org-roam-db-location (file-truename "./org-roam.db")) ;; 允许多个 org-roam 目录
        (eval setq-local org-roam-directory (file-truename "./"))))

(use-package cdlatex
  :straight (cdlatex
             :local-repo "../../lisp/" ;; 从 <user-dir>/lisp/ 读入 cdlatex-config.el
             :type nil)
  :defer 6
  :config ;; 导入 cdlatex 自定义设置
  (setq cdlatex-paired-parens "$[{(")
  (load-file (expand-file-name "cdlatex-config.el" user-emacs-directory)))

(setq outline-minor-mode-prefix [(control o)])

(add-to-list 'display-buffer-alist '(".*tex.*" (display-buffer-reuse-window)))
(add-to-list 'display-buffer-alist '(".*pdf.*" (display-buffer-reuse-window)))

(defun my-latex-hook ()
  (turn-on-cdlatex) 
  (turn-on-reftex) 
  (outline-minor-mode) ;; 大纲预览
  (outline-hide-body) ;; 启动时折叠文件
  (prettify-symbols-mode t)) ;; prettify 数学符号

(use-package pdf-tools
      :straight (:host github :repo "vedang/pdf-tools"
                       :branch "feature/emacs-26.3" ;; 不使用最新版
                       :build (:not native-compile)) ;; 禁用本地化编译
  :mode ("\\.pdf\\'" . pdf-view-mode) ;; pdf 文件默认打开方式
  :bind
  (:map pdf-view-mode-map
   ("d" . pdf-view-next-page-command)
   ("a" . pdf-view-previous-page-command)
   ("s" . pdf-view-scroll-up-or-next-page)
   ("w" . pdf-view-scroll-down-or-previous-page)
   :map pdf-history-minor-mode-map
   ("b" . pdf-history-backward)
   :map pdf-annot-minor-mode-map
   ("C-a a" . pdf-annot-add-highlight-markup-annotation)
   ("C-a s" . pdf-annot-add-squiggly-markup-annotation)
   ("C-a u" . pdf-annot-add-underline-markup-annotation)
   ("C-a d" . pdf-annot-delete))
  :custom
  (pdf-view-midnight-colors '("#000000" . "#9bCD9b")) ;; 夜间模式设置绿色底色
  :config
  (require 'pdf-annot) ;; 设置 pdf-annot-mimor-mode-map 必须
  (require 'pdf-history) ;; 设置 pdf-history-minor-mode-map 必须
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window) ;; 默认适应页宽
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode) ;; 默认夜间模式
  (pdf-tools-install))

(use-package tex
  :defer 8
  :straight auctex
  :custom
  (TeX-parse-self t) ;; 自动解析 tex 文件found
  (TeX-PDF-mode t) 
  (TeX-DVI-via-PDFTeX t)
  ;; 正向与反向搜索设置
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (TeX-view-program-selection '((output-pdf "PDF Tools"))) ;; 使用 pdf-tools 预览 pdf
  (TeX-source-correlate-start-server t)
  :config
  ;; 设置 LaTeX 语法高亮颜色及字体大小
  (require 'font-latex)
  (set-face-attribute 'font-latex-math-face nil :foreground "#f78c6c" :font "Cambria Math" :height 1.15) ;; 数学符号
  (set-face-attribute 'font-latex-script-char-face nil :foreground "#c792ea") ;; 上下标字符^与_
  (set-face-attribute 'font-latex-sedate-face nil :foreground "#ffcb6b" :font "Cambria Math" :height 1.15) ;; 关键字
  (setq-default TeX-master t) ;; 默认主文件询问
  (add-hook 'LaTeX-mode-hook 'my-latex-hook) ;; 加载LaTeX模式设置
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer) ;; 编译后更新 pdf 文件
  (load-file (expand-file-name "auctex-config.el" user-emacs-directory))) ;; 加载AucTeX变量设置

(use-package org-bullets ;; 标题符号
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸" "✿" "◉" "○" "✸" "✿")))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun my/follow-link-at-current-window () 
  (interactive)
  (let ((org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                      (vm-imap . vm-visit-imap-folder-other-frame)
                                      (gnus . gnus)
                                      (file . find-file)
                                      (wl . wl-other-frame)))))

    (org-open-at-point)))
(defun my/follow-link-at-current-window-mouse (event)
  (interactive (list last-command-event))
  (posn-set-point (event-end event))
  (let ((org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                      (vm-imap . vm-visit-imap-folder-other-frame)
                                      (gnus . gnus)
                                      (file . find-file)
                                      (wl . wl-other-frame)))))
    (org-open-at-point)))

(defun my/insert-inline-OCDL ()
  (interactive)
  (insert "\\(")
  (save-excursion (insert " \\)" )))
(defun my/insert-dollar-OCDL ()
  (interactive)
  (insert "$")
  (save-excursion (insert "$" )))
(defun my/insert-bra-OCDL ()
  (interactive)
  (insert "(")
  (save-excursion (insert ")" )))
(defun my/insert-sq-bra-OCDL ()
  (interactive)
  (insert "[")
  (save-excursion (insert "]" )))
(defun my/insert-curly-bra-OCDL ()
  (interactive)
  (insert "{")
  (save-excursion (insert "}" )))

(use-package org-appear :hook org-mode)

(use-package org-download
  :defer t
  :custom
  (org-download-heading-lvl 1) ;; 以一级标题作为图片文件夹
  :after org
  :bind (:map org-mode-map
              ("C-c i y" . org-download-yank)
              ("C-c i d" . org-download-delete)
              ("C-c i e" . org-download-edit))
  :config
  ;; 用文件名作为文件夹
  (defun my-org-download-method (link) 
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          (dirname (concat "./img/" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
      (setq org-download-image-dir dirname)
      (make-directory dirname t)
      (expand-file-name (funcall org-download-file-format-function filename) dirname)))
  (setq org-download-method 'my-org-download-method)
  ;; 在 Windows 系统下修复过时的convert.exe; 注意: 用户名文件夹不能含有空格!
  (defun my/org-download-clipboard ()
    (interactive)
    (let ((filename (expand-file-name "screenshot.png" temporary-file-directory)))
      (shell-command-to-string (format "magick clipboard: %s" filename))
      (when (file-exists-p filename)
        (org-download-image filename)
        (delete-file filename))))
  ;; 绑定剪贴板图片的快捷键
  (if my/is-windows
      (define-key org-mode-map (kbd "C-M-y") #'my/org-download-clipboard)
    (define-key org-mode-map (kbd "C-M-y") #'org-download-clipboard)))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(defun my-org-hook ()
  (org-indent-mode) ;; 自动缩进
  (variable-pitch-mode 1) ;; 可变间距字体
  (org-cdlatex-mode) ;; LaTeX 公式
  (visual-line-mode 1))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(use-package org
  :defer 10
  :custom
  (org-M-RET-may-split-line nil)
  (org-hide-emphasis-markers t) ;; 默认隐藏标记, 如=,~,*,_ 等; 与 org-appear 配合
  (org-startup-folded 'content) ;; 开启时折叠大纲
  (org-ellipsis " ▾") ;; 用小箭头代替...表示折叠
  (org-format-latex-options
   '(:foreground default :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.5 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))) ;; 公式预览设置
  ;; 以下为 LaTeX 语法高亮设置
  (org-highlight-latex-and-related '(native latex entities))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)

  (org-priority-lowest ?E) ;; org-agenda 的优先级设为A-E
  (org-priority-default ?D) ;; org-agenda 的默认优先级设为D
  (org-structure-template-alist ;; 用 org-tempo 快速插入代码块
   '(("el" . "src elisp")
     ("la" . "src latex")
     ("sh" . "src shell")
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
  :bind
  (:map org-mode-map
        ("C-c o" . my/follow-link-at-current-window) ;; 在当前窗口打开 org 文件
        ("C-<down-mouse-1>" . my/follow-link-at-current-window-mouse) ;; Ctrl+鼠标点击时, 在当前窗口打开 org 文件
        ("C-<drag-mouse-1>" . my/follow-link-at-current-window-mouse)
        :map org-cdlatex-mode-map ;; 实现配对输入$, (, [, { 及 \( 与 \)
        ("$" . my/insert-dollar-OCDL)
        ("C-$" . my/insert-inline-OCDL)
        ("(" . my/insert-bra-OCDL)
        ("[" . my/insert-sq-bra-OCDL)
        ("{" . my/insert-curly-bra-OCDL))
  :config
  (require 'org-tempo) ;; 保证 org-structure-template-alist 可用
  ;; 标题字体大小优化
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))
  ;; org 字体美化
  (require 'org-faces)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :foreground nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-block-end-line nil :foreground nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-property-value nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil  :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-drawer nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-document-info-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq org-fontify-quote-and-verse-blocks t) ;; 启用 org-qoute 变量为 quote 设置不同的字体
  (set-face-attribute 'org-quote nil :inherit 'fixed-pitch)
  (require 'org-indent)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

  (add-hook 'org-mode-hook 'my-org-hook))

(use-package org-present
  :defer t
  :config
  (defun my/org-present-prepare-slide (buffer-name heading)
    (org-overview)  ;; 仅显示顶层标题Show only top-level headlines
    (org-show-entry);; 展开当前标题Unfold the current entry
    (org-show-children))   ;; 显示当前子标题

  (defun my/org-present-start () ;; 开始幻灯片的设置
    (setq visual-fill-column-width 110
      visual-fill-column-center-text t) ;; 调整显示界面
    ;; 调整字体大小
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq header-line-format " ") ;; 在标题前加入空行
    (org-display-inline-images) ;; 显示图片
    (flyspell-mode 0) ;; 禁用拼写检查 (防止标红影响效果)
    (read-only-mode 1) ;; 只读模式
    )

  (defun my/org-present-end () ;; 重置上述设置
    (setq-local face-remapping-alist 
                '((default variable-pitch default)))      
    (setq header-line-format nil) 
    (org-remove-inline-images)
    (org-present-small)
    (flyspell-mode t)
    (read-only-mode 0))

  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide))

(use-package org-noter
  :defer t
  :straight (:host github
                   :repo "org-noter/org-noter"
                   :files ("*.el" "modules/*.el"))
  :bind
  (("C-c n n" . org-noter)
   :map org-noter-doc-mode-map
   ("M-e" . org-noter-insert-precise-note)
   ("e" . org-noter-insert-note))
  :custom
  (org-noter-highlight-selected-text t)
  (org-noter-notes-search-path '("~/repos/notes/ref/"))
  (org-noter-doc-split-fraction '(0.5 . 0.5))
  (org-noter-auto-save-last-location t))

(use-package org-ref
  :defer t
  :bind (:map org-mode-map
              ("C-c (". org-ref-insert-label-link)
              ("C-c )". org-ref-insert-ref-link)))

(setq zot_bib '("~/NutStore/1/NutStore/Zotero-Library/Better BibTeX Export/My Library.bib"
                "~/NutStore/1/NutStore/Zotero-Library/Better BibTeX Export/My Library-desktop.bib") ;; Zotero .bib 文件
      org_notes "~/repos/notes/ref/" ;; org-roam 文献笔记目录
      zot_pdf "~/NutStore/1/NutStore/Zotero-Library") ;; Zotero 同步文件夹
(setq my/daily-note-filename "%<%Y-%m-%d>.org" ;; 日记默认文件名
      my/daily-note-header "#+title: %<%Y-%m-%d %a>\n#+SETUPFILE: ~/repos/notes/latex-preamble.org\n\n[[roam:%<%Y-%B>]]\n\n") ;; 日记文件头

(use-package org-roam
  :defer 12
  :custom
  (org-roam-directory "~/repos/notes/") ;; 默认笔记目录
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template ;; 搜索节点信息显示
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-dailies-directory "daily/") ;; 默认日记目录
  (org-roam-dailies-capture-templates ;; 日记默认模板
   `(("d" "default" entry "* %?" ;; 普通条目
      :target (file+head ,my/daily-note-filename
                         ,my/daily-note-header))
     ("t" "task" entry "* TODO %?\n  %U\n  %a\n  %i" ;; 待办
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Tasks"))
      :empty-lines 1) 
     ("j" "journal" entry "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n" ;; 研究日志
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Log")))
     ("m" "meeting" entry "* %<%I:%M %p> - Meeting with %^{whom}  :meetings:\n\n%?\n\n" 
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Meeting")))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n k" . orb-insert-link)
         ("C-c n a" . orb-note-actions)
         ("C-c n d" . my/org-roam-jump-menu/body)
         ("C-c n P" . my/org-roam-insert-new-project)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n u" . org-roam-ui-mode)
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (define-key org-roam-mode-map [mouse-1] (kbd "C-u <return>")) ;; org-roam-buffer 界面左键相当于C-u <return>
  (setq org-roam-capture-templates  ;; org-roam 笔记模板
        '(("d" "default" plain "- tag :: \n %?" ;; 普及模板
           :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title} \n#+SETUPFILE: ./latex-preamble.org")
           :unnarrowed t)
          ("r" "bibliography reference in pdfs" plain ;; 文献模板
           "#+FILETAGS: reading research \n - tags :: %^{keywords} \n* %^{title}
:PROPERTIES:\n:Custom_ID: %^{citekey}\n:URL: %^{url}\n:AUTHOR: %^{author-or-editor}\n:NOTER_DOCUMENT: ~/Nutstore/1/Nutstore/Zotero-Library/%^{citekey}.pdf\n:NOTER_PAGE:\n:END:"      
           :target
           (file+head "ref/${citekey}.org" "#+title: ${title}\n#+SETUPFILE: ../latex-preamble.org\n"))
          ("a" "article/post/blog/discussion" plain ;; 其它阅读模板
           "#+FILETAGS: reading \n- tags :: \n- sources ::\n"      
           :target
           (file+head "article/%<%Y%m%d%H%M%S>-reading-${slug}.org" "#+title: ${title}\n#+SETUPFILE: ../latex-preamble.org\n"))
          ("s" "Seminar notes" plain "#+FILETAGS: seminar\n- title:\n- speaker:\n- event:\n- tags ::" ;; 学术报告模板
           :target
           (file+head "seminar/%<%Y%m%d>-seminar-${slug}.org" "#+title: ${title}\n#+SETUPFILE: ../latex-preamble.org\n#+filetags: seminar"))))
  (require 'org-roam-dailies) 
  (org-roam-db-autosync-mode) ;; 自动同步数据库
  (my/org-roam-refresh-agenda-list) ;; 自动收集 project 文件中的待办事项
  (add-to-list 'org-after-todo-state-change-hook ;; 将完成的待办事项备份至日记
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today)))))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :defer t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package helm-bibtex
  :defer t
  :custom
  (bibtex-completion-notes-path org_notes)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-insert-interface 'helm-bibtex)
  (orb-insert-link-description 'citekey)
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
(add-hook 'org-roam-mode-hook 'visual-line-mode) ;; 自动换行

(defvar my/org-roam-project-template ;; 项目笔记模板
  '("p" "project" plain "** TODO %?"
    :if-new (file+head+olp "%<%Y%m%d%H>-${slug}.org"
                           "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n"
                           ("Tasks"))))
(defun my/org-roam-filter-by-tag (tag-name) ;; 按 tag 搜索笔记; 需要 lexical binding
  (lambda (node)
    (member tag-name (org-roam-node-tags node)))) 
(defun my/org-roam-list-notes-by-tag (tag-name) ;; 按 tag 显示笔记
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
  capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-insert-new-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; Select a project file to open, creating it if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates (list my/org-roam-project-template)))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

(defun my/org-roam-capture-task ()
(interactive)
;; 新增项目后, 更新 org-agende 文件列表
(add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
;; 新增待办
(org-roam-capture- :node (org-roam-node-read
                          nil
                          (my/org-roam-filter-by-tag "Project"))
                   :templates (list my/org-roam-project-template)))

(defun my/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defun my/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defhydra my/org-roam-jump-menu (:hint nil)
  "
^Dailies^        ^Capture^       ^Jump^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^
"
  ("t" org-roam-dailies-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" my/org-roam-goto-month)
  ("e" my/org-roam-goto-year)
  ("c" nil "cancel"))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))
    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package magit
  :commands magit-status
  :autoload  magit-get-current-branch
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package perspective
  :defer 2
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :config
  (unless (equal persp-mode t)
    (persp-mode))
  (setq persp-state-default-file (expand-file-name ".persp-save" user-emacs-directory)))

(use-package flyspell
  :defer t
  :straight (:local-repo "../../lisp/" :type nil)
  :hook (org-mode LaTeX-mode) 
  :config
  (setq ispell-personal-dictionary (expand-file-name ".ispell" user-emacs-directory))) ;; 默认个人字典

(use-package shell
  :defer t
  :config 
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(use-package subed
  :straight
  (:host github :repo "sachac/subed" :files ("subed/*.el"))
  :config
  ;; Remember cursor position between sessions
  (add-hook 'subed-mode-hook 'save-place-local-mode)
  ;; Break lines automatically while typing
  (add-hook 'subed-mode-hook 'turn-on-auto-fill)
  ;; Break lines at 40 characters
  (add-hook 'subed-mode-hook (lambda () (setq-local fill-column 40)))
  ;; Some reasonable defaults
  (add-hook 'subed-mode-hook 'subed-enable-pause-while-typing)
  ;; As the player moves, update the point to show the current subtitle
  (add-hook 'subed-mode-hook 'subed-enable-sync-point-to-player)
  ;; As your point moves in Emacs, update the player to start at the current subtitle
  (add-hook 'subed-mode-hook 'subed-enable-sync-player-to-point)
  ;; Replay subtitles as you adjust their start or stop time with M-[, M-], M-{, or M-}
  (add-hook 'subed-mode-hook 'subed-enable-replay-adjusted-subtitle)
  ;; Loop over subtitles
  (add-hook 'subed-mode-hook 'subed-enable-loop-over-current-subtitle)
  ;; Show characters per second
  (add-hook 'subed-mode-hook 'subed-enable-show-cps))

(use-package python-mode
    :defer t)

(use-package cython-mode
    :mode "\\.pyx\\'"
    :config
    (add-hook 'cython-mode-hook #'flycheck-mode))

(use-package elpy
    :defer t
    :config
    (advice-add 'python-mode :before 'elpy-enable)
    (setq elpy-modules
          '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))

(use-package markdown-mode
    :mode "\\.md\\'"
    :custom
    (markdown-command "pandoc -F pandoc-crossref -F pandoc-citeproc -f markdown -t html -s --mathjax")
    :config
    (add-to-list 'process-coding-system-alist '("pandoc" utf-8-dos . utf-8-dos))
    (add-hook 'markdown-mode-hook 'turn-on-cdlatex)
    (markdown-enable-math t))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package esup
  :defer t
  :config
  (setq esup-depth 0))

(defun efs/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                     (time-subtract after-init-time before-init-time)))
             gcs-done))
  (add-hook 'emacs-startup-hook #'efs/display-startup-time)

(use-package shrface
  :defer t
  :after eww
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package eww
  :defer t
  :custom
  (eww-retrieve-command nil)
  (eww-search-prefix "https://google.com/search?q=")
  (shr-use-fonts nil)
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (add-hook 'eww-mode-hook 'visual-line-mode)
  (require 'shrface))

(setq initial-frame-alist '((fullscreen . maximized))) ;; 全屏
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ;; 禁用Emacs写入自定义变量
(setq gc-cons-threshold (* 2 1000 1000))
