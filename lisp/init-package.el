;;; init-package.el --- setttings for packages
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package restart-emacs)

(use-package drag-stuff
             :bind (("<M-up>" . drag-stuff-up)
                    ("<M-down>" . drag-stuff-down)))

(use-package ivy :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))
(ivy-mode 1)

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-isearch-backward))
 :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c f" . counsel-recentf)
	 ("C-c g" . counsel-git)))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
	:custom
	((all-the-icons-ivy-rich-icon t)
	 (all-the-icons-ivy-rich-color-icon t)
	 (all-the-icons-ivy-rich-icon-size 1.0)
	 (all-the-icons-ivy-rich-project t)
	 (all-the-icons-ivy-rich-field-width 80)
	 )
  :init (ivy-rich-mode 1))

(use-package which-key
  :defer nil
  :config (which-key-mode))

(use-package ivy-posframe
  :init (setq ivy-posframe-display-functions-alist
			  '((swiper . ivy-posframe-display-at-frame-center)
				(conmplete-symbol . ivy-posframe-display-at-point)
				(counsel-M-x . ivy-posframe-display-at-frame-center)
				(counsel-find-file . ivy-posframe-display-at-frame-center)
				(ivy-switch-buffer . ivy-posframe-display-at-frame-center)
				(t . ivy-posframe-display-at-frame-center)))
                (ivy-posframe-mode 1))

(use-package ace-window 
             :bind (("M-o" . 'ace-window)))

(use-package indent-bars
	:custom
		(indent-bars-treesit-support t)
    (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))

		(indent-bars-no-descend-lists t)
	  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				       if_statement with_statement while_statement)))
 	  (indent-bars-treesit-ignore-blank-lines-types '("module"))

		(indent-bars-treesit-wrap '((lua
                             expression_list function_declaration if_statement
                             elseif_statement else_statement while_statement for_statement
                             repeat_statement comment)))

		(indent-bars-treesit-wrap '((rust arguments parameters)))
		(indent-bars-treesit-scope '((rust trait_item impl_item 
                                   macro_definition macro_invocation 
                                   struct_item enum_item mod_item 
                                   const_item let_declaration 
                                   function_item for_expression 
                                   if_expression loop_expression 
                                   while_expression match_expression 
                                   match_arm call_expression 
                                   token_tree token_tree_pattern 
                                   token_repetition)))
		
		:hook ((prog-mode) . indent-bars-mode))


(setq
    indent-bars-no-descend-lists t
    indent-bars-color '(highlight :face-bg t :blend 0.7)
    indent-bars-pattern "|"
    indent-bars-width-frac 0.25
		indent-bars-highlight-current-depth '(:color "red" :blend 0.7)
		indent-bars-starting-column 0
  )

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq-local 
			indent-tabs-mode t ; make sure tabs-based indenting is on, even if we disable it globally
			indent-bars-no-descend-lists nil) ; elisp is mostly continued lists!  allow bars to descend inside
	    (indent-bars-mode 1)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq-local 
			indent-tabs-mode t ; make sure tabs-based indenting is on, even if we disable it globally
			indent-bars-no-descend-lists nil) ; elisp is mostly continued lists!  allow bars to descend inside
	    (indent-bars-mode 1)))


(use-package all-the-icons
  :ensure t
  :config
  ;; 注意：安装完成后，请执行命令 M-x all-the-icons-install-fonts，
  ;; 这将会自动下载并安装 all-the-icons 所需字体，如 Font Awesome、Material Design Icons 等，
  ;; 否则图标可能显示为乱码或问号。
  (unless (file-exists-p (expand-file-name "fonts/all-the-icons.ttf" user-emacs-directory))
    (message "请运行 M-x all-the-icons-install-fonts 安装 all-the-icons 字体。")))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))


;; 安装 meow
(use-package meow
  :ensure t)


;; --------------------------------------------
;; 配置 Projectile：项目管理工具，用于快速在项目间跳转、搜索文件、查找符号等
;; --------------------------------------------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)  ;; 全局启用 Projectile 模式
  :config
  ;; 根据实际情况设置你的项目目录，可以添加多个目录
  (setq projectile-project-search-path '("~/projects/"))
  ;; 结合 Ivy 使用更加流畅的补全体验
  (setq projectile-completion-system 'ivy)
  ;; 建议绑定 Projectile 的命令前缀，例如 "C-c p"
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (message "Projectile 已启动"))


(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode))  ;; 将 C-; 绑定为 iedit-mode 开关
  :config
  ;; 可选配置：如果你不希望 iedit 额外绑定第二个切换键可以设置为 nil
  (setq iedit-toggle-key-default nil)
  ;; 为 iedit 模式添加一个退出按键（按 C-c C-c 退出 iedit 模式）
  (define-key iedit-mode-keymap (kbd "C-c C-c") 'iedit-done))

;; 自定义 iedit-dwim 函数：
(defun iedit-dwim (arg)
  "启动 iedit 模式。
如果传入前缀参数，则在整个缓冲区中匹配，否则仅匹配当前函数内部的内容。"
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-defun)
        (iedit-mode)))))

(use-package multiple-cursors
  :ensure t)

;; ================================
;; Org-Roam 设置
;; ================================

;; 设置 org-roam 主目录为 ~/.emacs.d/org-roam/
(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (expand-file-name "org-roam/" user-emacs-directory)) ;; ~/.emacs.d/org-roam/
  (org-roam-dailies-directory "daily/") ;; ~/.emacs.d/org-roam/daily/
  (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n u" . org-roam-ui-mode))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  ;; 自动创建目录（如果不存在）
  (let* ((main-dir (expand-file-name "org-roam/" user-emacs-directory))
         (daily-dir (expand-file-name "daily/" main-dir)))
    (unless (file-directory-p main-dir)
      (make-directory main-dir t))
    (unless (file-directory-p daily-dir)
      (make-directory daily-dir t)))
  ;; 启用数据库自动同步
  (org-roam-db-autosync-mode))

;; 可视化 UI（浏览器中查看笔记图谱）
(use-package org-roam-ui
  :straight t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

;; ================================
;; Org-mode 标题与列表美化 —— org-modern
;; ================================

;; 基础 Org 设置
(setq org-startup-indented t              ;; 自动缩进
      org-hide-leading-stars t            ;; 隐藏原生星号前缀
      org-ellipsis " ▼"                   ;; 折叠提示符
      org-pretty-entities t               ;; 显示符号替换（√ → ✓ 等）
      org-hide-emphasis-markers t         ;; 隐藏 *强调* 的星号
      org-image-actual-width '(300))      ;; 图像宽度

;; 安装并启用 org-modern
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :init
  (setq
   ;; headline bullets 替代符号（依次是一级到五级）
   org-modern-star '("◉" "○" "✿" "✾" "❀")
   ;; 项目列表符号替代
   org-modern-list '((?- . "–") (?+ . "⁃") (?* . "•"))
	 ;; 使用漂亮的 Unicode 标题符号
   org-modern-headline '("◉" "○" "✿" "✸" "✜" "✢" "✧" "◆")
   ;; 设置 TODO 样式
   org-modern-todo-faces
   '(("TODO"  . (:inherit (bold org-todo) :background "#ffcccc" :foreground "#990000"))
     ("DONE"  . (:inherit (bold org-done) :background "#ccffcc" :foreground "#006600")))
   ;; 美化 checkbox
   org-modern-checkbox
   '((?X . "☑") (?- . "❍") (?\s . "☐"))
   ;; 水平分割线样式
   org-modern-horizontal-rule "──────────"
	 ;; 列表项美化符号
   org-modern-list '((43 . "➕") (45 . "➖") (42 . "•")) ; + - *
   ;; 代码块样式
   org-modern-block-fringe nil
   org-modern-block-name t
   ;; 表格横线样式
   org-modern-table-vertical 1
   org-modern-table-horizontal 0.2
   ;; 标签样式（例如 :tag:）
   org-modern-tag nil
	 ;; 时间戳格式化
   org-modern-timestamp t
   ;; 缩进线（可选）
   org-modern-indent nil))

;; 自动隐藏星号的 foreground（适配主题背景）
(defun my/org-modern-fix-org-hide-face (&rest _)
  "让 `org-hide` face 与当前背景一致，实现真正隐藏星号。"
  (when (facep 'org-hide)
    (let ((bg (face-background 'default nil 'default)))
      (set-face-foreground 'org-hide bg))))

;; 加入到主题切换后自动调用
(advice-add 'load-theme :after #'my/org-modern-fix-org-hide-face)
;; 初始加载时也运行一次
(my/org-modern-fix-org-hide-face)


;; org-download
(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method             'attach
        org-download-screenshot-method  "snippingtool"
        org-download-image-dir          (expand-file-name "org-images" user-emacs-directory))
  (org-download-enable))


;; --- 检查 site-lisp/fd 目录 ------------
(use-package dirvish
	:ensure t
	:defer nil  ;; 确保启动时加载
	:config
	;; 使用 Dirvish 内置的 icons 主题，它会显示 Dirvish 自带的图标风格
	(setq dirvish-theme 'icons)
	;; 是否显示隐藏文件；此处设为 nil 表示默认不显示隐藏文件
	(setq dirvish-show-hidden-files nil)
	;; 设置 Dirvish 用于缓存持久数据（例如图片缓存、属性缓存）的目录
	(setq dirvish-cache-dir (expand-file-name "dirvish-cache" user-emacs-directory))
	;; 根据 CUSTOMIZING.org 建议，进一步定制属性显示：
	(setq dirvish-hide-details nil))


;; 安装 lsp-bridge
(use-package lsp-bridge
  :straight (lsp-bridge
             :type git
             :host github
             :repo "manateelazycat/lsp-bridge"
             ;; 把 acm 子目录也加到 load-path
             :files ("*.el" "acm/*.el"))
  :defer t                                    ; 不在启动时加载
  :commands (lsp-bridge-mode global-lsp-bridge-mode))

;; 手动把它加到 load-path —— 
(add-to-list 'load-path
             (expand-file-name "straight/repos/lsp-bridge" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "straight/repos/lsp-bridge/acm" user-emacs-directory))

;; 确保已安装 tree-sitter 及 tree-sitter-langs（推荐使用 use-package 来管理它们）
(use-package tree-sitter
  :ensure t
  :hook ((c-mode
          c++-mode
          lua-mode
          python-mode)
         . tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; 自动检查并安装 s，如果没有安装
(unless (package-installed-p 's)
  (package-refresh-contents)
  (package-install 's))
(require 's)

(use-package ts-fold
  :straight (ts-fold
             :type git
             :host github
             :repo "emacs-tree-sitter/ts-fold")  ; 指定 GitHub 仓库地址
  :hook (after-init . global-ts-fold-mode)   ; 启动后全局开启折叠
  :config
  ;; 如果你想细调，参考 ts-fold README 中的自定义选项
  )

;; 针对 C/C++、Lua、Python 等语言启用 ts-fold-mode
(dolist (mode '(c-mode-hook c++-mode-hook lua-mode-hook python-mode-hook))
  (add-hook mode #'ts-fold-mode))


;; --- Emacs-gdb 调试 C/C++ 调试配置 ---
;; 使用 use-package 配置内置 GDB 前端（gdb-mi）
(use-package gdb
  :ensure nil
  :config
  ;; 启用多窗口调试模式：启动 GDB 后自动把调试窗口布局为类似 IDE 的多个窗口，
  ;; 包括：源代码窗口、GDB 命令窗口、断点及调用栈窗口、变量监视窗口等。
  (setq gdb-many-windows t)
  
  ;; 自动打开主函数所在窗口。注意：这需要调试时 GDB 能正确定位 main 函数。
  (setq gdb-show-main t))
  

;; 定义一个交互式函数，启动 GDB 调试 C/C++ 程序。
;; 这里使用 -i=i / --annotate=3 参数，使 GDB 输出进入 MI 模式，从而让 Emacs 更好解析并显示信息。
(defun my-gdb-run ()
  "选择一个可执行文件并启动 GDB 调试会话。
依次提示输入：
  1. 程序名前的额外调试参数（例如 -ex \"break main\" 等）；
  2. 程序名后的额外参数或命令（例如传递给程序的参数），
如果后置参数不以 \"--args\" 开头，将自动添加该前缀。
最终构造的命令格式为：
  gdb [前置参数] -i=mi [可执行文件] [后置参数]"
  (interactive)
  (let* ((exe (read-file-name "选择可执行文件: "))  ; 选择待调试的可执行文件
         (default-prompt (format "gdb %s " exe))
         ;; 读取在程序名前添加的额外参数
         (pre-extra (read-string (concat default-prompt "请输入在程序名前添加的额外参数(可选)：")))
         ;; 若前置参数中包含了 -i=mi，则移除以避免重复
         (pre-extra (if (string-match-p "-i=mi" pre-extra)
                        (progn
                          (message "检测到重复的 -i=mi 参数，已自动移除。")
                          (replace-regexp-in-string "-i=mi\\s-*" "" pre-extra))
                      pre-extra))
         ;; 读取在程序名后添加的额外参数或命令
         (post-extra (read-string (concat default-prompt "请输入在程序名后添加的额外参数(可选)：")))
         ;; 如果后置参数不为空且不以 "--args" 开头，则自动添加 "--args " 前缀
         (post-extra (if (and (not (string-empty-p post-extra))
                              (not (string-prefix-p "--args" (string-trim-left post-extra))))
                         (concat "--args " post-extra)
                       post-extra))
         ;; 构造最终启动命令：
         ;; gdb [前置参数] -i=mi [可执行文件] [后置参数]
         (final-cmd (concat "gdb " pre-extra " -i=mi " exe " " post-extra)))
    ;; 显示最终命令供确认
    (if (yes-or-no-p (format "最终启动命令：\n%s\n是否确认启动？" final-cmd))
        (progn
          (message "启动命令：%s" final-cmd)
          (gdb final-cmd))
      (message "已取消启动 GDB 调试会话."))))


(use-package sly
  :ensure t
  :init
  ;; 指定你 Common Lisp 实现的路径，比如这里使用 SBCL
  (setq inferior-lisp-program "sbcl")
  :config
  ;; 默认使用 sly-fancy 提供更完善的扩展功能
  (sly-setup '(sly-fancy)))

(defun my-auto-start-sly-for-common-lisp ()
  "如果当前访问的文件扩展名为 .lisp，并且不是 Emacs Lisp 文件，则自动启动 SLY。
这样可以确保 Common Lisp 文件自动开启 SLY，而不干扰 .el 文件。"
  (when (and buffer-file-name
             (string-match-p "\\.lisp\\'" buffer-file-name)
             (not (derived-mode-p 'emacs-lisp-mode)))
    ;; 如果 SLY 尚未连接，则调用 sly 启动 SLY 环境
    (unless (bound-and-true-p sly-mode)
      (sly))))

;; 将自动启动函数加入 lisp-mode 的 hook 中
(add-hook 'lisp-mode-hook #'my-auto-start-sly-for-common-lisp)

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package json-navigator
  :ensure t
  :commands json-navigator-mode)
(global-set-key (kbd "C-c j") 'json-pretty-print-buffer)

;; 安装并配置 rainbow-delimiters 插件
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; 自定义括号嵌套层级的颜色（可调整）
  (set-face-foreground 'rainbow-delimiters-depth-1-face  "#7fbbb3") ; 淡青
  (set-face-foreground 'rainbow-delimiters-depth-2-face  "#83c092") ; 淡绿
  (set-face-foreground 'rainbow-delimiters-depth-3-face  "#dbbc7f") ; 黄色
  (set-face-foreground 'rainbow-delimiters-depth-4-face  "#e69875") ; 橙红
  (set-face-foreground 'rainbow-delimiters-depth-5-face  "#d699b6") ; 粉紫
  (set-face-foreground 'rainbow-delimiters-depth-6-face  "#a7c080") ; 浅绿
  (set-face-foreground 'rainbow-delimiters-depth-7-face  "#e67e80") ; 红色
  (set-face-foreground 'rainbow-delimiters-depth-8-face  "#a3be8c") ; 浅草绿
  (set-face-foreground 'rainbow-delimiters-depth-9-face  "#d08770") ; 土黄
  ;; 非法括号颜色
  (set-face-foreground 'rainbow-delimiters-unmatched-face "#ff0066"))

(use-package vterm
	:ensure t)

(use-package lv
  :straight t)

(use-package pyim
  :straight t
  :init
  ;; 🧠 初始化前设置默认方案（一定要放在 :init 或 use-package 顶部）
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin)
  :config
  ;; 设置切换输入法快捷键
  (global-set-key (kbd "C-\\") 'toggle-input-method)

  ;; 📘 使用 pyim 内置拼音词库（需单独加载 pyim-basedict）
  (use-package pyim-basedict
    :straight t
    :config
    (pyim-basedict-enable))

  ;; 💬 使用 posframe 显示候选词窗口（需安装 posframe）
  (use-package posframe
    :straight t)

  ;; 🪟 设置 pyim 使用 posframe 浮动显示候选词
  (setq pyim-page-tooltip 'posframe)

  ;; ✅ 可选：设置 posframe 的样式与最大宽度
  (setq pyim-posframe-border-width 2)
  (setq pyim-posframe-min-width 20)
  (setq pyim-posframe-min-height 4)
)


(provide 'init-package)
