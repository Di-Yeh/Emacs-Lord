;;; init-package.el --- setttings for packages
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package restart-emacs)
(setq straight-vc-git-default-clone-depth 1)

(use-package drag-stuff
             :bind (("<M-up>" . drag-stuff-up)
                    ("<M-down>" . drag-stuff-down)))

;; 使用 Vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode 1))

;; 使用 Marginalia 显示候选项额外信息（类似 ivy-rich）
(use-package marginalia
  :straight t
  :init
  (marginalia-mode 1))

;; Orderless 提供模糊匹配（比 ivy 更自由）
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)) ; 使用 orderless 做为主要匹配风格
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))) ; 文件路径可用 partial

;; Consult 替代 Swiper 和 Counsel
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)               ; 替代 swiper
         ("C-r" . consult-line)								; 向后搜索
         ("C-c f" . consult-recent-file)			; 替代 counsel-recentf
         ("C-c g" . consult-git-grep))				; 替代 counsel-git
  :custom
  ;; ✅ 启用实时预览功能（等效 swiper）
  (consult-preview-key '(:debounce 0.2 any))  ;; 或 '(:debounce 0.3 any)
  ;; ✅ 默认不显示太多 preview 窗口
  (consult-narrow-key "<")
  ;; 可选：让 consult-line 默认从当前点开始往后找
  (consult-line-start-from-top nil))

;; ✅ 这个函数会确保实时预览在 minibuffer 启用
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

;; Embark 提供对候选项的上下文操作（光标移动到候选项时按 C-. 可以弹出操作菜单）
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings)) ;; 查看当前 keymap 的所有绑定
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; 更好地与 consult 结合
(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; which-key
(use-package which-key
  :ensure t
  :defer nil
  :config
  (which-key-mode))

;; 可选：美化 minibuffer（建议搭配）
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package ace-window 
             :bind (("M-o" . 'ace-window)))

(use-package async
  :straight t
  :init
  ;; 自动加载 async byte-compile 的 hook（可选）
  (dired-async-mode 1) ; 若你常用 Dired
  (async-bytecomp-package-mode 1)) ; 对插件异步 byte-compile，加快 Emacs 启动


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

;; --------------------------------------------
;; 配置 Projectile：项目管理工具，用于快速在项目间跳转、搜索文件、查找符号等
;; --------------------------------------------
;; 启用项目管理工具 Projectile
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)  ;; 全局启用 Projectile 模式
  :config
  ;; 根据实际情况设置你的项目目录，可以添加多个目录
  (setq projectile-project-search-path '("~/projects/"))
  ;; 结合 Ivy 使用更加流畅的补全体验
  (setq projectile-completion-system 'default)
  ;; 建议绑定 Projectile 的命令前缀，例如 "C-c p"
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (message "Projectile 已启动"))

;; 使用 Consult 封装的 Projectile 接口，提供更好 UI 和功能
(use-package consult-projectile
  :straight t
  :after (consult projectile))

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

;; 使用示例：
;; 1. 在编辑代码或文本时，将光标放在某个单词上。
;; 2. 按下 C-; 进入 iedit 模式，此时所有与该单词相同的部分会高亮显示，
;;    编辑其中一个区域，其他区域会同步更新。
;; 3. 当编辑完成后，按下 C-c C-c 结束 iedit 模式。

;; 为 iedit-dwim 绑定一个快捷键，比如 M-I（Alt + I），你可以根据自己的习惯更改：
(global-set-key (kbd "M-I") 'iedit-dwim)

(use-package multiple-cursors
  :ensure t)

;; multiple cursor
(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ================================
;; Org-Roam 设置
;; ================================

(use-package org-roam
  :ensure t
  :init
  ;; 预先定义占位目录（启动时不设置具体目录）
  (setq org-roam-directory nil)
  (setq org-roam-dailies-directory "daily/"))

;; 自定义函数：选择 org-roam 根目录并自动设置
(require 'org-roam-dailies)

(defvar my/org-roam-dir-file
  (expand-file-name ".org-roam-dir.el" user-emacs-directory)
  "保存 org-roam 目录路径的文件。")

(defun my/load-org-roam-directory ()
  "从配置文件读取 org-roam 目录。"
  (when (file-exists-p my/org-roam-dir-file)
    (load-file my/org-roam-dir-file)
    (when (and (boundp 'my/saved-org-roam-directory)
               (file-directory-p my/saved-org-roam-directory))
      (setq org-roam-directory (file-truename my/saved-org-roam-directory))
      (setq org-roam-dailies-directory "daily/")
      (org-roam-db-sync)
      (org-roam-db-autosync-mode 1)
      (message "已载入 org-roam 目录：%s" org-roam-directory))))

(defun my/set-org-roam-directory ()
  "手动选择 org-roam 的主目录，并设置相关路径。"
  (interactive)
  (let* ((selected-dir (read-directory-name "选择 org-roam 笔记主目录: "))
         (daily-dir (expand-file-name "daily/" selected-dir)))
    ;; 保存路径
    (setq my/saved-org-roam-directory (file-truename selected-dir))
    ;; 保存到文件
    (with-temp-file my/org-roam-dir-file
      (insert (format "(setq my/saved-org-roam-directory %S)\n"
                      my/saved-org-roam-directory)))
    ;; 应用设置
    (setq org-roam-directory my/saved-org-roam-directory)
    (setq org-roam-dailies-directory "daily/")
    ;; 创建目录
    (unless (file-directory-p org-roam-directory)
      (make-directory org-roam-directory t))
    (unless (file-directory-p daily-dir)
      (make-directory daily-dir t))
    ;; 同步数据库
    (org-roam-db-sync)
    (org-roam-db-autosync-mode 1)
    (message "org-roam 目录已设置为: %s" org-roam-directory)))

;; 全局快捷键绑定
(global-set-key (kbd "C-c C-d") #'my/set-org-roam-directory)

;; 启动时尝试自动加载目录设置
(my/load-org-roam-directory)


;; 可视化 UI（浏览器中查看笔记图谱）
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

;; ================================
;; Org-mode 标题与列表美化
;; ================================

;; 基础 Org 设置
(setq org-startup-indented t              ;; 自动缩进
      org-hide-leading-stars t            ;; 隐藏原生星号前缀
      org-ellipsis " ▼"                   ;; 折叠提示符
      org-pretty-entities t               ;; 显示符号替换（√ → ✓ 等）
      org-hide-emphasis-markers t         ;; 隐藏 *强调* 的星号
      org-image-actual-width '(300))      ;; 图像宽度

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :init
  (setq
   ;; 设置 headline 的符号（最多支持8级）
   org-superstar-headline-bullets-list '("♚" "♛" "♜" "♝" "♞" "♟" "☭" "✿" "◉")
   ;; 美化列表符号 - 替换 -, +, *
   org-superstar-item-bullet-alist '((?- . "•") (?+ . "⊕")) ;; 原版 '((?- . "➖") (?+ . "➕") (?* . "•"))
   ;; 隐藏前导星号（只保留替代符号）
   org-hide-leading-stars t
   org-superstar-leading-bullet " "
   ;; 美化 checkbox
   org-superstar-special-todo-items t))

;; 美化 checkbox 样式
(setq org-checkbox-symbols
      '((?X . "[☑]") 
        (?\s . "[☐]") 
        (?- . "[❍]")))
(font-lock-add-keywords 'org-mode
  '(("\\[\\([X ]\\)\\]" . font-lock-function-name-face)))

;; 设置 TODO / DONE 的样式（可自行修改颜色）
(setq org-todo-keyword-faces
      '(("TODO" . (:background "#ffcccc" :foreground "#990000" :weight bold))
        ("DONE" . (:background "#ccffcc" :foreground "#006600" :weight bold))))

;; 自动隐藏星号的 foreground（让星号看不见，但结构还在）
(defun my/org-hide-leading-star-fix (&rest _)
  "让 `org-hide` face 与背景一致，实现隐藏星号效果。"
  (when (facep 'org-hide)
    (let ((bg (face-background 'default nil 'default)))
      (set-face-foreground 'org-hide bg))))

;; 加入到主题切换 hook
(advice-add 'load-theme :after #'my/org-hide-leading-star-fix)
;; 初始执行一次
(my/org-hide-leading-star-fix)


;; org-download
(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method             'attach
        org-download-screenshot-method  "snippingtool"
        org-download-image-dir          (expand-file-name "org-images" user-emacs-directory))
  (org-download-enable))


;; --- dirvish ------------
;; 安装 dirvish
(use-package dirvish
  :straight t
  :defer nil  ;; 启动时立即加载
  :config
  ;; 开启 Dirvish 模式
  (dirvish-override-dired-mode)

  ;; ✅ 主题设为图标（你已安装 all-the-icons 和字体）
  (setq dirvish-theme 'icons)

  ;; ✅ 默认不显示隐藏文件（可透过 `h` 切换）
  (setq dirvish-show-hidden-files nil)

  ;; ✅ 避免访问系统文件造成的权限错误
  (setq dirvish-hide-details nil)  ;; 显示详细信息
  (setq dirvish-attributes '(all-the-icons file-size subtree-state vc-state))  ;; 显示图标和其他属性

  ;; ✅ 禁止尝试访问受限系统文件
  (setq dirvish-side-auto-refresh nil)  ;; 避免频繁触发文件扫描
  (setq dirvish-side-follow-current-file nil)  ;; 不要追踪当前 buffer 文件

  ;; ✅ 缓存设置
  (setq dirvish-cache-dir (expand-file-name "dirvish-cache" user-emacs-directory))

  ;; ✅ 快捷键提示
  (define-key dirvish-mode-map (kbd "?") #'dirvish-dispatch) ;; 显示可用命令
  (define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle) ;; 子目录展开
  (define-key dirvish-mode-map (kbd "a") #'dirvish-quick-access) ;; 快速访问
  (define-key dirvish-mode-map (kbd "f") #'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y") #'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N") #'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "h") #'dirvish-history-jump)  ;; 历史目录跳转
  (define-key dirvish-mode-map (kbd "s") #'dirvish-setup-menu)

  ;; ✅ 启用图片和音频预览（跳过 vipsthumbnail）
  (setq dirvish-preview-enabled t)
  (setq dirvish-preview-dispatchers '(image audio))  ;; 避免 text 触发错误

  (message "✅ Dirvish 加载完成"))


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
  ;; 设置 pyim 为默认输入法
  (setq default-input-method "pyim")

  ;; ✅ 拼音方案：quanpin = 全拼（也可用 'ziranma 表示双拼）
  (setq pyim-default-scheme 'quanpin) ;; quanpin

  ;; ✅ 增加候选词数量（默认是 5）
  (setq pyim-page-length 9) ;; 显示更多候选词（例如9个）

  :config
  ;; 切换中英文输入法快捷键（C-\）
  (global-set-key (kbd "C-\\") 'toggle-input-method)

  ;; 加载内置拼音词库（基于 pyim-basedict）
  (use-package pyim-basedict
    :straight t
    :config
    (pyim-basedict-enable))

  ;; ✅ 使用 posframe 漂浮提示窗口
  (setq pyim-page-tooltip 'posframe)

  ;; 可选：额外美化 posframe 样式
  (use-package posframe
    :straight t)
  (setq pyim-posframe-border-width 2)
  (setq pyim-posframe-min-width 20)
  (setq pyim-posframe-min-height 4)

  ;; 建议启用词频记忆（提升输入精度）
  (setq pyim-dcache-auto-update t))

;; 快速跳转增强插件：avy（可视化跳转字符/单词/行）
(use-package avy
  :ensure t
  :defer t
  :bind
  (("M-g c" . avy-goto-char)         ;; 跳转到指定字符（当前窗口可见区域）
   ("M-g 2" . avy-goto-char-2)       ;; 跳转到一对字符（更精准）
   ("M-g w" . avy-goto-word-1)       ;; 跳转到以某字符开头的单词
   ("M-g l" . avy-goto-line))        ;; 跳转到指定行
  :config
  ;; 设置跳转提示风格：预览字母提示（默认 'pre）
  (setq avy-style 'pre)

  ;; 如果你觉得按键提示太快消失，可手动设置等待时间（单位为秒）
  (setq avy-timeout-seconds 2)

  (message "🚀 avy 快速跳转已加载"))

;; vlf：Very Large File 支援
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup)

  ;; 打开大文件时自动询问是否使用 vlf
  (defun my/maybe-enable-vlf ()
    "如果当前 buffer 的文件大于 500MB，询问是否启用 vlf。"
    (let* ((file (buffer-file-name))
           (size (when file
                   (nth 7 (file-attributes file)))))
      (when (and size (> size (* 500 1024 1024))) ;; > 500MB
        (when (yes-or-no-p (format "文件超过 500MB，是否使用 vlf 打开？"))
          (vlf-mode 1)))))

  ;; 添加到文件打开 hook 中
  (add-hook 'find-file-hook #'my/maybe-enable-vlf))


(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :custom
  ;; ✅ 启用持久化历史记录
  (undo-tree-auto-save-history t)
  ;; ✅ 设置统一保存目录
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "Undo-tree/" user-emacs-directory))))
  :config
  ;; 创建目录（如果不存在）
  (let ((dir (expand-file-name "Undo-tree/" user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

;; 删除超过 7 天的 undo 文件（可选）
(run-at-time "1 day" (* 24 60 60)  ;; 每天执行
             (lambda ()
               (let ((dir (expand-file-name "Undo-tree/" user-emacs-directory)))
                 (when (file-directory-p dir)
                   (dolist (file (directory-files dir t "\\.~undo-tree~$"))
                     (when (> (float-time (time-subtract (current-time) (nth 5 (file-attributes file))))
                              (* 7 24 60 60))  ; 超过 7 天
                       (delete-file file)))))))



(provide 'init-package)
