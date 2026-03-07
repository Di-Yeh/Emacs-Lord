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

;; ============================
;; indent-bars - 通用设置
;; ============================
(use-package indent-bars
	:custom
		(indent-bars-treesit-support t)
    (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))
	:hook ((c-mode csharp-mode rust-mode) . indent-bars-mode))

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

(use-package neotree
	:ensure t)

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

;;; --- Completion 基础 ---
(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)              ;; 自动弹出
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-preselect 'prompt)
  (corfu-popupinfo-mode t))   ;; 显示文档 (类似 company-quickhelp)

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :custom (corfu-popupinfo-delay 0.2))

;;; --- CAPE：额外补全功能 ---
(use-package cape
  :straight t
  :init
  ;; 常用补全绑定（可选）
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

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

(use-package clang-format+
	:ensure t)
(use-package clang-format
	:ensure t)

(add-hook 'c-mode-common-hook #'clang-format+-mode)

;; 使用 nasm-mode（适用于 MASM/NASM）
(use-package nasm-mode
  :ensure t
  :mode ("\\.\\(asm\\|s\\|S\\)\\'" . nasm-mode))

(setq asm-comment-char ?\#) ; 默认是 `;`，你可以按需设置为 `#` 或 `;`
(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-always-indent 'complete)
            (electric-indent-local-mode -1) ; 防止自动缩进乱跳
            (setq indent-tabs-mode t)
            (setq tab-width 8)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

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

;; --------------------------------------------------
;; eglot 基础配置（Emacs 28+）
;; --------------------------------------------------

(setq straight-vc-git-default-clone-depth 1)
(use-package markdown-mode
  :straight t  ; 明确指定用 straight 安装
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package cmake-mode
  :straight t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; 自動生成 XML 文件註釋
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(defun my-insert-vs-doc ()
  "直接插入 VS 風格的 XML 註釋，不依賴名稱查找。"
  (interactive)
  (require 'yasnippet nil t)
  (if (fboundp 'yas-expand-snippet)
      (let ((vs-tpl "/// <summary>\n/// $1\n/// </summary>\n/// <param name=\"$2\">$3</param>\n/// <returns>$4</returns>$0"))
        ;; 直接展開字串模板，不需要 yas-lookup-snippet
        (yas-expand-snippet vs-tpl))
    (message "錯誤：yasnippet 插件尚未就緒")))

;; 綁定一個「絕對安全」的快捷鍵
(global-set-key (kbd "C-c s") #'my-insert-vs-doc)

;; Separedit (這是編輯註釋的神器，強烈建議保留)
(use-package separedit
  :straight t
  :bind ("C-c '" . separedit))

(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle)) ; 设置快捷键 F8 开关

(require 'eglot)
;; 不要自动在所有支持的 mode 启动 eglot
;; 你需要时再手动 M-x eglot
(setq eglot-autoshutdown t)   ;; buffer 关闭就退出 LSP
(setq eglot-confirm-server-initiated-edits nil)

(add-to-list 'eglot-server-programs
             '((c-mode c++-mode) "clangd"))

(setq completion-styles '(orderless))
(setq tab-always-indent 'complete)

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change)
        flycheck-idle-change-delay 1.0))

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
