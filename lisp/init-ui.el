;;; init-ui.el--- Custom UI configuration 
;;; -*- lexical-binding: t -*-
;;; Commentary:
;; 这里可以简单描述下你的配置用途
;;; Code:

;; 安装主题包
(use-package catppuccin-theme
  :ensure t)

(use-package monokai-pro-theme
  :ensure t)

(use-package modus-themes
  :ensure t)

;; 默认加载 Catppuccin 的 Mocha 主题（Catppuccin 的主题依赖于变量 catppuccin-flavor）
(setq catppuccin-flavor 'mocha)
(load-theme 'modus-vivendi t)

;; -------------------------------
;; 自动安装所需要的包
;; -------------------------------
(dolist (pkg '(memoize powerline spaceline winum))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; -------------------------------
;; 加载必要的包
;; -------------------------------
(require 'powerline)
(require 'spaceline)
(require 'winum)


(winum-mode 1)

;; -------------------------------
;; 设置 powerline 的分隔符样式
;; -------------------------------
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'arrow))

;; -------------------------------
;; 自定义 spaceline 模式行
;; -------------------------------
(use-package spaceline
  :ensure t
  :after powerline
  :config
	;; buffer 名称
	(spaceline-define-segment my-buffer-id
  (when (and (buffer-file-name) (fboundp 'all-the-icons-icon-for-buffer))
    (concat (all-the-icons-icon-for-buffer) (powerline-buffer-id))))

  ;; 主模式
  (spaceline-define-segment my-major-mode
    "Major mode name."
    (format-mode-line mode-name))

  ;; 当前项目路径（project.el）
  (spaceline-define-segment my-project-dir
    "Project root directory."
    (when (fboundp 'project-current)
      (when-let* ((proj (project-current))
                  (root (car (project-roots proj))))
        (abbreviate-file-name root))))


  (spaceline-define-segment my-flycheck
  "Flycheck status counts with icons."
  (when (and (boundp 'flycheck-current-errors) flycheck-mode)
    (let-alist (flycheck-count-errors flycheck-current-errors)
      (concat
       (all-the-icons-faicon "times-circle" :height 1.0 :v-adjust 0 :face '(:foreground "red"))   ; 错误图标
       (format " %d " (or .error 0))
       (all-the-icons-faicon "exclamation-triangle" :height 1.0 :v-adjust 0 :face '(:foreground "yellow")) ; 警告图标
       (format " %d " (or .warning 0))
       (all-the-icons-faicon "info-circle" :height 1.0 :v-adjust 0 :face '(:foreground "green"))    ; 提示图标
       (format " %d" (or .info 0))))))


;; ----------------- lsp/dap 状态段 -----------------
;; 定义一个全局变量来保存 dap-hydra 的状态
(defvar my-dap-hydra-active nil
  "非 nil 表示当前 dap-hydra 激活，应显示 DAP-Mode 状态。")

;; 定义包装函数，将 dap-hydra 激活期间设置 my-dap-hydra-active 为 t，
;; 当 hydra 退出时复位为 nil
(defun my-dap-hydra-wrapper (orig-fun &rest args)
  "包装 dap-hydra 类命令，在执行前设置 my-dap-hydra-active 为 t，退出后复位为 nil。"
  (setq my-dap-hydra-active t)
  (force-mode-line-update)
  (unwind-protect
      (apply orig-fun args)
    (setq my-dap-hydra-active nil)
    (force-mode-line-update)))

;; 尝试包装 dap-hydra 入口命令
(when (fboundp 'dap-hydra)
  (advice-add 'dap-hydra :around #'my-dap-hydra-wrapper))
(when (fboundp 'dap-hydra/body)
  (advice-add 'dap-hydra/body :around #'my-dap-hydra-wrapper))

	;; ------------------- LSP 状态段 -------------------
	(spaceline-define-segment my-lsp-status
		"返回一个字符串，描述当前激活的是 lsp-mode 还是 lsp-bridge，
	如果两者都没有启用，则显示 'No LSP'."
		(cond
		 ((and (boundp 'lsp-bridge-mode) lsp-bridge-mode)
			(concat
			 (propertize "🦜 " 'face '(:foreground "#00ff00" :height 1.0))
			 (propertize "LSP-Bridge" 'face '(:foreground "#00ff00" :height 1.0))))
		 ((and (boundp 'lsp-mode) lsp-mode)
			(concat
			 (propertize "🐦‍ " 'face '(:foreground "#9966ff" :height 1.0))
			 (propertize "LSP-Mode" 'face '(:foreground "#9966ff" :height 1.0))))
		 (t (concat 
				 (propertize "💤 " 'face '(:foreground "#b3e6ff" :height 1.0))
				 (propertize "No LSP" 'face '(:foreground "#b3e6ff" :height 1.0))))))


	;; ------------------- Meow 状态段 -------------------
	;; 定义缓冲区局部变量，用来保存当前 Meow 状态，默认是 "Normal"
	(defvar my-meow-current-state "Normal"
		"当前的 Meow 状态。默认是 \"Normal\"。")
	(make-variable-buffer-local 'my-meow-current-state)

	;; 当进入 Insert 模式时调用，将当前缓冲区的状态设置为 Insert
	(defun my-update-meow-state-to-insert ()
		"将当前缓冲区的 Meow 状态设置为 Insert，并刷新 mode-line。"
		(setq-local my-meow-current-state "Insert")
		(force-mode-line-update)
		(message "Switched to Insert"))

	(add-hook 'meow-insert-mode-hook #'my-update-meow-state-to-insert)

	;; 使用 post-command-hook 检查按键事件，
	;; 如果检测到 ESC，就更新当前缓冲区状态为 Normal。
	(defun my-check-for-escape ()
		"检查当前命令键序列，如果第一个事件是 ESC，则将当前缓冲区的 Meow 状态更新为 Normal。"
		(let ((keys (this-command-keys-vector)))
			(when (and (> (length keys) 0)
								 (eq (aref keys 0) 'escape))
				(setq-local my-meow-current-state "Normal")
				(force-mode-line-update)
				(message "Switched to Normal (ESC detected)"))))

	(add-hook 'post-command-hook #'my-check-for-escape)

	;; 修改 mode-line 显示函数，根据当前缓冲区变量 my-meow-current-state 显示状态
	(defun my-meow-defun ()
		"返回当前缓冲区的 Meow 状态字符串，供 Spaceline 显示。
	如果启用了 meow（meow-global-mode 或 meow-mode）：
	- 当状态为 Insert 时显示淡紫色 '🐱 Insert'；
	- 当状态为 Normal（默认）时显示橙色 '🐱 Normal'；
	如果 meow 没启用，则返回空字符串。"
		(if (or (bound-and-true-p meow-global-mode)
						(bound-and-true-p meow-mode))
				(let* ((state my-meow-current-state)
							 (icon "🐱 ")
							 (color (if (string= state "Insert")
													"#cc99ff"  ;; 淡紫色用于 Insert
												"#ff8800"))) ;; 橙色用于 Normal
					(concat
					 (propertize icon 'face `(:foreground ,color :weight bold))
					 (propertize state 'face `(:foreground ,color :weight bold))))
			""))

	;; 保证每个命令后刷新 mode-line（可选，但有助于状态即时反映）
	(add-hook 'post-command-hook #'force-mode-line-update)

	;; 将 Meow 状态作为 Spaceline 的 segment 显示
	(spaceline-define-segment my-meow-status
		"显示当前 Meow 状态：
	- Insert 模式下显示淡紫色 '🐱 Insert'；
	- Normal 模式下显示橙色 '🐱 Normal'。
	因为状态是缓冲区局部的，所以每个窗口的显示互相独立。"
		(my-meow-defun))

	;; time显示
	(spaceline-define-segment my-time
  "Current time string."
  (format-time-string "%H:%M"))

  ;; 当前窗口编号
  (spaceline-define-segment my-winum
    "Window number (winum)."
    (when (bound-and-true-p winum-mode)
      (format "Window %s" (winum-get-number-string))))

  ;; 光标位置
  (spaceline-define-segment my-position
    "Cursor line:column."
    (format-mode-line "%l:%c"))


  ;; 安装 spaceline 布局
  (spaceline-install
   'main
   ;; 左侧
   '((my-winum)
		 (my-meow-status)
		 (my-project-dir)
     (my-buffer-id)
     (my-major-mode)
		 (my-flycheck)
     )
   ;; 右侧
   '((my-lsp-status)
		 (my-position)
     (my-time)))  ;; 启用时间显示（display-time-mode）

  (powerline-reset))

;; -------------------------------
;; 启用时间显示（右下角）
;; -------------------------------
(display-time-mode 1)
(setq display-time-default-load-average nil)

;; -------------------------------
;; 启用我们自定义的 spaceline 主题
;; -------------------------------
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))



;; 定义一个交互式主题切换函数
(defun my/switch-theme ()
  "交互式切换主题。
提供所有已安装的主题（即 custom-available-themes 返回的列表）
以及额外的 Catppuccin 自定义选项（Frappe/Mocha/Macchiato/Latte）。"
  (interactive)
  ;; 自定义的 Catppuccin 选项列表（显示名称与风格对应）
  (let* ((catppuccin-options '("Catppuccin (Frappe)"
                               "Catppuccin (Mocha)"
                               "Catppuccin (Macchiato)"
                               "Catppuccin (Latte)"))
         ;; 取出所有已加载主题的名称（字符串形式），并去掉 "catppuccin"，以免重复
         (other-themes (remove "catppuccin"
                               (mapcar #'symbol-name (custom-available-themes))))
         ;; 合并自定义的 Catppuccin 选项和其他主题列表
         (theme-list (append catppuccin-options other-themes))
         (choice (completing-read "选择主题: " theme-list nil t)))
    ;; 先禁用所有当前启用的主题，避免冲突
    (mapc #'disable-theme custom-enabled-themes)
    (if (member choice catppuccin-options)
        (progn
          (cond
           ((string-match-p "Frappe" choice) (setq catppuccin-flavor 'frappe))
           ((string-match-p "Macchiato" choice) (setq catppuccin-flavor 'macchiato))
           ((string-match-p "Latte" choice) (setq catppuccin-flavor 'latte))
           (t (setq catppuccin-flavor 'mocha)))
          ;; 加载 Catppuccin 主题时只需调用 load-theme 'catppuccin，因为 catppuccin-flavor 已经决定了风格
          (load-theme 'catppuccin t)
          (message "已切换至 %s" choice))
      (progn
				;; 对于其它主题，choice 已经是 custom-available-themes 中的名称
				(load-theme (intern choice) t)
				(message "已切换至 %s" choice)))
		(powerline-reset)))

;; 绑定全局快捷键 C-t 调用主题切换函数
(global-set-key (kbd "C-t") 'my/switch-theme)




(provide 'init-ui)
