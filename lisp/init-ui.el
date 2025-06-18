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


;;; ----------------------------------------------
;;; 基础：自动安装所需包
;;; ----------------------------------------------
(dolist (pkg '(memoize powerline spaceline winum all-the-icons diff-hl))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; ----------------------------------------------
;;; 加载必要库
;;; ----------------------------------------------
(require 'powerline)
(require 'spaceline)
(require 'spaceline-config)  ;; 必须先载入，提供 spacíeline-install
(require 'winum)
(require 'all-the-icons)  ;; 确保 icon 可用
(require 'diff-hl)


(winum-mode 1)  ;; 启用窗口编号

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))  ;; 实时刷新

;;; ----------------------------------------------
;;; 每次 load-theme 后修复 mode-line face
;;; ----------------------------------------------
(defun my/fix-mode-line-faces (&rest _args)
  "重置 mode-line 和 powerline 相关 face，防止主题覆盖。"
  ;; 基础 mode-line
  (dolist (face '(mode-line mode-line-inactive))
    (set-face-attribute face nil
                        :background "#2C313C" :foreground "#ffffff"
                        :box nil :overline nil :underline nil :inherit nil))
  ;; powerline 常用 face
  (set-face-attribute 'powerline-active1   nil :background "#3E4451" :foreground "#ffffff" :inherit nil)
  (set-face-attribute 'powerline-active2   nil :background "#4B5263" :foreground "#ffffff" :inherit nil)
  (set-face-attribute 'powerline-inactive1 nil :background "#2C2C2C" :foreground "#888888" :inherit nil)
  (set-face-attribute 'powerline-inactive2 nil :background "#1E1E1E" :foreground "#666666" :inherit nil))
;; 在 load-theme 后执行
(advice-add 'load-theme :after #'my/fix-mode-line-faces)

;;; ----------------------------------------------
;;; 定义 spaceline segment 专用 face
;;; ----------------------------------------------
(defface my/spaceline-face-theme-one
  '((t (:background "#ff8533" :foreground "#1E1E1E" :box nil :weight bold)))
  "segments 样式 1" :group 'spaceline)

(defface my/spaceline-face-theme-two
  '((t (:background "#9933ff" :foreground "#ffffff" :box nil :weight bold)))
  "segments 样式 2" :group 'spaceline)

(defface my/spaceline-face-theme-three
  '((t (:background "#3d3d5c" :foreground "#1e1e1e" :box nil :weight bold)))
  "segments 样式 3" :group 'spaceline)

(defface my/spaceline-face-theme-four
  '((t (:background "#2C2C2C" :foreground "#ffffff" :box nil :weight bold)))
  "segments 样式 4" :group 'spaceline)

;;; ----------------------------------------------
;;; powerline 分隔符配置
;;; ----------------------------------------------
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'wave)
  (setq powerline-height 32)  ;; 按需调整
	(setq powerline-default-separator-dir 'right) ; 让左右 segment 分割明确
  )

;;; ----------------------------------------------
;;; 定义 spaceline segments & 安装布局
;;; ----------------------------------------------
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

	(defun my-project-dir-short (path)
		"返回 PATH 的最后两个目录名称，用于显示项目路径的缩略信息。
	例如：\"/home/user/projects/myproject\" 将返回 \"projects/myproject\"."
		(let* ((path (directory-file-name path))  ;; 去掉尾部斜杠
					 (components (split-string path "/" t)))
			(if (>= (length components) 2)
					(concat (nth (- (length components) 2) components) "/" (car (last components)))
				(car components))))

	;; ----------------- 当前项目路径（project.el） -----------------
	(spaceline-define-segment my-project-dir
		"显示项目根目录的缩略路径（仅显示最后两个目录名称）。"
		(when (fboundp 'project-current)
			(when-let* ((proj (project-current))
									(root (car (project-roots proj))))
				(my-project-dir-short (abbreviate-file-name root)))))


  (spaceline-define-segment my-flycheck
  "Flycheck status counts with icons."
  (when (and (boundp 'flycheck-current-errors) flycheck-mode)
    (let-alist (flycheck-count-errors flycheck-current-errors)
      (concat
       (all-the-icons-faicon "times-circle" :height 1.0 :v-adjust 0 :face '(:background "#2C2C2C" :foreground "#ff0066"))   ; 错误图标
       (format " %d " (or .error 0))
       (all-the-icons-faicon "exclamation-triangle" :height 1.0 :v-adjust 0 :face '(:background "#2C2C2C" :foreground "#ffff00")) ; 警告图标
       (format " %d " (or .warning 0))
       (all-the-icons-faicon "info-circle" :height 1.0 :v-adjust 0 :face '(:background "#2C2C2C" :foreground "#00ff00"))    ; 提示图标
       (format " %d" (or .info 0))))))


	;; ----------------- lsp状态段 -----------------
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
			 (propertize "🦜 " 'face '(:background "#3d3d5c" :foreground "#00ff00" :height 1.0))
			 (propertize "LSP-Bridge" 'face '(:background "#3d3d5c" :foreground "#00ff00" :height 1.0))))
		 ((and (boundp 'lsp-mode) lsp-mode)
			(concat
			 (propertize "🐦‍ " 'face '(:background "#3d3d5c" :foreground "#9966ff" :height 1.0))
			 (propertize "LSP-Mode" 'face '(:background "#3d3d5c" :foreground "#9966ff" :height 1.0))))
		 (t (concat 
				 (propertize "💤 " 'face '(:background "#3d3d5c" :foreground "#b3e6ff" :height 1.0))
				 (propertize "No LSP" 'face '(:background "#3d3d5c" :foreground "#b3e6ff" :height 1.0))))))


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
      (format "Buffer %s" (winum-get-number-string))))

  ;; 光标位置
  (spaceline-define-segment my-position
    "Cursor line:column."
    (format-mode-line "%l:%c"))


	;;; ==============================
	;;; Diff-hl Segment for Spaceline
	;;; ==============================

	(spaceline-define-segment my-diff-hl
		"使用 `diff-hl` 显示 Git 变更统计：+新增 -删除 ~修改，前置 Git 图标。
	若未启用 diff-hl 或无变更时，仅显示 Git 图标。"
		(when (and (bound-and-true-p diff-hl-mode)
							 buffer-file-name
							 (vc-backend buffer-file-name))
			(let* ((added    (diff-hl-insert-count))
						 (deleted  (diff-hl-delete-count))
						 (changed  (diff-hl-change-count))
						 (has-diff (or (> added 0) (> deleted 0) (> changed 0))))
				(concat
				 ;; 图标部分
				 (all-the-icons-faicon "git"
															 :height 1.0
															 :v-adjust 0
															 :face '(:background"#9933ff" :foreground "#f1502f"))
				 " "
				 ;; 内容部分
				 (if has-diff
						 (string-trim
							(concat
							 (when (> added 0)   (format "+%d " added))
							 (when (> deleted 0) (format "-%d " deleted))
							 (when (> changed 0) (format "~%d " changed))))
					 "Clean")))))

	(defun diff-hl-insert-count ()
		"统计当前 buffer 中 diff-hl 插入的行数。"
		(diff-hl--count-overlays 'diff-hl-insert))

	(defun diff-hl-delete-count ()
		"统计当前 buffer 中 diff-hl 删除的行数。"
		(diff-hl--count-overlays 'diff-hl-delete))

	(defun diff-hl-change-count ()
		"统计当前 buffer 中 diff-hl 修改的行数。"
		(diff-hl--count-overlays 'diff-hl-change))

	(defun diff-hl--count-overlays (type)
		"统计所有属于 TYPE 的 overlay（由 diff-hl 管理）。"
		(let ((count 0))
			(dolist (ov (overlays-in (point-min) (point-max)))
				(when (eq (overlay-get ov 'face) type)
					(setq count (1+ count))))
			count))

	;;; ----------------------------------------------
	;;; 定义文件统计 segment
	;;; ----------------------------------------------
	(spaceline-define-segment my-file-stats
		"显示当前文件的字符数、单词数和行数，图标 + 彩色显示。"
		(when buffer-file-name
			(let* ((char-count (buffer-size))
						 (word-count (count-words (point-min) (point-max)))
						 (line-count (count-lines (point-min) (point-max)))
						 ;; 图标配置（图标 + 高度 + 调整 + 颜色）
						 (icon-char (all-the-icons-material "text_fields"
																								 :height 1.0 :v-adjust 0
																								 :face '(:foreground "#ffcc00" :background "#3d3d5c")))
						 (icon-word (all-the-icons-octicon "book"
																								:height 1.0 :v-adjust 0
																								:face '(:foreground "#66ff66" :background "#3d3d5c")))
						 (icon-line (all-the-icons-faicon "bars"
																							 :height 1.0 :v-adjust 0
																							 :face '(:foreground "#66ccff" :background "#3d3d5c"))))
				;; 拼接显示内容
				(concat
				 icon-char " " (propertize (format "%d" char-count) 'face '(:foreground "white" :background "#3d3d5c"))
				 " "
				 icon-word " " (propertize (format "%d" word-count) 'face '(:foreground "white" :background "#3d3d5c"))
				 " "
				 icon-line " " (propertize (format "%d" line-count) 'face '(:foreground "white" :background "#3d3d5c"))))))



	;; -------------------------------
	;; 安装 spaceline 布局，并指定 face
	;; -------------------------------
	(spaceline-install
	 'main
	 ;; 左侧 segments 列表
	 `((my-winum        :face 'my/spaceline-face-theme-one :priority 50)		; 窗口编号
		 (my-buffer-id    :priority 60)		; buffer 名称 + 图标
		 (my-major-mode   :face 'my/spaceline-face-theme-two :priority 70)   ; major-mode 名称
		 (my-flycheck     :face 'my/spaceline-face-theme-four :priority 20)   ; flycheck 错误/警告/提示图标
		 (my-file-stats	  :face 'my/spaceline-face-theme-three :priority 75)
		 (my-project-dir  :face 'my/spaceline-face-theme-one :priority 15 :max-length 40)		; 项目路径
		 (my-diff-hl		  :face 'my/spaceline-face-theme-two :priority 10)
		 (my-meow-status  )   ; Meow 模式高亮
		)

	 ;; 右侧 segments 列表
	 `(
		 (my-lsp-status  :face 'my/spaceline-face-theme-three :priority 80)		; LSP 状态
		 (my-position    :priority 85)		; 行:列
		 (my-time        :face 'my/spaceline-face-theme-one :priority 100) 	; 时间
		))


  ;; 确保安装完成后刷新
  (powerline-reset))

;;; ----------------------------------------------
;;; 启用spaceline 主函数与时钟
;;; ----------------------------------------------
(display-time-mode 1)
(setq display-time-default-load-average nil)

(setq-default mode-line-format
  '("%e"
    (:eval
     (let* ((lhs (spaceline--seg main 'lhs))
            (rhs (spaceline--seg main 'rhs))
            ;; 计算剩余宽度：窗口总宽 – lhs 长度 – rhs 长度
            (fill-width (max 0 (- (window-total-width) 
                                  (length lhs) 
                                  (length rhs)))))
       (concat
        lhs
        ;; 填充成空白或分隔符
        (powerline-fill 'mode-line fill-width)
        rhs)))))

(setq-default mode-line-end-spaces (make-string 0 ?\s))
(setq-default truncate-lines t)

(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))


(load-theme 'modus-vivendi t)

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
		(powerline-reset)
		))

;; 绑定全局快捷键 C-t 调用主题切换函数
(global-set-key (kbd "C-t") 'my/switch-theme)




(provide 'init-ui)
