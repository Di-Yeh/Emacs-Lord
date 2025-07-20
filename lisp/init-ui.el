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

;;; -*- lexical-binding: t -*-
;;; ----------------------------------------------
;;; 基础：自动安装所需包
;;; ----------------------------------------------
(dolist (pkg '(powerline spaceline winum all-the-icons diff-hl))
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
                        :background "#1E1E1E" :foreground "#ffffff"
                        :box nil :overline nil :underline nil :inherit nil :height 1.45)))

;; 在 load-theme 后执行
(advice-add 'load-theme :after #'my/fix-mode-line-faces)

;;; ----------------------------------------------
;;; 定义 spaceline segment 专用 face
;;; ----------------------------------------------
(defface my/spaceline-face-theme
  '((t (:background "#1E1E1E" :foreground "#ffffff" :box nil :weight bold)))
  "segments 样式 1" :group 'spaceline)

;;; ----------------------------------------------
;;; powerline 分隔符配置
;;; ----------------------------------------------
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator nil)
  (setq powerline-height 40)  ;; 按需调整
	(setq powerline-default-separator-dir 'right) ; 让左右 segment 分割明确
  )

;;; ----------------------------------------------
;;; 定义 spaceline segments & 安装布局
;;; ----------------------------------------------
(use-package spaceline
  :ensure t
  :after powerline
  :config
	
	;; ------------------- buffer 名称 -------------------
	(spaceline-define-segment my-buffer-id
  (when (and (buffer-file-name) (fboundp 'all-the-icons-icon-for-buffer))
    (concat (all-the-icons-icon-for-buffer) (powerline-buffer-id))))

  ;; ------------------- 主模式 -------------------
  (spaceline-define-segment my-major-mode
    "Major mode name."
    (format-mode-line mode-name))

	;; ----------------- 当前项目路径（project.el） -----------------
  (defun my-project-dir-short (path)
  "返回 PATH 的最后两个目录名称，用于显示项目路径的缩略信息。
例如：\"/home/user/projects/myproject\" 将返回 \"projects/myproject\"."
  (let* ((path (directory-file-name path))  ;; 去掉尾部斜杠
         (components (split-string path "/" t)))
    (if (>= (length components) 2)
        (concat (nth (- (length components) 2) components) "/" (car (last components)))
      (car components))))

	(spaceline-define-segment my-project-dir
		"显示项目根目录的缩略路径（仅显示最后两个目录名称），附带图标和配色。"
		(when (fboundp 'project-current)
			(when-let* ((proj (project-current))
									(root (car (project-roots proj))))
				(let ((icon (all-the-icons-octicon "file-directory"
																					 :face '(:foreground "#ff8533")
																					 :height 1.0 :v-adjust 0))
							(short-path (propertize (my-project-dir-short (abbreviate-file-name root))
																			'face '(:foreground "#ff9966"))))
					(format "%s %s" icon short-path)))))

	;; ------------------- 保存文件 -------------------
  (defvar my/save-status-show-text nil
  "指示是否在 modeline 中显示保存状态的文字。")

	(defun my/show-saved-status-text ()
		"保存后短暂显示 Saved 文字。"
		(setq my/save-status-show-text t)
		(run-at-time "3 sec" nil
								 (lambda ()
									 (setq my/save-status-show-text nil)
									 (force-mode-line-update))))

	(defun my/setup-save-status-hook ()
		"在当前 buffer 中设置保存状态显示的钩子。"
		(add-hook 'after-save-hook #'my/show-saved-status-text nil t))

	(add-hook 'find-file-hook #'my/setup-save-status-hook)

	(spaceline-define-segment my-save-status
		"显示保存状态图标 + 文本提示。"
		(let* ((modified (buffer-modified-p))
					 (icon (all-the-icons-material "save"
																				 :height 1.0
																				 :v-adjust -0.1
																				 :face `(:foreground ,(if modified "#ff6600" "#66ff66"))))
					 (text (cond
									(modified (propertize " No Save" 'face '(:foreground "#ff6600")))
									(my/save-status-show-text (propertize " Saved" 'face '(:foreground "#66ff66")))
									(t ""))))
			(concat icon text)))



	;; ----------------- flycheck设置 -----------------
  (spaceline-define-segment my-flycheck
  "Flycheck status counts with icons."
  (when (and (boundp 'flycheck-current-errors) flycheck-mode)
    (let* ((counts (flycheck-count-errors flycheck-current-errors))
           (errors   (or (cdr (assq 'error counts)) 0))
           (warnings (or (cdr (assq 'warning counts)) 0))
           (infos    (or (cdr (assq 'info counts)) 0))
           (icon-search (all-the-icons-faicon "search"
																							:height 1.0 :v-adjust 0 :face '(:foreground "#cc66ff")))
           (txt   (propertize ":"         'face '(:foreground "#cc66ff")))
           (icon-error (all-the-icons-faicon "times-circle"
                                              :height 1.0 :v-adjust 0 :face '(:foreground "#ff0066")))
           (icon-warning (all-the-icons-faicon "exclamation-triangle"
                                                :height 1.0 :v-adjust 0 :face '(:foreground "#ffff00")))
           (icon-info (all-the-icons-faicon "info-circle"
                                             :height 1.0 :v-adjust 0 :face '(:foreground "#00ff00"))))
      (format "%s%s[ %s %d %s %d %s %d ]"
              icon-search txt
              icon-error errors
              icon-warning warnings
              icon-info infos))))

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
			 (propertize "LSP-Bridge" 'face '(:foreground "#00ff00" :height 1.0))))
		 ((and (boundp 'lsp-mode) lsp-mode)
			(concat
			 (propertize "LSP-Mode" 'face '(:foreground "#9966ff" :height 1.0))))
		 (t (concat 
				 (propertize "No LSP" 'face '(:foreground "#b3e6ff" :height 1.0))))))

	;; ------------------- time显示 -------------------
  (spaceline-define-segment my-time
  "Current time string with FontAwesome clock icon."
  (concat
		(all-the-icons-faicon "clock-o"
												 :height 1.0
												 :v-adjust 0
												 :face '(:foreground "#ff8533"))
												 " "
												 (format-time-string "%H:%M")
												 "               "))


  ;; ------------------- 当前窗口编号 -------------------
  (spaceline-define-segment my-winum
  "Window number (winum)，使用图标代替文字。"
  (when (bound-and-true-p winum-mode)
    (concat
     (all-the-icons-faicon "bookmark"
                           :height 1.0
                           :v-adjust 0
                           :face '(:foreground "#ff8533"))
     " "
     (winum-get-number-string))))

  ;; ------------------- 光标位置 -------------------
  (spaceline-define-segment my-position
    "Cursor line:column."
		(concat
		 (all-the-icons-faicon "pencil"
													 :height 1.0
													 :v-adjust 0
													 :face '(:foreground "#b3b3ff"))
		 " "
		 (format-mode-line "%l:%c")))


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
															 :face '(:foreground "#f1502f"))
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
           ;; 显示文字
           (file-char (propertize "char" 'face '(:foreground "#ffcc00")))
           (file-word (propertize "word" 'face '(:foreground "#66ff66")))
           (file-line (propertize "line" 'face '(:foreground "#66ccff")))
           (file-attach (all-the-icons-material "attach_file" :height 1.0 :v-adjust 0 :face '(:foreground "#ff3399")))
           (file-txt   (propertize ":"     'face '(:foreground "#ff3399"))))
      ;; 拼接显示内容
      (format "%s%s[ %s %d %s %d %s %d ]"
              file-attach file-txt
              file-char char-count
              file-word word-count
              file-line line-count))))

	;; -------------------------------
	;; 安装 spaceline 布局，并指定 face
	;; -------------------------------
	(spaceline-install
	 'main
	 ;; 左侧 segments 列表
	 `((my-winum        :face 'my/spaceline-face-theme :priority 90)		; 窗口编号
		 (my-buffer-id    :face 'my/spaceline-face-theme :priority 85)		; buffer 名称 + 图标
		 (my-save-status 	:face 'my/spaceline-face-theme :priority 85)		; 文件保存
		 (my-major-mode   :face 'my/spaceline-face-theme :priority 70)   	; major-mode 名称
		 (my-flycheck     :face 'my/spaceline-face-theme :priority 20)   	; flycheck 错误/警告/提示图标
		 (my-file-stats	  :face 'my/spaceline-face-theme :priority 75)		; 字数计算
		 (my-project-dir  :face 'my/spaceline-face-theme :priority 15 :max-length 40)		; 项目路径
		 (my-diff-hl		  :face 'my/spaceline-face-theme :priority 10)		; git
		)

	 ;; 右侧 segments 列表
	 `(
		 (my-lsp-status  :face 'my/spaceline-face-theme :priority 80)		; LSP 状态
		 (my-position    :face 'my/spaceline-face-theme :priority 80)		; 行:列
		 (my-time        :face 'my/spaceline-face-theme :priority 100) 	; 时间
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
