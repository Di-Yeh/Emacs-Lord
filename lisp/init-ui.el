;;; init-ui.el--- Custom UI configuration 
;;; -*- lexical-binding: t -*-
;;; Commentary:
;; 这里可以简单描述下你的配置用途
;;; Code:

;; 安装主题包
(use-package catppuccin-theme
  :ensure t)

(use-package modus-themes
  :ensure t)

(use-package spacemacs-theme
  :straight t
  :defer t)

;; 默认加载 Catppuccin 的 Mocha 主题（Catppuccin 的主题依赖于变量 catppuccin-flavor）
(setq catppuccin-flavor 'mocha)

;;; -*- lexical-binding: t -*-
;;; ----------------------------------------------
;;; 基础：自动安装所需包
;;; ----------------------------------------------
(dolist (pkg '(powerline spaceline all-the-icons diff-hl))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;; ----------------------------------------------
;;; 加载必要库
;;; ----------------------------------------------
(require 'powerline)
(require 'spaceline)
(require 'spaceline-config)  ;; 必须先载入，提供 spacíeline-install
(require 'all-the-icons)  ;; 确保 icon 可用
(require 'diff-hl)

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))  ;; 实时刷新

;;; ----------------------------------------------
;;; 🖌️ 自定义 mode-line & spaceline face，防止被主题覆盖
;;; ----------------------------------------------
(defun my/fix-mode-line-faces (&rest _args)
  "统一清除 theme/spaceline/powerline 在 theme 切换时残留的 box 属性，
并把背景都设成 #1E1E1E。"
  (let ((bg "#1E1E1E")
        (fg-active   "#ffffff")
        (fg-inactive "#888888"))
    ;; 基本 mode-line
    (dolist (face '(mode-line mode-line-inactive))
      (when (facep face)
        (set-face-attribute face nil
                            :background bg
                            :foreground (if (eq face 'mode-line) fg-active fg-inactive)
                            :box nil
                            :overline nil
                            :underline nil
                            :height 1.15)))
    ;; Powerline/Spaceline 背景段
    (dolist (face '(powerline-active1 powerline-active2
                    powerline-inactive1 powerline-inactive2
                    spaceline-highlight-face
                    spaceline-evil-normal
                    spaceline-evil-insert
                    spaceline-evil-visual
                    spaceline-evil-replace
                    spaceline-evil-motion
                    spaceline-unmodified))
      (when (facep face)
        (set-face-attribute face nil
                            :background bg
                            :box nil
                            ;; 继承 mode-line, 保留文字色
                            :inherit 'mode-line)))))

;; 在每次 load-theme 后延迟执行，确保覆盖所有 theme 动作
(advice-add 'load-theme :after
            (lambda (&rest _)
              (run-at-time 0.1 nil #'my/fix-mode-line-faces)))

;; 启动时先执行一次
(my/fix-mode-line-faces)


;;; ----------------------------------------------
;;; 定义 spaceline segment 专用 face
;;; ----------------------------------------------
(defface my/spaceline-face-theme
  '((t (
				:background "#1E1E1E"
	 			:foreground "#ffffff" :weight bold)))
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
	
	;; ------------------- 文件路径 + 文件名 -------------------
	(spaceline-define-segment my-buffer-id
		"显示文件图标 + 缩略路径（最后两个目录） + 文件名，例如：.emacs.d/lisp/init-ui.el"
		(when (and (buffer-file-name) (fboundp 'all-the-icons-icon-for-buffer))
			(let* ((filepath (abbreviate-file-name (buffer-file-name)))
						 ;; 去掉结尾斜杠并分割路径
						 (components (split-string filepath "/" t))
						 ;; 获取最后三个部分：最后两个目录 + 文件名
						 (last-components (last components (min 3 (length components))))
						 (short-path (mapconcat #'identity last-components "/"))
						 (icon (all-the-icons-icon-for-buffer))
						 ;; 给路径加颜色
						 (styled-path (propertize short-path 'face '(:foreground "#d70751"))))
				(concat icon " " styled-path))))


  ;; ------------------- 主模式 -------------------
  (spaceline-define-segment my-major-mode
    "Major mode name."
    (format-mode-line mode-name))

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
           (icon-error (all-the-icons-faicon "times-circle"
                                              :height 1.0 :v-adjust 0 :face '(:foreground "#ff0066")))
           (icon-warning (all-the-icons-faicon "exclamation-triangle"
                                                :height 1.0 :v-adjust 0 :face '(:foreground "#ffff00")))
           (icon-info (all-the-icons-faicon "info-circle"
                                             :height 1.0 :v-adjust 0 :face '(:foreground "#00ff00"))))
      (format "%s %d %s %d %s %d"
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
		"返回一个字符串，描述当前激活的是 lsp-mode 还是 lsp-bridge"
		(cond
		 ((and (boundp 'lsp-bridge-mode) lsp-bridge-mode)
			(concat
			 (propertize "LSP-Bridge" 'face '(:foreground "#00ff00" :height 1.0))))
		 ((and (boundp 'lsp-mode) lsp-mode)
			(concat
			 (propertize "LSP-Mode" 'face '(:foreground "#9966ff" :height 1.0))))))

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
												 "          "))

  ;; ------------------- 光标位置 ------------------
	(spaceline-define-segment my-position
  "以 [ 行:列 ] 方式显示光标位置，并为数字部分设置红色背景。"
  (let* ((line (propertize (format-mode-line "%l")
                           'face '(:background "#ff0000" :foreground "black")))
         (col  (propertize (format-mode-line "%c")
                           'face '(:background "#ff0000" :foreground "black")))
         (sep  (propertize ":" 'face '(:background "#ff0000" :foreground "black"))))
    (concat
     " ["
     line sep col
     "]")))



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
           (file-char (propertize "Ⓒ" 'face '(:foreground "#ffcc00")))
           (file-word (propertize "Ⓦ" 'face '(:foreground "#66ff66")))
           (file-line (propertize "Ⓛ" 'face '(:foreground "#66ccff"))))
      ;; 拼接显示内容
      (format "%s %d %s %d %s %d"
              file-char char-count
              file-word word-count
              file-line line-count))))

  ;;; ----------------------------------------------
	;;; 输入法状态显示（[英]/[中]）
	;;; ----------------------------------------------

  ;; 返回当前输入法状态的字符串，用于 mode-line
	(defun my/mode-line-input-method-indicator ()
		"显示当前输入法状态：[英] 或 [中]"
		(let ((method current-input-method))
			(if (and method (string-prefix-p "pyim" method))
					;; 当前为中文输入法，突出显示“中”
					(concat "[" (propertize "中" 'face '(:foreground "#ff0000")) "]")
				;; 否则为英文
				(concat "[" (propertize "En" 'face '(:foreground "#ffff66")) "]"))))

	;; 用于强制刷新 mode-line
	(defun my/refresh-mode-line ()
		"强制刷新 mode-line。"
		(force-mode-line-update t))

	;; 在输入法切换时刷新 mode-line
	(add-hook 'input-method-activate-hook #'my/refresh-mode-line)
	(add-hook 'input-method-inactivate-hook #'my/refresh-mode-line)

	;; 如果你用 spaceline，可以这样加进去（否则见下方 vanilla mode-line 添加方式）
	(when (featurep 'spaceline)
		(spaceline-define-segment my-input-method
			(my/mode-line-input-method-indicator))

		;; 添加到主 mode-line 格式中（你也可以插入别的位置）
		(spaceline-compile
			"my-line"
			'((my-input-method :face highlight-face)
				my-buffer-id)
			'((line-column :separator "|")
				buffer-encoding)))


  ;;; ----------------------------------------------
	;;; 以百分比显示当前在buffer中的位置
  ;;; ----------------------------------------------
	(spaceline-define-segment buffer-percent-position
  "以百分比显示当前在 buffer 中的位置。"
  (let* ((total (point-max))
         (pos (point))
         (percent (if (> total 0)
                      (floor (* 100 (/ (float pos) total)))
                    0)))
    (format "%d%%%%" percent)))

  ;;; ----------------------------------------------
	;;; 显示编码风格和系统图标
  ;;; ----------------------------------------------
  (spaceline-define-segment buffer-encoding-eol
  "显示当前 buffer 的编码与行结尾风格，并加上对应操作系统图标。"
  (when (and buffer-file-name buffer-file-coding-system) ; 仅在已保存文件时处理
    (let* ((raw-cs buffer-file-coding-system)
           (cs (cond
                ((vectorp raw-cs) (aref raw-cs 0))
                ((listp   raw-cs) (car    raw-cs))
                (t                raw-cs)))
           (eol-type (coding-system-eol-type cs))      ; 0=Unix,1=DOS,2=Mac
           (eol-str  (nth eol-type '("Unix" "DOS" "Mac")))
           (icon     (pcase eol-type
                       (0 (all-the-icons-faicon "linux"   :height 1.0 :v-adjust 0 :face '(:foreground "#ffcc00")))
                       (1 (all-the-icons-faicon "windows" :height 1.0 :v-adjust 0 :face '(:foreground "#3399ff")))
                       (2 (all-the-icons-faicon "apple"   :height 1.0 :v-adjust 0 :face '(:foreground "#ff0066")))))
           (base     (coding-system-base cs))
           (name     (upcase (symbol-name base)))
           (text     (format "%s | %s" name eol-str)))
      (concat " " icon " " text))))


  ;;; ----------------------------------------------
	;;; 显示最近输入的字符或按键
  ;;; ----------------------------------------------
	(defface my/spaceline-last-input-face
  '((t (:foreground "black" :background "#76a1ef" :weight bold)))
  "Face for displaying last input or command in spaceline.")

	(defvar my/spaceline-last-input ""
  "记录最近一次输入的命令或按键。")

	(defun my/update-last-input ()
  "更新最后一个按键或命令到 `my/spaceline-last-input`。"
  (let ((key (this-command-keys-vector)))
    (setq my/spaceline-last-input
          (if (and (vectorp key) (> (length key) 0))
              (key-description key)
            ""))))
	(add-hook 'pre-command-hook #'my/update-last-input)

	(spaceline-define-segment my-last-input
  "显示最近一次输入的按键或命令。"
  (when (and my/spaceline-last-input
             (not (string-empty-p my/spaceline-last-input)))
    (propertize (format "[%s]" my/spaceline-last-input)
                'face 'my/spaceline-last-input-face)))

	;; -------------------------------
	;; 安装 spaceline 布局，并指定 face
	;; -------------------------------
	(spaceline-install
	 'main
	 ;; 左侧 segments 列表
	 `((my-buffer-id    :face 'my/spaceline-face-theme :priority 85 :max-width 40)		; buffer 名称 + 图标
		 (my-save-status  :face 'my/spaceline-face-theme :priority 85)		; 文件保存
		 (my-major-mode   :face 'my/spaceline-face-theme :priority 70)   	; major-mode 名称
		 (my-position    	:face 'my/spaceline-face-theme :priority 80)		; 行:列
		 (buffer-percent-position :face 'my/spaceline-face-theme :priority 80) ; 以百分比显示当前在buffer中的位置
		 (my-flycheck     :face 'my/spaceline-face-theme :priority 20)   	; flycheck 错误/警告/提示图标
		 (my-file-stats	  :face 'my/spaceline-face-theme :priority 75)		; 字数计算
		 (buffer-encoding-eol :face 'my/spaceline-face-theme :priority 80) ; 显示编码风格和系统图标
		 (my-diff-hl		  :face 'my/spaceline-face-theme :priority 10)		; git
		)

	 ;; 右侧 segments 列表
	 `((my-lsp-status  :face 'my/spaceline-face-theme :priority 80)		; LSP 状态
		 (my-last-input  :face 'my/spaceline-face-theme :priority 80) ; 最近输入讯息
		 (my-input-method :face 'my/spaceline-face-theme :priority 80)	; 输入法状态显示
		 (my-time        :face 'my/spaceline-face-theme :priority 100) 	; 时间
		))


  ;; 确保安装完成后刷新
  (powerline-reset))

;;; ----------------------------------------------
;;; 启用spaceline 主函数与时钟
;;; ----------------------------------------------
(display-time-mode 1)
(setq display-time-default-load-average nil)

;; (setq-default mode-line-format
;;   '("%e"
;;     (:eval
;;      (let* ((lhs (spaceline--seg main 'lhs))
;;             (rhs (spaceline--seg main 'rhs))
;;             ;; 计算剩余宽度：窗口总宽 – lhs 长度 – rhs 长度
;;             (fill-width (max 0 (- (window-total-width) 
;;                                   (length lhs) 
;;                                   (length rhs)))))
;;        (concat
;;         lhs
;;         ;; 填充成空白或分隔符
;;         (powerline-fill 'mode-line fill-width)
;;         rhs)))))

(setq-default mode-line-end-spaces (make-string 0 ?\s))
(setq-default truncate-lines t)
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

;;; ----------------------------------------------
;;; 🌗 交互式主题切换函数，支持 Catppuccin 与其它主题
;;; ----------------------------------------------
(defun my/switch-theme ()
  "交互式切换主题，支持 Catppuccin 多种风格和普通已安装主题。"
  (interactive)
  (let* ((catppuccin-options '("Catppuccin (Frappe)"
                               "Catppuccin (Mocha)"
                               "Catppuccin (Macchiato)"
                               "Catppuccin (Latte)"))
         (other-themes (remove "catppuccin"
                               (mapcar #'symbol-name (custom-available-themes))))
         (theme-list (append catppuccin-options other-themes))
         (choice (completing-read "选择主题: " theme-list nil t)))
    ;; 先关闭所有已启用主题
    (mapc #'disable-theme custom-enabled-themes)

    ;; 判断是否为 Catppuccin 风格
    (if (member choice catppuccin-options)
        (progn
          (cond
           ((string-match-p "Frappe" choice) (setq catppuccin-flavor 'frappe))
           ((string-match-p "Macchiato" choice) (setq catppuccin-flavor 'macchiato))
           ((string-match-p "Latte" choice) (setq catppuccin-flavor 'latte))
           (t (setq catppuccin-flavor 'mocha)))
          (load-theme 'catppuccin t)
          (message "已切换至 %s" choice))
      ;; 其它普通主题
      (load-theme (intern choice) t)
      (message "已切换至 %s" choice))

    ;; ✅ 每次切换主题后自动刷新 mode-line
    (my/fix-mode-line-faces)

    ;; ✅ 如果使用 powerline 或 spaceline，刷新它
    (when (fboundp 'powerline-reset)
      (powerline-reset))))

;; 全局快捷键：使用 C-t 快速切换主题
(global-set-key (kbd "C-t") 'my/switch-theme)

(load-theme 'wheatgrass t)

(provide 'init-ui)
