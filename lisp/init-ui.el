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
				(message "已切换至 %s" choice)))))

;; 绑定全局快捷键 C-t 调用主题切换函数
(global-set-key (kbd "C-t") 'my/switch-theme)



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
;; (require 'flycheck)

(winum-mode 1)

;; -------------------------------
;; 设置 powerline 的分隔符样式
;; -------------------------------
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'arrow))

;; -------------------------------
;; 定义 Catppuccin Mocha 颜色
;; -------------------------------
(setq my-catppuccin-bg      "#2e3440")	;; 主背景色
(setq my-catppuccin-fg      "#e5e9f0")	;; 主文字色
(setq my-catppuccin-accent  "#f7768e")	;; 强调色
(setq my-catppuccin-active1 "#4c566a")	;; 区块1背景
(setq my-catppuccin-active2 "#b980ff")	;; 区块2背景


;; 设置主 mode-line 外观
(set-face-attribute 'mode-line nil
                    :background my-catppuccin-bg
                    :foreground my-catppuccin-fg
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :background my-catppuccin-active1
                    :foreground my-catppuccin-fg
                    :box nil)

(set-face-attribute 'powerline-active1 nil
                    :background my-catppuccin-active1
                    :foreground my-catppuccin-fg)

(set-face-attribute 'powerline-active2 nil
                    :background my-catppuccin-active2
                    :foreground my-catppuccin-fg)



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
       (all-the-icons-faicon "times-circle" :height 1.0 :v-adjust 0)   ; 错误图标
       (format " %d " (or .error 0))
       (all-the-icons-faicon "exclamation-triangle" :height 1.0 :v-adjust 0) ; 警告图标
       (format " %d " (or .warning 0))
       (all-the-icons-faicon "info-circle" :height 1.0 :v-adjust 0)    ; 提示图标
       (format " %d" (or .info 0))))))


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
		 (my-project-dir)
     (my-buffer-id)
     (my-major-mode)
		 (my-flycheck)
     )
   ;; 右侧
   '((my-position)
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









(provide 'init-ui)
