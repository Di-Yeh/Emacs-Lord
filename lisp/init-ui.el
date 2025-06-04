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
(load-theme 'catppuccin t)

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

;; --- winum ---
(use-package winum
	:ensure t)
(require 'winum)
(winum-mode)

;; --- eyebrowse ---
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t))



;; --- 自动安装所需要的包 ---
(dolist (pkg '(memoize powerline spaceline))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; -------------------------------
;; 加载必要的包
;; -------------------------------
(require 'powerline)
(require 'spaceline)
(require 'spaceline-config)

;; -------------------------------
;; 使用 spaceline-spacemacs-theme
;; -------------------------------
;; 注意：该主题会自动配置 spaceline 的各个 segment，但会依赖 powerline-default-separator
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'arrow))  ;; 设置箭头分隔符

(use-package spaceline
  :ensure t
  :after powerline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-compile))


;; -------------------------------
;; 配置 Catppuccin (Mocha) 颜色
;; -------------------------------
;; 以下定义了几个常用的颜色变量，可以根据需要调整
(setq my-catppuccin-bg      "#1e1e2e")  ;; 主背景色
(setq my-catppuccin-fg      "#cdd6f4")  ;; 主前景色（文字颜色）
(setq my-catppuccin-accent  "#f38ba8")  ;; 某些高亮或强调使用的颜色（例如按钮的颜色）
(setq my-catppuccin-active1 "#252634")  ;; 用于部分区块的背景
(setq my-catppuccin-active2 "#363646")  ;; 用于另一部分区块的背景

;; 设置 mode-line 的总体外观
(set-face-attribute 'mode-line nil
                    :background my-catppuccin-bg
                    :foreground my-catppuccin-fg
                    :box nil)

;; 为非激活窗口设置较暗的背景
(set-face-attribute 'mode-line-inactive nil
                    :background my-catppuccin-active1
                    :foreground my-catppuccin-fg
                    :box nil)

;; 调整 spaceline 的内部分段所用的 face
(set-face-attribute 'powerline-active1 nil
                    :background my-catppuccin-active1
                    :foreground my-catppuccin-fg)
(set-face-attribute 'powerline-active2 nil
                    :background my-catppuccin-active2
                    :foreground my-catppuccin-fg)






(provide 'init-ui)
