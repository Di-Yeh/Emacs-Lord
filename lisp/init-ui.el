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

(use-package winum
	:ensure t)

(winum-mode 1)


;;; -*- lexical-binding: t -*-
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; 重要功能保留，精简不必要的部分
  (doom-modeline-buffer-file-name-style 'truncate-with-project) ;; 文件名样式
  (doom-modeline-window-number t)              ;; 显示 buffer 编号
  (doom-modeline-project-detection 'auto)      ;; 项目检测
  (doom-modeline-lsp t)                        ;; 显示 LSP 状态
  (doom-modeline-line-number t)                ;; 显示当前行号
  (doom-modeline-checker-icon t)               ;; 显示错误图标
  (doom-modeline-anzu t)                       ;; 显示匹配计数
  (doom-modeline-recording t)                  ;; 宏录制提示
  (doom-modeline-height 45)                    ;; modeline 高度
  (doom-modeline-major-mode-icon t)            ;; 显示 major-mode 图标
  (doom-modeline-vcs-max-length 12)            ;; 显示 Git 信息
  (column-number-mode t)                       ;; 显示列号
  (display-time-mode 1)                        ;; 在 modeline 显示时间
  (doom-modeline-enable-word-count t)          ;; 启用总字数统计
  (doom-modeline-buffer-name t)                ;; 显示 buffer 名称
  (doom-modeline-percent-position '(-3 "%p"))  ;; 显示文件百分比位置
  (doom-modeline-total-line-number t)          ;; 显示总行数
  (setq inhibit-compacting-font-caches t))


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
		;;(powerline-reset)
		))

;; 绑定全局快捷键 C-t 调用主题切换函数
(global-set-key (kbd "C-t") 'my/switch-theme)




(provide 'init-ui)
