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






;;; -*- lexical-binding: t -*-

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; 不显示各种 minor mode 名称（例如 helm、ivy、compy 等）
  (doom-modeline-minor-modes nil)
  ;; 启用内置字数统计（仅在 text-mode 派生的缓冲区中有效）
  (doom-modeline-enable-word-count t)
  ;; 文件名采用带项目名称的截断显示形式
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; 设定 buffer-position 的显示格式，显示当前行、列以及百分比
  (doom-modeline-buffer-position-format '("L %l, C %c (%p)"))
  :config
  (column-number-mode 1)
  (display-line-numbers-mode 1))










(provide 'init-ui)
