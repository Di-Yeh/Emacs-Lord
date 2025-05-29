(use-package doom-themes
  :ensure t
  :init
  ;; 在 init 阶段就设置默认主题，确保在其它 UI 插件加载前生效
  (load-theme 'doom-acario-dark t)
  :config
  ;; 保存主题的文件位置
  (defvar my/theme-save-file (locate-user-emacs-file ".last-theme")
    "保存最近使用的主题名称。")

  ;; 当前主题
  (defvar my/current-theme 'doom-acario-dark
    "当前启用的 Doom 主题。")

  ;; 尝试加载上次保存的主题
  (when (file-exists-p my/theme-save-file)
    (let* ((theme-name (string-trim (with-temp-buffer
                                      (insert-file-contents my/theme-save-file)
                                      (buffer-string))))
           (theme-symbol (intern theme-name)))
      (when (member theme-symbol (custom-available-themes))
        (setq my/current-theme theme-symbol)
        (load-theme my/current-theme t))))

  ;; Doom theme 一些增强功能（可选）
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; C-t 切换主题界面（之前定义的）
(defun my/choose-doom-theme ()
  "弹出 Doom 主题选择器，立即切换并保存当前主题。"
  (interactive)
  (let* ((themes (cl-remove-if-not
                  (lambda (sym)
                    (string-prefix-p "doom-" (symbol-name sym)))
                  (custom-available-themes)))
         (theme (intern (completing-read "选择 Doom 主题: " themes nil t))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (setq my/current-theme theme)
    (with-temp-file my/theme-save-file
      (insert (symbol-name theme)))
    (message "✅ 当前主题已切换为: %s" theme)))

(global-set-key (kbd "C-t") #'my/choose-doom-theme)



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
