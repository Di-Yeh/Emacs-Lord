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




;; 状态栏
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
	(setq doom-modeline-support-imenu t)
  ;; 显示图标（确保安装 nerd-font）
  (doom-modeline-icon t)
  ;; 显示 buffer 编号（适配 winum, window-numbering, ace-window）
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-window-number t)
  ;; 显示项目根路径
  (doom-modeline-project-detection 'auto)
  ;; 显示 LSP 信息
  (doom-modeline-lsp t)
  ;; 显示Python虚拟环境版本
  (doom-modeline-python-executable "python3")
  ;; 显示当前行号
  (doom-modeline-line-number t)
  ;; flycheck/flymake 错误/警告
  (doom-modeline-checker-simple-format t)
  (doom-modeline-checker-icon t)
  ;; 显示电池信息
  (display-battery-mode t)
  (doom-modeline-battery t)
  ;; 显示匹配计数（anzu/iedit等）
  (doom-modeline-anzu t)
  ;; 宏录制提示
  (doom-modeline-irc t)
  (doom-modeline-env-version t)
  (doom-modeline-recording t)
  ;; 高度调整
  (doom-modeline-height 30)
  ;; 文件名样式
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; 开启 major-mode 图标
  (doom-modeline-major-mode-icon t)
  ;; 显示 Git 信息
  (doom-modeline-vcs-max-length 12)
	;(doom-modeline-line-number t)     ;; 显示行号
	(column-number-mode t)            ;; 显示列号（内建变量
	(display-time-mode 1) ;; 常显
	(display-time-24hr-format t) ;;格式
	(doom-modeline-lsp-icon t)
	(doom-modeline-time-analogue-clock t)
	(doom-modeline-enable-word-count t)
	(doom-modeline-buffer-name t)
	(doom-modeline-percent-position '(-3 "%p"))
	(doom-modeline-total-line-number t)
	(doom-modeline-workspace-name t)
	(doom-modeline-display-default-persp-name t)
	(doom-modeline-persp-icon t)
	(doom-modeline-time t)
	(doom-modeline-gnus-timer 2)
	;; Don’t compact font caches during GC.
	(inhibit-compacting-font-caches t))




(provide 'init-ui)
