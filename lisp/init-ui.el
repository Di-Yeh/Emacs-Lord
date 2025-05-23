;; ─────────────────────────────────────────────────────────────
;; Catppuccin 多主题风格支持：latte、frappe、macchiato、mocha
;; 默认加载 mocha，可使用 C-t 依次切换四种风格

;; ─────────────────────────────────────────────────────────────

;; 安装主题（确保你已经安装了 catppuccin-theme 包）
(use-package catppuccin-theme
  :ensure t
  :init
  (load-theme 'catppuccin t)) ;; 启动时默认风格

;; 定义 catppuccin 可用风格
(defvar my/catppuccin-flavors '(latte frappe macchiato mocha)
  "Catppuccin 可用风格列表。")

;; 当前索引
(defvar my/catppuccin-index 0
  "当前 catppuccin 风格索引。")

;; 切换主题函数
(defun my/cycle-catppuccin-themes ()
  "轮换 Catppuccin 主题风格（latte, frappe, macchiato, mocha）。"
  (interactive)
  (setq catppuccin-flavor (nth my/catppuccin-index my/catppuccin-flavors))
  (catppuccin-reload)
  (message "Catppuccin 风格切换为：%s" catppuccin-flavor)
  (setq my/catppuccin-index (mod (1+ my/catppuccin-index) (length my/catppuccin-flavors))))

;; 绑定快捷键 Ctrl-t 切换风格
(global-set-key (kbd "C-t") #'my/cycle-catppuccin-themes)


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
