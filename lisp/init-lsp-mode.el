;;; init-lsp-mode.el --- LSP configuration via lsp-mode

;;; Commentary:
;; 此配置自动安装并初始化 lsp-mode 以及相关插件，
;; 并借助 lsp-booster 确保 JSON 解析采用 plist 方式。
;;; Code:

;; 定义需要安装的插件列表
(setq lsp-mode-required-packages
      '(lsp-mode yasnippet lsp-treemacs helm-lsp
                 projectile hydra flycheck company
                 avy which-key helm-xref dap-mode))
(when (cl-find-if-not #'package-installed-p lsp-mode-required-packages)
  (package-refresh-contents)
  (mapc #'package-install lsp-mode-required-packages))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  ;; 设置 LSP 快捷键前缀，例如：C-c l r 重命名，C-c l d 跳转定义等
  (setq lsp-keymap-prefix "C-c l")

  :config
  ;; clangd 相关参数：自动补全、背景索引、禁用自动 include
  (setq lsp-clients-clangd-args
        '("--background-index"          ; 后台索引，加快跳转响应
          "--clang-tidy"                ; 启用 clang-tidy
          "--completion-style=detailed" ; 显示完整补全信息
          "--header-insertion=never"  	; 不自动插入头文件
					"--compile-commands-dir=."))  ; 如果你将 compile_commands.json 放在项目根目录

  ;; 通用 LSP 设置
  (setq lsp-completion-provider :capf                    ; 使用 Emacs 的 Capf 补全
        lsp-enable-symbol-highlighting t                 ; 高亮变量名
        lsp-diagnostic-package :none                     ; 不用内建诊断系统（你可能用 flycheck）
        lsp-completion-enable-additional-text-edit nil   ; 禁用附加的编辑动作
        lsp-prefer-flymake nil                           ; 使用 flycheck 而不是 flymake
        lsp-auto-install-server nil))                    ; 不自动下载语言服务器


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 1.5
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 1
        lsp-ui-peek-enable t
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        lsp-ui-peek-fontify 'on-demand
        lsp-ui-imenu-auto-refresh t
        lsp-ui-imenu-kind-position 'top))
(define-key global-map (kbd "C-c l s") 'lsp-ui-doc-show)
(define-key global-map (kbd "C-c l l") 'lsp-ui-doc-hide)


(use-package lsp-treemacs
  :ensure t
  :after lsp  ;; 确保 lsp-mode 已经加载
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols)
  :config
  ;; 开启 project 和 LSP workspace 双向同步（可选）
  (lsp-treemacs-sync-mode 1))


;; =========================
;; DAP 模式配置示例（交互选择参数，并动态修正尾随点问题）
;; =========================
(require 'ht)  ;; 用于构造 hash table

;; 根据操作系统查找 gdb 可执行文件
(defun my/get-gdb-path ()
  "返回 gdb 可执行文件路径。
Windows 下使用 'where gdb'，Linux/Mac 下使用 'which -a gdb'。
当多于一项时，利用 completing-read 进行选择。"
  (let* ((cmd (if (eq system-type 'windows-nt)
                  "where gdb"
                "which -a gdb"))
         (output (shell-command-to-string cmd))
         (lines (split-string output "\n" t)))  ;; 去除空行
    (if (null lines)
        (error "未找到 gdb，请确保 gdb 已安装且在 PATH 中")
      (if (= (length lines) 1)
          (string-trim (car lines))
        (string-trim (completing-read "选择 gdb 路径: " lines nil t))))))

(defun my/normalize-path (path)
  "Normalize PATH for Windows: 将正斜杠替换成反斜杠并转成小写。"
  (if (eq system-type 'windows-nt)
      ;; 注意：这里 "\\" 表示实际的反斜杠，replace-regexp-in-string 会转换所有 '/' 为 '\\'
      (downcase (replace-regexp-in-string "/" "\\\\" path))
    path))

(defun my/dap-register-c-c++ ()
  "交互式注册一个 C/C++ 调试模板到 dap-mode。
使用文件选择器选择目标可执行文件和目录选择器选择调试工作目录，
并自动生成源映射（将当前文件所在目录映射到本地目录）。
模板中使用 gdb 及你已经配置好的 gdb 路径。"
  (interactive)
  (let* ((gdb-path (my/normalize-path (my/get-gdb-path)))
         ;; 如果当前 buffer 有文件，则使用其所在目录；否则使用 default-directory
         (default-dir (if buffer-file-name
                          (file-name-directory (file-truename buffer-file-name))
                        default-directory))
         ;; 用文件选择器选择目标可执行文件，显示完整路径
         (target (my/normalize-path (read-file-name "请选择目标可执行文件: " default-dir nil t)))
         ;; 用目录选择器选择调试工作目录
         (cwd (my/normalize-path (read-directory-name "选择调试工作目录: " default-dir nil t)))
         (src-map (ht)))
    ;; 使用目录映射：将当前文件所在目录映射为本地目录
    (when buffer-file-name
      (let* ((local (my/normalize-path (file-truename buffer-file-name)))
             (local-dir (file-name-directory local)))
        (puthash (downcase local-dir) (downcase local-dir) src-map)
        (message "注册模板时自动生成源映射：\n键: %s\n值: %s" local-dir local-dir)))
    (dap-register-debug-template
     "C/C++ Debug Registered"
     (list :type "gdb"
           :request "launch"
           :name "C/C++ Debug Registered"
           :gdbpath gdb-path
           :target target
           :cwd cwd
           :sourceFileMap src-map))
    (message "已注册调试模板：C/C++ Debug Registered")
    (message "请确保编译时加上了调试符号 (-g) 且关闭了过高的优化，否则断点可能不会生效。")))

;; ------------------------------
;; 载入并初始化 dap-mode
;; ------------------------------
(use-package dap-mode
  :ensure t
  :after lsp-mode  ;; 确保 lsp-mode 先加载
  :config
  (dap-auto-configure-mode)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

;; ------------------------------
;; 快捷键绑定
;; ------------------------------
(define-key global-map (kbd "C-c l d") 'dap-hydra)           ;; 调试操作面板
(define-key global-map (kbd "C-c l R") 'my/dap-register-c-c++)   ;; 注册调试模板
(define-key global-map (kbd "C-c l b") 'dap-breakpoint-toggle)   ;; 切换当前行断点

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)   ; 补全注解右对齐
  (company-minimum-prefix-length 1)       ; 输入 1 个字符就开始补全
  (company-idle-delay 0.1)                ; 补全延迟时间
  (company-show-numbers t)                ; 显示候选项编号
  (company-tooltip-limit 10)              ; 显示最多 10 个候选项
  (company-require-match nil)             ; 不强制匹配
  (company-dabbrev-downcase nil)          ; 保留大小写
  (company-global-modes '(not eshell-mode shell-mode)) ; 不在 shell 中启用
  :config
  ;; 默认使用 Emacs 字体缩放同步调整 popup
  (setq company-tooltip-minimum-width 40) ;; 保持宽度统一
  ;; 更优雅的样式颜色
  (custom-set-faces
   '(company-tooltip ((t (:inherit default :background "#282c34" :foreground "#bbc2cf"))))
   '(company-tooltip-selection ((t (:background "#3e4451" :foreground "#ffffff"))))
   '(company-tooltip-common ((t (:foreground "#c678dd" :weight bold))))
   '(company-tooltip-annotation ((t (:foreground "#56b6c2"))))
   '(company-scrollbar-bg ((t (:background "#3e4451"))))
   '(company-scrollbar-fg ((t (:background "#61afef"))))
   '(company-tooltip-search ((t (:foreground "#98be65" :weight bold))))
   '(company-preview ((t (:background "#1c1f24" :foreground "#5B6268"))))
   '(company-preview-common ((t (:inherit company-preview :foreground "#c678dd"))))))

(use-package kind-icon
  :straight t
  :after company
  :custom
  (kind-icon-default-face 'corfu-default) ;; 跟 corfu 同样的图标样式
  :config
  (add-to-list 'company-format-margin-function #'kind-icon-margin-formatter))

;; 让 Emacs completion-at-point 全局不分大小写
(setq completion-ignore-case               t
      read-buffer-completion-ignore-case  t
      read-file-name-completion-ignore-case t)

;; 用 flex 做模糊匹配，basic 保留原生前缀匹配
(setq completion-styles '(flex basic))

;; --------------------------------------------------
;; Flycheck 配置（全局启用，保存时检查）
;; --------------------------------------------------
;; Flycheck 语法检查配置，配合 lsp-mode 使用 clangd 做诊断
(use-package flycheck
  :ensure t
  :init
  ;; 全局启用 Flycheck
  (global-flycheck-mode)

  :config
  ;; 检查时机：只在保存或启用 mode 时检查，不在编辑时实时检查
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; 禁用 flycheck 自带的 C/C++ checker，使用 lsp 提供的诊断
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook
              (lambda ()
                (setq-local flycheck-disabled-checkers
                            '(c/c++-clang c/c++-gcc c/c++-msvc))
                ;; 设置 lsp-mode 使用 flycheck 来显示诊断信息
                (setq-local lsp-diagnostics-provider :flycheck)))))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(json-python-json))
  (flycheck-add-mode 'json-jsonlint 'json-mode))

(require 'helm-xref)
(helm-mode)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)


(which-key-mode)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)



(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))



(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
