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
  (setq lsp-keymap-prefix "C-c l")
	:custom
  (setq lsp-clients-clangd-args
      '("--background-index"         ;; 后台索引
        "--clang-tidy"               ;; 打开 clang-tidy
        "--completion-style=detailed";; 丰富补全格式
        "--header-insertion=never"   ;; 关掉自动插 include
        ))
  :config
  (setq lsp-completion-provider :capf
        lsp-enable-symbol-highlighting t
				lsp-diagnostic-package :none
				lsp-completion-enable-additional-text-edit nil
        lsp-prefer-flymake nil
        lsp-auto-install-server nil))


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
  :config
  (setq company-minimum-prefix-length 1
        company-show-quick-access t))

;; 让 Emacs completion-at-point 全局不分大小写
(setq completion-ignore-case               t
      read-buffer-completion-ignore-case  t
      read-file-name-completion-ignore-case t)

;; 用 flex 做模糊匹配，basic 保留原生前缀匹配
(setq completion-styles '(flex basic))



(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :config
  ;; 如果系统是 Windows，则重载获取桌面名称的函数，以防止 wmctrl 的依赖问题
  (when (eq system-type 'windows-nt)
    (defun company-box--get-desktop-name ()
      "在 Windows 上返回一个虚拟桌面名称，绕过 wmctrl 依赖。"
      "windows-desktop"))
  ;; 设置 company-box 使用 all-the-icons 来显示图标
  (when (require 'all-the-icons nil t)
    (setq company-box-icons-all-the-icons
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :v-adjust -0.2))
            (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :v-adjust -0.2))
            (Method        . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Function      . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Field         . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :v-adjust -0.2))
            (Class         . ,(all-the-icons-material "class"                    :height 0.8 :v-adjust -0.2))
            (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.2))
            (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :v-adjust -0.2))
            (Property      . ,(all-the-icons-material "settings"                 :height 0.8 :v-adjust -0.2))
            (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :v-adjust -0.2))
            (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :v-adjust -0.2))
            (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :v-adjust -0.2))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :v-adjust -0.2))
            (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :v-adjust -0.2))
            (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :v-adjust -0.2))
            (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :v-adjust -0.2))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :v-adjust -0.2))
            (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :v-adjust -0.2))
            (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :v-adjust -0.2))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :v-adjust -0.2))
            (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :v-adjust -0.2))
            (Event         . ,(all-the-icons-material "event"                    :height 0.8 :v-adjust -0.2))
            (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :v-adjust -0.2))
            (Template      . ,(all-the-icons-material "short_text"               :height 0.8 :v-adjust -0.2))))
    ;; 如果需要，你也可以设置 company-box-backends-colors 为 nil 或其它合适的值
    (setq company-box-backends-colors nil)))


;; --------------------------------------------------
;; Flycheck 配置（全局启用，保存时检查）
;; --------------------------------------------------
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  ;; 只在保存文件和启用时检查，不在编辑时自动检查
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
	;; 对 C/C++ 及头文件，禁用 Flycheck 本身的 checker，只用 LSP 提供的诊断
  (dolist (hook '(c-mode-common-hook c++-mode-hook))
    (add-hook hook
              (lambda ()
                (when (and buffer-file-name
                           (string-match-p "\\.\\(c\\|cc\\|cpp\\|cxx\\|h\\|hpp\\)$"
                                           buffer-file-name))
                  (setq-local flycheck-disabled-checkers
                              '(c/c++-clang c/c++-gcc c/c++-msvc))
                  ;; 使用 LSP diagnostics
                  (setq-local lsp-diagnostics-provider :flycheck))))))

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
