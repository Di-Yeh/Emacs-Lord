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



;;; ================================
;;; 定义当前编译器变量
;;; ================================
(defvar my/compile-commands-compiler "clang++"
  "当前用于生成 compile_commands.json 的编译器，可为 clang++, gcc, cl.exe, clang-cl.exe 等。")

(defvar my/compile-commands-msvc-p
  (lambda () (string-match-p "\\(?:cl\\.exe\\|clang-cl\\)" my/compile-commands-compiler))
  "当编译器是 MSVC 或 clang-cl 时为 t，否则 nil.")

;;; ================================
;;; 切换编译器模板（C-c i c）
;;; ================================
(defun my/switch-compile-commands-compiler ()
  "交互式切换 Clang/GCC/MSVC/Other 模板，并保存到 ~/.emacs.d/。"
  (interactive)
  (let* ((choices '("Clang" "GCC" "MSVC" "Other"))
         (sel     (completing-read "选择编译器模板: " choices nil t))
         name path file)
    (cond
     ((string-equal sel "Clang")
      (setq name "clang" path "clang++"))
     ((member sel '("GCC" "MSVC"))
      (setq name (downcase sel)
            path (read-file-name (format "输入 %s 可执行文件路径: " sel)
                                 default-directory nil t)))
     (t
      (setq name (read-string "输入自定义模板名称: " nil nil "mycc")
            path (read-file-name (format "输入 %s 可执行文件路径: " name)
                                 default-directory nil t))))
    (setq my/compile-commands-compiler path)
    (setq file (expand-file-name (format "compile-commands-compiler-%s.el" name)
                                 user-emacs-directory))
    (with-temp-file file
      (insert (format ";; 自动保存的编译器模板: %s\n" name))
      (insert (format "(setq my/compile-commands-compiler %S)\n" path)))
    (message "✅ 切换到 [%s] 模板: %s\n已写入: %s" name path file)
    ;; 询问是否重启 clangd
    (when (and (bound-and-true-p lsp-mode)
               (fboundp 'lsp-restart-workspace)
               (yes-or-no-p "立即重启 clangd (lsp-mode)？"))
      (lsp-restart-workspace))))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c i c") #'my/switch-compile-commands-compiler))

;;; ================================
;;; 生成 compile_commands.json (C-c i j)
;;; ================================
(require 'json)
(require 'projectile)

(defun my/generate-compile-commands ()
  "根据 `my/compile-commands-compiler` 重新生成 compile_commands.json，并可重启 clangd。"
  (interactive)
  (let* ((root    (or (and (fboundp 'projectile-project-root)
                           (projectile-project-root))
                      default-directory))
         (out     (expand-file-name "compile_commands.json" root))
         (files   (directory-files-recursively
                   root "\\.\\(c\\|cc\\|cpp\\|cxx\\|h\\|hpp\\)$"))
         (is-msvc (funcall my/compile-commands-msvc-p))
         (entries
          (mapcar
           (lambda (file)
             (if is-msvc
                 ;; MSVC/clang-cl: 用 arguments 数组
                 `(("directory" . ,root)
                   ("arguments" .
                    ,(let ((args (list
                                  my/compile-commands-compiler
                                  (format "/I%s" (expand-file-name "include" root))
                                  "/nologo"
                                  "/c" (file-relative-name file root)
                                  "/Fo"
                                  (concat
                                   (file-relative-name
                                    (expand-file-name "build" root) root)
                                   "\\"
                                   (file-name-sans-extension
                                    (file-relative-name file root))
                                   ".obj"))))
                       args))
                   ("file" . ,file))
               ;; Clang/GCC: 用 command 字符串
               `(("directory" . ,root)
                 ("command"   .
                  ,(format "%s -I%s -std=c++17 -c %s -o %s"
                           my/compile-commands-compiler
                           (expand-file-name "include" root)
                           file
                           (expand-file-name
                            (format "build/%s.o"
                                    (file-name-sans-extension
                                     (file-name-nondirectory file)))
                            root)))
                 ("file" . ,file))))
           files)))
    ;; 写文件
    (unless files
      (user-error "❌ 未找到任何 C/C++ 源文件"))
    (when (or (not (file-exists-p out))
              (yes-or-no-p (format "覆盖 %s? " out)))
      (with-temp-file out
        (insert (json-encode entries)))
      (message "✅ %s 已生成/更新，使用编译器：%s"
               out my/compile-commands-compiler))
    ;; 如果是 MSVC，需要在 clangd 参数里加 --query-driver
    (when is-msvc
      (setq lsp-clients-clangd-args
            (list (format "--query-driver=%s" my/compile-commands-compiler))))
    ;; 重启 clangd
    (when (and (bound-and-true-p lsp-mode)
               (fboundp 'lsp-restart-workspace)
               (yes-or-no-p "立即重启 clangd (lsp-mode)？"))
      (lsp-restart-workspace))))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c i j") #'my/generate-compile-commands))

;;; ================================
;;; 添加include路径 (C-c i a)
;;; ================================
(require 'json)
(require 'projectile)
(require 'cl-lib)

(defvar my/compile-commands-compiler "clang++"
  "当前用于生成 compile_commands.json 的编译器，可为 clang++, gcc, cl.exe, clang-cl.exe 等。")

(defun my/compile-commands-msvc-p ()
  "若当前编译器是 MSVC 或 clang-cl，则返回 t。"
  (string-match-p "\\(?:cl\\.exe\\|clang-cl\\)" my/compile-commands-compiler))

(defun my/update-compile-commands-includes (paths)
  "将 PATHS 列表添加到 compile_commands.json 中所有条目的 include 参数里，并可重启 clangd。
支持两种模式：
- 对带 \"arguments\" 数组的条目（MSVC/clang-cl）：在编译器后插入 `/Ipath`。
- 对带 \"command\" 字符串的条目（Clang/GCC）：在命令最前插入 `-Ipath`。"
  (let* ((root      (or (and (fboundp 'projectile-project-root)
                             (projectile-project-root))
                        default-directory))
         (json-file (expand-file-name "compile_commands.json" root)))
    (unless (file-exists-p json-file)
      (user-error "❌ 未找到 compile_commands.json，请先运行 C-c i j 生成"))

    ;; 准备 flag 列表：MSVC 用 "/Ipath"，其余用 "-Ipath"
    (let* ((msvc?      (my/compile-commands-msvc-p))
           (flags-list (mapcar (lambda (p)
                                 (if msvc?
                                     (concat "/I" p)
                                   (concat "-I" p)))
                               paths))
           (data       (with-temp-buffer
                         (insert-file-contents json-file)
                         (let ((json-object-type 'alist)
                               (json-array-type  'list))
                           (json-read)))))

      ;; 对每个 entry 进行更新
      (setq data
            (mapcar
             (lambda (entry)
               (cond
                ;; 如果有 arguments 数组
                ((assoc 'arguments entry)
                 (let* ((args-pair (assoc 'arguments entry))
                        (old-args  (cdr args-pair))
                        ;; 去掉旧同名 flag，防止重复
                        (cleaned   (cl-remove-if
                                    (lambda (a)
                                      (or (and msvc? (string-match-p "^/I" a))
                                          (and (not msvc?) (string-match-p "^-I" a))))
                                    old-args))
                        ;; 新 args：compiler + flags + 其余 args
                        (new-args  (append
                                    (list (car old-args)) ; 编译器本身
                                    flags-list
                                    (cdr cleaned))))    ; 剩余参数
                   (setcdr args-pair new-args)
                   entry))

                ;; 否则若有 command 字符串
                ((assoc 'command entry)
                 (let* ((cmd-pair (assoc 'command entry))
                        (old-cmd  (cdr cmd-pair))
                        ;; 去掉旧的 -Ixxx 或 /Ixxx
                        (cleaned  (replace-regexp-in-string
                                   (concat "\\(?:-I\"?[^\"]+\"?\\)"
                                           "\\|\\(?:/I\"?[^\"]+\"?\\)")
                                   ""
                                   old-cmd))
                        ;; 新命令：flags + 空格 + 原命令
                        (new-cmd  (string-join
                                   (append flags-list
                                           (list cleaned))
                                   " ")))
                   (setcdr cmd-pair new-cmd)
                   entry))

                ;; 否则不处理
                (t entry)))
             data))

      ;; 写回文件
      (with-temp-file json-file
        (insert (json-encode data)))
      (message "✅ compile_commands.json 已更新 include：%s"
               (string-join flags-list " "))

      ;; 重启 clangd
      (when (and (bound-and-true-p lsp-mode)
                 (fboundp 'lsp-restart-workspace)
                 (yes-or-no-p "立即重启 clangd (lsp-mode)？"))
        (lsp-restart-workspace)))))

(defun my/interactive-add-include ()
  "交互式输入一个或多个 include 路径，添加到 compile_commands.json。"
  (interactive)
  (let ((paths '())
        path)
    (cl-block nil
      (while t
        (setq path
              (read-directory-name
               "输入 include 路径（回车留空结束）："
               (or (and (fboundp 'projectile-project-root)
                        (projectile-project-root))
                   default-directory)
               nil t))
        (when (string-empty-p path)
          (cl-return))
        (push (expand-file-name path) paths)
        (unless (yes-or-no-p "继续添加更多路径？")
          (cl-return))))
    (if (null paths)
        (message "❗ 未输入任何路径，已取消。")
      (my/update-compile-commands-includes
       (nreverse paths)))))

;; 绑定快捷键 C-c i a
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c i a") #'my/interactive-add-include))





(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))



(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
