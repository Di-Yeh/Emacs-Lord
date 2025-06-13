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
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))
 

;; --------------------------------------------------
;; 添加 include 路径到 .dir-locals.el（支持合并多个路径）
;; 快捷键：C-c i a
;; --------------------------------------------------
(require 'projectile)
(require 'cl-lib)

(defun my/add-clang-include-to-dir-locals ()
  "将 include 路径添加到项目的 .dir-locals.el 中的 flycheck-clang-include-path。
支持合并多个 include 路径，自动避免重复。"
  (interactive)
  (let* ((include-dir (read-directory-name "选择要添加的 include 路径: "))
         (escaped-include (replace-regexp-in-string "\\\\" "/" include-dir)) ;; 替换 Windows 反斜杠
         (project-root (or (projectile-project-root) default-directory))
         (save-dir (read-directory-name "选择 .dir-locals.el 保存路径: " project-root))
         (dir-locals-file (expand-file-name ".dir-locals.el" save-dir))
         (new-entry `((flycheck-clang-include-path . ("$new$")))) ;; 临时模板
         (new-path escaped-include)
         updated-alist)

    ;; 读取已有 .dir-locals.el 内容
    (setq updated-alist
          (if (file-exists-p dir-locals-file)
              (with-temp-buffer
                (insert-file-contents dir-locals-file)
                (read (current-buffer)))
            '()))

    ;; 更新 c-mode 和 c++-mode 条目
    (dolist (mode '(c-mode c++-mode))
      (let* ((mode-entry (assoc mode updated-alist))
             (settings (copy-tree (cdr mode-entry)))
             (includes (cdr (assoc 'flycheck-clang-include-path settings))))
        (unless (member new-path includes)
          (setq includes (append includes (list new-path))))
        (setq settings (assq-delete-all 'flycheck-clang-include-path settings))
        (push `(flycheck-clang-include-path . ,includes) settings)
        (setq updated-alist (assq-delete-all mode updated-alist))
        (push `(,mode . ,settings) updated-alist)))

    ;; 保存新的 .dir-locals.el
    (with-temp-file dir-locals-file
      (insert ";; 自动生成的 .dir-locals.el - 包含 Flycheck Clang include 路径配置\n")
      (pp updated-alist (current-buffer))
      (message "已添加 include 路径到：%s" dir-locals-file))))

;; 设置快捷键 C-c i a 来调用该功能
(global-set-key (kbd "C-c i a") #'my/add-clang-include-to-dir-locals)


;; --------------------------------------------------
;; 临时添加 include 路径到当前会话（不会写入文件）
;; 快捷键：C-c i p
;; --------------------------------------------------
(require 'cl-lib)

(defun my/add-clang-include-path-session ()
  "临时将 include 路径添加到 `flycheck-clang-include-path`。
仅在当前 Emacs 会话中有效，不会写入 .dir-locals.el 文件。
支持重复使用并自动合并路径。"
  (interactive)
  (let* ((include-dir (read-directory-name "选择要添加的 include 路径: "))
         (escaped-include (replace-regexp-in-string "\\\\" "/" include-dir))
         (current-paths (or flycheck-clang-include-path '())))
    (if (member escaped-include current-paths)
        (message "路径已存在，无需重复添加。")
      (setq flycheck-clang-include-path
            (append current-paths (list escaped-include)))
      (message "已添加路径: %s" escaped-include))))

;; 快捷键绑定：C-c i p 添加临时路径
(global-set-key (kbd "C-c i p") #'my/add-clang-include-path-session)



;; --------------------------------------------------
;; 修复后的 Clangd 编译器兼容路径注入功能
;; 修复问题：默认路径设为 ~/.emacs.d/.emacs-clang-drivers.el
;; 防止未定义变量 lsp-clients-clangd-args 报错
;; --------------------------------------------------

;; 确保变量已定义（避免初期加载时报 void 错）
(defvar lsp-clients-clangd-args nil
  "Arguments to pass to clangd.")

;; 保存文件路径移至 ~/.emacs.d
(defvar my/clangd-driver-file (expand-file-name ".emacs-clang-drivers.el" user-emacs-directory)
  "保存用户指定 clangd driver 路径的文件。")

(defun my/read-clangd-drivers ()
  "从文件读取 clangd driver 路径列表。"
  (when (file-readable-p my/clangd-driver-file)
    (with-temp-buffer
      (insert-file-contents my/clangd-driver-file)
      (condition-case nil
          (read (current-buffer))
        (error nil)))))

(defun my/save-clangd-drivers (driver-list)
  "将 DRIVER-LIST 保存到配置文件。"
  (with-temp-file my/clangd-driver-file
    (prin1 driver-list (current-buffer))))

(defun my/select-compiler-path (compiler-name default-path)
  "交互式输入编译器路径，显示默认路径。"
  (read-file-name (format "输入 %s 路径（当前路径：%s）: " compiler-name default-path)
                  (file-name-directory default-path)
                  default-path t))

(defun my/select-clangd-drivers ()
  "选择使用的编译器并保存到配置文件。"
  (interactive)
  (let* ((choice (completing-read "选择编译器: " '("GCC" "MSVC" "其它") nil t))
         (drivers
          (cond
           ((string= choice "GCC")
            (list (my/select-compiler-path "GCC" default-directory)))
           ((string= choice "MSVC")
            (list (my/select-compiler-path "MSVC" default-directory)))
           ((string= choice "其它")
            (let* ((name (read-string "请输入编译器名称: "))
                   (path (my/select-compiler-path name default-directory)))
              (list path))))))
    (if (and (file-exists-p my/clangd-driver-file)
             (yes-or-no-p "检测到已有驱动路径，是否合并保存？"))
        (let* ((existing (my/read-clangd-drivers))
               (merged (delete-dups (append existing drivers))))
          (my/save-clangd-drivers merged)
          (message "已合并保存 driver 路径。"))
      (my/save-clangd-drivers drivers)
      (message "已保存 driver 路径。"))

    ;; 更新 lsp-clients-clangd-args
    (setq lsp-clients-clangd-args
          (cl-remove-if (lambda (s) (string-prefix-p "--query-driver" s))
                        lsp-clients-clangd-args))
    (add-to-list 'lsp-clients-clangd-args
                 (concat "--query-driver=" (string-join (my/read-clangd-drivers) ",")))
    (message "已更新 lsp-clients-clangd-args。")

    (when (yes-or-no-p "是否立即重启 LSP 会话以应用新设置？")
      (lsp-restart-workspace))))

;; 启动时自动注入已保存的驱动器路径（前提是文件存在且内容合法）
(let ((saved-drivers (my/read-clangd-drivers)))
  (when (and saved-drivers
             (not (cl-some (lambda (s) (string-prefix-p "--query-driver" s))
                           lsp-clients-clangd-args)))
    (add-to-list 'lsp-clients-clangd-args
                 (concat "--query-driver=" (string-join saved-drivers ",")))))

;; 快捷键绑定
(global-set-key (kbd "C-c i c") #'my/select-clangd-drivers)



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



;; 自动生成 compile_commands.json 的辅助工具 -*- lexical-binding: t; -*-
(defun my/find-project-root ()
  "尝试查找当前 buffer 所在的项目根目录。"
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (locate-dominating-file default-directory "compile_commands.json")
      default-directory))

(defun my/find-source-file ()
  "查找 main.c 或 main.cpp 文件，如果不存在则询问用户。"
  (let* ((project-root (my/find-project-root))
         (main-c (expand-file-name "main.c" project-root))
         (main-cpp (expand-file-name "main.cpp" project-root)))
    (cond
     ((file-exists-p main-c) "main.c")
     ((file-exists-p main-cpp) "main.cpp")
     (t (read-string "未找到 main.c 或 main.cpp，请输入源文件名: ")))))

(defun my/select-cpp-standard ()
  "让用户选择 C++ 标准版本。"
  (completing-read "请选择 C++ 标准: " '("c++11" "c++14" "c++17" "c++20") nil t nil nil "c++17"))

(defun my/gather-include-dirs ()
  "尝试收集项目中可用的 include/src/lib 路径，并允许用户添加更多路径。"
  (let* ((project-root (my/find-project-root))
         (default-dirs '("include" "src" "lib"))
         (collected (seq-filter (lambda (dir)
                                   (file-directory-p (expand-file-name dir project-root)))
                                 default-dirs))
         (more t))
    (while (and (yes-or-no-p "是否要添加额外的 include/src/lib 路径？")
                more)
      (let ((custom (read-directory-name "请输入要添加的路径: " project-root)))
        (push (file-relative-name custom project-root) collected)
        (setq more (yes-or-no-p "还要添加其它路径吗？"))))
    collected))

(defun my/generate-compile-commands-json (output-path)
  "创建 compile_commands.json 文件。"
  (let* ((source-file (my/find-source-file))
         (standard (my/select-cpp-standard))
         (include-dirs (my/gather-include-dirs))
         (include-flags (mapconcat (lambda (d) (concat "-I" d)) include-dirs " "))
         (command (format "g++ -std=%s %s -c %s" standard include-flags source-file))
         (json-string (format "[{\n  \"directory\": \"./\",\n  \"command\": \"%s\",\n  \"file\": \"%s\"\n}]" command source-file)))
    (with-temp-file output-path
      (insert json-string))
    (message "✅ compile_commands.json 模板已创建于: %s" output-path)))

(defun my/check-or-create-compile-commands ()
  "检查 compile_commands.json 是否存在，否则询问是否指定或创建。"
  (interactive)
  (let* ((project-root (my/find-project-root))
         (json-path (expand-file-name "compile_commands.json" project-root)))
    (if (file-exists-p json-path)
        (message "✅ compile_commands.json 已存在: %s" json-path)
      (if (yes-or-no-p "❓ 未发现 compile_commands.json，是否已有该文件？")
          (let ((user-path (read-file-name "请输入 compile_commands.json 路径: " project-root)))
            (when (file-exists-p user-path)
              (copy-file user-path json-path t)
              (message "✅ 已复制 compile_commands.json 到项目目录。")))
        (when (yes-or-no-p "是否要创建一个 compile_commands.json 模板？")
          (my/generate-compile-commands-json json-path))))))

;; 快捷键绑定
(global-set-key (kbd "C-c i j") #'my/check-or-create-compile-commands)




(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))



(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
