;;; init-cpp.el --- C/C++ Config
;;; Commentary:
;;; Code:

(use-package clang-format+
	:ensure t)
(use-package clang-format
	:ensure t)

;; 自定义 C/C++ 缩进风格为 my-c-style
(c-add-style "my-c-style"
             '("bsd"                          ;; 基于 BSD 风格（可换成 "k&r", "stroustrup", "java" 等）
               (c-basic-offset . 4)           ;; 设置缩进为 4 空格
               (indent-tabs-mode . nil)       ;; 使用空格代替 TAB
               (tab-width . 4)))              ;; TAB 宽度设为 4

(defun my-c-c++-mode-hook ()
  (c-set-style "my-c-style")                 ;; 使用自定义风格
  (setq indent-tabs-mode nil)                ;; 强制空格缩进
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq backward-delete-char-untabify-method 'nil) ;;退格删除
  (electric-indent-local-mode 1)             ;; 保持回车自动缩进开启
	;; 仅在 clang-format-buffer 存在时才添加 before-save-hook
  (when (fboundp 'clang-format-buffer)
    (add-hook 'before-save-hook 'clang-format-buffer nil t)))

;; 绑定钩子到 C/C++ 模式
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)

(add-hook 'c-mode-common-hook #'clang-format+-mode)

;; -------------------------------
;; 全局哈希表：记录每个目录下打开的 C/C++ 缓冲区数目
;; -------------------------------
(defvar my-cpp-buffer-count (make-hash-table :test 'equal)
  "哈希表，键为目录（字符串），值为当前该目录下打开的 C/C++ 缓冲区数目。")

;; -------------------------------
;; 函数：在指定目录创建 .clang-format 文件
;; -------------------------------
(defun my-create-clang-format-in-dir (dir)
  "在目录 DIR 下创建 .clang-format 文件（如果不存在的话），内容为预定义的格式规则。"
  (let ((file (expand-file-name ".clang-format" dir))
        (content "BasedOnStyle: LLVM\nIndentWidth: 4\nBreakBeforeBraces: Allman\nAllowShortFunctionsOnASingleLine: None\nColumnLimit: 100\nSortIncludes: false\n"))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert content))
      (message "Created .clang-format in %s" dir))))

;; -------------------------------
;; 函数：在指定目录删除 .clang-format 文件
;; -------------------------------
(defun my-delete-clang-format-in-dir (dir)
  "在目录 DIR 下删除 .clang-format 文件（如果存在的话）。"
  (let ((file (expand-file-name ".clang-format" dir)))
    (when (file-exists-p file)
      (delete-file file)
      (message "Deleted .clang-format in %s" dir))))

;; -------------------------------
;; 当打开 C/C++ 文件时调用的函数
;; -------------------------------
(defun my-cpp-buffer-added ()
  "在 C/C++ 缓冲区打开时调用：记录所在目录计数，并在首次打开时创建 .clang-format 文件。"
  (let ((dir (file-truename default-directory)))
    ;; 记录并递增该目录下的缓冲区计数
    (let ((count (gethash dir my-cpp-buffer-count 0)))
      (puthash dir (1+ count) my-cpp-buffer-count))
    ;; 如果是第一个缓冲区，则创建 .clang-format 文件
    (when (= (gethash dir my-cpp-buffer-count) 1)
      (my-create-clang-format-in-dir dir))))

;; -------------------------------
;; 当关闭 C/C++ 缓冲区时调用的函数
;; -------------------------------
(defun my-cpp-buffer-removed ()
  "在 C/C++ 缓冲区关闭时调用：减少所在目录计数，并在最后一个关闭时删除 .clang-format 文件。"
  (when (or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode))
    (let* ((dir (file-truename default-directory))
           (count (gethash dir my-cpp-buffer-count 0)))
      (when (> count 0)
        (puthash dir (1- count) my-cpp-buffer-count)
        (when (<= (gethash dir my-cpp-buffer-count) 0)
          (remhash dir my-cpp-buffer-count)
          (my-delete-clang-format-in-dir dir))))))

;; -------------------------------
;; 在 Emacs 退出时清理所有 .clang-format 文件
;; -------------------------------
(defun my-delete-all-clang-format-on-exit ()
  "退出 Emacs 时删除当前哈希表中所有目录下的 .clang-format 文件。"
  (maphash (lambda (dir _)
             (my-delete-clang-format-in-dir dir))
           my-cpp-buffer-count))

(add-hook 'kill-emacs-hook #'my-delete-all-clang-format-on-exit)

;; -------------------------------
;; 绑定钩子：C/C++ 模式下自动调用以上函数
;; -------------------------------
(add-hook 'c-mode-hook 'my-cpp-buffer-added)
(add-hook 'c++-mode-hook 'my-cpp-buffer-added)
(add-hook 'kill-buffer-hook 'my-cpp-buffer-removed)

(electric-pair-mode 1)


;;; ==================================================
;;; 持久化文件：~/.emacs.d/compile-commands-compiler.el
;;; ==================================================
(defconst my/compile-commands-compiler-file
  (expand-file-name "compile-commands-compiler.el" user-emacs-directory)
  "存放 `my/compile-commands-compiler-alist` 及当前选中模板的文件。")


;;; ==================================================
;;; 全局变量：模板列表和当前模板
;;; ==================================================
(defvar my/compile-commands-compiler-alist nil
  "alist 保存 模板名 → 编译器命令/路径。例如:
  ((\"clang\" . \"clang++\")
   (\"gcc\"   . \"/usr/bin/gcc\")
   (\"msvc\"  . \"C:/…/cl.exe\")
   … )")

(defvar my/compile-commands-compiler-template nil
  "当前选中的编译器模板名，必须是 `my/compile-commands-compiler-alist` 中的 key。")

;;; ==================================================
;;; 启动时：加载或初始化
;;; ==================================================
(if (file-exists-p my/compile-commands-compiler-file)
    ;; 如果文件存在，就 load 它
    (load my/compile-commands-compiler-file)
  ;; 第一次使用时，初始化默认 clang
  (setq my/compile-commands-compiler-alist
        '(("clang" . "clang++")))
  (setq my/compile-commands-compiler-template "clang"))

;;; ==================================================
;;; 交互式函数：选择编译器模板 (C-c i c)
;;; ==================================================
(defun my/switch-compile-commands-compiler ()
  "交互式选择编译器：clang/gcc/msvc/other，并记忆到 `my/compile-commands-compiler-file`。
1. 默认已有 \"clang\" → \"clang++\"。
2. 选 gcc/msvc：首次输入路径，以后复用保存值。
3. 选 other：输入新模板名 & 路径，加入选择列表。"
  (interactive)
  ;; 固定内建选项
  (let* ((builtins '("clang" "gcc" "msvc"))
         ;; 从 alist 中提取已保存的模板名（去掉内建，保留 custom）
         (customs  (seq-remove
                    (lambda (name) (member name builtins))
                    (mapcar #'car my/compile-commands-compiler-alist)))
         ;; 选单 = 内建 + custom + "other"
         (choices  (append builtins customs '("other")))
         (sel      (completing-read
                    "选择编译器模板: " choices nil t))
         name path)
    ;; 如果选择 "other"，先读一个新名字
    (when (string-equal sel "other")
      (setq name (read-string "输入自定义模板名称: "))
      (setq sel name))  ; 用自定义名字作为 sel

    ;; 处理路径：如果 alist 中已有路径且非空，则复用；否则提示读路径
    (let ((existing (cdr (assoc sel my/compile-commands-compiler-alist))))
      (if (and existing (not (string-empty-p existing)))
          (message "📂 模板 [%s] 已配置，路径：%s" sel existing)
        ;; 否则提示输入完整路径或命令
        (setq path
              (read-file-name
               (format "输入 %s 可执行文件/命令: " sel)
               (if (fboundp 'projectile-project-root)
                   (projectile-project-root)
                 default-directory)
               nil t))))

    ;; 如果 path 被设置，则更新 alist
    (when path
      ;; 删除旧条目，再把 (sel . path) 加到 alist 头部
      (setq my/compile-commands-compiler-alist
            (cons (cons sel path)
                  (assq-delete-all sel my/compile-commands-compiler-alist)))
      (message "✅ 模板 [%s] 设置为：%s" sel path))

    ;; 更新当前模板
    (setq my/compile-commands-compiler-template sel)

        ;; 持久化写入同一个文件：注意 alist 前要加 '
    (with-temp-file my/compile-commands-compiler-file
      (insert ";; -*- emacs-lisp -*-\n")
      (insert ";; 自动保存：编译器模板及路径\n")
      ;; 注意这里的 '%S，确保写成 '(("clang" . "clang++") ...)
      (insert (format "(setq my/compile-commands-compiler-alist '%S)\n"
                      my/compile-commands-compiler-alist))
      (insert (format "(setq my/compile-commands-compiler-template %S)\n"
                      my/compile-commands-compiler-template)))
    (message "🔖 已保存到：%s" my/compile-commands-compiler-file)

    ;; 可选重启 clangd，让新模板生效
    (when (and (bound-and-true-p lsp-mode)
               (fboundp 'lsp-restart-workspace)
               (yes-or-no-p "是否立即重启 clangd (lsp-mode)？"))
      (lsp-restart-workspace))))

    ;; 自动设置 Flycheck checker（仅影响当前 buffer）
    (when (derived-mode-p 'c-mode 'c++-mode)
      (let ((checker (pcase name
                       ("clang" 'c/c++-clang)
                       ("gcc"   'c/c++-gcc)
                       ("msvc"  'c/c++-msvc)
                       (_       nil))))
        (if checker
            (progn
              (setq-local flycheck-checker checker)
              ;; 移除不相关的 Checker（避免冲突）
              (setq-local flycheck-disabled-checkers
                          (cl-remove-if (lambda (x) (eq x checker))
                                        '(c/c++-clang c/c++-gcc c/c++-msvc)))
              (message "✅ 当前 buffer Flycheck 检查器已切换为：%s" checker))
          ;; 其他未知编译器，禁用所有默认 Checker
          (setq-local flycheck-disabled-checkers
                      '(c/c++-clang c/c++-gcc c/c++-msvc))
          (message "ℹ️ 当前编译器未知，Flycheck 已禁用本地 Checker，仅保留 LSP 诊断。"))))


;; 绑定 C-c i c
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c i c") #'my/switch-compile-commands-compiler))




;;; ================================
;;; MSVC 判断函数
;;; ================================
(defun my/compile-commands-msvc-p ()
  "如果当前模板对应的编译器是 MSVC 或 clang-cl，则返回 t。"
  (let* ((alist my/compile-commands-compiler-alist)
         (tmpl  my/compile-commands-compiler-template)
         (path  (cdr (assoc tmpl alist))))
    (and path
         (string-match-p "\\(?:\\\\cl\\.exe\\|clang-cl\\)" path))))

;;; ================================
;;; 生成 compile_commands.json (C-c i j)
;;; ================================
(require 'json)
(require 'projectile)

(defun my/generate-compile-commands ()
  "根据当前模板生成 compile_commands.json，并可重启 clangd。
使用 `my/compile-commands-compiler-template` 及 `my/compile-commands-compiler-alist`。"
  (interactive)
  (let* ((root      (or (and (fboundp 'projectile-project-root)
                             (projectile-project-root))
                       default-directory))
         (out       (expand-file-name "compile_commands.json" root))
         (files     (directory-files-recursively
                     root "\\.\\(c\\|cc\\|cpp\\|cxx\\|h\\|hpp\\)$"))
         (is-msvc   (my/compile-commands-msvc-p))
         (compiler  (cdr (assoc my/compile-commands-compiler-template
                                my/compile-commands-compiler-alist)))
         (entries
          (mapcar
           (lambda (file)
             (let ((rel (file-relative-name file root)))
               (if is-msvc
                   ;; MSVC/clang-cl: 用 arguments 数组
                   `(("directory" . ,root)
                     ("arguments" . ,(let ((args (list
                                                  compiler
                                                  (format "/I%s" (expand-file-name "include" root))
                                                  "/nologo"
                                                  "/c" rel
                                                  "/Fo"
                                                  (concat "build\\"
                                                          (file-name-sans-extension
                                                           rel)
                                                          ".obj"))))
                                      args))
                     ("file" . ,file))
                 ;; Clang/GCC: 用 command 字符串
                 `(("directory" . ,root)
                   ("command"   .
                    ,(format "%s -I%s -std=c++17 -c %s -o %s"
                             compiler
                             (expand-file-name "include" root)
                             file
                             (expand-file-name
                              (format "build/%s.o" (file-name-sans-extension
                                                    (file-name-nondirectory file)))
                              root)))
                   ("file" . ,file)))))
           files)))
    ;; 确保有源文件
    (unless files
      (user-error "❌ 未找到任何 C/C++ 源文件"))
    ;; 写入 JSON
    (when (or (not (file-exists-p out))
              (yes-or-no-p (format "覆盖 %s？ " out)))
      (with-temp-file out
        (insert (json-encode entries)))
      (message "✅ %S 生成/更新 %s" compiler out))
    ;; MSVC 情况下注入 --query-driver
    (when is-msvc
      (setq lsp-clients-clangd-args
            (list (format "--query-driver=%s" compiler))))
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



;; 自动在命令结束后 revert 当前 buffer
(defun my/reload-current-buffer ()
  "在不提示确认的情况下，重载当前 buffer，以便让新的 compile_commands.json 或 include 设置生效。"
  (when (buffer-file-name)
    (revert-buffer :ignore-auto :noconfirm)))

;; 1. 在切换模板后调用
(with-eval-after-load 'lsp-mode
  (advice-add 'my/switch-compile-commands-compiler :after
              (lambda (&rest _) (my/reload-current-buffer))))

;; 2. 在生成 compile_commands.json 后调用
(advice-add 'my/generate-compile-commands :after
            (lambda (&rest _) (my/reload-current-buffer)))

;; 3. 在添加 include 路径后调用
(advice-add 'my/update-compile-commands-includes :after
            (lambda (&rest _) (my/reload-current-buffer)))



;; CMake
;; --------------------------------------------
;; 配置 cmake-mode：为 CMakeLists.txt 和 .cmake 文件提供语法高亮和缩进
;; --------------------------------------------
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (message "cmake-mode 加载成功！"))

;; --------------------------------------------
;; 配置 cmake-ide：自动调用 CMake 生成编译配置
;; 注意：如果你已经利用 CMake 导出 compile_commands.json 配置了 lsp-mode，
;; 那么 cmake-ide 的作用就可以有限；但如果你希望让 Emacs 自动调用 CMake，
;; 下面的配置仍然是有用的。
;; --------------------------------------------
(use-package cmake-ide
  :ensure t
  :config
  ;; 指定构建目录，确保在该目录中生成编译文件（如 compile_commands.json）
  (setq cmake-ide-build-dir "build")
  (cmake-ide-setup)
  (message "cmake-ide 配置完成"))

;; ------------------------
;; CMake 项目构建
;; ------------------------

(defun my/create-cmake-project ()
  "创建一个新的 CMake 项目。依次询问项目目录名称和源文件名称，
在指定目录下创建项目目录、源文件以及 CMakeLists.txt，并自动打开 CMakeLists.txt 供编辑。
默认的 CMakeLists.txt 模板包含最低版本要求、项目名称设置（默认为项目目录名称）
以及一个添加可执行文件的命令。"
  (interactive)
  ;; 这里不设默认值，直接使用当前 buffer 所在的目录作为起始目录
  (let* ((base-dir (read-directory-name "请选择项目存放的父级目录: " default-directory nil nil))
         (dir (read-string "请输入项目目录名称: "))
         (src-file (read-string "请输入源文件名称 (例如 main.cpp): "))
         (project-name (if (string= dir "") "MyProject" dir))
         (project-dir (expand-file-name dir base-dir))
         (src-path (expand-file-name src-file project-dir))
         (cmakelists-path (expand-file-name "CMakeLists.txt" project-dir)))
    ;; 创建项目目录（如果不存在则递归创建）
    (unless (file-exists-p project-dir)
      (make-directory project-dir t))
    ;; 创建源文件（如果不存在，就创建并写入简单的注释模板）
    (unless (file-exists-p src-path)
      (with-temp-buffer
        (insert "// " src-file " - 源文件\n")
        (insert "// 这里编写项目 " project-name " 的代码\n")
        (write-region (point-min) (point-max) src-path)))
    ;; 创建 CMakeLists.txt 模板
    (with-temp-buffer
      (insert "cmake_minimum_required(VERSION 3.10)\n")
      (insert (format "project(%s)\n\n" project-name))
      (insert "# 设置 C++ 标准为 C++11\n")
      (insert "set(CMAKE_CXX_STANDARD 11)\n")
      (insert "set(CMAKE_CXX_STANDARD_REQUIRED ON)\n\n")
      (insert (format "# 添加可执行文件 %s，其源文件为 %s\n" project-name src-file))
      (insert (format "add_executable(%s %s)\n" project-name src-file))
      (write-region (point-min) (point-max) cmakelists-path))
    ;; 打开 CMakeLists.txt 供编辑
    (find-file cmakelists-path)
    (message "项目 '%s' 已创建于 %s" project-name project-dir)))


(defun my/cmake-configure-and-build ()
  "从当前目录中选择一个目录作为 CMake 的构建目录 (-B)，
接着询问是否添加额外参数（例如 -DCMAKE_BUILD_TYPE=Debug），
最后确认后执行 CMake 配置，并提供错误处理功能。
配置成功后，还可选择立即开始编译构建。"
  (interactive)
  (let* ((build-dir (read-directory-name "请选择构建目录 (-B 参数)： " default-directory nil nil))
         (extra-params (read-string "请输入额外的 CMake 参数（例如 -DCMAKE_BUILD_TYPE=Debug，可留空）： "))
         (configure-cmd (format "cmake -S . -B \"%s\" %s" build-dir extra-params)))
    (if (not (yes-or-no-p (format "确定要执行以下命令吗？\n%s\n" configure-cmd)))
        (message "已取消配置。")
      (condition-case err
          (progn
            (message "执行命令：%s" configure-cmd)
            (shell-command configure-cmd)
            ;; 配置完成后判断是否生成了 CMakeCache.txt（成功配置后该文件必然生成）
            (if (not (file-exists-p (expand-file-name "CMakeCache.txt" build-dir)))
                (error "CMake 配置失败，请检查输出信息！")
              (message "CMake 配置成功。"))
            (when (yes-or-no-p "配置完成，是否立即开始编译构建？")
              (let ((build-cmd (format "cmake --build \"%s\"" build-dir)))
                (message "开始构建：%s" build-cmd)
                (compile build-cmd))))
        (error (message "错误发生：%s" (error-message-string err)))))))




;; ───────────────────────────────────────────────────────
;; Visual Studio 项目支持：devenv / nmake 构建 & 运行
;; F7 编译，F5 运行（sln 工程）
;; C-F7 编译 Makefile 工程
;; 自动判断 .sln 使用 devenv.com，否则尝试 Makefile 使用 nmake
;; 记得使用 Cross Tools Command Prompt 打开Emacs
;; ───────────────────────────────────────────────────────

(defun my/get-vs-build-options ()
  "交互式选择 Visual Studio 构建配置和平台。默认 Debug | x86。"
  (let ((config (completing-read "配置: " '("Debug" "Release") nil t "Debug"))
        (plat (completing-read "平台: " '("x86" "x64") nil t "x86")))
    (list config plat)))

(defun my/compile-project ()
  "尝试自动查找 .sln 或 Makefile，并调用 devenv.com 或 nmake 进行构建。"
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      (lambda (f)
                                        (or (directory-files f nil "\\.sln$")
                                            (file-exists-p (expand-file-name "Makefile" f))))))
         (sln-file (car (directory-files dir t "\\.sln$")))
         (makefile (expand-file-name "Makefile" dir)))
    (cond
     ;; 使用 devenv.com 编译 .sln
     ((and sln-file (file-exists-p sln-file))
      (let* ((opts (my/get-vs-build-options))
             (conf (nth 0 opts))
             (plat (nth 1 opts)))
        ;; 使用 devenv.com 可确保输出完整信息返回到 shell
        (compile (format "devenv.com \"%s\" /build \"%s|%s\"" sln-file conf plat))))
     ;; 使用 nmake 编译 Makefile
     ((file-exists-p makefile)
      (compile (format "nmake /f \"%s\"" makefile)))
     ;; 无法识别
     (t
      (message "当前目录未找到 .sln 或 Makefile，无法编译。")))))



;; ───────────────────────────────
;; C-F7：强制使用 nmake 构建
;; ───────────────────────────────

(defun my/nmake-build ()
  "始终使用 nmake 编译当前目录下的 Makefile。"
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      (lambda (f) (file-exists-p (expand-file-name "Makefile" f)))))
         (makefile (expand-file-name "Makefile" dir)))
    (if (file-exists-p makefile)
        (compile (format "nmake /f \"%s\"" makefile))
      (message "未找到 Makefile"))))


;; ───────────────────────────────
;; F5：运行构建后的可执行文件（仅限 .sln 工程）
;; 假设输出路径为 $(Platform)/$(Configuration)/项目名.exe
;; ───────────────────────────────
(defun my/run-vs-exe ()
  "运行 Visual Studio 项目生成的 exe 文件（根据 sln 工程名自动推断）。
当选择 x86 时，输出路径设为 /Debug/ProjectName.exe 或 /Release/ProjectName.exe；
x64 输出路径保持为 x64/Debug 或 x64/Release。"
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      (lambda (f)
                                        (directory-files f nil "\\.sln$"))))
         (sln-file (car (directory-files dir t "\\.sln$"))))
    (if sln-file
        (let* ((opts (my/get-vs-build-options))
               (conf (nth 0 opts))
               (plat (nth 1 opts))
               ;; 假设项目名与 sln 文件名一致
               (exe-name (file-name-base sln-file))
               ;; 对于 x86，仅使用配置名作为目录；对于 x64，使用 "x64/配置" 目录
               (out-dir (if (string-equal plat "x86")
                            (expand-file-name conf dir)
                          (expand-file-name (format "%s/%s" plat conf) dir)))
               (exe-path (expand-file-name (concat exe-name ".exe") out-dir)))
          (if (file-exists-p exe-path)
              (progn
                (message "运行可执行文件: %s" exe-path)
                (start-process "run-vs-exe" "*run*" exe-path))
            (message "可执行文件不存在: %s" exe-path)))
      (message "当前目录未找到 .sln 工程"))))










(provide 'init-cpp)
