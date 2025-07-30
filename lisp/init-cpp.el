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

;; --------------------------------------------
;; compile_commands.json 配置
;; --------------------------------------------
;; 自动创建 compile_commands.json（支持手动指定 include path）
(defun my/create-compile-commands-json ()
  "在当前目录生成 compile_commands.json 文件，带有基本字段结构，并询问语言类型与标准。"
  (interactive)
  (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
         (json-file (expand-file-name "compile_commands.json" dir))
         (lang (completing-read "选择语言类型: " '("c++" "c") nil t nil nil "c++"))
         (version (read-string (format "输入 %s 标准版本 (默认: %s): " lang (if (string= lang "c++") "c++11" "c11"))
                               nil nil (if (string= lang "c++") "c++11" "c11")))
         (file (or buffer-file-name (read-file-name "选择 source 文件：" dir nil t)))
         (command (format "clang%s -std=%s" (if (string= lang "c++") "++" "") version))
         (entry `(("directory" . ,dir)
                  ("file" . ,file)
                  ("command" . ,command))))
    (if (file-exists-p json-file)
        (when (y-or-n-p "⚠️ compile_commands.json 已存在，是否覆盖？")
          (with-temp-file json-file
            (insert (json-encode (list entry))))
          (message "✅ 已覆盖 compile_commands.json"))
      (with-temp-file json-file
        (insert (json-encode (list entry))))
      (message "✅ 已创建 compile_commands.json"))
    (revert-buffer)))

;; 自动设置 clangd 编译参数（告诉 clangd 去哪里找 compile_commands.json）
(defun my/setup-clangd-from-compile-commands ()
  "若当前目录存在 compile_commands.json，则配置 clangd 使用该文件。"
  (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
         (file (expand-file-name "compile_commands.json" dir)))
    (when (file-exists-p file)
      ;; clangd 会自动查找当前目录，理论上这步可省略，但我们加上更保险
      (setq-local lsp-clients-clangd-args
                  (list (format "--compile-commands-dir=%s" dir))))))

;; 在打开 C/C++ 文件时自动读取 compile_commands.json 设置 clangd 参数
(add-hook 'c-mode-hook #'my/setup-clangd-from-compile-commands)
(add-hook 'c++-mode-hook #'my/setup-clangd-from-compile-commands)

;; 绑定快捷键
(global-set-key (kbd "C-c i c") #'my/create-compile-commands-json)

;; --------------------------------------------
;; compile_commands.json 添加路径
;; --------------------------------------------
;; 为 compile_commands.json 添加 include 路径，并保证 JSON 只有一个 command 字段
(defun my/add-include-path-to-compile-commands ()
  "交互式为 compile_commands.json 添加 -I include 路径到 command，并保留原有 directory/file/command。"
  (interactive)
  (let* ((dir        (file-name-directory (or buffer-file-name default-directory)))
         (json-file  (expand-file-name "compile_commands.json" dir)))
    (unless (file-exists-p json-file)
      (user-error "请先创建 compile_commands.json（使用 C-c i c）"))
    ;; 交互式收集所有 -I 路径
    (let ((paths '())
          (more t))
      (while more
        (let ((p (read-directory-name "选择 include 目录: " dir nil t)))
          (push (concat "-I" (expand-file-name p)) paths))
        (setq more (y-or-n-p "继续添加其他路径？")))
      ;; 读取原有 JSON，并提取必要字段
      (let* ((json-object-type 'alist)
             (json-array-type  'list)
             (json-key-type    'string)
             (data   (json-read-file json-file))
             (old    (car data))
             ;; 用 assoc/cdr 提取
             (dir-val  (cdr (assoc "directory" old)))
             (file-val (cdr (assoc "file"      old)))
             (cmd-val  (cdr (assoc "command"   old)))
             ;; 合成新的 command：原始 + 所有 -I 路径
             (new-cmd  (string-join (append (list cmd-val) (reverse paths)) " ")))
        ;; 重建 entry，只保留三项
        (let ((entry `(("directory" . ,dir-val)
                       ("file"      . ,file-val)
                       ("command"   . ,new-cmd))))
          (with-temp-file json-file
            (insert (json-encode (list entry))))
          (message "✅ 已更新 compile_commands.json 的 command 字段")
          (revert-buffer))))))

;; 绑定快捷键
(global-set-key (kbd "C-c i a") #'my/add-include-path-to-compile-commands)

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
