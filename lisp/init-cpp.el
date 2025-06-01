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
  (add-hook 'before-save-hook 'clang-format-buffer nil t)) ;; 保存时自动格式化代码

;; 绑定钩子到 C/C++ 模式
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)

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
        (content "BasedOnStyle: LLVM\nIndentWidth: 4\nBreakBeforeBraces: Allman\nAllowShortFunctionsOnASingleLine: None\nColumnLimit: 100\n"))
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





;; ----------
;; g++编译配置
;; ----------
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++"))

(global-set-key (kbd "<f5>") 'quickrun)




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

(global-set-key (kbd "<f7>") #'my/compile-project)

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

(global-set-key (kbd "<C-f7>") #'my/nmake-build)

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


(global-set-key (kbd "<C-f5>") #'my/run-vs-exe)







(provide 'init-cpp)
