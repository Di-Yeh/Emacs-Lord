;;; init-cpp.el --- C/C++ Config
;;; Commentary:
;;; Code:

(use-package clang-format+
	:ensure t)
(use-package clang-format
	:ensure t)

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
        (content "BasedOnStyle: LLVM\nIndentWidth: 4\nBreakBeforeBraces: Allman\nAllowShortFunctionsOnASingleLine: None\nColumnLimit: 0\nSortIncludes: false\n"))
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

(provide 'init-cpp)
