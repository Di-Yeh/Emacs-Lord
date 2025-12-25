;;; init-lsp.el

;;; Commentary:
;;; Code:

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

;; -----------------------------------------------
;; Common Lisp
;; -----------------------------------------------
(use-package sly
  :ensure t
  :init
  ;; 指定你的 Common Lisp 实现，例如使用 SBCL
  (setq inferior-lisp-program "sbcl")
  :config
  ;; 启用 sly-fancy 增强功能
  (sly-setup '(sly-fancy)))

(defun my-auto-start-sly-for-common-lisp ()
  "若当前文件为 Common Lisp（.lisp 或 .lsp），自动启动 SLY，排除 Emacs Lisp。"
  (when (and buffer-file-name
             (string-match-p "\\.lisp\\'\\|\\.lsp\\'" buffer-file-name)
             (not (derived-mode-p 'emacs-lisp-mode)))
    ;; 如果 SLY 尚未激活，则调用 sly 启动连接
    (unless (bound-and-true-p sly-mode)
      (sly))))

;; 在 Lisp 模式下自动检测文件类型并启动 SLY
(add-hook 'lisp-mode-hook #'my-auto-start-sly-for-common-lisp)

(use-package sly-asdf :after sly)
(use-package sly-quicklisp :after sly)
(use-package sly-macrostep :after sly)

(defun my/common-lisp-pair-behavior ()
  "在 SLY / Common Lisp 下也让光标留在括号内。"
  (setq-local electric-pair-inhibit-predicate
              (lambda (char)
                ;; 保留默认判断，但不要因为是 Lisp 就放到括号外
                (electric-pair-default-inhibit char))))

(add-hook 'sly-mode-hook #'my/common-lisp-pair-behavior)
(add-hook 'lisp-mode-hook #'my/common-lisp-pair-behavior)

;; -----------------------------------------------
;; Clojure Lisp
;; -----------------------------------------------
(use-package clojure-mode
  :straight t
	:hook ((clojure-mode . lsp)
         (clojure-mode . flycheck-mode))
  :config
  ;; 让 lsp-mode 的诊断都通过 flycheck 抛出
  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq-local lsp-diagnostics-provider :flycheck))))

(use-package cider
  :straight t
  :hook ((clojure-mode . cider-mode))
  :config
  (setq cider-repl-display-help-banner nil
        cider-allow-jack-in-without-project t
        cider-repl-pop-to-buffer-on-connect t))

(global-set-key (kbd "C-c C-j j") 'cider-jack-in)

(defun my/cider-cleanup-nrepl-port ()
  "在 nREPL 断开后删除项目根目录下的 .nrepl-port 文件（如存在）。"
  (let ((root (locate-dominating-file default-directory ".nrepl-port")))
    (when root
      (let ((port-file (expand-file-name ".nrepl-port" root)))
        (when (file-exists-p port-file)
          (ignore-errors
            (delete-file port-file))
          (message "☠ Deleted .nrepl-port: %s" port-file))))))

(add-hook 'cider-disconnected-hook #'my/cider-cleanup-nrepl-port)

;; -----------------------------------------------
;; Racket Lisp
;; -----------------------------------------------
(use-package racket-mode
  :straight t
  :mode "\\.rkt\\'"
  :hook ((racket-mode . racket-smart-open-bracket-mode))
  :config
  (setq racket-program "racket"))

;; 快捷键：运行、编译、进入 REPL
(global-set-key (kbd "C-^ r r") 'racket-run)
(global-set-key (kbd "C-^ r c") 'racket-compile)
(global-set-key (kbd "C-^ r R") 'racket-repl)

;; 绑定快捷键
(global-set-key (kbd "C-c i a") #'my/add-include-path-to-compile-commands)

;; ———— 自定义 C/Java/C# 缩进风格 ————
(c-add-style "my-prog-style"
             '("bsd"                           ; 基于 BSD 风格
               (c-basic-offset . 4)            ; 4 空格缩进
               (indent-tabs-mode . nil)        ; 用空格而非 TAB
               (tab-width . 4)
               (c-offsets-alist
                (case-label . +)               ; case 多缩进一级
                (inline-open . 0)
                (substatement-open . 0)
                (block-open . 0)
                (arglist-intro . +)
                (arglist-close . 0))))


(defun my-prog-mode-hook ()
  "统一编程风格：C/C++/Java/C# 等。
仅在保存时自动格式化。"
  (c-set-style "my-prog-style")
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq backward-delete-char-untabify-method nil)

  ;; 禁止输入时的自动缩进
  (electric-indent-local-mode -1)

  ;; 在保存时执行 clang-format-buffer（如果存在）
  (when (fboundp 'clang-format-buffer)
    (add-hook 'before-save-hook #'clang-format-buffer nil t)))

;; ———— 各语言挂钩 ————
(add-hook 'c-mode-common-hook #'my-prog-mode-hook)  ; 统一 C/C++/Java/C# 风格
(add-hook 'java-mode-hook #'my-prog-mode-hook)
(add-hook 'csharp-mode-hook #'my-prog-mode-hook)

(provide 'init-lsp)
;;; init-lsp.el ends here