;;; init-lsp-bridge.el --- Minimal & Optimized LSP Bridge Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; 本配置专注于使用 lsp-bridge 内建的功能，高效响应以及资源占用最小化。
;; 注意：上层配置（init-lsp.el）会在大文件时调用本配置文件，并激活 lsp-bridge-mode，
;;       同时在 lsp-bridge 缓冲区中关闭 company、flycheck 与 lsp-ui 等插件。
;;; Code:

;; 性能调优：提高 GC 阈值与进程输出缓存
(setq gc-cons-threshold (* 200 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; 加载 lsp-bridge 核心（假定已安装在 user-emacs-directory/site-lisp/lsp-bridge 中）
(add-to-list 'load-path (expand-file-name "site-lisp/lsp-bridge" user-emacs-directory))
(require 'lsp-bridge)

;; 针对常见编程语言自动启动 lsp-bridge
(dolist (hook '(c-mode-hook
                c++-mode-hook
                lua-mode-hook
                python-mode-hook))
  (add-hook hook #'lsp-bridge-mode))

;; 请求防抖设置：仅在 lsp-bridge 内部没有默认设置时才定义
(unless (boundp 'lsp-bridge-request-debounce-time)
  (defvar lsp-bridge-request-debounce-time 0.5
    "Debounce time (in seconds) for sending lsp-bridge requests."))

;; 节流重复预编译头请求：如果上次请求距当前不到1秒则跳过
(defvar my-lsp-bridge-last-preamble-time 0
  "记录上次触发预编译头请求的时间戳。")
(defun my-lsp-bridge-throttle-preamble (orig-fn &rest args)
  "节流重复的预编译头请求。
若上次请求距当前不足 1 秒，则跳过；否则执行 ORIG-FN 。"
  (let ((now (float-time)))
    (if (< (- now my-lsp-bridge-last-preamble-time) 1.0)
        (progn
          (message "[LSP-Bridge] 节流重复的预编译头请求")
          nil)
      (setq my-lsp-bridge-last-preamble-time now)
      (apply orig-fn args))))
(when (fboundp 'lsp-bridge--get-preamble)
  (advice-add 'lsp-bridge--get-preamble :around #'my-lsp-bridge-throttle-preamble))

;; Python 路径检测：支持 Windows、macOS 与 Linux
(defvar lsp-bridge-python-path-file
  (expand-file-name "lsp-bridge-python-path.txt" user-emacs-directory)
  "保存 lsp-bridge 使用的 Python 可执行文件路径。")
(defun detect-or-set-python-path ()
  "检测或设置 lsp-bridge 所需的 Python 解释器路径。"
  (cond
   ((eq system-type 'windows-nt)
    (unless (file-exists-p lsp-bridge-python-path-file)
      (let ((py-path (read-string "请输入 Python 路径（例如：C:/Python311/python.exe）：")))
        (with-temp-file lsp-bridge-python-path-file
          (insert py-path))))
    (let ((py-path (with-temp-buffer
                     (insert-file-contents lsp-bridge-python-path-file)
                     (string-trim (buffer-string)))))
      (when (and py-path (file-executable-p py-path))
        (setq lsp-bridge-python-command py-path))))
   ((or (eq system-type 'gnu/linux)
        (eq system-type 'darwin))
    (let ((default-python (or (executable-find "python3")
                              (executable-find "python"))))
      (when default-python
        (setq lsp-bridge-python-command default-python))))))
(detect-or-set-python-path)

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
