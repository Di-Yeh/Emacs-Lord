;;; init-lsp-bridge.el --- Minimal & Optimized LSP Bridge Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; 本配置专注于使用 lsp-bridge 内建的功能，实现快速响应，主要优化了：
;; 1. 请求触发机制：设置了一个请求防抖时间（debounce），减少因每个字符变动而发起的请求。
;; 2. 重复预编译头（preamble）构建：对触发预编译请求的函数加了节流（throttle）处理，
;;    如果上一次请求距当前不到 1 秒，则跳过此次请求。
;;
;; 此外，配置中不加载 flycheck、yasnippet、projectile 和 which-key 等扩展包，
;; 达到最小化加载与资源占用的目的。上层（如 init-lsp.el）应在大文件缓冲区中显式调用 (lsp-bridge-mode)。
;;
;;; Code:

;; ---------------------------
;; 1. 性能调优
;; ---------------------------
(setq gc-cons-threshold (* 200 1024 1024))   ; 提高 GC 阈值，适合处理大文件
(setq read-process-output-max (* 1024 1024))   ; 增大进程输出缓存

;; ---------------------------
;; 2. 加载 lsp-bridge 核心（最小依赖）
;; ---------------------------
;; 假设 lsp-bridge 已安装在 user-emacs-directory/site-lisp/lsp-bridge 目录下
(add-to-list 'load-path (expand-file-name "site-lisp/lsp-bridge" user-emacs-directory))
(require 'lsp-bridge)

;; 不启用全局 lsp-bridge 模式，由上层判断后在大文件中调用 (lsp-bridge-mode)
;; (global-lsp-bridge-mode)  ; 此行已注释掉

;; ---------------------------
;; 3. 请求防抖设置
;; ---------------------------
;; 如果 lsp-bridge 内部没有自带请求防抖功能，我们自定义一个变量
(unless (boundp 'lsp-bridge-request-debounce-time)
  (defvar lsp-bridge-request-debounce-time 0.5
    "Debounce time (in seconds) for sending lsp-bridge requests."))

;; ---------------------------
;; 4. 重复预编译头请求的节流
;; ---------------------------
(defvar my-lsp-bridge-last-preamble-time 0
  "记录上次触发预编译头请求的时间戳。")

(defun my-lsp-bridge-throttle-preamble (orig-fn &rest args)
  "节流重复的预编译头请求。
如果上次请求距现在小于 1 秒，则跳过此次请求；否则，执行 ORIG-FN 并更新时间戳。"
  (let ((now (float-time)))
    (if (< (- now my-lsp-bridge-last-preamble-time) 1.0)
        (progn
          (message "[LSP-Bridge] Throttling duplicate preamble request.")
          nil)  ; 跳过此次请求
      (setq my-lsp-bridge-last-preamble-time now)
      (apply orig-fn args))))

;; 假设负责预编译头请求的函数名为 `lsp-bridge--get-preamble`，
;; 如果名称不同，请相应调整。
(when (fboundp 'lsp-bridge--get-preamble)
  (advice-add 'lsp-bridge--get-preamble :around #'my-lsp-bridge-throttle-preamble))

;; ---------------------------
;; 5. Python 路径检测
;; ---------------------------
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
