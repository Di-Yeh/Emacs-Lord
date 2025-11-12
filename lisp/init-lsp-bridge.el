;;; init-lsp-bridge.el --- Minimal & Optimized LSP Bridge Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; 本配置专注于使用 lsp-bridge 内建功能，以实现高效响应和资源占用最小化。除了超快的
;; 代码补全，lsp-bridge 还支持如下特性：
;;  - 实时代码诊断与错误提示（语法检查）
;;  - 内联函数签名及文档预览
;;  - 代码导航（跳转到定义、查找引用等）
;;
;; 注意：因你全局设置了 lsp-mode 和 lsp-bridge互斥，
;; 当其中一个启动时会自动禁用另一个插件——包括这些额外的插件。
;; 本文件在 lsp-bridge 模式下将重新加载 company、company-box 和 yasnippet，
;; 以便在 lsp-bridge 缓冲区中继续使用补全、图标显示和 snippet 模板。
;;
;;; Code:

;; -------------------------
;; 性能调优
;; -------------------------
(setq gc-cons-threshold (* 200 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; -------------------------
;; 加载 lsp-bridge 核心
;; -------------------------
(require 'lsp-bridge)

;; 启用 lsp-bridge 内建诊断功能（用于检查语法错误等）
(setq lsp-bridge-diagnostic-enable t)

;; -------------------------
;; 请求防抖与预编译节流
;; -------------------------
(unless (boundp 'lsp-bridge-request-debounce-time)
  (defvar lsp-bridge-request-debounce-time 0.5
    "Debounce time (in seconds) for sending lsp-bridge requests."))

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

;; -------------------------
;; Python 路径检测
;; -------------------------
(defvar lsp-bridge-python-path-file
  (expand-file-name "lsp-bridge-python-path.txt" user-emacs-directory)
  "保存 lsp-bridge 使用的 Python 可执行文件路径。")

(defun detect-or-set-python-path ()
  "检测或设置 lsp-bridge 所需的 Python 解释器路径。
如果已存在保存的 Python 路径，则优先使用其中内容；否则根据不同操作系统调用
shell 命令获得候选路径列表，供用户选择后保存。"
  (if (file-exists-p lsp-bridge-python-path-file)
      (let ((py-path (with-temp-buffer
                       (insert-file-contents lsp-bridge-python-path-file)
                       (string-trim (buffer-string)))))
        (if (and py-path (file-executable-p py-path))
            (setq lsp-bridge-python-command py-path)
          (message "保存的 Python 路径无效，重新选择...")))
    (let ((paths
           (cond
            ((eq system-type 'windows-nt)
             (split-string (shell-command-to-string "where python") "\n" t))
            ((or (eq system-type 'gnu/linux) (eq system-type 'darwin))
             (let* ((list1 (split-string (shell-command-to-string "which -a python3") "\n" t))
                    (list2 (split-string (shell-command-to-string "which -a python") "\n" t)))
               (delete-dups (append list1 list2))))
            (t nil))))
      (if (and paths (> (length paths) 0))
          (let ((choice (completing-read "选择一个 Python 解释器路径: " paths nil t)))
            (if (and choice (file-executable-p choice))
                (progn
                  (setq lsp-bridge-python-command choice)
                  (with-temp-file lsp-bridge-python-path-file
                    (insert choice)))
              (message "选择的路径无效，请检查！")))
        (message "未找到可用的 Python 解释器，请手动设置！")))))
(detect-or-set-python-path)

;; -------------------------
;; 诊断列表显示（语法错误检查）
;; -------------------------
(global-set-key (kbd "C-c l e") #'lsp-bridge-diagnostic-list)


(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here