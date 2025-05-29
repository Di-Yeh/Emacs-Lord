;;; init-lsp.el --- Central LSP configuration with lsp-bridge integration

;;; Commentary:
;; 该配置根据文件大小及类型智能选择 LSP 后端：
;; - 对于 Emacs Lisp 文件，跳过 LSP；
;; - 对于编程文件（derived from prog-mode）：
;;     如果项目检测不到，则以当前文件所在目录作为项目根目录；
;;     如果文件大小 <= 200KB，则启动 lsp-mode；
;;     如果文件大小 > 200KB，则提示用户是否使用 lsp-bridge：
;;       若选择“是”，则 require 'init-lsp-bridge 并启用 (lsp-bridge-mode)；
;;       否则使用 lsp-mode。
;;
;; 同时，在 lsp-bridge 模式下自动禁用 company、flycheck 以及 lsp-ui，以免资源重复消耗。

;;; Code:

;; 先加载 lsp-mode 相关配置
(require 'init-lsp-mode)

;; 定义专用函数，在 lsp-bridge 缓冲区关闭冗余插件
(defun my/disable-lsp-mode-extras-for-bridge ()
  "在启用 lsp-bridge 的缓冲区中关闭 company、flycheck 与 lsp-ui 提升性能。"
  (when (bound-and-true-p lsp-bridge-mode)
    ;; 关闭冗余的补全框架
    (when (bound-and-true-p company-mode)
      (company-mode -1))
    ;; 关闭 flycheck 检查
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    ;; 关闭 lsp-ui 显示
    (when (bound-and-true-p lsp-ui-mode)
      (lsp-ui-mode -1))
    (message "在 lsp-bridge 模式下禁用了 company、flycheck 与 lsp-ui.")))

;; 当 lsp-bridge-mode 启动后调用该函数
(add-hook 'lsp-bridge-mode-hook #'my/disable-lsp-mode-extras-for-bridge)

(defun my/setup-lsp ()
  "初始化 LSP 配置：
- Emacs Lisp 文件跳过 LSP。
- 对 prog-mode 文件，根据项目检测与文件大小选择后端：
    * 小文件 (<=200KB)：启动 lsp-mode；
    * 大文件 (>200KB)：提示使用 lsp-bridge（或 fallback 为 lsp-mode）。
- 其他类型文件默认加载 lsp-mode。"
  (unless (or (bound-and-true-p my-lsp-setup-done)
              (minibufferp))
    (setq-local my-lsp-setup-done t)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (message "当前为 Emacs Lisp 文件，不启用 LSP。"))
     ((derived-mode-p 'prog-mode)
      ;; 设定默认目录：尝试使用 Projectile 检测项目根目录
      (if (and (fboundp 'projectile-project-p)
               (projectile-project-p))
          (let ((proj-root (projectile-project-root)))
            (setq default-directory (file-name-as-directory proj-root))
            (message "检测到项目，切换到项目根目录: %s" default-directory))
        (when buffer-file-name
          (setq default-directory (file-name-directory buffer-file-name))
          (message "未检测到项目，使用文件所在目录: %s" default-directory)))
      ;; 根据文件大小选择 LSP 后端
      (let ((size (when buffer-file-name
                    (nth 7 (file-attributes buffer-file-name)))))
        (if (and size (> size (* 200 1024)))
            (if (yes-or-no-p (format "该文件 (%d KB) 超过200KB，是否使用 lsp-bridge？ " (/ size 1024)))
                (progn
                  (require 'init-lsp-bridge)
                  (lsp-bridge-mode)
                  (message "大文件，已激活 lsp-bridge 模式。"))
              (progn
                (lsp)
                (message "大文件，但继续使用 lsp-mode。")))
          (progn
            (lsp)
            (message "小文件，已激活 lsp-mode。")))))
     (t
      (lsp)
      (message "非编程文件，默认加载 lsp-mode。")))))

(add-hook 'find-file-hook #'my/setup-lsp)

(provide 'init-lsp)
;;; init-lsp.el ends here
