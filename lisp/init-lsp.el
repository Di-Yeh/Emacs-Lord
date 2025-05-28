;;; init-lsp.el --- Central LSP configuration

;;; Commentary:
;; 本配置在打开文件时根据文件大小及模式判断采用哪种 LSP 后端：
;; - 对于 Emacs Lisp 文件，跳过 LSP （避免无谓警告）；
;; - 对于编程文件（derived from prog-mode）：
;;     如果项目检测不到，则以当前文件所在目录作为项目根目录；
;;     如果文件大小小于或等于200KB，则启动 lsp-mode；
;;     如果文件大小超过200KB，则提示用户是否使用 lsp-bridge；
;;       若选择“是”，则 require init-lsp-bridge 并调用 (lsp-bridge-mode)
;;       否则仍启用 lsp-mode。
;; 这样每个缓冲区只会启用一种后端，互不干扰。

;;; Code:

(require 'init-lsp-mode)  ; 假设该文件已配置好 lsp-mode 的相关内容

(defun my/setup-lsp ()
  "初始化 LSP 配置：
- 如果是 Emacs Lisp 文件，则不启动 LSP。
- 如果是编程文件（prog-mode），先尝试判断项目根目录，
  然后按文件大小决定使用 lsp-mode 或 lsp-bridge（大于200KB时提示）。
- 其他文件默认加载 lsp-mode。"
  (unless (or (bound-and-true-p my-lsp-setup-done)
              (minibufferp))
    (setq-local my-lsp-setup-done t)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (message "当前为 Emacs Lisp 文件，跳过 LSP 初始化。"))
     ((derived-mode-p 'prog-mode)
      ;; 切换到项目根目录，如 projectile 检测不到，则以文件所在目录作为根
      (if (and (fboundp 'projectile-project-p)
               (projectile-project-p))
          (let ((proj-root (projectile-project-root)))
            (setq default-directory (file-name-as-directory proj-root))
            (message "切换到项目根目录: %s" default-directory))
        (when buffer-file-name
          (setq default-directory (file-name-directory buffer-file-name))
          (message "未检测到项目，使用文件所在目录: %s" default-directory)))
      ;; 判断文件大小，超过200KB时提示使用 lsp-bridge
      (let ((size (when buffer-file-name
                    (nth 7 (file-attributes buffer-file-name)))))
        (if (and size (> size (* 200 1024)))
            (if (yes-or-no-p (format "该文件 (%d KB) 超过200KB，是否使用 lsp-bridge？ " (/ size 1024)))
                (progn
                  (require 'init-lsp-bridge)
                  (lsp-bridge-mode)
                  (message "大文件，已加载 lsp-bridge。"))
              (progn
                (lsp)
                (message "大文件，但用户选择使用 lsp-mode。")))
          (progn
            (lsp)
            (message "小文件，已加载 lsp-mode。")))))
     (t
      (lsp)
      (message "非编程文件，默认加载 lsp-mode。")))))

(add-hook 'find-file-hook #'my/setup-lsp)


(provide 'init-lsp)
;;; init-lsp.el ends here
