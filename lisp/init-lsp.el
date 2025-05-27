;;; init-lsp.el --- Language Server Protocol configuration

;;; Commentary:
;; Chose lsp-mode or lsp-bridge

;;; Code:

(defun my/use-lsp-bridge-p ()
  "当文件大于 200KB 时，提示是否使用 lsp-bridge 替代 lsp-mode。"
  (when buffer-file-name
    (let ((size (nth 7 (file-attributes buffer-file-name))))
      (and size
           (> size (* 200 1024))
           (yes-or-no-p (format "文件超过 200KB（%s KB），是否使用 lsp-bridge？" (/ size 1024)))))))

(defun my/setup-lsp ()
  "根据判断加载 lsp-mode 或 lsp-bridge。"
  (if (my/use-lsp-bridge-p)
      (require 'init-lsp-bridge)
    (require 'init-lsp-mode)))

;; 通用 hook，适用于 C/C++、Lua、Python 等
(dolist (hook '(c-mode-common-hook
                lua-mode-hook
                python-mode-hook))
  (add-hook hook #'my/setup-lsp))




(provide 'init-lsp)
;;; init-lsp.el ends here
