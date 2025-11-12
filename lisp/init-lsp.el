;;; init-lsp.el --- Central LSP configuration with lsp-bridge integration

;;; Commentary:
;; 该配置根据用户选择 LSP 后端：
;; - 对于 Emacs Lisp 文件，跳过 LSP；
;;

;;; Code:

;; 定义专用函数，在 lsp-bridge 缓冲区关闭冗余插件
(defun my/disable-lsp-mode-extras-for-bridge ()
  "在启用 lsp-bridge 的缓冲区中关闭 flycheck 与 lsp-ui 提升性能。"
  (when (bound-and-true-p lsp-bridge-mode)
    ;; 关闭 flycheck 检查
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    ;; 关闭 lsp-ui 显示
    (when (bound-and-true-p lsp-ui-mode)
      (lsp-ui-mode -1))
    (message "在 lsp-bridge 模式下禁用了 flycheck 与 lsp-ui.")))

;; 当 lsp-bridge-mode 启动后调用该函数
(add-hook 'lsp-bridge-mode-hook #'my/disable-lsp-mode-extras-for-bridge)

;; ———— Eldoc & Hover 管理 ————

(defun my/lsp-mode-enable-hover ()
  "在 lsp-mode 启用时，打开 hover 支持。"
  (when (fboundp 'lsp-eldoc-mode)
    (lsp-eldoc-mode 1))
  (eldoc-mode 1))

(defun my/lsp-mode-disable-hover ()
  "在切出 lsp-mode 时，关闭 hover 支持。"
  (when (fboundp 'lsp-eldoc-mode)
    (lsp-eldoc-mode -1))
  (eldoc-mode -1)
  (when (fboundp 'lsp--on-idle)
    (cancel-function-timers #'lsp--on-idle))
  (remove-hook 'eldoc-documentation-functions #'lsp-eldoc-function t))


;; ———— 清理函数 ————

(defun my/cleanup-lsp-mode ()
  "彻底停用 lsp-mode：断开、关 workspace、停 timer、停 hover、停 minor-mode。"
  (when (bound-and-true-p lsp-mode)
    (ignore-errors (lsp-disconnect))
    (dolist (ws (ignore-errors (lsp-workspaces))) 
      (ignore-errors (lsp-workspace-shutdown ws)))
    (my/lsp-mode-disable-hover)
    (lsp-mode -1)))

(defun my/cleanup-lsp-bridge ()
  "彻底停用 lsp-bridge 及其进程。"
  (when (fboundp 'lsp-bridge-stop-process)
    (ignore-errors (lsp-bridge-stop-process)))
  (when (bound-and-true-p lsp-bridge-mode)
    (lsp-bridge-mode -1))
  (my/lsp-mode-disable-hover))


;; ———— 主控制函数 ————

(defun my/start-lsp-mode ()
  "启动 lsp-mode（关闭 lsp-bridge）。"
  (interactive)
  (my/cleanup-lsp-bridge)
  (require 'init-lsp-mode)
  (setq lsp-eldoc-enable-hover t)
  (my/lsp-mode-enable-hover)
  (lsp)
  (message "✅ 已启动 lsp-mode（已关闭 lsp-bridge）"))

(defun my/start-lsp-bridge ()
  "启动 lsp-bridge（关闭 lsp-mode）。"
  (interactive)
  (my/cleanup-lsp-mode)
  (require 'init-lsp-bridge)
  (setq lsp-eldoc-enable-hover nil)
  (my/lsp-mode-disable-hover)
  (lsp-bridge-mode 1)
  (message "✅ 已启动 lsp-bridge（已关闭 lsp-mode）"))


;; ———— 快捷键绑定 ————

(global-set-key (kbd "C-c l t m") #'my/start-lsp-mode)
(global-set-key (kbd "C-c l t b") #'my/start-lsp-bridge)

(defun my/show-lsp-status ()
  "显示当前 LSP 后端状态。"
  (interactive)
  (cond
   ((bound-and-true-p lsp-mode)
    (message "📘 当前后端：lsp-mode"))
   ((bound-and-true-p lsp-bridge-mode)
    (message "🔵 当前后端：lsp-bridge"))
   (t
    (message "⚪ 当前无 LSP 后端启用"))))

(global-set-key (kbd "C-c l t s") #'my/show-lsp-status)

(require 'init-cpp)
(require 'init-asm)

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

;; 绑定钩子到 prog 模式
(add-hook 'prog-mode 'my-prog-mode-hook)

(provide 'init-lsp)
;;; init-lsp.el ends here