;;; init-lisp.el --- setttings for lisp
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sly
  :ensure t
  :init
  ;; 指定你 Common Lisp 实现的路径，比如这里使用 SBCL
  (setq inferior-lisp-program "sbcl")
  :config
  ;; 默认使用 sly-fancy 提供更完善的扩展功能
  (sly-setup '(sly-fancy)))

(defun my-auto-start-sly-for-common-lisp ()
  "如果当前访问的文件扩展名为 .lisp，并且不是 Emacs Lisp 文件，则自动启动 SLY。
这样可以确保 Common Lisp 文件自动开启 SLY，而不干扰 .el 文件。"
  (when (and buffer-file-name
             (string-match-p "\\.lisp\\'" buffer-file-name)
             (not (derived-mode-p 'emacs-lisp-mode)))
    ;; 如果 SLY 尚未连接，则调用 sly 启动 SLY 环境
    (unless (bound-and-true-p sly-mode)
      (sly))))

;; 将自动启动函数加入 lisp-mode 的 hook 中
(add-hook 'lisp-mode-hook #'my-auto-start-sly-for-common-lisp)

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

(global-set-key (kbd "C-c C-z") 'cider-jack-in)

(use-package paredit
  :straight t
  :hook (clojure-mode . paredit-mode))


(use-package flycheck-clj-kondo
  :ensure t
  :after flycheck
  :hook (clojure-mode . flycheck-clj-kondo-setup))

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


(provide 'init-lisp)
