;;; init-lisp.el --- setttings for lisp
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  :hook ((clojure-mode . cider-mode)
				 (clojure-mode . aggressive-indent-mode))
  :config
  (setq cider-repl-display-help-banner nil
        cider-allow-jack-in-without-project t
        cider-repl-pop-to-buffer-on-connect t))

(global-set-key (kbd "C-c C-z") 'cider-jack-in)

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
  :hook ((racket-mode . paredit-mode)
         (racket-mode . aggressive-indent-mode)
         (racket-mode . company-mode)
         (racket-mode . racket-smart-open-bracket-mode))
  :config
  (setq racket-program "racket"))

;; 快捷键：运行、编译、进入 REPL
(global-set-key (kbd "C-^ r r") 'racket-run)
(global-set-key (kbd "C-^ r c") 'racket-compile)
(global-set-key (kbd "C-^ r R") 'racket-repl)

;; -----------------------------------------------
;; Lisp Plugin
;; -----------------------------------------------
(use-package aggressive-indent
  :straight t)

(use-package paredit
  :config
  (message "🧠 Paredit 启用结构化括号编辑"))

(provide 'init-lisp)
