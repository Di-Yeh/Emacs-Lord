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
  :straight t)

(use-package cider
  :straight t
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-repl-display-help-banner nil
        cider-allow-jack-in-without-project t
        cider-repl-pop-to-buffer-on-connect t))

(use-package flycheck-clj-kondo
  :straight t
  :after (flycheck clojure-mode))
  
;; ---------------------------------------------
;; 🔒 防止 clojure-mode 触发任何 lsp/eglot
;; ---------------------------------------------
(with-eval-after-load 'clojure-mode
  (setq-local lsp-enabled-clients nil)
  (remove-hook 'clojure-mode-hook #'lsp)
  (remove-hook 'clojure-mode-hook #'lsp-deferred)
  (remove-hook 'clojure-mode-hook #'eglot-ensure))

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

(provide 'init-lisp)
