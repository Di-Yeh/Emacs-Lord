;;; init-asm.el --- 汇编语言配置 (MASM/NASM) 支持 -*- lexical-binding: t; -*-
;; --------------------------------------------------
;; Emacs 汇编开发环境（适用于 MASM/NASM）
;; 提供语法高亮、补全、snippet、语法检查支持
;; 不使用 LSP，轻量实用
;; --------------------------------------------------

;; 使用 nasm-mode（适用于 MASM/NASM）
(use-package nasm-mode
  :ensure t
  :mode ("\\.\\(asm\\|s\\|S\\)\\'" . nasm-mode))

(setq asm-comment-char ?\#) ; 默认是 `;`，你可以按需设置为 `#` 或 `;`
(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-always-indent 'complete)
            (electric-indent-local-mode -1) ; 防止自动缩进乱跳
            (setq indent-tabs-mode t)
            (setq tab-width 8)))

(provide 'init-asm)