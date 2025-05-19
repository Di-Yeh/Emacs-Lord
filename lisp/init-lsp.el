;; 安装 lsp-mode 和相关插件
(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (python-mode . lsp))
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")     ;; 可选：设置 LSP 快捷键前缀
  :config
  (setq lsp-enable-symbol-highlighting t
        lsp-prefer-flymake nil))       ;; 使用 flycheck 而不是 flymake（推荐）

;; 可选：lsp-ui 提供更好的 UI（如悬浮文档）
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t       ;; ← 光标悬浮就显示文档
        lsp-ui-doc-position 'at-point       ;; 文档显示在光标处
        lsp-ui-doc-delay 0.3                ;; 延迟更短
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t        ;; ← 行内显示悬浮信息
        lsp-ui-sideline-show-diagnostics t  ;; ← 行内显示诊断
        lsp-ui-sideline-show-code-actions t ;; ← 显示修复建议
        lsp-ui-sideline-delay 0.1))


(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access t))


;; 语法检查器（推荐）
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(define-key global-map (kbd "C-c l d") 'lsp-ui-doc-show)
(define-key global-map (kbd "C-c l l") 'lsp-ui-doc-hide)



(provide 'init-lsp)
