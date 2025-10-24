;;; init-lsp-mode.el --- LSP configuration via lsp-mode

;;; Commentary:
;; 此配置自动安装并初始化 lsp-mode 以及相关插件，
;; 并借助 lsp-booster 确保 JSON 解析采用 plist 方式。
;;; Code:

;; 定义需要安装的插件列表
(setq lsp-mode-required-packages
      '(lsp-mode yasnippet lsp-treemacs helm-lsp
                 projectile hydra flycheck company
                 avy which-key helm-xref dap-mode))
(when (cl-find-if-not #'package-installed-p lsp-mode-required-packages)
  (package-refresh-contents)
  (mapc #'package-install lsp-mode-required-packages))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  ;; 设置 LSP 快捷键前缀，例如：C-c l r 重命名，C-c l d 跳转定义等
  (setq lsp-keymap-prefix "C-c l")

  :config
  ;; 通用 LSP 设置
  (setq lsp-completion-provider :capf                    ; 使用 Emacs 的 Capf 补全
        lsp-enable-symbol-highlighting t                 ; 高亮变量名
        lsp-diagnostic-package :none                     ; 不用内建诊断系统（你可能用 flycheck）
        lsp-completion-enable-additional-text-edit nil   ; 禁用附加的编辑动作
        lsp-prefer-flymake nil                           ; 使用 flycheck 而不是 flymake
        lsp-auto-install-server nil))                    ; 不自动下载语言服务器


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 1.5
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 1
        lsp-ui-peek-enable t
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        lsp-ui-peek-fontify 'on-demand
        lsp-ui-imenu-auto-refresh t
        lsp-ui-imenu-kind-position 'top))
(define-key global-map (kbd "C-c l s") 'lsp-ui-doc-show)
(define-key global-map (kbd "C-c l l") 'lsp-ui-doc-hide)


(use-package lsp-treemacs
  :ensure t
  :after lsp  ;; 确保 lsp-mode 已经加载
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols)
  :config
  ;; 开启 project 和 LSP workspace 双向同步（可选）
  (lsp-treemacs-sync-mode 1))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)   ; 补全注解右对齐
  (company-minimum-prefix-length 1)       ; 输入 1 个字符就开始补全
  (company-idle-delay 0.1)                ; 补全延迟时间
  (company-show-numbers t)                ; 显示候选项编号
  (company-tooltip-limit 10)              ; 显示最多 10 个候选项
  (company-require-match nil)             ; 不强制匹配
  (company-dabbrev-downcase nil)          ; 保留大小写
  (company-global-modes '(not eshell-mode shell-mode)) ; 不在 shell 中启用
  :config
  ;; 默认使用 Emacs 字体缩放同步调整 popup
  (setq company-tooltip-minimum-width 40) ;; 保持宽度统一
  ;; 更优雅的样式颜色
  (custom-set-faces
   '(company-tooltip ((t (:inherit default :background "#282c34" :foreground "#bbc2cf"))))
   '(company-tooltip-selection ((t (:background "#3e4451" :foreground "#ffffff"))))
   '(company-tooltip-common ((t (:foreground "#c678dd" :weight bold))))
   '(company-tooltip-annotation ((t (:foreground "#56b6c2"))))
   '(company-scrollbar-bg ((t (:background "#3e4451"))))
   '(company-scrollbar-fg ((t (:background "#61afef"))))
   '(company-tooltip-search ((t (:foreground "#98be65" :weight bold))))
   '(company-preview ((t (:background "#1c1f24" :foreground "#5B6268"))))
   '(company-preview-common ((t (:inherit company-preview :foreground "#c678dd"))))))

(use-package kind-icon
  :straight t
  :after company
  :custom
  (kind-icon-default-face 'corfu-default) ;; 跟 corfu 同样的图标样式
  :config
  (add-to-list 'company-format-margin-function #'kind-icon-margin-formatter))

;; 让 Emacs completion-at-point 全局不分大小写
(setq completion-ignore-case               t
      read-buffer-completion-ignore-case  t
      read-file-name-completion-ignore-case t)

;; 用 flex 做模糊匹配，basic 保留原生前缀匹配
(setq completion-styles '(flex basic))

;; --------------------------------------------------
;; Flycheck 配置（全局启用，保存时检查）
;; --------------------------------------------------
;; Flycheck 语法检查配置，配合 lsp-mode 使用 clangd 做诊断
(use-package flycheck
  :ensure t
  :init
  ;; 全局启用 Flycheck
  (global-flycheck-mode)

  :config
  ;; 检查时机：只在保存或启用 mode 时检查，不在编辑时实时检查
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(json-python-json))
  (flycheck-add-mode 'json-jsonlint 'json-mode))

(require 'helm-xref)
(helm-mode)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)


(which-key-mode)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)



(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))



(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
