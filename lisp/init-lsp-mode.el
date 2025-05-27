;;; init-lsp-mode.el --- Language Server Protocol configuration

;;; Commentary:
;; Main LSP setup using lsp-mode, lsp-ui, company, flycheck and emacs-lsp-booster

;;; Code:

(require 'init-lsp-booster)

;; 安装并配置 lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (lua-ts-mode . lsp)
         (python-mode . lsp))
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-clients-clangd-args
        '("--header-insertion=never"
          "--background-index"
          "--pch-storage=memory"
          "--clang-tidy=false"
          "--completion-style=detailed"))

  (setq lsp-completion-provider :capf
        lsp-enable-file-watchers nil
        lsp-diagnostic-package :none
        lsp-enable-symbol-highlighting t
        lsp-prefer-flymake nil
        lsp-enable-snippet nil
        lsp-auto-install-server nil)

  (add-hook 'after-save-hook #'flycheck-buffer)

  ;; 自动设定 roblox lua-language-server 路径
  (let* ((base-path (cond
                     ((eq system-type 'windows-nt)
                      "C:/Users/user/AppData/Roaming/.emacs.d/.cache/lsp/lua-roblox-language-server")
                     ((eq system-type 'darwin)
                      "~/.emacs.d/.cache/lsp/lua-roblox-language-server")
                     ((eq system-type 'gnu/linux)
                      "~/.emacs.d/.cache/lsp/lua-roblox-language-server")))
         (binary-path (cond
                       ((eq system-type 'windows-nt)
                        (concat base-path "/extension/server/bin/Windows/lua-language-server.exe"))
                       ((eq system-type 'darwin)
                        (concat base-path "/extension/server/bin/macOS/lua-language-server"))
                       ((eq system-type 'gnu/linux)
                        (concat base-path "/extension/server/bin/Linux/lua-language-server")))))
    (when (file-exists-p binary-path)
      (setq lsp-clients-lua-language-server-bin binary-path
            lsp-clients-lua-language-server-main-location (file-name-directory binary-path)))))

;; UI 增强：lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 1
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 1))

;; 自动补全插件
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-show-quick-access t))

;; 语法检查插件
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; 快捷键绑定
(define-key global-map (kbd "C-c l d") 'lsp-ui-doc-show)
(define-key global-map (kbd "C-c l l") 'lsp-ui-doc-hide)


;; ================= Configuring Emacs as a IDE =================
(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
                                            projectile hydra flycheck company
                                            avy which-key helm-xref))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'helm-xref)
(helm-mode)

(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

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
