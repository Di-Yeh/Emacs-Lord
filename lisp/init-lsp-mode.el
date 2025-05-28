;;; init-lsp-mode.el --- LSP configuration via lsp-mode (using plist with lsp-booster)
;;; Commentary:
;; 此配置会在启动时自动安装 lsp-mode 及相关插件，
;; 并在 lsp-mode 初始化前由 lsp-booster 强制把 JSON 解析设为 plist。
;; 为了兼容 lsp-mode 内部可能调用 gethash，我们添加了一个 advice，
;; 如果传入的是 plist（列表）且 lsp-use-plists 为 t，则返回 plist-get 的值。
;;
;; 注意：不再设置 json-object-type 为 hash-table，而是完全依赖 lsp-booster 的设置。
;;; Code:

;; 不要设置 json-object-type 为 hash-table，让 lsp-booster 设置生效
;; (setq json-object-type 'hash-table)  <-- 这行请删除或注释掉

;; 定义需要安装的插件列表
(setq lsp-mode-required-packages
      '(lsp-mode yasnippet lsp-treemacs helm-lsp
        projectile hydra flycheck company
        avy which-key helm-xref dap-mode))
(when (cl-find-if-not #'package-installed-p lsp-mode-required-packages)
  (package-refresh-contents)
  (mapc #'package-install lsp-mode-required-packages))

(require 'init-lsp-booster)  ; 加载 lsp-booster，这会设置 plist 模式

;; LSP 模式配置
(use-package lsp-mode
  :ensure t
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
  ;; 自动设定 roblox lua-language-server 的路径（根据系统类型拼接路径）
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

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 1
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 1))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-show-quick-access t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; helm 相关配置
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

;; --- 关键：使得 lsp-mode 内部对 gethash 的调用支持 plist
(defun my-lsp-gethash-advice (orig-fn key table &optional default)
  "如果 TABLE 是 plist 集合（列表）且 lsp-use-plists 为真，则使用 plist-get；否则调用 ORIG-FN."
  (if (and lsp-use-plists (listp table))
      (or (plist-get table key) default)
    (funcall orig-fn key table default)))
(advice-add 'gethash :around #'my-lsp-gethash-advice)

;; --- 加载 dap-mode 配置（在 lsp-mode 加载后）
(with-eval-after-load 'lsp-mode
  (require 'init-dap))

(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
