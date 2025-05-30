;;; init-lsp-mode.el --- LSP configuration via lsp-mode

;;; Commentary:
;; 此配置自动安装并初始化 lsp-mode 以及相关插件，
;; 并借助 lsp-booster 确保 JSON 解析采用 plist 方式。
;;; Code:

;; 定义需要安装的插件列表
(setq lsp-mode-required-packages
      '(lsp-mode yasnippet lsp-treemacs helm-lsp
                 projectile hydra flycheck company
                 avy which-key helm-xref))
(when (cl-find-if-not #'package-installed-p lsp-mode-required-packages)
  (package-refresh-contents)
  (mapc #'package-install lsp-mode-required-packages))

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
  ;; 自动设定 Lua 语言服务器路径
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
(define-key global-map (kbd "C-c l d") 'lsp-ui-doc-show)
(define-key global-map (kbd "C-c l l") 'lsp-ui-doc-hide)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-show-quick-access t))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :config
  ;; 如果系统是 Windows，则重载获取桌面名称的函数，以防止 wmctrl 的依赖问题
  (when (eq system-type 'windows-nt)
    (defun company-box--get-desktop-name ()
      "在 Windows 上返回一个虚拟桌面名称，绕过 wmctrl 依赖。"
      "windows-desktop"))
  ;; 设置 company-box 使用 all-the-icons 来显示图标
  (when (require 'all-the-icons nil t)
    (setq company-box-icons-all-the-icons
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :v-adjust -0.2))
            (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :v-adjust -0.2))
            (Method        . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Function      . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Field         . ,(all-the-icons-material "functions"                :height 0.8 :v-adjust -0.2))
            (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :v-adjust -0.2))
            (Class         . ,(all-the-icons-material "class"                    :height 0.8 :v-adjust -0.2))
            (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.2))
            (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :v-adjust -0.2))
            (Property      . ,(all-the-icons-material "settings"                 :height 0.8 :v-adjust -0.2))
            (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :v-adjust -0.2))
            (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :v-adjust -0.2))
            (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :v-adjust -0.2))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :v-adjust -0.2))
            (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :v-adjust -0.2))
            (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :v-adjust -0.2))
            (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :v-adjust -0.2))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :v-adjust -0.2))
            (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :v-adjust -0.2))
            (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :v-adjust -0.2))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :v-adjust -0.2))
            (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :v-adjust -0.2))
            (Event         . ,(all-the-icons-material "event"                    :height 0.8 :v-adjust -0.2))
            (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :v-adjust -0.2))
            (Template      . ,(all-the-icons-material "short_text"               :height 0.8 :v-adjust -0.2))))
    ;; 如果需要，你也可以设置 company-box-backends-colors 为 nil 或其它合适的值
    (setq company-box-backends-colors nil)))




(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
