;;; init-lsp-bridge.el --- LSP Bridge Configuration -*- lexical-binding: t; -*-

;; ================= Configuring Emacs as a IDE =================
(setq package-selected-packages '(yasnippet projectile hydra flycheck company avy which-key helm-xref))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

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
      company-minimum-prefix-length 1)

;; ================= LSP Bridge Configuration =================

(add-to-list 'load-path (expand-file-name "site-lisp/lsp-bridge" user-emacs-directory))
(require 'lsp-bridge)

;; 使用 yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; 启用 lsp-bridge-mode 自动补全等功能
(global-lsp-bridge-mode)

;; 各语言支持
(add-hook 'c-mode-hook #'lsp-bridge-mode)
(add-hook 'c++-mode-hook #'lsp-bridge-mode)
(add-hook 'lua-mode-hook #'lsp-bridge-mode)
(add-hook 'python-mode-hook #'lsp-bridge-mode)

;; ================= 自动检测 Python 环境 =================
(defvar lsp-bridge-python-path-file (expand-file-name "lsp-bridge-python-path.txt" user-emacs-directory)
  "保存用户指定的 Python 路径")

(defun detect-or-set-python-path ()
  (cond
   ((eq system-type 'windows-nt)
    (unless (file-exists-p lsp-bridge-python-path-file)
      (let ((py-path (read-string "请输入你已安装的 Python 路径（例如：C:/Python311/python.exe）: ")))
        (with-temp-file lsp-bridge-python-path-file
          (insert py-path))))
    (let ((py-path (with-temp-buffer
                     (insert-file-contents lsp-bridge-python-path-file)
                     (string-trim (buffer-string)))))
      (when (file-executable-p py-path)
        (setq lsp-bridge-python-command py-path))))

   ((or (eq system-type 'gnu/linux)
        (eq system-type 'darwin))
    ;; Linux / macOS 默认使用 python3
    (let ((default-python (or (executable-find "python3")
                              (executable-find "python"))))
      (when default-python
        (setq lsp-bridge-python-command default-python))))))

(detect-or-set-python-path)

;; ================= Python EPC 模块检测 =================
;; (unless (executable-find "python")
;;   (warn "⚠ Python 未安装或未加入 PATH，请检查 Python 安装！"))

;; (let ((check-epc (shell-command-to-string "python -c \"import epc\" 2>&1")))
;;   (when (string-match "ModuleNotFoundError" check-epc)
;;     (warn "⚠ Python 缺少 epc 模块，请运行: pip install epc orjson sexpdata six paramiko")))

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
