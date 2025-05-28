;;; init-lsp-booster.el --- LSP booster integration

;;; Commentary:
;; 提供 emacs-lsp-booster 支持及 JSON patch，强制将 JSON 解析为 plist 格式。

;;; Code:

(defvar lsp-booster-path (expand-file-name "~/.emacs.d/site-lisp/lsp-booster/lsp-booster.exe")
  "Path to emacs-lsp-booster executable.")

;; 启用 plist 支持
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

(defun lsp-booster--json-object-type-patch ()
  "Force `json-object-type' to be `plist' globally for lsp-mode compatibility."
  (setq json-object-type 'plist
        json-array-type 'list
        json-key-type 'symbol
        json-false nil
        json-null nil))

(add-hook 'lsp-before-initialize-hook #'lsp-booster--json-object-type-patch)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))

(advice-add (if (fboundp 'json-parse-buffer) 'json-parse-buffer 'json-read)
            :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((resolved (executable-find (car orig-result))))
            (setcar orig-result resolved))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(provide 'init-lsp-booster)
;;; init-lsp-booster.el ends here
