(add-to-list 'load-path "/path/to/lsp-pascal")
(require 'lsp-pascal)

(add-hook 'pascal-mode-hook #'lsp)
(add-hook 'opascal-mode-hook #'lsp)

;; set this in case the language server is not available in PATH
(setq lsp-pascal-command "/path/to/pasls")


(provide 'init-pascal)
