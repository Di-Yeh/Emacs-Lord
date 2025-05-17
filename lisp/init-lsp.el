(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(add-to-list 'eglot-server-programs '((python-mode) "pylso"))
(add-hook 'python-mode-hook #'eglot-ensure)



(provide 'init-lsp)
