(setq-default tab-width 4)
(setq tab-width 4)

(global-set-key (kbd "TAB") 'self-insert-command)

(define-key global-map (kbd "RET") 'default-indent-new-line)
(define-key global-map (kbd "M-j") 'newline-and-indent)

(provide 'init-keymap)
