(setq-default tab-width 2)

(global-set-key (kbd "TAB") 'self-insert-command)

(define-key global-map (kbd "RET") 'default-indent-new-line)
(define-key global-map (kbd "M-j") 'newline-and-indent)

;; Enable Evil
(require 'evil)
(define-key global-map (kbd "C-<") 'evil-mode)

(provide 'init-keymap)
