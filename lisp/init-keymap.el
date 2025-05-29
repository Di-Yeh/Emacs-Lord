(setq-default tab-width 2)

(defun my/scroll-window-down ()
  "模拟鼠标往下滚动视窗：向下显示一行内容，即文本上移一行。"
  (interactive)
  (if (fboundp 'scroll-up-line)
      (scroll-up-line 1)
    ;; 如果你的 Emacs 版本没有 scroll-up-line，则用 scroll-up
    (scroll-up 1)))

(defun my/scroll-window-up ()
  "模拟鼠标往上滚动视窗：向上显示一行内容，即文本下移一行。"
  (interactive)
  (if (fboundp 'scroll-down-line)
      (scroll-down-line 1)
    ;; 如果你的 Emacs 版本没有 scroll-down-line，则用 scroll-down
    (scroll-down 1)))

;; 全局绑定示例：
(global-set-key (kbd "M-n") 'my/scroll-window-down)
(global-set-key (kbd "M-p") 'my/scroll-window-up)


(global-set-key (kbd "TAB") 'self-insert-command)

(define-key global-map (kbd "RET") 'default-indent-new-line)
(define-key global-map (kbd "M-j") 'newline-and-indent)


(provide 'init-keymap)
