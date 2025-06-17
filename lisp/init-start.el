;;; init-start.el --- setttings for start

;; 乱码配置
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq gc-cons-threshold most-positive-fixnum)

;; 视窗配置
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; 全局开启
(setq-default truncate-lines t)

;; 禁用备份文件
(setq make-backup-files nil)

;; 启用 delete-selection-mode（选区时按键会作用于选区）
(delete-selection-mode 1)
(defun my/delete-word-backward (arg)
  "删除前一个 word，不保存到 kill-ring。"
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun my/delete-word-forward (arg)
  "删除下一个 word，不保存到 kill-ring。"
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my/delete-line-backward ()
  "删除当前行光标前的所有内容，不保存到 kill-ring。"
  (interactive)
  (delete-region (line-beginning-position) (point)))

(defun my/kill-line-no-save ()
  "从当前光标位置删除到行尾，但不将删除的文本保存到 kill-ring 中。
如果光标位于行尾，则删除该行的换行符。"
  (interactive)
  (if (eolp)
      (when (not (eobp))    ; 如果不是缓冲区最后一行，则删除行尾的换行符
        (delete-char 1))
    (delete-region (point) (line-end-position))))
    

(global-set-key (kbd "C-<backspace>") 'my/delete-word-backward)
(global-set-key (kbd "M-<backspace>") 'my/delete-word-backward)
(global-set-key (kbd "C-S-<backspace>") 'my/delete-line-backward)
;; 绑定 C-k 为我们自定义的不保存删除内容的命令
(global-set-key (kbd "C-k") 'my/kill-line-no-save)



;; 字体
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(default ((t (:family "Cascadia Code" :foundry "outline" :slant normal :weight semi-bold :height 120 :width normal)))))


(use-package emacs
  :ensure t
    :config 
    (setq display-line-numbers-type 'relative) 
    (global-display-line-numbers-mode t)
)

(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(setq-default bidi-paragraph-direction 'left-to-right)

(provide 'init-start)
