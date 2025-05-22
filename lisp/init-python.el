;; 添加 Python的 quickrun 命令
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "python"
    '((:command . "python")  ;; 使用 Python
      (:exec    . ("%c %s"))
      (:description . "Run python script"))
    :default "python"))

(global-set-key (kbd "C-S-p") 'quickrun)  ;; Python


(provide 'init-python)
