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



;; lua-mode
(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :hook ((lua-mode . lsp-deferred)
         (lua-mode . flycheck-mode)))

(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "lua"
    '((:command . "lua")  ;; 使用 Python
      (:exec    . ("%c %s"))
      (:description . "Run lua script"))
    :default "lua"))

(global-set-key (kbd "C-S-l") 'quickrun)



;; ----------
;; g++编译配置
;; ----------
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++"))





(provide 'init-quickrun)
