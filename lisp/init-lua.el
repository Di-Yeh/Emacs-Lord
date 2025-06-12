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

(provide 'init-lua)
