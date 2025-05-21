(add-to-list 'load-path "/path/to/lsp-lua")
(require 'lsp-lua)

;; 添加 Lua 的quickrun 命令
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "Lua"
    '((:command . "Lua")  ;; 使用 Lua
      (:exec    . ("%c %s"))
      (:description . "Run Lua script"))
    :default "Lua"))

(global-set-key (kbd "C-S-l") 'quickrun)  ;; lua

(provide 'init-lua)
