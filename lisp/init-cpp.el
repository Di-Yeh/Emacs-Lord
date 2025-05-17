;; Configuring Emacs as a C/C++ IDE
(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast


;; 自定义 C/C++ 缩进风格为 my-c-style
(c-add-style "my-c-style"
             '("bsd"                          ;; 基于 BSD 风格 ;; 也可以切换为 "k&r", "stroustrup", "java", 等风格
               (c-basic-offset . 4)           ;; 设置缩进为 4 空格
               (indent-tabs-mode . nil)       ;; 使用空格代替 TAB
               (tab-width . 4)))              ;; TAB 宽度设为 4

(defun my-c-c++-mode-hook ()
  (c-set-style "my-c-style")                 ;; 使用自定义风格
  (setq indent-tabs-mode nil)               ;; 强制空格缩进
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq backward-delete-char-untabify-method -1) ;;退格删除
  (electric-indent-local-mode 1)            ;; 保持回车自动缩进开启
  )

;; 把钩子绑定到 C/C++ 模式
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)


;; C/C++编译配置
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

(global-set-key (kbd "<f5>") 'quickrun)



(provide 'init-cpp)
