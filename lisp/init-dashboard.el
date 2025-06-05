(use-package page-break-lines
  :ensure t
  :hook (dashboard-mode . page-break-lines-mode))

;; use-package with package.el
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner '"~/.emacs.d/logo/DiYeh.png")
  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'all-the-icons) ; use 'all-the-icons package
  (setq dashboard-set-heading-icons t)
	(setq dashboard-set-init-info t)           ;; 显示启动时间信息
  (setq dashboard-set-file-icons t))

(setq-default dashboard-center-content t)

(require 'dashboard)
;; Set the title
(setq dashboard-banner-logo-title "Chase your dreams, You Only Live Once!")

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

(setq dashboard-item-shortcuts '((recents   . "r")
                                 (projects  . "p")
																 (agenda . "a")))

(setq dashboard-items '((projects  . 10)
                        (recents . 10)
												(agenda . 10)))

(setq dashboard-page-separator  "\n\n\f\f\n\n")

(setq dashboard-navigation-cycle t)
(dashboard-setup-startup-hook)



(provide 'init-dashboard)
