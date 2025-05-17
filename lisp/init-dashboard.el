;; use-package with package.el

;; use-package with package.el:
(use-package dashboard
  :init
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

(require 'dashboard)
(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs!")
;; Set the banner
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
;; vertically center content
(setq dashboard-vertically-center-content t)
;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

(setq dashboard-item-shortcuts '((recents   . "r")
                                 (projects  . "p")))

(setq dashboard-items '((projects  . 5)
                        (recents . 5)))

(setq dashboard-page-separator "\n\n\n")

(setq dashboard-navigation-cycle t)





(provide 'init-dashboard)
