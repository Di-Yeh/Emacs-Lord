;; use-package with package.el

;; use-package with package.el:
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner '"~/.emacs.d/logo/DiYeh.png")
  (setq dashboard-center-content t)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)) 

(require 'dashboard)
(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "Chase your dreams, You Only Live Once!")

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)


(setq dashboard-item-shortcuts '((recents   . "r")
                                 (projects  . "p")
								 (bookmarks  . "b")
								 (agenda  . "a")
								 (registers . "e")))

(setq dashboard-items '((projects  . 5)
                        (recents . 5)
						(bookmarks . 5)
						(agenda . 5)
						(registers . 5)))

(setq dashboard-page-separator "\n\n")
(setq dashboard-navigation-cycle t)



;(setq dashboard-set-init-info nil)
;(setq dashboard-init-info "\n✦ ──────────────────────────── ✦ ✧ ✦ ──────────────────────────── ✦")



(provide 'init-dashboard)
