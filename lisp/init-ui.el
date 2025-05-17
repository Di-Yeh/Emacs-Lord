;; 主题
(use-package ahungry-theme)
(load-theme 'ahungry t)

;; 状态栏
(use-package spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)

(use-package nerd-icons
  :config
  (if (display-graphic-p)
      (setq nerd-icons-use-svg t)
    (setq nerd-icons-use-svg nil)))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(provide 'init-ui)
