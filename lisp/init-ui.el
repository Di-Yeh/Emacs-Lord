;; 主题
(use-package ahungry-theme)
(load-theme 'ahungry t)

;; 状态栏
(use-package spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)

(use-package all-the-icons
  :if (display-graphic-p))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)


(provide 'init-ui)
