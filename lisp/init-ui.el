;; 主题
;(use-package kanagawa-themes)
;;(load-theme 'kanagawa-wave t)
;; (load-theme 'kanagawa-dragon t)
;; (load-theme 'kanagawa-lotus t)
(load-theme 'badwolf t)

;; 状态栏
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'powerline)
  (sml/setup))



(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(provide 'init-ui)
