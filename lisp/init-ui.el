;; 主题
(use-package ahungry-theme)
(load-theme 'ahungry t)

;; 定义你喜欢的主题列表（顺序很重要）
(defvar my/themes '(ahungry leuven)
  "我喜爱的主题清单，依序切换。")

;; 当前主题索引
(defvar my/theme-index 0
  "当前主题索引。")

;; 切换主题函数
(defun my/cycle-themes ()
  "依序切换预设主题 my/themes。"
  (interactive)
  (let ((theme (nth my/theme-index my/themes)))
    ;; 关闭所有启用的主题
    (mapc #'disable-theme custom-enabled-themes)
    ;; 加载新主题
    (load-theme theme t)
    (message "已加载主题：%s" theme)
    ;; 更新索引
    (setq my/theme-index (mod (1+ my/theme-index) (length my/themes)))))

;; 绑定快捷键 Ctrl-T
(global-set-key (kbd "C-t") 'my/cycle-themes)




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
