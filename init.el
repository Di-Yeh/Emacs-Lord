;;; init.el --- main config

(add-to-list 'load-path
	     (expand-file-name (concat user-emacs-directory "lisp")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-start)
(require 'init-elpa)
(require 'init-package)
(require 'init-ui)
(require 'init-keymap)
(require 'init-dashboard)
(require 'init-const)
(require 'init-lsp)
(require 'init-cpp)
(require 'init-python)


(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)



(when (display-graphic-p)
  ;; 设置 Emoji fallback 字体为 Windows 自带的 Segoe UI Emoji
  (set-fontset-font t 'emoji "Segoe UI Emoji" nil 'prepend)
  ;; 补充 Unicode 范围（有些 emoji 不属于 'emoji 类别）
  (dolist (range '((#x1f300 . #x1f6ff)  ; pictographs
                   (#x1f900 . #x1f9ff)  ; supplemental pictographs
                   (#x2600  . #x26FF)   ; misc symbols
                   (#x2700  . #x27BF)   ; dingbats
                   (#x1FA70 . #x1FAFF))) ; extended symbols
    (set-fontset-font t (cons (car range) (cdr range)) "Segoe UI Emoji" nil 'prepend)))




;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;(require 'eaf)
;(require 'eaf-pdf-viewer)
;(require 'eaf-browser)
;(require 'eaf-music-player)
;(require 'eaf-video-player)
;(require 'eaf-image-viewer)


(when (file-exists-p custom-file)
  (load-file custom-file))
