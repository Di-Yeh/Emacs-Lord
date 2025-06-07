;;; init.el --- main config

(add-to-list 'load-path
	     (expand-file-name (concat user-emacs-directory "lisp")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-start)
(require 'init-const)
(require 'init-elpa)
(require 'init-package)
(require 'init-lsp)

(require 'init-dashboard)
(require 'init-ui)

(require 'init-keymap)
(require 'init-meow)

(require 'init-cpp)
(require 'init-lua)
(require 'init-python)
(require 'init-markdown)

(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)



(defun my/setup-emoji-font ()
  "为 emoji 设置专用字体，仅补充显示，不影响主字体。"
  (when (display-graphic-p)
    (let ((emoji-font
           (cond
            ((eq system-type 'darwin) "Apple Color Emoji")      ; macOS 自带
            ((eq system-type 'windows-nt) "Segoe UI Emoji")     ; Windows 自带
            ((eq system-type 'gnu/linux) "Noto Color Emoji")))) ; Linux 常用
      ;; 如果系统中存在 emoji 字体
      (when (member emoji-font (font-family-list))
        ;; 设置字体集：所有 emoji 字符使用此字体显示
        (set-fontset-font t 'emoji emoji-font nil 'prepend)
        ;; 额外处理常见范围：Unicode Emoji 补充区段（如 ✨ 等）
        (dolist (range '((#x1f300 . #x1f6ff) ; Misc symbols and pictographs
                         (#x1f900 . #x1f9ff) ; Supplemental Symbols and Pictographs
                         (#x2600  . #x26FF)  ; Misc symbols
                         (#x2700  . #x27BF)  ; Dingbats
                         (#x1FA70 . #x1FAFF))) ; Extended emoji
          (set-fontset-font t (cons (car range) (cdr range)) emoji-font nil 'prepend))))))
  
(add-hook 'after-init-hook #'my/setup-emoji-font)


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






(when (file-exists-p custom-file)
  (load-file custom-file))
