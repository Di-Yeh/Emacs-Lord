;;; init.el --- main config

(add-to-list 'load-path
	     (expand-file-name (concat user-emacs-directory "lisp")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-start)
(require 'init-const)
(require 'init-elpa)
(require 'init-package)
(require 'init-lsp)
(require 'init-lisp)

(require 'init-dashboard)
(require 'init-ui)

(require 'init-keymap)

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


;; 判断 ~/.emacs.d/site-lisp/emacs-application-framework/ 是否存在
(let ((eaf-dir (expand-file-name "site-lisp/emacs-application-framework/" user-emacs-directory)))
  (when (file-directory-p eaf-dir)
    ;; 添加 EAF 路径到 load-path
    (add-to-list 'load-path eaf-dir)

    ;; 加载 EAF 主模块
    (require 'eaf)

    ;; 以下是常用模块，可根据需求裁剪或添加
    (require 'eaf-browser)
    (require 'eaf-pdf-viewer)
    (require 'eaf-video-player)
    (require 'eaf-music-player)
    (require 'eaf-image-viewer)

    ;; 你也可以在此设置一些默认配置，例如键绑定或启用选项
    ;; (setq eaf-browser-default-zoom 1.25)
    (message "✅ EAF 加载完成。")))


;; 字体
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; … 你的 default 和 org-level-* 定义保持不变 …
 '(default ((t (:family "CaskaydiaCove NF" :foundry "outline"
                        :slant normal :weight semi-bold
                        :height 120 :width normal))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1)))))

;; --------------------------------
;; 中文字体设置：SimHei，继承 default face 的高度
;;       —— 这样就能和英文一起缩放了！
;; --------------------------------
(when (display-graphic-p)
  (dolist (charset '(kana han cjk-misc bopomofo))
    ;; 只指定 family，不指定 size
    (set-fontset-font t
                      charset
                      (font-spec :family "SimHei"))))

;; --------------------------------
;; 显示相对行号
;; --------------------------------
(use-package emacs
  :ensure t
	:config 
	(setq display-line-numbers-type 'relative) 
	(global-display-line-numbers-mode t)
)

;; 设置当前 Frame 透明度：90% 前景 / 70% 非激活时
(set-frame-parameter (selected-frame) 'alpha '(95 . 85))

;; 默认新建 Frame 也应用
(add-to-list 'default-frame-alist '(alpha . (95 . 85)))



(when (file-exists-p custom-file)
  (load-file custom-file))
