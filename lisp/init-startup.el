;; 乱码配置
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq gc-cons-threshold most-positive-fixnum)

;; 视窗配置
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; 全局开启
(setq-default truncate-lines t)

;; 禁用备份文件
(setq make-backup-files nil)


;; 设置 emoji 使用合适字体渲染
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





(provide 'init-startup)
