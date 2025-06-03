(setq-default tab-width 2)

(defun my/scroll-window-down ()
  "模拟鼠标往下滚动视窗：向下显示一行内容，即文本上移一行。"
  (interactive)
  (if (fboundp 'scroll-up-line)
      (scroll-up-line 1)
    ;; 如果你的 Emacs 版本没有 scroll-up-line，则用 scroll-up
    (scroll-up 1)))

(defun my/scroll-window-up ()
  "模拟鼠标往上滚动视窗：向上显示一行内容，即文本下移一行。"
  (interactive)
  (if (fboundp 'scroll-down-line)
      (scroll-down-line 1)
    ;; 如果你的 Emacs 版本没有 scroll-down-line，则用 scroll-down
    (scroll-down 1)))





(defun my/get-installed-plugin-names ()
  "返回当前已安装插件的名称列表，从 `package-alist` 中提取（字符串列表）。"
  (if (boundp 'package-alist)
      (delete-dups
       (mapcar (lambda (pkg)
                 (symbol-name (car pkg)))
               package-alist))
    '()))

(defun my/choose-plugin-and-display-functions ()
  "提示你选择一个插件名称，然后显示所有与该插件名称匹配的命令及其功能介绍（取其文档字符串第一行）。"
  (interactive)
  (let* ((plugins (my/get-installed-plugin-names))
         (plugin (completing-read "请选择插件名称: " plugins nil t)))
    (if (string= plugin "")
        (message "未选择插件。")
      (let ((commands-list '()))
        ;; 遍历 obarray，挑选出所有命令，且命令名称中包含所选插件的字符串。
        (mapatoms (lambda (sym)
                    (when (and (commandp sym)
                               (string-match-p (regexp-quote plugin)
                                               (symbol-name sym)))
                      (push sym commands-list)))
                  obarray)
        ;; 对命令按名称排序
        (setq commands-list
              (sort commands-list
                    (lambda (a b)
                      (string< (symbol-name a) (symbol-name b)))))
        ;; 将结果输出到临时 buffer 中
        (with-output-to-temp-buffer "*Plugin Functions*"
          (princ (format "插件 \"%s\" 相关功能介绍：\n\n" plugin))
          (if commands-list
              (dolist (cmd commands-list)
                (let* ((doc (documentation cmd t))
                       (intro (if (and doc (not (string= doc "")))
                                  (car (split-string doc "\n"))
                                "无说明")))
                  (princ (format "%-40s: %s\n" (symbol-name cmd) intro))))
            (princ (format "未找到与 \"%s\" 相关的命令." plugin))))))))




;; 自定义 iedit-dwim 函数：
(defun iedit-dwim (arg)
  "启动 iedit 模式。
如果传入前缀参数，则在整个缓冲区中匹配，否则仅匹配当前函数内部的内容。"
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-defun)
        (iedit-mode)))))

;; 使用示例：
;; 1. 在编辑代码或文本时，将光标放在某个单词上。
;; 2. 按下 C-; 进入 iedit 模式，此时所有与该单词相同的部分会高亮显示，
;;    编辑其中一个区域，其他区域会同步更新。
;; 3. 当编辑完成后，按下 C-c C-c 结束 iedit 模式。





;; 全局绑定示例：
(global-set-key (kbd "M-n") 'my/scroll-window-down)
(global-set-key (kbd "M-p") 'my/scroll-window-up)


(global-set-key (kbd "TAB") 'self-insert-command)

(define-key global-map (kbd "RET") 'default-indent-new-line)
(define-key global-map (kbd "M-j") 'newline-and-indent)


;; 绑定快捷键 C-c h 调用该函数
(global-set-key (kbd "C-c h") 'my/choose-plugin-and-display-functions)

;; 为 iedit-dwim 绑定一个快捷键，比如 M-I（Alt + I），你可以根据自己的习惯更改：
(global-set-key (kbd "M-I") 'iedit-dwim)

;; multiple cursor
(global-set-key (kbd "C-M->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; markdown-view-mode
(global-set-key (kbd "C-c m v") 'markdown-view-mode)
(global-set-key (kbd "C-c m m") 'markdown-mode)




(provide 'init-keymap)
