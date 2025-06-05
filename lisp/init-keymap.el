(setq-default tab-width 2)

;; -*- lexical-binding: t; -*-

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


(defun my/indent-region-or-tab ()
  "如果选中区域就缩进选区，否则插入四个空格"
  (interactive)
  (if (use-region-p)
      (let ((deactivate-mark nil))  ; 防止区域被自动取消
        (indent-rigidly (region-beginning) (region-end) tab-width))
    (insert "    ")))

(defun my/unindent-region ()
  "向左缩进（减少缩进）"
  (interactive)
  (when (use-region-p)
    (let ((deactivate-mark nil))  ; 防止区域被自动取消
      (indent-rigidly (region-beginning) (region-end) (- tab-width)))))

(defun my/toggle-comment-region-or-line ()
  "注释或取消注释当前行或选中区域"
	(interactive)
	(if (use-region-p)
			(comment-or-uncomment-region (region-beginning) (region-end))
		(comment-line 1)))

(defun my/change-char ()
  "如果光标所在处有字符，则提示用户输入新的字符，
并将该字符替换原字符。例如光标在 'a' 上，按下 'c'
后，提示(Replace 'a' with: )，输入 'b' 后 'a' 替换成 'b'。"
  (interactive)
  (if (null (char-after))
      (message "光标处没有字符！")
    (let* ((current (char-after))
           (prompt (format "Replace '%c' with: " current))
           (new-char (read-char prompt)))
      (delete-char 1)
      (insert-char new-char 1))))




(defun my/interactive-query-replace ()
  "从光标开始进行交互式替换操作。
首先提示输入查找字符串和替换字符串，
然后从光标处开始搜索整个缓冲区（以光标为起点，
继续到缓冲区末尾，再从缓冲区开头搜索到光标前一个位置）的所有匹配位置，
依次提示用户:
  - [n] 替换当前匹配，
  - [p] 跳过当前匹配，
  - [a] 替换所有剩余匹配，
  - [q] 退出替换流程。"
  (interactive)
  (let* ((search (read-string "查找: "))
         (replace (read-string "替换为: "))
         (matches '())
         (count 0)
         (start (point)))
    (if (string= search "")
        (message "查找字符串不能为空")
      ;; 收集匹配位置：先从光标到缓冲区末尾，再从缓冲区起始到光标前.
      (let ((matches1 '())
            (matches2 '()))
        (save-excursion
          (goto-char start)
          (while (search-forward search nil t)
            (push (copy-marker (match-beginning 0)) matches1)))
        (save-excursion
          (goto-char (point-min))
          (while (and (< (point) start)
                      (search-forward search start t))
            (push (copy-marker (match-beginning 0)) matches2)))
        (setq matches (append (nreverse matches1) (nreverse matches2))))
      (if (null matches)
          (message "在指定范围内没有找到匹配项")
        (while matches
          (goto-char (car matches))
          ;; 如果当前位置依然匹配（替换可能改变了内容）才进行交互提示
          (if (looking-at (regexp-quote search))
              (let ((choice (let ((minor-mode-overriding-map-alist nil))
                              (read-char-choice "选项: [n] 替换当前, [p] 跳过当前, [a] 替换全部, [q] 退出: " 
                                                '(?n ?p ?a ?q)))))
                (cond
                 ((eq choice ?n)
                  (replace-match replace t t)
                  (setq count (1+ count))
                  (pop matches))
                 ((eq choice ?p)
                  ;; 什么也不做，跳过该匹配
                  (pop matches))
                 ((eq choice ?a)
                  (while matches
                    (goto-char (car matches))
                    (when (looking-at (regexp-quote search))
                      (replace-match replace t t)
                      (setq count (1+ count)))
                    (pop matches))
                  (keyboard-quit))  ; 退出替换流程
                 ((eq choice ?q)
                  (setq matches nil))))
            (pop matches)))
        (message "替换结束，共替换 %d 处" count)))))














;; ------------
;; 全局绑定示例：
;; ------------
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

;; 将缩进函数绑定到 C-c >
(global-set-key (kbd "C->") 'my/indent-region-or-tab)

;; 将反缩进函数绑定到 C-c <
(global-set-key (kbd "C-<") 'my/unindent-region)

;; 將注釋函數綁定到 C-c /
(global-set-key (kbd "C-c /") 'my/toggle-comment-region-or-line)

;; 將替換字符函數綁定到 C-c c
(global-set-key (kbd "C-c c") 'my/change-char)

;; 將替換文字函數綁定到 C-c r
(global-set-key (kbd "C-c r") 'my/interactive-query-replace)

;; lsp-ui-peek-find-definitions
(global-set-key (kbd "C-c l F") 'lsp-ui-peek-find-definitions)

;; lsp-ui-peek-find-references
(global-set-key (kbd "C-c l f") 'lsp-ui-peek-find-references)

;; lsp-ui-imenu
(global-set-key (kbd "C-c l m") 'lsp-ui-imenu)

;; neotree
(global-set-key (kbd "C-c n") 'neotree-toggle)

;; drivish
(global-set-key (kbd "C-c d") 'dirvish)




(provide 'init-keymap)
