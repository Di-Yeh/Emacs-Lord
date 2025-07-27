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


(defun my/query-replace-read-args-advice (orig-fun &rest args)
  "Advice for `query-replace-read-args'.
在原始 PROMPT 前面添加自定义选项说明，而不单独调用 `message`。"
  (let* ((original-prompt (car args))
         (new-prompt (concat "选项: [y]=替换当前, [n]=跳过当前, [!]=替换全部, [q]=退出: " original-prompt)))
    (apply orig-fun (cons new-prompt (cdr args)))))

(advice-add 'query-replace-read-args :around #'my/query-replace-read-args-advice)
(advice-add 'query-replace-regexp-read-args :around #'my/query-replace-read-args-advice)


(defun my/list-lsp-diagnostics ()
  "显示当前诊断信息:
如果当前启用了 lsp-bridge，则使用 `lsp-bridge-diagnostic-list`；
否则使用 Flycheck 的 `flycheck-list-errors`。"
  (interactive)
  (if (and (bound-and-true-p lsp-bridge-mode)
           (fboundp 'lsp-bridge-diagnostic-list))
      (lsp-bridge-diagnostic-list)
    (if (fboundp 'flycheck-list-errors)
        (flycheck-list-errors)
      (message "当前环境中未检测到 lsp-bridge 或 flycheck。"))))


;; 调整窗口宽度：通用函数和专门的包装命令
(defun my/adjust-window-width (delta)
  "将当前窗口的宽度调整 DELTA 大小（正值增大，负值减小）。
如果调用后窗口宽度没有变化，则显示提示信息。"
  (let ((old-width (window-width)))
    (enlarge-window-horizontally delta)
    (when (= (window-width) old-width)
      (message "无法进一步调整窗口宽度！"))))

(defun my/shrink-window-width ()
  "缩小当前窗口宽度 1 列。"
  (interactive)
  (my/adjust-window-width -1))

(defun my/enlarge-window-width ()
  "增加当前窗口宽度 1 列。"
  (interactive)
  (my/adjust-window-width 1))


;; 调整窗口高度：通用函数和专门的包装命令
(defun my/adjust-window-height (delta)
  "将当前窗口的高度调整 DELTA 大小（正值增大，负值减小）。
如果调用后窗口高度没有变化，则显示提示信息。"
  (let ((old-height (window-height)))
    (enlarge-window delta)
    (when (= (window-height) old-height)
      (message "无法进一步调整窗口高度！"))))

(defun my/shrink-window-height ()
  "缩小当前窗口高度 1 行。"
  (interactive)
  (my/adjust-window-height -1))

(defun my/enlarge-window-height ()
  "增加当前窗口高度 1 行。"
  (interactive)
  (my/adjust-window-height 1))


;; 定义 Vim 风格的 open 命令
(defun my/open-insert-line ()
  "模仿 Vim 的 'o' 命令：在当前行末尾插入新行（并自动缩进），
然后进入插入模式。"
  (interactive)
  (end-of-line)
  (newline-and-indent))


(defvar my/line-select-offset 0
  "相对于锚点的行偏移，用于连续扩展行选区。")

(defvar my/line-select-anchor nil
  "选区的起始锚点：首次按键时的行首位置。")

(defun my/_select-lines (target-pos)
  "从锚点 `my/line-select-anchor` 到 TARGET-POS 建立选区，并将光标移到 TARGET-POS。"
  (goto-char target-pos)
  ;; mark 在锚点处
  (set-mark my/line-select-anchor)
  (activate-mark))

(defun my/select-line-next ()
  "向下连续选整行。首次选当前行，后续依次扩展到下一行，光标移到行尾。"
  (interactive)
  (unless (eq last-command 'my/select-line-next)
    ;; 重置偏移和锚点
    (setq my/line-select-offset 0
          my/line-select-anchor (line-beginning-position)))
  ;; 计算目标行号
  (let* ((anchor-line (line-number-at-pos my/line-select-anchor))
         (target-line (+ anchor-line my/line-select-offset))
         (target-end-pos
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- target-line))
            (line-end-position))))
    ;; 建立选区并移动光标
    (my/_select-lines target-end-pos))
  ;; 为下次扩展做准备
  (cl-incf my/line-select-offset))

(defun my/select-line-previous ()
  "向上连续选整行。首次选当前行，后续依次扩展到上一行，光标移到行首。"
  (interactive)
  (unless (eq last-command 'my/select-line-previous)
    ;; 重置偏移和锚点
    (setq my/line-select-offset 0
          my/line-select-anchor (line-beginning-position)))
  ;; 计算目标行号
  (let* ((anchor-line (line-number-at-pos my/line-select-anchor))
         (target-line (+ anchor-line (- my/line-select-offset)))
         (target-start-pos
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- target-line))
            (line-beginning-position))))
    ;; 建立选区并移动光标
    (my/_select-lines target-start-pos))
  ;; 为下次扩展做准备
  (cl-incf my/line-select-offset))


;; ------------
;; 全局绑定示例：
;; ------------

(global-set-key (kbd "M-n") 'my/scroll-window-down)
(global-set-key (kbd "M-p") 'my/scroll-window-up)

(global-set-key (kbd "TAB") 'self-insert-command)

(define-key global-map (kbd "RET") 'default-indent-new-line)
(define-key global-map (kbd "M-j") 'newline-and-indent)

(global-set-key (kbd "C-M-/") #'my/select-line-next)
(global-set-key (kbd "C-M-?") #'my/select-line-previous)

(global-set-key (kbd "C-S-o") 'my/open-insert-line)

(global-set-key (kbd "C-{") 'my/shrink-window-width)
(global-set-key (kbd "C-}") 'my/enlarge-window-width)
(global-set-key (kbd "M-[") 'my/shrink-window-height)
(global-set-key (kbd "M-]") 'my/enlarge-window-height)

(global-set-key (kbd "C-<return>") 'electric-newline-and-maybe-indent)

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

;; 將替換字符函數綁定到 C-c C
(global-set-key (kbd "C-c C") 'my/change-char)

;; lsp-ui-peek-find-definitions
(global-set-key (kbd "C-c l F") 'lsp-ui-peek-find-definitions)

;; lsp-ui-peek-find-references
(global-set-key (kbd "C-c l f") 'lsp-ui-peek-find-references)

;; lsp-treemacs-symbols
(global-set-key (kbd "C-c l m") 'lsp-treemacs-symbols)

;; lsp-treemacs-show-
(global-set-key (kbd "C-c l e") 'lsp-treemacs-errors-list)

;; lsp显示错误
(global-set-key (kbd "C-c l e") 'my/list-lsp-diagnostics)

;; ts-fold
(global-set-key (kbd "C-c t t") 'ts-fold-toggle)    ;; 切换当前语法节点的折叠状态
(global-set-key (kbd "C-c t o") 'ts-fold-open-all)    ;; 展开所有折叠节点
(global-set-key (kbd "C-c t c") 'ts-fold-close-all)   ;; 折叠所有语法节点

;; g++配置快捷键
(global-set-key (kbd "<f5>") 'quickrun)

;; Cmake 构建快捷键
(global-set-key (kbd "C-c c") 'my/create-cmake-project)
(global-set-key (kbd "C-c B") 'my/cmake-configure-and-build)

;; Visual Studio 项目编译快捷键
;; devenv.com
(global-set-key (kbd "<f7>") #'my/compile-project)
;; nmake
(global-set-key (kbd "<C-f7>") #'my/nmake-build)
;; 运行
(global-set-key (kbd "<C-f5>") #'my/run-vs-exe)

;; 将调试函数绑定到全局快捷键
(global-set-key (kbd "C-M-g") 'my-gdb-run)

;; org-roam
(global-set-key (kbd "C-c C-n") 'org-roam-node-find)













(provide 'init-keymap)
