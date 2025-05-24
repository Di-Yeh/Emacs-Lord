;; 必须要先 require，才能定义 kesymap
(require 'meow)

;; 启用 transient-mark-mode 用于 region 支持
(transient-mark-mode 1)

;; ========== 启用 tab/shift-tab 缩进选区 ==========
(defun my/indent-region-or-tab ()
  "如果选中区域就缩进选区，否则插入 tab"
  (interactive)
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) tab-width)
    (insert "    ")))  ;; 或者 (indent-for-tab-command) 用真实 tab

(defun my/unindent-region ()
  "向左缩进（减少缩进）"
  (interactive)
  (when (use-region-p)
    (indent-rigidly (region-beginning) (region-end) (- tab-width))))

;; ========== 注释函数定义 ==========
(defun my/toggle-comment-region-or-line ()
  "注释或取消注释当前行或选中区域"
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

;; ========== meow 键绑定整合 ==========
(with-eval-after-load 'meow
  ;; g = 原来的 meow-visit（跳转选择）
  ;; v = Vim 风格 Visual 模式（选择模式）
  ;; u = undo
  ;; / = 注释选区或当前行
  (meow-normal-define-key
   '("s" . meow-visual-line-expand)           ;; 进入 Visual 模式
   '("u" . undo)                              ;; 撤销
   '("/" . my/toggle-comment-region-or-line)  ;; 注释切换
	 ;; 支持 DEL / Backspace 删除选中区域
	 '("DEL" . delete-region)
	 '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)))

(defun my/meow-setup ()
	(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
	;; 定义 motion 模式（一般不动它，但你想 h/j/k/l 也能在 motion 模式用可以加）
	(meow-motion-overwrite-define-key
	 '("h" . meow-left)
	 '("j" . meow-next)
	 '("k" . meow-prev)
	 '("l" . meow-right))

	;; 定义 normal 模式（你想要的 Vim 行为）
	(meow-normal-define-key
	 ;; 基本移动
	 '("h" . meow-left)
	 '("j" . meow-next)
	 '("k" . meow-prev)
	 '("l" . meow-right)

	 ;; 插入模式
	 '("i" . meow-insert)

	 ;; 删除，撤销，复制
	 '("d" . meow-kill)
	 '("y" . meow-yank)
	 '("p" . meow-clipboard-yank)
	 '("u" . undo)

	 ;; 选中一整行
	 '("v" . meow-visit)

	 ;; 退出 visual / normal 模式
	 '("<escape>" . ignore))

	;; leader 键（空格）
	(meow-leader-define-key
	 '("s" . save-buffer)
	 '("w" . other-window)
	 '("q" . kill-this-buffer)
	 '("f" . find-file)
	 '("b" . switch-to-buffer)
	 '("?" . meow-cheatsheet))

	(meow-normal-define-key
	'(">" . my/indent-region-or-tab)
	'("<" . my/unindent-region)))

;; 启用自定义配置
(my/meow-setup)
(meow-global-mode 1)


(provide 'init-meow)
