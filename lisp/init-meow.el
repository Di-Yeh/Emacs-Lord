;;; -*- lexical-binding: t -*-

;; ---------------------------
;; 定义用于 Shift 选择模式的命令
;; ---------------------------

(defun my/shift-select-left ()
  "向左移动一个字符，更新选区。"
  (interactive)
  (backward-char 1))

(defun my/shift-select-right ()
  "向右移动一个字符，更新选区。"
  (interactive)
  (forward-char 1))

(defun my/shift-select-up ()
  "向上移动一行，更新选区。"
  (interactive)
  (previous-line 1))

(defun my/shift-select-down ()
	"向下移动一行，更新选区。"
	(interactive)
	(next-line 1))

(defun my/shift-select-activate ()
  "激活 Shift 选择模式：若区域未激活则调用 set-mark-command，然后
使用 transient keymap 将 h/j/k/l 重绑定到扩展选区命令，直到退出。"
  (interactive)
  (unless (region-active-p)
    (set-mark-command nil))
  (message "Shift-select mode: 使用 h/j/k/l 调整选区，任意其它键退出。")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "h") #'my/shift-select-left)
     (define-key map (kbd "j") #'my/shift-select-down)
     (define-key map (kbd "k") #'my/shift-select-up)
     (define-key map (kbd "l") #'my/shift-select-right)
     map)
   t))

;; 定义 Vim 风格的 append 命令，即 “a”
(defun my/meow-append ()
  "模仿 Vim 的 'a' 命令：如果光标不在行尾，先向右移动一格，
然后进入插入模式。"
  (interactive)
  (unless (eolp)
    (forward-char 1))
  (meow-insert))

;; 定义 Vim 风格的 open 命令，即 “o”
(defun my/meow-open-line ()
  "模仿 Vim 的 'o' 命令：在当前行末尾插入新行（并自动缩进），
然后进入插入模式。"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (meow-insert))

;; 下面是你之前的 meow 配置，可以在已有的键绑定部分中添加这两个命令
(require 'meow)
(transient-mark-mode 1)

;; ---------------------------
;; 自定义 “y” 用于复制选取范围内文字
;; ---------------------------
(defun my/meow-yank ()
  "复制选中的区域到 kill ring，并退出 Visual 模式（取消选区）。
如果没有选中区域，则复制当前行的文字。"
  (interactive)
  (if (use-region-p)
      (progn
        (kill-ring-save (region-beginning) (region-end))
        (deactivate-mark)
        (message "区域已复制"))
    (kill-ring-save (line-beginning-position) (line-end-position))
    (message "当前行已复制")))


;; ---------------------------
;; 自定义 “d”，单击删除一个字符，双击删除整行，删除内容不进入 kill ring
;; ---------------------------
(defvar my/d-timer nil
  "用于检测 meow 模式下 d 键双击的定时器。")

(defvar my/d-double-tap-delay 0.3
  "两次按 d 键之间的最大间隔（秒），低于该值认为是双击。")

(defvar my/d-timer nil
  "用于检测 meow 模式下 d 键双击的定时器。")

(defvar my/d-double-tap-delay 0.3
  "两次按 d 键之间的最大间隔（秒），低于该值认为是双击。")

(defun my/meow-delete ()
  "在 Meow 中：
- 如果有选区，则直接删除选中区域内的文字，并取消选区；
- 如果没有选区，则区分单击与双击：
    单击 d 键：删除光标处的一个字符；
    快速连续两次 d 键：删除光标所在整行（包括行尾换行符，如果存在）。
删除的内容不会进入 kill ring。"
  (interactive)
  (if (use-region-p)
      ;; 如果存在选区，则直接删除选区内的内容
      (progn
        (delete-region (region-beginning) (region-end))
        (deactivate-mark)
        (message "选区内容已删除"))
    (if my/d-timer
        ;; 如果定时器存在，说明这是第二次按下 d 键（双击）
        (progn
          (cancel-timer my/d-timer)
          (setq my/d-timer nil)
          (let* ((line-beg (line-beginning-position))
                 (line-end (line-end-position))
                 (delete-end (if (and (< line-end (point-max))
                                      (eq (char-after line-end) ?\n))
                                 (1+ line-end)
                               line-end)))
            (delete-region line-beg delete-end))
          (message "整行已删除"))
      ;; 第一次按下，则启动定时器等待可能的双击
      (setq my/d-timer (run-with-timer my/d-double-tap-delay nil
                                       (lambda ()
                                         (delete-char 1)
                                         (setq my/d-timer nil)
                                         (message "删除了一个字符")))))))

;;	==============================================
;;											MEOW
;;	==============================================

;; 修改后的键绑定：新增 "a" 和 "o" 命令，分别模拟 Vim 的 append 和 open-line
(with-eval-after-load 'meow
  (meow-normal-define-key
   '("a" . my/meow-append)          				;; Vim 的 a：附加模式
   '("o" . my/meow-open-line)       				;; Vim 的 o：在下方插入新行
   '("s" . my/shift-select-activate)    		;; s 激活“Shift选择”模式
   '("S" . meow-visual-line-expand)       	;; 如果你还需要视觉行扩展，可保留到大写 S
   '("v" . meow-visit)              				;; 保留 v 进入原有的 Visual 模式
   '("u" . undo)                    				;; 撤销
   '("/" . my/toggle-comment-region-or-line)
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
   '("9" . meow-expand-9)
	 '("w" . forward-word)
	 '("W" . backward-word)))

(defun my/meow-setup ()
  "自定义 Meow 配置。"
  (setq meow-cheatsheet-layout 'qwerty)
  ;; 定义 motion 模式（基本的 hjkl 键移动）
  (meow-motion-overwrite-define-key
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right))
  ;; 定义 normal 模式（类似 Vim 行为）
  (meow-normal-define-key
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("i" . meow-insert)
   '("d" . my/meow-delete)
   '("y" . my/meow-yank)
   '("p" . meow-clipboard-yank)
   '("u" . undo)
	 '("c" . my/change-char)
	 '("F" . lsp-ui-peek-find-definitions)
   '("f" . lsp-ui-peek-find-references)
	 '("m" . lsp-treemacs-symbols)
	 '("r" . my/interactive-query-replace)
	 '("t" . ts-fold-toggle)
	 '("O" . ts-fold-open-all)
	 '("C" . ts-fold-close-all)
   '("<escape>" . ignore))
  ;; Leader 键绑定（空格为前缀）
  (meow-leader-define-key
   '("s" . save-buffer)
   '("w" . other-window)
   '("q" . kill-this-buffer)
   '("f" . find-file)
   '("b" . switch-to-buffer)
   '("?" . meow-cheatsheet)
	 '("D" . dirvish)
	 '("n" . neotree-toggle)
	 '("p" . projectile-compile-project)
	 '("e" . eshell)
	 '("d" . dap-hydra))
	
  ;; 缩进相关绑定
  (meow-normal-define-key
   '(">" . my/indent-region-or-tab)
   '("<" . my/unindent-region)))

(my/meow-setup)
(meow-global-mode 1)

(provide 'init-meow)
