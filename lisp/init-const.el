;; ----------------------------
;; 跨平台定义系统类型变量
;; ----------------------------
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos)
                           (eq system-type 'windows-nt)))

;; ----------------------------
;; 启动时默认隐藏编译警告（特别是 Linux）
;; ----------------------------
(when *is-linux*
  ;; 隐藏 native-comp 编译警告，不弹窗
  (setq native-comp-async-report-warnings-errors nil))

;; 默认仅显示 error 级别，忽略 warning/info
(setq warning-minimum-level :error)

;; （可选）完全忽略某些 warning 类型
;; (setq warning-suppress-types '((comp) (native-compile)))


;; ----------------------------
;; 定义一个 toggle 函数，允许你动态切换显示/隐藏警告
;; ----------------------------

(defvar my/warning-verbosity-hidden t
  "t 表示隐藏大多数警告，仅显示 error；nil 表示显示所有警告。")

(defun my/toggle-warning-verbosity ()
  "切换警告显示级别与 native-comp 编译信息（仅在 Linux 有效）。"
  (interactive)
  (if my/warning-verbosity-hidden
      (progn
        (setq warning-minimum-level :warning)
        (when *is-linux*
          (setq native-comp-async-report-warnings-errors t))
        (setq my/warning-verbosity-hidden nil)
        (message "⚠️ 警告信息已开启（warning-level: :warning）"))
    (setq warning-minimum-level :error)
    (when *is-linux*
      (setq native-comp-async-report-warnings-errors nil))
    (setq my/warning-verbosity-hidden t)
    (message "✅ 警告信息已关闭（warning-level: :error）")))

;; ----------------------------
;; 绑定一个快捷键（可选）
;; ----------------------------
(global-set-key (kbd "C-c C-w") #'my/toggle-warning-verbosity)


(defun my/check-and-convert-to-utf8 ()
  "如果当前 buffer 的编码不是 UTF-8，则提示是否转换为 UTF-8-unix。"
  (let ((coding buffer-file-coding-system))
    ;; 检查当前编码是否不是 utf-8 或 utf-8-unix
    (unless (or (eq coding 'utf-8)
                (eq coding 'utf-8-unix))
      ;; 弹出提示
      (when (yes-or-no-p
             (format "当前文件编码为 %s，不是 UTF-8。是否转换为 UTF-8？" coding))
        (set-buffer-file-coding-system 'utf-8 t)
        (message "已转换为 UTF-8 编码。保存文件以生效。")))))

;; 加入文件打开的 hook：每次打开文件时检查编码
(add-hook 'prog-mode #'my/check-and-convert-to-utf8)


(provide 'init-const)
