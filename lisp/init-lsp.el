;;; init-lsp.el --- Central LSP configuration with lsp-bridge integration

;;; Commentary:
;; 该配置根据文件大小及类型智能选择 LSP 后端：
;; - 对于 Emacs Lisp 文件，跳过 LSP；
;; - 对于编程文件（derived from prog-mode）：
;;     如果项目检测不到，则以当前文件所在目录作为项目根目录；
;;     如果文件大小 <= 200KB，则启动 lsp-mode；
;;     如果文件大小 > 200KB，则提示用户是否使用 lsp-bridge：
;;       若选择“是”，则 require 'init-lsp-bridge 并启用 (lsp-bridge-mode)；
;;       否则使用 lsp-mode。
;;
;; 同时，在 lsp-bridge 模式下自动禁用 company、flycheck 以及 lsp-ui，以免资源重复消耗。

;;; Code:

;; 先加载 lsp-mode 相关配置
(require 'init-lsp-mode)

;; 定义专用函数，在 lsp-bridge 缓冲区关闭冗余插件
(defun my/disable-lsp-mode-extras-for-bridge ()
  "在启用 lsp-bridge 的缓冲区中关闭 company、flycheck 与 lsp-ui 提升性能。"
  (when (bound-and-true-p lsp-bridge-mode)
    ;; 关闭冗余的补全框架
    (when (bound-and-true-p company-mode)
      (company-mode -1))
    ;; 关闭 flycheck 检查
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    ;; 关闭 lsp-ui 显示
    (when (bound-and-true-p lsp-ui-mode)
      (lsp-ui-mode -1))
    (message "在 lsp-bridge 模式下禁用了 company、flycheck 与 lsp-ui.")))

;; 当 lsp-bridge-mode 启动后调用该函数
(add-hook 'lsp-bridge-mode-hook #'my/disable-lsp-mode-extras-for-bridge)

(defun my/setup-lsp ()
  "初始化 LSP 配置：
- Emacs Lisp 文件跳过 LSP。
- 对 C/C++、Python、Lua 的编程文件，根据项目检测与文件大小选择后端：
    * 大文件 (>500KB)：提示使用 lsp-bridge（或 fallback 为 lsp-mode）。
    * 小文件 (<=500KB)：启动 lsp-mode。
- 对其他编程文件，不启用 LSP（避免卡顿）。"
  (unless (or (bound-and-true-p my-lsp-setup-done)
              (minibufferp))
    (setq-local my-lsp-setup-done t)
    (cond
     ;; Emacs Lisp 直接跳过
     ((eq major-mode 'emacs-lisp-mode)
      (message "当前为 Emacs Lisp 文件，不启用 LSP。"))
     ;; 仅对指定的编程语言启用 LSP
     ((and (derived-mode-p 'prog-mode)
           (member major-mode '(clojure-mode clojurec-mode clojurescript-mode c-mode c++-mode python-mode lua-mode))) ; 如果要使用clojure就加入 clojure-mode clojurec-mode clojurescript-mode 即可
      ;; 设定默认目录：尝试使用 Projectile 检测项目根目录
      (if (and (fboundp 'projectile-project-p)
               (projectile-project-p))
          (let ((proj-root (projectile-project-root)))
            (setq default-directory (file-name-as-directory proj-root))
            (message "检测到项目，切换到项目根目录: %s" default-directory))
        (when buffer-file-name
          (setq default-directory (file-name-directory buffer-file-name))
          (message "未检测到项目，使用文件所在目录: %s" default-directory)))
      ;; 根据文件大小选择 LSP 后端
      (let ((size (when buffer-file-name
                    (nth 7 (file-attributes buffer-file-name)))))
        (if (and size (> size (* 500 1024)))
            (if (yes-or-no-p (format "该文件 (%d KB) 超过500KB，是否使用 lsp-bridge？ " (/ size 1024)))
                (progn
                  (require 'init-lsp-bridge)
                  (lsp-bridge-mode)
                  (message "大文件，已激活 lsp-bridge 模式。"))
              (progn
                (lsp)
                (message "大文件，但继续使用 lsp-mode。")))
          (progn
            (lsp)
            (message "小文件，已激活 lsp-mode。")))))
     ;; 对其他编程文件，不启用 LSP，给出提示
     ((derived-mode-p 'prog-mode)
      (message "当前编程语言（%s）未配置 LSP 支持。" major-mode))
     (t
      (lsp)
      (message "非编程文件，默认加载 lsp-mode。")))))

(add-hook 'find-file-hook #'my/setup-lsp)


;; ———— Eldoc & Hover 管理 ————

(defun my/lsp-mode-enable-hover ()
  "在 lsp-mode 启用时，打开 hover 支持。"
  (when (fboundp 'lsp-eldoc-mode)  (lsp-eldoc-mode 1))
  (eldoc-mode 1))

(defun my/lsp-mode-disable-hover ()
  "在切出 lsp-mode 时，关闭 hover 支持。"
  ;; 关闭 lsp-eldoc
  (when (fboundp 'lsp-eldoc-mode)  (lsp-eldoc-mode -1))
  ;; 关闭 eldoc
  (eldoc-mode -1)
  ;; 取消所有 lsp--on-idle hover 定时器
  (when (fboundp 'lsp--on-idle)
    (cancel-function-timers #'lsp--on-idle))
  ;; 从 eldoc hook 中移除 lsp-eldoc-function
  (remove-hook 'eldoc-documentation-functions #'lsp-eldoc-function t))

;; 全局在 lsp-mode 启动后打开 hover
(add-hook 'lsp-mode-hook #'my/lsp-mode-enable-hover)


;; ———— 开/关 清理函数 ————

(defun my/cleanup-lsp-mode ()
  "彻底停用 lsp-mode：断开、关 workspace、停 timer、停 hover、停 minor-mode。"
  (when (bound-and-true-p lsp-mode)
    (lsp-disconnect)
    (dolist (ws (lsp-workspaces)) (lsp-workspace-shutdown ws))
    (my/lsp-mode-disable-hover)
    (lsp-mode -1)))

(defun my/cleanup-lsp-bridge ()
  "彻底停用 lsp-bridge 及其进程。"
  (when (fboundp 'lsp-bridge-stop-process) (lsp-bridge-stop-process))
  (when (bound-and-true-p lsp-bridge-mode)  (lsp-bridge-mode -1))
  ;; 确保疑似残留的 Eldoc/hover 都关掉
  (my/lsp-mode-disable-hover))


;; ———— 切换主函数 ————

(defun my/toggle-lsp-backend ()
  "在当前缓冲区切换 LSP 后端并可选地重启（重载） buffer。

- 若激活 lsp-bridge，则先清理它，启动 lsp-mode + hover  
- 若激活 lsp-mode，则先清理它，启动 lsp-bridge（禁用 hover）  
- 若都没启，则启动 lsp-mode + hover

切换完成后，询问是否重载当前 buffer。重载会重新走一次 find-file/prog-mode 钩子，
/// 有助于确认 LSP 后端切换是否生效。"
  (interactive)
  ;; 确保已加载 init-lsp-bridge
  (unless (fboundp 'lsp-bridge-mode)
    (require 'init-lsp-bridge))

  (cond
   ;; —— 从 lsp-bridge 切到 lsp-mode —— 
   ((bound-and-true-p lsp-bridge-mode)
    (my/cleanup-lsp-bridge)
    (setq lsp-eldoc-enable-hover t)
    (when (fboundp 'lsp-eldoc-mode) (lsp-eldoc-mode 1))
    (require 'init-lsp-mode)
    (lsp)
    (message "✅ 切换到 lsp-mode，并已启用 hover"))

   ;; —— 从 lsp-mode 切到 lsp-bridge —— 
	 ((bound-and-true-p lsp-mode)
	 (my/cleanup-lsp-mode)
	 (setq lsp-eldoc-enable-hover nil)
	 (when (fboundp 'lsp-eldoc-mode) (lsp-eldoc-mode -1))
	 (require 'init-lsp-bridge)
	 (lsp-bridge-mode 1)
	 (message "✅ 切换到 lsp-bridge，并已禁用 hover")
	 ;; 延迟1秒刷新 spaceline，确保 diagnostic 正确载入
	 ;; (run-at-time "1 sec" nil (lambda ()
	 ;; 														(spaceline-refresh)
	 ;; 														(force-mode-line-update)))
	 )

   ;; —— 默认：启动 lsp-mode + hover —— 
   (t
    (require 'init-lsp-mode)
    (setq lsp-eldoc-enable-hover t)
    (when (fboundp 'lsp-eldoc-mode) (lsp-eldoc-mode 1))
    (lsp)
    (message "ℹ 未检测到后端，已启动 lsp-mode，并启用 hover"))))


;; 绑定一个快捷键测试
(global-set-key (kbd "C-c l t") #'my/toggle-lsp-backend)


(require 'init-cpp)
(require 'init-asm)

(provide 'init-lsp)
;;; init-lsp.el ends here

