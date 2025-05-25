;; Configuring Emacs as a C/C++ IDE
(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))


;; 自定义 C/C++ 缩进风格为 my-c-style
(c-add-style "my-c-style"
             '("bsd"                          ;; 基于 BSD 风格 ;; 也可以切换为 "k&r", "stroustrup", "java", 等风格
               (c-basic-offset . 4)           ;; 设置缩进为 4 空格
               (indent-tabs-mode . nil)       ;; 使用空格代替 TAB
               (tab-width . 4)))              ;; TAB 宽度设为 4

(defun my-c-c++-mode-hook ()
  (c-set-style "my-c-style")                 ;; 使用自定义风格
  (setq indent-tabs-mode nil)               ;; 强制空格缩进
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq backward-delete-char-untabify-method -1) ;;退格删除
  (electric-indent-local-mode 1)            ;; 保持回车自动缩进开启
  )

;; 把钩子绑定到 C/C++ 模式
(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)


;; g++编译配置
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec . ("%c -std=c++1z %o -o %e %s"
                "%e %a"))
      (:remove . ("%e")))
    :default "c++"))

(global-set-key (kbd "<f5>") 'quickrun)




;; ───────────────────────────────────────────────────────
;; Visual Studio 项目支持：devenv / nmake 构建 & 运行
;; F7 编译，F5 运行（sln 工程）
;; C-F7 编译 Makefile 工程
;; 自动判断 .sln 使用 devenv.com，否则尝试 Makefile 使用 nmake
;; 记得使用 Cross Tools Command Prompt 打开Emacs
;; ───────────────────────────────────────────────────────

(defun my/get-vs-build-options ()
  "交互式选择 Visual Studio 构建配置和平台。默认 Debug | x86。"
  (let ((config (completing-read "配置: " '("Debug" "Release") nil t "Debug"))
        (plat (completing-read "平台: " '("x86" "x64") nil t "x86")))
    (list config plat)))

(defun my/compile-project ()
  "尝试自动查找 .sln 或 Makefile，并调用 devenv.com 或 nmake 进行构建。"
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      (lambda (f)
                                        (or (directory-files f nil "\\.sln$")
                                            (file-exists-p (expand-file-name "Makefile" f))))))
         (sln-file (car (directory-files dir t "\\.sln$")))
         (makefile (expand-file-name "Makefile" dir)))
    (cond
     ;; 使用 devenv.com 编译 .sln
     ((and sln-file (file-exists-p sln-file))
      (let* ((opts (my/get-vs-build-options))
             (conf (nth 0 opts))
             (plat (nth 1 opts)))
        ;; 使用 devenv.com 可确保输出完整信息返回到 shell
        (compile (format "devenv.com \"%s\" /build \"%s|%s\"" sln-file conf plat))))
     ;; 使用 nmake 编译 Makefile
     ((file-exists-p makefile)
      (compile (format "nmake /f \"%s\"" makefile)))
     ;; 无法识别
     (t
      (message "当前目录未找到 .sln 或 Makefile，无法编译。")))))

(global-set-key (kbd "<f7>") #'my/compile-project)

;; ───────────────────────────────
;; C-F7：强制使用 nmake 构建
;; ───────────────────────────────

(defun my/nmake-build ()
  "始终使用 nmake 编译当前目录下的 Makefile。"
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      (lambda (f) (file-exists-p (expand-file-name "Makefile" f)))))
         (makefile (expand-file-name "Makefile" dir)))
    (if (file-exists-p makefile)
        (compile (format "nmake /f \"%s\"" makefile))
      (message "未找到 Makefile"))))

(global-set-key (kbd "<C-f7>") #'my/nmake-build)

;; ───────────────────────────────
;; F5：运行构建后的可执行文件（仅限 .sln 工程）
;; 假设输出路径为 $(Platform)/$(Configuration)/项目名.exe
;; ───────────────────────────────

(defun my/run-vs-exe ()
  "运行 Visual Studio 项目生成的 exe 文件（根据 sln 工程名自动推断）。"
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      (lambda (f) (directory-files f nil "\\.sln$"))))
         (sln-file (car (directory-files dir t "\\.sln$"))))
    (if sln-file
        (let* ((opts (my/get-vs-build-options))
               (conf (nth 0 opts))
               (plat (nth 1 opts))
               ;; 项目名（假设与 sln 文件名一致）
               (exe-name (file-name-base sln-file))
               ;; 假设输出路径为 ./Debug/ProjectName.exe
               (out-dir (expand-file-name (format "%s/%s" plat conf) dir))
               (exe-path (expand-file-name (concat exe-name ".exe") out-dir)))
          (if (file-exists-p exe-path)
              (progn
                (message "运行可执行文件: %s" exe-path)
                (start-process "run-vs-exe" "*run*" exe-path))
            (message "可执行文件不存在: %s" exe-path)))
      (message "当前目录未找到 .sln 工程"))))

(global-set-key (kbd "<C-f5>") #'my/run-vs-exe)







(provide 'init-cpp)
