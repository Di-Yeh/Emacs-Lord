;;; init-elpa.el --- 混合使用 package.el 和 straight.el -*- lexical-binding: t; -*-
;; ====================================================================
;; 一、package.el 初始化（响应 :ensure t）
;; ====================================================================
(setq package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-check-signature nil)

(require 'package)
(unless package--initialized
  (package-initialize))
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; ====================================================================
;; 二、bootstrap straight.el（响应 :straight t）
;; ====================================================================
(defvar bootstrap-version)
(let* ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el"
                          user-emacs-directory))
       (bootstrap-url
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously bootstrap-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 把 use-package 本身交给 straight 管理
(straight-use-package 'use-package)
;; **不**让 use-package 全局都走 straight
(setq straight-use-package-by-default nil)


;; ====================================================================
;; 三、use-package 基础配置
;; ====================================================================
(require 'use-package)
;; 让 “没写 :straight” 的 use-package 都默认 :ensure t
(setq use-package-always-ensure    t
      use-package-always-defer     t
      use-package-expand-minimally t
      use-package-verbose          t)

;; 例子：
;; (use-package magit)           ; 会自动用 package.el 安装（等同于 :ensure t）
;; (use-package some-lib
;;   :straight t)                ; 会用 straight.el clone & build
;; (use-package built-in-lib
;;   :straight nil :ensure nil)  ; 内置包，不安装也不报错

(provide 'init-elpa)
;;; init-elpa.el ends here
