;;; init-elpa.el --- straight.el + use-package 统一管理 -*- lexical-binding: t; -*-

;; ====================================================================
;; 软件源（供 package.el 及其他老插件参考，如果都不再用 package.el 可删）
;; ====================================================================
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-check-signature nil)  ;; 有时签名验签失败

;; ====================================================================
;; 禁用内置 package.el 在 Emacs 启动时自动加载（early-init.el 中也可放）
;; ====================================================================
(setq package-enable-at-startup nil)

;; ====================================================================
;; 1. Bootstrap straight.el
;; ====================================================================
(defvar bootstrap-version)
(let* ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el"
                          user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ====================================================================
;; 2. 让 straight.el 管理 use-package，并默认用它来安装所有包
;; ====================================================================
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ====================================================================
;; 3. 原有的 use-package 配置
;; ====================================================================
(require 'use-package)
(setq use-package-always-defer    t
      use-package-always-demand   nil
      use-package-expand-minimally t
      use-package-verbose         t)

(provide 'init-elpa)
;;; init-elpa.el ends here
