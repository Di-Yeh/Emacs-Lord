;;; init-package.el --- setttings for packages

;;; Commentary:
;;; Code:

(use-package restart-emacs)

(use-package drag-stuff
             :bind (("<M-up>" . drag-stuff-up)
                    ("<M-down>" . drag-stuff-down)))

(use-package ivy :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))
(ivy-mode 1)

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-isearch-backward))
 :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c f" . counsel-recentf)
	 ("C-c g" . counsel-git)))


(use-package which-key
  :defer nil
  :config (which-key-mode))

(use-package ivy-posframe
  :init (setq ivy-posframe-display-functions-alist
			  '((swiper . ivy-posframe-display-at-frame-center)
				(conmplete-symbol . ivy-posframe-display-at-point)
				(counsel-M-x . ivy-posframe-display-at-frame-center)
				(counsel-find-file . ivy-posframe-display-at-frame-center)
				(ivy-switch-buffer . ivy-posframe-display-at-frame-center)
				(t . ivy-posframe-display-at-frame-center)))
                (ivy-posframe-mode 1))

(use-package ace-window 
             :bind (("M-o" . 'ace-window)))

(use-package neotree)
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree) 
;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;; -*- lexical-binding: t -*-
(use-package dirvish
  :ensure t
  :defer nil  ;; 确保启动时加载
  :config
  ;; 指定 fd 的路径（不依赖于系统 PATH），确保 fd.exe 放到 site-lisp/fd 路径下
  (setq dirvish-fd-binary (expand-file-name "site-lisp/fd" user-emacs-directory))
  ;; 使用 Dirvish 内置的 icons 主题，这里的 'icons 会让 Dirvish 显示它自带的图标和风格，
  ;; 与当前 Emacs 主题无关（即按 Dirvish 自己的预设样式呈现）
  (setq dirvish-theme 'icons)
  ;; 是否显示隐藏文件：根据需要调整；此处设为 nil 表示默认不显示隐藏文件
  (setq dirvish-show-hidden-files nil)
  ;; 设置 Dirvish 用于缓存持久数据（例如图片缓存、属性缓存）的目录；
  ;; 这个目录建议单独分离出来，便于 Dirvish 管理
  (setq dirvish-cache-dir (expand-file-name "dirvish-cache" user-emacs-directory))
  ;; 根据 CUSTOMIZING.org 建议，进一步定制属性显示（推荐使用 dirvish-attributes）：
  ;; 设置文件列表中显示哪些文件详情，可以根据需要开启或关闭细节信息
  ;; 例如，如果你希望一开始就显示文件的大小、日期等详细信息：
  (setq dirvish-hide-details nil)
  ;; 其他可选项：
  ;; 你可以根据文档配置 dired-listing-switches（推荐使用 GNU ls 的长参数），
  ;; 或者在 Windows 下保持默认。如需调整, 参考 CUSTOMIZING.org 中关于 ls 选项的说明。
  ;; (setq insert-directory-program "gls") ; 如果你在 macOS 或 Linux 下使用 GNU ls
  )


(use-package  projectile)


(use-package highlight-parentheses
  :ensure t)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)


(use-package indent-bars
	:custom
		(indent-bars-treesit-support t)
    (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))

		(indent-bars-no-descend-lists t)
	  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				       if_statement with_statement while_statement)))
 	  (indent-bars-treesit-ignore-blank-lines-types '("module"))

		(indent-bars-treesit-wrap '((lua
                             expression_list function_declaration if_statement
                             elseif_statement else_statement while_statement for_statement
                             repeat_statement comment)))

		(indent-bars-treesit-wrap '((rust arguments parameters)))
		(indent-bars-treesit-scope '((rust trait_item impl_item 
                                   macro_definition macro_invocation 
                                   struct_item enum_item mod_item 
                                   const_item let_declaration 
                                   function_item for_expression 
                                   if_expression loop_expression 
                                   while_expression match_expression 
                                   match_arm call_expression 
                                   token_tree token_tree_pattern 
                                   token_repetition)))
		
		:hook ((prog-mode) . indent-bars-mode))

;(setq-default indent-tabs-mode nil)  ; 使用空格缩进
;(setq-default tab-width 4)           ; tab = 4 空格

(setq
    indent-bars-no-descend-lists t
    indent-bars-color '(highlight :face-bg t :blend 0.7)
    indent-bars-pattern "|"
    indent-bars-width-frac 0.25
		indent-bars-highlight-current-depth '(:color "red" :blend 0.7)
		indent-bars-starting-column 0
  )

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq-local 
			indent-tabs-mode t ; make sure tabs-based indenting is on, even if we disable it globally
			indent-bars-no-descend-lists nil) ; elisp is mostly continued lists!  allow bars to descend inside
	    (indent-bars-mode 1)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq-local 
			indent-tabs-mode t ; make sure tabs-based indenting is on, even if we disable it globally
			indent-bars-no-descend-lists nil) ; elisp is mostly continued lists!  allow bars to descend inside
	    (indent-bars-mode 1)))


(use-package all-the-icons
  :ensure t
  :config
  ;; 注意：安装完成后，请执行命令 M-x all-the-icons-install-fonts，
  ;; 这将会自动下载并安装 all-the-icons 所需字体，如 Font Awesome、Material Design Icons 等，
  ;; 否则图标可能显示为乱码或问号。
  (unless (file-exists-p (expand-file-name "fonts/all-the-icons.ttf" user-emacs-directory))
    (message "请运行 M-x all-the-icons-install-fonts 安装 all-the-icons 字体。")))




(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package treemacs
  :ensure t)

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))


;; 安装 meow
(use-package meow
  :ensure t)


;; Org mode 基础配置
(use-package org
  :ensure t                     				;; org 是 Emacs 自带的，不用重新安装
  :hook ((org-mode . visual-line-mode))  ;; 自动换行更美观
         ;;(org-mode . org-indent-mode))  ;; 缩进模式
  :config
  ;; 基本外观设置
  (setq org-hide-emphasis-markers t     ;; 隐藏加粗/斜体等标记符号
        org-startup-indented t          ;; 启动时自动缩进
        org-ellipsis " ▼ "              ;; 折叠符号
        org-pretty-entities t)          ;; 显示数学符号等更漂亮

  ;; 绑定快捷键（可选）
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda))






(provide 'init-package)
