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
(global-set-key [f8] 'neotree-toggle)

(use-package  projectile)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

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


(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-mode)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

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



(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("p" . markdown-preview-mode)))

;; 记得安装pandoc
;; Windows: https://github.com/jgm/pandoc
;; macOS/Linux:
;; sudo apt install pandoc        # Ubuntu
;; sudo pacman -S pandoc          # Arch
;; brew install pandoc            # macOS



(setq markdown-fontify-code-blocks-natively t)  ;; 高亮代码块










(provide 'init-package)
