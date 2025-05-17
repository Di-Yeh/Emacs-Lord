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


(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-show-quick-access t))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))



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
(global-set-key [f8] 'neotree-toggle)


(use-package  projectile)



(provide 'init-package)
