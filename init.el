;;; init.el --- main config

(add-to-list 'load-path
	     (expand-file-name (concat user-emacs-directory "lisp")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-start)
(require 'init-elpa)
(require 'init-package)
(require 'init-keymap)
(require 'init-ui)
(require 'init-const)
(require 'init-lsp)
(require 'init-cpp)


(when (file-exists-p custom-file)
  (load-file custom-file))
