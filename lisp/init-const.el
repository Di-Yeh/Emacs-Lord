(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

(when *is-linux*
  ;; 静默 native-comp 的警告，不弹出来干扰界面
  (setq native-comp-async-report-warnings-errors 'quiet))


(defun my/toggle-native-warnings ()
  "Toggle display of native conmpilation warnings."
  (interactive)
  (if (eq native-comp-async-report-warnings-errors 'quiet)
      (progn
        (setq native-comp-async-report-warnings-errors t)
        (message "Native compilation warnings ENABLED"))
    (setq native-comp-async-report-warnings-errors 'quiet)
    (message "Native compilation warnings QUIET")))




(provide 'init-const)
