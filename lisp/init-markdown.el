;; init-markdown.el

;; -------------------------------
;; 安装并加载所需插件
;; -------------------------------

;; 实时预览插件
(use-package impatient-mode
  :ensure t)

;; Markdown 编辑模式配置
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :hook ((markdown-mode . lsp)                ;; 启用 lsp-mode，以便 Marksman 提供 LSP 支持
         (markdown-mode . impatient-mode))    ;; 启动 impatient-mode 实时预览
  :config
  ;; 加载 lsp-marksman 模块为 Markdown 启用 LSP 支持
  (require 'lsp-marksman)
  ;; 如果 Marksman 不在系统 PATH 中，请取消下面行的注释并设置其完整路径
  ;; (setq lsp-marksman-server-path "C:/your/path/to/marksman")
  ;; 避免 keymap 提前加载导致 markdown-mode-map 未定义
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c p") #'my-markdown-preview))
  ;; 指定一个可用的 Markdown 转换命令，此处使用 pandoc（请确保 pandoc 已安装）
  (setq markdown-command "pandoc -f markdown -t html"))

;; -------------------------------
;; 定义 Markdown 转换与预览函数
;; -------------------------------

(defun my-markdown-to-html (markdown-text)
  "使用 `markdown-command` 将 MARKDOWN-TEXT 转换为 HTML。
这里调用的是 shell 命令，因此需要确保 `markdown-command` 变量设置正确。"
  (with-temp-buffer
    (insert markdown-text)
    (if (zerop (shell-command-on-region (point-min)
                                        (point-max)
                                        markdown-command
                                        t t))
        (buffer-string)
      (error "Markdown conversion error"))))

(defun my-markdown-preview ()
  "将当前 Markdown Buffer 转换为 HTML，并通过 impatient-mode 预览。
预览地址通常为 http://localhost:8080/imp/ 。"
  (interactive)
  ;; 如果 HTTP 服务器还未启动，则启动之
  (unless (and (boundp 'httpd-port) httpd-port)
    (httpd-start))
  (let* ((html (my-markdown-to-html (buffer-string)))
         (tmp-buf (get-buffer-create "*imp-markdown-preview*")))
    (with-current-buffer tmp-buf
      (erase-buffer)
      (insert html)
      ;; 将当前缓冲区内容缓存给 impatient-mode 进行预览
      (imp-httpd-make-cached-buffer (current-buffer)))
    (message "Preview available at: http://localhost:8080/imp/")))

(setq markdown-fontify-code-blocks-natively t)  ;; 高亮代码块


(provide 'init-markdown)
