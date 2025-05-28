;;; init-dap.el --- DAP mode configuration for C/C++ debugging -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; 本文件预先注册了四个调试模板，并定义了交互式命令 `my/dap-debug-choose`：
;;
;; 1. C/C++ Debug  
;;    - 启动本地调试会话（使用 MI 调试器，即 GDB），适合大多数 C/C++ 开发场景。
;;
;; 2. LLDB Run Configuration  
;;    - 利用 LLDB 启动调试会话，适用于 macOS 或偏好 LLDB 的用户。
;;
;; 3. GDBServer Connect Configuration  
;;    - 用于远程调试，附加到远程 gdbserver 进程，需要输入远程主机地址与端口。
;;
;; 4. GDB Run Configuration  
;;    - 与 C/C++ Debug 类似，但用于需要额外 GDB 参数的情况。
;;
;; 当你按下快捷键 C-c d d 时，系统将：
;;   - 显示上述四个选项供你选择；
;;   - 对于本地调试模板（C/C++ Debug、LLDB Run Configuration、GDB Run Configuration），依次提示输入目标程序（executable）的路径和工作目录；
;;   - 对于远程调试模板（GDBServer Connect Configuration），提示输入远程主机和端口；
;;   - 如果所选模板使用 GDB，而 MI 调试器路径为空，则提示你输入 GDB 的完整路径，并保存到 "~/.emacs.d/dap-gdb-path.txt"；
;;   - 同时，调试配置中会设置 `:miDebuggerPath` 和 `:dap-server-path` 为该调试器路径，以便正确启动适配器。
;;
;;; Code:

;; 显式加载 dap-mode 及相关模块
(require 'dap-mode)
(require 'dap-ui)
(require 'dap-cpptools)

;; 预先注册调试模板
(dap-register-debug-template "C/C++ Debug"
  (list :type "cppdbg"
        :request "launch"
        :name "C/C++ Debug"
        :miDebuggerPath ""      ;; 后续填充
        :target ""              ;; 待填写目标程序
        :cwd ""                 ;; 待填写工作目录
        :args []
        :stopAtEntry t
        :environment []
        :externalConsole nil
        :MIMode "gdb"))

(dap-register-debug-template "LLDB Run Configuration"
  (list :type "lldb"
        :request "launch"
        :name "LLDB Run Configuration"
        :target ""              ;; 待填写目标程序
        :cwd ""                 ;; 待填写工作目录
        :args []
        :stopAtEntry t
        :environment []))

(dap-register-debug-template "GDBServer Connect Configuration"
  (list :type "cppdbg"
        :request "attach"
        :name "GDBServer Connect Configuration"
        :miDebuggerPath ""      ;; 后续填充
        :host ""                ;; 待填写远程主机地址
        :port 0                ;; 待填写远程调试端口
        :MIMode "gdb"))

(dap-register-debug-template "GDB Run Configuration"
  (list :type "cppdbg"
        :request "launch"
        :name "GDB Run Configuration"
        :miDebuggerPath ""      ;; 后续填充
        :target ""              ;; 待填写目标程序
        :cwd ""                 ;; 待填写工作目录
        :args []
        :stopAtEntry t
        :environment []
        :externalConsole nil
        :MIMode "gdb"))

;; 定义全局变量保存 MI 调试器（GDB）的路径
(defvar my/dap-gdb-path nil
  "保存用于 dap-mode 调试的 MI 调试器 (GDB) 路径。")

(defun my/dap-get-gdb-path ()
  "返回 MI 调试器 GDB 路径；若未设置则提示输入，并保存到 ~/.emacs.d/dap-gdb-path.txt。"
  (if (and my/dap-gdb-path (not (string-empty-p my/dap-gdb-path)))
      my/dap-gdb-path
    (let* ((config-file (expand-file-name "dap-gdb-path.txt" user-emacs-directory))
           (gdb-path (if (file-exists-p config-file)
                         (with-temp-buffer
                           (insert-file-contents config-file)
                           (string-trim (buffer-string)))
                       (read-file-name "Enter full GDB executable path: " "C:/" nil t))))
      (with-temp-file config-file
        (insert gdb-path))
      (setq my/dap-gdb-path gdb-path)
      gdb-path)))

;;;###autoload
(defun my/dap-debug-choose ()
  "交互式启动调试会话。
显示四个预注册调试模板供选择：
- 对于本地调试模板（C/C++ Debug、LLDB Run Configuration、GDB Run Configuration），提示输入目标程序路径和工作目录；
- 对于远程调试模板（GDBServer Connect Configuration），提示输入远程主机和端口；
如果所选模板使用 GDB，而 MI 调试器路径为空，则提示输入并保存 GDB 路径。
最终配置传递给 `dap-debug` 启动调试会话。"
  (interactive)
  (let* ((choice (completing-read "Select debug configuration: "
                                  '("C/C++ Debug" "LLDB Run Configuration" "GDBServer Connect Configuration" "GDB Run Configuration")
                                  nil t))
         debug-config)
    (cond
     ((string= choice "GDBServer Connect Configuration")
      (let ((host (read-string "Enter remote host: " "localhost"))
            (port (read-number "Enter remote port: " 1234)))
        (setq debug-config
              (list :type "cppdbg"
                    :request "attach"
                    :name "GDBServer Connect Configuration"
                    :miDebuggerPath ""   ;; 待填充
                    :host host
                    :port port
                    :MIMode "gdb"))))
     (t
      (let* ((default-target (if (and buffer-file-name (file-exists-p buffer-file-name))
                                 (expand-file-name (file-name-nondirectory buffer-file-name))
                               ""))
             (target (read-file-name "Enter target executable: " default-directory default-target t))
             (cwd (read-directory-name "Enter working directory: " default-directory)))
        (setq debug-config
              (cond
               ((string= choice "C/C++ Debug")
                (list :type "cppdbg"
                      :request "launch"
                      :name "C/C++ Debug"
                      :miDebuggerPath ""   ;; 待填充
                      :target target
                      :cwd cwd
                      :args []
                      :stopAtEntry t
                      :environment []
                      :externalConsole nil
                      :MIMode "gdb"))
               ((string= choice "LLDB Run Configuration")
                (list :type "lldb"
                      :request "launch"
                      :name "LLDB Run Configuration"
                      :target target
                      :cwd cwd
                      :args []
                      :stopAtEntry t
                      :environment []))
               ((string= choice "GDB Run Configuration")
                (list :type "cppdbg"
                      :request "launch"
                      :name "GDB Run Configuration"
                      :miDebuggerPath ""   ;; 待填充
                      :target target
                      :cwd cwd
                      :args []
                      :stopAtEntry t
                      :environment []
                      :externalConsole nil
                      :MIMode "gdb"))
               (t (error "Unsupported configuration selected")))))))
    ;; 针对使用 GDB 的模板（包括远程连接），若 :miDebuggerPath 为空，则提示输入并同时设置 :dap-server-path
    (when (member choice '("C/C++ Debug" "GDBServer Connect Configuration" "GDB Run Configuration"))
      (let ((gdb (my/dap-get-gdb-path)))
        (setq debug-config (plist-put debug-config :miDebuggerPath gdb))
        (setq debug-config (plist-put debug-config :dap-server-path (list gdb)))))
    (dap-debug debug-config)))

;; 快捷键绑定
(global-set-key (kbd "C-c d d") 'my/dap-debug-choose)
(global-set-key (kbd "C-c d b") 'dap-breakpoint-toggle)

(provide 'init-dap)
;;; init-dap.el ends here
