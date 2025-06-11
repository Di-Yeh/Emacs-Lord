;;; init-asm.el --- 汇编语言配置 (MASM/NASM) 支持 -*- lexical-binding: t; -*-
;; --------------------------------------------------
;; Emacs 汇编开发环境（适用于 MASM/NASM）
;; 提供语法高亮、补全、snippet、语法检查支持
;; 不使用 LSP，轻量实用
;; --------------------------------------------------

;; 使用 nasm-mode（适用于 MASM/NASM）
(use-package nasm-mode
  :ensure t
  :mode ("\\.\\(asm\\|s\\|S\\)\\'" . nasm-mode))


(setq asm-comment-char ?\#) ; 默认是 `;`，你可以按需设置为 `#` 或 `;`
(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-always-indent 'complete)
            (electric-indent-local-mode -1) ; 防止自动缩进乱跳
            (setq indent-tabs-mode t)
            (setq tab-width 8)))


;; --------------------------------------------------
;; 启用 company-mode 补全（适用于 NASM / MASM）
;; 提供：Intel 指令 + x86/x64 寄存器 + MASM/NASM 宏/伪指令等关键字
;; --------------------------------------------------
(use-package company
  :ensure t
  :hook (nasm-mode . company-mode)
  :config
  ;; 提高补全灵敏度
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)

  ;; 定义 NASM / MASM 补全关键字（指令 + 寄存器 + 伪指令 + 宏）
  (defvar company-nasm-keywords
    (let ((instructions '(
      ;; Intel 汇编指令（通用）
      "mov" "xchg" "lea" "nop" "push" "pop" "pusha" "popa" "pushad" "popad"
      "add" "sub" "imul" "mul" "idiv" "div" "neg" "inc" "dec"
      "adc" "sbb" "cmp" "test"
      ;; 跳转与调用
      "jmp" "je" "jne" "jg" "jge" "jl" "jle" "ja" "jae" "jb" "jbe"
      "jo" "jno" "jp" "jnp" "jz" "jnz" "loop" "loopz" "loope" "loopnz" "loopne"
      "call" "ret" "retn" "retf" "int" "iret"
      ;; 逻辑与位操作
      "and" "or" "xor" "not"
      "shl" "shr" "sar" "sal" "rol" "ror" "rcl" "rcr"
      ;; 字符串指令
      "movsb" "movsw" "movsd" "movsq"
      "stosb" "stosw" "stosd" "stosq"
      "lodsb" "lodsw" "lodsd" "lodsq"
      "scasb" "scasw" "scasd" "scasq"
      "cmpsb" "cmpsw" "cmpsd" "cmpsq"
      ;; 标志与控制
      "pushf" "popf" "lahf" "sahf" "stc" "clc" "cmc"
      "std" "cld" "sti" "cli"
      ;; 浮点指令（FPU）
      "fld" "fst" "fstp" "fadd" "fsub" "fmul" "fdiv"
      "fchs" "fabs" "fsqrt" "fxch" "fcomp" "fwait"
      ;; SSE / AVX（常见）
      "movaps" "movups" "addps" "subps" "mulps" "divps" "sqrtps"
      ;; I/O / 系统
      "in" "out" "insb" "outsb" "hlt" "xlat" "cpuid" "rdtsc"))

          (registers '(
      ;; x86 16/32 位寄存器
      "eax" "ebx" "ecx" "edx" "esi" "edi" "esp" "ebp"
      "ax" "bx" "cx" "dx" "si" "di" "sp" "bp"
      "al" "ah" "bl" "bh" "cl" "ch" "dl" "dh"
      ;; x64 扩展寄存器
      "rax" "rbx" "rcx" "rdx" "rsi" "rdi" "rsp" "rbp"
      "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15"
      "r8d" "r9d" "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"
      "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"
      "r8b" "r9b" "r10b" "r11b" "r12b" "r13b" "r14b" "r15b"))

          (directives '(
      ;; NASM 伪指令 / 宏
      "section" "segment" "global" "extern" "bits" "org" "align"
      "db" "dw" "dd" "dq" "dt" "resb" "resw" "resd" "resq" "rest"
      "equ" "times" "%define" "%assign" "%ifdef" "%ifndef" "%endif"
      "%macro" "%endmacro" "%include"

      ;; MASM 特有关键字
      ".model" ".stack" ".code" ".data" ".data?" ".const"
      "proc" "endp" "end" "invoke" "assume" "ptr" "offset"
      "dup" "macro" "endif" "if" "else" "elseif"
      "comment" "include" "exitm" "type" "struct" "ends" "union")))

      ;; 合并并去重
      (delete-dups (append instructions registers directives))))
    "汇编语言（NASM/MASM）补全关键字列表。")

  ;; 定义补全后端
  (defun company-nasm-backend (command &optional arg &rest ignored)
    "用于 NASM/MASM 的 company-mode 补全后端。"
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-nasm-backend))
      (prefix (and (eq major-mode 'nasm-mode)
                   (company-grab-symbol)))
      (candidates
       (cl-remove-if-not
        (lambda (c) (string-prefix-p arg c))
        company-nasm-keywords))))

  ;; 注册补全后端（必须在 company 加载后）
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-nasm-backend))



;; 启用 flycheck 检查（需要系统已安装 nasm）
(use-package flycheck
  :ensure t
  :hook (nasm-mode . flycheck-mode)
  :config
  (flycheck-define-checker nasm
    "A syntax checker for NASM (Netwide Assembler)."
    :command ("nasm" "-f" "win64" "-o" "nul" source)
    :error-patterns
    ((error line-start (file-name) ":" line ": error: " (message) line-end)
     (warning line-start (file-name) ":" line ": warning: " (message) line-end))
    :modes nasm-mode)
  (add-to-list 'flycheck-checkers 'nasm))

(provide 'init-asm)
