# 我的 Emacs Lord 配置🐦‍⬛

## 展示图✨

![1](https://github.com/user-attachments/assets/d0ffaa1e-ed86-4fd2-a650-182f848f76e7)
****
![2](https://github.com/user-attachments/assets/6d93c041-d754-4d60-8a64-aeeccc256624)
****
![3](https://github.com/user-attachments/assets/8160ccf7-dc58-49d4-9696-ee8839be8e28)
****
![4](https://github.com/user-attachments/assets/75f221e8-8ab1-46e2-ae3a-967272c25645)

## 字体配置✒️
下载文件之后进入fonts目录里面，把字体下载好再进行配置

## 安装与配置🏗️

### emacs-application-framework配置📦
1. 在"site-lisp"的文件目录运行这行命令`git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git`（前提是要先安装git）
2. 安装python (这里以python 3.11做演示)
3. 使用`pip`安装以下依赖库
```bash
pip3 install PyQt6
pip3 install pyqt6-webengine 
pip3 install sexpdata
pip3 install epc
pip3 install fitz
pip3 install frontend
  ```
4. 在"site-lisp/emacs-application-framework/"文件目录中运行以下指令
```bash
chmod +x ./install-eaf.py
./install-eaf.py
```

### lsp-bridge配置🌉
1. 安装python (这里以python 3.11做演示)
2. 使用`pip`安装以下依赖库
```bash
pip install epc orjson sexpdata six paramiko watchdog pyright
```
3. 在启动大型项目或文件时会询问python的安装路径（仅限Windows系统），在cmd执行`where python`指令并把安装路径复制到输入框中
4. 启动后查看lsp-bridge的log查看是否能够正常运行

### dirvish配置🗂️

**Linux系统** `apt-get install fd` 或者其他安装包

**Windows系统** `winget install sharkdp.fd` 或是下载fd.exe [fd](https://github.com/sharkdp/fd/releases)（下载对应操作系统的版本）并且放到环境变量中

### pandoc配置📑
1. 安装pandoc [下载链接](https://github.com/jgm/pandoc/releases)（下载对应操作系统的版本）
2. 直接安装即可

## 其它需求🎁
clang [下载链接](https://releases.llvm.org/download.html)

msys2 [下载链接](https://www.msys2.org/)

python [下载链接](https://www.python.org/downloads/)

Mingw [下载链接](https://sourceforge.net/projects/mingw/)

Node js [下载链接](https://nodejs.org/zh-cn/download)




