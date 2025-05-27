# 我的 Emacs Lord 配置👑

## 展示图✨
![1](https://github.com/user-attachments/assets/35cfa3d2-6c7b-4d06-8300-943e0011f137)

![2](https://github.com/user-attachments/assets/79ea4ae7-ad33-4dbd-97a7-616ea2b640dd)

![3](https://github.com/user-attachments/assets/8bcfb61a-91a0-4063-8d6d-274172e57af9)


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

### lsp-booster配置🛠️
1. 在"site-lisp"的文件目录运行这行命令`git clone https://github.com/blahgeek/emacs-lsp-booster.git`（前提是要先安装git）
2. 安装rustup.exe [Rust toolchain](https://www.rust-lang.org/tools/install)
3. 运行`cargo install emacs-lsp-booster`这段代码即可






