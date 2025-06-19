# 我的Emacs配置 🌀

## 预览🔍
* [展示图](#展示图)
* [字体](#字体)
* [安装](#安装)
  * [Windows](#Windows)
  * [Linux](#Linux)
****

## 展示图✨

![1](https://github.com/user-attachments/assets/5077dd1c-c452-44a4-b8d3-23871d30e1a3)
****
![2](https://github.com/user-attachments/assets/adc953af-35c8-45f4-9c0b-b4f79d72fd3f)
****
![3](https://github.com/user-attachments/assets/83e40f7c-9f80-450f-b07f-0e4bbbbda241)
****
![4](https://github.com/user-attachments/assets/ccc7c4e6-8330-455a-89a3-16f46886c3f8)
****

## 字体✒️
下载文件之后进入fonts目录里面，把字体下载好再进行配置

**Windows系统**

把`fonts/`目录下的所有字体移动到`C:\Windows\Fonts\`目录下即可
****

**Linux系统**

在`fonts/`目录下执行以下步骤：
1. `mkdir -p ~/.fonts`
2. `cp /path/to/your/font.ttf ~/.fonts/` 或者复制多个.ttf或.otf文件 `cp *.ttf ~/.fonts/`
3. `fc-cache -fv`
****

## 安装🏗️

根据自己的操作系统选择对应的配置流程
****

### Windows🪟
**环境配置🎁**

**git** [下载链接](https://git-scm.com/downloads/win)

**clang** [下载链接](https://releases.llvm.org/download.html)

**sbcl** [下载链接](https://www.sbcl.org/platform-table.html)

**msys2** [下载链接](https://www.msys2.org/) 或 **Mingw** [下载链接](https://sourceforge.net/projects/mingw/) 择一

**python** [下载链接](https://www.python.org/downloads/)

**nodejs** [下载链接](https://nodejs.org/zh-cn/download)

**masm** [下载链接](https://www.masm32.com/download.htm)

**emacs-application-framework配置📦**

1. 在`site-lisp/`文件目录运行这行命令`git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git`
2. 安装python (python 3.12版本)
3. 使用`pip`安装以下依赖库
```bash
pip3 install PyQt6 pyqt6-webengine sexpdata epc fitz frontend
```
4. 在`site-lisp/emacs-application-framework/`文件目录中打开`git bash`运行以下指令
```bash
chmod +x ./install-eaf.py
./install-eaf.py
```
5. 选择要安装的eaf功能即可

**lsp-bridge配置🌉**
1. 安装python (python 3.12版本)
2. 使用`pip`安装以下依赖库
```bash
pip install epc orjson sexpdata six paramiko watchdog pyright
```

**dirvish配置🗂️**

使用 **winget** `winget install sharkdp.fd`

或是下载fd.exe [fd](https://github.com/sharkdp/fd/releases)（下载对应操作系统的版本）并且放到环境变量中

**pandoc配置📑**

1. 安装pandoc [下载链接](https://github.com/jgm/pandoc/releases)（下载对应操作系统的版本）并且放到环境变量中
2. 直接安装即可
****

### Linux🐧
**适用操作系统**：Debian Mint Ubuntu Kali

**环境配置🎁**

**git**
```bash
sudo apt install git
```

**clang**
```bash
sudo apt install clang clang++ clang-format
```

**sbcl**
```bash
sudo apt install sbcl
```

**python**
1. 先执行`sudo apt update`
2. 安装依赖
```bash
sudo apt install -y make build-essential libssl-dev zlib1g-dev \
  libbz2-dev libreadline-dev libsqlite3-dev curl git libncursesw5-dev \
  xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev
```
3. 安装 pyenv（用 curl 或 git）
```bash
curl https://pyenv.run | bash
```
4. 将以下内容填写在`~/.bashrc`或`~/.zshrc`文件
```bash
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
```
5. 重新打开终端，或者执行`source ~/.bashrc` 或 `source ~/.zshrc`
6. 安装python（python 3.11.9）
```bash
pyenv install 3.11.9
```
7. 设置全局使用
```bash
pyenv global 3.11.9
```
测试结果✅
```bash
python --version      # 输出 Python 3.11.9
python3 --version     # 也输出 Python 3.11.9
which python3         # 应该是 ~/.pyenv/shims/python3
```

**nodejs**
```bash
sudo apt install nodejs
```

**npm**
```bash
sudo apt install npm
```

**nasm**
```bash
sudo apt install nasm
```

**emacs-application-framework配置📦**

1. 在`site-lisp/`文件目录运行这行命令`git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git`
2. 安装python (python 3.12版本)
3. 使用`pip`安装以下依赖库
```bash
pip3 install PyQt6 pyqt6-webengine sexpdata epc fitz frontend
```
4. 在`site-lisp/emacs-application-framework/`文件目录中执行以下指令
```bash
chmod +x ./install-eaf.py
./install-eaf.py
```
5. 选择要安装的eaf功能即可

**lsp-bridge配置🌉**
1. 安装python (python 3.12版本)
2. 使用`pip`安装以下依赖库
```bash
pip install epc orjson sexpdata six paramiko watchdog pyright
```

**dirvish配置🗂️**

执行这条指令 `sudo apt install fd-find` 

**pandoc配置📑**

安装pandoc [下载链接](https://github.com/jgm/pandoc/releases)（下载对应操作系统的版本）并且放到环境变量中


