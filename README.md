
# 🌟 我的 Emacs 配置说明书 🌀

> ⚙️ 本配置适用于 Windows 和 Linux（Debian 系列），含语言支持、图形界面优化、开发工具链等完整说明。

---

## 🎬 目录导航
- [🌠 展示图](#展示图)
- [🖋️ 字体配置](#字体配置)
- [📦 安装说明](#安装说明)
  - [🪟 Windows 环境](#windows环境)
  - [🐧 Linux 环境](#linux环境)

---

## 🌠 展示图

![img1](https://github.com/user-attachments/assets/10663792-8df5-402c-bcf8-eeda822d6694)  
![img2](https://github.com/user-attachments/assets/3b064db0-5b01-40b3-bd6c-2731deb98d4c)  
![img3](https://github.com/user-attachments/assets/481976a8-e57f-4d96-8da8-8e0252a40f04)  
![img4](https://github.com/user-attachments/assets/784551fe-dd28-4874-9ff5-c6f1ff3968ab)

---

## 🖋️ 字体配置

### 💻 Windows 系统
将 `fonts/` 目录下所有字体复制到  
📁 `C:\Windows\Fonts\` 即可。

---

### 🐧 Linux 系统
执行以下命令：
```bash
mkdir -p ~/.fonts
cp *.ttf ~/.fonts/   # 或者拷贝单个字体
fc-cache -fv
```

---

## 📦 安装说明

### 🪟 Windows 环境

#### 📥 Emacs 安装
[GNU 官方下载](https://ftp.gnu.org/gnu/emacs/windows/)｜[镜像站点](https://mirror.ossplanet.net/gnu/emacs/windows/)

#### ⚙️ 开发环境依赖
| 工具名称 | 下载链接 |
|---------|----------|
| Git | [🔗](https://git-scm.com/downloads/win) |
| Clang | [🔗](https://releases.llvm.org/download.html) |
| SBCL | [🔗](https://www.sbcl.org/platform-table.html) |
| Racket | [🔗](https://download.racket-lang.org/) |
| CMake | [🔗](https://cmake.org/download/) |
| Python | [🔗](https://www.python.org/downloads/) |
| NodeJS | [🔗](https://nodejs.org/zh-cn/download) |
| Lua | [🔗](https://luabinaries.sourceforge.net/download.html) |
| MASM | [🔗](https://www.masm32.com/download.htm) |
| MSYS2 | [🔗](https://www.msys2.org/) 或 [Mingw](https://sourceforge.net/projects/mingw/) |

---

#### 🧬 Clojure 配置
- [Clojure 安装包](https://github.com/casselc/clj-msi)  
- [OpenJDK](https://adoptium.net/zh-CN)  
- [clj-kondo](https://github.com/clj-kondo/clj-kondo)

---

#### 🖼️ Emacs Application Framework（EAF）
```bash
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git site-lisp/
pip install PyQt6 pyqt6-webengine sexpdata epc fitz frontend
cd site-lisp/emacs-application-framework
chmod +x install-eaf.py
./install-eaf.py
```

---

#### 🌉 LSP-Bridge
```bash
pip install epc orjson sexpdata six paramiko watchdog pyright setuptools
```

---

#### 🗂️ Dirvish 文件浏览器
```bash
winget install sharkdp.fd
pacman -S mingw-w64-ucrt-x86_64-libvips  # 透过 MSYS2
```

或者下载并加入系统变量：  
[下载 fd](https://github.com/sharkdp/fd/releases)

---

#### 📑 Pandoc
[🔗 安装地址](https://github.com/jgm/pandoc/releases)

---

### 🐧 Linux 环境（适用：Debian、Mint、Ubuntu、Kali）

#### ⚙️ 基础工具安装
```bash
sudo apt install git clang clangd clang-format gcc g++ sbcl racket cmake
```

#### 🐍 Python（使用 pyenv 安装推荐）
```bash
sudo apt install build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl git libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev

curl https://pyenv.run | bash

# 添加到 ~/.bashrc 或 ~/.zshrc
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# 安装并使用 Python
pyenv install 3.11.9
pyenv global 3.11.9
```

---

#### 🌐 Node.js & NPM
```bash
sudo apt install nodejs npm
```

---

#### 🌙 Lua / NASM / vterm
```bash
sudo apt install lua5.3 nasm libvterm-dev cmake make gcc
```

---

#### ☯️ Clojure 配置（Linux）
```bash
sudo apt install openjdk-17-jdk clojure leiningen

curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
chmod +x install-clj-kondo
./install-clj-kondo
```

---

#### 🖼️ Emacs-Application-Framework（EAF）
```bash
cd site-lisp/
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git
pip install PyQt6 pyqt6-webengine sexpdata epc fitz frontend
chmod +x ./install-eaf.py && ./install-eaf.py
```

---

#### 🌉 LSP-Bridge（Linux）
```bash
pip install epc orjson sexpdata six paramiko watchdog pyright setuptools
```

---

#### 🗂️ Dirvish 配置（Linux）
```bash
sudo apt install fd-find libvips-tools ffmpegthumbnailer imagemagick poppler-utils
```

---

#### 📑 Pandoc 安装（可选）
[🔗 下载 Pandoc](https://github.com/jgm/pandoc/releases)
