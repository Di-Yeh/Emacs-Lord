#!/bin/bash
set -e

echo "=== Emacs 开发环境初始化脚本（Ubuntu/Debian/Mint） ==="

# 检查 Emacs 是否安装
if ! command -v emacs &> /dev/null; then
  echo "❌ 未检测到 Emacs，请先手动安装。"
  exit 1
fi

# 检查 .emacs.d 是否存在
if [ ! -d "$HOME/.emacs.d" ]; then
  echo "❌ 未找到 ~/.emacs.d，请先 clone 配置仓库。"
  echo "示例：git clone https://github.com/yourname/your-config ~/.emacs.d"
  exit 1
fi

# Step 1: 安装 APT 依赖包
echo ">>> 安装开发工具和语言支持..."
sudo apt update
sudo apt install -y \
  git gcc g++ clang clang-format clangd \
  sbcl nasm nodejs npm \
  lua5.3 liblua5.3-dev \
  make build-essential libssl-dev zlib1g-dev \
  libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm \
  libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev \
  libffi-dev liblzma-dev cmake fd-find

# Step 2: 设置 lua 命令别名（确保 lua 是 5.3）
if ! command -v lua &> /dev/null || [[ "$(lua -v 2>&1)" != *"5.3"* ]]; then
  echo ">>> 设置 lua -> lua5.3 的软链接"
  sudo ln -sf /usr/bin/lua5.3 /usr/local/bin/lua
  sudo ln -sf /usr/bin/luac5.3 /usr/local/bin/luac
fi

# Step 3: 安装 pyenv（包括多重验证和导出）
echo ">>> 安装 pyenv..."

if [ ! -d "$HOME/.pyenv" ]; then
  curl https://pyenv.run | bash
else
  echo "✔️ 已检测到 ~/.pyenv，跳过重新安装"
fi

# 设置环境变量（自动写入 .bashrc 和临时环境）
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if ! grep -q 'pyenv init' ~/.bashrc; then
  echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc
  echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
  echo 'eval "$(pyenv init -)"' >> ~/.bashrc
  echo 'eval "$(pyenv virtualenv-init -)"' >> ~/.bashrc
  echo ">>> 已将 pyenv 设置添加到 ~/.bashrc"
fi

# 激活 pyenv 环境
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Step 4: 安装 Python 3.11.9
if ! pyenv versions | grep -q "3.11.9"; then
  echo ">>> 安装 Python 3.11.9，请稍等..."
  pyenv install 3.11.9
else
  echo "✔️ Python 3.11.9 已安装"
fi

# 设置全局版本
pyenv global 3.11.9
hash -r
echo ">>> 当前 Python 版本：$(python --version)"

# Step 5: 安装 pip3 依赖
echo ">>> 安装 pip3 依赖库..."
pip3 install --upgrade pip
pip3 install PyQt6 pyqt6-webengine sexpdata epc fitz frontend \
               orjson six paramiko watchdog pyright

echo "✅ 所有步骤完成！"
echo "📌 请重新登录或执行 'source ~/.bashrc' 以确保 pyenv 生效。"
