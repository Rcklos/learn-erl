## 写在开头

这个是在wsl上基于Neovim搭建的Erlang开发环境，记录下过程。

### 先看看版本

wsl及linux子系统发行版本： 

![wsl2](http://cdn.lentme.cn/202303040104951.png)

### 更新apt源

对于初使用WSL的同学， 

```bash
# 遇事不决，先换个apt源并更新下工具
cd /etc/apt
sudo cp sources.list sources.list.bak
sudo vi sources.list
```

打开`sources.list`后直接用以下内容覆盖一下即可

```bash
# 默认注释了源码镜像以提高 apt update 速度，如有需要可自行取消注释
deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy main restricted universe multiverse
# deb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy main restricted universe multiverse
deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-updates main restricted universe multiverse
# deb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-updates main restricted universe multiverse
deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-backports main restricted universe multiverse
# deb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-backports main restricted universe multiverse

# deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-security main restricted universe multiverse
# # deb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-security main restricted universe multiverse

deb http://security.ubuntu.com/ubuntu/ jammy-security main restricted universe multiverse
# deb-src http://security.ubuntu.com/ubuntu/ jammy-security main restricted universe multiverse

# 预发布软件源，不建议启用
# deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-proposed main restricted universe multiverse
# # deb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ jammy-proposed main restricted universe multiverse
```

更新apt：

```bash
sudo apt update
```

可以更新或安装一下必要工具包，以免在后续步骤中出现`Command Not Found`

```bash
# 包含了各种工具包，如GCC之类的，有特定版本需求的话就自己定制安装
sudo apt install build-essential
```

## 安装Neovim

```bash
# stable版本可能较低，直接用最新版
sudo apt-add-repository ppa:neovim-ppa/unstable
# 更新apt
sudo apt-get update
# 安装Neovim
sudo apt-get install neovim
```

安装完成后可以直接使用我Neovim配置

```
# 创建.config目录
mkdir -p .config
# 拉取配置
git clone https://github.com/Rcklos/avimitin_nvim.git ~/.config/nvim --branch erlang
# 拉取配置后第一次打开nvim随便退出，需要一段时间来下载一些东西
nvim
```

第一次打开左下角会弹出这句话，这时候可以先去冲杯茶。

![第一次打开](http://cdn.lentme.cn/202303040047707.png)

卡得太久的话可以`Ctrl + c`，然后输入`:qa!`，再重新进一下，我的电脑一般会在二十秒内弹出以下界面

![安装界面](http://cdn.lentme.cn/202303040052520.png)

安装完成后，按一下`q`即可关闭弹窗，然后`:qa`退出Neovim再重新打开一下，这时候Neovim还不能编辑erlang，还需要接着后续安装一些软件。

## 安装依赖软件

### 语法解析库

tree-sitter虽然有nvim的插件，但可以和CLI配套使用

```bash
# 下载
wget https://github.com/tree-sitter/tree-sitter/releases/download/v0.20.7/tree-sitter-linux-x64.gz

# 解压
gzip -d tree-sitter-linux-x64.gz

# 更名并移动到本地目录
mkdir -p ~/.local/bin
chmod +x tree-sitter-linux-x64 && mv tree-sitter-linux-x64 ~/.local/bin/tree-sitter

# 添加以下内容到~/.bashrc
# export PATH=$PAHT:$HOME/.local/bin
# 刷新bash
source ~/.bashrc
```

### 安装Node环境

我使用的Neovim有很多插件是依赖Node环境的，所以也要配置以下

```bash
# 下载安装
wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash

# 刷新配置
source ~/.bashrc

# 安装node
nvm install 16 --lts

# 使用node版本
nvm use 16 --lts

# 查看node版本
node -v
```

### lua lsp

Neovim的配置是纯`lua`的，所以安装以下lua的lsp是有必要的。

(当然，一直git pull我的配置也可以一直用于开发erlang，我有空会进行相关维护)

安装步骤：

```bash
# 下载release包
wget https://github.com/LuaLS/lua-language-server/releases/download/3.6.13/lua-language-server-3.6.13-linux-x64.tar.gz

# 解压安装
mkdir -p ~/.local/lua-language-server
tar -zxvf lua-language-server-3.6.13-linux-x64.tar -C ~/.local/lua-language-server
rm lua-language-server-3.6.13-linux-x64.tar
# 添加以下内容到~/.bashrc
# export PATH=$PATH:$HOME/.local/lua-language-server/bin
# 刷新bash
source ~/.bashrc
```

### erlang lsp

这时候可以安装以下erlang的lsp，我使用的是`erlang_ls`。

在安装`erlang_ls`之前要确保拥有`Erlang/OTP`和`rebar3`的环境：

asdf可以用来管理`Erlang/OTP`版本，可以针对整个全局或项目来针对性配置erlang版本，所以这里就不选用`apt install erlang`来进行安装了，而是用asdf来管理。

```bash
# 安装asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.11.2

# 在~/.bashrc加入以下内容
# . "$HOME/.asdf/asdf.sh"

# 刷新一下bash
source ~/.bashrc

# 安装asdf-erlang前所需的依赖全部装上
sudo apt-get -y install build-essential autoconf m4 libncurses5-dev libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev libgl1-mesa-dev libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop libxml2-utils libncurses-dev openjdk-11-jdk

# 为asdf添加erlang插件
asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git 

# 查看一下asdf的插件
asdf plugin list # 如果输出有erlang，则已经安装成功 

# 安装最新版本的erlang
asdf install erlang latest

# 配置erlang
asdf global erlang latest

# 第一次运行erl可能需要下面这条命令
asdf exec erl

# 后续可以用erl直接运行
```

现在开始安装rebar3，这个就比较简单了

```bash
# 下载rebar3
wget https://github.com/erlang/rebar3/releases/download/3.20.0/rebar3 && chmod +x rebar3

# 在~/.bashrc添加以下内容
# export PATH=$HOME/.cache/rebar3/bin:$PATH
# 刷新bash
source ~/.bashrc
```

终于到了安装`erlang_ls`的时候了

```bash
# 新建个目录，用于管理erlang_ls
mkdir -p ~/.local/erlang_ls

# 下载erlang_ls源码
git clone https://github.com/erlang-ls/erlang_ls.git

# 编译
make

# 编译安装
PREFIX=~/.local/erlang_ls make install

# 在~/.bashrc添加以下内容
# export PATH=$PATH:$HOME/.local/erlang_ls/bin
# 刷新bash
source ~/.bashrc

# 可以删除erlang_ls源码，当然也可以保留
# rm -rf erlang_ls
```

经历了这么多，现在Neovim已经可以流畅编程erlang了，让我们来验证一下

```bash
mkdir hello_erlang
cd hello_erlang

# 下载erlang.mk
wget https://erlang.mk/erlang.mk

# 生成项目
make -f erlang.mk bootstrap bootstrap-rel

nvim src/my_module.erl
```

编辑以下内容

```erlang
-module(my_modle).
-export(hello/0).

hello() ->
    io:fwrite('hello! I\'m rcklos.~n').
```

然后再编辑`hello_erlang_app.erl`，就有代码提示啦。

![nvim代码提示](http://cdn.lentme.cn/202303041337778.png)