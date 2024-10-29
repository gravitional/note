# oh-my-zsh

[Oh My Zsh 插件](https://github.com/ohmyzsh/ohmyzsh/wiki/Plugins)
[Mac/Linux使用ZSH (oh-my-zsh)](https://www.jianshu.com/p/fa6aa9329be6)
[oh-my-zsh国内镜像安装和更新方法](https://www.jianshu.com/p/6b47198fd430)
[Gitee 极速下载](https://gitee.com/mirrors): `Gitee 极速下载` 是为了提升国内下载速度的镜像仓库, 每日同步一次.

## 从gitee安装的方式

[Gitee极速下载/oh-my-zsh的评论](https://gitee.com/colo9)
通过修改环境变量, 实现便捷安装 oh my zsh

```bash
REMOTE=https://gitee.com/mirrors/oh-my-zsh.git
sh -c "$(curl -fsSL https://gitee.com/mirrors/oh-my-zsh/raw/master/tools/install.sh)"
```

## 手动修改内嵌地址安装

`oh-my-zsh`的 github 地址在国内可能用不了, 可以考虑使用`gitee`的镜像.

首先需要安装`zsh`, 可以直接用 `sudo apt install zsh`.
然后安装`oh-my-zsh`, 下载[码云安装脚本install.sh](https://gitee.com/mirrors/oh-my-zsh)

```bash
wget https://gitee.com/mirrors/oh-my-zsh/raw/master/tools/install.sh
```

编辑 `install.sh`, 找到以下部分

```bash
# Default settings
ZSH=${ZSH:-~/.oh-my-zsh}
REPO=${REPO:-ohmyzsh/ohmyzsh}
REMOTE=${REMOTE:-https://github.com/${REPO}.git}
BRANCH=${BRANCH:-master}
```

把其中的

```bash
REPO=${REPO:-ohmyzsh/ohmyzsh}
REMOTE=${REMOTE:-https://github.com/${REPO}.git}
```

替换为

```bash
REPO=${REPO:-mirrors/oh-my-zsh}
REMOTE=${REMOTE:-https://gitee.com/${REPO}.git}
```

编辑后保存, 运行安装即可. (运行前先给`install.sh`权限, `chmod +x install.sh`).
安装完成后修改同步的`仓库地址`

```bash
cd ~/.oh-my-zsh
git remote set-url origin https://gitee.com/mirrors/oh-my-zsh.git
git pull
```

## [zsh-autosuggestions](https://gitee.com/graviton/zsh-autosuggestions.git)

见 INSTALL.md.

将仓库克隆到 `$ZSH_CUSTOM/plugins (by default ~/.oh-my-zsh/custom/plugins)`,
使用 国内镜像代替

```bash
git clone https://gitee.com/graviton/zsh-autosuggestions.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
# git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
```

添加到 `~/.zshrc` oh-my-zsh 插件启动项

```bash
plugins=(
    # other plugins...
    zsh-autosuggestions
)
```

开启新的 terminal session.

## Powerline 状态条

`Powerline` 是一个极棒的 `Vim` 编辑器的状态行插件,这个插件是使用 Python 开发的,
主要用于显示状态行和提示信息,适用于很多软件,比如 `bash`, `zsh`, `tmux` 等等.
[使用Powerline为VIM和Bash注入强劲动力](https://linux.cn/article-8118-1.html)

首次安装`pip`,即python包管理器,在 Debian, Ubuntu 和 Linux Mint 中安装 `pip`

```bash
sudo apt-get install python3-pip
```

然后你可以通过 pip 命令安装 `Powerline`.

```bash
pip3 install git+git://github.com/powerline/powerline
```

## 安装 Powerline 的字体

`Powerline` 使用特殊的符号来为开发者显示特殊的箭头效果和符号内容.
因此你的系统中必须要有符号字体或者补丁过的字体.
通过下面的 `wget` 命令下载最新的系统字体及字体配置文件.

```bash
wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf
wget https://github.com/powerline/powerline/raw/develop/font/10-powerline-symbols.conf
```

然后你将下载的字体放到字体目录下 `/usr/share/fonts` 或者 `/usr/local/share/fonts`,
或者你可以通过 `xset q` 命令找到一个有效的字体目录.

```bash
mv PowerlineSymbols.otf /usr/share/fonts/
```

接下来你需要通过如下命令更新你系统的字体缓存.

```bash
fc-cache -vf /usr/share/fonts/
```

其次安装字体配置文件.

```bash
mv 10-powerline-symbols.conf /etc/fonts/conf.d/
```

## Vim 中的 Powerline

首先通过如下命令获取 `powerline` 的安装位置.

```bash
pip3 show powerline-status
```

在 `~/.vimrc` 中添加如下内容打开该插件(译注: 注意同样需要根据你的系统情况**修改路径**).

```bash
set rtp+=/home/tom/.local/lib/python3.6/site-packages/powerline/bindings/vim/
set laststatus=2
set t_Co=256
```

然后你打开 `vim` 后会看到一个新的状态行

## bash 中的 Powerline

如果希望在 `bash shell` 中默认打开 `Powerline`,可以在 `~/.bashrc` 中添加如下内容.

首先通过如下命令获取 powerline 的安装位置.

```bash
pip3 show powerline-status
...
Location: XXXXX
...
```

一旦找到 `powerline` 的具体位置后,根据你系统的情况替换到下列行中的 `XXXXX` 对应的位置.

```bash
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
source XXXXX/powerline/bindings/bash/powerline.sh
```

然后退出后重新登录,现在 `powerline` 的状态行应该如下显示了.
