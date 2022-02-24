# 常用软件

## Ubuntu Dock

[如何移除或禁用 Ubuntu Dock](https://zhuanlan.zhihu.com/p/48078003)

使用 `Dash to Panel` 扩展:

`Dash to Panel` 是 Gnome Shell 的一个高度可配置面板,是 Ubuntu Dock 或 Dash to Dock 的一个很好的替代品(Ubuntu Dock 是从 Dash to Dock 分叉而来的).
安装和启动 `Dash to Panel` 扩展会禁用 Ubuntu Dock,因此你无需执行其它任何操作.
你可以从 extensions.gnome.org 来安装 [Dash to Panel](https://extensions.gnome.org/extension/1160/dash-to-panel/).

如果你改变主意并希望重新使用 Ubuntu Dock,那么你可以使用 Gnome Tweaks 应用程序禁用 Dash to Panel,或者通过单击以下网址旁边的 X 按钮完全移除 `Dash to Panel`: https://extensions.gnome.org/local/ .

## oh-my-zsh

[Oh My Zsh 插件](https://github.com/ohmyzsh/ohmyzsh/wiki/Plugins)
[Mac/Linux使用ZSH (oh-my-zsh)](https://www.jianshu.com/p/fa6aa9329be6)
[oh-my-zsh国内镜像安装和更新方法](https://www.jianshu.com/p/6b47198fd430)
[ Gitee 极速下载](https://gitee.com/mirrors):`Gitee 极速下载` 是为了提升国内下载速度的镜像仓库, 每日同步一次.

`oh-my-zsh`的 github 地址在国内可能用不了, 可以考虑使用`gitee`的镜像.

首先需要安装`zsh`, 可以直接用`sudo apt install zsh`.  然后安装`oh-my-zsh`, 下载[码云安装脚本install.sh](https://gitee.com/mirrors/oh-my-zsh)

```bash
wget https://gitee.com/mirrors/oh-my-zsh/raw/master/tools/install.sh
```

编辑`install.sh`, 找到以下部分

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

## Powerline 状态条

`Powerline` 是一个极棒的 `Vim` 编辑器的状态行插件,这个插件是使用 Python 开发的,主要用于显示状态行和提示信息,适用于很多软件,比如 `bash`, `zsh`, `tmux` 等等.
[使用Powerline为VIM和Bash注入强劲动力](https://linux.cn/article-8118-1.html)

首次安装`pip`,即python包管理器,在 Debian, Ubuntu 和 Linux Mint 中安装 `pip`

```bash
apt-get install python3-pip
```

然后你可以通过 pip 命令安装 `Powerline`.

```bash
pip3 install git+git://github.com/powerline/powerline
```

### 安装 Powerline 的字体

`Powerline` 使用特殊的符号来为开发者显示特殊的箭头效果和符号内容.因此你的系统中必须要有符号字体或者补丁过的字体.
通过下面的 `wget` 命令下载最新的系统字体及字体配置文件.

```bash
wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf
wget https://github.com/powerline/powerline/raw/develop/font/10-powerline-symbols.conf
```

然后你将下载的字体放到字体目录下 `/usr/share/fonts` 或者 `/usr/local/share/fonts`,或者你可以通过 `xset q` 命令找到一个有效的字体目录.

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

### bash 中的 Powerline

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

### Vim 中的 Powerline

首先通过如下命令获取 `powerline` 的安装位置.

```bash
pip3 show powerline-status
```

在 `~/.vimrc` 中添加如下内容打开该插件(译注: 注意同样需要根据你的系统情况修改路径).

```bash
set rtp+=/home/tom/.local/lib/python3.6/site-packages/powerline/bindings/vim/
set laststatus=2
set t_Co=256
```

然后你打开 `vim` 后会看到一个新的状态行

## tldr

`tldr`(Too Long; Don't Read): 帮你更加高效地学习`linux`命令

```bash
pip3 install --user tldr
```

## X窗口系统

`X`窗口系统(使GUI工作的底层引擎)内建了一种机制,支持快速拷贝和粘贴技巧.

按下鼠标左键,沿着文本拖动鼠标(或者双击一个单词)高亮了一些文本,
那么这些高亮的文本就被拷贝到了一个由X管理的缓冲区里面.然后按下鼠标中键,这些文本就被粘贴到光标所在的位置.

>`ctrl-c` and `ctrl-v`,这两个控制代码对于Shell 有不同的含义,它们在早于Microsoft Windows许多年之前就赋予了不同的意义.

可以把聚焦策略设置为"跟随鼠标",这样鼠标移动到的窗口,就可以接受输入

## 图形界面

[Linux图形界面知识](https://blog.csdn.net/qq_16093937/article/details/83269106)

`X`是协议, 不是具体的某个软件. `XFree86`只是实现`X`协议的一个免费X服务器软件
`X11R6` 是 X Protocol version 11 Release 6,(`X`协议第`11`版第`6`次发行)的意思
窗口管理器的作用是最大化, 最小化, 移动, 关闭窗口等.而这些不是`X`服务器来负责完成的.

`KDE` 和`GNOME`是`Linux`里最常用的图形界面操作环境, 他们不仅仅是一个窗口管理器那么简单,  `KDE`是`K Desktop Environment` 的缩写.
他不仅是一个窗口管理器, 还有很多配套的应用软件和方便使用的桌面环境, 比如任务栏, 开始菜单, 桌面图标等等.
`GNOME`是`GNU Network Object Model Environment` 的缩写.和`KDE`一样, 也是一个功能强大的综合环境.

办公室的电脑在4K分辨率下会卡顿, 2K则正常.

## 幕后控制台

即使仿真终端没有运行,后台仍然有几个终端会话运行.他们叫做虚拟终端或者虚拟控制台.

在大多数Linux发行版中,可以通过按下 `Ctrl+Alt+F1` 到 `Ctrl+Alt+F6` 访问.

切换可以直接按`Alt+F1..F6`.返回图形桌面,按下`Alt+F7`

## 将标准输出重定向到剪贴板

[将标准输出重定向到剪贴板](https://blog.csdn.net/tcliuwenwen/article/details/103752486)

作为一名优秀的程序员,终端和复制粘贴必将是必不可少的,手动将输出复制粘贴不应该是一名优秀程序员的作风.
那么如何将标准输出重定向到剪贴板方便我们粘贴呢?

安装`xsel`或者`xclip`

```bash
sudo apt install xsel
sudo apt install xclip
```

***
将输出通过管道重定向到剪贴板

```bash
ls | xsel -ib  # 使用xsel
ls | xclip -select clip  # 使用xclip
```

***
可以考虑使用别名来简短命令这里不再赘述

## 剪贴板管理程序

[10 款最佳剪贴板管理器](https://www.linuxprobe.com/10-best-linux-clipboard.html)

+ CopyQ
+ GPaste
+ Klipper
+ Clipman
+ Diodon

etc

## Linux 录屏软件

[Linux 录屏软件有哪些](https://www.zhihu.com/question/51920876)

如果是`Gnome3`系用户,可以按`ctrl + shift + alt + r`,屏幕右下角有红点出现,则开始录屏,
要结束的话再按一次`ctrl + shift + alt + r`,录好的视频在`~/video`下

***
修改默认30秒的问题, 改成1小时

```bash
gsettings set org.gnome.settings-daemon.plugins.media-keys max-screencast-length 3600
```

## 美化

shell: [ohmyzsh](https://github.com/ohmyzsh/ohmyzsh)
themes: [vimix-gtk-themes](https://github.com/vinceliuice/vimix-gtk-themes)
icon: [vimix-icon-theme/](https://github.com/vinceliuice/vimix-icon-theme/)

`sudo apt install gnome-shell-extensions`

extensions: `Blyr `,`Dash to panel`,`User themes`,`openweather`

****
Plymouth

[Ubuntu 16.04美化 -- Plymouth(splash screen/开机画面)主题安装](https://blog.csdn.net/mutilcam_prince/article/details/78299628)

[www.gnome-look.org](www.gnome-look.org)上有大量的`Ubuntu Plymouth`主题,也就是通常所说的开机画面主题,
但是几乎所有的主题在`16.04`之后变的不可用了,那是因为从`16.04`开始, `plymouth`主题存放路径已经变了,
而网络上的主题还是对应的老版路径,那就是`/lib/plymouth/themes/`,`16.04`之后已改为: `/usr/share/plymouth/themes/`.
这导致老版的主题不光用作者写的脚本安装不上,即便是自己手动复制到主题目录里,也不能正常使用.

本篇文章重点介绍一下老版`plymouth`主题如何安装到`16.04`上.

首先正常的话,`16.04`已经默认安装了一个`plymouth`主题,
如果不知道何种原因,你的`16.04`没有默认安装`plymouth`的默认主题,那么可以通过下面这个命令安装:

` sudo apt-get install plymouth-themes`

这条命令会自动创建`/usr/share/plymouth/themes/`并且附带几个简单默认的主题.

那么下面我们来开始安装自定义主题.以[`NSA Splash Screen`] (https://www.gnome-look.org/p/1173975/)为例.

***
下载主题,然后解压后得到一个目录.(本文以解压到`~/Downloads`为例)解压完成后,
重点是`images`目录, `nsa.plymouth`, `nsa.script`
那么我们可以得知,这个主题叫`nsa`
`Plymouth`的主题名称和主题文件名以及主题目录名,必须完全一致,不然会报错.
所以我们首先需要把这个`skd1993-nsa-plymouth-50df7fd`目录名改成`nsa`

***
查看并修改主题文件

`nsa`是个老版本的主题,这个是怎么看出来的呢,用文本编辑器打开`nsa.plymouth`:

[Plymouth Theme]
Name=nsa
Description=An NSA style cool bootsplash
ModuleName=script

[script]
ImageDir=/lib/plymouth/themes/nsa-plymouth
ScriptFile=/lib/plymouth/themes/nsa-plymouth/nsa.script

正如之前所说,`ImageDir`和`ScriptFile`对应的路径已经不存在了.因此我们需要对其进行修改.
对于我们这个主题来说,具体是这样的:

```bash
ImageDir=/usr/share/plymouth/themes/nsa
ScriptFile=/usr/share/plymouth/themes/nsa/nsa.script
```

保存退出.

***
安装主题

把主题文件夹复制到`plymouth`的`theme`目录

`sudo cp -r  ~/Downloads/nsa   /usr/share/plymouth/themes/`

然后安装这个主题.

`sudo update-alternatives --install /usr/share/plymouth/themes/default.plymouth       default.plymouth        /usr/share/plymouth/themes/nsa/nsa.plymouth  100`

更新一下`plymouth`,根据提示手动输入序号选择我们刚刚安装的主题.

`sudo update-alternatives --config default.plymouth`

最后更新一下`initramfs`

`sudo update-initramfs -u`

完成
重启即可看到效果.

***
如果在`sudo update-alternatives –config default.plymouth`这一步出现错误提示:

```bash
W: plymouth module "(/usr/lib/i386-linux-gnu/plymouth//.so)" missing, skipping that theme.
```

这是个内核级的错误,此时不要重启,不然可能卡在开机界面.绝大多数可能是某个步骤中的路径搞错了.
仔细检查所有步骤的路径,然后重来一遍.

本文中的三四步骤, 可以通过一个shell脚本完成.
下面贴上脚本内容(仅限本主题, 其他主题可以简单将文中的`nsa`更换为相应主题的名称, 前提是修改完`nsa.script`)

```bash
#!/bin/bash
# 打印提示信息, 选择颜色使其更醒目
echo  -e "\033[36m Copying new files...  \033[0m"
# 在/usr/share/plymouth/themes文件夹下, 创建目录存放主题文件
sudo mkdir /usr/share/plymouth/themes/nsa
# 复制主题内容, 其中使用了花括号展开, 这里相当于直接给出
sudo cp --recursive {images,nsa.plymouth,nsa.script,README.md} /usr/share/plymouth/themes/nsa
# 安装主题, 语法是: update-alternatives --install link name path priority ... 分别是2级链接, 一级连接, 实际路径
echo -e "\033[31m Installing the theme...  \033[0m"
sudo update-alternatives --install /usr/share/plymouth/themes/default.plymouth default.plymouth /usr/share/plymouth/themes/nsa/nsa.plymouth  100
# 使用 update-alternatives 设置为默认主题, 依照提示选择
echo -e "\033[32m Please Select your theme and set it default...  \033[0m"
sudo update-alternatives --config default.plymouth
# 更新
sudo update-initramfs -u
echo -e "\033[36m Installation Complete!  \033[0m"
```

将上述代码保存到主题目录下, 文件名比如为`xxx.sh`

```bash
chmod +x xxx.sh
./xxx.sh
```

即可完成安装

****
其他主题安装

有的主题也可能自带安装脚本, 比如

[ubuntu-touch: A Plymouth startup and shutdown animated splash](http://gnome-look.org/content/show.php/colours%3A+Ubuntu+rainbow+plymouth+theme?content=163234)

1. 下载解压缩, 进入`ubuntu-touch-splash`
2. `./install-ubuntu-touch [ENTER]`.将安装`plymouth-x11`软件包以提供测试主题的功能, 而无需重新启动.
之后, 将显示新主题的启动和关闭的10秒测试.

如果修改不小心导致启动屏幕黑屏, 则恢复的最快方法是重新提取下载文件, 然后重新安装它.
如果尚未对`ubuntu-touch-splash`文件夹中的文件进行任何更改, 则可以使用以下方法重复测试:
`  ./test-plymouth [输入]`

在启动动画的持续时间非常非常短的情况下, 可以通过在`ubuntu-touch-splash`文件夹中执行来获得改进的效果:
` ./assert-framebuffer`
如果您最终对该主题不满意, 还原可以在主题目录下执行:

```bash
./update-plymouth [ENTER]
```

根据要求选择`ubuntu-logo`或者其他之前使用的主题.

## KDE 桌面环境

现在, 如果您不喜欢KDE或出于任何原因想要删除此环境, 请按照以下步骤在Ubuntu上卸载KDE:

```bash
sudo apt --purge remove kde-standard
sudo apt autoremove
reboot
```

注意: 在安装KDE软件包时安装的某些应用程序必须手动删除.  像(Konsole, Konqueror等)之类的应用
重新启动后, 如果遇到CLI登录屏幕, 请不要慌张.
这是因为您已将SDDM设置为默认显示管理器, 现在已将其删除.  因此, 我们将`gdm3`设置为默认值.

```bash
sudo systemctl restart gdm3
sudo dpkg-reconfigure gdm3
reboot
```

重新启动系统, 然后检查是否一切正常.
