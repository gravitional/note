# ubuntu_1a

## 常用软件

### Ubuntu Dock

[如何移除或禁用 Ubuntu Dock](https://zhuanlan.zhihu.com/p/48078003)

使用 `Dash to Panel` 扩展:

`Dash to Panel` 是 Gnome Shell 的一个高度可配置面板,是 Ubuntu Dock 或 Dash to Dock 的一个很好的替代品(Ubuntu Dock 是从 Dash to Dock 分叉而来的).
安装和启动 `Dash to Panel` 扩展会禁用 Ubuntu Dock,因此你无需执行其它任何操作.
你可以从 extensions.gnome.org 来安装 [Dash to Panel](https://extensions.gnome.org/extension/1160/dash-to-panel/).

如果你改变主意并希望重新使用 Ubuntu Dock,那么你可以使用 Gnome Tweaks 应用程序禁用 Dash to Panel,或者通过单击以下网址旁边的 X 按钮完全移除 `Dash to Panel`: https://extensions.gnome.org/local/ .

### oh-my-zsh

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
安装完成后修改仓库地址

```bash
cd ~/.oh-my-zsh
git remote set-url origin https://gitee.com/mirrors/oh-my-zsh.git
git pull
```

### Powerline 状态条

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

#### 安装 Powerline 的字体

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

#### bash 中的 Powerline

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

#### Vim 中的 Powerline

首先通过如下命令获取 `powerline` 的安装位置.

```bash
pip3 show powerline-status
```

在 `~/.vimrc` 中添加如下内容打开该插件(译注：注意同样需要根据你的系统情况修改路径).

```bash
set rtp+=/home/tom/.local/lib/python3.6/site-packages/powerline/bindings/vim/
set laststatus=2
set t_Co=256
```

然后你打开 `vim` 后会看到一个新的状态行

### tldr

`tldr`(Too Long; Don't Read)：帮你更加高效地学习`linux`命令

```bash
pip3 install --user tldr
```

## X窗口系统

`X`窗口系统(使GUI工作的底层引擎)内建了一种机制,支持快速拷贝和粘贴技巧.

按下鼠标左键,沿着文本拖动鼠标(或者双击一个单词)高亮了一些文本,
那么这些高亮的文本就被拷贝到了一个由X管理的缓冲区里面.然后按下鼠标中键,这些文本就被粘贴到光标所在的位置.

>`ctrl-c` and `ctrl-v`,这两个控制代码对于Shell 有不同的含义,它们在早于Microsoft Windows许多年之前就赋予了不同的意义.

可以把聚焦策略设置为"跟随鼠标",这样鼠标移动到的窗口,就可以接受输入

### 图形界面

[Linux图形界面知识](https://blog.csdn.net/qq_16093937/article/details/83269106)

`X`是协议, 不是具体的某个软件. `XFree86`只是实现`X`协议的一个免费X服务器软件
`X11R6` 是 X Protocol version 11 Release 6,(`X`协议第`11`版第`6`次发行)的意思
窗口管理器的作用是最大化, 最小化, 移动, 关闭窗口等.而这些不是`X`服务器来负责完成的.

`KDE` 和`GNOME`是`Linux`里最常用的图形界面操作环境, 他们不仅仅是一个窗口管理器那么简单,  `KDE`是`K Desktop Environment` 的缩写.
他不仅是一个窗口管理器, 还有很多配套的应用软件和方便使用的桌面环境, 比如任务栏, 开始菜单, 桌面图标等等.
`GNOME`是`GNU Network Object Model Environment` 的缩写.和`KDE`一样, 也是一个功能强大的综合环境.

办公室的电脑在4K分辨率下会卡顿, 2K则正常. 

### 幕后控制台

即使仿真终端没有运行,后台仍然有几个终端会话运行.他们叫做虚拟终端或者虚拟控制台.

在大多数Linux发行版中,可以通过按下 `Ctrl+Alt+F1` 到 `Ctrl+Alt+F6` 访问.

切换可以直接按`Alt+F1..F6`.返回图形桌面,按下`Alt+F7`

### 将标准输出重定向到剪贴板

[将标准输出重定向到剪贴板](https://blog.csdn.net/tcliuwenwen/article/details/103752486)

作为一名优秀的程序员,终端和复制粘贴必将是必不可少的,手动将输出复制粘贴不应该是一名优秀程序员的作风.
那么如何将标准输出重定向到剪贴板方便我们粘贴呢？

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

### 剪贴板管理程序

[10 款最佳剪贴板管理器](https://www.linuxprobe.com/10-best-linux-clipboard.html)

+ CopyQ
+ GPaste
+ Klipper
+ Clipman
+ Diodon

etc

### Linux 录屏软件

[Linux 录屏软件有哪些](https://www.zhihu.com/question/51920876)

如果是`Gnome3`系用户,可以按`ctrl + shift + alt + r`,屏幕右下角有红点出现,则开始录屏,
要结束的话再按一次`ctrl + shift + alt + r`,录好的视频在`~/video`下

***
修改默认30秒的问题, 改成1小时

```bash
gsettings set org.gnome.settings-daemon.plugins.media-keys max-screencast-length 3600
```

### 美化

shell: [ohmyzsh](https://github.com/ohmyzsh/ohmyzsh)
themes: [vimix-gtk-themes](https://github.com/vinceliuice/vimix-gtk-themes)
icon: [vimix-icon-theme/](https://github.com/vinceliuice/vimix-icon-theme/)

`sudo apt install gnome-shell-extensions`

extensions: `Blyr `,`Dash to panel`,`User themes`,`openweather`

****
Plymouth

[Ubuntu 16.04美化——Plymouth(splash screen/开机画面)主题安装](https://blog.csdn.net/mutilcam_prince/article/details/78299628)

[www.gnome-look.org](www.gnome-look.org)上有大量的`Ubuntu Plymouth`主题,也就是通常所说的开机画面主题,
但是几乎所有的主题在`16.04`之后变的不可用了,那是因为从`16.04`开始, `plymouth`主题存放路径已经变了,
而网络上的主题还是对应的老版路径,那就是`/lib/plymouth/themes/`,`16.04`之后已改为：`/usr/share/plymouth/themes/`.
这导致老版的主题不光用作者写的脚本安装不上,即便是自己手动复制到主题目录里,也不能正常使用.

本篇文章重点介绍一下老版`plymouth`主题如何安装到`16.04`上.

首先正常的话,`16.04`已经默认安装了一个`plymouth`主题,
如果不知道何种原因,你的`16.04`没有默认安装`plymouth`的默认主题,那么可以通过下面这个命令安装：

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

`nsa`是个老版本的主题,这个是怎么看出来的呢,用文本编辑器打开`nsa.plymouth`：

[Plymouth Theme]
Name=nsa
Description=An NSA style cool bootsplash
ModuleName=script

[script]
ImageDir=/lib/plymouth/themes/nsa-plymouth
ScriptFile=/lib/plymouth/themes/nsa-plymouth/nsa.script

正如之前所说,`ImageDir`和`ScriptFile`对应的路径已经不存在了.因此我们需要对其进行修改.
对于我们这个主题来说,具体是这样的：

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
如果在`sudo update-alternatives –config default.plymouth`这一步出现错误提示：

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
# 安装主题, 语法是：update-alternatives --install link name path priority ... 分别是2级链接, 一级连接, 实际路径
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
如果尚未对`ubuntu-touch-splash`文件夹中的文件进行任何更改, 则可以使用以下方法重复测试：
`  ./test-plymouth [输入]`
 
在启动动画的持续时间非常非常短的情况下, 可以通过在`ubuntu-touch-splash`文件夹中执行来获得改进的效果：
` ./assert-framebuffer`
如果您最终对该主题不满意, 还原可以在主题目录下执行：

```bash
./update-plymouth [ENTER]
```

根据要求选择`ubuntu-logo`或者其他之前使用的主题. 

### KDE 桌面环境

现在, 如果您不喜欢KDE或出于任何原因想要删除此环境, 请按照以下步骤在Ubuntu上卸载KDE：

```bash
sudo apt --purge remove kde-standard
sudo apt autoremove
reboot
```

注意：在安装KDE软件包时安装的某些应用程序必须手动删除.  像(Konsole, Konqueror等)之类的应用
重新启动后, 如果遇到CLI登录屏幕, 请不要慌张. 
这是因为您已将SDDM设置为默认显示管理器, 现在已将其删除.  因此, 我们将`gdm3`设置为默认值. 

```bash
sudo systemctl restart gdm3
sudo dpkg-reconfigure gdm3
reboot
```

重新启动系统, 然后检查是否一切正常. 

## ubuntu 备份和恢复

教程中排除的目录

```bash
--exclude=/proc 
--exclude=/tmp 
# --exclude=/home 
--exclude=/lost+found 
--exclude=/media 
--exclude=/mnt 
--exclude=/run
--exclude=/sys
```

### 备份方法1

***
分区 id 记录

设备 挂载点 原始uuid 分区格式 新uuid new设备

`/boot/efi` 2EEE-149A `FAT (32-bit version)`
`none` 3278e9ef-8c04-4eaa-bfde-af2313f36545 `Swap`
`/`  7aba3e2f-f94d-454c-9ad6-9098d658401a `Ext4`

***
可以用ubuntu18.04自带软件`gnome-disks`查看自己系统分区情况：
在新电脑上, 可以用`GParted`对硬盘进行分区

***
首先启动live CD, 选择`try Ubuntu`；

开始对需要用live CD备份的分区进行挂载, 所以挂载除了`swap`之外的分区, 对比我们之前创建的表来看下面的挂载命令会更容易理解：

```bash
#获取最高权限
sudo su
#-- 在mnt下创建root目录和efi目录, 分别用来挂载原系统的root分区和efi分区
mkdir /mnt/root   /mnt/efi  /mnt/home
#-- 将原系统根分区挂载到/mnt目录
mount /dev/sdb5   /mnt/root
#将原系统的boot/efi分区挂载到/mnt/efi目录
mount /dev/sda2   /mnt/efi
#-- 将原系统的home分区挂载到/mnt/home
mount /dev/sdb6   /mnt/home
```

***
插入U盘或硬盘开始备份：

```bash
#-- 将root, home和efi分区备份到硬盘中
mksquashfs /mnt/root /media/ubuntu/你的移动硬盘/备份的目录/root.sfs
mksquashfs /mnt/efi /media/ubuntu/你的移动硬盘/备份目录/efi.sfs
mksquashfs /mnt/home /media/ubuntu/你的移动硬盘/备份目录/home.sfs
sync
#-- 卸载刚刚挂载的分区和硬盘
umount /media/ubuntu/* /mnt/*
```

`root`, `home`和`efi`分区备份完成. 

***
对新硬盘进行分区

在新电脑上启动live CD, 选择试用, 打开live CD中自带分区工具`GParted`, 对新电脑进行分区, 分区大小根据个人情况而定, 参照之前填的表：

```bash
efi分区
swap分区,如果不需要休眠, 其实不用分这个, 而且固态硬盘不分这个比较好
root分区(主分区或逻辑分区无所谓)
home分区(主分区或逻辑分区无所谓)
```

***
将备份文件恢复到各个分区

使用`live CD`自带软件`gnome-disks`查看新分区的信息, 完善之前的表, 我的情况如下:

插入存有备份文件的移动硬盘开始恢复备份：

```bash
sudo su
#-- 在mnt下创建root, home和efi目录, 分别用来挂载新电脑的root, home和efi分区
mkdir /mnt/root /mnt/efi  /mnt/home
#-- 挂载新电脑的根分区到/mnt/root目录
mount /dev/sdc5 /mnt/root
#-- 挂载新电脑的efi分区到/mnt/efi
mount /dev/sdc1 /mnt/efi
#-- 挂载新电脑home分区到/mnt/home
mount /dev/sdc6 /mnt/home
#-- 新建/backup/root/, /backup/home和/backup/efi目录,使用loop模式将之前准备好的sfs文件挂载到对应的目录下
mkdir /backup
cd /backup
mkdir root efi home
mount -o loop /media/ubuntu/存放备份文件的路径/root.sfs /backup/root
mount -o loop /media/ubuntu/存放备份文件的路径/efi.sfs  /backup/efi
mount -o loop /media/ubuntu/存放备份文件的路径/home.sfs  /backup/home
#-- 开始恢复
cp -a /backup/root/* /mnt/root
cp -a /backup/efi/* /mnt/efi 
cp -a /backup/home/* /mnt/home 
# -- 同步数据并取消挂载
sync
umount /backup/*
umount /mnt/*
umount /media/ubuntu/*
```

***
启动引导修复

恢复完成之后, 开始来修改引导文件,首先使用`/mnt`目录来挂载新电脑完整的文件系统, 再次掏出我们之前填好的表：

```bash
sudo su
# -- 移除之前创建的临时文件夹 
rm -r /mnt/root /mnt/efi /mnt/home
# -- 首先挂载新电脑的根目录
mount /dev/sdc5 /mnt
#-- 挂载efi
mount /dev/sdc1 /mnt/boot/efi
# -- 挂载home
mount /dev/sdc6 /mnt/home
# 接下来挂载虚拟文件系统, 为后面的修复做准备
mount --o bind /dev   /mnt/dev
mount --o bind /proc   /mnt/proc
mount --o bind /sys   /mnt/sys
# 将liveCD的根目录改为新电脑的根目录确保之后的操作安全. (这里也请大佬指点, 是否只是安全？)
# chroot /mnt
```

接下来修改两个文件分别位于新电脑的`/etc/fstab`和`/boot/grub/grub.cfg`(不用手动修改), 接下来我们再次掏出我们的表：

```bash
# -- 用nano编辑器打开/etc/fstab
nano /etc/fstab
```

按照提示, 和`uuid`变化的对照表, 修改`root`,`/boot/efi`,`/home`等的`uuid`

`/boot/grub/grub.cfg`这个文件是用`grub-mkconfig`自动生成的, 所以不需要手动修改. 

>It is automatically generated by grub-mkconfig using templates  from /etc/grub.d and settings from /etc/default/grub

然后一定要更新`grub`引导:

```bash
# -- grub安装到efi分区
grub-install /dev/sdc1
update-grub
sync #确保数据写入硬盘
#-- 退出chroot
exit
umount /mnt/*/media/ubuntu/* 
exit
reboot
```

系统重启, 拔出你的`live CD`, 不出意外电脑开机将会进入你备份恢复完成的新电脑. 

### 问题排查

如果无法开机/开机时间过长, 首先(使用 liveCD进入系统查看)确保`/etc/fstab`中的硬盘`uuid`没有写错. 

使用下面命令查看问题

```bash
systemd-analyze
systemd-analyze blame
systemd-analyze critical-chain
## 以及
sudo cat /var/log/boot.log | less
```

如果之前有`swap`分区, 还原的时候没有`swap`分区, 修改`initramfs`配置：

```bash
sudo vim /etc/initramfs-tools/conf.d/resume
#在文件中添加一行
RESUME=none
sudo update-grub
sudo update-initramfs -u
```

以下命令在排除错误时可能用到

```bash
sudo blkid
sudo fdisk -l
sudo dmesg
```

[Ubuntu 18.04 如何添加或删除 SWAP 交换分区](https://www.sysgeek.cn/ubuntu-18-04-swap/)
[Linux系统制作Ubuntu18.04启动盘](https://blog.csdn.net/xiaoma_2018/article/details/85059930)

```bash
# 插入U盘, 在Linux系统中打开终端, 查看 U 盘信息：
sudo fdisk -l 
# 然后卸载掉 U 盘：
sudo umount /dev/sdb*
# U 盘格式化：
sudo mkfs.vfat /dev/sdb -I # 使用 -I 选项创建
#完成格式化后查看磁盘信息：
# 最后使用 dd 命令制作启动盘：
sudo dd if=ubuntu-18.04.1-desktop-amd64.iso of=/dev/sdb bs=10M
```

### Ubuntu系统备份2

[Ubuntu系统备份](https://zhuanlan.zhihu.com/p/51827233)

备份前可以先清理一下缓存

```bash
# 清理旧版本的软件缓存
sudo apt-get autoclean
# 清理所有软件缓存
sudo apt-get clean
# 删除系统不再使用的孤立软件
sudo apt-get autoremove
```

ubuntu 秉承一切皆文件的思想, 系统备份就相当于把整个`/`(根目录)所有文件打包压缩保存

主要有两种方式备份还原：

+ `tar` 命令
+ `livecd` 模式

首先介绍下 `tar` 命令备份

```bash
# 备份前先切换到root用户, 避免权限问题
sudo su
# 再切换到 /(根目录)
cd /
# 备份系统
tar -cvpzf /media/Disk/myDisk/ubuntu_backup@ $(date +%Y-%m+%d).tar.gz --exclude=/proc --exclude=/tmp --exclude=/home --exclude=/lost+found --exclude=/media --exclude=/mnt --exclude=/run /
```

***

tar命令参数：

+ `-c`： 新建一个备份文档
+ `-v`： 显示详细信息
+ `-p`： 保存权限, 并应用到所有文件
+ `-z`： 用 `gzip` 压缩备份文档, 减小空间
+ `-f`： 指定压缩包名称(带路径), 只能做最后一个参数
+ `–exclude`： 排除指定目录, 不进行备份

注意, 如果没有把`/home`或者`/boot`目录单独分一个区, 一定不要加`–exclude=/home`或`–exclude=/boot`参数！

文件目录介绍

`/proc`：一个虚拟文件系统, 系统运行的每一个进程都会自动在这个目录下面创建一个进程目录. 既然是系统自动创建, 也就没必要备份的必要了. 
`/tmp`：一个临时文件夹, 系统的一些临时文件会放在这里. 
`/lost+found`：系统发生错误时(比如非法关机), 可以在这里找回一些丢失文件. 
`/media`：多媒体挂载点, 像u盘, 移动硬盘, windons分区等都会自动挂载到这个目录下. 
`/mnt`：临时挂载点, 你可以自己挂载一些文件系统到这里. 
`/run`：系统从启动以来产生的一些信息文件. 
`/home`：用户家目录, 存放用户个人文件和应用程序. 
`/boot`：和系统启动相关的文件, 像grub相关文件都放在这里, 这个目录很重要！

为了保险起见, 也可以对`/home`和`/boot`备份, 但是备份频率完全没必要和/分区一样高. 
比如`/`分区每周备份一次, 那`/home`和`/boot`完全可以一个月备份一次, 因为这两个分区出问题的概率真的很小, 而且变动也不会太频繁. 

```bash
tar -cvpzf /media/Disk/my_Disk/ubuntu_home_backup@`date +%Y-%m-%d`.tar.gz /home
tar -cvpzf /media/Disk/myDisk/ubuntu_boot_backup@`date +%Y-%m-%d`.tar.gz /boot
```

***

打包过程中会遇到如下错误或警告信息

```bash
tar: Exiting with failure status due to previous errors 
```

这个问题其实不是真正的错误信息,  真正的错误信息混杂在标准输出(stout)中, 重新执行命令并把`v`参数去掉即可看到真正问题所在. 

```bash
tar: Removing leading '/' from member names  
#或 
tar: Removing leading '/' from hard link targets 
```

这个问题其实不影响程序的执行, 产生的原因是`tar`在压缩的过程中自动帮我们去掉了路径前的`/`, 也就是tar压缩后的包是按照相对路径压缩的. 
 当我们恢复时,  就需要通过 `-C` 参数手动指定解压到 `/` 目录,  如：

```bash
tar zxvpf ubuntu_20170120_11.tar.bz2 -C / 
```

可以使用`-P`参数来指定按照绝对路径打包：

```bash
tar -cvpzf /media/Disk/myDisk/ubuntu_backup@`date +%Y-%m+%d`.tar.gz --exclude=/proc --exclude=/tmp --exclude=/home --exclude=/lost+found --exclude=/media --exclude=/mnt --exclude=/run -P /
```

另外, 如果出现

```bash
tar: /dev/shm: file changed as we read it 
或 
tar: /run/udev/control: socket ignored 
```

这个 `socket ignored` 产生的原因是压缩的过程中文件正在使用, 无需理会, 不影响压缩. 

```bash
tar: /run/user/1000/gvfs: Cannot stat: Permission denied 
```

这个问题不用理会, 与虚拟文件系统有关, 不影响压缩. 

Ubuntu系统U盘(livecd)方式备份

启动过程中从U盘启动, 采用试用Ubuntu系统的方式, 进入`livecd`模式(试用ubuntu)

```bash
sudo su
fdisk -l
```

其中`boot`分区为`/dev/sda5`,`home`分区为`/dev/sda7`,主分区`/`为`/dev/sda8`. 

分别进行挂载：

```bash
mount /dev/sda5 /boot
mount /dev/sda7 /home
mount /dev/sda8 /mnt
```

再对移动硬盘进行挂载：

```bash
mount /media/ubuntu/移动硬盘对应盘符 /data
```

进行备份：

```bash
mksquashfs /mnt /data/ubuntu_main.sfs(文件名任意)
mksquashfs /home /data/ubuntu_home.sfs(文件名任意)
mksquashfs /boot /data/ubuntu_boot.sfs(文件名任意)
sync(让系统保存一下数据)
```

卸载硬盘：

```bash
umount /data
umount /mnt
umount /home
umount /boot
```

到此备份成功. 

### 系统还原

系统备份的意义就在于系统哪天发生意外时可以系统还原拯救回来

这里有两种还原方式, 如果你系统出问题了, 但是还可以进入终端, 那就可以直接解压备份文件进行还原. 
但是如果你连系统都不能登录了, 就要使用`LiveCD`(U盘启动盘)进行还原了. 

#### tar命令还原系统

```bash
# 备份前先切换到`root`用户, 避免权限问题
sudo su
# 再切换到/(根目录)
cd /
# 还原
tar -xvpzf /media/Disk/myDisk/ubuntu_backup@2016-6-6.tar.gz -C /
```

注意先创建一个临时目录用于挂载你的`/`根目录分区, `sdaX`代表你的`/`根目录分区, 
如果不知道就用`fdisk -l`查看一下, 另外如果你的移动硬盘没有被自动挂载, 你也需要手动创建一个临时目录进行挂载. 

Note：
因为 tar还原是只会覆盖相同的文件, 但是这种方法只是恢复备份时的文件, 就是说如果某些文件丢失或损坏了, 这样可以恢复修复这些文件, 但不能删除自备份到恢复前这期间所生成的其它文件.
假如你备份系统时有`1234`这四个文件, 如果三天后, 由于某些原因变成了`1234'5`(`4`改变了), 你恢复后, 就会变成`12345`, 其中`4'`恢复成备份时的文件, `5`保留. 
所以大家要是想彻底还原成备份时候的样子最好彻底删除根目录下的所有文件, 然后再还原, 这样就可以还原成备份时的样子了. 
删除整个文件系统, 比如运行命令`rm -fr /*`, 那么你还原系统后一定要把你之前没有备份的目录手动创建, 不然重启系统是有问题的. 

```bash
mkdir proc tmp lost+found media mnt run
```

***
进入 LiveCD 之后 还原系统

```bash
#切换到root用户
sudo su
#进入到 / 目录
cd /
mkdir /mnt/sys
mount /dev/sdaX /mnt/sys
tar -xvpzf /media/myDisk/ubuntu_boot_backup@2016-6-6.tar.gz -C /mnt/sys
```

执行恢复命令之前请再确认一下你所键入的命令是不是你想要的, 执行恢复命令可能需要一段不短的时间. 

恢复命令结束时, 你的工作还没完成, 别忘了重新创建那些在备份时被排除在外的目录：

```bash
mkdir proc
mkdir lost+found
mkdir mnt
mkdir sys
```

等等

当你重启电脑, 你会发现一切东西恢复到你创建备份时的样子了！

#### livecd 还原 

还有一个稍微复杂点的`livecd`方式还原, 如下：

先要对之前的启动文件和分区文件做一个备份, 分别为`/etc/fstab`,`/etc/fstab.d`(可能没有),`/boot/grub/grub.cfg`：

```bash
cp /etc/fstab /media/用户名/移动硬盘对应盘符/
cp /boot/grub/grub.cfg /media/用户名/移动硬盘对应盘符/
```

接下来进行恢复系统, 同样利用系统`u`盘进入`livecd`模式. 同样进入`root`模式, 查看分区情况

假设分区情况如上：`/dev/sda1`为`boot`分区, `/dev/sda2`为主分区, `/dev/sda3`为`home`分区. 对`home`分区和主分区进行格式化：

```bash
sudo su
mkfs.ext4 /dev/sda2 # root
mkfs.ext4 /dev/sda3 # home
```

然后分别进行挂载：

```bash
mount /dev/sda2 /mnt #挂载 root
# 新建home和boot文件:
mkdir /mnt/home 
mkdir /mnt/boot
#挂载其他两个盘：
mount /dev/sda1 /mnt/boot #挂载 boot
mount /dev/sda3 /mnt/home # 挂载 home
# 挂载数据盘：
mkdir /rescovery/mnt
mkdir /rescovery/home
mkdir /rescovery/boot
mount -o loop /media/ubuntu/移动硬盘盘符/ubuntu_main.sfs /rescovery/mnt  # 这里使用了 loop mount, 循环挂载
mount -o loop /media/ubuntu/移动硬盘盘符/ubuntu_home.sfs /rescovery/home
mount -o loop /media/ubuntu/移动硬盘盘符/ubuntu_boot.sfs /rescovery/boot
```

```bash
cp -a /recovery/mnt/* /mnt
cp -a /recovery/home/* /mnt/home
cp -a /recovery/boot/* /mnt/boot
# 然后拷贝之前的fstab和grub.cfg文件到硬盘：
cp /media/ubuntu/移动硬盘盘符/fstab /mnt/etc/
cp /media/ubuntu/移动硬盘盘符/grub.cfg /mnt/boot/grub/
# 挂载虚拟文件系统, 这是为了后面修复引导做准备. 
mount --bind /dev /mnt/dev
mount --bind /proc /mnt/proc
mount --bind /sys /mnt/sys
# chroot进入已经还原的操作系统. 
chroot /mnt
```

### 查看当前UUID

由于我们格式化了分区, 所以`UUID`发生了变化, 若不修改, 系统将无法正常挂载分区, 导致启动异常. 
故需要修改本机系统的`UUID`设置,当前终端不要关闭, 新建一个终端, 输入`blkid`:

在`fstab`文件中更新上面两个新硬盘的`UUID`,在`chroot`过的端口输入：

```bash
nano /etc/fstab
# 更改两个UUID, ctrl+x退出, Y保存. 进行grub的更新：
grub-install /dev/sda
update-grub
退出并卸载盘：
exit
umount /mnt/dev
umount /mnt/sys
umount /proc
sync
```

重启即可

### mount 简介

`-o --options opts`:使用指定的安装选项.  `opts`参数是用逗号分隔的列表.  例如：

```bash
mount LABEL=mydisk -o noatime,nodev,nosuid
```

复制文件：

在其他位置重新挂载文件结构的一部分.  语法是：

```bash
mount --bind olddir newdir
```

或使用以下`fstab`条目：

```bash
/olddir /newdir none bind
```

调用之后, 可以在两个位置访问相同的内容. 

重要的是要理解`bind`不会在`kernel VFS`中创建任何`second-class `或`special node`.  
`bind`只是`attach`文件系统的另一种操作. 没有一个特定的地方去记录哪些文件系统是通过`bind`附加到系统上的. 
`olddir` 和 `newdir` 是独立的, 并且可以卸载`olddir`. 

也可以将单个文件重新挂载(在单个文件上).  也可以使用`bind`从常规目录创建`mountpoint`, 例如：

```bash
mount --bind foo foo
```

## texlive lyx 相关

### xelatex 脚本

[shell 参数换行 & shell 输出换行的方法](https://blog.csdn.net/donaldsy/article/details/99938408)

首先测试一下括号的用法:

```bash
tex_list=1; echo $tex_list; tex_list=$( { ls -x *.tex } ); echo $tex_list;
tex_list=1; echo $tex_list; tex_list=$( ( ls -x *.tex ) ); echo $tex_list;
tex_list=$(ls -x *.tex; ls -x *.log); echo $tex_list;
tex_list=$( (ls -x *.tex; ls -x *.log) ); echo $tex_list;
```

```bash
#!/bin/bash

# 设置格式化相关的部分
delimiter="echo -e \\\n+++++++++++++"
nameis="name is :"
eval  $delimiter

# 默认文件名是 main,否则使用文件夹中的tex文件名
tex_usual="main"
echo "tex_usual $nameis $tex_usual"
eval  $delimiter

# 当前tex文件列表,去掉后缀

tex_list=$(ls -x *.tex)
echo "tex_list $nameis $tex_list"

tex_here=${tex_list//".tex"/}
echo "tex_here $nameis $tex_here"
eval  $delimiter

# 判断当前tex文件列表中是否包含 main.tex
# 若有 main.tex,使用之,若没有,则使用 列表中的tex
# tex_file=${${tex_here}%% *}

if [[ $tex_usual =~ $tex_here ]]
then
    tex_file=$tex_usual
else
    tex_file=${tex_here}
fi

echo "tex_file $nameis $tex_file"
eval  $delimiter

# 可增加输出文件夹选项 -auxdir=temp -outdir=temp
# 还有 -shell-escape 选项

# 把下面这行加入到 ~/.latexmkrc,指定 pdf 查看程序
# $pdf_previewer = 'evince %O %S';
# -silent 可以抑制输出

latexmk -xelatex  -silent -pv  -view=pdf -bibtex -cd -recorder -file-line-error -halt-on-error -interaction=nonstopmode -synctex=1 -view=pdf ${tex_file}

## 输出错误记录
eval  $delimiter
echo 'error message'
eval  $delimiter
## 用 tail 减少输出数量
## grep -m 100 -i -n --color -P -B 0 -A 8 "\[\d+\]" ./$tex_file".log" | tail -n 50
grep -m 10 -i -n --color -P -B 0 -A 8 "\[\d+\]" ./$tex_file".log"
```

***

默认情况下,`echo`关闭了对转义字符的解释,添加 `-e `参数可打开`echo`对转义字符的解释功能.`-E`关闭转义字符,是默认值.

```bash
echo -e "hello\n wrold" #换行输出 hello world
echo -E "hello\n wrold" #输出 hello\n world, 默认情况
```

### texlive安装与卸载

***
[Linux环境下LaTex的安装与卸载](https://blog.csdn.net/l2563898960/article/details/86774599)
[Ubuntu Texlive 2019 安装与环境配置](https://blog.csdn.net/williamyi96/java/article/details/90732304)
[TexLive 2019 安装指南](https://zhuanlan.zhihu.com/p/64530166)
[TeX Live - Quick install](https://tug.org/texlive/quickinstall.html)

***
准备工作:下载,清除

注意:安装 `lyx`, `apt` 会默认安装 `tex2017`版本,覆盖掉新版的`texlive2020`

注意:如果重新安装,请务必完全删除之前的失败安装,默认情况下,这将在这两个目录中:

```bash
rm -rf /usr/local/texlive/2020
rm -rf ~/.texlive2020
```

或者参考下面的命令

```bash
sudo rm -rf /usr/local/texlive/2020
rm -rf ~/.texlive2020
sudo rm -rf /usr/local/texlive
sudo rm -rf /usr/local/share/texmf
sudo rm -rf /var/lib/texmf
sudo rm -rf /etc/texmf
sudo apt-get purge texlive*
sudo apt-get remove tex-common --purge
```

***
进行安装

因为下载好的是一个`iso`镜像文件,所以下载好之后,还需要挂载到`/mnt`目录下

```bash
sudo mount -o ro,loop,noauto texlive2020-20200406.iso /mnt
```

+ `ro` :     Mount the filesystem read-only.
+ `loop` : loop 文件
+ `auto` :   Can be mounted with the -a option.
+ `noauto` : Can only be mounted explicitly (i.e., the  -a  option  will  not cause the filesystem to be mounted).

***
接着运行`install-tl`脚本进行安装.

若要更改安装目录或其他选项,请阅读提示和说明.
一般需要更改路径到自己有读写权限的文件夹下面,按`D`,然后按`1`,输入比如`~/texlive/2020`

更改目录到

+ `TEXDIR:         /home/tome/texlive/2020`
+ `main tree:      /home/tome/texlive/2020/texmf-dist`

+ `TEXMFLOCAL:     /home/tome/texlive/texmf-local`
+ `TEXMFSYSVAR:    /home/tome/texlive/2020/texmf-var`
+ `TEXMFSYSCONFIG: /home/tome/texlive/2020/texmf-config`
+ `TEXMFVAR:       ~/.texlive2020/texmf-var`
+ `TEXMFCONFIG:    ~/.texlive2020/texmf-config`
+ `TEXMFHOME:      ~/texmf`

```bash
cd /tex_iso_directory
sudo ./install-tl --profile installation.profile
[... messages omitted ...]
Enter command: i
[... when done, see below for post-install ...]
```

安装程序的接口:文本,GUI,批处理
安装程序支持:文本,图形,和批处理接口.(Linux系统下没有图像安装,在Windows下支持图形安装)

`install-tl -gui text #`使用简单文本模式.也是输入`install-tl`默认选项.

`install-tl --profile=profile #`进行一个批处理安装,需要一个 `profile` (配置文件),为了创建一个`profile`,最简单的方式是使用`tlpkg/texlive.profile`文件,这是安装器在安装成功后生成的文件.

***
卸载镜像文件

```bash
sudo umount /mnt
```

***
字体配置

```bash
sudo cp /home/tom/texlive/2020/texmf-var/fonts/conf/texlive-fontconfig.conf /etc/fonts/conf.d/20-texlive.conf
sudo fc-cache -fsv
```

***
环境变量

安装完之后有提示:

```bash
Add /home/tom/texlive/2020/texmf-dist/doc/man to MANPATH.
Add /home/tom/texlive/2020/texmf-dist/doc/info to INFOPATH.
Most importantly, add /home/tom/texlive/2020/bin/x86_64-linux
to your PATH for current and future sessions.
```

我用的是`zsh`,如果用的是`bash`则修改`~/.bashrc`,其中的`/home/tom/texlive/2020`改称你安装时的路径
直接把下面的语句添加到`.zshrc`文件末尾.

```bash
export MANPATH=${MANPATH}:/home/tom/texlive/2020/texmf-dist/doc/man
export INFOPATH=${INFOPATH}:/home/tom/texlive/2020/texmf-dist/doc/info
export PATH=${PATH}:/home/tom/texlive/2020/bin/x86_64-linux
```

***
验证安装是否成功

```bash
tex -v
```

***
设置默认纸张尺寸

`tlmgr paper letter`

***
ubuntu 仓库的texlive

使用`apt`命令从`ubuntu`仓库安装的`texlive`可以使用`dpkg -L texlive-full`查询

安装在 `/usr/local/`目录下,
`texmf`(TDS的根目录)在`/usr/share/texmf` and `/usr/share/texlive/texmf-dist`

### texlive常用命令

用`texlive**.iso`手动安装的 texlive 是可以正常使用下面这些命令的,而用 `debian`源`apt`安装的,可能会出问题.

`tlmgr [option]... action [option]... [operand]...`

安装好 `texlive` 后

如果使用`tlmgr option` 报错
`cannot setup TLPDB in /home/USER/texmf at /usr/bin/tlmgr line 5308.`

原因如下:

未初始化`tlmgr`时会产生此错误. 在大多数情况下,以普通用户身份启动以下命令可以解决此问题：

`$ tlmgr init-usertree`

此命令将在您的家目录内创建几个文件夹. 请参见手册页以获取解释：

>在用户模式下使用`tlmgr`之前,您必须使用`init-usertree`操作设置用户树.
>这将创建`usertree / web2c`和`usertree / tlpkg / tlpobj`,以及最小的`usertree / tlpkg / texlive.tlpdb`.
>此时,您可以通过添加`--usermode`命令行选项来告诉`tlmgr`执行(支持的)动作.

***
下面这些是`tlmgr`的常用命令:

+ `tlmgr option repository ctan`
+ `tlmgr option repository http://mirror.ctan.org/systems/texlive/tlnet`
+ `tlmgr repository list`
+ `tlmgr update --self`
+ `tlmgr update  --all`

如果要使用清华的`mirror`:

`tlmgr option repository https://mirrors.tuna.tsinghua.edu.cn/CTAN/systems/texlive/tlnet`

[texlive home page](https://tug.org/texlive/) 
[texlive installation and updates](https://tug.org/texlive/pkginstall.html) texlive 安装和更新
[archive of tlnet ](https://www.texlive.info/tlnet-archive/) ：各个年份的 tex 更新, 可以选择用来更新的 repository 的版本
[texlive.info](https://texlive.info/) 查看各种关于 texlive 的信息

告诉`tlmgr`使用附近的CTAN镜像进行将来的更新； 如果您从DVD映像安装了TeX Live,并且想要持续更新,则很有用.
这两个命令是等效的. `ctan`只是给定URL的别名. 
注意：`mirror.ctan.org`解析为许多不同的主机,它们并没有完全同步. 我们建议仅(最多)每天更新一次,而不要更频繁.

+ `tlmgr update --list` 报告将要更新的内容,而无需实际更新任何内容.
+ `tlmgr update --all` 使本地TeX安装与软件包存储库中的安装相对应(从CTAN更新时通常很有用).
+ `tlmgr info pkg` 显示有关软件包内容的详细信息,例如搜索所有软件包中内容的安装状态和描述.

可能遇到的错误：

[tlmgr: unexpected return value from verify_checksum: -5](https://tex.stackexchange.com/questions/528634/tlmgr-unexpected-return-value-from-verify-checksum-5)

出现这个错误是由于某个`repository`的`signing key`过期了, 
首先可以使用`tlmgr repository list`列出所有的库, 使用`tlmgr key list`列出所有的`keys`

首先把`repository`更换到对应`debian`发行版的仓库, 比如使用 `2019` 版本的 `repository` , 
`tlmgr option repository https://www.texlive.info/tlnet-archive/2019/12/31/tlnet/`

然后把[tug](https://www.tug.org/texlive/)的 `GPG` key 加入到 `tlmgr` 的key 列表中

```bash
curl -fsSL https://www.tug.org/texlive/files/texlive.asc | tlmgr key add -
```

这样就不会出现`erify_checksum: -5`错误了. 

总结：

+ `tlmgr key list`列出所有的`key`
+ `tlmgr repository list`列出使用的仓库
+ `curl -fsSL https://www.preining.info/rsa.asc | tlmgr key add -`为`contrib`仓库添加新的gpg key
+ `tlmgr install  --verify-repo=none pkg` 免去验证

curl -fsSL https://www.preining.info/rsa.asc | tlmgr key add -

### tlmgr 命令

***
`install [option]... pkg...`

如果尚未安装,请安装命令行上给出的每个`pkg`.
(它不涉及现有软件包；有关如何获取软件包的最新版本,请参见`更新`操作.)

默认情况下,这还会安装给定pkg所依赖的所有软件包. 选项：

+ `--dry-run` : 实际没有安装任何东西. 而是将要执行的动作写入终端.
+ `--file`: 不从安装库中获取软件包, 使用命令行上给出的软件包文件. 这些文件必须是标准的`TeX Live`软件包文件(包含`tlpobj`文件).
+ `--force`:如果存在对`tlmgr`本身(或基本基础结构的其他部分)的更新,
则除非给出此选项,否则`tlmgr`将退出紧急状态并且不会执行安装. 不建议.
+ `--no-depends`:不要安装依赖项. (默认情况下,安装软件包可确保满足该软件包的所有依赖关系.)
+ `--no-depends-at-all`:通常,当您安装附带二进制文件的软件包时,还将安装相应的二进制软件包.
也就是说,对于软件包`foo`,软件包`foo.i386-linux`也将安装在`i386-linux`系统上. 
此选项抑制了这种行为,并且还暗示了`--no-depends`.
除非您确定自己在做什么,否则不要使用它.
+ `--reinstall`:即使似乎已经安装了软件包(即TLPDB中已存在),也要重新安装软件包(包括集合的依赖项).
这对于从意外删除层次结构中的文件中恢复非常有用.

***

+ `conf [texmf|tlmgr|updmap [--conffile file] [--delete] [key [value]]]`
+ `conf auxtrees [--conffile file] [show|add|delete] [value]`

仅使用`conf`,即可显示TeX Live的常规配置信息,包括活动配置文件,路径设置等. 
这就像运行`texconfig conf`一样,但是可以在所有支持的平台上运行.

使用`conf texmf`,`conf tlmgr`或`conf updmap`之一显示`ROOT / texmf.cnf`(用户特定的`tlmgr`配置)中保存的所有键/值对(即所有设置) 文件(请参见下文)或第一个(通过`kpsewhich`找到的)`updmap.cfg`文件.

`conf`显示的`PATH`值与`tlmgr`使用的值相同. 包含`tlmgr`可执行文件的目录会自动添加到从环境继承的PATH值之前.

这是更改配置值的实际示例. 如果在安装过程中启用了通过`\ write18`执行的(部分或全部)系统命令,则可以在以后将其禁用：

```bash
tlmgr conf texmf shell_escape 0
```

子命令`auxtrees`允许完全在用户控制下添加和删除任意其他texmf树.
`auxtrees show`显示其他树的列表,`auxtrees add`树将树添加到列表中,`auxtrees remove`树从列表中删除树(如果存在).

树中不应包含`ls-R`文件(否则,如果`ls-R`过时,则可能找不到文件). 
通过操作`ROOT / texmf.cnf`中的Kpathsea变量`TEXMFAUXTREES`来生效. 例：

```bash
tlmgr conf auxtrees add /quick/test/tree
tlmgr conf auxtrees remove /quick/test/tree
```

在所有情况下,如果需要,都可以通过选项`--conffile`文件显式指定配置文件.

警告：此处是用于更改配置值的一般工具,但是强烈建议不要以这种方式修改设置. 
同样,不对键或值进行错误检查,因此可能发生任何破损.

### 手动安装宏包

如果无法使用`tlmgr`自动安装宏包, 例如`ubuntu`自带的`texlive`, 默认的版本比远程仓库中的低, 无法自动升级. 
可以直接从网上下载想要安装的宏包, 大部分宏包已经打包成标准格式, 例如`siunitx.tds.zip`, [siunitx](https://www.ctan.org/pkg/siunitx).
直接解压到`texlive`的安装目录即可.

如何定位安装目录呢？可以参考[Installing TeX fonts](http://www.tug.org/fonts/fontinstall.html), 虽然这个文章主要是介绍安装字体的. 
使用类似下面的命令找出`texlive`的安装目录. 

```bash
kpsewhich --var-value TEXMF
tlmgr conf # 这个会输出texlive大部分配置的信息
```

我电脑上`texlive`安装在`/usr/share/texmf `, 这是共享目录, 如果安装在这里, 所有账户都能使用. 
此外在家目录下, 即`/home/tom/texmf`还有一个用户目录树, 建议把宏包解压到这里, 不会影响`ubuntu`自带的发行版. 
如果你的`home`没有这个文件夹, 可以运行`tlmgr init-usertree`产生一个. 
由于路径中`/home/tom/texmf`在前面, 把宏包装在这里, 就会被优先使用. 

### jabref

[JabRef中文手册](https://blog.csdn.net/zd0303/article/details/7676807)

entry 时间戳

本功能可以在`选项->偏好设置->通用设置`中关闭或配置. `JabRef`能自动的产生一个包含题录加入数据库的日期的域.

格式：

时间戳记的格式由包含指示词的字符串确定,指示词表示日期各部分的位置.
以下是一些可用的指示字母(示例在括号中给出,为： 2005年9月14日(星期三)下午5.45)：

+ yy: year (05)
+ yyyy: year (2005)
+ MM: month (09)
+ dd: day in month (14)
+ HH: hour in day (17)
+ mm: minute in hour (45)

这些指示符可以与标点符号和空格一起使用. 几个例子：

+ `yyyy.MM.dd gives 2005.09.14`
+ `yy.MM.dd gives 05.09.14`
+ `yyyy.MM.dd HH:mm gives 2005.09.14 17:45`

### lyx 报错

***
如果桌面环境使用`Gnome`默认, 也就是`Wayland`协议, 默认`ibus`输入法在`lyx`下无法使用. 报错为

```bash
Warning: Ignoring XDG_SESSION_TYPE=wayland on Gnome. Use QT_QPA_PLATFORM=wayland to run on Wayland anyway.
```

按照[SDB:在 Wayland 中启用输入法](https://zh.opensuse.org/SDB:%E5%9C%A8_Wayland_%E4%B8%AD%E5%90%AF%E7%94%A8%E8%BE%93%E5%85%A5%E6%B3%95)
操作仍然不行.

进入 `KDE` 或 `GNOME` 的 `Wayland` 会话之后, 您可能会发现输入法(Fcitx 或 iBus)无法使用. 
最新的稳定版 `Fcitx` 和 `iBus` 都已经了基本的 `Wayland` 支持, 通过 X 的协议转接实现. 
`Wayland` 读取的环境配置文件是`/etc/environment` 而不是 `X` 所读取的环境变量文件. 因此对 `X` 有效的输入法配置在 `Wayland` 上不起效果了. 以管理员权限编辑它：

```bash
sudo vi /etc/environment
```

这个文件应该是空的, 只有几行注释. 添加下面几行, 以 `Fcitx` 为例：

```bash
INPUT_METHOD=fcitx
GTK_IM_MODULE=fcitx
QT_IM_MODULE=fcitx
XMODIFIERS=@im=fcitx
```

如果您使用 iBus 的话, 那么应该添加这几行：

```bash
INPUT_METHOD=ibus
GTK_IM_MODULE=ibus
QT_IM_MODULE=ibus
XMODIFIERS=@im=ibus
```

之后请重启您的系统.  

***
有时安装好了texlive,也安装好了`lyx`,却仍然报错,这个时候一般是因为路径(`$PATH`)没有配置好,
lyx没有检测到texlive的各种文件.参考 [LYX Manuals](https://wiki.lyx.org/uploads//LyX/Manuals/1.6.4//Manuals.pdf)

`LYX`的一些功能可以从`LYX`内部进行配置,而无需重新配置文件. 
首先,`LYX`能够检查您的系统,以查看可以使用哪些程序,`LATEX`文档类和`LATEX`软件包. 
它使用此知识为多个`Preferences`设置提供合理的默认值.
尽管在系统上安装`LYX`时已经完成了此配置,但是您可能需要在本地安装一些项目,
新的`LATEX`类,而`LYX`看不到这种变化.
要强制LYX重新检查系统,您应该使用`Tools,Reconfigure`. 然后,您应该重新启动`LYX`以确保考虑到更改.

添加`tex`文件的路径到`$PATH`中的时候,注意尽量把新的`tex`路径添加到`$PATH`前面,
以防止之前安装的残留掩盖新的路径.也就是,

```bash
if [[ $SHELL == "/bin/zsh" ]] ;then
echo "\n# add texlive paths" >> ~/.zshrc
echo "export MANPATH=your_texlive_path/texmf-dist/doc/man:${MANPATH}" >> ~/.zshrc
echo "export INFOPATH=your_texlive_path/texmf-dist/doc/info:${INFOPATH}" >> ~/.zshrc
echo "export PATH=your_texlive_path/bin/x86_64-linux:${PATH}" >> ~/.zshrc
fi
```

直接使用`apt`仓库安装的`texlive`套装和`lyx`一般没有问题.

### lyx 默认pdf查看

在`tools-preferences-File Handling-File Formats`

在 `Format` 一栏中选中`PDF(XeTex)`  或者其他想要更改的格式,然后在 `Viewer`中更改程序,或者自定义程序位置.

### latexmk 选项

一般来说, `latexmk` 的通用`cmd`命令形式为:

`latexmk [options] [file]`

所有的选项可以用单个`-`连字符,也可以用双连字符`--`引入,e.g., "latexmk -help" or "latexmk --help".

***
注意:

除了文档里列出的选项, `latexmk`认识几乎所有the options recognized by the latex, pdflatex programs (and their relatives),
在当前的 TexLive and MikTeX 发行版中.

这些程序的一些选项还会引起 latexmk 的特殊 action or behavior,在本文档中有解释.否则,它们被直接传递给latex or pdflatex.
run `latexmk -showextraoptions`给出选项列表,这些选项被直接传递给latex or pdflatex.

***
注意:

"Further processing" 意味着需要运行其他程序,或者再次运行`latex`(etc),如果没有 `errors` 的话.
如果你不想让`latex`在遇到错误的时候停下,应该使用 latexmk's option `-interaction=nonstopmode`

`-xelatex`  使用`xelatex`编译
`-pv `   - preview document.  (Side effect turn off continuous preview)
` -pv-`   - turn off preview mode
`-pvc`   - preview document and continuously update.  (This also turns  on force mode, so errors do not cause latexmk to stop.)
(Side effect: turn off ordinary preview mode.)
`-pvc-`  - turn off -pvc

`-view=default` - viewer is default (dvi, ps, pdf)
`-view=ps`      - viewer is for ps
`-view=pdf`     - viewer is for pdf

`-bibtex`       - use bibtex when needed (default)
`-bibtex-`      - never use bibtex

`-cd`    - Change to directory of source file when processing it

`-recorder` - Use -recorder option for (pdf)latex (to give list of input and output files)
` -recorder-` - Do not use -recorder option for (pdf)latex

***
简单传递的命令

`-error-line=n` set the width of context lines on terminal error messages
`-half-error-line=n`      set the width of first lines of contexts in terminal error messages

`-file-line-error `       enable `file:line:error` style messages
`-halt-on-error`          stop processing at the first error
`-interaction=STRING`     set interaction mode (STRING=batchmode/nonstopmode/scrollmode/errorstopmode)
`-synctex=NUMBER`         generate `SyncTeX` data for previewers if nonzero

### 安装latex包

[Ubuntu/Mint下LaTeX宏包安装及更新](https://blog.csdn.net/codeforces_sphinx/article/details/7315044)

一般使用texlive的包管理工具,否则需要手动安装:

1. Get the package from [CTAN](http://www.ctan.org/CTAN) or wherever.
2. 如果其中有一个文件是`.ins` 结尾的,打开终端,执行命令`latex foiltex.ins`,就获得了安装需要的包.大多数 latex 包没有打包,所以可以跳过这一步.
3. 现在你需要决定,这个包要安装给所有用户使用,还是only for you.
4. 在*nix 系统上(OSX),给所有用户使用,安装到`local` TeX tree, 给自己使用,安装到`user`TeX tree.

查看`texmf.cnf`文件,它通常在`$TEXMF/web2c`文件夹,但是可以用`kpsewhich texmf.cnf`定位.

`local` Tree 的位置在 `TEXMFLOCAL` variable 中定义,通常像是`/usr/local/share/texmf`.
`user`  Tree 的位置在`TEXMFHOME`中定义,通常像是`$HOME/texmf` or `$HOME/.texliveXXXX`

如果这些变量没有定义,你需要手工指定.修改`local` Tree 可能需要 root 权限.建议修改 user tree, 因为在升级的时候,不会被覆盖.这样在备份系统的时候,可以一起备份.

现在,你需要告诉 Latex 有新文件.这取决于 LaTex 发行版.

1. 对于 TeXLive,运行`texhash`,可能需要 root 权限
2. 对于MikTeX,运行 `Settings (Admin)` and press the button marked `Refresh FNDB`

5. 最后,你需要告诉 LyX 有新的包可以使用.在LyX 中,运行 `Tools->Reconfigure` and then restart LyX

现在,新的文档 class 可以选择了,`Document->Settings->Document Class`.

### latex包安装方式2

首先要找到默认宏包所在目录,一般是:

```bash
/usr/share/texmf/tex/latex
/usr/share/texmf-texlive/tex/latex
```

1. 如果是安装一个新的宏包,就直接把宏包的压缩文件扔进第一个目录下,直接解压就行,注意解压后的文件里可能有安装说明,照着安装说明做就是了.
如果是更新一个宏包,一般都可以在第二个目录下找到,把原先的宏包重命名成`*-backup`,再解压新下载的宏包压缩文件,同时如果有安装说明的话,也照着做.
2. 之后要对宏包重新标记下,终端下执行

```bash
# texhash
```

`Log off/Log in`后,就完成了~

### latex pdf 裁剪

`texlive` 自带了一个叫做 `pdfcrop` 的 `perl` 脚本

使用方法如下：

`pdfcrop --margins 3 --clip input.pdf output.pdf; ` 或者

```bash
pdfcrop --clip --bbox '120 480 570 830' input.pdf output.pdf;
pdfcrop --clip --bbox '60 660 516 775' moban.pdf moban_crop.pdf && evince moban_crop.pdf  # 国科大试卷的裁减参数
```

四个数字的含义是,以左下角为原点,给出`left bottom right top`的数值,单位是`point`

`1 point`=`0.3527 mm`=`1/72 inch`.
A4纸张(mm) `210` * `297`=`595.4 point`*`842.1 point`.

## loop 设备

loop 设备 (循环设备)

[loop 设备 (循环设备)](https://blog.csdn.net/neiloid/article/details/8150629)

### loop 设备介绍

在类 UNIX 系统里,`loop` 设备是一种伪设备(pseudo-device),或者也可以说是仿真设备.它能使我们像块设备一样访问一个文件.

在使用之前,一个 `loop` 设备必须要和一个文件进行连接.这种结合方式给用户提供了一个替代块特殊文件的接口.因此,如果这个文件包含有一个完整的文件系统,那么这个文件就可以像一个磁盘设备一样被 `mount` 起来.

上面说的文件格式,我们经常见到的是 CD 或 DVD 的 ISO 光盘镜像文件或者是软盘(硬盘)的 `*.img` 镜像文件.通过这种 `loop mount` (回环`mount`)的方式,这些镜像文件就可以被 `mount` 到当前文件系统的一个目录下.

至此,顺便可以再理解一下 `loop` 的含义:对于第一层文件系统,它直接安装在我们计算机的物理设备之上；
而对于这种被 `mount` 起来的镜像文件(它也包含有文件系统),它是建立在第一层文件系统之上,
这样看来,它就像是在第一层文件系统之上再绕了一圈的文件系统,所以称为 `loop`.

在 Linux 里,`loop` 设备的设备名形如:

```bash
ls /dev/loop*
/dev/loop0  /dev/loop2  /dev/loop4  /dev/loop6
/dev/loop1  /dev/loop3  /dev/loop5  /dev/loop7
... ...
```

例如,要在一个目录下 mount 一个包含有磁盘镜像的文件,需要分 2 步走:

```bash
losetup /dev/loop0 disk.img           #使磁盘镜像文件与循环设备连结起来
mount /dev/loop0 /home/groad/disk_test   #将循环设备 mount 到目录 disk_test 下
```

经过上面的两个命令后,镜像文件就如同一个文件系统挂载在 `disk_test` 目录下,当然我们也可以往镜像里面添加文件.

其实上面的两个步骤可以写成一个步骤:

```bash
mount -t minix -o loop ./disk.img ./disk_test
```

## snap

[Ubuntu使用snap安装常用软件](https://www.jianshu.com/p/4049b97151a1)

什么是`snap`, `snap`是一种全新的软件包管理方式, 它类似一个容器拥有一个应用程序所有的文件和库, 各个应用程序之间完全独立. 
所以使用`snap`包的好处就是它解决了应用程序之间的依赖问题, 使应用程序之间更容易管理. 但是由此带来的问题就是它占用更多的磁盘空间. 

`Snap`的安装包扩展名是`.snap`, 类似于一个容器, 它包含一个应用程序需要用到的所有文件和库(`snap`包包含一个私有的`root`文件系统, 里面包含了依赖的软件包). 
它们会被安装到单独的目录；各个应用程序之间相互隔离. 使用`snap`有很多好处, 首先它解决了软件包的依赖问题；其次, 也使应用程序更容易管理. 

现在支持`snap`的应用并不多, `snap`软件包一般安装在`/snap`目录下.
