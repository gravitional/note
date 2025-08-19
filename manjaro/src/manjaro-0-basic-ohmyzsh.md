# manjaro

[Manjaro-KDE安装配置全攻略](https://zhuanlan.zhihu.com/p/114296129)
[Manjaro安装后调教(Gnome/KDE)|](https://www.airnan.cn/2024/10/29/jDDcZulZ)

## 安装

### 制作 USB 安装工具

[Writing to a USB Stick in Linux](https://wiki.manjaro.org/index.php/Burn_an_ISO_File#Writing_to_a_USB_Stick_in_Linux)

```bash
sudo dd bs=4M if=/home/tom/downloads/manjaro-kde-24.2.1-241216-linux612.iso of=/dev/sdc status=progress oflag=sync
```

安装的时候, 务必把语言选择为 `英文`.
微星主板上, 开机连续按下`F11`打开 UEFI 启动顺序界面,
选择 Manjaro 安装选项, 如果有 nvidia 显卡,
可以选择使用 proprietary 驱动(nvidia私有驱动)启动安装页面,
安装过程中将安装私有驱动.

### 系统安装

这里我假设你的电脑引导方式是 `UEFI`, 如果你的 `ssd` 是 `NVMe` 协议 `M.2` 的,
需要在进入安装界面之前先进去 `BIOS` 里面修改从硬盘的启动形式, 把 `RAID` 改成 `AHCI`,
保存退出, 否则进入安装界面你不会看到你的`NVMe`硬盘, 做好这件事, 其他就都和普通 `ssd` 一样了.

还需要注意的一点是, 修改成`AHCI`模式之后, 重新进入`Windows`时会有问题,
这个时候不要慌, 等电脑自动重启第三次的时候, 进入安全模式启动 `Windows`,
进去之后重启系统, 再次进入 `Windows` 就不需要安全模式了

插上 `U盘`, 在电脑`logo`出现之前狂按`F12`手动选择从`U`盘启动.
如果你的电脑有`NVIDIA`和 `Intel` 双显卡的话, 开机界面将 `drive` 改成 `nonfree`,
这样系统会自动帮你安装适配的 `NVIDIA` 驱动(简直太方便了)
如果这一步你没有改, 进去之后手动安装 `NVIDIA` 驱动千万不要自己随便安装,
这样很可能会导致下次启动进入`X-Window`界面失败, 具体怎么安装可以参考`Manjaro WiKi`的解决方案

为保安装一步成功请在启动安装程序之前先联网,
一般来讲, 你只要在 `Windows` 下给 `Manjaro` 预留出磁盘空间,
如果你有多个硬盘记得在安装窗口的最上面选对硬盘,
安装时候的分区方案 选择 `取代一个分区`, 点击你之前分好的硬盘空间, 接着下一步即可.

安装nvidia驱动参考 [manjaro 配置显卡](https://wiki.manjaro.org/index.php/Configure_Graphics_Cards/zh-cn).

如果你需要手动分区, 看下面

### 分区方案

例如在一块 `1TB` 的 SSD 上安装manjaro, 内存为 `32GB`,
选对硬盘之后, 安装程序推荐的默认分区方案, 按排列顺序是:

+ 300 MB; `fat32`; 标记为 `boot`, `esp`; 挂载点 `/boot/efi`; 安装 EFI grub 启动文件
+ 869.90 GB; `ext4`; 挂载点 `/`; 根目录
+ 34.32 GB; swap; 挂载为 linux swap 分区.

### 其他初始化设定

#### 安装Clash Verge Rev

[最萌的云导航站](https://release.cutecloud.net/)
[Manjaro安装后调教(Gnome/KDE) ](https://www.airnan.cn/2024/10/29/jDDcZulZ)
[参考官方安装文档: ](https://www.clashverge.dev/install.html)
[clash-verge-rev](https://github.com/clash-verge-rev/clash-verge-rev)

直接使用 yay 安装

```bash
sudo pacman -S yay
yay -S clash-verge-rev-bin
```

+ 定义命令行代理:

```bash
# ===================================== system proxy
# where proxy
proxyOn () {
export https_proxy="http://127.0.0.1:7897" http_proxy"=http://127.0.0.1:7897" all_proxy="socks5://127.0.0.1:7897"
  echo "HTTP Proxy on"
}
# where noproxy
proxyOff () {
  unset http_proxy
  unset https_proxy
  unset all_proxy
  echo "HTTP Proxy off"
}
```

+ firefox 需要手动设置使用代理;
设置->拉到最下面, 网络设置->设置->手动配置代理.
分别设置http代理和端口 为

    ```bash
    127.0.0.1, 7897
    ```

#### 生成 ssh key, git pull

参考 [linux-17.1-ssh.md](https://gitee.com/graviton/note/blob/master/linux/src/linux-17.1-ssh.md)

## plasma桌面 基本使用

[linux查看, 添加, 删除环境变量](https://blog.csdn.net/mayue_web/article/details/97023615)
[如何查看Linux桌面环境版本和详细信息? ](https://www.dongmanai.cn/post/3D61B932AaB8.html)

+ `Manjaro` KDE 桌面的大部分软件,
配置快捷键的 快捷键 是 `Ctrl+Alt+,`(逗号)

查看 KDE plasma 版本

```bash
plasmashell --version
```

Linux 查看环境变量
显示当前用户的所有环境变量

```bash
printenv
# 或
env
```

查看指定环境变量

```bash
printenv
# 或
env
```

使用`set`查看所有本地定义的环境变量.

### pacman 换源

启动`terminal`, 输入以下命令, 生成可用中国镜像站列表:

```bash
sudo pacman-mirrors -i -c China -m rank
```

在弹出的框中选一个最快的源, 一个就好, 选多了会降低速度.
完成之后需要执行:

```bash
# 3.刷新缓存
sudo pacman -Syy

# 4.更新系统
sudo pacman -Syu
```

### archlinuxcn

若要安装`archlinuxcn`的源,参考[ArchlinuxCN 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/archlinuxcn/)

打开配置文件: `sudo vim /etc/pacman.conf`, 在末尾输入:

```bash
[archlinuxcn]
Server = https://mirrors.ustc.edu.cn/archlinuxcn/$arch
Server = https://mirrors.tuna.tsinghua.edu.cn/archlinuxcn/$arch
Server = https://mirrors.hit.edu.cn/archlinuxcn/$arch
Server = https://repo.huaweicloud.com/archlinuxcn/$arch
```

保存退出, 接着更新缓存, 导入 `PGP`Keys:

```bash
sudo pacman -Syy && sudo pacman -S archlinuxcn-keyring
```

### 软件仓库镜像源

1. 设置 *软件商店* 的镜像源
    运行软件仓库软件, 点击右上角菜单, 选择首选项, 在常规选项卡中找到官方软件仓库,
    将镜像源调整到 `China`, 刷新列表即可.

2. 启用AUR和Flatpak
    在第三方选项卡中打开AUR和Flatpak开关, 关闭窗口后会自动刷新列表.

3.修改Flatpak源
    在这里有一个问题, 当Flatpak镜像源没有修改时, 启用Flatpak并关闭软件仓库后便无法再次打开,
    因此, 需要手动切换以下国内的镜像源, 然后重启即可

```bash
sudo flatpak remote-modify flathub --url=https://mirror.sjtu.edu.cn/flathub
```

## 安装软件

### 安装 base-devel, vscode

[如何在 Arch Linux 上安装 Visual Studio Code](https://cn.linux-console.net/?p=12138)
[Visual Studio Code](https://wiki.archlinux.org/title/Visual_Studio_Code)
[manjaro](https://blog.csdn.net/qq_19000143/article/details/103111617)

在使用 yay 安装 vscode 时会报错, 没安装 fakeroot, binutils 等打包基本工具,
需要安装此基本 devel 工具链.

```bash
sudo pacman -S base-devel
# 安装 vscode 预编译二进制包
yay -S visual-studio-code-bin
```

### yay

`Manjaro` 背靠`Arch`软件仓库,

```bash
sudo pacman -S yay
```

`yay`是一个用`Go`语言写的一个`AUR`助手, 有些时候官方仓库没有你想要的软件, 就需要通过`yay`来安装,有了`yay`, 以后就不用`sudo pacman`了

### [Yakuake](https://wiki.archlinuxcn.org/zh-my/Yakuake)

Yakuake 是一个适用于 KDE 的下拉式终端, 类似于 GNOME 的 Guake, Tilda或 Quake 中使用的终端.

安装后, 您可以在终端启动 yakuake:

```bash
 yakuake
```

Yakuake 启动后, 您可以点击"配置 Yakuake" 通过点击菜单按钮
(界面右下中间)并选择配置键盘快捷键以更改收放终端的热键, 默认是 F12.

### fcitx5 输入法(Flexible Input Method Framework)

[Manjaro KDE —— Fcitx5(输入法)](https://zhuanlan.zhihu.com/p/577060385)
[Fcitx5官方文档](https://fcitx-im.org/wiki/Using_Fcitx_5_on_Wayland)

复制粘贴命令:

```bash
sudo pacman -S fcitx5
sudo pacman -S fcitx5-configtool
sudo pacman -S fcitx5-qt
sudo pacman -S fcitx5-gtk
sudo pacman -S fcitx5-chinese-addons
sudo pacman -S fcitx5-material-color
sudo pacman -S kcm-fcitx5
sudo pacman -S fcitx5-lua
```

修改环境变量(调用Fctix5输入法),
**但是如果使用 wayland 显示服务器, 推荐不设置这个**

```bash
sudo vim /etc/environment

# 复制粘贴代码:
GTK_IM_MODULE=fcitx
QT_IM_MODULE=fcitx
XMODIFIERS=@im=fcitx
INPUT_METHOD=fcitx5
SDL_IM_MODULE=fcitx5

# 保存, 确定, 退出; 重启; 或者注销重新登入也可以
reboot
```

Fcitx5 配置中文输入(`xxx` 为选项)

电脑重启右下角出现 `输入法图标`;
右击 `输入法图标`, 点击菜单里的 `配置` 选项打开 输入法的配置页;
点击右下角 `添加输入法` 出现输入法的菜单;
有 `双拼, 拼音, 五笔` 等输入法, 选中你要添加的输入法再点击右下角的 `添加`;
(笔者用的是小鹤双拼, 点击 双拼输入法横条右边的  `配置图标` 配置小鹤双拼就行了);
还能开启———— 云拼 和 输入预测.

![ime1](https://pic2.zhimg.com/v2-940ae8eba1e4b9c3e88e8576f3f1a425_1440w.jpg)
![ime2](https://pic4.zhimg.com/v2-b882f8bf3197efe6e7cc3542db0d2807_1440w.jpg)
![ime3](https://pic2.zhimg.com/v2-13c6178c89a89cc5f838bf34ec7e3fe9_1440w.jpg)
![ime4](https://pic2.zhimg.com/v2-e1b4a012a648048bad99fce92dc9d9db_1440w.jpg)

#### CapsLock  切换输入法, Xmodmap

[如何在 Linux 使用 Caps Lock 切换输入法](https://wancat.cc/post/capslock/)
[Xmodmap](https://wiki.archlinuxcn.org/wiki/Xmodmap)
[Changing your caps lock into Ctrl in X](http://efod.se/capslock/)

或者用 bash 输入 `fcitx5-configtool` 也可以直达此界面.
一般可设置  `输入法`->`分组1`:

+ 输入法关闭 `键盘--汉语`
+ 输入法开启 `双拼`

然后是 `配置全局选项`,
切换, 禁用输入法, 选择 `Super+空格`, `Multi_key`
把其他的一些不需要的快捷键禁用掉.

设置 切换大小写按键为 `Caps_Lock`(`Multi_key`),

我们先查询一下 `Caps_Lock` 对应的 `keycode`

```bash
xmodmap -pke | grep Caps_Lock

keycode  66 = Caps_Lock NoSymbol Caps_Lock
```

可以看到键盘上的 `Caps_Lock` 对应的是 keycode `66`,
接下來把 66 改成对应 `Multi_key`.

```bash
xmodmap -pke > ~/.Xmodmap  #存储设定
vim ~/.Xmodmap
# 将 keycode 66 处改成;  ! 表示注释
keycode  66 = Multi_key NoSymbol Multi_key
# 在最底下加入
clear lock

xmodmap ~/.Xmodmap     # 载入设定
```

根据 ArchLinux wiki, `~/.Xmodmap` 会自动被 GDM, SDDM, XDM 载入,
如果是使用其他的请自行设定.

最后打开 ``fcitx5-configtool``, 将输入法切换按键设定为 `Multi_key`.
建议保险的话多设定一组切换键, 避免设定失败卡在中文无法切换.

![ime3](https://wancat.cc/img/capslock/fcitx-config.png)

#### Fcitx5-Material-Color(自定义输入法外观)

```bash
编辑配置文件
sudo nano ~/.config/fcitx5/conf/classicui.conf

复制粘贴代码:
# 垂直候选列表
Vertical Candidate List=False

# 按屏幕 DPI 使用
PerScreenDPI=True

# Font (设置成你喜欢的字体)
Font="思源黑体 CN Medium 13"

# 主题
Theme=Material-Color-Blue

! 详细使用方法点击标题直达开源项目!
(#号后面的内容也能复制进去; )
```

![ime5](https://pica.zhimg.com/v2-a0662fd7e6c7c820f2aa9c0c3f70278e_1440w.jpg)

##### 另一种安装方式

安装拼音输入法, 安装`fcitx5`(输入法框架)

```bash
yay -S fcitx5-im
```

配置`fcitx5`的环境变量:

```bash
nano ~/.pam_environment
```

内容为:

```pam
GTK_IM_MODULE DEFAULT=fcitx
QT_IM_MODULE  DEFAULT=fcitx
XMODIFIERS    DEFAULT=\@im=fcitx
SDL_IM_MODULE DEFAULT=fcitx
```

安装中文维基百科词库:

```bash
yay -S fcitx5-pinyin-zhwiki
```

配置主题:

```bash
yay -S fcitx5-material-color
```

[大佬制作的fcitx5主题](https://github.com/ayamir/fcitx5-nord)

安装

```bash
git clone https://github.com/tonyfettes/fcitx5-nord.git
mkdir -p ~/.local/share/fcitx5/themes/
cd fcitx5-nord
cp -r nord-Dark/ nord-Light/ ~/.local/share/fcitx5/themes/
```

然后编辑`~/.config/fcitx5/conf/classicui.conf`, 更改主题指定的行,

```bash
Theme=Nord-Dark
# or
Theme=Nord-Light
```

然后重启`fcitx5`, `fcitx5 -r`

完成之后就可以注销, 重新登录之后打开`fcitx5-configtool`编辑一下相应配置.

## 字体大小

调整 plasma 桌面字体大小
settings-> Appearance & style -> Text & fonts

### 调整 fcitx5 输入法候选托盘字体大小

[fcitx5设置字体和大小](https://segmentfault.com/a/1190000044892285)

终端输入 `fcitx5-configtool` 回车, 打开输入法设置界面
也就是 `设置` 中的
`Language & Time`->`Input Method`,
页面下方 `Configure addons...`-> `Classic User Interface`->
点击右侧设置按钮, 即可设置各种字体大小.
影响输入法候选栏大小主要是其中的 `Font`

![img](https://segmentfault.com/img/bVdcwHe)
![img](https://segmentfault.com/img/bVdcwHf)
![img](https://segmentfault.com/img/bVdcwHg)
![img](https://segmentfault.com/img/bVdcwHe)

### kate markdown lsp 语言服务器

[artempyanykh/marksman](https://github.com/artempyanykh/marksman)

```bash
yay -S marksman
```

### 配置 oh-my-zsh

首先说一下这个过程不需要使用`pacman/yay`安装软件, 首先修改默认`shell`为`zsh`

```bash
chsh -s /usr/bin/zsh
#安装ohmyzsh, 使用国内镜像
REMOTE=https://gitee.com/mirrors/oh-my-zsh.git
sh -c "$(curl -fsSL https://gitee.com/mirrors/oh-my-zsh/raw/master/tools/install.sh)"
```

如果之前用 github 源安装的, 可以修改地址到国内

```bash
cd ~/.oh-my-zsh
git remote set-url origin https://gitee.com/mirrors/oh-my-zsh.git
git pull
```

### zsh-syntax-highlighting

安装`zsh-syntax-highlighting`: [提供命令高亮](https://github.com/zsh-users/zsh-syntax-highlighting)
作者建议把这个插件放在插件列表的最后

```bash
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
```

#### 安装[`autosuggestions`](https://github.com/zsh-users/zsh-autosuggestions)

作用是记住你之前使用过的命令, 使用国内镜像代替 github

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

#### 安装 `incr` (这个插件会拖慢`zsh`的速度, 新手入门可以试试)

```bash
git clone git://github.com/makeitjoe/incr.zsh $ZSH_CUSTOM/plugins/incr
```

启用所有插件

```bash
nano ~/.zshrc
# 将 plugins=(git) 改为:
plugins=(git tmux  sudo extract zsh-autosuggestions zsh-syntax-highlighting)
```

这个`sudo`是`ohmyzsh`自带的插件, 功能是在你输入的命令的开头添加`sudo `, 方法是`双击Esc`
`extract`也是自带插件, 不用再去记不同文件的解压命令, 方法是`extract +你要解压的文件名`.
保存退出之后, 手动修改`konsole`的默认`bash`为`zsh`: (右键`Konsole - 编辑当前方案`),打开`konsole`执行`source ~/.zshrc`, 配置完毕.

### 安装Vimplus

现代化的vim插件管理工具, 开箱即用, 不使用vim的可以略过

```bash
git clone https://github.com/chxuan/vimplus.git ~/.vimplus
# gitee 地址为
git clone git@gitee.com:chxuan/vimplus.git ~/.vimplus

cd ~/.vimplus
./install.sh
```

### AppImage,  AppImageLauncher

[在Ubuntu中运行和管理AppImage](https://blog.csdn.net/u012899618/article/details/144206660)

```bash
yay -S appimagelauncher
```

### 中文字体补充

```bash
sudo pacman -S adobe-source-han-sans-cn-fonts
sudo pacman -S wqy-microhei
```

### 安装BAT国内软件

#### 安装Tim

```bash
yay -S deepin-wine-tim
```

安装过程中出现选择输入`n`就好

切换`deepin-wine`环境

```bash
sh /opt/deepinwine/apps/Deepin-Tim/run.sh -d
```

有问题直接去 [countstarlight/deepin-wine-tim-arch ](https://github.com/countstarlight/deepin-wine-tim-arch/issues) 开issue反馈就好了,
如果这个版本的卡或者有其他问题, 建议安装:

```bash
yay -S deepin.com.qq.office
```

如果这个也没办法装, 则使用`linuxqq`

```bash
yay -S linuxqq
```

#### 安装微信

[微信](https://wiki.archlinuxcn.org/wiki/%E5%BE%AE%E4%BF%A1)

微信打包于 [wechat-binAUR](https://aur.archlinux.org/packages/wechat-bin/), 并提供带有一些针对微信的修复包 [wechatAUR](https://aur.archlinux.org/packages/wechat/).
后者有进程管理, 沙盒(可选), 输入法及 HiDPI 修复等功能.
也可以使用 Flatpak 从 Flathub 安装 com.tencent.WeChat.

aur 现在已经有官方的 wechat-linux 版
aur/wechat-bin 4.0.1.11-2 (+4 1.43) (已安装)  This is a repackage of WeChat.

```bash
yay -S wechat-bin
```

`electron`版:

```bash
yay -S electron-wechat
```

#### 百度网盘

```bash
yay -S baidunetdisk-bin
```

#### 阿里云盘命令行版

```bash
yay -S aur/aliyunpan-go-bin
```

### 安装其他软件

`tldr`(Too Long; Don't Read): 帮你更加高效地学习`linux`命令

```bash
pip install --user tldr
```

如果下载太慢:

```bash
mkdir -p ~/.config/pip
nano ~/.config/pip/pip.conf
```

写入如下内容

```bash
[global]
index-url = https://pypi.tuna.tsinghua.edu.cn/simple
```

这样就永久地修改了用户级别的`pip`下载源

如果安装失败则用:

```bash
yay -S tldr
```

`ranger`: 终端文件浏览器

```bash
yay -S ranger
```

`wps`:中文版, 想要英文版把后面那个包去掉

```bash
yay -S wps-office wps-office-mui-zh-cn
```

如果你使用`fcitx5`的话, 还需要修改`/usr/bin/wps`和`/usr/bin/wpp`, 将下面这行代码加到文件开头:

```bash
export QT_IM_MODULE="fcitx5"
```

网易云音乐:

```bash
yay -S netease-cloud-music
```

这样得到的版本不能在搜索框输入中文,

高颜值, 开发活跃的第三方客户端:

```bash
yay -S yesplaymusic
```

qq音乐:

```bash
yay -S qqmusic-bin
```

一个支持全平台听歌的软件: `FeelUown`

```bash
yay -S feeluown
```

根据装完之后的提示装对应平台的依赖

chrome

```bash
yay -S google-chrome
```

百度网盘:

```bash
yay -S baidunetdisk
```

或者命令行(CLI)的:

```bash
yay -S baidupcs-go
```

坚果云:

```bash
yay -S nutstore
```

为知笔记: 全平台通用, 有云端同步, 支持md的笔记

```bash
yay -S wiznote
```

Typora: 我认为最舒适的md编辑器

```bash
yay -S typora
```

***
`flameshot`: 强大的截图工具

```bash
yay -S flameshot
```

设置快捷键启动的方式

`设置->快捷键->自定义快捷键 -> 编辑 -> 新建 -> 全局快捷键 -> 命令/URL`, 转到`触发器`标签, 设置习惯的快捷键.
转到`动作`标签,在`命令/URL`中填上`/usr/bin/flameshot gui`

***
`timeshift` : 强大好用的备份, 回滚系统工具

```bash
yay -S timeshift
```

XDM: Linux下最快的下载神器

```bash
yay -S xdman
```

建议直接去[https://subhra74.github.io/xdm/](https://subhra74.github.io/xdm/)下载压缩包安装, 比命令行快很多

***
`calibre`: 电子书管理神器

```bash
yay -S calibre
```

系统托盘那开启夜间颜色控制, 不需要安装redshift了

### 字体

使用Windows/Mac OS字体:
[Fonts (简体中文) - ArchWiki​](https://wiki.archlinux.org/index.php/Fonts_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#Microsoft_%E5%AD%97%E4%BD%93)

这里建议直接拷贝字体文件

[渲染效果优化](https://zhuanlan.zhihu.com/p/343934880)

### 美化

安装`latte-dock`

```bash
yay -S latte-dock
```

添加一个新空面板, 默认会出现在桌面上方, 可以删除下面自带的面板, 在新面板上添加必要的部件: 应用程序面板, 数字时钟, 托盘, 还可以加全局菜单, 显示面板等等

启动`latte-dock`, 下方就会出现一个dock栏, 具体配置看自己爱好.
除dock栏中时钟的方法: 右键`配置lattedock`,接着右键时钟,选择`移除`就好了

***
`设置-外观`中选择你喜欢的主题什么的安装并且应用即可
`设置-工作区行为-桌面特效` 中可以启用一些华丽的特效
`设置-开机和关机`中更改登录屏幕等效果
`设置-工作区行为-常规行为-点击行为`设置双击打开文件/文件夹的设置

修改`/home/user`下的用户文件夹名称为英文:
先去手动修改文件夹名称, 然后在 `设置 -> 应用程序 -> 地点` 中修改

## mathematica

[Mathematica 激活指南](https://tiebamma.github.io/InstallTutorial)

## texlive 等其他软件, tex 字体

[otf-stix](https://aur.archlinux.org/packages/otf-stix)

```bash
yay -S texlive-bin texlive-doc texlive-fontsrecommended texlive-games texlive-meta texlive-latexextra texlive-latex texlive-latexrecommended texlive-xetex  texlive-mathscience texlive-pstricks texlive-fontsextra texlive-context texlive-luatex texlive-binextra  texlive-bibtexextra texlive-pictures texlive-metapost texlive-langcjk  texlive-humanities texlive-formatsextra  texlive-fontutils texlive-langchinese
yay -S clash-verge-rev-bin
yay -S powershell-lts-bin
yay -S neofetc
yay -S vdhcoapp
yay -S lyx
yay -S qtcreator
yay -S microsoft-edge-beta-bin
yay -S otf-latin-modern
yay -S otf-stix
```