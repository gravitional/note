# manjaro

[Manjaro-KDE安装配置全攻略](https://zhuanlan.zhihu.com/p/114296129)

## 系统安装

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
安装时候的分区方案直接选择取代一个分区, 点击你之前分好的硬盘空间, 接着下一步即可.

如果你需要手动分区:
如果你的硬盘是 `ssd+hdd`, 并且打算把系统装在`hdd`下的话, 建议直接用 `windows`的 `efi` 分区,
`hdd`中划分一个区出来挂载`/`区, 或者在`ssd`中分一个
`150MB`(当然大点也行)的区出来挂载`/boot/efi`, 安装完毕重启即可.

### 换源

启动`terminal`, 输入:

```bash
sudo pacman-mirrors -i -c China -m rank
```

在弹出的框中选一个最快的源, 一个就好, 选多了会降低速度.
若要安装`archlinuxcn`的源,参考[ArchlinuxCN 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/archlinuxcn/)

打开配置文件: `sudo vim /etc/pacman.conf`, 在末尾输入:

```bash
[archlinuxcn]
Server = https://mirrors.tuna.tsinghua.edu.cn/archlinuxcn/$arch
```

保存退出, 接着更新缓存, 导入 `PGP`Keys:

```bash
sudo pacman -Syy && sudo pacman -S archlinuxcn-keyring
```

### 安装软件

`Manjaro` 背靠`Arch`软件仓库,

```bash
sudo pacman -S yay
```

`yay`是一个用`Go`语言写的一个`AUR`助手, 有些时候官方仓库没有你想要的软件, 就需要通过`yay`来安装,有了`yay`, 以后就不用`sudo pacman`了

***
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

完成之后就可以注销, 重新登录之后打开`fcitx5-configtool`编辑一下相应配置:

### 配置ohmyzsh

首先说一下这个过程不需要使用`pacman/yay`安装软件, 首先修改默认`shell`为`zsh`

```bash
chsh -s /usr/bin/zsh
#安装ohmyzsh
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | sh
```

安装`zsh-syntax-highlighting`: [提供命令高亮](https://github.com/zsh-users/zsh-syntax-highlighting)
作者建议把这个插件放在插件列表的最后

```bash
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
```

+ 安装[`autosuggestions`](https://github.com/zsh-users/zsh-autosuggestions): 记住你之前使用过的命令

```bash
git clone git://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
```

+ 安装 `incr`: (需要注意的是这个插件会拖慢`zsh`的速度, 新手入门可以试试)

```bash
git clone git://github.com/makeitjoe/incr.zsh $ZSH_CUSTOM/plugins/incr
```

启用所有插件

```bash
nano ~/.zshrc
# 将 plugins=(git) 改为:
plugins=(git tmux  sudo extract  zsh-autosuggestions zsh-syntax-highlighting)
```

这个`sudo`是`ohmyzsh`自带的插件, 功能是在你输入的命令的开头添加`sudo `, 方法是`双击Esc`
`extract`也是自带插件, 不用再去记不同文件的解压命令, 方法是`extract +你要解压的文件名`.
保存退出之后, 手动修改`konsole`的默认`bash`为`zsh`: (右键`Konsole - 编辑当前方案`),打开`konsole`执行`source ~/.zshrc`, 配置完毕.

### 安装Vimplus

(现代化的vim插件管理工具, 开箱即用, 不使用vim的可以略过

```bash
git clone https://github.com/chxuan/vimplus.git ~/.vimplus
cd ~/.vimplus
./install.sh
```

### 安装腾讯系软件

安装Tim

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

***
安装微信

`deepin-wine`版:

```bash
yay -S deepin-wine-wechat
```

切换到`deepin-wine`环境:

```bash
/opt/apps/com.qq.weixin.deepin/files/run.sh -d
```

关于字体发虚问题:

在切换到`deepin-wine`环境后, 在`terminal`输入下面的指令:

```bash
env WINEPREFIX="$HOME/.deepinwine/Deepin-TIM" /usr/bin/deepin-wine winecfg
```

在弹出的窗口中选择`windows xp`, 将`DPI`调大(默认是`96`), 我调成了`120`

微信的同样, 只需要将命令改为:

```bash
env WINEPREFIX="$HOME/.deepinwine/Deepin-WeChat" /usr/bin/deepin-wine winecfg
```

`electron`版:

```bash
yay -S electron-wechat
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

***
修改`/home/user`下的用户文件夹名称为英文:
先去手动修改文件夹名称, 然后在 `设置 -> 应用程序 -> 地点` 中修改

***
双系统时间同步

按照[ Arch WiKi ](https://wiki.archlinux.org/index.php/System_time_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))的建议, 这里修改`Windows`系统的注册表:
`win+X` 以管理员方式打开PowerShell, 输入

```bash
reg add "HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\TimeZoneInformation" /v RealTimeIsUniversal /d 1 /t REG_QWORD /f
```

如果你的`Windows`是`32`位的, 把上述代码中的`REG_QWORD`改成`REG_DWORD`

同时禁用`Windows`的自动同步时间功能

***
查看系统信息

```bash
yay -S neofetch lolcat # lolcat 只是渐变色的工具
neofetch | lolcat
```

装好`Manjaro`必须要有的习惯:

1. 必须要做`timeshift`, 以防你哪天玩坏只能重装
2. 每天要开机执行一遍`sudo pacman -Syyu`

虽说`Manjaro`相对`Arch`应该稳定一点, 但终究是滚动发行版, 还是有滚挂的风险, 防止滚挂的最好办法就是 及时滚 长时间不更新必挂.

## 维护

### manjaro 开机红色提示

`Failed to start Load/Save Screen Backlight Brightness of amdgpu_bl0`

[Load/Save Screen Backlight Brightness](https://www.linux.org/threads/failed-to-start-load-save-screen-backlight-brightness-of-amdgpu_bl1.31998/)

开关机的时候出现上面的错误提示, 查看运行状态

```bash
sudo systemctl status systemd-backlight@backlight:amdgpu_bl0
```

手动启动

```bash
sudo systemctl start systemd-backlight@backlight:amdgpu_bl0
```

关于 `systemd` 可以参考:
[Systemd 入门教程: 命令篇](http://www.ruanyifeng.com/blog/2016/03/systemd-tutorial-commands.html)
[systemd](https://wiki.archlinux.org/index.php/Systemd_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))
[Kernel parameters ](https://wiki.archlinux.org/index.php/Kernel_parameters)

添加`systemd`开机启动配置, `sudo vim /etc/systemd/system/startup-brightness.service`

```systemd
[Unit]
Description=Dummy service for attempting to start the problematic amdgpu_bl0 service

[Service]
Type=simple
ExecStart=systemctl start systemd-backlight@backlight:amdgpu_bl0

[Install]
WantedBy=multi-user.target
```

然后启用: `sudo systemctl enable startup-brightness.service`. 下次开机虽然还有提示, 但是`systemd-backlight@backlight:amdgpu_bl0`会自动运行.

***
也可以手动调节亮度, [manjaro 手动调节屏幕亮度](https://zhuanlan.zhihu.com/p/138880080)

亮度由`ACPI`内核模块控制, 这个模块的接口在以下位置: `/sys/class/backlight`.
使用`root`用户进入这个文件夹, 其中`max_brightness`表示亮度的最大值, 笔者的设备显示为`255`, 想要修改亮度的话直接修改`brightness`即可: `echo 255 > brightness`

***
[Failed to start Load/Save Screen Backlight Brightness of](https://bbs.archlinux.org/viewtopic.php?id=254654)

`ls /sys/class/backlight`可以查看`linux`背光子系统.
对于报错的另一个设备: `Failed to start Load/Save Screen Backlight Brightness of backlight:acpi_video1`, 提示设备不存在, 直接禁用这个服务:
`sudo systemctl mask systemd-backlight@backlight:acpi_video0.service`

### manjaro grub 内核参数

***
[systemd](https://wiki.archlinux.org/index.php/Systemd_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

***
`apparmor=1 security=apparmor`
[AppArmor](https://wiki.archlinux.org/index.php/AppArmor#Installation)

AppArmor是基于Linux安全模块(Linux Security Modules, LSM)实施的强制性访问控制(Mandatory Access Control,MAC)系统.

***
[udev](https://wiki.archlinux.org/index.php/Udev_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))
`udev` 是 `Linux` 内核的设备管理器. 总的来说, 它取代了 `devfs` 和 `hotplug`, 负责管理 `/dev` 中的设备节点.
同时, `udev` 也处理所有用户空间发生的硬件添加, 删除事件, 以及某些特定设备所需的固件加载.

与传统的顺序加载相比, `udev` 通过并行加载内核模块提供了潜在的性能优势.
异步加载模块的方式也有一个天生的缺点: 无法保证每次加载模块的顺序, 如果机器具有多个块设备, 那么它们的设备节点可能随机变化. 例如如果有两个硬盘, `/dev/sda` 可能会随机变成`/dev/sdb`

`Debug output`

要获取硬件调试信息, 请使用内核参数`udev.log-priority=debug`.  或者, 您可以设置`/etc/udev/udev.conf`

```bash
udev_log="debug"
```

***
[Kernel parameters ](https://wiki.archlinux.org/index.php/Kernel_parameters)
[Power management/Suspend and hibernate](https://wiki.archlinux.org/index.php/Power_management/Suspend_and_hibernate#Hibernation)
[Blacklisting](https://wiki.archlinux.org/index.php/Kernel_module#Blacklisting)

`resume=UUID=0f163abf-8e60-4626-a4de-54332c64db51`. `resume`指定从休眠状态唤醒时要使用的`swap`设备.

在升级到内核版本`4.15.3`后, 唤醒系统时可能会黑屏, 只在黑屏上留下一个静止不动的鼠标指针.  `Blacklisting` 这个模块 `nvidiafb` 可能会有帮助.

***
方法1: 在 `/etc/modprobe.d/` 中创建 `.conf` 文件, 使用 `blacklist` 关键字屏蔽不需要的模块, 例如如果不想装入 `pcspkr` 模块:

```bash
vim /etc/modprobe.d/nobeep.conf
# Do not load the pcspkr module on boot
blacklist pcspkr
```

注意:  `blacklist` 命令将屏蔽一个模板, 所以不会自动装入, 但是如果其它非屏蔽模块需要这个模块, 系统依然会装入它. 要避免这个行为, 可以让 `modprobe` 使用自定义的 `install` 命令, 而不是像往常一样将模块插入内核, 因此您可以通过以下方式强制模块始终无法加载:

```bash
/etc/modprobe.d/blacklist.conf
...
install MODULE /bin/true
...
```

****
方法2: 从引导加载程序,例如`grub`, 将模块列入黑名单.
如`Kernel parameters`中所述, 只需将`module_blacklist=modname1,modname2,modname3` 添加到引导加载程序的内核行中即可.
注意:  将多个模块列入黑名单时, 请注意, 它们之间仅用逗号分隔.  空格或其他内容可能会破坏语法.

[Kernel parameters](https://wiki.archlinux.org/index.php/Kernel_parameters)
对于初学者, 建议编辑 `/etc/default/grub` 并将您的内核选项添加至 `GRUB_CMDLINE_LINUX_DEFAULT` 行:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="quiet splash"
GRUB_CMDLINE_LINUX_DEFAULT="module_blacklist=nvidiafb"
```

然后重新生成 `grub.cfg` 文件:

```bash
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

### vscode 登陆报错

[Troubleshooting keychain issues](https://code.visualstudio.com/docs/editor/settings-sync#_troubleshooting-keychain-issues)
[Sync requests to re-login every 2 minutes](https://github.com/microsoft/vscode/issues/92972)
[install gnome keyring and qtkeychain](https://rtfm.co.ua/en/linux-the-nextcloud-client-qtkeychain-and-the-the-name-org-freedesktop-secrets-was-not-provided-by-any-service-files-error/)

`KDE` 环境下, `vs-code`登陆账户无法保存, 下次启动仍然提示登陆. 问题出在`keychain`. 解决方法是安装 gnome keyring and qtkeychain

```bash
yay -S qtkeychain gnome-keyring
# 验证现在存在
ls -l /usr/share/dbus-1/services/ | grep secret
cat /usr/share/dbus-1/services/org.freedesktop.secrets.service
```

### vscode 更新报错

[Arch Linux Chinese Community Repository](https://github.com/archlinuxcn/repo)

vscode 的二进制包`visual-studio-code-bin`位于`archlinucn`仓库, 可能是由于 `PGP keys` 更新, 执行

```bash
sudo pacman -Syy && sudo pacman -S archlinuxcn-keyring
```

### lyx 打开文件窗口报错

[/usr/lib/qt/plugins/kf5/kio/file.so](https://forum.manjaro.org/t/unable-to-create-io-slave-klauncher-said-error-loading-usr-lib-qt-plugins-kf5-kio-file-so/1217)

类似于

    Unable to create io-slave. klauncher said: Error loading  '/usr/lib/qt/plugins/kf5/kio/file.so'

+ 如果进行了系统更新, 应该尝试重新启动你的设备.
+ 如果你确实更新了,请确保没有部分更新.

```bash
sudo pacman-mirrors -f && sudo pacman -Syyu
```

甚至重新安装`kio`也会有帮助, 然后按照上面的建议重新启动.

## archwiki 帮助

[Help:Reading](https://wiki.archlinux.org/index.php/Help:Reading_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

### 安装软件包

绝大部分文章仅是给出软件包的名字, 不会列出详细的软件包安装命令.

***
官方软件包:如果软件包位于 官方软件仓库, 安装指令是这样: `Install the foobar package`. 这意味着需要执行:

```bash
# pacman -S foobar
```

***
Arch 用户仓库(AUR): 如果软件包来自AUR, 你会看到:`Install the foobarAUR package.`

这意味这您需要打开`foobarAUR`链接, 下载 `PKGBUILD`, 解压, 验证内容, 然后在文件目录执行:

```bash
$ makepkg -sri
```

### 控制 systemd 单元

绝大部分文章仅仅要求你`启动`(start),`启用`(enable),`停止`(stop),`重启`(restart)`systemd` 单元(例如: 服务), 不会列出详细的命令. 你会看见类似这样的说明:
`启动 example.service.`

这意味着你需要执行

```bash
# systemctl start example.service
```

`systemd` 页面包含了管理 systemd 单元的详细介绍.  下面列出一些常用的命令:

立即激活单元: `# systemctl start <单元>`
立即停止单元: `# systemctl stop <单元>`
重启单元: `# systemctl restart <单元>`
重新加载配置: `# systemctl reload <单元>`
输出单元运行状态: `$ systemctl status <单元>`
检查单元是否配置为自动启动: `$ systemctl is-enabled <单元>`
开机自动激活单元: `# systemctl enable <单元>`
设置单元为自动启动并立即启动这个单元:`# systemctl enable --now unit`
取消开机自动激活单元: `# systemctl disable <单元>`
禁用一个单元(禁用后, 间接启动也是不可能的): `# systemctl mask <单元>`
取消禁用一个单元: `# systemctl unmask <单元>`

## pacman

[pacman](https://wiki.archlinux.org/index.php/Pacman_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))
[PACMAN(8)](https://man.archlinux.org/man/pacman.8)

使用过其它发行版的用户, 可以参考 Pacman Rosetta 中的对比.
[Pacman Rosetta](https://wiki.archlinux.org/index.php/Pacman_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)/Rosetta_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

### pacman-mirrors

使用下面的命令选择较快的镜像.

```bash
sudo pacman-mirrors -i -c China -m rank
```

***
`pacman-mirrors`:产生Manjaro Linux 的镜像列表

+ `-i,--interactive [-default]`: 互动模式. `-default`强迫载入默认的镜像文件, 忽略任何自定义pool.
+ `-c`: 指定国家
+ `-m,--method METHOD`: 默认的方法是`rank`, 但可以选择`random`. `rank`: 按访问延迟排列镜像. `random`:随机排列镜像.

### 安装指定的包

安装或者升级单个软件包, 或者一列软件包(包含依赖包), 使用如下命令:

```bash
# pacman -S package_name1 package_name2 ...
```

用正则表达式安装多个软件包(参见 这个帖子):

```bash
# pacman -S $(pacman -Ssq package_regex)
```

有时候在不同的软件仓库中, 一个软件包有多个版本(比如`[extra]`和`[testing]`). 可以选择一个来安装:

```bash
# pacman -S extra/package_name
```

安装多个含有相似名称的软件包, 而并非整个包组或全部匹配的软件包;  例如, `plasma`:

```bash
# pacman -S plasma-{desktop,mediacenter,nm}
```

当然, 可以多层扩展, 并不作限制:

```bash
# pacman -S plasma-{workspace{,-wallpapers},pa}
```

### 虚包

虚拟软件包是一个特殊的软件包, 它本身并不存在, 但由一或多个其它软件包提供.
虚拟软件包允许其它软件包不以某一个特定的包为依赖, 以应对有多个候选的情况. 虚包不能用它们的名称安装, 相反它们会在你安装提供虚包的软件包时被安装到你的系统中.

### 安装包组

一些包属于一个可以同时安装的软件包组. 例如, 运行下面的命令

```bash
# pacman -S gnome
```

会提醒用户选择 `gnome` 内需要安装的包.
有的包组包含大量的软件包, 有时用户只需其中几个. 除了逐一键入序号外, `pacman` 还支持选择或排除某个区间内的的软件包:

```bash
Enter a selection (default=all): 1-10 15
```

这将选中序号 `1` 至 `10` 和 `15` 的软件包. 而

```bash
Enter a selection (default=all): ^5-8 ^2
```

将会选中除了序号 `5` 至 `8` 和 `2` 之外的所有软件包.

想要查看哪些包属于 `gnome` 组, 运行:

```bash
# pacman -Sg gnome
```

也可以访问[https://archlinux.org/groups/](https://archlinux.org/groups/) 查看可用的包组.

注意:  如果列表中的包已经安装在系统中, 它会被重新安装, 即使它已经是最新的. 可以用 `--needed` 选项覆盖这种行为.

### 删除软件包

删除单个软件包, 保留其全部已经安装的依赖关系

```bash
# pacman -R package_name
```

删除指定软件包, 及其所有没有被其他已安装软件包使用的依赖关系:

```bash
# pacman -Rs package_name
```

上面这条命令在移除包含其他所需包的组时有时候会拒绝运行. 这种情况下可以尝试

```bash
# pacman -Rsu package_name
```

要删除软件包和所有依赖这个软件包的程序:
警告:  此操作是递归的, 请小心检查, 可能会一次删除大量的软件包.

```bash
# pacman -Rsc package_name
```

要删除一个被其他软件包依赖的软件包, 但是不删除依赖这个软件包的其他软件包:
警告:  此操作有破坏系统的能力, 应该尽量避免使用. 详情请看`避免某些 Pacman 命令`.

```bash
# pacman -Rdd package_name
```

pacman 删除某些程序时会备份重要配置文件, 在其后面加上*.pacsave扩展名. -n 选项可以避免备份这些文件:

```bash
pacman -Rn package_name
```

注意:  pacman 不会删除软件自己创建的文件(例如主目录中的"点文件"不会被删除. )

### 升级软件包

警告:

>建议用户遵守[System maintenance (简体中文)#更新系统](https://wiki.archlinux.org/index.php/Pacman_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)/Rosetta_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))的指导, 定期更新系统, 并不盲目地执行这些命令.
>Arch 只支持系统完整升级, 详细参见System maintenance (简体中文)#不支持部分升级和安装软件包.

一个 `pacman` 命令就可以升级整个系统. 花费的时间取决于系统有多老. 这个命令会同步非本地(local)软件仓库并升级系统的软件包:

```bash
# pacman -Syu
```

### 查询包数据库

`pacman` 使用 `-Q` 参数查询本地软件包数据库,  `-S` 查询同步数据库, 以及 `-F` 查询文件数据库.
要了解每个参数的子选项, 分别参见 `Pacman -Q --help`, `Pacman -S --help`和`Pacman -F --help`.

`pacman` 可以在包数据库中查询软件包, 查询位置包含了软件包的名字和描述:

```bash
$ pacman -Ss string1 string2 ...
```

有时, `-s`的内置正则会匹配很多不需要的结果, 所以应当指定仅搜索包名, 而非描述或其他子段:

```bash
$ pacman -Ss '^vim-'
```

要查询已安装的软件包:

```bash
$ pacman -Qs string1 string2 ...
```

按文件名查找软件库:

```bash
$ pacman -F string1 string2 ...
```

显示软件包的详尽的信息:

```bash
$ pacman -Si package_name
```

查询本地安装包的详细信息:

```bash
$ pacman -Qi package_name
```

使用两个 `-i` 将同时显示备份文件和修改状态:

```bash
$ pacman -Qii package_name
```

要获取已安装软件包所包含文件的列表:

```bash
$ pacman -Ql package_name
```

查询远程库中软件包包含的文件:

```bash
$ pacman -Fl package_name
```

检查软件包安装的文件是否都存在:

```bash
$ pacman -Qk package_name
```

两个参数`k`将会执行一次更彻底的检查.  查询数据库获取某个文件属于哪个软件包:

```bash
$ pacman -Qo /path/to/file_name
```

查询文件属于远程数据库中的哪个软件包:

```bash
$ pacman -F /path/to/file_name
```

要罗列所有不再作为依赖的软件包(孤立orphans):

```bash
$ pacman -Qdt
```

提示:  将上述命令添加到 pacman 的一个处理后 hook用于在如果有处理产生孤立包后获得提示. 这在当有软件包被仓库放弃时是有用的,
因为任何被抛弃的包都会成为本地的一个孤立包(除非它是被显式安装的).
要避免在没有找到孤立包时`failed to execute command`的错误, 在你的 `hook` 中为`Exec`使用如下的指令:

```bash
/usr/bin/bash -c "/usr/bin/pacman -Qtd || /usr/bin/echo '=> None found.'"
```

要罗列所有明确安装而且不被其它包依赖的软件包:

```bash
$ pacman -Qet
```

更多例子查看`pacman tips`.

### Pactree

注意:  `pactree(8)`不再是 `pacman` 的一部分. 它现在在 `pacman-contrib` 中.

要显示软件包的依赖树:

```bash
$ pactree package_name
```

检查一个安装的软件包被哪些包依赖, 将递归标识`-r`传递给 pactree, 或者使用 `pkgtoolsAUR` 中的 `whoneeds`

### 数据库结构

`pacman`数据库通常位于 `/var/lib/pacman/sync`. 对于每一个在`/etc/pacman.conf`中指定的软件仓库,  这里都有一个一致的数据库.
数据库文件夹里每个`tar.gz`文件都包含着一个仓库的软件包信息. 例如 `which` 包:

```bash
$ tree which-2.21-5
which-2.21-5
|-- desc
```

这个 `depends` 项列出了该软件的依赖包,  而`desc`有该包的介绍, 例如文件大小和MD5值 .

### 清理软件包缓存

`pacman` 将下载的软件包保存在 `/var/cache/pacman/pkg/` 并且不会自动移除旧的和未安装版本的软件包. 这样做有一些好处:

+ 这样允许降级软件包而不需要通过其他方式提取旧版本, 例如 Arch Linux Archive.
+ 被卸载的软件包可以轻易地直接从缓存文件夹重新安装, 不需要重新从软件仓库下载.

然而, 需要定期手动清理缓存来避免该文件夹无限制增大.

`pacman-contrib` 提供的 `paccache(8)` 脚本默认会删除所有缓存的版本和已卸载的软件包, 除了最近的`3`个会被保留:

```bash
# paccache -r
```

启用 和 启动 `paccache.timer`来每周删除不使用的包.

也可以自己设置保留最近几个版本:

```bash
# paccache -rk1
```

添加`-u`/`--uninstalled`开关来限制`paccache`的行为只作用于卸载的包. 例如清理所有卸载的包的缓存版本, 可以用以下命令:

```bash
# paccache -ruk0
```

或者你可以将安装其结合, 同时作用于安装的和卸载的包, 例如想要保留最近两个安装的包但是移除所有卸载的包的缓存版本, 使用以下命令:

```bash
# paccache -rk2 -ruk0
```

更多参数参见`paccache -h`.

pacman也有一些内建参数用于清除缓存和那些不再在`/etc/pacman.conf`配置文件中列出的软件仓库残留数据库文件.
然而pacman并不提供保留一定数量的过去版本的功能, 因此它比`paccache`的默认选项更加激进.

要删除目前没有安装的所有缓存的包, 和没有被使用的同步数据库, 执行:

```bash
# pacman -Sc
```

要删除缓存中的全部文件, 使用两次-c开关. 这是最为激进的方式, 将会清空缓存文件夹:

```bash
# pacman -Scc
```

警告:  应当避免从缓存中删除所有过去版本和卸载的包, 除非需要更多磁盘空间. 这样会导致无法降级或重新安装包而不再次下载他们

`pkgcachecleanAUR`以及`pacleanerAUR`是两个进一步清理缓存的替代工具

***
其它命令

升级系统时安装其他软件包:

```bash
# pacman -Syu package_name1 package_name2 ...
```

安装原因

`pacman`数据库按照软件包被安装的原因, 将其分为两类:

+ 显式安装: 那些真正地被传递给通用`pacman -S`和`-U`命令的包;
+ 依赖: 那些虽然(一般)从未被传递给`pacman`安装命令, 但由于被其它显式安装的包需要从而被隐式安装的包

当安装软件包时, 可以把安装原因强制设为依赖:

```bash
# pacman -D --asdeps package_name
```

### 配置

`pacman` 的配置文件位于`/etc/pacman.conf`.  `man pacman.conf` 可以查看配置文件的进一步信息.

***
通用选项: 通用选项都在`[options]`段. 阅读 man 手册或者查看默认的 pacman.conf 可以获得有关信息和用法.

***
升级前对比版本:要查看旧版和新版的有效安装包, 请取消`/etc/pacman.conf`中`"VerbosePkgLists"`的注释. 修改后的`pacman -Syu`会列出详细表格.

## git

### gitk

首先需要安装依赖包`tk`, 然后运行的时候可能会报错`unknown color name "BACKGROUND"`或者类似下面的

```bash
application-specific initialization failed: unknown color name "S_base3"
Error in startup script: unknown color name "S_base3"
    (database entry for "-background" in widget ".")
    invoked from within
"load /usr/lib/x86_64-linux-gnu/libtk8.6.so Tk"
    ("package ifneeded Tk 8.6.1" script)
    invoked from within
"package require Tk"
    (file "/usr/bin/gitk" line 10)
```

`xrdb`--X resource datebase, [X resources](https://wiki.archlinux.org/index.php/X_resources).
`Xrdb`是一种为GUI应用程序的各种属性指定默认值的方法. 这些属性之一是背景色. 其中有一个规范, 即gitk主窗口的背景颜色为`S_base3`, 但程序不知道如何解析该颜色名称.

`Xrdb`实际上是在`X root window`(`RESOURCE_MANAGER`)的子项中维护的, 对于应用程序是全局指定的.
初始化内容通常来自于用户家目录, 通常是`~/.Xresources`, 但也可以由桌面环境软件设置. 找出导致问题的原因可能很棘手, 因为许多程序都可以写入该属性.

`xrdb`程序可用于编辑资源数据库. 可以使用: `xrdb -query> xprops.txt`将当前内容列出到`xprops.txt`. 编辑文件以获取一些合理的值, 然后使用: `xrdb -load <xprops.txt`载入新值.

例如把`Solarized`的颜色方案添加到`xprops.txt`的前面,[solarized-TrevorBramble](https://github.com/altercation/solarized/blob/master/xresources/solarized).
其中使用了`#define`, 需要安装`gcc`编译器.

```c
#define S_yellow        #b58900
#define S_orange        #cb4b16
#define S_red           #dc322f
#define S_magenta       #d33682
```
