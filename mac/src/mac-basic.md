# macos 基础操作

## 常用命令

[请问有哪些实用的Mac终端命令](https://www.zhihu.com/question/27864961)

***
`say` 让 mac 读出一段文字

`$ sleep 10 && say "hello"`

***
`pbcopy`, 与之对应的是`pbpaste`

把命令行的输出直接复制到粘贴板:

```bash
$ sort -u | pbcopy
```

把粘贴板内容直接存入一个文件:

```bash
$ pbpaste > out.txt
```

用 `Finder` 打开当前目录

```bash
$ open .
```

***
`cdf`
可以让 `shell` 跳转到`finder`打开的文件夹, 不过要提前在`.zshrc`中的`plugin`中加入`osx`

***
`ccat`
可以让你的cat文件时候高亮代码
`brew install ccat`

***
`archey`

这是一个查看当前`mac`概览的插件, 可以用`brew`安装. 好处就是, 查看自己`ip`的时候不用输入`ifconfig`然后一行一行去看, 看`mac`的`关于本机`不用再去点菜单栏了.

`brew install archey`

***
`mdfind` 命令查找文件, 类似 `Spotlight` 查询效果的命令行

## 中文字体

查找 `字体册`, `font Book.app`应用.

`macos` 的字体存放在`/System/Library/Fonts`目录下,`macos` 预装的中文字体为,

```bash
Apple LiGothic Medium
Apple LiSung Light
Apple SD Gothic Neo Regular
AppleGothic Regular
AppleMyungjo Regula
Baoli SC Regular
BiauKai Regular
GB18030 Bitmap Regular
Hannotate SC Regular
HanziPen SC Regular
Hei Regular
Heiti TC Light
Hiragino Sans CNS W3
Hiragino Sans GB W3
Kai Regular
Kaiti SC Regular
Lantinghei SC Extralight
Libian SC Regular
LiHei Pro Medium
LingWai SC Medium
LiSong Pro Light
Nanum Gothic Regular
PCMyungjo Regular
PingFang SC Regular
SimSong Regular
Songti SC Regular
STFangsong Regular
STHeiti Light
STKaiti Regular
STSong Regular
Wawati SC Regular
Weibei SC Bold
Xingkai SC Light
Yuanti SC Regular
Yuppy SC Regular
```

## 命令行安装字体

[Sarasa Mono SC](https://github.com/laishulu/Sarasa-Mono-SC-Nerd)

`MacOS` 用户可以直接通过 `cask` 安装:

```bash
brew tap laishulu/cask-fonts
brew install font-sarasa-nerd
```

如果是安装本地字体文件, 参考[nstalling fonts from terminal](https://apple.stackexchange.com/questions/240381/installing-fonts-from-terminal-instead-of-font-book),
直接复制到对应的文件夹, `/Library/Fonts` (系统公用) or `~/Library/Fonts` (当前用户使用).

## XeLaTeX 使用系统字体

[修复 MacTeX 2015 无法按字体文件名调](https://liam.page/2015/07/11/mactex-2015-system-font/)

`XeLaTeX` 通过字体文件名调用字体需要 `kpathsea` 库的协助. 默认情况下, `kpathsea` 会搜索 `MacTeX` 自己的目录树(TEXMF).
如果希望 `kpathsea` 搜索系统字体目录的话, 还需要配置 `OSFONTDIR` 这个环境变量.
在 `MacTeX 2014` 中, 这个变量是默认配置好了的. 但是在 `MacTeX 2015` 中, 不知为何, 这个变量没有预先配置. 因此, 需要用户自行配置.

具体的配置方法如下:
编辑文件`/usr/local/texlive/2015/texmf.cnf`, 在文件末尾加上一行: `OSFONTDIR = /Library/Fonts//:~/Library/Fonts//`
保存文件

`macTex`的字体文件在`/usr/local/texlive/2020/texmf-dist/fonts`, 使用`fontspec`和`xetex`时, 可能不会自动搜索`TeXLive` tree, 参考
[Missing Dejavu Font](https://tex.stackexchange.com/questions/314298/missing-dejavu-font)
最简单的方法是直接把需要的字体文件复制到`/Users/tom/Library/fonts`, 或者`/Library/Fonts`目录下, 前者是用户字体文件夹. 使用`字体册.app`安装也可以, 但是速度感人.

## mac 命令行管理

[简单几条命令,轻松开启 macOS 系统隐藏功能 | 新手问号](https://sspai.com/post/41695)

***
配置 `Launchpad`

可以通过终端对 `Launchpad` 排列方式进行修改,复制以下代码至终端即可:

```bash
defaults write com.apple.dock springboard-columns -int 8;
defaults write com.apple.dock springboard-rows -int 7;
defaults write com.apple.dock ResetLaunchPad -bool TRUE;
killall Dock
```

命令中有两个数字 `8` 和 `7`,它们分别代表的是布局中的列数和行数,如果想更清除的了解该段命令,
可以参考[<通过终端命令改变 Launchpad 中应用图标的大小>](https://sspai.com/post/33299).

除了可以对 `Launchpad` 的布局进行更改,还可以根据自己的喜好对背景的模糊程度进行更改,复制以下代码至终端即可:

```bash
defaults write com.apple.dock springboard-blur-radius -int 100;
killall Dock
```

命令中有一个数字 `100` ,它代表的背景模糊的程度,你可以在 `0~255`的范围内选择.

***
修改截图属性

`Mac` 上自带的截图非常的还用,可以区域, 窗口, 延时截图.
但是截图会默认保存在你的桌面上,时间一长,你的桌面就会被五花八门的截图堆满.
对此,我们可以新建一个文件夹专门来存放截图,新建一个 `screenshot` 的文件夹在桌面或者任意一个你希望它待在的地方,将下述代码复制进终端即可:

```bash
mkdir -p ~/Pictures/screenshot
defaults write com.apple.screencapture location ~/Pictures/screenshot;
killall SystemUIServer
```

`~`之后填写你相应的文件夹路径即可.

除此之外,你也可以使用以下命令修改截图保存的类型,例如你想保存 `JPG` 格式的截图:

```bash
efaults write com.apple.screencapture type jpg && killall SystemUIServe
```

***
显示隐藏文件夹

在 `Windows` 上隐藏文件夹大家应该都是老手了, 转到 `Mac` 后,却发现隐藏文件夹和自己想象有那么一些不一样.
为了更好的把大家的`小秘密`藏到内心最深处的地方,也可以使用两段命令来完成操作.
跟前文一样,我们需要获取文件夹的路径,然后在终端中输入以下代码:

```bash
chflags hidden ~/Desktop/Hidden
```

你也可以使用`nohidden`重新让该文件夹显示.如果你要显示全部文件,推荐大家直接使用快捷键`Shift + Command + .`即可显示全部隐藏文件.

除此之外,如果你觉得自己桌面太乱了,但是这会又有人来看你的电脑,你可以使用一段命令行将桌面的文件全部隐藏起来,让桌面回归清爽,文件也依旧可以通过 Finder 中的桌面中找到:

```bash
defaults write com.apple.finder CreateDesktop -bool false; killall Finder
```

如果想重新看到桌面的图标,将 `false` 替换为 `true` 输入终端即可.

***
Dock 栏属性修改

Mac 中为了获得更大的可视空间,在不使用 `Dock` 时我们可以隐藏它.若要查看隐藏的 `Dock`,可以将指针移到 `Dock` 所在屏幕的边缘.
但是这个显示速度存在了一定的延迟,为了加速这个过程,我们可以使用一段命令行,让你的隐藏 `Dock` 弹出的时候更加的顺滑流畅:

```bash
defaults write com.apple.Dock autohide-delay -float 0 && killall Dock
```

使用后的效果,可以说是非常明显了,再也不会有在`挤牙膏`的感觉.

如果在你的使用下,`Dock` 栏上摆满了各类 App,却发现这不是自己想要的结果.你可以通过终端来重置你的 `Dock` 栏,让它回到最开始的状态:

```bash
defaults delete com.apple.dock; killall Dock
```

让屏幕亮的更久

Mac 在运行一段时间后,会自动进入睡眠.如果大家不想 Mac 那么快的进入书面,可以采用一些第三方软件来达到此目的.
其实与其下载一个软件占用 Mac 上精贵的储存,不如使用一段命令行就可以解决这些问题了.
下方命令行中的 `3600` 单位是秒,即你希望多长时间内你的 Mac 不会进入睡眠:

```bash
caffeinate -t 3600
```

***
应用安装与更新

`Mac Apple Store` 的连接情况大家也很清楚,况且还有很多应用并不在商店上架,或是非商店版本有更多的功能.
原来的时候,我们需要查找一个又一个的官网,然后下载安装,其实这么多繁琐操作,在终端里可以更快的完成.你只需要输入:

```bash
brew cask install App
```

将 `App` 替换为你需要安装的软件的名字即可.
但是使用前,需要你在电脑中安装 `Homebrew Cask` ,具体可以参考`再谈 Homebrew Cask 在 macOS 上安装应用的轻松感`.
大多数通过 `Cask` 安装的软件都自带更新选项,如果没有该选项,用户依旧可以通过终端进行更新,
在终端中输入`brew tap buo/cask-upgrade`,然后再输入下段命令即可更新全部应用:

```bash
brew cu
```

## diskutil 磁盘管理

[MacOS 磁盘管理工具 diskutil 介绍](https://www.jianshu.com/p/6a1f365617ad)

电脑上的操作系统, 应用程序和应用数据一般都需要保存在永久存储器中(通常就是硬盘),这样电脑断电后应用数据等就不会丢失.
为了更有效地组织磁盘上的数据信息,通常将磁盘预先划分成一个或多个磁盘分区,创建对应的文件系统,以方便计算机对各分区分别进行管理.
`MacOS` 系统自带一个图形化的磁盘管理工具(`Disk Utility`),同时还有一个命令行版本的 `diskutil`.
通过该命令的使用,可以很快捷地对本地磁盘进行擦除数据, 调整分区大小, 格式化等操作.

`diskutil` 命令的格式为:`diskutil <verb> <options>`

### verb 动作

不带任何选项的 `diskutil` 命令会列出该命令支持的 `verb` 及其对应的介绍:

```bash
 diskutil
Disk Utility Tool
Utility to manage local disks and volumes
```

列出的 `verb` 主要分为以下几类:

+ 获取磁盘和分区信息:如 `list`, `info`, `activity` 等
+ 挂(卸)载磁盘或卷:如 `mount`, `eject`, `mountDisk` 等
+ 验证, 修复磁盘分区或文件系统:如 `verifyVolume`, `repairDisk` 等
+ 分区操作:如 `splitPartitions`, `mergePartitions` 等
+ 其他:如 `appleRAID`, `apfs` 等
+ `diskutil listFilesystems`: 列出支持的文件系统格式

如不清楚某个 `verb` 的具体命令格式,可以直接使用 `diskutil` 命令加上该 `verb` 并且不带任何其他选项,命令行即输出该 `verb` 的使用介绍.
如 `eraseDisk` 的使用介绍:

```bash
$   diskutil eraseDisk
用法:  diskutil eraseDisk format name [APM[Format]|MBR[Format]|GPT[Format]]
        MountPoint|DiskIdentifier|DeviceNode
```

完全擦除现有的整个磁盘. 清除磁盘上的所有内容, 需要磁盘的所有权.
`format`是你擦除之后,重新格式化要得到的格式(`HFS+`等).例如:

擦除整个设备或者磁盘
`diskutil eraseDisk JHFS+ Untitled disk3`

### 获取磁盘分区信息

***
list

可以使用 `list` 选项简要列出 MacOS 系统的磁盘及分区信息,包括分区类型(TYPE), 分区名(NAME), 容量大小(SIZE)和标志符(IDENTIFIER)等.
如此时系统挂载了 `dmg` 映像文件,其信息也会显示在列表中(下表中的 disk3 ).

```bash
$   diskutil list
/dev/disk0 (internal, physical):
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:      GUID_partition_scheme                        *121.3 GB   disk0
   1:                        EFI EFI                     209.7 MB   disk0s1
   2:                 Apple_APFS Container disk1         121.1 GB   disk0s2
   ...
```

其中的 `/dev/disk0` 为内置磁盘,`/dev/disk2` 为外置磁盘(U 盘,已在 Windows 系统下格式化为 `FAT32` 格式),`/dev/disk3` 为 `DMG` 映像文件.
而 `/dev/disk1` 其实就是 `disk0s2` 作为 `APFS` 文件系统容器的具体信息.

***
info

`info` 选项可以列出指定磁盘或分区的详细信息.如查看 `disk2` (即 8 G 优盘)的信息:

```bash
 diskutil info disk2
   Device Identifier:        disk2
   Device Node:              /dev/disk2
   Whole:                    Yes
   Part of Whole:            disk2
   Device / Media Name:      DataTraveler 2.0
   ...
```

输出的信息包括设备标志符(Device Identifier), 设备节点(Device Node), 设备名(Device / Media Name), 容量大小(Disk Size), 块大小(Block Size)等.

也可以查看某个分区的详细信息:

```bash
 diskutil info disk1s1
   Device Identifier:        disk1s1
   Device Node:              /dev/disk1s1
   Whole:                    No
   Part of Whole:            disk1
   ...
```

### 擦除磁盘或分区

`eraseDisk` 选项用于擦除整个磁盘并重新格式化.该命令的格式为:

```bash
diskutil eraseDisk <format> <name> [APM|MBR|GPT] MountPoint|DiskIdentifier|DeviceNode
```

`format` 用于指定擦除数据后需要重新建立的文件系统类型.可以为 `%noformat%` 来跳过初始化文件系统的操作.其他支持的类型可以通过 `listFilesystems` 选项查看.

```bash
$   diskutil listFilesystems
Formattable file systems
...
```

可以使用 `diskutil eraseDisk ExFAT StarkyDisk GPT disk2` 命令将优盘数据擦除并格式化为 `ExFAT` 格式.

```bash
$   diskutil eraseDisk ExFAT StarkyDisk GPT disk2
Started erase on disk2
Unmounting disk
...
```

此时的优盘分区表变为 `GPT` 类型,且多了一个 `EFI` 分区.

也可以在擦除磁盘时指定分区表类型:

```bash
$   sudo diskutil eraseDisk ExFAT StarkyDisk MBR disk2
Password:
Started erase on disk2
```

此时的优盘分区表变为 `MBR` 类型:

其他擦除命令如 `eraseVolume` (完全擦除整个磁盘或某个磁盘分区,创建新的文件系统), `zeroDisk` (向整个磁盘或某个分区全部写入`'0'`)
使用 `zeroDisk` 命令擦除磁盘(该过程会花费很长的时间,我试了)后,该磁盘上的全部信息被抹除,同时也不再包含分区和文件系统信息.
则再次插入此优盘会提示你`初始化`或`格式化`该磁盘.

***
制作启动U盘

```bash
# 首先取消usb硬盘的挂载
diskutil unmountDisk /dev/disk6
# bs是读写快的大小, 太小会增大io, 降低效率, 一般1M~2M即可.
sudo dd if=/Users/tom/software/manjaro-kde-20.2.1-210103-linux59.iso of=/dev/disk6 bs=2M
```

### 创建磁盘分区

可以通过 `partionDisk` 选项完成对磁盘的分区操作.该命令的格式为:

```bash
diskutil partitionDisk MountPoint|DiskIdentifier|DeviceNode
        [numberOfPartitions] [APM|MBR|GPT]
        [part1Format part1Name part1Size part2Format part2Name part2Size
         part3Format part3Name part3Size ...]
```

命令选项中的 `Size` 用来指定分区的大小(以扇区数计量),合法的值包括带有指定后缀的浮点数.
其中的后缀有 `B(ytes)`, `S(512-byte-blocks)`, `K(ilobytes)`, `M(egabytes)`, `G(igabytes)`, `T(erabytes),` `P(etabytes)`,
也可以是 `%` 来表示对整个磁盘的占比.
最后一个分区会自动扩展到占用整个磁盘的剩余空间,如果想为最后一个分区指定固定的大小,可在其后再创建一个类型为`free space`的分区.

```bash
$   sudo diskutil partitionDisk disk2 3 MBR MS-DOS F01 3G JHFS+ F02 3G "Free Space" F03 0
Started partitioning on disk2
Unmounting disk
...
```

上面的命令在优盘(`disk2`)上创建了 3 个分区,第一个(F01)格式为 `FAT32`,大小是 `3 Gb`.第二个(F02)格式为 `JHFS+`,大小为 `3 Gb`.
最后一个是`自由空间`,大小为剩余的容量.所以实际上只是分了两个区,整体的分区表类型为 `MBR` .

### 分割/合并磁盘分区

`splitPartition` 选项可以用来将已存在的某个分区再分割成数个更小的分区,注意原分区上的所有数据都会丢失.
该选项的第一个参数为需要分割的分区的`挂载点`/`标志符`/`设备节点`,其余参数和使用 `partitionDisk` 时相同.

```bash
$   sudo diskutil list | grep disk2
/dev/disk2 (external, physical):
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS starky                  7.5 GB     disk2s2
$   sudo diskutil splitPartition disk2s2 2 MS-DOS F01 3g JHFS+ F02 3g
Started partitioning on disk2s2 starky
Splitting
```

上面的命令将优盘的第二个分区(`disk2s2`)又分割成了两个更小的分区,分别是 `FAT32` 格式的 `F01`(`disk2s2`),和 `JHFS+` 格式的 F02(`disk2s3`).
虽然命令中指定了 F02 的大小是 `3G`,因为是最后一个分区,所以自动扩展到占用剩余的磁盘空间.最后它的实际大小是 `4.5G`.

`mergePartitions` 选项用来将多个已存在的分区合并为一个大的分区.该选项的格式为:

```bash
diskutil mergePartitions [force] format name DiskIdentifier|DeviceNode DiskIdentifier|DeviceNode
```

第一个分区参数为起始分区,第二个分区参数为结束分区.这两个分区之间的所有分区都将被合并.
如果 `force` 选项没有被指定,且合并前的第一个分区是可调整大小的文件系统(如 `JHFS+`),则第一个分区上的数据会保留到合并后的分区.

```bash
$   sudo diskutil list | grep disk2
/dev/disk2 (external, physical):
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS F01                     2.9 GB     disk2s2
   3:       Microsoft Basic Data F02                     4.5 GB     disk2s4
$   sudo diskutil mergePartitions JHFS+ Starky disk2s2 disk2s4
Merging partitions into a new partition
...
```

### 调整分区大小(无损)

`resizeVolume` 选项可以无损调整(增加或缩减)分区大小.

将 `disk2s2` 分区缩减为 `4g` 大小,腾出的空间作为`free space`:

```bash
$   diskutil list | grep disk2
/dev/disk2 (external, physical):
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS F01                     7.5 GB     disk2s2
$   sudo diskutil resizeVolume disk2s2 4g
Resizing to 4000000000 bytes
```

将 `disk2s2` 分区扩展,并尽可能占用所有可用的自由空间.

```bash
$   sudo diskutil resizeVolume disk2s2 R
Resizing to full size (fit to fill)
...
/dev/disk2 (external, physical):
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS F01                     7.5 GB     disk2s2
```

## 挂载文件

[Mounty for NTFS](https://mounty.app/)

一个微型工具,用于以读写模式在macOS下重新 mount write-protected `NTFS` .

 相当于指令

```bash
$ sudo umount /Volumes/UNTITLED
$ sudo mount -t ntfs -o rw,auto,nobrowse /dev/disk3s1 ~/ntfs-volume
```

## 更新命令行工具

Error: Your Command Line Tools are too outdated.
Update them from Software Update in System Preferences or run:

```bash
softwareupdate --all --install --force
```

If that doesn't show you any updates, run:

```bash
sudo rm -rf /Library/Developer/CommandLineTools
sudo xcode-select --install
```

Alternatively, manually download them from:
[https://developer.apple.com/download/all/](https://developer.apple.com/download/all/).

You should download the Command Line Tools for Xcode 13.4.
