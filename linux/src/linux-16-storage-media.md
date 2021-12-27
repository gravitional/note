# 第十六章:存储媒介

在前面章节中,我们已经从文件级别看了操作数据.在这章里,我们将从设备级别来考虑数据. `Linux` 有着令人惊奇的能力来处理存储设备,
不管是物理设备,比如说硬盘,还是网络设备,或者是虚拟存储设备,像 `RAID`(独立磁盘冗余阵列)和 `LVM`(逻辑卷管理器).

然而,这不是一本关于系统管理的书籍,我们不会试图深入地覆盖整个主题.我们将努力做的就是 介绍一些概念和用来管理存储设备的重要命令.
我们将会使用 `USB` 闪存,`CD-RW` 光盘(因为系统配备了 `CD-ROM` 烧写器)和一张软盘(若系统这样配备),来做这章的练习题.

我们将看看以下命令:

+ `mount` – 挂载一个文件系统
+ `umount` – 卸载一个文件系统
+ `fsck` – 检查和修复一个文件系统
+ `fdisk` – 分区表控制器
+ `mkfs` – 创建文件系统
+ `fdformat` – 格式化一张软盘
+ `dd` — 把面向块的数据直接写入设备
+ `genisoimage` (`mkisofs`) – 创建一个 `ISO 9660` 的映像文件
+ `wodim` (`cdrecord`) – 把数据写入光存储媒介
+ `md5sum` – 计算 `MD5` 检验码

## 挂载和卸载存储设备

`Linux` 桌面系统的最新进展已经使存储设备管理对于桌面用户来说极其容易.大多数情况下,我们 只要把设备连接到系统中,它就能工作.在过去(比如说,2004年),这个工作必须手动完成.
在非桌面系统中(例如,服务器中),这仍然是一个主要地手动过程,因为服务器经常有极端的存储需求 和复杂的配置要求.
管理存储设备的第一步是把设备连接到文件系统树中.这个过程叫做挂载,允许设备参与到操作系统中.

回想一下第三章,类 Unix 的操作系统,像 `Linux`,维护单一文件系统树,设备连接到各个结点上.
这与其它操作系统形成对照,比如说 `MS-DOS` 和 `Windows` 系统中,每个设备(例如 `C:\`,`D:\`,等) 保持着单独的文件系统树.

有一个叫做`/etc/fstab` 的文件可以列出系统启动时要挂载的设备(典型地,硬盘分区). 下面是 来自于 `Fedora7` 系统的`/etc/fstab` 文件实例:

```bash
LABEL=/12   /   ext3    defaults    1 1
LABEL=/home  /home   ext3    defaults    1 2
LABEL=/boot  /boot   ext3    defaults    1 2
...
```

在这个实例中所列出的大多数文件系统是虚拟的,并不适用于我们的讨论.就我们的目的而言, 前三个是我们感兴趣的:

```bash
LABEL=/12   /   ext3    defaults    1 1
LABEL=/home  /home   ext3    defaults    1 2
LABEL=/boot  /boot   ext3    defaults    1 2
```

这些是硬盘分区.每行由六个字段组成,如下所示:
***
表16-1: `/etc/fstab` 字段
字段 内容 说明

1. `设备名`:
传统上,这个字段包含与物理设备相关联的设备文件的实际名字,比如说`/dev/hda1`(第一个IDE 通道上第一个主设备分区).
然而今天的计算机,有很多热插拔设备(像 USB 驱动设备),许多 现代的 Linux 发行版用一个文本标签和设备相关联.
当这个设备连接到系统中时, 这个标签(当储存媒介格式化时,这个标签会被添加到存储媒介中)会被操作系统读取.
那样的话,不管赋给实际物理设备哪个设备文件,这个设备仍然能被系统正确地识别.
2. `挂载点`: 设备所连接到的文件系统树的目录.
3. `文件系统类型` : Linux 允许挂载许多文件系统类型.大多数本地的Linux 文件系统是 `ext3`,但是也支持很多其它的,比方说 `FAT16` (msdos),`FAT32` (vfat),`NTFS`(ntfs),`CD-ROM`(iso9660),等等.
4. `选项`:  文件系统可以通过各种各样的选项来挂载.例如,挂载只读的文件系统, 或者挂载阻止执行任何程序的文件系统(一个有用的安全特性,避免删除媒介.)
5. `频率`: 一位数字,指定是否和在什么时间用 `dump` 命令来备份一个文件系统.
6. `次序`: 一位数字,指定 `fsck` 命令按照什么次序来检查文件系统.

## 查看挂载的文件系统列表

这个 `mount` 命令被用来挂载文件系统.执行这个不带参数的命令,将会显示 一系列当前挂载的文件系统:

```bash
$ mount
/dev/sda2 on / type ext3 (rw)
...
```

这个列表的格式是:`设备 on 挂载点 type 文件系统类型(可选的)`.

例如,第一行所示设备`/dev/sda2` 作为根文件系统被挂载,文件系统类型是 `ext3`,并且可读可写(这个`rw`选项).

在这个列表的底部有两个有趣的条目.
倒数第二行显示了在读卡器中的一张`2G` 的 SD 内存卡,挂载到了`/media/disk` 上.
最后一行 是一个网络设备,挂载到了`/misc/musicbox` 上.

第一次实验,我们将使用一张 `CD-ROM`.首先,在插入 `CD-ROM` 之前,我们将看一下系统:

```bash
$ mount

/dev/mapper/VolGroup00-LogVol00 on / type ext3 (rw)
proc on /proc type proc (rw)
```

这个列表来自于 `CentOS 5`系统,使用 `LVM`(逻辑卷管理器)来创建它的根文件系统.
正如许多现在的 Linux 发行版一样,这个 系统试图自动挂载插入的 `CD-ROM`.当我们插入光盘后,我们看看下面的输出:

```bash
$ mount
/dev/mapper/VolGroup00-LogVol00 on / type ext3 (rw)
proc on /proc type proc (rw)
...
/dev/hdc on /media/live-1.0.10-8 type iso9660 (ro,noexec,nosuid,
nodev,uid=500)
```

当我们插入光盘后,除了额外的一行之外,我们看到和原来一样的列表.

在列表的末尾,我们 看到 CD-ROM 已经挂载到了`/media/live-1.0.10-8`上,它的文件类型是 `iso9660(CD-ROM)`.
就我们的实验目的而言,我们对这个设备的名字感兴趣.当你自己进行这个实验时,这个 设备名字是最有可能不同的.

警告:在随后的实例中,至关重要的是你要密切注意用在你系统中的实际设备名,并且 不要使用此文本中使用的名字!
还要注意音频 CD 和 CD-ROM 不一样.音频 CD 不包含文件系统,这样在通常意义上,它就不能被挂载了.

现在我们拥有 CD-ROM 光盘的设备名字,让我们卸载这张光盘,并把它重新挂载到文件系统树 的另一个位置.
我们需要超级用户身份(使用系统相应的命令)来进行操作,并且用 `umount`(注意这个命令的拼写)来卸载光盘:

```bash
$ su -
Password:
[root@linuxbox ~]# umount /dev/hdc
```

下一步是创建一个新的光盘挂载点.简单地说,一个挂载点就是文件系统树中的一个目录.它没有什么特殊的.
它甚至不必是一个空目录,即使你把设备挂载到了一个非空目录上,你也不能看到 这个目录中原来的内容,直到你卸载这个设备.
就我们的目的而言,我们将创建一个新目录:

```bash
[root@linuxbox ~]# mkdir /mnt/cdrom
```

最后,我们把这个 CD-ROM 挂载到一个新的挂载点上.这个`-t` 选项用来指定文件系统类型:

```bash
[root@linuxbox ~]# mount -t iso9660 /dev/hdc /mnt/cdrom
```

之后,我们可以通过这个新挂载点来查看 CD-ROM 的内容:

```bash
[root@linuxbox ~]# cd /mnt/cdrom
[root@linuxbox cdrom]# ls
```

注意当我们试图卸载这个 CD-ROM 时,发生了什么事情.

```bash
[root@linuxbox cdrom]# umount /dev/hdc
umount: /mnt/cdrom: device is busy
```

这是怎么回事呢?原因是我们不能卸载一个设备,如果某个用户或进程正在使用这个设备的话.
在这种 情况下,我们把工作目录更改到了 CD-ROM 的挂载点,这个挂载点导致设备忙碌.

我们可以很容易地修复这个问题 通过把工作目录改到其它目录而不是这个挂载点.

```bash
[root@linuxbox cdrom]# cd
[root@linuxbox ~]# umount /dev/hdc
```

现在这个设备成功卸载了.

### 为什么卸载重要

如果你看一下 `free` 命令的输出结果,这个命令用来显示关于内存使用情况的统计信息,你会看到一个统计值叫做`buffers`.
计算机系统旨在尽可能快地运行.系统运行速度的 一个阻碍是缓慢的设备.
打印机是一个很好的例子.即使最快速的打印机相比于计算机标准也 极其地缓慢.
一台计算机确实会运行地非常慢,如果它要停下来等待一台打印机打印完一页.
在早期的个人电脑时代(多任务之前),这真是个问题.

如果你正在编辑电子表格 或者是文本文档,每次你要打印文件时,计算机都会停下来而且变得不能使用.
计算机能以打印机可接受的最快速度把数据发送给打印机,但由于打印机不能快速地打印, 这个发送速度会非常慢.
这个问题被解决了,由于打印机缓存的出现,一个包含一些 RAM 内存 的设备,位于计算机和打印机之间.

通过打印机缓存,计算机把要打印的结果发送到这个缓存区, 数据会迅速地存储到这个 RAM 中,这样计算机就能回去工作,而不用等待.
与此同时,打印机缓存将会 以打印机可接受的速度把缓存中的数据缓慢地输出给打印机.
缓存被广泛地应用于计算机中,使其运行地更快.别让偶尔地需要读取或写入慢设备阻碍了系统的运行速度.

在实际与慢设备交互之前,操作系统会尽可能多的读取或写入数据到内存中的 存储设备里.
以 Linux 操作系统为例,你会注意到系统看似填充了多于它所需要的内存.
 这不意味着 Linux 正在使用所有的内存,它意味着 Linux 正在利用所有可用的内存,来作为缓存区.

这个缓存区允许非常快速地写入存储设备,因为写入物理设备的操作被延迟到后面进行.
同时, 这些注定要传送到设备中的数据正在内存中堆积起来.时不时地,操作系统会把这些数据 写入物理设备.

卸载一个设备需要把所有剩余的数据写入这个设备,所以设备可以被安全地移除.
如果 没有卸载设备,就移除了它,就有可能没有把注定要发送到设备中的数据输送完毕.
在某些情况下, 这些数据可能包含重要的目录更新信息,这将导致文件系统损坏,这是发生在计算机中的最坏的事情之一.

## 确定设备名称

有时很难来确定设备名称.在以前,这并不是很难.一台设备总是在某个固定的位置,也不会 挪动它.类 Unix的系统喜欢设备那样安排.
之前在开发 Unix 系统的时候,`更改一个磁盘驱动器`要用一辆 叉车从机房中移除一台如洗衣机大小的设备.

最近几年,典型的桌面硬件配置已经变得相当动态,并且 Linux 已经发展地比其祖先更加灵活.
在以上事例中,我们利用现代 Linux 桌面系统的功能来`自动地`挂载 设备,然后再确定设备名称
.
但是如果我们正在管理一台服务器或者是其它一些(这种自动挂载功能)不会 发生的环境,我们又如何能查清设备名呢?
首先,让我们看一下系统怎样来命名设备.如果我们列出目录`/dev`(所有设备的住所)的内容,我们 会看到许许多多的设备:

```bash
$ ls /dev
```

这个列表的内容揭示了一些设备命名的模式.这里有几个:
***
表16-2: Linux 存储设备名称
模式 设备

+ `/dev/fd*` 软盘驱动器
+ `/dev/hd*`老系统中的`IDE(PATA)`磁盘.
典型的主板包含两个 IDE 连接器或者是通道,每个连接器 带有一根缆线,每根缆线上有两个硬盘驱动器连接点.
缆线上的第一个驱动器叫做主设备, 第二个叫做从设备.设备名称这样安排,`/dev/hdb` 是指第一通道上的主设备名;
`/dev/hdb`是第一通道上的从设备名;`/dev/hdc`是第二通道上的主设备名,等等.末尾的数字表示 硬盘驱动器上的分区.
例如,`/dev/hda1`是指系统中第一硬盘驱动器上的第一个分区,而 `/dev/hda` 则是指整个硬盘驱动器.
+ `/dev/lp*` 打印机
+ `/dev/sd*` `SCSI` 磁盘.在最近的 Linux 系统中,内核把所有类似于磁盘的设备
(包括`PATA/SATA` 硬盘, 闪存,和 `USB` 存储设备,比如说可移动的音乐播放器和数码相机)看作 `SCSI` 磁盘.
剩下的命名系统类似于上述所描述的旧的`/dev/hd*`命名方案.
+ `/dev/sr*` 光盘(CD/DVD 读取器和烧写器)

另外,我们经常看到符号链接比如说`/dev/cdrom`,`/dev/dvd` 和`/dev/floppy`,它们指向实际的设备文件,提供这些链接是为了方便使用.

如果你工作的系统不能自动挂载可移动的设备,你可以使用下面的技巧来决定当可移动设备连接后,它是怎样被命名的.

首先,启动一个实时查看文件`/var/log/messages` (你可能需要超级用户权限):

```bash
sudo tail -f /var/log/messages
# or in ubuantu
sudo tail -f /var/log/kern.log
```

这个文件的最后几行会被显示,然后停止.下一步,插入这个可移动的设备.

在 这个例子里,我们将使用一个16MB 闪存.瞬间,内核就会发现这个设备, 并且探测它:

```bash
Jul 23 10:07:59 linuxbox kernel:
...
sd 3:0:0:0: [sdb] 31263 512-byte
sd 3:0:0:0: [sdb] Write Protect is
...
Jul 23 10:07:59 linuxbox kernel: sdb: sdb1
Jul 23 10:07:59 linuxbox kernel: sd 3:0:0:0: [sdb] Attached SCSI
removable disk
```

显示再次停止之后,输入 `Ctrl-c`,重新得到提示符.

输出结果的有趣部分是一再提及`[sdb]`, 这正好符和我们期望的 `SCSI` 磁盘设备名称.
知道这一点后,有两行输出变得颇具启发性:

```bash
Jul 23 10:07:59 linuxbox kernel: sdb: sdb1
Jul 23 10:07:59 linuxbox kernel: sd 3:0:0:0: [sdb] Attached SCSI
removable disk
```

这告诉我们这个设备名称是`/dev/sdb` 指整个设备,`/dev/sdb1`是这个设备的第一分区.
正如我们所看到的,使用 Linux 系统充满了有趣的监测工作.

小贴士:使用这个 `tail -f /var/log/messages` 技巧是一个很不错的方法,可以实时观察系统的一举一动.

既然知道了设备名称,我们就可以挂载这个闪存驱动器了:

```bash
$ sudo mkdir
$ sudo mount
$ df
```

这个设备名称会保持不变只要设备与计算机保持连接并且计算机不会重新启动.

## 创建新的文件系统

假若我们想要用 Linux 本地文件系统来重新格式化这个闪存驱动器,而不是它现用的 `FAT32`系统.这涉及到两个步骤:

1. (可选的)创建一个新的分区布局,若已存在的分区不是我们喜欢的.
2. 在这个闪存上创建一个新的空的文件系统.

注意!在下面的练习中,我们将要格式化一个闪存驱动器.拿一个不包含有用数据的驱动器 作为实验品,因为它将会被擦除!
再次,请确定你指定了正确的系统设备名称.未能注意此警告可能导致你格式化(即擦除)错误的驱动器!

## 用 fdisk 命令操作分区

[util-linux fdisk](https://wiki.archlinux.org/title/Fdisk_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

`fdisk` 程序允许我们直接在底层与类似磁盘的设备(比如说硬盘驱动器和闪存驱动器)进行交互.
使用这个工具可以在设备上编辑,删除,和创建分区. 以我们的闪存驱动器为例, 首先我们必须卸载它(如果需要的话),然后调用 `fdisk` 程序,如下所示:

```bash
$ sudo umount /dev/sdb1
$ sudo fdisk /dev/sdb
```

注意我们必须指定设备名称,必须是整个设备,而不是分区号.这个程序启动后,我们 将看到以下提示:

```bash
Command (m for help):
输入`m`会显示程序菜单:
Command action
a toggle a bootable flag
....
```

我们想要做的第一件事情是检查已存在的分区布局.输入`p`会打印出这个设备的分区表:

```bash
Disk /dev/sdb: 16 MB, 16006656 bytes
1 heads, 31 sectors/track, 1008 cylinders
Units = cylinders of 31 * 512 = 15872 bytes

Device Boot   Start         End           Blocks            Id     System
/dev/sdb1          2             1008          15608+        b        w95 FAT32
```

在此例中,我们看到一个 `16MB` 的设备只有一个分区`(1)`,此分区占用了可用的`1008`个柱面中的1006个, 并被标识为 `Windows 95 FAT32`分区.

有些程序会使用这个标志符来限制一些可以对磁盘所做的操作, 但大多数情况下更改这个标志符没有危害.
然而,为了叙述方便,我们将会更改它, 以此来表明是个 `Linux` 分区.在更改之前,首先我们必须找到被用来识别一个 `Linux` 分区的 `ID` 号码.

在上面列表中,我们看到 `ID` 号码`b`被用来指定这个已存在的分区.
要查看可用的分区类型列表, 参考之前的程序菜单.我们会看到以下选项:

`l list known partition types`

如果我们在提示符下输入`l`,就会显示一个很长的可能类型列表.
在它们之中会看到`b`为已存在分区 类型的 `ID` 号,而`83`是针对 Linux 系统的 `ID` 号.
回到之前的菜单,看到这个选项来更改分区 `ID` 号:

`t change a partition's system id`

我们先输入`t`,再输入新的 `ID` 号:

```bash
Command (m for help): t
Selected partition 1
Hex code (type L to list codes): 83
Changed system type of partition 1 to 83 (Linux)
```

这就完成了我们需要做得所有修改.
到目前为止,还没有实际接触这个设备(所有修改都存储在内存中, 而不是在此物理设备中),所以我们将会把修改过的分区表写入此设备,再退出.
为此,我们输入 在提示符下输入`w`:

```bash
Command (m for help): w
The partition table has been altered!
Calling ioctl() to re-read partition table.
WARNING: If you have created or modified any DOS 6.x
partitions, please see the fdisk manual page for additional
information.
Syncing disks.
$
```

如果我们已经决定保持设备不变,可在提示符下输入`q`,这将退出程序而没有写更改.
我们 可以安全地忽略这些不祥的警告信息.

## GNU Parted

[Parted (简体中文)](https://wiki.archlinux.org/title/Parted_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

`GNU Parted` 是创建和处理分区表的程序.  `GParted`是`GUI`前端.

`Parted` 有两种模式: 命令行和交互,请用下面命令启动: `#`表示超级用户模式

```bash
# parted device  # device 是要编辑的硬盘设备,例如 /dev/sda
```

***
`命令行模式`: 在命令行模式下,可以同时执行一个或多个命令:

```bash
# parted /dev/sda mklabel gpt mkpart P1 ext3 1MiB 8MiB
```

***
`交互模式`: 交互模式简化了分区过程,可以自动对设备执行分区操作. 打开需要新建分区表的设备:

```bash
sudo parted /dev/sdx
```

命令提示符会从 (`#`) 变成 (`parted`): 要查看可用的命令: `(parted) help`
完成操作后,用下面命令退出: `(parted) quit`.

如果命令没带参数,`Parted` 会进行询问:

```bash
(parted) mklabel
New disk label type? gpt
```

`parted 2.4` 开始,当使用`MiB`, `GiB`, `TiB` 等 `IEC`(国际电工委员会)单位时,`parted` 会使用精确数值,不进行修正,它们是`2`次幂的容量单位.
而使用`4GB`这样的设置时,可能会有前后`500MB`的误差. 所以在创建分区时,应该指定比特(`B`),扇区(`s`)或`IEC`二进制单位`MiB`,不要使用`MB`,`GB.

***
`创建新分区表`: 如果设备没有分区,或者要改变分区表类型,重建分区结构,需要新建分区表.

+ 打开分区: `# parted /dev/sdx`:
+ 为 `BIOS` 系统创建 `MBR/msdos` 分区表: `(parted) mklabel msdos`
+ 为`UEFI`系统创建 `GPT` 分区表: `(parted) mklabel gpt`

## 分区方案

您可以决定磁盘应该分为多少个区,每个分区如何映射至目录(一般称此为挂载点),取决于您的分区方案. 需要满足:

+ 至少需要创建一个 `/` (`root`) 目录,有些分区类型和`启动加载器`(`bootloader`)组合有额外的分区要求:
+ `BIOS/GPT`+`GRUB`: 需要按照 `BIOS` 启动分区设置 的方式创建一个 `1M` 或 `2M` 的 `EF02` 类型分区.
+ `UEFI` 的主板,需要一个 `EFI` 系统分区.
+ 如果您需要加密磁盘,则必须加以调整分区方案. 系统安装后,也可以再配置加密文件夹,容器或 `home` 目录.

系统需要需要 `/boot`, `/home` 等目录, [Arch 文件系统架构](https://man.archlinux.org/man/file-hierarchy.7) 有各目录的详细介绍.
如果没有创建单独的`/boot` 或 `/home` 分区,这些目录直接放到了根分区下面

用下面命令打开 `parted` 交互模式:

```bash
# parted /dev/sdx
```

用下面命令创建分区: `(parted) mkpart part-type fs-type start end`

+ `part-type` 是分区类型,可以选择 `primary`, `extended` 或 `logical`,仅用于 `MBR `分区表.
+ `fs-type` 是文件系统类型,支持的类型列表可以通过 `help mkpart` 查看.  `mkpart` 并不会实际创建文件系统, `fs-type` 参数仅是让 `parted` 设置一个 `1-byte` 编码,让启动管理器可以提前知道分区中有什么格式的数据. 参阅 Wikipedia:Disk partitioning#PC partition types.
Tips: 大多数 Linux native file systems 对应的分区码相同 (`0x83`), 所以对一个 ext4 格式的分区使用例如 ext2 类型是安全的.

+ `start` 是分区的起始位置,可以带单位, 例如 `1M` 指 `1MiB`.
+ `end` 是设备的结束位置(不是与 `start` 值的差),同样可以带单位,也可以用百分比,例如 `100%` 表示到设备的末尾.
+ 为了不留空隙,分区的开始和结束应该首尾相连.

Warning: 分区不相互重叠是很重要的: 如果您不想在设备中留下未使用的空间,请确保每个分区从上一个分区的结尾处开始. 如果看到下面警告:

```bash
Warning: The resulting partition is not properly aligned for best performance.
Ignore/Cancel?
```

表示分区没对齐,请参照分区对齐进行修正.

下面命令设置 `/boot` 为启动目录:

```bash
(parted) set <partition> boot on
```

`<partition>` 是分区的编号,从 `print` 命令获取.

***
UEFI/GPT 示例

首先需要一个`EFI`系统分区.如果是和 `Windows` 双系统启动,此分区已经存在,不要重新创建. 用下面命令创建分区 (建议大小是 `512MiB` ).

```bash
(parted) mkpart ESP fat32 1M 513M
(parted) set 1 boot on
```

剩下的空间可以按需要创建. 可以`root` 占用全部 `100%` 剩余空间:

```bash
(parted) mkpart primary ext4 513M 100%
```

也可以`/`(`20GiB`),剩下的给 `/home`:

```bash
(parted) mkpart primary ext4 513M 20.5G
(parted) mkpart primary ext4 20.5G 100%
```

创建 `/` (`20GiB`), `swap` (`4Gib`), 剩下给 `/home`:

```bash
(parted) mkpart primary ext4 513M 20.5G
(parted) mkpart primary linux-swap 20.5G 24.5G
(parted) mkpart primary ext4 24.5G 100%
```

***
`BIOS/MBR` 示例

单个`/`目录分区:

```bash
(parted) mkpart primary ext4 1M 100%
(parted) set 1 boot on
```

`/` (`20Gib`)分区,剩下的给 `/home`:

```bash
(parted) mkpart primary ext4 1M 20G
(parted) set 1 boot on
(parted) mkpart primary ext4 20G 100%
```

`/boot` (`100MiB`), `/` (`20Gib`), `swap` (`4GiB`) 剩下的给 `/home`:

```bash
(parted) mkpart primary ext4 1M 100M
(parted) set 1 boot on
(parted) mkpart primary ext4 100M 20G
(parted) mkpart primary linux-swap 20G 24G
(parted) mkpart primary ext4 24G 100%
```

***
双启动 Windows XP

如果您打算将一个同属于启动分区的 Windows XP 分区移动到另一块硬盘,只要将以下的注册表删除,之后就可以用 GParted 轻易地操作,Windows 不会出现任何问题:

    HKEY_LOCAL_MACHINE\SYSTEM\MountedDevices

## 调整分区

警告:  若要调整分区,必须先停止使用并卸载它. 如果无法卸载(比如,它被挂载到 `/`,使用安装介质或者备用系统.  )

注意:

+ 使用 `parted`,你只能移动分区的末尾.
+ 在 parted v4.2 `resizepart` 可能需要使用#Interactive `mode`
+ 这些说明适用于格式为`ext2`,`ext3`或 `ext4` 文件系统的分区.

如果要扩展分区,则必须先调整`分区`的大小,然后再调整其上的`文件系统`的大小;
如果要缩小分区,必须先调整`文件系统`的大小,再调整`分区`,以避免数据丢失.

***
扩展分区(在 `parted` 交互模式下):

```bash
(parted) resizepart number end
```

其中 `number` 是您正在扩展的分区的编号,而 `end` 是该分区的新末端(需要大于旧的末端).

然后,扩展此分区上的文件系统:

```bash
# resize2fs /dev/sdaX size
```

`sdaX` 代表您正在扩展的分区,而 `size` 是分区的新大小.

***
缩小分区上的文件系统:

```bash
# resize2fs /dev/sdaX size
```

其中 `sdaX` 代表您要缩小的分区,而 `size` 是该分区的新大小.

然后缩小分区(在 `parted` 交互模式下):

```bash
(parted) resizepart number end
```

其中 `number` 是您要缩小的分区的编号,而 `end` 是该分区的新末端(需要小于旧末端).

完成后,使用 `util-linux` 中的 `resizepart` 命令告诉内核新的分区大小:

```bash
# resizepart device number size
```

其中 `device` 是保存分区的设备,`number` 是分区的编号, `size` 是分区的新大小.

## 用mkfs命令创建一个新的文件系统

完成了分区编辑工作(它或许是轻量级的),是时候在我们的闪存驱动器上创建一个新的文件系统了.

为此,我们会使用 `mkfs`(make file system 的简写),它能创建各种格式的文件系统.
在此设备上创建一个 `ext3`文件系统,我们使用`-t` 选项来指定这个`ext3`系统类型,随后是我们要格式化的设备分区名称:

创建`ext4`格式的文件系统,可以使用`mkfs.ext4 `

```bash
$ sudo mkfs -t ext3 /dev/sdb1
mke2fs 1.40.2 (12-Jul-2007)
Filesystem label=
OS type: Linux
Block size=1024 (log=0)
...
```

当 `ext3` 被选为文件系统类型时,这个程序会显示许多信息.
若把这个设备重新格式化为它最初的`FAT32`文件系统,指定`vfat`作为文件系统类型:

```bash
$ sudo mkfs -t vfat /dev/sdb1
```

任何时候添加额外的存储设备到系统中时,都可以使用这个分区和格式化的过程.
虽然我们只以一个小小的闪存驱动器为例,同样的操作可以被应用到内部硬盘和其它可移动的存储设备上像 USB 硬盘驱动器.

## 测试和修复文件系统

在之前讨论文件`/etc/fstab` 时,我们会在每行的末尾看到一些神秘的数字.
每次系统启动时, 在挂载系统之前,都会按照惯例检查文件系统的完整性.

这个任务由 fsck 程序(是`file system check`的简写)完成.每个 `fstab` 项中的最后一个数字指定了设备的检查顺序.
在上面的实例中,我们看到首先检查根文件系统,然后是 `home` 和 `boot` 文件系统. 若最后一个数字 是零则相应设备不会被检查.

除了检查文件系统的完整性之外,`fsck` 还能修复受损的文件系统,其成功度依赖于损坏的数量.
在类 Unix 的文件系统中,文件恢复的部分被放置于 `lost+found` 目录里面,位于每个文件系统的根目录下面.

检查我们的闪存驱动器(首先应该卸载),我们能执行下面的操作:

```bash
$ sudo fsck /dev/sdb1
fsck 1.40.8 (13-Mar-2008)
e2fsck 1.40.8 (13-Mar-2008)
/dev/sdb1: clean, 11/3904 files, 1661/15608 blocks
```

以我的经验,文件系统损坏情况相当罕见,除非硬件存在问题,如磁盘驱动器故障.
在大多数系统中,系统启动阶段若探测到文件系统已经损坏了,则会导致系统停止下来, 在系统继续执行之前,会指导你运行 `fsck` 程序.

### 什么是 fsck?

在 Unix 文化中,`fsck`这个单词往往会被用来代替一个流行的词,`fsck`和这个词共享了三个字母.
尤其适用于,当你不得不使用 `fsck` 命令时,你可能会说出这个词汇.

## 格式化软盘

对于那些还在使用配备了软盘驱动器的计算机的用户,我们也能管理这些设备.

准备一 张可用的空白软盘要分两个步骤.首先,对这张软盘执行低级格式化,然后创建一个文件系统.
为了完成格式化,我们使用 `fdformat` 程序,同时指定软盘设备名称(通常为`/dev/fd0`):

```bash
$ sudo fdformat /dev/fd0
Double-sided, 80 tracks, 18 sec/track. Total capacity 1440 kB.
Formatting ... done
Verifying ... done
```

接下来,通过 `mkfs` 命令,给这个软盘创建一个 `FAT` 文件系统:

```bash
$ sudo mkfs -t msdos /dev/fd0
```

注意我们使用这个`msdos`文件系统类型来得到旧(小的)风格的文件分配表.
当一个软磁盘被准备好之后,则可能像其它设备一样挂载它.

## 直接把数据移入/出设备

虽然我们通常认为计算机中的数据以文件形式来组织数据,但也可以按`原始的`形式来考虑数据.

如果我们能看到磁盘驱动器上的数据,我们会发现它由大量的`数据块`组成,而操作系统把这些数据块看作目录和文件.
然而,如果把磁盘驱动器简单地看成一个数据块大集合,我们就能执行有用的任务,如克隆设备.

`dd` 程序能执行此任务.它可以把数据块从一个地方复制到另一个地方.它使用独特的语法(由于历史原因):

```bash
dd if=input_file of=output_file [bs=block_size [count=blocks]]
```

比方说我们有两个相同容量的 USB 闪存驱动器,并且要精确地把第一个驱动器(中的内容) 复制给第二个.
如果连接两个设备到计算机上,它们各自被分配到设备`/dev/sdb` 和 `/dev/sdc `上,
这样我们就能通过下面的命令把第一个驱动器中的所有数据复制到第二个驱动器中.

```bash
dd if=/dev/sdb of=/dev/sdc
```

或者,如果只有第一个驱动器被连接到计算机上,我们可以把它的内容复制到一个普通文件中供以后恢复或复制数据:

```bash
dd if=/dev/sdb of=flash_drive.img
```

警告!这个 `dd` 命令非常强大.
虽然它的名字来自于`数据定义`,有时候也把它叫做`清除磁盘` 因为用户经常会误输入 `if` 或 `of` 的规范.
在按下回车键之前,要再三检查输入与输出规范!

## 创建 CD-ROM 映像

写入一个可记录的 CD-ROM(一个 CD-R 或者是 CD-RW)由两步组成:
首先,构建一个 iso 映像文件, 这就是一个 CD-ROM 的文件系统映像,
第二步,把这个映像文件写入到 CD-ROM 媒介中.

### 创建一个 CD-ROM 的映像拷贝

如果想要制作一张现有 CD-ROM 的 iso 映像,我们可以使用 dd 命令来读取 CD-ROM 中的所有数据块, 并把它们复制到本地文件中.

比如说我们有一张 Ubuntu CD,用它来制作一个 iso 文件,以后我们可以用它来制作更多的拷贝.
插入这张 CD 之后,确定 它的设备名称(假定是`/dev/cdrom`),然后像这样来制作 iso 文件:

```bash
dd if=/dev/cdrom of=ubuntu.iso
```

这项技术也适用于 DVD 光盘,但是不能用于音频 CD,因为它们不使用文件系统来存储数据.
对于音频 CD,看一下 `cdrdao` 命令.

### 从文件集合中创建一个映像

创建一个包含目录内容的 iso 映像文件,我们使用 `genisoimage` 程序.

为此,我们首先创建 一个目录,这个目录中包含了要包括到此映像中的所有文件,然后执行这个 `genisoimage` 命令 来创建映像文件.

例如,如果我们已经创建一个叫做`~/cd-rom-files` 的目录,
然后用文件 填充此目录,再通过下面的命令来创建一个叫做 `cd-rom.iso` 映像文件:

```bash
genisoimage -o cd-rom.iso -R -J ~/cd-rom-files
```

`-R`选项添加元数据为 Rock Ridge 扩展,这允许使用长文件名和 POSIX 风格的文件权限.
同样地,这个`-J`选项使 Joliet 扩展生效,这样 Windows 中就支持长文件名了.

### 一个有着其它名字的程序...

如果你看一下关于创建和烧写光介质如 CD-ROMs 和 DVD 的在线文档,你会经常碰到两个程序 叫做 `mkisofs` 和 `cdrecord` .
这些程序是流行软件包`cdrtools`的一部分,`cdrtools`由 Jorg Schilling 编写成.

在2006年春天,Schilling 先生更改了部分 cdrtools 软件包的协议,Linux 社区许多人的看法是, 这创建了一个与 `GNU GPL` 不相兼容的协议.
结果,就 `fork` 了这个 `cdrtools` 项目, 目前新项目里面包含了 `cdrecord` 和 `mkisofs` 的替代程序,分别是 `wodim` 和 `genisoimage` .

## 写入 CD-ROM 镜像

有了一个映像文件之后,我们可以把它烧写到光盘中.下面讨论的大多数命令对可 记录的 CD-ROM 和 DVD 媒
介都适用.

### 直接挂载一个 ISO 镜像

有一个诀窍,我们可以用它来挂载 `iso` 映像文件,虽然此文件仍然在我们的硬盘中,但我们当作它已经在光盘中了.

添加 `-o loop` 选项来挂载(同时带有必需的 `-t iso9660` 文件系统类型), 挂载这个映像文件就好像它是一台设备,把它连接到文件系统树上:

```bash
mkdir /mnt/iso_image
mount -t iso9660 -o loop image.iso /mnt/iso_image
```

上面的示例中,我们创建了一个挂载点叫做`/mnt/iso_image`,然后把此映像文件 `image.iso` 挂载到挂载点上.

映像文件被挂载之后,可以把它当作,就好像它是一张 真正的 CD-ROM 或者 DVD.当不再需要此映像文件后,记得卸载它.

### 清除一张可重写入的 CD-ROM

可重写入的 CD-RW 媒介在被重使用之前需要擦除或清空.为此,我们可以用 `wodim` 命令,指定 设备名称和清空的类型.
`wodim` 程序提供了几种清空类型.最小(且最快)的是 `fast` 类型:

```bash
wodim dev=/dev/cdrw blank=fast
```

### 写入镜像

写入一个映像文件,我们再次使用 `wodim` 命令,指定光盘设备名称和映像文件名:

```bash
wodim dev=/dev/cdrw image.iso
```

除了设备名称和映像文件之外,`wodim` 命令还支持非常多的选项.
常见的两个选项是,`-v` 可详细输出, 和`-dao` 以 `disk-at-once` 模式写入光盘.

如果你正在准备一张光盘为的是商业复制,那么应该使用这种模式.
`wodim` 命令的默认模式是 track-at-once,这对于录制音乐很有用.

拓展阅读

我们刚才谈到了很多方法,可以使用命令行管理存储介质.看看我们所讲过命令的手册页. 一些命令支持大量的选项和操作.
此外,寻找一些如何添加硬盘驱动器到 Linux 系统(有许多)的在线教程, 这些教程也要适用于光介质存储设备.

友情提示

通常验证一下我们已经下载的 iso 映像文件的完整性很有用处.
在大多数情况下,iso 映像文件的贡献者也会提供 一个 `checksum` 文件.
一个 `checksum` 是一个神奇的数学运算的计算结果,这个数学计算会产生一个能表示目标文件内容的数字.
如果目标文件的内容即使更改一个二进制位,`checksum` 的结果将会非常不一样.

`生成checksum` 数字的最常见方法是使用 `md5sum` 程序.当你使用 `md5sum` 程序的时候, 它会产生一个独一无二的十六进制数字:

```bash
md5sum image.iso
34e354760f9bb7fbf85c96f6a3f94ece
image.iso
```

当你下载完映像文件之后,你应该对映像文件执行 `md5sum` 命令,然后把运行结果与发行商提供的 `md5sum` 数值作比较.

除了检查下载文件的完整性之外,我们也可以使用 `md5sum` 程序验证新写入的光学存储介质.

为此,首先我们计算映像文件的 `checksum` 数值,然后计算此光学存储介质的 `checksum` 数值.
这种验证光学介质的技巧是限定只对 光学存储介质中包含映像文件的部分计算 checksum 数值.
通过确定映像文件所包含的 2048 个字节块的数目(光学存储介质总是以 2048 个字节块的方式写入) 并从存储介质中读取那么多的字节块,我们就可以完成操作.

某些类型的存储介质,并不需要这样做.一个以 disk-at-once 模式写入的 CD-R,可以用下面的方式检验:

```bash
md5sum /dev/cdrom
34e354760f9bb7fbf85c96f6a3f94ece
/dev/cdrom
```

许多存储介质类型,如 DVD 需要精确地计算字节块的数目.

在下面的例子中,我们检验了映像文件 `dvd-image.iso `以及 DVD 光驱中磁盘 `/dev/dvd` 文件的完整性.
你能弄明白这是怎么回事吗?

```bash
md5sum dvd-image.iso
dd if=/dev/dvd bs=2048 count=$(( $(stat -c "%s" dvd-image.iso) / 2048 )) |  md5sum
```
