# hardware

## Hostname

[What Is a Hostname? ](https://www.lifewire.com/what-is-a-hostname-2625906)

域名(英语:`Domain Name`),又称网域,是由一串用点分隔的名字组成的`Internet`上某一台计算机或计算机组的名称,
用于在数据传输时对计算机的定位标识(有时也指地理位置)

由于`IP`地址具有不方便记忆并且不能显示地址组织的名称和性质等缺点,人们设计出了域名,
并通过网域名称系统(`DNS`,`Domain Name System`)来将域名和`IP`地址相互映射,使人更方便地访问互联网,
而不用去记住能够被机器直接读取的`IP`地址数串

A `hostname` is a label assigned to a `device` (a host) on a `network`.

It distinguishes one device from another on a specific network or over the internet.
The hostname for a computer on a home network may be something like `new laptop`, `Guest-Desktop`, or `FamilyPC`.

Hostnames are also used by `DNS` servers so you can access a website by a common, easy-to-remember name. This way, you don't have to remember a string of numbers (an `IP address`) to open a website.

A computer's hostname may instead be referred to as a computer name, sitename, or nodename.
You may also see hostname `spelled` as host name.

### 主机的例子,Hostname

Each of the following is an example of a Fully Qualified Domain Name with its hostname written off to the side:

+ `www.google.com: www`
+ `images.google.com: images`
+ `products.office.com: products`
+ `www.microsoft.com: www`

The hostname (like `products`) is the text that *precedes* the `domain` name (for example, office), which is the text that comes before the *top-level domain* (`.com`).

### 在 windows 中找出 Hostname

Executing `hostname` from the Command Prompt is the easiest way to show the hostname of a computer.

## Linux查看硬件信息

[用 Linux 命令显示硬件信息 ](https://linux.cn/article-11422-1.html)

最简单的方法是使用标准的 Linux GUI 程序之一:

+ `i-nex` 收集硬件信息, 并且类似于 Windows 下流行的 `CPU-Z` 的显示.
+ `HardInfo` 显示硬件具体信息, 甚至包括一组八个的流行的性能基准程序, 你可以用它们运算你的系统性能.
+ `KInfoCenter` 和 `Lshw` 也能够显示硬件的详细信息, 并且可以从许多软件仓库中获取.

### 硬件概述

`inxi` 命令能够列出包括 CPU, 图形, 音频, 网络, 驱动, 分区, 传感器等详细信息. 当论坛里的人尝试帮助其他人解决问题的时候, 他们常常询问此命令的输出.

```bash
inxi -Fxz
```

`-F` 参数意味着你将得到完整的输出, `x` 增加细节信息, `z` 参数隐藏像 `MAC` 和 `IP` 等私人身份信息.

***
`hwinfo` 和 `lshw` 命令以不同的格式显示大量相同的信息:
`hwinfo --short` 或 `lshw -short`
这两条命令的长格式输出非常详细, 但也有点难以阅读:
`hwinfo` 或`lshw`

### CPU 详细信息

通过命令你可以了解关于你的 CPU 的任何信息. 使用 `lscpu` 命令或与它相近的 `lshw` 命令查看 `CPU` 的详细信息:

`lscpu`  或 `lshw -C cpu`

在这两个例子中, 输出的最后几行都列出了所有 `CPU` 的功能. 你可以查看你的处理器是否支持特定的功能.

使用这些命令的时候, 你可以使用 `grep` 过滤信息. 例如, 只查看 CPU 品牌和型号:

```bash
lshw -C cpu | grep -i product
```

仅查看 `CPU` 的速度(兆赫兹):

```bash
lscpu | grep -i mhz
```

或其 `BogoMips` 额定功率:

```bash
lscpu | grep -i bogo
```

`grep` 命令的 `-i`参数代表搜索结果忽略大小写.

### 内存

`Linux` 命令行使你能够收集关于你的计算机内存的所有可能的详细信息.
你甚至可以不拆开计算机机箱就能确定是否可以为计算机添加额外的内存条.

使用 `dmidecode` 命令列出每根内存条和其容量:

```bash
dmidecode -t memory | grep -i size
```

使用以下命令获取系统内存更多的信息, 包括类型, 容量, 速度和电压:

```bash
lshw -short -C memory
```

你肯定想知道的一件事是你的计算机可以安装的最大内存:

```bash
dmidecode -t memory | grep -i max
```

现在检查一下计算机是否有空闲的插槽可以插入额外的内存条. 你可以通过使用命令在不打开计算机机箱的情况下就做到:

```bash
lshw -short -C memory | grep -i empty
```

输出为空则意味着所有的插槽都在使用中.

### 显卡

确定你的计算机拥有多少显卡内存需要下面的命令. 首先使用 `lspci` 列出所有设备信息然后过滤出你想要的显卡设备信息:

```bash
lspci | grep -i vga
```

视频控制器的设备号输出信息通常如下:

```bash
00:02.0 VGA compatible controller: Intel Corporation 82Q35 Express Integrated Graphics Controller (rev 02)
```

现在再加上视频设备号重新运行 `lspci` 命令:

```bash
lspci -v -s 00:02.0
```

输出信息中 `prefetchable` 那一行显示了系统中的显卡内存大小:

使用`xrandr` 可以设置屏幕输出的大小, 方向或反射.  它还可以设置屏幕尺寸.

***
安装专有显卡驱动:

首先查看显卡硬件型号`ubuntu-drivers devices`, 使用`ubuntu-drivers -h`可以查看使用帮助:

用法: `ubuntu-drivers [OPTIONS] COMMAND [ARGS]...`

选项:

`--gpgpu`:gpgpu驱动程序
`--free-only`:仅考虑免费软件包
`--package-list PATH`:使用已安装软件包列表,创建文件(在`install`模式)
`--no-oem`: 不包括OEM软件包,默认值: `False`
`-h`, `--help`:显示此消息并退出.

命令:

`autoinstall`:已弃用, 请改用`install`
`debug`:打印有关驱动程序的所有可用信息和调试数据.
`devices`:显示所有需要驱动程序的设备以及哪些软件包可用
`install`:安装驱动程序`[driver[:version][,driver[:version]]]`
`list`:显示适用于当前系统的所有驱动程序包.
`  list-oem`:显示适用于此系统的所有OEM软件包

***
使用下面的命令展示当前内存使用量(兆字节):

```bash
free -m
```

这条命令告诉你多少内存是空闲的, 多少命令正在使用中以及交换内存的大小和是否正在使用. `top` 命令为你提供内存使用更加详细的信息.
它显示了当前全部内存和 CPU 使用情况并按照进程 ID, 用户 ID 及正在运行的命令细分. 同时这条命令也是全屏输出.

### 磁盘文件系统和设备

你可以轻松确定有关磁盘, 分区, 文件系统和其他设备信息.

显示每个磁盘设备的描述信息:

```bash
lshw -short -C disk
```

通过以下命令获取任何指定的 `SATA` 磁盘详细信息, 例如其型号, 序列号以及支持的模式和扇区数量等:

```bash
hdparm -i /dev/sda
```

当然, 如果需要的话你应该将 `sda` 替换成 `sdb` 或者其他设备号.

列出所有磁盘及其分区和大小:

```bash
lsblk
```

使用以下命令获取更多有关扇区数量, 大小, 文件系统 ID 和 类型以及分区开始和结束扇区:

```bash
fdisk -l
```

要启动 Linux, 你需要确定 `GRUB` 引导程序的可挂载分区. 你可以使用 `blkid` 命令找到此信息. 它列出了每个分区的唯一标识符(UUID)及其文件系统类型(例如 ext3 或 ext4):

```bash
blkid
```

使用以下命令列出已挂载的文件系统和它们的挂载点, 以及已用的空间和可用的空间(兆字节为单位):

```bash
df -m
```

最后, 你可以列出所有的 `USB` 和 `PCI` 总线以及其他设备的详细信息:

```bash
lsusb
```

或

```bash
lspci
```

### 网络

Linux 提供大量的网络相关命令, 下面只是几个例子.

查看你的网卡硬件详细信息:

```bash
lshw -C network
```

`ifconfig` 是显示网络接口的传统命令:

```bash
ifconfig -a
```

但是现在很多人们使用:

```bash
ip link show
```

或

```bash
netstat -i
```

在阅读输出时, 了解常见的网络缩写十分有用:

缩写    含义

+ `lo`    回环接口
+ `eth0` 或 `enp*`    以太网接口
+ `wlan0`    无线网接口
+ `ppp0`    点对点协议接口(由拨号调制解调器, PPTP VPN 连接或者 USB 调制解调器使用)
+ `vboxnet0` 或 `vmnet*`    虚拟机网络接口

表中的星号是通配符, 代表不同系统的任意字符.

使用以下命令显示默认网关和路由表:

```bash
ip route | column -t
```

或

```bash
netstat -r
```

### 软件

让我们以显示最底层软件详细信息的两条命令来结束.
例如, 如果你想知道是否安装了最新的固件该怎么办? 这条命令显示了 `UEFI` 或 `BIOS` 的日期和版本:

```bash
dmidecode -t bios
```

内核版本是多少, 以及它是 64 位的吗? 网络主机名是什么? 使用下面的命令查出结果:

```bash
uname -a
```

### 快速查询表

用途   命令

+ 显示所有硬件信息   `inxi -Fxz` 或 `hwinfo --short` 或 `lshw  -short`
+ `CPU` 信息   `lscpu` 或 `lshw -C cpu`
+ 显示 `CPU` 功能(例如 PAE, SSE2)  `lshw -C cpu | grep -i capabilities`
+ 报告 `CPU` 位数   `lshw -C cpu | grep -i width`
+ 显示当前内存大小和配置   `dmidecode -t memory | grep -i size` 或 `lshw -short -C memory`
+ 显示硬件支持的最大内存   `dmidecode -t memory | grep -i max`
+ 确定是否有空闲内存插槽   `lshw -short -C memory | grep -i empty`(输出为空表示没有可用插槽)
+ 确定显卡内存数量   `lspci | grep -i vga` 然后指定设备号再次使用; 例如: `lspci -v -s 00:02.0`
显卡内存数量就是  `prefetchable` 的值
+ 显示当前内存使用情况  `free -m` 或 `top`
+ 列出磁盘驱动器   `lshw -short -C disk`
+ 显示指定磁盘驱动器的详细信息   `hdparm -i /dev/sda`(需要的话替换掉 `sda` )
+ 列出磁盘和分区信息   `lsblk`(简单) 或 `fdisk -l`(详细)
+ 列出分区 ID(UUID)   `blkid`
+ 列出已挂载文件系统挂载点以及已用和可用空间   d`f -m`
+ 列出 USB 设备   `lsusb`
+ 列出 PCI 设备   `lspci`
+ 显示网卡详细信息  ` lshw -C network`
+ 显示网络接口   `ifconfig -a` 或 `ip link show` 或 `netstat -i`
+ 显示路由表   `ip route | column -t` 或 `netstat -r`
+ 显示 UEFI/BIOS 信息   `dmidecode -t bios`
+ 显示内核版本网络主机名等   `uname -a`

[Linux下/proc目录简介](https://blog.csdn.net/zdwzzu2006/article/details/7747977)

Linux 内核提供了一种通过 `/proc` 文件系统,在运行时访问内核内部数据结构, 改变内核设置的机制.proc文件系统是一个伪文件系统,它只存在内存当中,而不占用外存空间.它以文件系统的方式为访问系统内核数据的操作提供接口.

用户和应用程序可以通过proc得到系统的信息,并可以改变内核的某些参数.
由于系统的信息,如进程,是动态改变的,所以用户或应用程序读取proc文件时,proc文件系统是动态从系统内核读出所需信息并提交的.
下面列出的这些文件或子文件夹,并不是都是在你的系统中存在,这取决于你的内核配置和装载的模块.
另外,在`/proc`下还有三个很重要的目录:`net`,`scsi`和`sys`. `Sys`目录是可写的,可以通过它来访问或修改内核的参数,而`net`和`scsi`则依赖于内核配置.例如,如果系统不支持`scsi`,则`scsi`目录不存在.

除了以上介绍的这些,还有的是一些以数字命名的目录,它们是进程目录.
系统中当前运行的每一个进程都有对应的一个目录在`/proc`下,以进程的 `PID`号为目录名,它们是读取进程信息的接口.
而`self`目录则是读取进程本身的信息接口,是一个`link`.
