# bash-3

## Ubuntu 镜像使用帮助

清华大学的源

域名选择

```bash
https://mirrors.tuna.tsinghua.edu.cn 自动选择
https://mirrors6.tuna.tsinghua.edu.cn 只解析 IPv6
https://mirrors4.tuna.tsinghua.edu.cn 只解析 IPv4
```

Ubuntu 的软件源配置文件是 `/etc/apt/sources.list`.将系统自带的该文件做个备份,将该文件替换为下面内容,即可使用 `TUNA` 的软件源镜像.

```bash
# 默认注释了源码镜像以提高 apt update 速度,如有需要可自行取消注释
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic main restricted universe multiverse
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-updates main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-updates main restricted universe multiverse
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-backports main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-backports main restricted universe multiverse
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-security main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-security main restricted universe multiverse

# 预发布软件源,不建议启用
# deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-proposed main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-proposed main restricted universe multiverse
```

### dpkg-buildpackage

`dget` -- Download Debian source and binary packages

SYNOPSIS

```
dget [options] URL ...
dget [options] [--all] package[=version] ...
```

DESCRIPTION

dget downloads Debian packages.

In the first form, dget fetches the requested `URLs`.  If this is a `.dsc` or `.changes` file,
then dget acts as a source-package aware form of wget: it also fetches any files referenced in the `.dsc/.changes` file.
The downloaded source is then checked with `dscverify` and, if successful, unpacked by `dpkg-source`.

6.1. 完整的(重)构建

为保证完整的软件包(重)构建能顺利进行,你必须保证系统中已经安装

    build-essential 软件包；

    列于 Build-Depends 域的软件包(参看 第 4.1 节 `control`)；

    列于 Build-Depends-indep 域的软件包(参看 第 4.1 节 `control`).

然后在源代码目录中执行以下命令:

$ dpkg-buildpackage -us -uc

这样会自动完成所有从源代码包构建二进制包的工作,包括:

    清理源代码树(debian/rules clean)

    构建源代码包(dpkg-source -b)

    构建程序(debian/rules build)

    构建二进制包(fakeroot debian/rules binary)

    制作 .dsc 文件

    用 dpkg-genchanges 命令制作 .changes 文件.

如果构建结果令人满意,那就用 debsign 命令以你的私有 GPG 密钥签署 .dsc 文件和 .changes 文件.你需要输入密码两次. [63]

对于非本土 Debian 软件包,比如 gentoo, 构建软件包之后,你将会在上一级目录(~/gentoo) 中看到下列文件:

    gentoo_0.9.12.orig.tar.gz

    这是原始的源代码 tarball,最初由 dh_make -f ../gentoo-0.9.12.tar.gz 命令创建,它的内容与上游 tarball 相同,仅被重命名以符合 Debian 的标准.

    gentoo_0.9.12-1.dsc

    这是一个从 control 文件生成的源代码概要,可被 dpkg-source(1) 程序解包.

    gentoo_0.9.12-1.debian.tar.gz

    这个压缩的 Tar 归档包含你的 debian 目录内容.其他所有对于源代码的修改都由 quilt 补丁存储于 debian/patches 中.

    如果其他人想要重新构建你的软件包,他们可以使用以上三个文件很容易地完成.只需复制三个文件,再运行 dpkg-source -x gentoo_0.9.12-1.dsc. [64]

    gentoo_0.9.12-1_i386.deb

    这是你的二进制包,可以使用 dpkg 程序安装或卸载它,就像其他软件包一样.

    gentoo_0.9.12-1_i386.changes

    这个文件描述了当前修订版本软件包中的全部变更,它被 Debian FTP 仓库维护程序用于安装二进制和源代码包.它是部分从 changelog 和 .dsc 文件生成的.

    随着你不断完善这个软件包,程序的行为会发生变化,也会有更多新特性添加进来.下载你软件包的人可以查看这个文件来快速找到有哪些变化,Debian 仓库维护程序还会把它的内容发表至 debian-devel-changes@lists.debian.org 邮件列表.

在上传到 Debian FTP 仓库中前,gentoo_0.9.12-1.dsc 文件和 gentoo_0.9.12-1_i386.changes 文件必须用 debsign 命令签署,其中使用你自己存放在 ~/.gnupg/ 目录中的 GPG 私钥. 用你的公钥,可以令 GPG 签名证明这些文件真的是你的.

debsign 命令可以用来以指定 ID 的 GPG 密钥进行签署 (这方便了赞助(sponsor)软件包), 只要照着下边在 ~/.devscripts 中的内容:

DEBSIGN_KEYID=Your_GPG_keyID

.dsc 和 .changes 文件中很长的数字串是其中提及文件的 SHA1/SHA256 校验和.下载你软件包的人可以使用 sha1sum(1) 或 sha256sum(1) 来进行核对.如果校验和不符,则说明文件已被损坏或偷换

### Hostname

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

### Examples of a Hostname

Each of the following is an example of a Fully Qualified Domain Name with its hostname written off to the side:

+ `www.google.com: www`
+ `images.google.com: images`
+ `products.office.com: products`
+ `www.microsoft.com: www`

The hostname (like `products`) is the text that *precedes* the `domain` name (for example, office), which is the text that comes before the *top-level domain* (`.com`).

### How to Find a Hostname in Windows

Executing `hostname` from the Command Prompt is the easiest way to show the hostname of a computer.

## Linux查看硬件信息 

[用 Linux 命令显示硬件信息 ](https://linux.cn/article-11422-1.html)

最简单的方法是使用标准的 Linux GUI 程序之一:

+ `i-nex` 收集硬件信息, 并且类似于 Windows 下流行的 `CPU-Z` 的显示. 
+ `HardInfo` 显示硬件具体信息, 甚至包括一组八个的流行的性能基准程序, 你可以用它们评估你的系统性能. 
+ `KInfoCenter` 和 `Lshw` 也能够显示硬件的详细信息, 并且可以从许多软件仓库中获取. 

### 硬件概述

`inxi` 命令能够列出包括 CPU, 图形, 音频, 网络, 驱动, 分区, 传感器等详细信息. 当论坛里的人尝试帮助其他人解决问题的时候, 他们常常询问此命令的输出. 

```bash
inxi -Fxz
```

`-F` 参数意味着你将得到完整的输出, `x` 增加细节信息, `z` 参数隐藏像 `MAC` 和 `IP` 等私人身份信息. 

***
`hwinfo` 和 `lshw` 命令以不同的格式显示大量相同的信息：
`hwinfo --short` 或 `lshw -short`
这两条命令的长格式输出非常详细, 但也有点难以阅读：
`hwinfo` 或`lshw`

### CPU 详细信息

通过命令你可以了解关于你的 CPU 的任何信息. 使用 `lscpu` 命令或与它相近的 `lshw` 命令查看 `CPU` 的详细信息：

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

使用 `dmidecode` 命令列出每根内存条和其容量：

```bash
dmidecode -t memory | grep -i size
```

使用以下命令获取系统内存更多的信息, 包括类型, 容量, 速度和电压：

```bash
lshw -short -C memory
```

你肯定想知道的一件事是你的计算机可以安装的最大内存：

```bash
dmidecode -t memory | grep -i max
```

现在检查一下计算机是否有空闲的插槽可以插入额外的内存条. 你可以通过使用命令在不打开计算机机箱的情况下就做到：

```bash
lshw -short -C memory | grep -i empty
```

输出为空则意味着所有的插槽都在使用中. 

### 显卡

确定你的计算机拥有多少显卡内存需要下面的命令. 首先使用 `lspci` 列出所有设备信息然后过滤出你想要的显卡设备信息:

```bash
lspci | grep -i vga
```

视频控制器的设备号输出信息通常如下：

```bash
00:02.0 VGA compatible controller: Intel Corporation 82Q35 Express Integrated Graphics Controller (rev 02)
```

现在再加上视频设备号重新运行 `lspci` 命令：

```bash
lspci -v -s 00:02.0
```

输出信息中 `prefetchable` 那一行显示了系统中的显卡内存大小:

使用`xrandr` 可以设置屏幕输出的大小, 方向或反射.  它还可以设置屏幕尺寸.

***
安装专有显卡驱动:

首先查看显卡硬件型号`ubuntu-drivers devices`, 使用`ubuntu-drivers -h`可以查看使用帮助：

用法：`ubuntu-drivers [OPTIONS] COMMAND [ARGS]...`

选项：

`--gpgpu`:gpgpu驱动程序
`--free-only`:仅考虑免费软件包
`--package-list PATH`:使用已安装软件包列表,创建文件(在`install`模式)
`--no-oem`: 不包括OEM软件包,默认值：`False`
`-h`, `--help`:显示此消息并退出. 

命令：

`autoinstall`:已弃用, 请改用`install`
`debug`:打印有关驱动程序的所有可用信息和调试数据. 
`devices`:显示所有需要驱动程序的设备以及哪些软件包可用
`install`:安装驱动程序`[driver[:version][,driver[:version]]]`
`list`:显示适用于当前系统的所有驱动程序包. 
`  list-oem`:显示适用于此系统的所有OEM软件包

***
使用下面的命令展示当前内存使用量(兆字节)：

```bash
free -m
```

这条命令告诉你多少内存是空闲的, 多少命令正在使用中以及交换内存的大小和是否正在使用. `top` 命令为你提供内存使用更加详细的信息. 
它显示了当前全部内存和 CPU 使用情况并按照进程 ID, 用户 ID 及正在运行的命令细分. 同时这条命令也是全屏输出.

### 磁盘文件系统和设备

你可以轻松确定有关磁盘, 分区, 文件系统和其他设备信息. 

显示每个磁盘设备的描述信息：

```bash
lshw -short -C disk
```

通过以下命令获取任何指定的 `SATA` 磁盘详细信息, 例如其型号, 序列号以及支持的模式和扇区数量等：

```bash
hdparm -i /dev/sda
```

当然, 如果需要的话你应该将 `sda` 替换成 `sdb` 或者其他设备号. 

列出所有磁盘及其分区和大小：

```bash
lsblk
```

使用以下命令获取更多有关扇区数量, 大小, 文件系统 ID 和 类型以及分区开始和结束扇区：

```bash
fdisk -l
```

要启动 Linux, 你需要确定 `GRUB` 引导程序的可挂载分区. 你可以使用 `blkid` 命令找到此信息. 它列出了每个分区的唯一标识符(UUID)及其文件系统类型(例如 ext3 或 ext4)：

```bash
blkid
```

使用以下命令列出已挂载的文件系统和它们的挂载点, 以及已用的空间和可用的空间(兆字节为单位)：

```bash
df -m
```

最后, 你可以列出所有的 `USB` 和 `PCI` 总线以及其他设备的详细信息：

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

`ifconfig` 是显示网络接口的传统命令：

```bash
ifconfig -a
```

但是现在很多人们使用：

```bash
ip link show
```

或

```bash
netstat -i
```

在阅读输出时, 了解常见的网络缩写十分有用：

缩写    含义

+ `lo`    回环接口
+ `eth0` 或 `enp*`    以太网接口
+ `wlan0`    无线网接口
+ `ppp0`    点对点协议接口(由拨号调制解调器, PPTP VPN 连接或者 USB 调制解调器使用)
+ `vboxnet0` 或 `vmnet*`    虚拟机网络接口

表中的星号是通配符, 代表不同系统的任意字符. 

使用以下命令显示默认网关和路由表：

```bash
ip route | column -t
```

或

```bash
netstat -r
```

### 软件

让我们以显示最底层软件详细信息的两条命令来结束. 
例如, 如果你想知道是否安装了最新的固件该怎么办？这条命令显示了 `UEFI` 或 `BIOS` 的日期和版本:

```bash
dmidecode -t bios
```

内核版本是多少, 以及它是 64 位的吗？网络主机名是什么？使用下面的命令查出结果：

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
+ 确定显卡内存数量   `lspci | grep -i vga` 然后指定设备号再次使用；例如：`lspci -v -s 00:02.0` 
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

## shell 语法

### 空白字符

[对C标准中空白字符的理解](https://blog.csdn.net/boyinnju/article/details/6877087)
[Shell中去掉文件中的换行符简单方法](https://blog.csdn.net/Jerry_1126/java/article/details/85009615)

`C`标准库里`<ctype.h>`中声明了一个函数:

`int isspace(int c);`

该函数判断字符`c`是否为一个空白字符.

`C`标准中空白字符有六个:
空格(`' '`), 换页(`'\f'`), 换行(`'\n'`), 回车(`'\r'`), 水平制表符(`'\t'`), 垂直制表符(`'\v'`)

***
空格: ASCII码为`0x20`,而不是`0x00`.`0x00`代表空(`NULL`)

`0X00-0XFF` `16`进制一共`256`个,刚好是一个`bit`的范围.

***
回车('\r')效果是输出回到本行行首,结果可能会将这一行之前的输出覆盖掉,例如执行:

```bash
puts("hello world!\rxxx");
#在终端输出的是:
xxxlo world!
```

如果将上面的字符串写入文件中,例如执行:

```bash
char *s = "hello world!\rxxx";
FILE *str = fopen("t.txt","r");
fwrite(s, 16, 1, str);
```

用文本编辑器打开`t.txt`.显示的效果将由打开的编辑器所决定.
vi将`\r`用`^M`代替,而记事本就没有显示该字符.

***
换行('\n')
顾名思义,换行就是转到下一行输出.例如:

```bash
puts("hello\nworld!");
#在终端中将输出
hello
world!
```

但需要注意的是,终端输出要达到换行效果用``\n``就可以,但要在文本文件输出中达到换行效果在各个系统中有所区别.
在`*nix`系统中,每行的结尾是"`\n`",windows中则是"`\n\r`",mac则是"`\r`".

***
水平制表符('\t')

相信大家对'\t'还是比较熟悉的.一般来说,其在终端和文件中的输出显示相当于按下键盘`TAB`键效果.
一般系统中,显示水平制表符将占8列.同时水平制表符开始占据的初始位置是第`8*n`列(第一列的下标为0).例如:

```bash
puts("0123456\txx");
puts("0123456t\txx");
```

***
垂直制表符('\v')

垂直制表符不常用.它的作用是让`'\v'`后面的字符从下一行开始输出,且开始的列数为``\v``前一个字符所在列后面一列.例如:

```bash
puts("01\v2345");
```

***
换页('\f')

换页符的在终端的中的效果相当于`*nix`中`clear`命令.
终端在输出`'\f'`之后内容之前,会将整个终端屏幕清空空,然后在输出内容.给人的该觉是在`clear`命令后的输出字符串.

最后我想说明一点,`\t \r, \v \f`也是控制字符,它们会控制字符的输出方式.
它们在终端输出时会有上面的表现,但如果写入文本文件,一般文本编辑器(vi或记事本)对`\t \r, \v \f`的显示是没有控制效果的.

### shell 换行

把换行符注释掉,如果同时想插入注释,可以用`$()`或者两个`backtick`包裹注释

```bash
emcc -o ./dist/test.html `# 目标文件` \
--shell-file ./tmp.html `# 模板文件` \
--source-map-base dist `# source map 根路径` \
-O3 `# 优化级别` \
```

### 删除换行符

文件中每行都以`\n`结尾,如果要去掉换行符,使用`sed`命令

```bash
[root@host ~]# sed -i 's/\n//g' FileName
```

或者使用`tr`命令: tr - translate or delete characters

```bash
[root@host ~]# cat fileName | tr  -d '\n'
```

有一种简单的方法:

`xargs` - build and execute command lines from standard input

 ```bash
cat FileName | xargs | echo -n   # 连文件末尾换行符也去掉
# 或者
cat FileName | xargs           # 会保留文件末尾的换行符
 ```

### eval

[Shell 中eval的用法](https://blog.csdn.net/luliuliu1234/article/details/80994391)

```bash
eval command-line
```

其中`command-line`是在终端上键入的一条普通命令行.
然而当在它前面放上`eval`时,其结果是`shell`在执行命令行之前扫描它两次.如:

```bash
$ pipe="|"
$ eval ls $pipe wc -l
1
2
3
```

shell第1次扫描命令行时,它替换出`pipe`的值`|`,接着`eval`使它再次扫描命令行,这时shell把`|`作为管道符号了.

如果变量中包含任何需要`shell`直接在命令行中看到的字符,就可以使用eval.
命令行结束符(`;  |  &`),I/o重定向符(`< >`)和引号就属于对shell具有特殊意义的符号,必须直接出现在命令行中.

`eval echo \$$#`取得最后一个参数, 如:

```bash
$ cat last    #此处last是一个脚本文件,内容是下一行显示
$  eval echo \$$#
$ ./last one two three four

four
```

第一遍扫描后,shell把反斜杠去掉了.当shell再次扫描该行时,它替换了`$4`的值,并执行echo命令

***
以下示意如何用`eval`命令创建指向变量的`指针`:

```bash
x=100
ptrx=x
eval echo \$$ptrx  #指向 ptrx,用这里的方法可以理解上面的例子
eval $ptrx=50 #将 50 存到 ptrx 指向的变量中.
echo $x
```

```bash
# ptrx 指向x
echo $ptrx
x
# \$ 转义之后,再跟 x 连成一个字符串
echo \$$ptrx
$x
# eval 执行两次扫描,所以相当于 echo $x
eval echo \$$ptrx
```

### chmod

chmod - change file mode bits

SYNOPSIS

`chmod [OPTION]... MODE[,MODE]... FILE...`
`chmod [OPTION]... OCTAL-MODE FILE...`
`chmod [OPTION]... --reference=RFILE FILE...`

DESCRIPTION

chmod 后面可以接符号表示新的权限,也可以接一个octal number --表示新的mode bits.

符号mode的格式一般是`[ugoa...][[-+=][perms...]...]`,`perms`一般是`0`,或者`rwxXst`中的多个字符,
或者`ugo`中的一个字符.多种符号mode可以给出,用逗号隔开.

`ugoa`表示控制特定用户访问权限:

+ u:the user who owns it
+ g:other users in the file's group
+ o:other users not in the file's group
+ a:all  users
如果没有给出,默认就是 a,but bits that are set in the umask are not affected.

operator `+`添加权限,`-`删除权限,`=`设置为`xxx`,except that a directory's unmentioned set user and group ID bits are not affected.

`rwxXst`表示mode bits,read (r), write (w), execute (or  search  for directories)  (x),
execute/search  only if the file is a directory or already has execute permission for some user (X),
set user or group ID on execution (s), restricted deletion flag or sticky bit (t)

或者指定`ugo`中的一个,
the permissions granted to the user who owns the file (u),
 the permissions granted to other users who are members of the file's group (g),
 and the permissions granted to users that are in neither of the two preceding categories (o).

***
数字模式

数字mode 是1到4个 octal digits(0-7),derived by adding up the bits with values 4, 2, and 1.

省略的数字被认为是前置的`0`.

第一位数字选择用户组
the set user ID (4) and
set group  ID(2)  and
restricted deletion or sticky (1) attributes.

第二位数字选择权限
read (4), write (2), and execute (1);

第三位数字设定组中其他用户的权限

第四位数字设定不在组中用户的权限

## shell脚本

### shell脚本中的符号

[shell脚本中一些特殊符号](https://www.cnblogs.com/xuxm2007/archive/2011/10/20/2218846.html)

### formfactor 脚本

复制结果的脚本

```bash
#!/usr/bin/env python3
import os,shutil,time,gfm
# 复制到论文中的都是 ci==1.50 的结果
user_name='tom'
# 配置计算结果目录,论文目录,论文压缩文件目录
originpath=os.getcwd()
result_path=os.path.join(originpath,'expression-results/')
paper_path=os.path.join('/home',user_name,'private','paper-2.prd/')
desk_path=os.path.join('/home',user_name,'Desktop','paper.ff/')
# 复制计算结果到论文目录
shutil.copy(os.path.join(result_path,'fig.baryons.ge.charge.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig4.pdf'))
shutil.copy(os.path.join(result_path,'fig.baryons.ge.neutral.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig5.pdf'))
shutil.copy(os.path.join(result_path,'fig.baryons.gm.charge.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig2.pdf'))
shutil.copy(os.path.join(result_path,'fig.baryons.gm.neutral.L-0.90.ci-1.50.pdf'),os.path.join(paper_path,'fig3.pdf'))
# cd 到论文目录,重新编译论文
os.chdir(paper_path)
# 清除之前的编译结果,重新编译
os.system('latexmk -C')
os.system('./build.sh')
# 如果桌面有压缩文件目录,就删除,shutil.copytree需要目标不存在
src_list=['fig1.pdf','fig2.pdf','fig3.pdf','fig4.pdf','fig5.pdf','octetFF.tex','octetFF.pdf']
# 把论文目录的东西复制到桌面目录中
if  os.path.isdir(desk_path):
    for src in src_list:
        shutil.copy2(src,desk_path)
else:
    os.mkdir(desk_path)
    for src in src_list:
        shutil.copy2(src,desk_path)

## 切换到桌面整理目录
os.chdir(desk_path)

print("+++++++\nthe file left in",os.getcwd(),"\n+++++++")
os.listdir(desk_path)

# 产生论文压缩文件
os.system('rm ../paper.7z; 7z a ../paper.7z '+desk_path)
# 回到原来的文件夹
os.listdir(originpath)
```

## 环境变量

[Linux环境变量总结](https://www.jianshu.com/p/ac2bc0ad3d74)

`Linux`是一个多用户多任务的操作系统,可以在Linux中为不同的用户设置不同的运行环境,具体做法是设置不同用户的环境变量.

### Linux环境变量分类

***
按照生命周期来分,Linux环境变量可以分为两类:

1. 永久的:需要用户修改相关的配置文件,变量永久生效.
2. 临时的:用户利用`export`命令,在当前终端下声明环境变量,关闭Shell终端失效.

***
按照作用域来分,Linux环境变量可以分为:

1. 系统环境变量:系统环境变量对该系统中所有用户都有效.
2. 用户环境变量:顾名思义,这种类型的环境变量只对特定的用户有效.

### Linux设置环境变量的方法

1. 在`/etc/profile`文件中添加变量 对所有用户生效(永久的)

用`vim`在文件`/etc/profile`文件中增加变量,该变量将会对`Linux`下所有用户有效,并且是`永久的`.
例如:编辑`/etc/profile`文件,添加`CLASSPATH`变量

```bash
vim /etc/profile
export CLASSPATH=./JAVA_HOME/lib;$JAVA_HOME/jre/lib
```

要想马上生效,运行`source /etc/profile`,不然只能在下次重进此用户时生效.

2. 在用户目录下的`~/.bashrc`文件中增加变量 [对单一用户生效(永久的)]

用`vim ~/.bashrc`文件中增加变量,改变量仅会对当前用户有效,并且是`永久的`.

```bash
vim ~/.bashrc
export CLASSPATH=./JAVA_HOME/lib;$JAVA_HOME/jre/lib
```

1. 直接运行`export`命令定义变量 [只对当前shell(BASH)有效(临时的)]

在shell的命令行下直接使用`export 变量名=变量值`

定义变量,该变量只在当前的shell(BASH)或其子shell(BASH)下是有效的,
shell关闭了,变量也就失效了,再打开新shell时就没有这个变量,需要使用的话还需要重新定义.

### Linux环境变量使用

Linux中常见的环境变量有:

***
PATH:指定命令的搜索路径

PATH声明用法:

```bash
export PATH=$PAHT:<PATH 1>:<PATH 2>:<PATH 3>:--------:< PATH n >
```

你可以自己加上指定的路径,中间用冒号隔开.环境变量更改后,在用户下次登陆时生效.

可以利用`echo $PATH`查看当前当前系统PATH路径.

    HOME:指定用户的主工作目录(即用户登陆到Linux系统中时,默认的目录).
    HISTSIZE:指保存历史命令记录的条数.
    LOGNAME:指当前用户的登录名.
    HOSTNAME:指主机的名称,许多应用程序如果要用到主机名的话,通常是从这个环境变量中来取得的
    SHELL:指当前用户用的是哪种Shell.
    LANG/LANGUGE:和语言相关的环境变量,使用多种语言的用户可以修改此环境变量.
    MAIL:指当前用户的邮件存放目录.

注意:上述变量的名字并不固定,如HOSTNAME在某些Linux系统中可能设置成HOST

2. Linux也提供了修改和查看环境变量的命令,下面通过几个实例来说明:

+ `echo`  显示某个环境变量值 `echo $PATH`
+ `export`  设置一个新的环境变量 `export HELLO="hello"` (可以无引号)
+ `env`  显示所有环境变量
+ `set`  显示本地定义的`shell`变量
+ `unset`  清除环境变量 `unset HELLO`
+ `readonly`  设置只读环境变量 `readonly HELLO`

3. C程序调用环境变量函数

+ `getenv()` 返回一个环境变量.
+ `setenv()` 设置一个环境变量.
+ `unsetenv()` 清除一个环境变量.

## Linux Grub2

[grub2详解(翻译和整理官方手册)](https://www.cnblogs.com/f-ck-need-u/archive/2017/06/29/7094693.html#auto_id_37)
[官方手册原文](https://www.gnu.org/software/grub/manual/html_node/Simple-configuration.html#Simple-configuration)

`grub2-mkconfig`是根据`/etc/default/grub`文件来创建配置文件的.该文件中定义的是`grub`的全局宏,修改内置的宏可以快速生成`grub`配置文件.
实际上在`/etc/grub.d/`目录下还有一些`grub`配置脚本,这些`shell`脚本读取一些脚本配置文件(如`/etc/default/grub`),根据指定的逻辑生成`grub`配置文件.
若有兴趣,不放读一读`/etc/grub.d/10_linux`文件,它指导了创建`grub.cfg`的细节,例如如何生成启动菜单.

```bash
[root@xuexi ~]# ls /etc/grub.d/
00_header  00_tuned  01_users  10_linux  20_linux_xen  20_ppc_terminfo  30_os-prober  40_custom  41_custom  README
```

在`/etc/default/grub`中,使用`key=vaule`的格式,`key`全部为大小字母,如果`vaule`部分包含了空格或其他特殊字符,则需要使用引号包围.

例如,下面是一个`/etc/default/grub`文件的示例:

```bash
[root@xuexi ~]# cat /etc/default/grub
GRUB_TIMEOUT=5
GRUB_DISTRIBUTOR="$(sed 's, release .*$,,g' /etc/system-release)"
GRUB_DEFAULT=saved
GRUB_DISABLE_SUBMENU=true
GRUB_TERMINAL_OUTPUT="console"
GRUB_CMDLINE_LINUX="crashkernel=auto biosdevname=0 net.ifnames=0 rhgb quiet"
GRUB_DISABLE_RECOVERY="true"
```

虽然可用的宏较多,但可能用的上的就几个:
`GRUB_DEFAULT`, `GRUB_TIMEOUT`, `GRUB_CMDLINE_LINUX`和`GRUB_CMDLINE_LINUX_DEFAULT`.

以下列出了部分key.

***
`GRUB_DEFAULT`

默认的菜单项,默认值为`0`.其值可为数值`N`,表示从`0`开始计算的第`N`项是默认菜单,也可以指定对应的`title`表示该项为默认的菜单项.
使用数值比较好,因为使用的`title`可能包含了容易改变的设备名.例如有如下菜单项

```grub
menuentry 'Example GNU/Linux distribution' --class gnu-linux --id example-gnu-linux {
    ...
}
```

如果想将此菜单设为默认菜单,则可设置`GRUB_DEFAULT=example-gnu-linux`.

如果`GRUB_DEFAULT`的值设置为`saved`,则表示默认的菜单项是`GRUB_SAVEDEFAULT`或`grub-set-default`所指定的菜单项.

### UEFI 系统下的安装

[GRand Unified Bootloader](https://wiki.archlinux.org/index.php/GRUB_(简体中文))

安装

本节假设您正在 `x86_64` 系统上安装 `GRUB`. 对于 `IA32` (32 位) `UEFI `系统(不要和 32 位 CPU 相混淆),  将`x86_64-efi`替换成`i386-efi`. 
首先安装软件包 `grub` 和 `efibootmgr`. 其中`GRUB`是启动引导器, `efibootmgr`被 `GRUB` 脚本用来将启动项写入 `NVRAM` . 

然后按照下列步骤安装 `GRUB`：

挂载 `EFI` 系统分区, 在本节之后的内容里, 把 `esp` 替换成挂载点. 
选择一个启动引导器标识, 这里叫做 `new`. 这将在 `esp/EFI/` 中创建一个`new`目录来储存 `EFI` 二进制文件, 而且这个名字还会在 `UEFI` 启动菜单中表示 `new` 启动项. 
执行下面的命令来将 `GRUB EFI` 应用 `grubx64.efi` 安装到 `esp/EFI/new/`, 并将其模块安装到 `/boot/grub/x86_64-efi/`. 

```
# grub-install --target=x86_64-efi --efi-directory=esp --bootloader-id=new
```

上述安装完成后 GRUB 的主目录将位于 `/boot/grub/`. 注意上述例子中, grub-install 还将在固件启动管理器中创建一个条目, 名叫 `new`. 

在配置完成后, 记得生成主配置文件. 

提示： 如果你使用了 `--removable` 选项, 那 `GRUB` 将被安装到 `esp/EFI/BOOT/BOOTX64.EFI` (当使用 `i386-efi` 时是 e`sp/EFI/BOOT/BOOTIA32.EFI`), 
此时即使 `EFI` 变量被重设或者你把这个驱动器接到其他电脑上, 你仍可从这个驱动器上启动. 
通常来说, 你只要像操作 `BIOS` 设备一样在启动时选择这个驱动器就可以了. 
如果和 `Windows` 一起多系统启动, 注意 `Windows` 通常会在那里安装一个 `EFI` 可执行程序, 这只是为了重建 `Windows` 的 UEFI 启动项. 

注意：
`--efi-directory` 和 `--bootloader-id` 是 `GRUB UEFI` 特有的. -`-efi-directory` 替代了已经废弃的 `--root-directory`. 
您可能注意到在 `grub-install` 命令中没有一个 `<device_path>` 选项, 例如 `/dev/sda`. 事实上即使提供了 `<device_path>`, 也会被 `GRUB` 安装脚本忽略, 因为 `UEFI` 启动加载器不使用 MBR 启动代码或启动扇区. 
确保 `grub-install` 命令是在你想要用 GRUB 引导的那个系统上运行的. 也就是说如果你是用安装介质启动进入了安装环境中, 你需要在 `chroot` 之后再运行 `grub-install`. 
如果因为某些原因不得不在安装的系统之外运行 `grub-install`, 在后面加上 `--boot-directory= `选项来指定挂载 `/boot` 目录的路径, 例如 `--boot-directory=/mnt/boot`. 

### grub 修复

[Linux与Windows 10用grub引导教程](https://www.cnblogs.com/jpfss/p/9462792.html)

首先, 在你装了 Windows 之后, Windows 在装机过程中会将硬盘划分出一个约 `100MB` 大小的分区, 称为 `EFI` 分区. 
这个分区就是起引导作用的. 在资源管理器中是看不到的这个分区的, 可以在磁盘管理中看到, 管理则需要借助 `DiskGenius` 工具. 
可以看到该分区包含 3 个文件夹(如果你没有装 `Linux` 的话, 就只有两个), 分别是 `Boot`, `Microsoft` 和 `Manjaro`, 其中 `Boot` 文件夹就是 `UEFI` 引导所必需的文件. 

我们继续打开 `Microsoft/Boot` 文件夹, 将会看到许多文件, 这些文件就是启动 `Windows 10` 所必需的, 包含了语言包, 字体等, `BCD` 包含了 `Windows` 引导开始以后的信息. 
其中, `bootmgfw.efi `是 `Windows` 默认引导文件. 

```bash
EFI/Boot/bootx64.efi
EFI/Microsoft/Boot/bootmgfw.efi
```

以上是采用 `UEFI` 启动 `Windows 10` 的文件结构, 也就是说, 当你按下开机按钮的时候, 首先 `UEFI` 找到 `EFI` 分区的 `Boot` 文件夹, 然后加载 `bootx64.efi` 文件, 读取文件信息, 找到 `EFI/Microsoft/Boot/bootmgfw.efi`, 按照 `bootmgfw.efi` 的要求, 加载所需的启动信息, 启动 `Windows 10`. 

***
准备工作

在正式装系统之前, 我们还需要做一些准备工作：

关闭 `Windows` 的快速启动
这个功能的作用是在于关机的时候不完全断电, 类似将系统处于`休眠`状态, 这样可以让开机更加迅速. 但这也就导致了只能使用 `Windows` 系统. 

关闭 `BIOS` 的 `Secure Boot `的功能

在默认情况下, `UEFI` 固件只会加载那些被签名的引导程序. 在缺少 `Secure Boot` 功能的传统 PC 机上, 恶意的后门程序可以加载自身, 进而摇身一变伪装成一个引导程序. 
这样的话, `BIOS` 就会在启动的时候加载后门程序, 这样它就可以躲过操作系统, 把自己隐藏得很深. 
但是不得不说, 这对我们安装 `Linux` 造成了很大的困扰, 也是直接导致我们重启到 Windows 10 后进不去 `Linux` 的原因. 
首先我们要关闭这个功能：进入 `BIOS` 找到 `Secure Boot`, 选择 `disabled`, 这样就关闭了. 当然, 有些人进入 `BIOS` 会发现 Secure Boot 这个选项是灰色的(比如我的就是), 这时你需要先给你的 `BIOS` 设一个密码, 然后就能关 Secure Boot 了. 

***
安装 Linux

所有的准备都已经完成, 这时就可以准备刻录 U 盘了, 推荐`Rufus`和`USBWriter`. 
刻录完成后, 重启按 `f12`, 选择从 USB 设备启动, 对于绝大多数发行版来说一路回车就行了, 只需要注意一点：在选择挂载 `boot` 位置的时候, 一定要挂载在 `efi` 分区, 别的都不行. 
重启之后, 不出意外的话, 你会直接进入 Windows 10, 不要担心, 这时 Linux 已经安装成功了, 我们只需要将引导文件替换一下. 

***
替换引导文件

先用 `DG` 打开 `EFI` 分区, 你会看到多了一个文件夹, 名称取决于你安装的是哪一个发行版. 
我安装的是 `Manjaro Linux`, 名称就是 `Manjaro`, 打开之后会发现里面有一个名为 `grubx64.efi` 的文件, 这就是启动 `Linux` 的引导文件. 
和 `Windows 10` 的 `bootmgfw.efi` 类似, 我们想要用 `grubx64.efi` 引导代替掉 `bootmgfw.efi`, 这样就可以用 `GRUB` 引导了. 步骤：

进入管理员命令行. 方法：`win + x`, 再按 `a`
输入 `bcdedit /set {bootmgr} path \EFI\Manjaro\grubx64.efi`. 提示操作成功的话, 就完成了. 

注：如果输入以上命令提示`参数错误`的话, 将`{bootmgr}`改为 `'{bootmgr}'`, 原因是 `PowerShell` 和 `CMD` 语法的差别. 

至此, 如果你安装的是除 `Arch` 之外绝大多数发行版, 那么接下来就和你没有啥关系了, 你已经成功了, 好好享受吧！

开机之后会发现进入 `GRUB` 的引导了, 通常会包含至少三个选项(以 `Manjaro` 举例)：`Manjaro`, `Manjaro 高级选项`和 `Windows Manager`. 
这就代表你已经完美的解决了 Windows 和 Linux 双系统引导的问题. 

***
修复 `Windows` 引导

这一点是我安装 Arch Llinux 的时候发现的, Arch Linux 安装过程是手动安装的, 在编写 GRUB 的时候会扫描不到 Windows Manager 所在的分区(当然可能不是所有人都会遇到), 
所以在 GRUB 界面可能会看不到 Windows Manager 选项, 导致进不去 Windows 10, 这里就需要手动编辑 GRUB 信息, 
我们打开 `/boot/grub/grub.cfg` 文件, 发现里面确实没有 Windows 10 的启动信息, 在后面加上：

```bash
menuentry "Microsoft Windows 10" {
    insmod part_gpt
    insmod fat
    insmod search_fs_uuid
    insmod chain
    search --fs-uuid --set=root $hints_string $fs_uuid
    chainloader /EFI/Microsoft/Boot/bootmgfw.efi
    }
```

注意：

这里的 `$hints_string`, 代表的是终端执行命令：

```bash
sudo grub-probe --target=hints_string /boot/efi/EFI/Microsoft/Boot/bootmgfw.efi
```

后的输出；

而 `$fs_uuid` 代表的是：

```bash
sudo grub-probe --target=fs_uuid /boot/efi/EFI/Microsoft/Boot/bootmgfw.efi
```

的输出. 然后保存. 在终端执行命令：`sudo grub-mkconfig -o /boot/grub/grub.cfg`, 就 `OK` 了. 

到此, Arch Linux 和 Windows 10 双系统也配置完毕了. 

***
附加问题

+ 在 Windows 10 进行了一个大更新后, 会发现 GRUB 引导界面没有了, 还是直接进入了 Windows 10, 这时只需要按照 替换引导文件 的方法重新输入一遍命令就行. 
+ 使用 Linux 某个发行版一段时间之后, 难免会想尝试一下另一个发行版. 这时请务必将之前的发型版的引导文件删除, 否则可能会出现无论怎么设置都无法进入 GRUB 的情况. 例如：我之前用的是 `Ubuntu`, 我现在换成了 Manjaro, 我就需要用 DG 删除 EFI 分区的 Ubuntu 文件夹. 
+ 在我使用 `Manjaro` 更新了一次 `Linux` 的内核后, 进不去 Windows 10 了, 这个时候千万不要直接修复 `Windows 10` 引导, 这会格式化 `EFI` 分区, 只需要按上面修复 `Windows` 引导 的方法编辑一下 `GRUB` 就可以了. 

***
[Arch Wiki 上的示例](https://wiki.archlinux.org/index.php/GRUB_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%90%AF%E5%8A%A8%E8%8F%9C%E5%8D%95%E6%9D%A1%E7%9B%AE%E7%A4%BA%E4%BE%8B)

这个模式寻找 `Windows` 的启动加载器的位置, 然后当用户选择了相应的菜单条目的时候, 通过链式载入的方法在 `GRUB` 之后加载它. 
这里主要的任务是找到 EFI 系统分区然后从上面运行启动加载器. 注意： 这个启动项仅在 `UEFI` 模式下才起作用, 而且 `Windows` 和 `UEFI` 的位数必须相同. 

```bash
if [ "${grub_platform}" == "efi" ]; then
    menuentry "Microsoft Windows Vista/7/8/8.1 UEFI/GPT" {
    insmod part_gpt
    insmod fat
    insmod chain
    search --no-floppy --fs-uuid --set=root $hints_string $fs_uuid
    chainloader /EFI/Microsoft/Boot/bootmgfw.efi
    }
fi
```

其中 `$hints_string` 和 `$fs_uuid` 由下述两个命令得到. 

`$fs_uuid` 命令检测 EFI 系统分区的 `UUID` ：

```bash
# grub-probe --target=fs_uuid esp/EFI/Microsoft/Boot/bootmgfw.efi
1ce5-7f28
```

或者你可以(以 `root` 身份)运行 `blkid` 然后从结果中找到 `EFI` 系统分区的 `UUID` . 

`$hints_string` 命令可以确定 EFI 系统分区的位置, 在当前的例子中是 `harddrive 0`：

```bash
# grub-probe --target=hints_string esp/EFI/Microsoft/Boot/bootmgfw.efi
--hint-bios=hd0,gpt1 --hint-efi=hd0,gpt1 --hint-baremetal=ahci0,gpt1
```

这两个命令都是假设 `Windows` 使用的 `ESP` 是挂载在`$esp`上的. 当然,  `Windows` 的 `EFI` 文件路径可能有变,因为这就是 `Windows` .... 

### 我使用的 grub 自定义

[Writing your own configuration file](https://www.gnu.org/software/grub/manual/grub/grub.html#search)

+ 编辑 `/etc/default/grub`

```grub
GRUB_DEFAULT=0 #设置默认为第一个
GRUB_TIMEOUT=10 #设置等待时间
GRUB_TIMEOUT_STYLE=menu # 强制显示启动菜单
```

+ 编辑 `/etc/grub.d/40_custom`, 添加 `windows`启动项, 

```bash
if [ "${grub_platform}" == "efi" ]; then
    menuentry "Windows UEFI GPT" {
    insmod part_gpt
    insmod fat
    insmod chain
    search --no-floppy --set=root --fs-uuid "C045-C995"
    chainloader /EFI/Microsoft/Boot/bootmgfw.efi
    }
fi
```

然后运行`sudo os-prober ;sudo update-grub`

### grub 命令行手动引导

链式加载`UEFI`模式下安装的 `Windows/Linux`

```bash
insmod fat
set root=(hd1,gpt1)
chainloader /EFI/Microsoft/Boot/bootmgfw.efi
boot
```

### grub 参数

[nomodeset, quiet and splash](https://askubuntu.com/questions/716957/what-do-the-nomodeset-quiet-and-splash-kernel-parameters-mean)

+ `nomodeset` ; 最新的内核已将`video mode setting`移入内核. 
So all the programming of the hardware specific clock rates and registers on the video card 发生在内核中, 
而不是`X`服务器启动后的`X`驱动中. 如此, 可以在开机时展示高分辨率`splash/boot`画面, 并实现平滑过渡, 不用闪烁一下子. 
然而有些显卡无法正常工作, 并且最终会出现黑屏. 添加`nomodeset`参数指示内核在加载`X`之前不加载显卡驱动程序, 而改用`BIOS`模式. 

+ `quiet splash`; `splash`(由`/boot/grub/grub.cfg`设置)导致显示启动画面. 同时, 您希望启动过程安静一些, 否则所有类型的消息都会覆盖启动屏幕. 
尽管在`GRUB`中指定了这些参数, 但它们是影响内核或其模块加载的内核参数, 而不是改变`GRUB`行为的参数. 来自`GRUB_CMDLINE_LINUX_DEFAULT`的重要部分是`CMDLINE_LINUX`

+ `acpi`, `noapic` and `nolapic`;通常不需要这些参数, 除非你的`BIOS`很古老, 不支持这些标准. 

`ACPI`(Advanced Configuration and Power Interface)是用于处理电源管理的标准. 
较早的系统可能不完全支持`ACPI`, 因此有时它有助于向内核提示不使用它. `acpi=off`

`APIC`(`Advanced Programmable Interrupt Controller`)是在较新的系统上发行的一种功能. 
`"local"`版本称为`LAPIC`. 该控制器可以生成和处理中断, 中断是硬件用来传递消息的信号. 
同样, `APIC`的某些实现在较旧的系统上可能会出现问题, 因此禁用它很有用.  `noapic` ,`nolapic`. 

有时, `APIC`可以正常工作, 但是传递消息可能会减慢速度, 这可能会干扰音频和视频处理. 人们也可能出于这个原因禁用它. 

### ACPI 伪装 windows

[Manjaro折腾记录](https://blog.csdn.net/hustcw98/article/details/81979172)
[how to set acpi_osi parameter in the grub](https://unix.stackexchange.com/questions/246672/how-to-set-acpi-osi-parameter-in-the-grub)

有时从suspend恢复时, 会发现登录后整个桌面一片黑色, 只有一个亮亮的鼠标可以移动. 

```bash
vim /etc/default/grub
## 在 GRUB_CMDLINE_LINUX_DEFAUT= 后面添加下面的语句
acpi_osi=! acpi_osi='Windows 2009'
```

`acpi_osi=!`清空表示`acpi_osi`原来的值, 然后假装成`Windows 7, Win Server 2008 R2`, 
具体的版本对应可以参考：[如何使用 _OSI 识别 ACPI 中的 Windows 版本](https://docs.microsoft.com/zh-cn/windows-hardware/drivers/acpi/winacpi-osi).

对于`Debian`系列的发行版, 可能需要转义引号, 写成`acpi_osi=\"Windows 2009\"`,修改后, 运行`sudo update-grub`重新生成引导配置. 
可以使用`cat /proc/cmdline`检查内核启动的参数, 参考[Linux中proc/cmdline](https://blog.csdn.net/baidu_33879812/article/details/104906774)

***
`Manjaro`在更新内核到`Linux 5.11.14-1`的时候出现问题, 开机提示`NMI watchdog: Watchdog detected hard LOCKUP on cpu x`,删除这个配置之后可以正常进入. 

***
20210423  测试:

+ `GRUB_CMDLINE_LINUX_DEFAULT="apparmor=1 security=apparmor resume=UUID=0f163abf-8e60-4626-a4de-54332c64db51 udev.log_priority=3"`: 不设置这个选项, 可以正常启动.
+ 加上`acpi_osi=! acpi_osi='Windows 2009'` : 可以正常启动
+ 加上`acpi_osi=! acpi_osi='Windows 2015'`  : 可以正常启动

三种参数的开机日志基本没啥区别

## 概念解释

### 查看网络配置

EXAMPLES

+ `ip addr` Shows addresses assigned to all network interfaces.
+ `ip neigh` Shows the current neighbour table in kernel.
+ `ip link set x up` Bring up interface x.
+ `ip link set x down` Bring down interface x.
+ `ip route` Show table routes.

EXAMPLES

+ `ip ro` Show all route entries in the kernel.
+ `ip route add default via 192.168.1.1 dev eth0` Adds a default route (for all addresses) via the local gateway 192.168.1.1 that can be reached on device eth0.
+ `ip route add 10.1.1.0/30 encap mpls 200/300 via 10.1.1.1 dev eth0` Adds an ipv4 route with mpls encapsulation attributes attached to it.
+ `ip -6 route add 2001:db8:1::/64 encap seg6 mode encap segs 2001:db8:42::1,2001:db8:ffff::2 dev eth0` Adds an IPv6 route with SRv6 encapsulation and two segments attached.

***

`Ubuntu 18.04 Server` 安装好后,Netplan 的默认描述文件是:`/etc/netplan/50-cloud-init.yaml`.

[Ubuntu18.04的网络配置 netplan]

[Ubuntu18.04的网络配置 netplan]: https://blog.csdn.net/uaniheng/article/details/104233137?utm_medium=distribute.pc_relevant_t0.none-task-blog-BlogCommendFromMachineLearnPai2-1.nonecase&depth_1-utm_source=distribute.pc_relevant_t0.none-task-blog-BlogCommendFromMachineLearnPai2-1.nonecase

### 配置netplan 固定ip

`vim /etc/netplan/50-cloud-init.yaml `

配置如下:

```bash
network:
    ethernets:
        enp3s0:
            addresses: [192.168.0.20/24]  //IP址
            gateway4: 192.168.0.1  // 网关
            nameservers:
             addresses: [114.114.114.114, 192.168.0.1] //DNS
            dhcp4: no
            optional: no
    version: 2
```

或者配置dhcp自动获取ip

`vim /etc/netplan/50-cloud-init.yaml `

配置如下:

```bash
network:
    ethernets:
        enp3s0:
            dhcp4: true
            optional: yes
    version: 2
```

应用:

`sudo netplan apply`

### 查看MAC地址

+ `ifconfig | awk '/eth/{print $1,$5}'`
+ `arp -a | awk '{print $4}`
+ `sudo lshw -C network`
+ `sudo lshw -c network | grep serial`

```bash
wlp2s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc mq state UP group default qlen 1000
      link/ether 10:5b:ad:df:4c:cd brd ff:ff:ff:ff:ff:ff
    inet 192.168.32.6/24 brd 192.168.32.255 scope global dynamic noprefixroute wlp2s0
       valid_lft 4185sec preferred_lft 4185sec
    inet6 fe80::310a:df04:1f02:d5ac/64 scope link noprefixroute
       valid_lft forever preferred_lft forever
```

`link/ether 10:5b:ad:df:4c:cd brd ff:ff:ff:ff:ff:ff` This is the mac address

### 子网掩码

[为什么每台电脑都要设置子网掩码?](https://www.zhihu.com/question/263438014/answer/278015413)
[24 28 30 位的子网掩码是多少 ](https://zhidao.baidu.com/question/517459209.html)

例如:
IP地址为`130.39.37.100`,
网络地址为`130.39.0.0`,
子网地址为`130.39.37.0`,
子网掩码为`255.255.255.0`,
网络地址部分和子网标识部分对应`1`,host部分对应`0`.
使用CIDR表示为:`130.39.37.100/24`即`IP地址/ 掩码长度`.

`ipv4`是`8bit.8bit.8bit.8bit`的形式,二进制到十六进制是`4`位到`1`位,`8bit`相当于两个`16`进制数字.
所以`ipv4`是四段地址:`0x0x.0x0x.0x0x.0x0x`,每段两个`16`进制数字.
`24`表示掩码开头有`24`个`1`,对应地址中的`3`段,也就是公网地址是`3`段.

`awk`执行按位**与**操作.

```bash
awk 'BEGIN {
awk 'BEGIN {
    num1 = 10
    num2 = 6
    printf "(%d AND %d) = %d\n", num1, num2, and(num1, num2)
}'
```

设

```ip
A:10.1.1.10 /24
B:10.1.1.20 /24
C:50.1.1.80 /24
```

AB在同一局域网,C位于外网.

三个表:

+ `ARP`表:主机维护,存放`IP`地址和`MAC`地址对应关系.
+ `MAC`地址表:交换机维护,存放`MAC`地址和交换机端口对应关系.
+ 路由表:路由器维护,存放`IP`地址和路由器端口对应关系.

首先`AB`通信,
例如`A`要给`B`发送一个数据包,目前`A`知道`B`的`IP`地址,根据掩码规则判定`B`和自己在同一个局域网,同一个广播域.

接下来`A`通过广播方式获取`B`的`MAC`地址,添加到自己的`ARP`表中.
然后把要发送的包封装,然后发送给交换机,交换机收到数据包后解封装得到`B`的`MAC`地址,
根据`MAC`地址表转发到`B`所连接的交换机端口,完成发送.

如果`A`要和`C`通信,发送一个包给`C`的话,也只知道`C`的`IP`地址,然后`A`根据掩码规则发现`C`和自己不是同一个局域网的,
广播不到`C`,所以`A`只能把数据包发给网关,由网关发出去给到`C`.

`A`同样通过广播方式获取网关的`MAC`地址,然后把`C`的`IP`地址和网关的`MAC`地址封装到数据包后发给交换机,
交换机解封装后对比`MAC`地址表,发现是发给网关的包,就转发到网关即路由器所在的交换机端口.
路由器收到包之后再解封装,得到`C`的`IP`地址,然后根据自己的路由表转发到相应的端口.完成通信.

所以如果计算机上不设置子网掩码,从第一步就不能完成,下面就更不能继续了.
如果同一个广播域里有机器设置不同的子网掩码,依然能够通信,只不过有的内网包需要到网关绕一圈.

外网包的话只要网关设置对了就没问题.

### 查看登录用户

[linux查看当前登录用户](https://blog.csdn.net/wanchaopeng/article/details/88425067)

+ `w`命令:显示目前登入系统的用户信息. 选项:

+ `-f`: 开启或关闭显示用户从何处登入系统.
+ `-h`: 不显示各栏位的标题信息列.
+ `-s`: 使用简洁格式列表,不显示用户登入时间,终端机阶段作业和程序所耗费的CPU时间.
+ `-u`: 忽略执行程序的名称,以及该程序耗费CPU时间的信息.
+ `-V`: 显示版本信息.

输出的结果的含义:

+ `USER` 登录的用户名
+ `TTY` 登录终端
+ `FROM` 从哪个IP地址登录
+ `LOGIN`@ 登录时间
+ `IDLE` 用户闲置时间
+ `JCPU` 指的是和该终端连接的所有进程占用的时间,这个时间里并不包括过去的后台作业时间,但却包括当前正在运行的后台作业所占用的时间
+ `PCPU` 当前进程所占用的时间
+ `WHAT` 当前正在运行的命令

+ `who`; 显示当前已登录的用户信息,输出的结果有:用户名,登录终端,登录的时间
+ `last`; 列出目前与过去登入系统的用户相关信息.

+ `-R`: 省略 hostname 的栏位
+ `-n`:指定输出记录的条数.
+ `-f file`:指定用文件`file`作为查询用的`log`文件.
+ `-t time`:显示到指定的时间.
+ `-h `:显示帮助.
+ `-i` or`--ip`:以`数字`and `点`的形式显示`ip`地址.
+ `-x`:显示系统关闭, 用户登录和退出的历史.

命令的输出包含:用户名,登录终端,登录IP,登录时间,退出时间(在线时间)

+ `lastlog`;检查某特定用户上次登录的时间. 选项:

+ `-b`, `--before DAYS`: 仅打印早于 DAYS 的最近登录记录
+ `-h`, `--help`: 显示此帮助信息并推出
+ `-R`, `--root CHROOT_DIR` directory to chroot into
+ `-t`, `--time DAYS` : 仅打印比 DAYS 新近的登录记录
+ `-u`, `--user LOGIN` : 打印 LOGIN 用户的最近登录记录

注意:`lastlog`命令默认读取的是`/var/log/wtmp`这个文件的数据,一定注意这个文件不能用`vi`来查看.
命令输出包括:用户名,登录终端,登录`IP`,最后一次登录时.

### Linux目录含义

[Linux各目录含义](https://www.jianshu.com/p/142deb98ed5a)

+ FHS标准; `linux`系统的目录都遵循一个标准, 即由`Linux`基金会发布的 文件系统层次结构标准 (`Filesystem Hierarchy Standard`, FHS).
这个标准里面定义了linux系统该有哪些目录,各个目录应该存放什么,起什么作用等等:

目录  含义

+ `/bin`  `binary`,即用来存放二进制可执行文件,并且比较特殊的是`/bin`里存放的是所有一般用户都能使用的可执行文件,如:`cat`, `chmod`, `chown`, `mv`, `mkdir`, `cd` 等常用指令
+ `/boot`  存放开机时用到的引导文件
+ `/dev`  device(并不是`develop`哦),任何设备都以文件的形式存放在这个目录中
+ `/etc`  `Editable Text Configuration`(早期含义为`etcetera`,但是有争议),存放系统配置文件,如各种服务的启动配置,账号密码等
+ `/home`  用户的主目录,每当新建一个用户系统都会在这个目录下创建以该用户名为名称的目录作为该用户的主目录.并且在命令行中~代表当前用户的主目录,~yousiku表示yousiku这个用户的主目录
+ `/lib`  library,存放着系统开机时所需的函数库以及/bin和/sbin目录下的命令会调用的函数库
+ `/lib64`  存放相对于/lib中支持64位格式的函数库
+ `/media`  可移除的媒体设备,如光盘,DVD等
+ `/mnt`  `mount`,临时挂载的设备文件
+ `/opt`  `optional`,可选的软件包,即第三方软件.我们可以将除了系统自带软件之外的其他软件安装到这个目录下
+ `/proc`  `process`,该目录是一个虚拟文件系统,即该目录的内容存放于内存中而不是硬盘中,存放着系统内核以及进程的运行状态信息
+ `/root`  超级管理员root的主目录
+ `/run`  最近一次开机后所产生的各项信息,如当前的用户和正在运行中的守护进程等
+ `/sbin`  存放一些只有root账户才有权限执行的可执行文件,如init, ip, mount等命令
+ `/srv`  service,存放一些服务启动后所需的数据
+ `/sys`  system,与/proc类似也是一个虚拟文件系统,存放系统核心与硬件相关的信息
+ `/tmp`  temporary,存放临时文件,可以被所有用户访问,系统重启时会清空该目录
+ `/usr`  Unix Software Resource(并不是指user哦),存放着所有用户的绝大多数工具和应用程序(下文详细介绍)
+ `/var`  variable,存放动态文件,如系统日志,程序缓存等(下文详细介绍)

+ `/usr`目录; `Unix Software Resource` 意为 `Unix`系统软件资源.
系统自带的软件都装在这个目录下(好比Windows系统的`C:\Windows`),用户安装的第三方软件也在这个目录下(好比Windows系统的`C:\Program Files`).
不同的是, 在Windows系统上安装软件通常将该软件的所有文件放置在同一个目录下,但在Linux系统, 安装软件会将该软件的不同文件分别放置在`/usr`目录下的不同子目录下.
而不应该自行创建该软件自己的独立目录. `/usr`目录一般有以下子目录:

目录  含义

+ `/usr/bin`  即`/bin`,用链接文件到方式将`/bin`链接至此
+ `/usr/etc`  应用程序的配置文件
+ `/usr/games`  与游戏相关的数据
+ `/usr/include`  `c/c++`程序的头文件
+ `/usr/lib`  即`/lib`,用链接文件到方式将`/lib`链接至此
+ `/usr/lib64`  即`/lib64`,用链接文件到方式将`/lib64`链接至此
+ `/usr/libexec`  不常用的执行文件或脚本
+ `/usr/local`  应用程序的安装目录,每个应用程序目录下还会有对应的`bin`, `etc`, `lib`等目录
+ `/usr/sbin`  即`/sbin`,用链接文件到方式将`/sbin`链接至此
+ `/usr/share`  共享文件,通常是一些文字说明文件,如软件文档等
+ `/usr/src`  `source`,应用程序源代码
+ `/usr/tmp`  应用程序临时文件

### 输入法

添加删除输入法在系统设置目录, 直接搜索`settings-Region&Language--input sources`, 添加输入法是按照语言进行的, 先选择语言, 然后可以选择具体的输入法. 
如`Intelligent Pinyin`

+ 切换输入法可以使用如下命令：`im-config -s fcitx`, 
`-s`:    无动作； 对可能发生的事件进行模拟,但实际上不更改配置文件. 

+ 如果要查看当前可用的输入法可以使用 `im-config -l` , 更多查看 `man im-config`.
+ `ibus-setup`: 图形界面程序, 用于设置`ibus`输入法框架

+ 查看环境变量`$XDG_CURRENT_DESKTOP`的值来看自己处于哪个图形环境.
+ 重启输入法; `ibus restart`: 
+ 重启 ibus 守护进程； `ibus-daemon -drx`
    + `-d --daemonize`:作为后台程序运行
    + `-r, --replace`: 如果有旧的`ibus-daemon`在运行, 就替换它. 
    + `-x, --xim`: 运行`XIM`服务器

+ gedit设置默认编码UTF-8;
[gedit默认编码设置](https://blog.csdn.net/miscclp/article/details/39154639).
在终端下输入:

```bash
gsettings set org.gnome.gedit.preferences.encodings candidate-encodings "['UTF-8', 'GB18030', 'GB2312', 'GBK', 'BIG5', 'CURRENT', 'UTF-16']"
```

+ 导入ibus词库
[iBus拼音输入法导入搜狗词库](https://blog.csdn.net/betabin/article/details/7798668)

终端下输入`ibus-setup`--`Input Method`--`Chinese - intelligent pinyin`, 
点击右侧的 `preference`--`user data`--`import`, 把制作好的词库导入进去即可. 测试:

    亥姆霍兹方程
    重整化群

### 定制自己的libpinyin

[ibus下定制自己的libpinyin](https://blog.csdn.net/godbreak/article/details/9031887)

智能拼音输入法从`ibus-pinyin`更名为`ibus-libpinyin`

`libpinyin`添加了词库导入功能,并刚刚修复相关`bug`,所以要先更新`libpinyin`到最新版.
在`libpinyin`的配置界面(可以从`语言选项`---`输入源`找到,实在找不到,`/usr/share/ibus-libpinyin/setup/main2.py`),可以找到**用户数据导入选项**.

这个要求文件:

1. 文件采用本地编码格式
2. 格式为每行`字符 拼音 位置(可选)`,且字符数和拼音数要对应,例如`你好 ni'hao 5`.

去搜狗词库下搜狗细胞词库文件,然后下个**深蓝词库转换器**(`exe`),`wine`中打开转换器,选择从搜狗细胞词库转换到手机`QQ`格式,转换结束后不要选择文件保存本地,编码格式不大对,在输出框里面全选复制粘贴到你的文本编辑器,保存为`.txt`后缀.
然后在`libpinyin`配置界面导入即可.导入完成后,`kill ibus-engine-libpinyin`进程,再切回拼音输入法.

## 日常维护

### 开机报错

[System program problem detected?](https://askubuntu.com/questions/1160113/system-program-problem-detected)

查看转储到您的磁盘上的崩溃报告. 目录是`/var/crash/`, 它将包含几个文件, 这些文件将您指向它所涉及的软件包以及崩溃的原因. 
该目录描述为：

>`/var/crash`：系统崩溃转储(可选)
>该目录包含系统故障转储. 
>自本标准发布之日起, Linux不支持系统故障转储, 但其他可能符合FHS的系统也可能支持系统转储. 

`Ubuntu`版本使用此(可选)目录来转储崩溃和执行崩溃的软件包, 称为`apport` (and `whoopsie`). 
如果您想获得关于崩溃的真正详细的报告, 请安装`GDB`：`The GNU Project Debugger` with `sudo apt-get install gdb`. 

+ 如何摆脱它

取决于您所说的`摆脱`. 理想的解决方法是检查报告中包含的内容, 然后尝试找到解决方法. 
如果不需要包装或良性包装, 也可以将其清除. 多数情况下, 它是一项核心功能. 

您可以选择以下任意一种来删除崩溃报告, 直到实际删除该软件包为止(如果错误来自于`apport`本身, 那将非常具有讽刺意味)：

+ `sudo rm /var/crash/*`将删除旧的崩溃并停止通知您, 直到某些软件包再次崩溃为止. 
+ 您可以通过`sudo systemctl disable apport`停止服务(并通过`sudo systemctl enable apport`再次启用它)
+ 如果不想看到崩溃报告, 可以通过`vim /etc/default/apport`将其禁用. 并将`enabled = 1`更改为` enabled = 0`. 反向编辑将再次启用它. 
+ 您可以使用`sudo apt purge apport`(使用`sudo apt install apport`再次安装)
+还有一种桌面方法(`问题报告`选项)：

[如何阅读和使用崩溃报告](https://askubuntu.com/questions/346953/how-to-read-and-use-crash-reports)有一些有趣的答案. 
它有一个示例崩溃报告和一种跟踪崩溃的方法. 
