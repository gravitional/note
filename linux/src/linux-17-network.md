# Linux 第十七章:网络系统

几乎网络系统层面的任何东西都能由 Linux 来实现.
`Linux` 被用来创建各式各样的网络系统和装置, 包括防火墙,路由器,名称服务器,网络连接式存储设备等等.

被用来配置和操作网络系统的命令数目,就如网络系统一样巨大.我们仅仅关注一些最经常使用到的命令.
我们要研究的命令包括监测网络和传输文件的命令.另外,我们还会探讨用来远端登录的 `ssh` 程序.

这章会介绍:

+ `ping` - 发送 `ICMP ECHO_REQUEST` 软件包到网络主机
+ `traceroute` - 打印到一台网络主机的路由数据包
+ `netstat` - 打印网络连接,路由表,接口统计数据,伪装连接,和多路广播成员
+ `ftp` - 因特网文件传输程序
+ `wget` - 非交互式网络下载器
+ `ssh` - OpenSSH SSH 客户端(远程登录程序)

`pint ifconfig netstat traceroute` 等在软件包 `gnome-nettool` 里面

假定你已经知道了一些网络系统背景知识.在因特网时代,每个计算机用户都需要理解基本的网络系统概念.
为了能够充分利用这一章节的内容,我们应该熟悉以下术语:

+ `IP` (网络协议)地址
+ `主机`和`域名`
+ `URI`(统一资源标识符)

请查看下面的`拓展阅读`部分,有几篇关于这些术语的有用文章.

注意:一些将要讲到的命令可能(取决于系统发行版)需要从系统发行版的仓库中安装额外的软件包, 并且一些
命令可能需要超级用户权限才能执行.

## 检查和监测网络

即使你不是一名系统管理员,检查一个网络的性能和运作情况也很有帮助, 最基本的网络命令是 `ping`:

```bash
ping
```

这个 `ping` 命令发送一个特殊的网络数据包,叫做`IMCP ECHO_REQUEST`,到 一台指定的主机.
大多数接收这个包的网络设备将会回复它,来允许网络连接验证.

注意:大多数网络设备(包括 Linux 主机)都可以被配置为忽略这些数据包.
通常,这样做是出于网络安全原因,部分地遮蔽一台主机免受一个潜在攻击者地侵袭.配置防火墙来阻塞 `IMCP` 流量也很普遍.

例如,看看我们能否连接到网站 `linuxcommand.org`(我们最喜欢的网站之一), 我们可以这样使用 `ping` 命令:

```bash
$ ping linuxcommand.org
```

一旦启动,`ping` 命令会按特定间隔(默认是一秒)持续发送数据包,直到它被中断:

```bash
$ ping linuxcommand.org
PING linuxcommand.org (66.35.250.210) 56(84) bytes of data.
...
```

按下组合键 `Ctrl-c`,中断这个命令之后,`ping` 打印出运行统计信息.一个正常工作的网络会报告零个数据包丢失.
一个成功执行的`ping`命令会意味着网络的各个部件(网卡,电缆,路由,网关) 都处于正常的工作状态.

***
一些概念介绍:

`Maximum Transmission Unit`,缩写`MTU`,中文名是: 最大传输单元.它是哪一层网络的概念?

从下面这个表格中可以看到,在`7`层网络协议中,`MTU`是数据链路层的概念.`MTU`限制的是数据链路层的`payload`,也就是上层协议的大小,例如`IP`,`ICMP`等.

***
`OSI`中的层  功能  `TCP/IP`协议族

+ 应用层  文件传输,电子邮件,文件服务,虚拟终端  `TFTP`,`HTTP`,`SNMP`,`FTP`,`SMTP`,`DNS`,`Telnet`
+ 表示层  `数据格式化`,`代码转换`,`数据加密`  `没有协议`
+ 会话层  解除或建立与别的接点的联系  `没有协议`
+ 传输层  提供端对端的接口  `TCP`,`UDP`
+ 网络层  为数据包选择路由  `IP`,`ICMP`,`RIP`,`OSPF`,`BGP`,`IGMP`
+ 数据链路层  传输有地址的帧以及错误检测功能  `SLIP`,`CSLIP`,`PPP`,`ARP`,`RARP`,`MTU`
+ 物理层  以二进制数据形式在物理媒体上传输数据  `ISO2110`,`IEEE802`,`IEEE802.2`

***
`tracepath`, `tracepath6` - traces path to a network host discovering MTU along this path

`traceroute` 程序(`ubuntu` 使用相似的 `tracepath` 程序来代替)会显示从本地到指定主机 要经过的所有`跳数`的网络流量列表.

例如,看一下到达 `soso.com` 网站,需要经过的路由 器,我们将这样做:

```
$ traceroute soso.com
traceroute to soso.com (216.34.181.45), 30 hops max, 40 bytepackets
1 ipcop.localdomain (192.168.1.1) 1.066 ms 1.366 ms 1.720 ms
2 * * *
...
...
```

从输出结果中,我们可以看到连接测试系统到 `soso.com` 网站需要经由`30`个路由器.
对于那些 提供标识信息的路由器,我们能看到它们的主机名,`IP` 地址和性能数据,这些数据包括三次从本地到 此路由器的往返时间样
本.

对于那些没有提供标识信息的路由器(由于路由器配置,网络拥塞,防火墙等 方面的原因),我们会看到几个星号,正如行中所示.

[什么是MTU? 为什么MTU值普遍都是1500? ](https://developer.aliyun.com/article/222535)

## netstat

`netstat` 程序被用来检查各种各样的网络设置和统计数据.通过此命令的许多选项,我们 可以看看网络设置中的各种特性.
使用`-ie`选项,我们能够查看系统中的网络接口:

```bash
$ netstat -ie
enp0s31f6: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 192.168.218.191  netmask 255.255.255.0  broadcast 192.168.218.255
        inet6 xxxx:xxxx:1:2218:8eec:4bff:fe91:d65b  prefixlen 64  scopeid 0x0<global>
        ...
lo: flags=73<UP,LOOPBACK,RUNNING>  mtu 65536
    inet 127.0.0.1  netmask 255.0.0.0
    inet6 ::1  prefixlen 128  scopeid 0x10<host>
    ...
```

在上述实例中,我们看到我们的测试系统有两个网络接口.
第一个,叫做`enp0s31f6`,是 因特网接口,和第二个,叫做 `lo`,是内部回环网络接口,它是一个虚拟接口,系统用它来 `自言自语`.

当执行日常网络诊断时,要查看的重要信息是每个网络接口第一行开头出现的单词 `UP`,
说明这个网络接口已经生效,还要查看第二行中 `inet` 字段出现的有效 `IP` 地址.

对于使用 `DHCP`(动态主机配置协议)的系统,在这个字段中的一个有效 `IP` 地址则证明了 `DHCP` 工作正常.
使用这个`-r`选项会显示内核的网络路由表.这展示了系统是如何配置网络之间发送数据包的.

```bash
$ netstat -r
Kernel IP routing table
Destination     Gateway         Genmask             Flags   MSS     Window          irtt Iface
192.168.1.1         *             255.255.255.0         U        0         0          0 enp0s31f6
default             192.168.1.1         0.0.0.0                 UG          0       0 enp0s31f6
...
```

在这个简单的例子里面,我们看到了,位于防火墙之内的局域网中,一台客户端计算机的典型路由表.

第一行显示了目的地 `192.168.1.0`.IP 地址以零结尾是指网络,而不是个人主机, 所以这个目的地意味着局域网中的任何一台主机.
下一个字段,`Gateway`, 是网关(路由器)的名字或 `IP` 地址,用它来连接当前的主机和目的地的网络.
若这个字段显示一个星号,则表明不需要网关.

最后一行包含目的地 `default`.指的是发往任何表上没有列出的目的地网络的流量.
在我们的实例中,我们看到网关被定义为地址 `192.168.1.1` 的路由器,它应该能 知道怎样来处理目的地流量.
`netstat` 程序有许多选项,我们仅仅讨论了几个.查看 `netstat` 命令的手册,可以 得到所有选项的完整列表.

## 网络中传输文件

网络有什么用处呢?除非我们知道了怎样通过网络来传输文件.
有许多程序可以用来在网络中传送数据.我们先讨论两个命令,随后的章节里再介绍几个命令.

### ftp

`ftp` 命令属于真正的`经典`程序之一,它的名字来源于其所使用的协议,就是文件传输协议.

`FTP` 被广泛地用来从因特网上下载文件.
大多数,并不是所有的,网络浏览器都支持 `FTP`, 你经常可以看到它们的 URI 以协议`ftp://`开头.

在出现网络浏览器之前,`ftp` 程序已经存在了.
 `ftp` 程序可用来与 `FTP` 服务器进行通信,`FTP` 服务器就是存储文件的计算机,这些文件能够通过 网络下载和上传.

`FTP`(它的原始形式)并不是安全的,因为它会以明码形式发送帐号的姓名和密码.
这就意味着 这些数据没有加密,任何嗅探网络的人都能看到.
由于此种原因,几乎因特网中所有 `FTP` 服务器 都是匿名的.
一个匿名服务器能允许任何人使用注册名`anonymous`和无意义的密码登录系统.

在下面的例子中,我们将展示一个典型的会话,
从匿名 `FTP` 服务器-- `fileserver`的`/pub/_images/Ubuntu-8.04`的目录下,
使用 ftp 程序下载一个 Ubuntu 系统映像文件.

```bash
$ ftp fileserver
Connected to fileserver.localdomain.
...
Password:
...
ftp> cd pub/cd\_images/Ubuntu-8.04
250 Directory successfully changed.
ftp> ls
200 PORT command successful. Consider using PASV.
...
ftp> lcd Desktop
Local directory now /home/me/Desktop
ftp> get ubuntu-8.04-desktop-i386.iso
local: ubuntu-8.04-desktop-i386.iso remote: ubuntu-8.04-desktop-
i386.iso
...
ftp> bye
```

这里是对会话期间所输入命令的解释说明:

***
表17-1:
命令 意思

+ `ftp fileserver` 唤醒 `ftp` 程序,让它连接到 FTP 服务器,`fileserver` .
+ `anonymous` 登录名.输入登录名后,将出现一个密码提示.一些服务器将会接受空密码,其它一些则会要求一个邮件地址形式的密码.如果是这种情况,试着输入`user@example.com`.
+ `cd pub/cd_images/Ubuntu-8.04` 跳转到远端系统中,要下载文件所在的目录下, 注意在大多数匿名的 FTP 服务器中,支持公共下载的文件都能在目录 `pub` 下找到
+ `ls` 列出远端系统中的目录.
+ `lcd Desktop` 跳转到本地系统中的 `~/Desktop` 目录下.在实例中,`ftp` 程序在工作目录 `~`下被唤醒. 这个命令把工作目录改为`~/Desktop`
+ `get ubuntu-8.04-desktop-i386.iso` 告诉远端系统传送文件到本地.因为本地系统的工作目录 已经更改到了`~/Desktop`,所以文件会被下载到此目录.
+ `bye` 退出远端服务器,结束 ftp 程序会话.也可以使用命令 `quit` 和 `exit` .

在 `ftp>` 提示符下,输入 `help`,会显示所支持命令的列表.
使用 `ftp` 登录到一台 授予了用户足够权限的服务器中,则可以执行很多普通的文件管理任务.虽然很笨拙, 但它真能工作.

### lftp - 更好的 ftp

ftp 并不是唯一的命令行形式的 FTP 客户端.
实际上,还有很多.其中比较好(也更流行的)是 `lftp` 程序, 由Alexander Lukyanov 编写完成.

虽然 `lftp` 工作起来与传统的 ftp 程序很相似,但是它带有额外的便捷特性,
包括 多协议支持(包括 HTTP),若下载失败会自动地重新下载,后台处理,用 tab 按键来补全路径名,还有很多.

### wget

另一个流行的用来下载文件的命令行程序是 `wget`.

若想从网络和 FTP 网站两者上都能下载数据,`wget` 是很有用处的.
不只能下载单个文件,多个文件,甚至整个网站都能下载.
下载 linuxcommand.org 网站的首页, 我们可以这样做:

```bash
$ wget http://linuxcommand.org/index.php
--11:02:51-- http://linuxcommand.org/index.php
=> `index.php'
Resolving linuxcommand.org... 66.35.250.210
...
11:02:51 (161.75 MB/s) - 'index.php' saved [3120]
```

这个程序的许多选项允许 `wget` 递归地下载,在后台下载文件(你退出后仍在下载),能完成未下载全的文件.
这些特性在命令手册,`better-than-average` 一节中有详尽地说明.

+ `-r` `--recursive`: Turn on recursive retrieving.    The default maximum depth is `5`.
+ `-b` `--background`:  Go to background immediately after startup.
If no output file is specified via the `-o`, output is redirected to wget-log.
+ `-c` `--continue` Continue getting a partially-downloaded file.
This is useful when you want to finish up a download started by a previous instance of Wget,
or by another program.  For instance: `wget -c ftp://sunsite.doc.ic.ac.uk/ls-lR.Z`
