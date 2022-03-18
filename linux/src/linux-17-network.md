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

### 检查和监测网络

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

### netstat

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

### 网络中传输文件

网络有什么用处呢?除非我们知道了怎样通过网络来传输文件.
有许多程序可以用来在网络中传送数据.我们先讨论两个命令,随后的章节里再介绍几个命令.

#### ftp

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

#### wget

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
This is useful when you want to finish up a download started by a previous instance of Wget, or by another program.  For instance: `wget -c ftp://sunsite.doc.ic.ac.uk/ls-lR.Z`

### 与远程主机安全通信

通过网络来远程操控类 Unix 的操作系统已经有很多年了.

早些年,在因特网普遍推广之前,有 一些受欢迎的程序被用来登录远程主机.它们是 rlogin 和 telnet 程序.
然而这些程序,拥有和 `ftp` 程序 一样的致命缺点;它们以明码形式来传输所有的交流信息(包括登录命令和密码).
这使它们完全不 适合使用在因特网时代.

### ssh协议

为了解决这个问题,开发了一款新的协议,叫做 SSH(Secure Shell). SSH 解决了这两个基本的和远端主机安全交流的问题.

首先,它要认证远端主机是否为它 所知道的那台主机(这样就阻止了所谓的`中间人`的攻击),
其次,它加密了本地与远程主机之间 所有的通讯信息.

`SSH` 由两部分组成.

`SSH `**服务器**运行在远端主机上运行,在端口号`22`上监听将要到来的连接,而 `SSH` **客户端**用在本地系统中,用来和远端服务器通信.

大多数 Linux 发行版自带一个提供 SSH 功能的软件包,叫做 `OpenSSH`,来自于 `BSD` 项目.
一些发行版 默认包含客户端和服务器端两个软件包(例如,`Red Hat`),而另一些(比方说 `Ubuntu`)则只是提供客户端服务.
为了能让系统接受远端的连接,它必须安装 `OpenSSH-server` 软件包,配置运行它, 并且须允许它在 `TCP` 端口号上接收网络链接(如果系统运在防火墙之后) .

小贴示:如果你没有远端系统去连接,但还想试试这些实例,则确认安装了 `OpenSSH-server` 软件包 ,则可使用 `localhost` 作为远端主机的名字.这种情况下,计算机会和它自己创建网络连接.

用来与远端 `SSH` 服务器相连接的 `SSH` 客户端程序,顺理成章,叫做 `ssh`. 连接到远端名为 `remote-sys` 的主机,我们可以这样使用 `ssh` 客户端程序:

```bash
$ ssh remote-sys
The authenticity of host 'remote-sys (192.168.1.4)' can't be
established.
RSA key fingerprint is
41:ed:7a:df:23:19:bf:3c:a5:17:bc:61:b3:7f:d9:bb.
Are you sure you want to continue connecting (yes/no)?
```

第一次尝试连接,提示信息表明远端主机的真实性不能确立.这是因为客户端程序以前从没有看到过这个远端主机.
为了接受远端主机的身份验证凭据,输入`yes`.一旦建立了连接,会提示 用户输入他或她的密码:

```bash
Warning: Permanently added 'remote-sys,192.168.1.4' (RSA) to the list
of known hosts.
me@remote-sys's password:
```

成功地输入密码之后,我们会接收到远端系统的 `shell` 提示符:

```bash
Last login: Sat Aug 30 13:00:48 2008
[me@remote-sys ~]$
```

远端 `shell` 会话一直存在,直到用户输入 `exit` 命令后,则关闭了远程连接.这时候,本地的 shell 会话 恢复,本地 shell 提示符重新出现.

也有可能使用不同的用户名连接到远程系统.
例如,如果本地用户`me`,在远端系统中有一个帐号名`bob`,则用户 `me` 能够用 `bob` 帐号登录到远端系统,如下所示:

```bash
$ ssh bob@remote-sys
bob@remote-sys's password:
Last login: Sat Aug 30 13:03:21 2008
[bob@remote-sys ~]$
```

正如之前所讲到的,`ssh` 验证远端主机的真实性. 如果远端主机不能成功地通过验证,则会提示以下信息:

```bash
$ ssh remote-sys
@@@@@@@@@@@@@@@@@@@@@...
WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!
@
@@@@@@@@@@@@@@@@@...
IT IS POSSIBLE THAT SOMEONE IS DOING SOMETHING NASTY!
Someone could be eavesdropping on you right now (man-in-the-middle
attack)!
...
```

有两种可能的情形会提示这些信息.第一,某个攻击者企图制造`中间人`袭击.
这很少见, 因为每个人都知道ssh 会针对这种状况发出警告.

最有可能的罪魁祸首是远端系统已经改变了; 例如,它的操作系统或者是 SSH服务器重新安装了.
然而,为了安全起见,第一个可能性不应该 被轻易否定.当这条消息出现时,总要与远端系统的管理员查对一下.

当确定了这条消息归结为一个良性的原因之后,那么在客户端更正问题就很安全了.
使用文本编辑器(可能是vim)从文件`~/.ssh/known_hosts` 中删除废弃的钥匙, 就解决了问题.
在上面的例子里,我们看到这样一句话:

```bash
Offending key in /home/me/.ssh/known_hosts:1
```

这意味着文件 `known_hosts` 里面某一行包含攻击型的钥匙.
从文件中删除这一行,则 ssh 程序 就能够从远端系统接受新的身份验证凭据.

除了能够在远端系统中打开一个 shell 会话,ssh 程序也允许我们在远端系统中执行单个命令.
例如,在名为`remote-sys` 的远端主机上,执行 `free` 命令,并把输出结果显示到本地系统 shell 会话中.

```bash
$ ssh remote-sys free
me@twin4's password:
...
$
```

有可能以更有趣的方式来利用这项技术,比方说下面的例子,我们在远端系统中执行 `ls` 命令, 并把命令输出重
定向到本地系统中的一个文件里面.

```bash
$ ssh remote-sys 'ls *' > dirlist.txt
me@twin4's password:
$
```

注意,上面的例子中使用了单引号.这样做是因为我们不想路径名展开操作在本地执行 ;而希望 它在远端系统中被执行.
同样地,如果我们想要把输出结果重定向到远端主机的文件中,我们可以 把重定向操作符和文件名都放到单引号里面.

```bash
$ ssh remote-sys 'ls * > dirlist.txt'
```

### SSH 通道

当你通过 SSH 协议与远端主机建立连接的时候,其中发生的事就是在本地与远端系统之间 创建了一条加密通道.
通常,这条通道被用来把在本地系统中输入的命令安全地传输到远端系统, 同样地,再把执行结果安全地发送回来.
除了这个基本功能之外,SSH 协议允许大多数网络流量类型通过这条加密通道来被传送,在本地与远端系统之间创建某种 VPN(虚拟专用网络).

可能这个特性的最普遍使用是允许传递 `X` 窗口系统流量.

在运行着 `X` 服务器(也就是, 能显示 GUI 的机器)的系统中,有可能在远端启动和运行一个 `X` 客户端程序(一个图形化应用程序), 而应用程序的显示结果出现在本地.

这很容易完成,这里有个例子:
假设我们正坐在一台装有 Linux 系统, 叫做 `linuxbox` 的机器之前,且系统中运行着 `X` 服务器,
现在我们想要在名为 `remote-sys` 的远端系统中运行 `xload` 程序,但是要在我们的本地系统中看到这个程序的图形化输出.
我们可以这样做:

```bash
$ ssh -X remote-sys
me@remote-sys's password:
Last login: Mon Sep 08 13:23:11 2008
[me@remote-sys ~]$ xload
```

这个 `xload` 命令在远端执行之后,它的窗口就会出现在本地.
在某些系统中,你可能需要 使用 `-Y` 选项,而不是 `-X` 选项来完成这个操作.

### scp 和  sftp

这个 `OpenSSH` 软件包也包含两个程序,它们可以利用 `SSH` 加密通道在网络间复制文件. 第一个:`scp`(安全复制)被用来复制文件,与熟悉的 `cp` 程序非常相似.
最显著的区别就是源或者目标路径名要以远端主机的名字`remote-sys`开头,后跟一个**冒号**字符开头. 这里的`remote-sys`可以写在 `ssh` 的配置文件中,后面会讲如何配置 `~/.ssh/config`.
比如复制远程服务器`dell`下的`~/Desktop/draft.lyx`这个文件到当前目录`.`,下面的方法都是可以的

```bash
scp tom@192.168.218.191:~/Desktop/draft.lyx .
scp tom@dell:~/Desktop/draft.lyx .
scp dell:~/Desktop/draft.lyx .
scp dell:document.txt . #不写明路径则默认是家目录下

```

和 `ssh` 命令一样,如果你所期望的远端主机帐户与你本地系统中的不一致, 则可以把用户名添加到远端主机名的开头.

```bash
$ scp bob@remote-sys:document.txt .
```

第二个 `SSH` 文件复制命令是 `sftp`,正如其名字所示,它是 `ftp` 程序的安全替代品.
`sftp` 工作起来与我们 之前使用的 `ftp` 程序很相似;然而,它不用明码形式来传递数据,它使用加密的 `SSH` 通道.

`sftp` 有一个 重要特性强于传统的 `ftp` 命令,就是 `sftp` 不需要远端系统中运行 `FTP` 服务器.它仅仅要求 `SSH` 服务器.
这意味着任何一台能用 `SSH` 客户端连接的远端机器,也可当作类似于 `FTP` 的服务器来使用. 这里是一个样本会话:

```bash
$ sftp remote-sys
Connecting to remote-sys...
me@remote-sys's password:
sftp> ls
ubuntu-8.04-desktop-i386.iso
sftp> lcd Desktop
sftp> get ubuntu-8.04-desktop-i386.iso
Fetching /home/me/ubuntu-8.04-desktop-i386.iso to ubuntu-8.04-
desktop-i386.iso
/home/me/ubuntu-8.04-desktop-i386.iso 100% 699MB 7.4MB/s 01:35
sftp> bye
```

小贴示:这个 `SFTP` 协议被许多 `Linux` 发行版中的图形化文件管理器支持.
使用 `Nautilus` (GNOME), 或者是 `Konqueror` (KDE),我们都能在位置栏中输入以 `sftp://` 开头的 URI, 来操作存储在运行着 SSH 服务器的远端系统中的文件.

### Windows 中的 SSH 客户端

比方说你正坐在一台 Windows 机器前面,但是你需要登录到你的 Linux 服务器中,去完成 一些实际的工作,那该怎么办呢?
当然是得到一个 Windows 平台下的 SSH 客户端!有很多这样 的工具.

最流行的可能就是由 Simon Tatham 和他的团队开发的 `PuTTY` 了.
这个 `PuTTY` 程序 能够显示一个终端窗口,而且允许Windows 用户在远端主机中打开一个 SSH(或者 telnet)会话.
这个程序也提供了 `scp` 和 `sftp` 程序的类似物.

[PuTTY链接](http://www.chiark.greenend.org.uk/~sgtatham/putty/)

拓展阅读

Linux 文档项目提供了 Linux 网络管理指南,可以广泛地(虽然过时了)了解网络管理方面的知识.

[http://tldp.org/LDP/nag2/index.html](http://tldp.org/LDP/nag2/index.html)

`Wikipedia` 上包含了许多网络方面的优秀文章.这里有一些基础的:

[http://en.wikipedia.org/wiki/Internet_protocol_address](http://en.wikipedia.org/wiki/Internet_protocol_address)
[http://en.wikipedia.org/wiki/Host_name](http://en.wikipedia.org/wiki/Host_name)
[http://en.wikipedia.org/wiki/Uniform_Resource_Identifier](http://en.wikipedia.org/wiki/Uniform_Resource_Identifier)

### ssh ipv6

[Linux下通过IPv6使用SSH和SCP](http://beanocean.github.io/tech/2014/10/17/scp_ipv6/)

解决这个问题的主要思路有两个,第一个是在路由器上设置`NAT`,进行端口映射;
第二个便是利用`IPv6`登录. 其中,`IPv6`的方式最方便(Linux默认是开启`IPv6`服务的),无须多余设置,只需要知道`IPv6`地址即可.
具体方法如下: (假设`IPv6`地址为`2101:da8:a000:12:bc26:9915:4b1d:64cc`)

***
`ssh`远程登录服务器

```bash
# 用法:
ssh [username]@[IPv6_Host] -p [port number]
# 例子:
ssh lg@2101:da8:a000:12:bc26:9915:4b1d:64cc -p 1234
```

***
`SCP`拷贝文件

```bash
# 用法;
scp [username]@[IPv6_Host]:[file_path] [target_path]
# 例子:
scp lg@\[2101:da8:a000:12:bc26:9915:4b1d:64cc\]:/home/lg/example.c ~/home/lg/src
```

这里需要注意的是,由于`IPv6`地址中的冒号和`host`中的冒号有冲突,需要用中括号加转义字符的方式把`IPv6`的地址括起来.

### 补充:ssh的使用

***
连接服务器

[Linux SSH 使用](https://www.jianshu.com/p/e6d308e9162f)

确认安装好`ssh`并启动后,我们在`windows`, `mac`上或者其他`linux`服务器上通过以下命令便可以连接到这台主机

```bash
ssh root@192.168.0.105
```

`root` 表示你连接改服务器的用户名

`192.168.0.105` 是服务器`ip`.这个`ip`不能使用内网`ip`,如果是本地虚拟机的话,可以将连接方式改为`桥接`的方式.
然后用`ifconfig`查看本机公网`ip`

***
`ssh` 的 `config` 文件

先展示一下`SSH config` 语法关键字,如下五个:

+ `Host` 别名
+ `HostName` 主机名
+ `Port` 端口
+ `User` 用户名
+ `IdentityFile` 密钥文件的路径

这个`config`的路径在服务的位置是 `~/.ssh/config` 如果没有找到这个文件,就在`>~/.ssh/config`

那么说到这个文件我们怎么用呢?
实际上在平时的运维管理中,我们可能管理多台机器,可能是几台,十几台甚至上百台.
我们将这些服务器配置在`config`中,方便我们去连接和管理.

例如:(`IdentityFile`可以暂时不配置,`ssh`默认端口为`22`)

```bash
host "KatoUyi"
    HostName 192.168.0.105
    User root
    Port 22
    IdentityFile  ~/.ssh/id_rsa
    IdentitiesOnly  yes

host "NagaSiren"
    HostName 192.168.0.106
    User root
    Port 22
```

在配置了这个文件之后,我们不需要再通过 `ssh root@192.168.0.105` 这个命令去连接服务器了,
我们可以这么写连接语句 `ssh KatoUyi` .这样管理方式在一定程度上简化了我们的操作.

***
SSH免密登录ssh-keygen

[Linux 配置SSH免密登录`ssh-keygen`](https://www.jb51.net/article/163093.htm)

为了在不同平台/网络主机之间的通信安全, 很多时候我们都要通过`ssh`进行认证. `ssh`认证方式主要有2种:

+ 基于口令的安全认证: 每次登录的时候都要输入用户名和密码, 由于要在网络上传输密码, 可能存在中间人攻击的风险;
+ 基于密钥的安全认证: 配置完成后就可以实现免密登录, 这种方式更加安全 -- 不需要在网络上传递口令, 只需要传输一次公钥.
常见的`git`的`ssh`方式就是通过公钥进行认证的.

说明: 这里演示所用的服务器操作系统是`Cent OS 7`. 我们的目标是:
`A`服务器(`172.16.22.131`) 能免密登录`B`服务器 (`172.16.22.132`).

注意: `ssh`连接是单向的, `A`能免密登录`B`, 并不能同时实现`B`能免密登录`A`.

***
安装必需的软件

在操作之前, 先确保所需要的软件已经正常安装.

这里我们需要安装`ssh-keygen`和`ssh-copy-id`,

***
`ssh-keygen`创建公钥-私钥对

1. 在指定目录下生成`rsa`密钥, 并指定注释为`xxx`, 实现示例:

```bash
$ ssh-keygen -t rsa -f ~/.ssh/id_rsa -C "xxxxx"
#        ~密钥类型 ~密钥文件路径及名称 ~ 备注信息
Generating public/private rsa key pair.
Enter passphrase (empty for no passphrase): # 输入密码, 若不输入则直接回车
Enter same passphrase again: # 再次确认密码, 若不输入则直接回车
...
```

注意: 密钥的文件名称必须是`id_xxx`, 这里的`xxx`就是`-t`参数指定的密钥类型.
比如密钥类型是`rsa`, 那么密钥文件名就必须是`id_rsa`.

***
`ssh-keygen`常用参数说明:

+ `-t`: 密钥类型, 可以选择 `dsa | ecdsa | ed25519 | rsa`;
+ `-f`: 密钥目录位置, 默认为`/home/username/.ssh/`, 默认密钥文件名以`id_rsa`开头. 如果是`root`用户, 则在`/root/.ssh/id_rsa`.
+ `-C`: 指定此密钥的备注信息, 需要配置多个免密登录时, 建议补充;
+ `-N`: 指定此密钥对的密码, 如果指定此参数, 则命令执行过程中就不会出现交互确认密码的信息了.

举例说明: 同时指定目录位置, 密码, 注释信息, 就不需要输入回车键即可完成创建:

```bash
ssh-keygen -t rsa -f ~/.ssh/id_rsa -N shoufeng -C shoufeng
```

***
前往`~/.ssh/`目录下查看生成的文件:

```bash
# 生成的文件以test_rsa开头, test_rsa是私钥, test_rsa.pub是公钥:
$ ls
test_rsa test_rsa.pub

# 通过cat命令查看公钥文件:
$ cat id_rsa.pub
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC2...
# 可以看到最后有一个注释内容shoufeng
```

***
`ssh-copy-id` 把`A`的公钥发送给`B`

默认用法是: `ssh-copy-id root@172.16.22.132`,

`ssh-copy-id`命令连接远程服务器时的默认端口是`22,` 当然可以指定`文件`, 远程主机的`IP`, `用户`和`端口`:

```bash
# 指定要拷贝的本地文件, 远程主机的IP+用户名+端口号:
$ ssh-copy-id -i ~/.ssh/id_rsa.pub -p 22 root@172.16.22.132
...
root@172.16.22.132's password: # 输入密码后, 将拷贝公钥
...
```

`-i identity_file`

***
在A服务器上免密登录B服务器

```bash
$ ssh root@172.16.22.132
Last login: Fri Jun 14 08:46:04 2019 from 192.168.34.16 # 登录成功
```

***
也可以用`ssh-agent`和`ssh-add`命令

+ `ssh-agent`: a program to hold private keys used for public key authentication (RSA, DSA, ECDSA, Ed25519).
+ `ssh-add`: adds private key identities to the authentication agent

首先先将本机的 `rsa.pub`公钥追加到目标服务器的 `authorized_keys` 中.然后执行以下命令:

```bash
ssh-agent bash
ssh-add  ~/.ssh/'私钥文件 '
```

完成了这个之后,就可以直接用 `ssh root@192.168.0.106`  直接连接服务器而不需要输入密码了

***
扩展说明

其他方式发送公钥文件

上述步骤是通过`ssh-copy-id`工具发送公钥文件的, 当然我们也可以通过其他方式实现:

***
将`A`的公钥文件发给`B`:

通过`scp`命令将A服务器的公钥文件发送到B服务器的用户目录下, 因为还没有配置成功免密登录,
所以期间需要输入B服务器对应用户的密码:

```bash
$ scp id_rsa.pub root@172.16.22.132:/root/.ssh
root@172.16.22.132's password:
id_rsa.pub           100% 390  0.4KB/s 00:00
```

在B上创建`authorized_keys`文件:

```bash
$ cd /root/.ssh/
$ ls
id_rsa.pub
# 通过A服务器的公钥生成"authorized_keys"文件:
$ cat id_rsa.pub >> authorized_keys
$ cat authorized_keys
...
```

注意: 上述重定向时使用`>>`进行追加, 不要用`>`, 那会清空原有内容.

****
文件权限

为了让私钥文件和公钥文件能够在认证中起作用, 需要确保权限的正确性:

+ 对于`.ssh`目录以及其内部的公钥, 私钥文件, 当前用户至少要有执行权限, 其他用户最多只能有执行权限.
+ 不要图省事设置成`777`权限: 太大的权限不安全, 而且数字签名也不支持这种权限策略.
+ 对普通用户, 建议设置成`600`权限: `chmod 600 authorized_keys id_rsa id_rsa.pub`;
+ 对root用户, 建议设置成`644`权限: `chmod 644 authorized_keys id_rsa id_rsa.pub`.

***
windows 免密码登录

`windows`中生成密钥: 可以在`Xshell`中也可以达到类似效果,这种方式生成了密钥之后,可以将之保存起来.
当然也可以通过其他方式例如`git bash`中用`linux`指令生成,在这里不详细描述了.

那么我们怎么使用这个生成好的`ssh key`呢.
为了达到免密码的登录过程,我们需要将公钥放置在`authorized_keys`这个文件中.

我们需要先进入linux服务器,将我们选择的这个 `id_rsa_2048.pub` 的内容放置到linux服务器的`authorized_keys`文件中.
这样的话我们再访问就可以无密码连接了.

***
ssh 安全端口

端口安全指的是尽量避免服务器的远程连接端口被不法份子知道,为此而改变默认的服务端口号的操作.

在上一节中我们知道了`SSH`的默认端口是`22`.可以修改默认端口.
对应需要修改的文件是 `/etc/ssh/sshd_config`. 我们也可以同时监听多个端口.

### SSH连接Ubuntu图形界面

[SSH连接Ubuntu图形界面](https://www.jianshu.com/p/9279b2ed8821)

在`Ubuntu`系统下, `ssh`客户端一般是自带的,`ssh`服务端一般需要自己安装: `$sudo apt-get install openssh-server`.

假设发起连接的主机被称为客户端主机, 用大写字母`A`表示, `IP`为`A.A.A.A`, 被连接的主机为服务端主机, 用大写字母`B`表示, `IP`为`B.B.B.B`.
`ssh`连接成功后, 如果想要显示图形界面, 需要做一些配置. 步骤如下:

***
服务器端的`ssh`必须运行转发`X`界面, 在`ssh`服务器中, 也就是`B`中, 打开这个配置文件, 注意是`sshd_config`文件:

```bash
$sudo vim /etc/ssh/sshd_config
```

找到这一行, 编辑如下:

```bash
X11Forwarding yes
```

确保这个是`yes`(注意这个配置文件中的#是注释, 确保`X11Forwarding`前面没有`#`注释), 然后重启`ssh`服务:

```bash
systemctl restart ssh.service
```

***
客户端配置:打开配置文件, 注意是`ssh_config`文件:

```bash
$sudo vim /etc/ssh/ssh_config
```

找到以下语句并编辑:

```bash
ForwardAgent yes
ForwardX11 yes
ForwardX11Trusted yes
```

这`3`个确保是`yes`(注意这个配置文件中的`#`是注释, 确保你修改的行没有被`#`注释掉)

***
配置完成后, 进入`A`主机终端. 设置允许其他主机的`ssh`服务器的`X`界面连接过来`xhost +`, 然后执行:

```bash
$ssh -Y  username@192.168.3.102
```

注意`-Y`这个是大写的`Y`,接着会提示输入密码, 然后会连接到`B`服务器主机. 最后做个测试, 执行`$xclock`
如果这个时候在`A`主机`Ubuntu`界面中出现了一个时钟界面, 那么恭喜你成功了.

### ssh 选项

`-X`:启用 `X11` 转发. 这也可以在配置文件中针对每个主机进行指定.
应谨慎启用 `X11` 转发. 能够绕过远程主机上的文件权限的用户(对于用户的X授权数据库)可以通过转发的连接访问本地的X11显示.
然后攻击者可能能够执行诸如按键监控之类的活动.
为此, `X11` 转发默认受 `X11 SECURITY` 扩展限制. 请参考 `ssh -Y `选项以及`ssh_config(5)`中的 `ForwardX11Trusted` 指令以获取更多信息.
(Debian-specific:`X11` 转发默认不受 `X11 SECURITY` 扩展限制, 因为当前太多程序在这种模式下崩溃.
将 `ForwardX11Trusted` 选项设置为`no`以恢复上游行为. 取决于客户端的改进这可能会在未来改变. )

`-x`: 禁用 `X11` 转发.

`-Y`: 启用受信任的 `X11` 转发. 受信任的 `X11` 转发不受 `X11` 安全扩展控制的约束.
(Debian-specific:在默认配置中, 此选项等效于`-X`, 如上所述, `ForwardX11Trusted`默认为`yes`.
将 `ForwardX11Trusted` 选项设置为`no`以恢复上游行为. 取决于客户端的改进这可能会在未来改变. )

`-N`: 不执行远程命令.  这对于仅转发端口很有用.

`-f`: 要求 `ssh`在执行`command`之前进入后台. 如果用户希望`ssh`在后台, 同时不错过输入`passwords`或`passphrases`, 这很有用.
这意味着 `-n`.  在远程站点启动 `X11` 程序的推荐方法是: `ssh -f host xterm`.

如果 `ExitOnForwardFailure` 配置选项设置为`yes`, 则以 `-f` 启动的客户端将等待所有远程端口转发建立成功之后, 再将自己置于后台.

`-l login_name`: 指定在远程机器上登录的用户.  这也可以在配置文件中针对每个主机进行指定.

### sshfs

[A network filesystem client to connect to SSH servers ](https://github.com/libfuse/sshfs)
[使用 SSHFS 挂载远程的 Linux 文件系统及目录](https://www.linuxprobe.com/sshfs-linux-fires.html)

`SSHFS`(Secure SHell FileSystem)是一个客户端, 可以让我们通过 `SSH` 文件传输协议(`SFTP`)挂载远程的文件系统并且在本地机器上和远程的目录和文件进行交互.

`SFTP` 是一种通过 `SSH` 协议提供文件访问, 文件传输和文件管理功能的安全文件传输协议. 因为 `SSH` 在网络中从一台电脑到另一台电脑传输文件的时候使用数据加密通道,
并且 `SSHFS` 内置在 `FUSE`(用户空间的文件系统)内核模块, 允许任何非特权用户在不修改内核代码的情况下创建他们自己的文件系统.

***
在 Linux 系统上安装 `SSHFS`: 默认情况下, sshfs 包不存在所有的主流 `Linux` 发行版中, 你需要在你的 `Linux` 系统中启用 `epel`, 在 Yum 命令行的帮助下安装 `SSHFS` 及其依赖.

```bash
# yum install sshfs
# dnf install sshfs              [在 Fedora 22+ 发行版上]
$ sudo apt-get install sshfs     [基于 Debian/Ubuntu 的系统]
```

***
大概步骤为:

+ 创建 `SSHFS` 挂载目录
+ 使用 `SSHFS` 挂载远程的文件系统
+ 卸载远程的文件系统

```bash
$ sudo mkdir /mnt/tecmint     #创建一个挂载点目录
$ sudo sshfs -o allow_other tecmint@x.x.x.x:/home/tecmint/ /mnt/tecmint  #替换成你的 `IP` 地址和挂载点
$ sudo umount /mnt/tecmint # 和平常的卸载方式一样
```

如果你的 `Linux` 服务器配置为基于 `SSH` 密钥授权, 那么你将需要使用如下所示的命令行指定你的公共密钥的路径.

```bash
# sshfs -o IdentityFile=~/.ssh/id_rsa tecmint@x.x.x.x:/home/tecmint/ /mnt/tecmint
$ sudo sshfs -o allow_other,IdentityFile=~/.ssh/id_rsa tecmint@x.x.x.x:/home/tecmint/ /mnt/tecmint     [基于 Debian/Ubuntu 的系统]
```

如果你已经成功的运行了上面的命令并且没有任何错误, 你将会看到挂载在 `/mnt/tecmint` 目录下的远程的文件和目录的列表. 运行 `df -hT`命令, 你将会看到远程文件系统的挂载点.

```bash
# df -hT

Filesystem                          Type        Size  Used Avail Use% Mounted on
...
tecmint@192.168.0.102:/home/tecmint fuse.sshfs  324G   55G  253G  18% /mnt/tecmint
```

***
永久挂载远程文件系统

为了永久的挂载远程的文件系统, 你需要修改一个叫 `/etc/fstab` 的文件, 打开文件, 在底部添加下面的一行, 保存文件并退出. 下面条目表示使用默认的设置挂载远程的文件系统.

    sshfs#tecmint@x.x.x.x:/home/tecmint/ /mnt/tecmint fuse.sshfs defaults 0 0

确保服务器之间允许 `SSH` 无密码登录, 这样系统重启之后才能自动挂载文件系统.  如果你的服务器配置为基于 `SSH` 密钥的认证方式, 请加入如下行:

    sshfs#tecmint@x.x.x.x:/home/tecmint/ /mnt/tecmint fuse.sshfs IdentityFile=~/.ssh/id_rsa defaults 0 0

接下来, 你需要更新 `fstab` 文件使修改生效.

```bash
$ sudo mount -a   #基于 Debian/Ubuntu 的系统
```

## vnc 虚拟网络计算机

### archwiki

[TigerVNC](https://wiki.archlinux.org/title/TigerVNC_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

`TigerVNC` 是 `Virtual Network Computing` (VNC) 的一种实现. 它向用户提供一些远程功能, 包括:

+ 直接控制本地 `X` 会话.
+ 在一台机器上的后台并行 `X` 会话, 即并不显示在物理显示器上而是虚拟显示器. 即使用户断开连接, 在服务器上运行的所有程序依旧可以运行.

***
[run KDE Plasma 5 in VNC](https://unix.stackexchange.com/questions/315247/what-commands-are-needed-in-the-vnc-xstartup-file-to-run-kde-plasma-5-in-vn)

在`manjaro`上使用时, 连接到`vnc`服务器之后, 桌面是黑的, 可以打开终端, 这时执行`dbus-launch startplasma-x11 # or startplasma-wayland`即可打开桌面.

***
[D-Bus](https://wiki.archlinux.org/title/D-Bus)

`D-Bus` 是一个提供简便进程间通信的消息总线系统. 包含一个能以全系统或者针对一个用户会话运行的守护进程, 和一系列提供与 `D-Bus` 通信的库.
`dbus-launch`: 从 `shell` 脚本启动消息总线的程序

[Linux Device Model](https://linux-kernel-labs.github.io/refs/heads/master/labs/device_model.html)

`bus`是处理器和输入/输出设备之间的通信通道.  为了确保模型是通用的, 所有输入/输出设备都通过`bus`连接到处理器(它可以是没有对应的物理硬件的虚拟设备).

添加系统总线时, 它会出现在 `sysfs` 文件系统中的`/sys/bus`中.  与 `kobjects` 一样, 总线可以组织成层次结构, 并在 `sysfs` 中表示.

#### 为虚拟(无界面)会话运行 vncserver

***
初次设置
注意:  在物理内存允许的条件下, `Linux`系统可以拥有任意数量的VNC服务器 -- 它们同时并行运行, 互不干扰.

1. 用`vncpasswd`创建密码, 它会将哈希处理之后的密码存储在`~/.vnc/passwd`.
2. 编辑`/etc/tigervnc/vncserver.users`来定义用户映射. 这文件中定义的用户都会拥有独有的端口来运行它的会话.
这文件中的数字对应的是`TCP`端口. 默认情况下, `:1` 是`TCP`端口`5901`(`5900+1`).
如果需要运行一个并行的服务器, 第二个实例可以运行在下一个最大的, 未被占用的端口, 即`5902`(`5900+2`).
3. 创建`~/.vnc/config`, 至少要定义会话的类型, 比如`session=foo`(将`foo`替换为你想要运行的桌面环境).
你可以通过查看`/usr/share/xsessions/`里的`.desktop`文件来知道有哪些桌面环境在当前系统上可以使用. 比如:

~/.vnc/config

```bash
session=lxqt
geometry=1920x1080
localhost #这个配置会导致只能本地访问 vnc
alwaysshared
```

***
权限: 像对待 `~/.ssh` 一样保护 `~/.vnc` 是很好的做法, 虽然并非必须. 执行下面的命令来达到该目的:

```bash
$ chmod 700 ~/.vnc
```

***
启动与停止`tigervnc`

`systemctl start  vncserver@.service`, 如果需要让它随系统启动, `enable`它.
注意`/etc/tigervnc/vncserver.users`中定义的编号需要在`@`符号后面指定, 比如启动`:1`的命令是:

```bash
# systemctl start vncserver@:1
```

Note: 已经不再支持直接调用`/usr/bin/vncserver`了, 因为这样做不会建立完整可用的会话环境.
`systemd`服务是唯一受支持的使用`TigerVNC`的方式. 参见Issue #1096.

### 在物理显示器上(5900端口)运行VNC服务:

***
使用 `TigerVNC 的 x0vncserver` (推荐)

`TigerVNC` 提供名为 `x0vncserver` 的二进制文件, 它具有和 `x11vnc` 相类似的功能. 例如:

```bash
x0vncserver -display :0 -passwordfile ~/.vnc/passwd
```

欲获取更多信息, 执行`man x0vncserver`

***
使用 `x11vnc` (推荐)

如果需要远程控制物理显示, 使用 `x11vnc`. 参见 `X11vnc` 获取更多信息.

### 连接 VNC 服务

一个 `VNC` 服务可允许任意数量的客户端来连接. 下面给出一个简单例子, 其中 `VNC` 服务运行在 `10.1.10.2` 的 `5901`(`:1`, 使用速记法)端口:

```bash
$ vncviewer 10.1.10.2:1
```

#### 无密码验证

`-passwd` 开关允许我们定义服务器上 `~/.vnc/passwd` 文件的位置. 在服务器方面(无论是通过 `SSH` 还是物理接触), 用户需要有权访问该文件.
在任一情况下, 都应将该文件放在客户端文件系统的一个安全位置(例如一个仅给期望用户`read`权限的位置).

```bash
$ vncviewer -passwd /path/to/server-passwd-file
```

图形界面客户端示例

+ gtk-vnc
+ krdc
+ rdesktop
+ vinagre
+ remmina
+ vncviewer-jarAUR

### 使用 SSH 隧道加密 VNC 服务

#### 服务端配置

若希望从 `LAN` 保护之外访问 `VNC` 服务, 你需要考虑明文密码及客户端与服务端之间未加密通信的问题.
`VNC` 服务可以很简单地使用 `SSH` 隧道进行加密. 另外, 不要使用此方法对外界打开另一个端口, 因为通信会沿用户之前对 `WAN` 打开的 `SSH` 端口在隧道中依次进行.
在这种情况下, 强烈推荐使用 `-localhost` 开关运行 `vncserver`.
该开关仅允许接收从本机发起的连接, 并顺理成章地仅允许物理 `SSH` 连接并认证到机器上的用户.

```bash
$ vncserver -geometry 1440x900 -alwaysshared -dpi 96 -localhost :1
```

#### 客户端配置

既然服务器现在只接受本机的连接, 使用 `-L` 开关通过 `SSH` 连接到该机器来打开隧道. 例如:

```bash
$ ssh 目标机器IP -L 8900:localhost:5901
```

该命令将服务器的 `5901` 端口转发到客户机的 `8900` 端口. 一旦已通过 `SSH` 连接, 请保持该 `xterm` 或 `shell` 窗口开启 -- 它会作为和服务器通信的加密隧道.
为了通过 `VNC` 连接, 打开第二个 `xterm` 并连接到客户机的加密隧道上(而不是远程服务器的 `IP` 地址):

```bash
$ vncviewer localhost::8900
```

***
以下来自 `SSH` 的手册页面:

    -L [bind_address:]port:host:hostport
    -L [bind_address:]port:remote_socket
    -L local_socket:host:hostport
    -L local_socket:remote_socket

指定到本地`(client)`主机上给定 TCP 端口或 Unix 套接字的连接将被转发到给定远程端的`host`和`port`, 或 `Unix socket`.
通过分配一个`socket`来侦听本地端的 `TCP` 端口, 可选择绑定到指定的 `bind_address` 或 `Unix socket`.
每当`local port`或`socket`建立连接时, 连接通过安全通道转发(`forwarded`), 并连接到远程计算机上的口 `hostport` 或 `remote_socket`.

端口转发也可以在配置文件中指定. 只有超级用户才能转发特权端口.  `IPv6` 地址可以通过将地址括在方括号中来指定.

默认情况下, 根据`GatewayPorts`设置绑定本地端口. 显式的 `bind_address` 可用于将连接绑定到特定地址.
`bind_address`若为`localhost`, 表示绑定监听端口仅供本地使用, 而空地址或`*`表示该端口可以从所有`interfaces`使用.

### 在 Android 设备上通过 SSH 连接 VNC 服务器

为了在 `Android` 设备上通过 `SSH` 连接到 `VNC` 服务器, 需要具备以下条件:

1. 在要连接的机器上运行 `SSH` 服务
2. 在要连接的机器上运行 `VNC` 服务. (使用上面提到的 `-localhost `标志来运行服务)
3. 在 `Android` 设备上安装 `SSH` 客户端. (`ConnectBot` 是一个常见选择, 并在本向导中用作例子. )
4. 在 `Android` 设备上安装 `VNC` 客户端(`androidVNC`)

考虑为目标使用一些动态 `DNS` 服务, 来解决无固定 `IP` 地址问题.
在 `ConnectBot` 中, 键入 `IP` 并连接到所期望的机器. 按选项键, 选择端口转发, 并增加一个新端口然后保存之:

    Nickname: vnc
    Type: Local
    Source port: 5901
    Destination: 127.0.0.1:5901

在 `androidVNC` 中:

    Nickname: nickname
    Password: 设置 VNC 服务器时使用的密码
    Address: 127.0.0.1 (通过 ssh 连接之后, 我们在远程机器的"本地")
    Port: 5901

连接即可.

### 在开关机时启动关闭 VNC 服务

如果是`arch`, `vncserver.service`可能安装在`/usr/lib/systemd/system/vncserver.service`.
如果是`ubuntu`, 默认似乎没有安装`vncserver.service`, 自己创建一个: `sudo vim /etc/systemd/system/vncserver@.service`,

```.service
# nolisten=tcp 选项可以禁止使用`TCP`连接
# localhost 选项表示只能使用`ssh`隧道连接
# 详细查看 man vncviewer 的 "-via" 选项.

[Unit]
Description=Remote desktop service (VNC)
After=syslog.target network.target

[Service]
Type=forking
User=

# Clean any existing files in /tmp/.X11-unix environment
ExecStartPre=-/usr/bin/vncserver -kill %i
ExecStart=/usr/bin/vncserver %i
ExecStop=/usr/bin/vncserver -kill %i

[Install]
WantedBy=multi-user.target
```

### 复制远程机器剪贴板内容到本地

如果从远程机器到本机的复制不工作, 如该参考下面所述, 在服务器上执行 `autocutsel`:

```bash
$ autocutsel -fork
```

现在, 按下 `F8` 键显示 `VNC` 弹出菜单, 选择 `Clipboard: local -> remote` 选项.

你可以将上述命令放在 `~/.vnc/xstartup` 中, 以使其在 `VNC` 服务启动时自动运行.

### 解决无光标问题

如果使用 `x0vncserver` 时光标无法显示, 那么像下面这样启动 `vncviewer`:

```bash
$ vncviewer DotWhenNoCursor=1 <server>
```

或者将 `DotWhenNoCursor=1` 写在 `tigervnc` 的配置文件(默认在 `~/.vnc/default.tigervnc`)中.

### ubuntu 20.04

[tigervncserver crashes unless started with sudo](https://stackoverflow.com/questions/59709214/tigervncserver-crashes-unless-started-with-sudo)

`ubuntu` 上安装完`tigervncserver`, 用`systemctl`启动时失败, 原因可能是没有` ~/.vnc/xstartup`文件. 创建`~/.vnc/xstartup`, 内容如下:

```bash
#!/bin/bash

PATH=/usr/bin:/usr/sbin
unset SESSION_MANAGER
unset DBUS_SESSION_BUS_ADDRESS
exec cinnamon-session-cinnamon &
```

`exec`后面的值可以查看`/usr/share/xsessions/`中`.desktop`文件内的写法, 例如`cinnamon.desktop`中的值是`cinnamon-session-cinnamon`.

之后启动`vncserver`就不需要`sudo`了, 也不会`crash`.
我猜缺少可执行文件会阻止 `vncserver` 启动, 但是使用 `sudo` 权限, 它设法从某些文件中提取的默认设置开始, 这些文件只能由 `sudoers` 访问.
但在`ubuntu 20.04`上使用`gnome-session`能连接, 但是会有黑屏问题.
