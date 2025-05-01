# ssh 使用

## 与远程主机安全通信

通过网络来远程操控类 Unix 的操作系统已经有很多年了.

早些年,在因特网普遍推广之前,有 一些受欢迎的程序被用来登录远程主机.它们是 rlogin 和 telnet 程序.
然而这些程序,拥有和 `ftp` 程序 一样的致命缺点;它们以明码形式来传输所有的交流信息(包括登录命令和密码).
这使它们完全不 适合使用在因特网时代.

## ssh协议

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

## SSH 通道

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

## scp 和 sftp

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

## Windows 中的 SSH 客户端

比方说你正坐在一台 Windows 机器前面,但是你需要登录到你的 Linux 服务器中,
去完成 一些实际的工作,那该怎么办呢?
当然是得到一个 Windows 平台下的 SSH 客户端!有很多这样 的工具.

最流行的可能就是由 Simon Tatham 和他的团队开发的 `PuTTY` 了.
这个 `PuTTY` 程序 能够显示一个终端窗口,
而且允许Windows 用户在远端主机中打开一个 SSH(或者 telnet)会话.
这个程序也提供了 `scp` 和 `sftp` 程序的类似物.

[PuTTY链接](http://www.chiark.greenend.org.uk/~sgtatham/putty/)

安装 windows 可选组件, ssh 服务端

### windows scp

windows 上的 `scp` 指定端口使用大写的 `-P`

```bash
# 拷贝本地文件夹中的 myFile* 到远程 目录 /sdcard/1doc; 支持通配符
scp -P 8022 myFile* 192.168.3.28:/sdcard/1doc

# 将远程文件 /home/tom/.bashrc 拷贝到 当前目录.
scp 192.168.3.28:/home/tom/.bashrc .
```

### 拓展阅读

Linux 文档项目提供了 Linux 网络管理指南,可以广泛地(虽然过时了)了解网络管理方面的知识.

[http://tldp.org/LDP/nag2/index.html](http://tldp.org/LDP/nag2/index.html)

`Wikipedia` 上包含了许多网络方面的优秀文章.这里有一些基础的:

[http://en.wikipedia.org/wiki/Internet_protocol_address](http://en.wikipedia.org/wiki/Internet_protocol_address)
[http://en.wikipedia.org/wiki/Host_name](http://en.wikipedia.org/wiki/Host_name)
[http://en.wikipedia.org/wiki/Uniform_Resource_Identifier](http://en.wikipedia.org/wiki/Uniform_Resource_Identifier)

## ssh ipv6

[Linux下通过IPv6使用SSH和SCP](http://beanocean.github.io/tech/2014/10/17/scp_ipv6/)

解决这个问题的主要思路有两个,第一个是在路由器上设置`NAT`,进行端口映射;
第二个便是利用`IPv6`登录. 其中,`IPv6`的方式最方便(Linux默认是开启`IPv6`服务的),无须多余设置,只需要知道`IPv6`地址即可.
具体方法如下: (假设`IPv6`地址为`2101:da8:a000:12:bc26:9915:4b1d:64cc`)

### `ssh`远程登录服务器

```bash
# 用法:
ssh [username]@[IPv6_Host] -p [port number]
# 例子:
ssh lg@2101:da8:a000:12:bc26:9915:4b1d:64cc -p 1234
```

### `SCP`拷贝文件

```bash
# 用法;
scp [username]@[IPv6_Host]:[file_path] [target_path]
# 例子:
scp lg@\[2101:da8:a000:12:bc26:9915:4b1d:64cc\]:/home/lg/example.c ~/home/lg/src
```

这里需要注意的是,由于 `IPv6` 地址中的冒号和`host`中的冒号有冲突,
需要用 `\[` 符号把`IPv6`的地址括起来.

以下补充 `ssh` 的使用

## 连接服务器

[Linux SSH 使用](https://www.jianshu.com/p/e6d308e9162f)

确认安装好`ssh`并启动后,我们在`windows`, `mac`上或者其他`linux`服务器上通过以下命令便可以连接到这台主机

```bash
ssh root@192.168.0.105
```

`root` 表示你连接改服务器的用户名

`192.168.0.105` 是服务器`ip`.这个`ip`不能使用内网`ip`,如果是本地虚拟机的话,可以将连接方式改为`桥接`的方式.
然后用`ifconfig`查看本机公网`ip`

### `ssh` 的 `config` 文件

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

## SSH免密登录ssh-keygen

[Linux 配置SSH免密登录`ssh-keygen`](https://www.jb51.net/article/163093.htm)

为了在不同平台/网络主机之间的通信安全, 很多时候我们都要通过`ssh`进行认证. `ssh`认证方式主要有2种:

+ 基于口令的安全认证: 每次登录的时候都要输入用户名和密码, 由于要在网络上传输密码, 可能存在中间人攻击的风险;
+ 基于密钥的安全认证: 配置完成后就可以实现免密登录, 这种方式更加安全 -- 不需要在网络上传递口令, 只需要传输一次公钥.
常见的`git`的`ssh`方式就是通过公钥进行认证的.

说明: 这里演示所用的服务器操作系统是`Cent OS 7`. 我们的目标是:
`A`服务器(`172.16.22.131`) 能免密登录`B`服务器 (`172.16.22.132`).

注意: `ssh`连接是单向的, `A`能免密登录`B`, 并不能同时实现`B`能免密登录`A`.

### 安装必需的软件

在操作之前, 先确保所需要的软件已经正常安装.
这里我们需要安装`ssh-keygen`和`ssh-copy-id`,

### `ssh-keygen`创建公钥-私钥对

1. 在指定目录下生成`rsa`密钥, 并指定注释为`xxx`, 实现示例:

```bash
$ ssh-keygen -t rsa -f ~/.ssh/id_rsa -C "abc@def.com"
#        ~密钥类型 ~密钥文件路径及名称 ~ 备注信息
Generating public/private rsa key pair.
Enter passphrase (empty for no passphrase): # 输入密码, 若不输入则直接回车
Enter same passphrase again: # 再次确认密码, 若不输入则直接回车
...
```

注意: 密钥的文件名称必须是`id_xxx`, 这里的`xxx`就是`-t`参数指定的密钥类型.
比如密钥类型是`rsa`, 那么密钥文件名就必须是`id_rsa`.

`ssh-keygen`常用参数说明:

+ `-t`: 密钥类型, 可以选择 `dsa | ecdsa | ed25519 | rsa`;
+ `-f`: 密钥目录位置, 默认为`/home/username/.ssh/`, 默认密钥文件名以`id_rsa`开头. 如果是`root`用户, 则在`/root/.ssh/id_rsa`.
+ `-C`: 指定此密钥的备注信息, 需要配置多个免密登录时, 建议补充;
+ `-N`: 指定此密钥对的密码, 如果指定此参数, 则命令执行过程中就不会出现交互确认密码的信息了.

举例说明: 同时指定目录位置, 密码, 注释信息, 就不需要输入回车键即可完成创建:

```bash
ssh-keygen -t rsa -f ~/.ssh/id_rsa -N shoufeng -C shoufeng
```

### 前往`~/.ssh/`目录下查看生成的文件:

```bash
# 生成的文件以test_rsa开头, test_rsa是私钥, test_rsa.pub是公钥:
$ ls
test_rsa test_rsa.pub

# 通过cat命令查看公钥文件:
$ cat id_rsa.pub
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC2...
# 可以看到最后有一个注释内容shoufeng
```

### `ssh-copy-id` 把`A`的公钥发送给`B`

默认用法是: `ssh-copy-id root@172.16.22.132`,

`ssh-copy-id`命令连接远程服务器时的默认端口是`22,`
当然可以指定`文件`, 远程主机的`IP`, `用户`和`端口`:

```bash
# 指定要拷贝的本地文件, 远程主机的IP+用户名+端口号:
$ ssh-copy-id -i ~/.ssh/id_rsa.pub -p 22 root@172.16.22.132
...
root@172.16.22.132's password: # 输入密码后, 将拷贝公钥
...
```

`-i`后面跟上要认证的公钥, 自己的

+ 在A服务器上免密登录B服务器

```bash
$ ssh root@172.16.22.132
Last login: Fri Jun 14 08:46:04 2019 from 192.168.34.16 # 登录成功
```

### 也可以用`ssh-agent`和`ssh-add`命令

+ `ssh-agent`: a program to hold private keys used for public key authentication (RSA, DSA, ECDSA, Ed25519).
+ `ssh-add`: adds private key identities to the authentication agent

首先先将本机的 `rsa.pub` 公钥追加到目标服务器的 `authorized_keys` 中.
然后执行以下命令:

```bash
ssh-agent bash
ssh-add  ~/.ssh/'私钥文件 '
```

完成了这个之后,就可以直接用 `ssh root@192.168.0.106`  直接连接服务器而不需要输入密码了

### 扩展说明

其他方式发送公钥文件

上述步骤是通过`ssh-copy-id`工具发送公钥文件的, 当然我们也可以通过其他方式实现:

+ 将`A`的公钥文件发给`B`:

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

### 文件权限

为了让私钥文件和公钥文件能够在认证中起作用, 需要确保权限的正确性:

+ 对于`.ssh`目录以及其内部的公钥, 私钥文件, 当前用户至少要有执行权限, 其他用户最多只能有执行权限.
+ 不要图省事设置成`777`权限: 太大的权限不安全, 而且数字签名也不支持这种权限策略.
+ 对普通用户, 建议设置成`600`权限: `chmod 600 authorized_keys id_rsa id_rsa.pub`;
+ 对root用户, 建议设置成`644`权限: `chmod 644 authorized_keys id_rsa id_rsa.pub`.

### windows 免密码登录

`windows`中生成密钥: 可以在`Xshell`中也可以达到类似效果,这种方式生成了密钥之后,可以将之保存起来.
当然也可以通过其他方式例如`git bash`中用`linux`指令生成,在这里不详细描述了.

那么我们怎么使用这个生成好的`ssh key`呢.
为了达到免密码的登录过程,我们需要将公钥放置在`authorized_keys`这个文件中.

我们需要先进入linux服务器,将我们选择的这个 `id_rsa_2048.pub` 的内容放置到linux服务器的`authorized_keys`文件中.
这样的话我们再访问就可以无密码连接了.

### ssh 安全端口

端口安全指的是尽量避免服务器的远程连接端口被不法份子知道,为此而改变默认的服务端口号的操作.

在上一节中我们知道了`SSH`的默认端口是`22`.可以修改默认端口.
对应需要修改的文件是 `/etc/ssh/sshd_config`. 我们也可以同时监听多个端口.

## SSH连接Ubuntu图形界面

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

## ssh 选项

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

## sshfs

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
