# tricks

## Linux技巧

[应该知道的Linux技巧](https://coolshell.cn/articles/8883.html)

### 基础

+ 学习 `Bash` .你可以`man bash`来看看`bash`的东西,并不复杂也并不长.
你用别的shell也行,但是bash是很强大的并且也是系统默认的.(学习zsh或tsch只会让你在很多情况下受到限制)

+ 学习 `vim` . 在Linux下,基本没有什么可与之竞争的编译辑器(就算你是一个Emacs或Eclipse的重度用户).
你可以看看`<简明vim攻略>`和 `<Vim的冒险游戏>`以及 `<给程序员的Vim速查卡>` 还有 `<把Vim变成一个编程的IDE>` 等等.

+ 了解 `ssh`.明白不需要口令的用户认证(通过`ssh-agent`, `ssh-add`),学会用`ssh`翻墙,用`scp`而不是`ftp`传文件,等等.
你知道吗? `scp` 远端的时候, 你可以按`tab`键来查看远端的目录和文件(当然,需要无口令的用户认证),这都是 bash 的功劳.

+ 熟悉`bash`的作业管理,如: `&`, `Ctrl-Z`, `Ctrl-C`, `jobs`, `fg`, `bg`, `kill`, 等等.
当然,你也要知道`Ctrl+\`(SIGQUIT)和`Ctrl+C` (SIGINT)的区别.

+ 简单的文件管理 :  `ls` 和 `ls -l` (你最好知道 `ls -l` 的每一列的意思),
`less`, `head`, `tail` 和 `tail -f`, `ln` 和 `ln -s` (你知道明白`hard link`和`soft link`的不同和优缺点),
`chown`, `chmod`, `du` (如果你想看看磁盘的大小 `du -sk *`), `df`, `mount`,`find`
+ 基础的网络管理:  `ip` 或 `ifconfig`, `dig`,`netstat`, `ping`, `traceroute`, 等

+ 理解正则表达式,还有`grep`/`egrep`的各种选项.比如:  `-o`, `-A`, 和 `-B` 这些选项是很值得了解的.

+ 学习使用 `apt-get` 和 `yum` 来查找和安装软件(前者的经典分发包是Ubuntu,
后者的经典分发包是Redhat),我还建议你试着从源码编译安装软件.

### 日常

+ 在 `bash` 里,使用 `Ctrl-R` 而不是上下光标键来查找历史命令.

+ 在 `bash` 里,使用 `Ctrl-W` 来删除最后一个单词,使用 `Ctrl-U` 来删除一行.
请`man bash`后查找`Readline Key Bindings`一节来看看`bash`的默认热键,
比如: `Alt-.` 把上一次命令的最后一个参数打出来,而`Alt-*` 则列出你可以输入的命令.

+ 回到上一次的工作目录:  `cd –`  (回到`home`是` cd ~`)

+ 使用 `xargs`.这是一个很强大的命令.
你可以使用`-L`来限定有多少个命令,也可以用`-P`来指定并行的进程数.
如果你不知道你的命令会变成什么样,你可以使用`xargs echo`来看看会是什么样.
当然, `-I{}` 也很好用.示例:

```bash
find . -name \*.py | xargs grep some_function
cat hosts | xargs -I{} ssh root@{} hostname
```

+ `pstree -p` 可以帮你显示进程树.
+ 使用 `pgrep` 和 `pkill` 来找到或是`kill`某个名字的进程. (`-f` 选项很有用).

+ 了解可以发给进程的信号.例如: 要挂起一个进程,使用 `kill -STOP [pid]`.
使用 `man 7 signal` 来查看各种信号,使用`kill -l` 来查看数字和信号的对应表

+ 使用 `nohup` 或  `disown` 如果你要让某个进程运行在后台.
+ 使用`netstat -lntp`来看看有侦听在网络某端口的进程.当然,也可以使用`lsof`.
+ 在`bash`的脚本中,你可以使用`set -x`来debug输出.
使用 `set -e` 来当有错误发生的时候`abort`执行.考虑使用 `set -o pipefail` 来限制错误.
还可以使用`trap`来截获信号(如截获`ctrl+c`).

+ 在bash 脚本中,`subshells` (写在圆括号里的) 是一个很方便的方式来组合一些命令.
一个常用的例子是临时地到另一个目录中,例如:

```bash
# do something in current dir
(cd /some/other/dir; other-command)
# continue in original dir
```

+ 在 bash 中有很多**变量展开**. 如: 检查一个变量是否存在: `${name:?error message}`.
比如脚本参数的表达式:  `input_file=${1:?usage: $0 input_file}`.
计算表达式:  `i=$(( (i + 1) % 5 ))`.
一个序列:  `{1..10}`.
截断一个字符串:  `${var%suffix}` 和 `${var#prefix}`.
示例: `if var=foo.pdf; then echo ${var%.pdf}.txt;fi;`

+ 通过 `<(some command)` 可以把某命令当成一个文件.
示例: 比较一个本地文件和远程文件` /etc/hosts`:  `diff /etc/hosts <(ssh somehost cat /etc/hosts)`
+ 了解什么叫 `here documents` ,就是诸如 `cat <<EOF` 这样的东西.

+ 在 bash中,使用重定向到标准输出和标准错误.如:  `some-command >logfile 2>&1`.
另外,要确认某命令没有把某个打开了的文件句柄重定向给标准输入,最佳实践是加上 `</dev/null`,把`/dev/null`重定向到标准输入.
+ 使用 `man ascii` 来查看 `ASCII` 表.

+ 在远端的 ssh 会话里,使用 `screen` 或 `dtach` 来保存你的会话.
+ 若要`debug Web`,试试`curl` 和 `curl -I` 或是 `wget` .
(我觉得debug Web的利器是`firebug`,`curl`和`wget`是用来抓网页的.)

+ 把 `HTML` 转成文本: `lynx -dump -stdin`
+ 如果你要处理`XML`,使用 `xmlstarlet`
+ 对于 `Amazon S3`, `s3cmd` 是一个很方便的命令(还有点不成熟)
+ 在`ssh`中,知道怎么来使用`ssh`隧道.通过 `-L` or `-D` (还有`-R`) ,翻墙神器.
+ 你还可以对你的 `ssh` 做点优化.比如,`.ssh/config` 包含着一些配置: 
避免链接被丢弃,链接新的`host`时不需要确认,转发认证,以前使用压缩(如果你要使用scp传文件):

```bash
TCPKeepAlive=yes
ServerAliveInterval=15
ServerAliveCountMax=6
StrictHostKeyChecking=no
Compression=yes
ForwardAgent=yes
```

+ 如果你输入了一行命令,但你改变注意了,又不想删除它,
因为你要在历史命令中找到它,但你也不想执行它.
那么,你可以按下 `Alt-#` ,于是这个命令关就被加了一个`#`字符,于是就被注释掉了.

### 数据处理

+ 了解 `sort` 和 `uniq` 命令 (包括 `uniq` 的 `-u` 和 `-d` 选项).
+ 了解用 `cut`, `paste`, 和 `join` 命令来操作文本文件.很多人忘了在`cut`前使用`join`.
+ 如果你知道怎么用`sort`/`uniq`来做集合交集,并集,差集能很大地促进你的工作效率.
假设有两个文本文件`a`和`b`已经被 `uniq`了,那么,用`sort`/`uniq`会是最快的方式,
无论这两个文件有多大(`sort`不会被内存所限,你甚至可以使用`-T`选项,如果你的`/tmp`目录很小)

```bash
cat a b | sort | uniq > c   # c is a union b 并集
cat a b | sort | uniq -d > c   # c is a intersect b 交集
cat a b b | sort | uniq -u > c   # c is set difference a - b 差集
```

+ 了解和字符集相关的命令行工具,包括排序和性能.
很多的Linux安装程序都会设置`LANG `或是其它和字符集相关的环境变量.
这些东西可能会让一些命令(如: `sort`)的执行性能慢N多倍
(注: 就算是你用`UTF-8`编码文本文件,你也可以很安全地使用`ASCII`来对其排序).
如果你想`Disable`那个`i18n` 并使用传统的基于`byte`的排序方法,
那就设置`export LC_ALL=C` (实际上,你可以把其放在 `.bashrc`).
如果这设置这个变量,你的`sort`命令很有可能会是错的.

+ 了解 `awk` 和 `sed` ,并用他们来做一些简单的数据修改操作.
例如: 求第`3`列的数字之和:  `awk '{ x += $3 } END { print x }'`.这可能会比Python快`3`倍,并比Python的代码少`3`倍.

+ 使用 `shuf` 来打乱一个文件中的行或是选择文件中一个随机的行.
+ 了解`sort`命令的选项.了解`key`是什么(`-t`和`-k`).具体说来,你可以使用`-k1,1`来对第一列排序,`-k1`来对全行排序.
+ `sort -s` 会很有用.
例如: 如果你想对两列排序,先对第二列,再对第一列,那么你可以这样:  `sort -k1,1 | sort -s -k2,2`

+ 我们知道,在`bash`命令行下,`Tab`键是用来做目录文件自动完成的事的.
但是如果你想输入一个`Tab`字符(比如: 你想在`sort -t`选项后输入`<tab>`字符),
你可以先按`Ctrl-V`,然后再按`Tab`键,就可以输入`<tab>`字符了.当然,你也可以使用`\t`.

+ 如果你想查看二进制文件,你可以使用`hd`命令(在CentOS下是`hexdump`命令),
如果你想编译二进制文件,你可以使用`bvi`命令(http://bvi.sourceforge.net/ 墙)

+ 另外,对于二进制文件,你可以使用`strings`(配合`grep`等)来查看二进制中的文本.
+ 对于文本文件转码,你可以试一下` iconv`.或是试试更强的 `uconv` 命令(这个命令支持更高级的Unicode编码)
+ 如果你要分隔一个大文件,你可以使用`split`命令(split by size)和`csplit`命令(split by a pattern).

### 系统调试

+ 如果你想知道磁盘,CPU,或网络状态,你可以使用 `iostat`, `netstat`, 
`top` (或更好的 `htop`), 还有 `dstat` 命令.你可以很快地知道你的系统发生了什么事.
关于这方面的命令,还有`iftop`, `iotop`等(参看<28个Unix/Linux的命令行神器>)

+ 要了解内存的状态,你可以使用`free`和`vmstat`命令.
具体来说,你需要注意 `"cached"` 的值,这个值是`Linux`内核占用的内存.还有`free`的值.

+ Java 系统监控有一个小的技巧是,你可以使用`kill -3 <pid>` 发一个`SIGQUIT`的信号给`JVM`,
可以把堆栈信息(包括垃圾回收的信息)`dump`到`stderr/logs`.

+ 使用 `mtr `会比使用 `traceroute` 要更容易定位一个网络问题.
+ 如果你要找到哪个`socket`或进程在使用网络带宽,你可以使用 `iftop` 或 `nethogs`.

+ `Apache`的一个叫 `ab` 的工具是一个很有用的,
用`quick-and-dirty`的方式来测试网站服务器的性能负载的工作.
如果你需要更为复杂的测试,你可以试试 `siege`.

+ 如果你要抓网络包的话,试试 `wireshark` 或 `tshark.`

+ 了解 `strace` 和 `ltrace`.这两个命令可以让你查看进程的系统调用,
这有助于你分析进程的hang在哪了,怎么crash和failed的.
你还可以用其来做性能`profile`,使用 `-c` 选项,你可以使用`-p`选项来`attach`上任意一个进程.

+ 了解用`ldd`命令来检查相关的动态链接库.注意: `ldd`的安全问题

+ 使用`gdb`来调试一个正在运行的进程或分析`core dump`文件. 
参看我写的 [GDB中应该知道的几个调试方法]

+ 学会到 `/proc` 目录中查看信息.
这是一个Linux内核运行时记录的整个操作系统的运行统计和信息,
比如:  `/proc/cpuinfo`, `/proc/xxx/cwd`, `/proc/xxx/exe`, `/proc/xxx/fd/`, `/proc/xxx/smaps`.

+ 如果你调试某个东西为什么出错时,`sar`命令会有用.它可以让你看看 CPU, 内存, 网络, 等的统计信息.
+ 使用 `dmesg` 来查看一些硬件或驱动程序的信息或问题.

### Ubuntu死机解决方法

[https://www.jianshu.com/p/36fb9eed82a3](https://www.jianshu.com/p/36fb9eed82a3)

+ 进入`TTY`终端. `Ctrl+Alt+F1`进入`TTY1`终端字符界面, 输入用户名和密码以登录,
也可能是`Ctrl+Alt+F2`,`Ctrl+Alt+F3`等等, 不同系统可能有点点区别.
输入`top`命令, 找到可能造成假死的进程, 用`kill`命令结束掉进程. 然后`Ctrl+Alt+F7`回到桌面

+ 直接注销用户: `Ctrl+Alt+F1`进入`TTY1`终端字符界面, 输入用户名和密码以登录.
然后执行以下的任意一个命令注销桌面重新登录.

```bash
sudo pkill Xorg
# 或者
sudo restart lightdm
```

+ 底层方法: 如果上面两种方法不成功, 那有可能是比较底层的软件出现问题. 可以试试 :**reisub 方法**.

说具体一点, 是一种系统请求, 直接交给内核处理.
键盘上一般都有一个键 `SysRq`, 和`PrtSc`(PrintScreen,截屏)在一个键位上, 这就是系统请求的键.
这个方法可以在死机的情况下安全地重启计算机, 数据不会丢失.

其实 `SysRq`是一种叫做 `系统请求` 的东西, 按住`Alt-PrtSc` 的时候就相当于按住了`SysRq`键,
这个时候输入的一切都会直接由 `Linux` 内核来处理, 它可以进行许多低级操作.
这个时候 `reisub` 中的每一个字母都是一个独立操作, 分别表示:

+ `r` : `unRaw` 将键盘控制从 `X Server` 那里抢回来
+ `e` : `tErminate` 给所有进程发送 `SIGTERM` 信号, 让它们自己解决善后
+ `i` : `kIll` 给所有进程发送 `SIGKILL` 信号, 强制他们马上关闭
+ `s` : `Sync` 将所有数据同步至磁盘
+ `u` : `Unmount` 将所有分区挂载为只读模式
+ `b` : `reBoot` 重启

如果某一天你的 `Linux` 死机了, 键盘不听使唤了, `Ctrl+Alt+F1` 已经没有任何反应, 该怎么办呢?
使用'魔法键': `Alt+SysRq + r,e,i,s,u,b`(确实很好背, 就是单词 `busier`的倒写).

首先, 你的系统要支持这个功能, 接下来就是操作:
同时按下`<Alt>+<SysRq>`不行, 只会蹦出来一个屏幕截图窗口. 所以, 真正的做法应该是:

+ 伸出你的左手, 同时按住`<Ctrl>+<Alt>`键, 下面几步时, 一直不松开
+ 右手先按一下`<SysRq>`or`PrcSc`, 左手别松开, 等`1`秒,
+ 右手按一下`R`, 左手别松开, 等`1`秒,
+ 右手按一下`E`, 左手别松开. 这时包括桌面在内, 所有程序都会终止, 你会看到一个黑乎乎的屏幕, 稍微等一段时间
+ 右手依次按下`I`,`S`,`U`,`B`,左手别松开. 每按一次都等那么几秒种, 你会发现每按一次, 屏幕上信息都会有所变化.
最后按下`B`时, 屏幕显示`reset`, 这时你的左手可以松开了, 等几秒钟, 计算机就会安全重启.

+ 开启`SysRq`功能: [Linux中的SysRq魔术键](https://blog.csdn.net/jasonchen_gbd/article/details/79080576).
幸运的是: `Ubuntu` 默认已经开启了这个功能.

首先要确保内核打开了`CONFIG_MAGIC_SYSRQ`配置项, 这样`SysRq`的底层处理才可用.
另外内核中有一个宏定义`SYSRQ_DEFAULT_ENABLE`, 表示系统默认情况下是否启用`SysRq`功能键.
当然, 不管这个值是不是`yes`, 你都可以通过`proc`文件系统来开启或关闭`SysRq`键:

```bash
# 查看当前SysRq是否被开启(0表示关闭):
cat /proc/sys/kernel/sysrq
# 开启SysRq:
echo 1 > /proc/sys/kernel/sysrq
# 也可以使用sysctl命令
sysctl -w kernel.sysrq=1
```

实际上`sysctl`这条命令就是通过修改`/proc/sys/kernel/sysrq`来生效的.
可以把`kernel.sysrq=1`设置到`/etc/sysctl.conf`中, 使`SysRq`在下次系统重启仍生效.
[Ryzen随机卡死问题](https://mrswolf.github.io/zh-cn/2019/05/24/manjaro%E8%B8%A9%E5%9D%91%E8%AE%B0/\)

```bash
# 编辑或新建99-sysctl.conf
sudo nano /etc/sysctl.d/99-sysctl.conf
# 添加以下内容, 按Alt+PrtSc+REISUB可以安全重启
kernel.sysrq = 1
```

`0`表示完全关闭`SysRq`, `1`表示使能`SysRq`的所有功能,
还可以设置成其他数字来选择开启部分功能, 可参考内核里的`Documentation/sysrq.txt`.

我们可以直接通过按键的方式或者通过写`/proc/sysrq-trigger`的方式来触发`SysRq`的操作.
`SysRq`支持的操作可参考下面的HELP输出:
即`SysRq键+一个字母`来触发一个操作, 例如`SysRq+t`打印所有任务的状态.

也可以不通过按键, 而是写`/proc/sysrq-trigger`的方式, 用法形如:

```bash
echo c > /proc/sysrq-trigger
```

即向`sysrq-trigger`写入相应的字母即可.
并且, 无论`/proc/sys/kernel/sysrq`是什么值, 这种方式都是可用的.

### transmission 屏蔽Ipv4

`cd ~/.config/transmission/blocklists`

创建空文件,文件名任意,写入
`Ipv4:0.0.0.0-255.255.255.255`

然后
`cd ~/.config/transmission`
`vim settings.json`

设置`ip`屏蔽生效
`"blocklist-enabled": true,`

## Unix/Linux命令行常用工具

[28个Unix/Linux的命令行神器](https://coolshell.cn/articles/7829.html)

***
`dstat` & `sar`

`iostat`, `vmstat`, `ifstat` 三合一的工具, 用来查看系统性能(我在<性能调优攻略>中提到过那三个xxstat工具).

官方网站: http://dag.wieers.com/rpm/packages/dstat/

你可以这样使用:

```bash
alias dstat='dstat -cdlmnpsy'
dstat screenshot
```

***
`slurm`

查看网络流量的一个工具
官方网站:   Simple Linux Utility for Resource Management

***
`screen`, `dtach`, `tmux`, `byobu`

你是不是经常需要 `SSH` 或者 `telent` 远程登录到 `Linux` 服务器? 你是不是经常为一些长时间运行的任务而头疼,比如系统备份,`ftp` 传输等等.
通常情况下我们都是为每一个这样的任务开一个远程终端窗口,因为他们执行的时间太长了.
必须等待它执行完毕,在此期间可不能关掉窗口或者断开连接,否则这个任务就会被杀掉,一切半途而废了.

`Screen` 是一个可以在多个进程之间多路复用一个物理终端的窗口管理器.
`Screen`中有会话的概念,用户可以在一个`screen`会话中创建多个`screen`窗口,在每一个`screen`窗口中就像操作一个真实的`telnet/SSH`连接窗口那样.
请参看IBM DeveloperWorks的这篇文章<使用 `screen` 管理你的远程会话>

`dtach` 是用来模拟`screen`的`detach`的功能的小工具,其可以让你随意地`attach`到各种会话上 .

`tmux` 是一个优秀的终端复用软件,类似`GNU Screen`,但来自于`OpenBSD`,采用`BSD`授权.
使用它最直观的好处就是,通过一个终端登录远程主机并运行`tmux`后,在其中可以开启多个控制台而无需再浪费多余的终端来连接这台远程主机;
当然其功能远不止于此.
与`screen`相比的优点: 可以横向和纵向分割窗口,且窗格可以自由移动和调整大小.
可在多个缓冲区进行复制和粘贴,支持跨窗口搜索; 非正常断线后不需重新`detach`; ... 有人说--与`tmux`相比,`screen`简直弱爆了.

`byobu`是`Ubuntu`开发的,在`Screen`的基础上进行包装,使其更加易用的一个工具.
最新的`Byobu`,已经是基于`Tmux`作为后端了.可通过`byobu-tmux`这个命令行前端来接受各种与`tmux`一模一样的参数来控制它.

***
`multitail`

`MultiTail`是个用来实现同时监控多个文档,类似`tail`命令的功能的软件.
他和`tail`的区别就是他会在控制台中打开多个窗口,这样使同时监控多个日志文档成为可能.
他还可以看`log`文件的统计,合并`log`文件,过滤`log`文件,分屏, ...

官网: http://www.vanheusden.com/multitail/

***
`tpp`

终端下的`PPT`,要是在某某大会上用这个演示`PPT`,就太TMD的Geek了.

官网: http://www.ngolde.de/tpp.html

***
`xargs` & `parallel`

Executes tasks from input (even multithread).

`xargs` 是一个比较古老的命令,有简单的并行功能,这个不说了.
对于`GNU parallel` ( online manpage )来说,它不仅能够处理本机上多执行绪,还能分散至远端电脑协助处理.
而使用`GNU parallel`前,要先确定本机有安装`GNU parallel/ssh/ rsync`,远端电脑也要安装`ssh`.

***
`duplicity` & `rsyncrypto`

`Duplicity`是使用`rsync`算法加密的高效率备份软件,`Duplicity`支持目录加密生产和格式上传到远程或本地文件服务器.

`rsyncrypto` 就是`rsync+encryption`.对于`rsync`的算法可参看酷壳的`rsync`核心算法.

***
`nethack` & `slash'em`

`NetHack`(Wiki),20年历史的古老电脑游戏.
没有声音,没有漂亮的界面,不过这个游戏真的很有意思.网上有个家伙说: 如果你一生只做一件事情,那么玩NetHack.
这句话很惹眼,但也让人觉得这个游戏很复杂不容易上手.其实,这个游戏很虽然很复杂,却容易上手.
虽然玩通关很难,但上手很容易.NetHack上有许多复杂的规则,`the DevTeam thinks of everything`(开发团队想到了所有的事情).
各种各样的怪物,各种各样的武器...,有许多`spoilers`文件来说明其规则.
除了每次开始随机生成的地图,每次玩游戏,你也都会碰到奇怪的事情: 因为喝了一种药水,变成了机器人;
因为踢坏了商店的门被要求高价赔偿;你的狗为你偷来了商店的东西... 这有点象人生,你不能完全了解这个世界,但你仍然可以选择自己的面对方式.

网上有许多文章所这是最好的电脑游戏或最好的电脑游戏之一.
也许是因为它开放的源代码让人赞赏,古老的历史让人宽容,复杂的规则让人敬畏.
虽然它不是当前流行的游戏,但它比任何一个当前流行的游戏都更有可能再经受20年的考验.

`Slash'EM` 也是一个基于`NetHack`的经典游戏.

***
`lftp`

利用`lftp`命令行`ftp`工具进行网站数据的增量备份,镜像,就像使用`rsync`一样.

***
`ack`

`ack`是一个`perl`脚本,是`grep`的一个可选替换品.其可以对匹配字符有高亮显示.
是为程序员专门设计的,默认递归搜索,缺省提供多种文件类型供选.

***
`calcurse` & `remind+wyrd`

`calcurse` 是一个命令行下的日历和日程软件.`remind+wyrd`也很类似.
关于日历,我不得不提一个Linux的`Cycle`日历,也是一个神器,呵呵.

***
`newsbeuter` & `rsstail`

`newsbeuter` 和 `rsstail` 是命令行下RSS的阅读工具.

***
`powertop`

做个环保的程序员,看看自己的电脑里哪些程序费电.`PowerTOP` 是一个让 Intel 平台的笔记本电脑节省电源的 `Linux` 工具.
此工具由 `Intel` 公司发布.它可以帮助用户找出那些耗电量大的程序,通过修复或者关闭那些应用程序或进程,从而为用户节省电源.

***
`htop` & `iotop`

`htop` 和 `iotop`  用来查看进程,内存和`IO`负载.

***
`ttyrec` & `ipbt`

`ttyrec` 是一个 `tty` 控制台录制程序,其所录制的数据文件可以使用与之配套的 `ttyplay` 播放.
不管是你在 `tty` 中的各种操作,还是在 `tty` 中耳熟能详的软件,都可进行录制.

`ipbt` 是一个用来回放 `ttyrec` 所录制的控制台输入过程的工具.

与此类似的还有 `Shelr` 和 `termrec`

***
`rsync`

通过`SSH`进行文件同步的经典工具(核心算法)

***
`mtr`

`MTR`--`traceroute 2.0`,其是把 `traceroute` 和 `ping` 集成在一块的一个小工具 用于诊断网络.

***
`socat` & `netpipes`

`socat`是一个多功能的网络工具,名字来由是`Socket CAT`,可以看作是`netcat`的N倍加强版.
`netpipes` 和 `socat` 一样,主要是用来在命令行来进行`socket`操作的命令,这样你就可以在`Shell`脚本下行进`socket`网络通讯了.

***
`iftop` & `iptraf`

`iftop` 和`iptraf`可以用来查看当前网络链接的一些流量情况.

***
`siege` & `tsung`

`Siege` 是一个压力测试和评测工具,设计用于WEB开发这运算应用在压力下的承受能力:
可以根据配置对一个WEB站点进行多用户的并发访问,记录每个用户所有请求过程的相应时间,并在一定数量的并发访问下重复进行.

`Tsung` 是一个压力测试工具,可以测试包括`HTTP`, `WebDAV`, `PostgreSQL`, `MySQL`, `LDAP`, and `XMPP/Jabber`等服务器.
针对 `HTTP` 测试,`Tsung` 支持 `HTTP 1.0/1.1` ,包含一个代理模式的会话记录,支持 `GET`,`POST` 和 `PUT` 以及 `DELETE` 方法,支持 `Cookie` 和基本的 `WWW` 认证,同时还支持 `SSL`.

参看: 十个免费的`Web`压力测试工具

***
`ledger`

`ledger` 一个命令行下记帐的小工具.

***
`taskwarrior`

`TaskWarrior` 是一个基于命令行的 `TODO` 列表管理工具.
主要功能包括: 标签,彩色表格输出,报表和图形,大量的命令,底层`API`,多用户文件锁等功能.

***
`curl`

`cURL`是一个利用URL语法在命令行下工作的文件传输工具,`1997`年首次发行.
它支持文件上传和下载,所以是综合传输工具,但按传统,习惯称`cURL`为下载工具.
`cURL`还包含了用于程序开发的`libcurl`.
`cURL`支持的通讯协议有`FTP`,`FTPS`,`HTTP`,`HTTPS`,`TFTP`,`SFTP`,`Gopher`,`SCP`,`Telnet`,`DICT`,`FILE`,`LDAP`,`LDAPS`,`IMAP`,`POP3`,`SMTP` 和 `RTSP`.

***
`rtorrent` & `aria2`

`rTorrent` 是一个非常简洁,优秀,非常轻量的`BT`客户端. 它使用了 `ncurses` 库以 `C++` 编写, 因此它完全基于文本并在终端中运行.
将 `rTorrent` 用在安装有 `GNU Screen` 和 `Secure Shell` 的低端系统上作为远程的 `BT` 客户端是非常理想的.

`aria2` 是 `Linux` 下一个不错的高速下载工具.
由于它具有分段下载引擎,所以支持从多个地址或者从一个地址的多个连接来下载同一个文件.这样自然就大大加快了文件的下载速度.
`aria2` 也具有断点续传功能,这使你随时能够恢复已经中断的文件下载.
除了支持一般的 `http(s)` 和 `ftp` 协议外,`aria2` 还支持 `BitTorrent` 协议.这意味着,你也可以使用 `aria2` 来下载 `torrent` 文件.

***
`ttytter` & `earthquake`

`TTYtter` 是一个 `Perl` 写的命令行上发 `Twitter` 的工具,可以进行所有其他平台客户端能进行的事情,当然,支持中文.
脚本控,CLI控,终端控,Perl控的最愛.

`Earthquake` 也是一个命令行上的`Twitter`客户端.

***
`vifm` & `ranger`

`Vifm` 基于 `ncurses` 的文件管理器,`DOS`风格,用键盘操作.

***
`Ranger` 用 `Python` 完成,默认为使用 `Vim` 风格的按键绑定,比如 `hjkl(上下左右)`,`dd(剪切)`,`yy(复制)`等等.
功能很全,扩展/可配置性也非常不错.
类似`MacOS X`下Finder(文件管理器)的多列文件管理方式.支持多标签页.实时预览文本文件和目录.

***
`cowsay` & `sl`

`cowsay`  不说了,如下所示,哈哈哈.还有`xcowsay`,你可以自己搜一搜.

`sl`是什么? `ls`? ,呵呵,你会经常把`ls` 打成`sl`吗? 如果是的话,这个东西可以让你娱乐一下,你会看到一辆火车呼啸而过~~,相当拉风.
你可以使用`sudo apt-get install sl` 安装.

最后,再介绍一个命令中`linuxlogo`,你可以使用 `sudo apt-get install linuxlogo`来安装.
然后,就可以使用`linuxlogo -L`, 来看一下各种`Linux`的`logo`了

## 终端复用器 tmux

[Tmux 使用教程](https://www.ruanyifeng.com/blog/2019/10/tmux.html)
[Linux 守护进程的启动方法](http://www.ruanyifeng.com/blog/2016/02/linux-daemon.html)

`Tmux` 是一个终端复用器`(terminal multiplexer)`, 非常有用,属于常用的开发工具.

***
会话与进程
命令行的典型使用方式是,打开一个终端窗口(terminal window,以下简称`窗口`),在里面输入命令.
用户与计算机的这种临时的交互,称为一次`会话`(session).
会话的一个重要特点是,窗口与其中启动的进程是连在一起的.打开窗口,会话开始; 关闭窗口,会话结束,会话内部的进程也会随之终止,不管有没有运行完.

一个典型的例子就是,`SSH` 登录远程计算机,打开一个远程窗口执行命令.
这时,网络突然断线,再次登录的时候,是找不回上一次执行的命令的.因为上一次 `SSH` 会话已经终止了,里面的进程也随之消失了.
为了解决这个问题,会话与窗口可以`解绑`: 窗口关闭时,会话并不终止,而是继续运行,等到以后需要的时候,再让会话`绑定`其他窗口.

### oh-my-zsh 插件 tmux

[ ohmyzsh/ohmyzsh ](https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tmux)

该插件为终端多路复用器`tmux`提供`aliases`.  要使用它, 请将`tmux`添加到`zshrc`文件中的`plugins`数组中.

```zshrc
plugins=(... tmux)
```

别名 命令  描述

+ `ta`:  `tmux attach -t`  连接新`tmux`会话到后台运行的会话
+ `tad`:  `tmux attach -d -t`  分离某个已命名会话
+ `ts`:  `tmux new-session -s` 创建新的命名会话
+ `tl`:  `tmux list-sessions`  显示所有正在运行的会话
+ `tksv`:  `tmux kill-server`  终止所有正运行的会话
+ `tkss`:  `tmux kill-session -t`  终止某个已命名会话
+ `tmux`:  `_zsh_tmux_plugin_run`  开始一个新的`tmux`会话

### 基本用法

`Tmux` 一般需要自己安装.

```bash
# Ubuntu 或 Debian
$ sudo apt-get install tmux
# CentOS 或 Fedora
$ sudo yum install tmux
# Mac
$ brew install tmux
```

***
启动与退出
安装完成后,键入`tmux`命令,就进入了 `Tmux` 窗口.
命令会启动 `Tmux` 窗口,底部有一个状态栏. 大概如下所示

```bash
[session1]0:zsh* ~~~ "OP7050" 17:12 05-2月-21
```

状态栏的左侧是窗口信息(编号和名称), `session1`是会话的名字, `0:zsh*`是窗口的编号和名称, `*`表示这个窗口被激活. 右侧是系统信息.
按下`Ctrl+d`或者输入`exit`命令,就可以退出 `Tmux` 窗口, 并退出会话. 按`ctrl+b d`会退出窗口, 但将会话转移到后台运行.

***
会话快捷键
下面是一些会话相关的快捷键.

+ `Ctrl+b d`: 分离当前会话.
+ `Ctrl+b s`: 列出所有会话, 交互式选取
+ `Ctrl+b $`: 重命名当前会话.
+ `Ctrl+b c`: 创建新窗口.
+ `Ctrl+b ,`: 重命名当前窗口.
+ `Ctrl+b p`: 前一个窗口.
+ `Ctrl+b n`: 后一个窗口.

+ `tmux kill-session` 关闭一个会话
+ `tmux kill-window  killw` -- 关闭一个窗口

还可以加上`-a`选项使用

***
前缀键

`Tmux` 窗口有大量的快捷键.所有快捷键都要通过前缀键唤起.默认的前缀键是`Ctrl+b`,即先按下`Ctrl+b`,快捷键才会生效.
举例来说,帮助命令的快捷键是`Ctrl+b ?`.它的用法是,在 `Tmux` 窗口中,先按下`Ctrl+b`,再按下`?`,就会显示帮助信息.
然后,按下 `ESC` 键或`q`键,就可以退出帮助.

### 会话管理

***
新建会话
第一个启动的 `Tmux` 窗口,编号是`0`,第二个窗口的编号是`1`,以此类推.这些窗口对应的会话,就是 `0` 号会话,`1` 号会话.
使用编号区分会话,不太直观,更好的方法是为会话起名.

```bash
$ tmux new -s <session-name>
```

上面命令新建一个指定名称的会话.

***
分离会话

在 `Tmux` 窗口中,按下`Ctrl+b d`或者输入`tmux detach`命令,就会将当前会话与窗口分离.

```bash
$ tmux detach
```

上面命令执行后,就会退出当前 `Tmux` 窗口,但是会话和里面的进程仍然在后台运行.
`tmux ls`命令可以查看当前所有的 `Tmux` 会话.

```bash
$ tmux ls
# or
$ tmux list-session
```

***
接入会话

`tmux attach`命令用于重新接入某个已存在的会话.

```bash
# 使用会话编号
$ tmux attach -t 0

# 使用会话名称
$ tmux attach -t <session-name>
```

***
杀死会话

`tmux kill-session`命令用于杀死某个会话.

```bash
# 使用会话编号
$ tmux kill-session -t 0
# 使用会话名称
$ tmux kill-session -t <session-name>
```

***
切换会话

`tmux switch`命令用于切换会话.

```bash
# 使用会话编号
$ tmux switch -t 0
# 使用会话名称
$ tmux switch -t <session-name>
```

***
重命名会话

`tmux rename-session`命令用于重命名会话.

```bash
$ tmux rename-session -t 0 <new-name>
```

上面命令将`0`号会话重命名.

### 最简操作流程

综上所述,以下是 `Tmux` 的最简操作流程.

+ 查看现存会话, `tl`:  `tmux list-sessions`
+ 新建会话,`tmux new-session -s name`:`ts`
+ 在 `Tmux` 窗口运行所需的程序.
+ 按下快捷键`Ctrl+b d`将会话分离,
+ 下次使用时,重新连接到会话,`tmux attach-session -t name`:`ta`
+ `tkss`:  `tmux kill-session -t`  终止某个已命名会话
+ `tksv`:  `tmux kill-server`  终止所有正运行的会话

+ `ctrl+b c`:创建一个新窗口
+ `ctrl+b p`: 切换到上一个窗口
+ `ctrl+b n`: 切换到下一个窗口
+ `ctrl+b <数字>`: 切换到指定编号的窗口
+ `ctrl+b w `: 从列表中选择窗口
+ `ctrl+b ,`: 窗口重命名

+ `ctrl+b %`:划分左右两个窗格
+ `ctrl+b "`: 划分上下两个窗格
+ `ctrl+b <方向键>`: 切换输入到其他窗格
+ `ctrl+b ;`: 切换输入到上一个窗格
+ `ctrl+b {`: 当前窗格与上一个交换位置
+ `ctrl+b }`: 与下一个窗格交换位置
+ `ctrl+b ctrl+o`: 所有窗格向前移动一个位置
+ `ctrl+b alt+o`: 所有窗格向后移动一个位置
+ `ctrl+b x`:  关闭当前窗格
+ `ctrl+b !`: 将当前窗格拆分为独立窗口
+ `ctrl+b z`: 将当前窗格全屏显示, 按两次恢复原状
+ `ctrl+b ctrl+<方向>`: 调整窗口大小
+ `ctrl+b space`: 重新排列当前窗格.
+ `ctrl+b p`: 显示窗口编号.
+ `ctrl+b t`: 显示时间
+ `ctrl+b i`: 显示当前窗格信息

***
`tmux`允许将命令绑定到大多数键(可以有前缀键).
指定键时, 大多数写法表示自己(例如, `A`至`Z`).  `Ctrl`键的前缀可以是`C-`或`^`, `Alt`(Meta)为`M-`.
此外, 还接受以下特殊键名:
`Up`, `Down`, `Left`, `Right`, `BSpace`, `BTab`, `DC (Delete)`, `End`, `Enter`, `Escape`, `F1` to `F12`, `Home`, `IC (Insert)`, `NPage/PageDown/PgDn`, `PPage/PageUp/PgUp`, `Space`, and `Tab`. 请注意, 要绑定`'`或`"`键, 必须使用引号, 例如:

```tmux
bind-key '"' split-window
bind-key "'" new-window
```

与键绑定相关的命令如下:

```tmux
bind-key [-nr] [-T key-table] key command [arguments]
(alias: bind)
```

将`key`与`command`绑定. 按键组合存放在`key table`中. 默认情况下(不带`-T`, 该键绑定在`prefix`键表中.
这个表中的按键, 要首先按下`前缀键`, 再按后面的组合. 例如, 默认绑定中, `c`绑定到`prefix`表中的`new-window`, 因此`Ctrl-b c`会创建一个新窗口).
`root`表中的组合没有前置键, 直接生效. 比如将`c`绑定到根表中的新窗口(实际中并不推荐), 这样直接按下`c`就会创建一个新窗口. `-n`是`-T root`的别名.
也可以把`key`绑定到自定义的``key table`中, 可以使用`switch-client -T `命令切换到自定义的`key table`.
`-r` flag 表示按键可以重复, 请参见`repeat-time`选项.

## Imagemagick 使用

[ImageMagick](https://imagemagick.org/)

使用`ImageMagick`创建,编辑,合成或转换位图图像.
它可以读取和写入各种格式(超过200种)的图像,包括PNG,JPEG,GIF,HEIC,TIFF,DPX,EXR,WebP,Postscript,PDF和SVG.
`ImageMagick`可以调整图像大小,翻转,镜像,旋转,变形,剪切和变换图像,调整图像颜色,应用各种特殊效果或绘制文本,线条,多边形,椭圆和贝塞尔曲线.
`ImageMagick`是免费软件,可以即用型二进制分发形式提供,也可以作为源代码提供,您可以在开放应用程序和专有应用程序中使用,复制,修改和分发它们.  它是在派生的`Apache 2.0`许可下分发的.
`ImageMagick`利用多个计算线程来提高性能,并且可以读取,处理或写入兆,千兆或兆像素的图像大小.

`ImageMagick`有一个单文件版本--`magick`,在`Linux`上完整的便携式应用程序,无需安装.  只需下载并运行.
`AppImage`需要`FUSE`和`libc`才能运行.  许多发行版都具有开箱即用的有效`FUSE`设置.  但是,如果对您不起作用,则必须手动安装和配置`FUSE`.

[FUSE](https://github.com/AppImage/AppImageKit/wiki/FUSE)

AppImage需要FUSE才能运行.  Filesystem in Userspace (FUSE)是一种允许非root用户安装文件系统的系统. 在 Ubuntu 上安装`FUSE`:

```bash
sudo apt install fuse
sudo modprobe fuse
sudo groupadd fuse

user="$(whoami)"
sudo usermod -a -G fuse $user
```

下载便携版`magick`程序之后,运行以下命令,将绘制一个示例的进度图

```bash
magick -size 320x90 canvas:none -stroke snow4 -size 1x90 -tile gradient:white-snow4 \
  -draw 'roundrectangle 16, 5, 304, 85 20,40' +tile -fill snow \
  -draw 'roundrectangle 264, 5, 304, 85  20,40' -tile gradient:chartreuse-green \
  -draw 'roundrectangle 16,  5, 180, 85  20,40' -tile gradient:chartreuse1-chartreuse3 \
  -draw 'roundrectangle 140, 5, 180, 85  20,40' +tile -fill none \
  -draw 'roundrectangle 264, 5, 304, 85 20,40' -strokewidth 2 \
  -draw 'roundrectangle 16, 5, 304, 85 20,40' \( +clone -background snow4 \
  -shadow 80x3+3+3 \) +swap -background none -layers merge \( +size -pointsize 90 \
  -strokewidth 1 -fill red label:'50 %' -trim +repage \( +clone -background firebrick3 \
  -shadow 80x3+3+3 \) +swap -background none -layers merge \) -insert 0 -gravity center \
  -append -background white -gravity center -extent 320x200 cylinder_shaded.png
```

### 几何参数

Image Geometry

许多命令行选项采用几何参数来指定诸如所需的图像宽度和高度以及其他尺寸数量之类的内容.
因为用户想要图像的最终尺寸,大小和位置有如此多的变化(并且因为ImageMagick希望提供它们),所以geometry参数可以采用多种形式. 我们将在本节中描述其中的许多内容.

采用某些几何参数形式的图像选项和设置包括以下内容. 请记住,其中一些的 parse 方式略有不同. 有关更多选项,请参见有关单个选项或设置的文档.

```magic
-adaptive-resize , -border , -borderwidth , -chop , -crop , -density , -extent , -extract , -frame , -geometry , -iconGeometry , -liquid-rescale , -page ,
-region , -repage , -resize , -sample , -scale , -shave , -splice , -thumbnail , -window
```

geometry参数可以采用下表中列出的任何形式. 这些将在表格后面的小节中详细介绍.
通常的形式是`size[offset]`,这意味着需要大小,而`offset`是可选的. 有时也可能是`[size]offset`的形式. 在任何情况下,几何参数中都不允许有空格.

格式和含义:
大小;  一般说明(实际行为可能因不同的选项和设置而异)

+ `scale%`;     高度和宽度均按指定的百分比缩放.
+ `scale-x%xscale-y%`   按指定百分比分别缩放的高度和宽度.  (只需要一个`％`符号. )
+ `width`       给定的宽度,自动选择的高度以保留宽高比.
+ `xheight`     给定的高度,自动选择的宽度以保留宽高比.
+ `widthxheight`        给定的高度和宽度的最大值,保留宽高比.
+ `widthxheight^`      给定的宽度和高度的最小值,保留宽高比.
+ `widthxheight!`       着重给出宽度和高度,忽略原始宽高比.
+ `widthxheight>`      缩小尺寸大于相应的`width`或`height`参数的图像.
+ `widthxheight<`   放大尺寸小于相应的`width`或`height`参数的图像.
+ `area@`       调整图像大小以具有以像素为单位的指定区域. 长宽比被保留.
+ `x:y` 在此,`x`和`y`表示纵横比(例如`3:2=1.5`).
+ `{size}{offset}`  指定偏移量(默认为`+0+0`). 下面的`{size}`是指以上任何形式.
+ `{size}{+-}x{+-}y`    水平和垂直偏移`x`和`y`,以像素为单位. 两者都需要`+-`号(sign). 偏移量受`-gravity`设置的影响.
偏移不受`％`或其他大小运算符的影响. 请注意,除了`center`选项,对于所有重力选项,正`X`和`Y`偏移都朝向图像中心. 对于`East`,`+X`是左边.
对于`South`,`+Y`是上. 对于`SouthEast`,`+X`是左,`+Y`是上. 对于`center`,使用常规的`X`和`Y`方向约定(`+X`向左,`+Y`向下).

### Output Filename

### 文件名引用

使用`embedded formatting character `输出图像列表.
假设我们的输出文件名是`image-%d.jpg`, 并且我们的输入的图像列表包括3张图像.  您可以期望输出以下图像文件:

```bash
image-0.jpg
image-1.jpg
image-2.jpg
```

或检索图像属性以修改图像文件名.  例如,

```bash
magick rose: -set filename:area '%wx%h' 'rose-%[filename:area].png'
```

用以下文件名写入图像:

```bash
rose-70x46.png
```

最后, 要将多个JPEG图像转换为单独的PDF页面, 请使用:

```bash
magick *.jpg +adjoin page-%d.pdf
```

### crop 剪切图像

[Imagemagick 命令行工具](https://imagemagick.org/script/command-line-tools.php)

`-crop geometry{@}{!}`

切出图像的一个或多个矩形区域.

有关几何参数的完整详细信息,请参见图像几何.

`geometry`参数的宽度和高度给出了裁剪后剩余图像的大小,偏移量中的`x`和`y`(如果存在)给出了裁剪图像相对于原始图像的左上角的位置.
如果要指定删除的数量,请改用`-shave`.

如果存在x和y偏移,则生成单个图像,该图像由裁剪区域中的像素组成. 偏移量指定: 裁减区域的左上角相对于图像的左上角,向右和向下.
可以通过`-gravity`改变默认的方向, `-gravity`可以设置为
`NorthEast`, `East`, or `SouthEast`
`SouthWest`, `South`, or `SouthEast `

如果省略`x`和`y`偏移,则生成指定几何形状的`tiles`(平铺,瓷砖),这些图块覆盖整​​个`input`图像.
如果指定的几何形状超出输入图像的尺寸,则最右边的图块和底部的图块将较小.

您可以在几何参数中添加`@`,以将图像平均划分为生成的图块数量.

通过将`!`惊叹号标志添加到几何参数,将适当设置裁剪后的图像虚拟画布页面大小和偏移,表现成`viewport`or `window`的效果.
这意味着画布页面大小设置为与您指定的大小完全相同,图像偏移设置为裁剪区域的相对左上角.

如果裁剪后的图像在其虚拟画布上`missed`了实际图像,则会返回一个特殊的单像素透明`missed`图像,并给出`crop missed`警告.

在裁剪图像之前,可能需要`+repage `图像,以确保将裁剪坐标框重定位到可见图像的左上角.
同样,您可能希望在裁剪后使用``+repage ``来删除遗留的页面偏移量. 当您要写入支持图像偏移量的图像格式(例如`PNG`)时,尤其如此.

### Inline Image Crop

有时在读取图像时裁剪图像很方便.  假设您有数百张要转换为PNG缩略图的大JPEG图像:

```bash
magick '*.jpg' -crop 120x120+10+5 thumbnail%03d.png
```

此处读取所有图像, 然后裁剪.  读取每个图像时, 裁剪图像的速度更快且资源占用更少:

```bash
magick '*.jpg[120x120+10+5]' thumbnail%03d.png
```

在`ubuntu-20`上使用上面的命令转换`pdf`可能会报错,

```bash
gs: symbol lookup error: /usr/lib/x86_64-linux-gnu/libgs.so.9: undefined symbol: cmsCreateContext
```

下面的帖子建议更新`ghostscript`
[Errors converting PDF with Imagemagick on Ubuntu 18](https://askubuntu.com/questions/1258602/errors-converting-pdf-with-imagemagick-on-ubuntu-18)

从 [ArtifexSoftware/ghostpdl ](https://github.com/ArtifexSoftware/ghostpdl) 下载`ghostscript`的最新版本, 比如`ghostscript-9.53.3-linux-x86_64`,
用它替换`/usr/bin/gs`即可.

### density

-density width
-density widthxheight

设置图像的水平和垂直分辨率, 以渲染到设备.

此选项指定在对光栅图像进行编码时要存储的图像分辨率, 或在将Postscript, PDF, WMF和SVG等矢量格式渲染(reading)到光栅图像时指定要存储的图像分辨率.
图像分辨率提供了渲染到输出设备或光栅图像时要应用的度量单位. 默认的度量单位是每英寸点数(`DPI`).  `-units` 选项可用于选择每厘米点数(不同单位).

默认分辨率是每英寸72点, 相当于每像素一个点(Macintosh和Postscript标准). 计算机屏幕通常每英寸72或96点, 而打印机通常每英寸支持150, 300, 600或1200点.
要确定显示器的分辨率, 请使用标尺测量屏幕的宽度(以英寸为单位), 然后除以水平像素数(在1024x768显示器上为1024).

如果文件格式支持, 则此选项可用于更新存储的图像分辨率. 请注意, Photoshop存储并从专有的嵌入式配置文件中获取图像分辨率.
如果未从图像中删除此配置文件, 则Photoshop将继续使用其以前的分辨率来处理图像, 而忽略标准文件头中指定的图像分辨率.

`-density`选项设置属性, 并且不会更改基础栅格图像. 它可用于通过调整应用于像素的比例来调整渲染尺寸以用于桌面发布.
要调整图像的大小, 以使其具有相同的大小, 但分辨率不同, 请使用`-resample`选项.
