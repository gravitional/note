# bash-1

[runoob-linux-tutorial](https://www.runoob.com/linux/linux-tutorial.html)

## 查看帮助

1. `命令 -- help`
1. `man 命令`

后者更加详细

首先帮助中尖括号`<>`和方括号`[]`以及省略号`...`的含义,

在方括号内的表达式`[` 和 `]`之间的字符是可选的(要去掉括号).
在尖括号内的表达式`<`和`>`之间的字符, 是必须替换的表达式(要去掉括号).

省略号表示该选项可以单个或多个

以`7z`的参数为例:

```bash
7z <command> [<switches>...] <archive_name> [<file_names>...]  [<@listfiles...>]
```

```bash
7z <命令> [[选项]...] <档案名称(压缩包名称)> [<被压缩的文件名>...] [<@文件列表(如txt文件中的文件列表)...>]
```

实例分析

```bash
7z a -tzip -p111 archive.zip txt.txt  txt.txt文件压缩为archive.zip, 压缩方式为zip 密码为111
7z x -tzip -p111 archive.zip            解压 密码为111
```

`a`为命令, 命令至少要有一个

`-t /-p`为选项(`switch`), 可选`0`个或多个, `zip`和`111`分别为`-t`和`-p`的参数, 中间不需要空格
选项(如`-r`)放前放后都行

`7z`的命令不区分大小写, 有些命令如`cp`要区分

示例

```bash
7z x archive.zip
#从压缩档案 archive.zip 中释放所有文件到当前文件夹.
7z x archive.zip -oc:\soft *.cpp
#从压缩档案 archive.zip 中释放 *.cpp 文件到 c:\soft 文件夹.
```

[linux命令行括号帮助含义](https://blog.csdn.net/yandaonan/article/details/56489513)

## alias

[Linux中使用 alias 来简化命令行输入](https://blog.csdn.net/kobejayandy/article/details/38710859)
[bash alias的使用](https://www.jianshu.com/p/63e91c67e39a)

`alias`(别名), 顾名思义, 其作用大概为命令的别名, 那么就可以它的好处就显而易见了. 可以用来缩短一些又长又难记的命令.

方法一:

+ `vim ~/.bashrc` ===> 打开bash的配置文件
+ 输入 `alias 新命令='旧命令 -选项/参数'` ===> 修改命令别名
+ `source ~/.bashrc` ===> 使得修改完的命令别名生效
+ 命令别名的增删改查都可以在`.bashrc`操作

方法二:

+ 增/改: 输入 `alias 新命令='旧命令 -选项/参数'`即完成了命令别名的添加.
+ 删: `unalias` 命令
+ 查: `alias` 命令

## 压缩/解压

### 7z

```bash
7z a win.configrc.7z -pxxxxx `@tom.configrc
7z x win.configrc.7z -pxxxxx
```

### tar

解包: `tar xvf FileName.tar`
打包: `tar cvf FileName.tar DirName`
(`tar` 仅仅是打包, 也有压缩的选项)
*****

### `.gz`

解压1: `gunzip FileName.gz`
解压2: `gzip -d FileName.gz`
压缩: `gzip FileName`

`.tar.gz` 和 `.tgz`
解压: `tar zxvf FileName.tar.gz`
压缩: `tar zcvf FileName.tar.gz DirName`
*****

### `.bz2`

解压1: `bzip2 -d FileName.bz2`
解压2: `bunzip2 FileName.bz2`
压缩:  `bzip2 -z FileName`

`.tar.bz2`
解压: `tar jxvf FileName.tar.bz2`
压缩: `tar jcvf FileName.tar.bz2 DirName`
*****

### `.bz`

解压1: `bzip2 -d FileName.bz`
解压2: `bunzip2 FileName.bz`
压缩: 未知

`.tar.bz`
解压: `tar jxvf FileName.tar.bz`
压缩: 未知
*****

### `.Z`

解压: `uncompress FileName.Z`
压缩: `compress FileName`
`.tar.Z`

解压: `tar Zxvf FileName.tar.Z`
压缩: `tar Zcvf FileName.tar.Z DirName`
*****

### `.zip`

解压: `unzip FileName.zip`
压缩: `zip FileName.zip DirName`
*****

### `.rar`

解压: `rar x FileName.rar`
压缩: `rar a FileName.rar DirName`
*****

### `.lha`

解压: `lha -e FileName.lha`
压缩: `lha -a FileName.lha FileName`
*****

### `.rpm`

解包: `rpm2cpio FileName.rpm | cpio -div`
*****

### `.deb`

解包: `ar p FileName.deb data.tar.gz | tar zxf -`
*****

### `sEx`

`.tar` `.tgz` `.tar.gz` `.tar.Z` `.tar.bz` `.tar.bz2` `.zip` `.cpio` `.rpm` `.deb` `.slp` `.arj` `.rar` `.ace` `.lha` `.lzh` `.lzx` `.lzs` `.arc` `.sda` `.sfx` `.lnx` `.zoo` `.cab` `.kar` `.cpt` `.pit` `.sit` `.sea`

解压: `sEx x FileName.*`
压缩: `sEx a FileName.* FileName`

`sEx`只是调用相关程序, 本身并无压缩, 解压功能, 请注意!

### gzip 命令

减少文件大小有两个明显的好处, 一是可以减少存储空间, 二是通过网络传输文件时, 可以减少传输的时间.
`gzip` 是在 `Linux` 系统中经常使用的一个对文件进行压缩和解压缩的命令, 既方便又好用.

语法: `gzip [选项] <压缩(解压缩)的文件名>`

该命令的各选项含义如下:

+ `-c` 将输出写到标准输出上, 并保留原有文件.
+ `-d` 将压缩文件解压.
+ `-l` 对每个压缩文件, 显示下列字段: 压缩文件的大小; 未压缩文件的大小; 压缩比; 未压缩文件的名字
+ `-r` 递归式地查找指定目录并压缩其中的所有文件或者是解压缩.
+ `-t` 测试, 检查压缩文件是否完整.
+ `-v` 对每一个压缩和解压的文件, 显示文件名和压缩比.
+ `-num` 用指定的数字 `num` 调整压缩的速度, `-1` 或 `--fast` 表示最快压缩方法(低压缩比);`-9` 或`--best`表示最慢压缩方法(高压缩比).系统缺省值为`6`.

指令实例:

+ `gzip *%` 把当前目录下的每个文件压缩成`.gz`文件.
+ `gzip -dv *` 把当前目录下每个压缩的文件解压, 并列出详细的信息.
+ `gzip -l *` 详细显示例1中每个压缩的文件的信息, 并不解压.
+ `gzip usr.tar` 压缩`tar`备份文件`usr.tar`, 此时压缩文件的扩展名为`.tar.gz`.

[Linux/Ubuntu下解压命令](https://blog.csdn.net/u013063153/article/details/53894711)

## Linux 文件与目录管理

我们知道 `Linux` 的目录结构为树状结构, 最顶级的目录为根目录 `/`.
其他目录可以通过挂载添加到树中, 可以通过解除挂载移除.
在开始本教程前我们需要先知道什么是绝对路径与相对路径.

绝对路径:
路径的写法, 由根目录 `/` 写起, 例如:  `/usr/share/doc` 这个目录.

相对路径:
路径的写法, 不是由 `/` 写起, 例如由 `/usr/share/doc` 要到 `/usr/share/man` 底下时, 可以写成: `cd ../man` 这就是相对路径的写法啦!

### 处理目录的常用命令

接下来我们就来看几个常见的处理目录的命令吧:

+ `ls` : 列出目录
+ `cd` : 切换目录
+ `pwd` : 显示目前的目录
+ `mkdir` : 创建一个新的目录
+ `rmdir` : 删除一个空的目录
+ `cp` : 复制文件或目录
+ `rm` : 移除文件或目录
+ `mv` : 移动文件与目录, 或修改文件与目录的名称

你可以使用 `man [命令]` 来查看各个命令的使用文档, 如 : `man cp`.

### ls (列出目录)

在`Linux`系统当中,  `ls` 命令可能是最常被运行的.

语法:

```bash
[root@www ~]# ls [-aAdfFhilnrRSt] 目录名称
[root@www ~]# ls [--color={never,auto,always}] 目录名称
[root@www ~]# ls [--full-time] 目录名称
```

选项与参数:

`-a` : 全部的文件, 连同隐藏档( 开头为 `.` 的文件) 一起列出来(常用)
`-d` : 仅列出目录本身, 而不是列出目录内的文件数据(常用)
`-l` : 长数据串列出, 包含文件的属性与权限等等数据; (常用)

将家目录下的所有文件列出来(含属性与隐藏档)

```bash
[root@www ~]# ls -al ~
```

### cd (切换目录)

`cd`是`Change Directory`的缩写, 这是用来变换工作目录的命令.

语法:

```bash
cd [相对路径或绝对路径]
```

```bash
#使用 mkdir 命令创建 runoob 目录
[root@www ~]# mkdir runoob

#使用绝对路径切换到 runoob 目录
[root@www ~]# cd /root/runoob/

#使用相对路径切换到 runoob 目录
[root@www ~]# cd ./runoob/

# 表示回到自己的家目录, 亦即是 /root 这个目录
[root@www runoob]# cd ~

# 到目前的上一级目录, 即 /root 的上一级目录;
[root@www ~]# cd ..
```

### pwd (显示目前所在的目录)

`pwd` 是 `Print Working Directory` 的缩写, 也就是"显示目前所在目录".

```bash
[root@www ~]# pwd [-P]
```

选项与参数:

`-P` : 显示出确实的路径, 而非使用链接 (link) 路径.

实例: 单纯显示出目前的工作目录:

```bash
[root@www ~]# pwd
/root   <== 显示出目录

实例:显示出实际的工作目录, 而非链接本身的目录名.

[root@www ~]# cd /var/mail   <==注意, /var/mail是一个链接
[root@www mail]# pwd
/var/mail         <==列出目前的工作目录
[root@www mail]# pwd -P
/var/spool/mail   <==怎么回事?
[root@www mail]# ls -ld /var/mail
lrwxrwxrwx 1 root root 10 Sep  4 17:54 /var/mail -> spool/mail
# 看到这里知道为啥了吧? 因为 /var/mail 是链接档, 链接到 /var/spool/mail
# 所以, 加上 pwd -P 的选项后, 会显示完整路径
```

### mkdir (创建新目录)

如果想要创建新的目录的话, 那么就使用mkdir (make directory)吧.

语法:

```bash
mkdir [-mp] 目录名称
```

选项与参数:

`-m` : 配置文件的权限, 而不是默认权限 (umask)
`-p` : 帮助你直接将所需要的目录(包含上一级目录)递归创建

实例: 请到`/tmp`底下尝试创建数个新目录看看:

```bash
[root@www ~]# cd /tmp
[root@www tmp]# mkdir test    <==创建名为 test 的新目录
[root@www tmp]# mkdir test1/test2/test3/test4
mkdir: cannot create directory `test1/test2/test3/test4`:
No such file or directory       <== 没办法直接创建此目录

[root@www tmp]# mkdir -p test1/test2/test3/test4

# 加了这个 -p 的选项, 可以自行帮你创建多层目录

```

实例: 创建权限为 `rwx--x--x` 的目录.

```bash
[root@www tmp]# mkdir -m 711 test2
[root@www tmp]# ls -l
drwxr-xr-x  3 root  root 4096 Jul 18 12:50 test
drwxr-xr-x  3 root  root 4096 Jul 18 12:53 test1
drwx--x--x  2 root  root 4096 Jul 18 12:54 test2
```

上面的权限部分, 如果没有加上 `-m` 来强制配置属性, 系统会使用默认属性.

如果我们使用 `-m` , 如上例我们给予 `-m 711` 来给予新的目录 `drwx--x--x` 的权限.

### rmdir (删除空的目录)

语法:

```bash
rmdir [-p] 目录名称
```

选项与参数:

`-p` : 连同上一级"空的"目录也一起删除

删除 `runoob` 目录

```bash
[root@www tmp]# rmdir runoob/

将 mkdir 实例中创建的目录(/tmp 底下)删除掉!

[root@www tmp]# ls -l   <==看看有多少目录存在?
drwxr-xr-x  3 root  root 4096 Jul 18 12:50 test
drwxr-xr-x  3 root  root 4096 Jul 18 12:53 test1
drwx--x--x  2 root  root 4096 Jul 18 12:54 test2
[root@www tmp]# rmdir test   <==可直接删除掉, 没问题
[root@www tmp]# rmdir test1  <==因为尚有内容, 所以无法删除!
rmdir: `test1`: Directory not empty
[root@www tmp]# rmdir -p test1/test2/test3/test4
[root@www tmp]# ls -l      <==中test与test1不见了!
drwx--x--x  2 root  root 4096 Jul 18 12:54 test2
```

利用 `-p` 这个选项, 可以将 `test1/test2/test3/test4` 一次删除.

不过要注意的是, 这个 `rmdir` 仅能删除空的目录, 你可以使用 `rm` 命令来删除非空目录.

### cp (复制文件或目录)

`cp` 即拷贝文件和目录.

语法:

```bash
[root@www ~]# cp [-adfilprsu] 来源(source) 目标(destination)
[root@www ~]# cp [options] source1 source2 source3 .... directory
```

选项与参数:

`-a` : 相当于 `-pdr` 的意思, 至于 `pdr` 请参考下列说明; (常用)
`-d` : 若来源档为链接的属性(`link file`), 则复制链接档属性而非文件本身;
`-f` : 强制(`force`)的意思, 若目标文件已经存在且无法开启, 则移除后再尝试一次;
`-i` : 若目标档(`destination`)已经存在时, 在覆盖时会先询问动作的进行(常用)
`-l` : 进行硬式链接(`hard link`)的链接档创建, 而非复制文件本身;
`-p` : 连同文件的属性一起复制过去, 而非使用默认属性(备份常用);
`-r` : 递归持续复制,`recursive`, 用于目录的复制行为; (常用)
`-s` : 复制成为符号链接档 (`symbolic link`), 即"捷径"文件;
`-u` : 若 `destination` 比 `source` 旧才升级 `destination`

用 `root` 身份, 将 `root` 目录下的 `.bashrc` 复制到 `/tmp` 下, 并命名为`bashrc`

```bash
[root@www ~]# cp ~/.bashrc /tmp/bashrc
[root@www ~]# cp -i ~/.bashrc /tmp/bashrc
cp: overwrite `/tmp/bashrc`? n  <==n不覆盖, y为覆盖
```

### rm (移除文件或目录)

语法:

```bash
 rm [-fir] 文件或目录
```

选项与参数:

`-f` : 就是 `force` 的意思, 忽略不存在的文件, 不会出现警告信息;
`-i` : 互动模式, 在删除前会询问使用者是否动作
`-r` : 递归删除, 最常用在目录的删除, 这是非常危险的选项!

将刚刚在 `cp` 的实例中创建的 `bashrc` 删除掉:

```bash
[root@www tmp]# rm -i bashrc
rm: remove regular file `bashrc'? y
```

如果加上 `-i` 的选项就会主动询问, 避免你错误删除.

### mv (移动文件与目录, 或修改名称)

语法:

```bash
[root@www ~]# mv [-fiu] source destination
[root@www ~]# mv [options] source1 source2 source3 .... directory
```

选项与参数:

`-f` :  `force` 强制的意思, 如果目标文件已经存在, 不会询问而直接覆盖;
`-i` :  若目标文件 (`destination`) 已经存在时, 就会询问是否覆盖.
`-u` :  若目标文件已经存在, 且 `source` 比较新, 才会升级 (`update`)

复制一文件, 创建一目录, 将文件移动到目录中

```bash
[root@www ~]# cd /tmp
[root@www tmp]# cp ~/.bashrc bashrc
[root@www tmp]# mkdir mvtest
[root@www tmp]# mv bashrc mvtest
```

将刚刚的目录名称更名为 `mvtest2`

```bash
[root@www tmp]# mv mvtest mvtest2
```

### Linux 文件内容查看

`Linux` 系统中使用以下命令来查看文件的内容:

+ `cat`  由第一行开始显示文件内容,`catenate`
+ `tac`  从最后一行开始显示, 可以看出 `tac` 是 `cat` 的倒着写
+ `nl`   显示的时候, 顺便输出行号
+ `more` 一页一页的显示文件内容
+ `less` 与 `more` 类似, 但是比 `more` 更好的是, 他可以往前翻页!
+ `head` 只看头几行
+ `tail` 只看尾巴几行

你可以使用 `man [命令]`来查看各个命令的使用文档, 如 : `man cp`.

#### cat

由第一行开始显示文件内容

语法:

```bash
cat [-AbEnTv]
```

选项与参数:

`-A` : 相当于 `-vET` 的整合选项, 可列出一些特殊字符而不是空白而已;
`-b` : 列出行号, 仅针对非空白行做行号显示, 空白行不标行号!
`-E` : 将结尾的断行字节 `$` 显示出来;
`-n` : 列印出行号, 连同空白行也会有行号, 与 `-b` 的选项不同;
`-T` : 将 [tab] 按键以 `^I` 显示出来;
`-v` : 列出一些看不出来的特殊字符

检看 `/etc/issue` 这个文件的内容:

```bash
[root@www ~]# cat /etc/issue
CentOS release 6.4 (Final)
Kernel \r on an \m
```

`tac`

`tac`与`cat`命令刚好相反, 文件内容从最后一行开始显示, 可以看出 `tac` 是 `cat` 的倒写.如:

```bash
[root@www ~]# tac /etc/issue

Kernel \r on an \m
CentOS release 6.4 (Final)
```

#### nl 显示行号

numbered list
语法:

```bash
nl [-bnw] 文件
```

选项与参数:

`-b` : 指定行号指定的方式, 主要有两种:
`-b a` : 表示不论是否为空行, 也同样列出行号(类似 `cat -n`);
`-b t` : 如果有空行, 空的那一行不要列出行号(默认值);
`-n` : 列出行号表示的方法, 主要有三种:
`-n ln` : 行号在荧幕的最左方显示;
`-n rn` : 行号在自己栏位的最右方显示, 且不加 0 ;
`-n rz` : 行号在自己栏位的最右方显示, 且加 0 ;
`-w` : 行号栏位的占用的位数.

实例一: 用 `nl` 列出 `/etc/issue` 的内容

```bash
[root@www ~]# nl /etc/issue
     1  CentOS release 6.4 (Final)
     2  Kernel \r on an \m
```

#### more

一页一页翻动

```bash
[root@www ~]# more /etc/man_db.config
#
# Generated automatically from man.conf.in by the
# configure script.
....(中间省略)....
--More--(28%)  <== 重点在这一行喔! 你的光标会在这里等待命令
```

在 `more` 这个程序的运行过程中, 你有几个按键可以按:

`空白键 (space)` : 代表向下翻一页;
`Enter`        : 代表向下翻一行;
`/字串`        : 代表在这个显示的内容当中, 向下搜寻"字串"这个关键字;
`:f`          : 立刻显示出档名以及目前显示的行数;
`q`          : 代表立刻离开`more` , 不再显示该文件内容.
`b 或 [ctrl]-b` : 代表往回翻页, 不过这动作只对文件有用, 对管线无用.

#### less

一页一页翻动, 以下实例输出`/etc/man.config`文件的内容:

```bash
[root@www ~]# less /etc/man.config
#
# Generated automatically from man.conf.in by the
....(中间省略)....
:   <== 这里可以等待你输入命令
```

`less`运行时可以输入的命令有:

+ `空白键`    : 向下翻动一页;
+ `[pagedown]` : 向下翻动一页;
+ `[pageup]`  : 向上翻动一页;
+ `/字串`     : 向下搜寻"字串"的功能;
+ `?字串`     : 向上搜寻"字串"的功能;
+ `n`        : 重复前一个搜寻 (与 `/` 或 `?` 有关! )
+ `N`        : 反向的重复前一个搜寻 (与 `/` 或 `?` 有关! )
+ `q`        : 离开 `less` 这个程序;

#### head

取出文件前面几行

语法:

```bash
head [-n number] 文件
```

选项与参数:

`-n` : 后面接数字, 代表显示几行的意思

```bash
[root@www ~]# head /etc/man.config
# 默认的情况中, 显示前面 10 行! 若要显示前 20 行, 需要这样
[root@www ~]# head -n 20 /etc/man.config
```

#### tail

取出文件后面几行

语法:

```bash
tail [-n number] 文件
```

选项与参数:

`-n` : 后面接数字, 代表显示几行的意思
`-f` : 表示持续侦测后面所接的档名, 要等到按下`[ctrl]-c`才会结束`tail`的侦测

```bash
[root@www ~]# tail /etc/man.config
# 默认的情况中, 显示最后的十行,若要显示最后的 20 行, 需要这样:
[root@www ~]# tail -n 20 /etc/man.config
```

## 安装 Windows 字体

或者, 将所有的 `Windows` 字体复制到 /usr/share/fonts 目录下并使用一下命令安装字体:

```bash
mkdir /usr/share/fonts/WindowsFonts
cp /Windowsdrive/Windows/Fonts/* /usr/share/fonts/WindowsFonts
chmod 755 /usr/share/fonts/WindowsFonts/*
```

最后, 使用命令行重新生成`fontconfig` 缓存:

```bash
fc-cache
```

### vscode 调整字体

"editor.fontFamily": "Fira Code, Source Code Pro, Noto Sans CJK SC, monospace"

这个是用的谷歌的开源字体思源黑体.

`makrdown` 的话, 还有个"`markdown.preview.fontFamily`"设置.

>但是也许是我的版本较高, 版本`1.23.1`, 我的里面设置是这样子的:
>`"editor.fontFamily": "Consolas, Dengxian"`,
>英文字体用了`Consolas`, 如果不适用的字体就用`Dengxian`

我的设置

```json
"editor.fontFamily": "Fira Code Retina, Microsoft YaHei",
"editor.fontLigatures": true,
```

`makrdown` 的话, 还有个`"markdown.preview.fontFamily"`设置.

[refer1](https://zhuanlan.zhihu.com/p/40434062)
[refer2](https://segmentfault.com/a/1190000004168301)
[refer3][(https://www.v2ex.com/t/453862)

### oh-my-zsh 中定义的别名

为了方便使用, oh-my-zsh 定义了许多别名, 下面是其中一部分.

```bashrc
-='cd -'
...=../..
....=../../..
.....=../../../..
......=../../../../..
1='cd -'
2='cd -2'
3='cd -3'
4='cd -4'
5='cd -5'
6='cd -6'
7='cd -7'
8='cd -8'
9='cd -9'
_='sudo '
afind='ack -il'
diff='diff --color'
egrep='egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
fgrep='fgrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
g=git
ga='git add'
gaa='git add --all'
gam='git am'
gama='git am --abort'
gamc='git am --continue'
gams='git am --skip'
gamscp='git am --show-current-patch'
gap='git apply'
gapa='git add --patch'
gapt='git apply --3way'
gau='git add --update'
gav='git add --verbose'
gb='git branch'
gbD='git branch -D'
gba='git branch -a'
gbd='git branch -d'
gbda='git branch --no-color --merged | command grep -vE "^(\+|\*|\s*($(git_main_branch)|development|develop|devel|dev)\s*$)" | command xargs -n 1 git branch -d'
gbl='git blame -b -w'
gbnm='git branch --no-merged'
gbr='git branch --remote'
gbs='git bisect'
gbsb='git bisect bad'
gbsg='git bisect good'
gbsr='git bisect reset'
gbss='git bisect start'
gc='git commit -v'
'gc!'='git commit -v --amend'
gca='git commit -v -a'
'gca!'='git commit -v -a --amend'
gcam='git commit -a -m'
'gcan!'='git commit -v -a --no-edit --amend'
'gcans!'='git commit -v -a -s --no-edit --amend'
gcb='git checkout -b'
gcd='git checkout develop'
gcf='git config --list'
gcl='git clone --recurse-submodules'
gclean='git clean -id'
gcm='git checkout $(git_main_branch)'
gcmsg='git commit -m'
'gcn!'='git commit -v --no-edit --amend'
gco='git checkout'
gcount='git shortlog -sn'
gcp='git cherry-pick'
gcpa='git cherry-pick --abort'
gcpc='git cherry-pick --continue'
gcs='git commit -S'
gcsm='git commit -s -m'
gd='git diff'
gdca='git diff --cached'
gdct='git describe --tags $(git rev-list --tags --max-count=1)'
gdcw='git diff --cached --word-diff'
gds='git diff --staged'
gdt='git diff-tree --no-commit-id --name-only -r'
gdw='git diff --word-diff'
gf='git fetch'
gfa='git fetch --all --prune --jobs=10'
gfg='git ls-files | grep'
gfo='git fetch origin'
gg='git gui citool'
gga='git gui citool --amend'
ggpull='git pull origin "$(git_current_branch)"'
ggpur=ggu
ggpush='git push origin "$(git_current_branch)"'
ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'
ghh='git help'
gignore='git update-index --assume-unchanged'
gignored='git ls-files -v | grep "^[[:lower:]]"'
git-svn-dcommit-push='git svn dcommit && git push github $(git_main_branch):svntrunk'
gk='\gitk --all --branches'
gke='\gitk --all $(git log -g --pretty=%h)'
gl='git pull'
glg='git log --stat'
glgg='git log --graph'
glgga='git log --graph --decorate --all'
glgm='git log --graph --max-count=10'
glgp='git log --stat -p'
glo='git log --oneline --decorate'
globurl='noglob urlglobber '
glod='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'
glods='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'' --date=short'
glog='git log --oneline --decorate --graph'
gloga='git log --oneline --decorate --graph --all'
glol='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'
glola='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --all'
glols='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --stat'
glp=_git_log_prettily
glum='git pull upstream $(git_main_branch)'
gm='git merge'
gma='git merge --abort'
gmom='git merge origin/$(git_main_branch)'
gmt='git mergetool --no-prompt'
gmtvim='git mergetool --no-prompt --tool=vimdiff'
gmum='git merge upstream/$(git_main_branch)'
gp='git push'
gpd='git push --dry-run'
gpf='git push --force-with-lease'
'gpf!'='git push --force'
gpoat='git push origin --all && git push origin --tags'
gpristine='git reset --hard && git clean -dffx'
gpsup='git push --set-upstream origin $(git_current_branch)'
gpu='git push upstream'
gpv='git push -v'
gr='git remote'
gra='git remote add'
grb='git rebase'
grba='git rebase --abort'
grbc='git rebase --continue'
grbd='git rebase develop'
grbi='git rebase -i'
grbm='git rebase $(git_main_branch)'
grbs='git rebase --skip'
grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
grev='git revert'
grh='git reset'
grhh='git reset --hard'
grm='git rm'
grmc='git rm --cached'
grmv='git remote rename'
groh='git reset origin/$(git_current_branch) --hard'
grrm='git remote remove'
grs='git restore'
grset='git remote set-url'
grss='git restore --source'
grt='cd "$(git rev-parse --show-toplevel || echo .)"'
gru='git reset --'
grup='git remote update'
grv='git remote -v'
gsb='git status -sb'
gsd='git svn dcommit'
gsh='git show'
gsi='git submodule init'
gsps='git show --pretty=short --show-signature'
gsr='git svn rebase'
gss='git status -s'
gst='git status'
gsta='git stash push'
gstaa='git stash apply'
gstall='git stash --all'
gstc='git stash clear'
gstd='git stash drop'
gstl='git stash list'
gstp='git stash pop'
gsts='git stash show --text'
gstu='git stash --include-untracked'
gsu='git submodule update'
gsw='git switch'
gswc='git switch -c'
gtl='gtl(){ git tag --sort=-v:refname -n -l "${1}*" }; noglob gtl'
gts='git tag -s'
gtv='git tag | sort -V'
gunignore='git update-index --no-assume-unchanged'
gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
gup='git pull --rebase'
gupa='git pull --rebase --autostash'
gupav='git pull --rebase --autostash -v'
gupv='git pull --rebase -v'
gwch='git whatchanged -p --abbrev-commit --pretty=medium'
gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify --no-gpg-sign -m "--wip-- [skip ci]"'
history=omz_history
l='ls -lah'
la='ls -lAh'
ll='ls -lh'
ls='ls --color=tty'
lsa='ls -lah'
md='mkdir -p'
mma='mathematica -singleLaunch'
rd=rmdir
which-command=whence
```
