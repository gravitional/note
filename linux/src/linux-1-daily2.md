# linux-daily2

## curl wget

curl -fsSL https://www.preining.info/rsa.asc | tlmgr key add -

`-f, --fail`
(HTTP)服务器错误时`静默失败`(没有输出).  这样做主要是为了, 使脚本等更好地处理失败的尝试.
在正常情况下, 当`HTTP`服务器无法交付文档时, 它将返回`HTML文档`, 说明(通常还会描述原因及更多).
该`flag`将阻止`curl`输出该错误并返回`error 22`.

此方法不是`fail-safe`的, 并且有时会漏入不成功的响应代码, 尤其是在涉及验证时(response codes 401 and 407).

`-s, --silent`

静音或安静模式.  不显示进度表或错误消息.  使`Curl`静音.  仍会输出您要求的数据, 甚至到终端/标准输出, 除非您将其重定向.
除此选项外, 还可以使用`-S`, `--show-error`禁用进度表, 但仍显示错误消息.
另请参见`-v`, `--verbose` and `--stderr`.

`-S, --show-error`
与`-s`, `--silent`一起使用时, 如果`curl`失败, 它将使`curl`显示一条错误消息.

`-L, --location`

(HTTP)如果服务器报告请求的页面已移动到其他位置(由`Location:  header`  and  a `3XX`响应代码), 此选项将使`curl`在新位置上重做请求.
如果与`-i`, `--include`或`-l`,`--head`一起使用, 将显示所有请求页面的`headers`. 使用身份验证时, curl仅将其凭据发送到初始主机.
如果重定向将curl转移到其他主机, 它无法截获`user+password`.
另请参阅`--location-trusted`查看如何修改这项设置.
您可以使用`--max-redirs`选项来限制要遵循的重定向数量.

当curl跟随重定向并且请求不是简单的`GET`(例如`POST`或`PUT`)时, it will do the following request with a GET:
如果`HTTP`响应是`301`, `302`或`303`. 如果响应代码是任何其他`3xx`代码, curl will resend the following request using the same unmodified method.

您可以通过使用专用的选项`--post301,` `--post302` and `--post303`, 来告诉curl 对于`30x`response, 不要将 `non-GET` request method 更改为`GET`.

## 查看ip地址

使用`ip`命令

```bash
ip addr show
ip link show #查看 MAC 地址
```

## 安装额外解码器

如果你刚刚安装了 Ubuntu 或其他 Ubuntu 特色版本 如 Kubuntu,Lubuntu 等,你会注意到系统无法播放某些音频或视频文件.

对于视频文件,你可以在 Ubuntu 上安装 `VLC`.`VLC` 是 Linux 上的最佳视频播放器之一,它几乎可以播放任何视频文件格式.但你仍然会遇到无法播放音频和 `flash` 的麻烦.

好消息是 Ubuntu 提供了一个软件包来安装所有基本的媒体编解码器:`ubuntu-restricted-extras`.

ubuntu-restricted-extras 是一个包含各种基本软件,如 `Flash` 插件,`unrar` ,`gstreamer`,`mp4`,`Ubuntu` 中的 `Chromium` 浏览器的编解码器等的软件包.

由于这些软件不是开源软件,并且其中一些涉及软件专利,因此 Ubuntu 默认情况下不会安装它们.你必须使用 `multiverse` 仓库,它是 Ubuntu 专门为用户提供非开源软件而创建的仓库.

由于 `ubuntu-restrcited-extras` 软件包在 `multiverse` 仓库中,因此你应验证系统上已启用 `multiverse` 仓库:

```bash
sudo add-apt-repository multiverse
```

然后你可以使用以下命令安装:

```bash
sudo apt install ubuntu-restricted-extras
```

[What are Ubuntu Repositories](https://itsfoss.com/ubuntu-repositories/)
[一条命令在 Ubuntu 中安装所有基本的媒体编解码器 ](https://linux.cn/article-11906-1.html)

## source 命令

[Ubuntu如何使用source命令执行文件](http://www.xitongzhijia.net/xtjc/20150714/52870.html)

`Ubuntu source` 命令的作用就是将设置在文件中的配置信息马上生效,而不需要经过重启.

Ubuntu如何使用`source`命令执行文件

source命令用法:
`source filename` 或 `. filename`

在对编译系统核心时常常需要输入一长串的命令,如:

```bash
make mrproper
make menuconfig
make dep
make clean
make bzImage
......
```

如果把这些命令做成一个文件,让它自动顺序执行,对于需要多次反复编译系统核心的用户来说会很方便,
而用source命令就可以做到这一点,
它的作用就是把一个文件的内容当成shell来执行,先在linux的源代码目录下(如`/usr/src/linux-2.4.20`)建立一个文件,如`make_command`,在其中输入一下内容:

```bash
make mrproper &&
make menuconfig &&
make dep &&
make clean &&
...
```

文件建立好之后,每次编译核心的时候,只需要在`/usr/src/linux-2.4.20`下输入:`source make_command`即可

顺便补充一点,`&&`命令表示顺序执行由它连接的命令,但是只有它之前的命令成功执行完成了之后才可以继续执行它后面的命令.

另外执行source命令时如果提示command not found,是因为环境变量没配置好的原因,在终端运行如下命令即可修复:

`export PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/root/bin`

## 查看磁盘空间

`df`命令是linux系统以磁盘分区为单位查看文件系统,可以加上参数查看磁盘剩余空间信息,命令格式:

`df - report file system disk space usage`

***
SYNOPSIS

`df [OPTION]... [FILE]...`

+ `-a`, `--all` include pseudo, duplicate, inaccessible file systems
+ `-l`, `--local` limit listing to local file systems
+ `-h`, `--human-readable` print sizes in powers of 1024 (e.g., 1023M)
+ `-T`, `--print-type` print file system type

## find 过滤文件

删除日志文件

```bash
sudo /dev/null > /var/log/**.log
```

下面这个推荐使用,删除30天之前的旧文件

```bash
sudo find /var/log/ -type f -mtime +30 -exec rm -f {} \;
```

***
`find` - search for files in a directory hierarchy

`find [-H] [-L] [-P] [-D debugopts] [-Olevel] [starting-point...] [expression]`

***
`expression`

`starting points`列表之后的部分是`表达式`.  这是一种查询规范, 描述了我们如何匹配文件以及如何处理匹配的文件.
表达式由一系列事物组成:`Test`, `Actions`,...

***
`-exec command ;`

执行命令;  如果返回`0`状态, 则为`true`.  之后传递给`find`的参数都将作为命令的参数, 直到遇到`;`为止.
字符` {}`被替换为当前文件名命令参数中出现的任何地方的当前文件名, 而不仅仅是在单独存在的参数中.
这两种构造都可能需要转义(以`\`表示)或加引号以保护它们, 避免 shell 展开.
有关使用`-exec`选项的示例, 请参见示例部分. 对每个匹配的文件运行一次指定的命令.
该命令在起始目录中执行.  与`-exec`有关的操作具有不可避免的安全问题;  你应该使用`-execdir`选项代替.

***
`-exec command {} +`

在选定的文件上运行指定的命令, `-exec` action的变体. 但是通过在命令结尾附加上每个选中的文件名;
该命令的调用总数将远远少于匹配文件的数目.  命令行的构建方式与`xargs`几乎相同.
命令中仅允许使用一个`{}`实例, 并且当从`shell`调用`find`时,应该用引号保护起来, 例如`'{}'`, 以防止其被`shell`解释.
该命令在起始目录中执行.  如果有任何调用返回一个非零值作为退出状态, 则`find`返回一个非零退出状态.
如果`find`遇到错误, 有时可能会导致立即退出, 因此一些待处理的命令可能根本不会运行. This variant of `-exec` always returns `true`.

***
`-mtime n`
文件数据的最后修改时间为 `n*24` 小时. 请参阅`-atime`的注释, 以了解舍入如何影响文件修改时间的解释

`-type c`
File is of type c:

+ `b`  block (buffered) special
+ `c`  character (unbuffered) special
+ `d`  directory
+ `p`  named pipe (FIFO)
+ `f`  regular file

## 查看文件大小

[Ubuntu下查看文件, 文件夹和磁盘空间的大小](https://blog.csdn.net/BigData_Mining/java/article/details/88998472)

在实际使用`ubuntu`时候,经常要碰到需要查看文件以及文件夹大小的情况.
有时候,自己创建压缩文件,可以使用 `ls -hl`查看文件大小.参数`-h` 表示`Human-Readable`,使用`GB`,`MB`等易读的格式方式显示.对于文件夹的大小,`ll -h` 显示只有`4k`.

***
那么如何来查看文件夹的大小呢?

使用`du`命令查看文件或文件夹的磁盘使用空间,`–max-depth` 用于指定深入目录的层数.

如要查看当前目录已经使用总大小及当前目录下一级文件或文件夹各自使用的总空间大小,
输入`du -h --max-depth=1`即可.

如要查看当前目录已使用总大小可输入:`du -h --max-depth=0`

***

```bash
du [OPTION]... [FILE]...
du [OPTION]... --files0-from=F
```

+ `-s,` `--summarize`: 对每个参数仅显示总计
+ ` -h`, `--human-readable`: 以人类可读的格式显示大小(例如`1K` `234M` `2G`)
+ `-d,` `--max-depth=N`: 指定目录递归的层数;  `--max-depth = 0`与`--summaryize`相同
+ `--si`   like `-h`, 但是使用`1000`的幂而不是`1024`的幂
+ `-a,` `--all` :给出所有文件的统计, 而不仅仅是目录

## 创建链接

`ln` — 创建链接

`ln` 命令即可创建硬链接,也可以创建符号链接.

可以用其中一种方法来使用它:

+ `ln file link` 创建硬链接
+ `ln -s item link` 创建符号链接,`item` 可以是一个文件或是一个目录.

### 硬链接

硬链接和符号链接比起来,硬链接是最初 `Unix` 创建链接的方式,而符号链接更加现代. 在默认情况下,每个文
件有一个硬链接,这个硬链接给文件起名字.
当我们创建一个 硬链接以后,就为文件创建了一个额外的目录条目.硬链接有两个重要局限性:

1. 一个硬链接不能关联它所在文件系统之外的文件.这是说一个链接不能关联 与链接本身不在同一个磁盘分区
上的文件.
2. 一个硬链接不能关联一个目录.

一个硬链接和文件本身没有什么区别.不像符号链接,当你列出一个包含硬链接的目录 内容时,你会看到没有特
殊的链接指示说明.当一个硬链接被删除时,这个链接 被删除,但是文件本身的内容仍然存在(这是说,它所占
的磁盘空间不会被重新分配), 直到所有关联这个文件的链接都删除掉.知道硬链接很重要,因为你可能有时
会遇到它们,但现在实际中更喜欢使用符号链接,下一步我们会讨论符号链接.

### 符号链接

创建符号链接是为了克服硬链接的局限性.
符号链接生效,是通过创建一个特殊类型的文件,这个文件包含一个关联文件或目录的文本指针.
在这一方面, 它们和 Windows 的快捷方式差不多,当然,符号链接早于Windows 的快捷方式 很多年;-)

一个符号链接指向一个文件,而且这个符号链接本身与其它的符号链接几乎没有区别.
例如,如果你往一个符号链接里面写入东西,那么相关联的文件也被写入.

然而, 当你删除一个符号链接时,只有这个链接被删除,而不是文件自身.
如果先于符号链接删除文件,这个链接仍然存在,但是不指向任何东西.
在这种情况下,这个链接被称为坏链接.
在许多实现中,`ls` 命令会以不同的颜色展示坏链接,比如说红色,来显示它们的存在.

***
创建硬链接

现在,我们试着创建链接.首先是硬链接.我们创建一些关联我们 数据文件的链接:

```bash
[me@linuxbox playground]$ ln fun fun-hard
[me@linuxbox playground]$ ln fun dir1/fun-hard
[me@linuxbox playground]$ ln fun dir2/fun-hard
```

`ls` 命令有一种方法,来展示(文件索引节点)的信息.在命令中加上``-i``选项:

***
创建符号链接

建立符号链接的目的是为了克服硬链接的两个缺点:硬链接不能跨越物理设备, 硬链接不能关联目录,只能是文
件.符号链接是文件的特殊类型,它包含一个指向 目标文件或目录的文本指针.

符号链接的建立过程相似于创建硬链接:

```bash
[me@linuxbox playground]$ ln -s fun fun-sym
[me@linuxbox playground]$ ln -s ../fun dir1/fun-sym
[me@linuxbox playground]$ ln -s ../fun dir2/fun-sym
```

第一个实例相当直接,在 ln 命令中,简单地加上`-s`选项就可以创建一个符号链接, 而不是一个硬链接.

`fun-sym` 的列表说明了它是一个符号链接,通过在第一字段中的首字符`l` 可知,并且它还指向`../fun`,也是正确的.

当建立符号链接时,你即可以使用绝对路径名:

```bash
ln -s /home/me/playground/fun dir1/fun-sym
```

也可用相对路径名,正如前面例题所展示的.使用相对路径名更令人满意, 因为它允许一个包含符号链接的目录重命名或移动,而不会破坏链接.

## basename

***
截取文件名和后缀

编写Shell脚本的过程中,经常会和文件名和文件路径打交道.
如果用户输入了一个文件的全名(可能包含绝对路径和文件后缀),如何得到文件的路径名,文件名,文件后缀这些信息呢.
Shell脚本拥有强大的字符串处理能力,如果把文件名当做字符串,我们不难使用`cut`或`sed`这样的工具得到我们想要的结果.

```bash
$fullfile=/the/path/foo.txt
$fullname=$(basename $fullfile)
$dir=$(dirname $fullfile)
$filename=$(echo $fullname | cut -d . -f1)
$extension=$(echo $fullname | cut -d . -f2)
$ echo $dir , $fullname , $filename , $extension
/the/path , foo.txt , foo , txt
```

这里使用`basename`命令可以直接得到包含后缀的文件名,而`dirname`命令可以得到路径名,
然后就能简单的用`cut`截取文件名和后缀名.

***
更复杂的情况

如果对付简单应用场景,到这里已经可以打完收工了,但是有时候文件可能不止有一个后缀,比如`*.tar.gz`,怎样得到最后一个后缀呢?
再`cut`一回? 当然可以,但是如果文件名是`mylib.1.0.1a.zip`这样的呢? 呃......正则表达式肯定可以.

```bash
$ fullname=mylib.1.0.1a.zip
$ filename=$(echo $fullname | sed 's/\.[^.]*$//')
$ extension=$(echo $fullname | sed 's/^.*\.//')
$ echo $filename, $extension
mylib.1.0.1a, zip
```

这里面的逻辑是这样的:

文件名:把以`.`字符开头以后一直到行尾都是非`.`字符的子串替换为空.
后缀名:把从行首开始以`.`字符结尾的子串替换为空.

光用语言把这两个正则表达式描述出来脑细胞也要死不少.有没有像上面`cut`版本一样简单容易理解的方法呢?
由于`.`分隔符的个数不确定,正常使用`cut`来分割最后一个`.`字符是不太可能的.
但是我们可使用`rev`命令将字符串反转一下,区分后缀和文件名的`.`字符位置就确定了.
截取了想要的部分之后,再次反转就得到了我们想要的内容.

```bash
$ fullname=mylib.1.0.1a.zip
$ filename=$(rev <<< $fullname | cut -d . -f2- | rev)
$ extension=$(rev <<< $fullname | cut -d . -f1 | rev)
$ echo $filename, $extension
mylib.1.0.1a, zip
```

***
使用参数扩展

其实不借助复杂的正则表达式,甚至不调用`basename`, `dirname`, `cut`, `sed`命令,`shel`l脚本一样可以做到所有的操作.
看下面的实现:

```bash
$ fullfile=/the/path/mylib.1.0.1a.zip
$ fullname="${fullfile##*/}"
$ dir="${fullfile%/*}"
$ extension="${fullname##*.}"
$ filename="${fullname%.*}"
$ echo $dir , $fullname , $filename , $extension
/the/path , mylib.1.0.1a.zip , mylib.1.0.1a , zip
```

真是不能再简洁了,大括号之内变量名配合几个神奇的字符,就是Shell的参数扩展(Parameter Extension)功能.

+ `${fullfile##*/}`:从前面开始删除`fullfile`中最大匹配(longest matching pattern) `*/` 的字符串
+ `${fullfile%/*}`:从后面开始删除`fullfile`中最小匹配(shortest matching pattern) `/*` 的字符串
+ `${fullname##*.}`:从前面开始删除`fullname`中最大匹配(longest matching pattern) `*.` 的字符串
+ `${fullname%.*}`:从后面开始删除`fullname`中最小匹配(shortest matching pattern) `.*` 的字符串

参数扩展有多种形式,在shell编程中可以用作参数的拼接,字符串的替换,参数列表截取,变量初值等操作,
这里不再详述,请参考后面的功能列表和官方文档

***
使用`basename`命令输出所有`*.tex`的名字

```bash
basename -s '.tex' $(ls *.tex) | xargs echo
```

## inode

[Linux的inode的理解](https://www.cnblogs.com/itech/archive/2012/05/15/2502284.html)

***
inode是什么

理解`inode`,要从文件储存说起.
文件储存在硬盘上,硬盘的最小存储单位叫做`扇区`(Sector).每个扇区储存`512`Byte(相当于`0.5KB`).
操作系统读取硬盘的时候,不会一个个扇区地读取,这样效率太低,而是一次性连续读取多个扇区,即一次性读取一个"块"(`block`).
这种由多个扇区组成的`块`,是文件存取的最小单位.`块`的大小,最常见的是`4KB`,即连续八个 `sector`组成一个 `block`.

文件数据都储存在`块`中,那么很显然,我们还必须找到一个地方储存文件的元信息,比如文件的创建者, 文件的创建日期, 文件的大小等等.
这种储存文件元信息的区域就叫做`inode`,中文译名为`索引节点`, `index node`.

***
inode的内容

`inode`包含文件的元信息,具体来说有以下内容:

+ 文件的字节数
+ 文件拥有者的User ID
+ 文件的Group ID
+ 文件的读, 写, 执行权限
+ 文件的时间戳,共有三个:`ctime` 指 `inode` 上一次变动的时间, `mtime` 指文件内容上一次变动的时间, `atime` 指文件上一次打开的时间.
+ 链接数,即有多少文件名指向这个 `inode`
+ 文件数据`block`的位置

可以用`stat`命令,查看某个文件的`inode`信息:

```bash
stat example.txt
```

总之,除了文件名以外的所有文件信息,都存在`inode`之中.至于为什么没有文件名,下文会有详细解释.

***
inode的大小

`inode`也会消耗硬盘空间,所以硬盘格式化的时候,操作系统自动将硬盘分成两个区域.一个是数据区,存放文件数据; 另一个是`inode`区(`inode table`),存放`inode`所包含的信息.
每个`inode`节点的大小,一般是`128`字节或`256`字节.
`inode`节点的总数,在格式化时就给定,一般是每1KB或每2KB就设置一个`inode`.
假定在一块1GB的硬盘中,每个`inode`节点的大小为128字节,每`1KB`就设置一个`inode`,那么`inode table`的大小就会达到`128MB`,占整块硬盘的`12.8%`.

查看每个硬盘分区的`inode`总数和已经使用的数量,可以使用`df`命令.

```bash
df -i
```

查看每个`inode`节点的大小,可以用如下命令:

```bash
sudo dumpe2fs -h /dev/hda | grep "Inode size"
```

由于每个文件都必须有一个`inode`,因此有可能发生`inode`已经用光,但是硬盘还未存满的情况.这时,就无法在硬盘上创建新文件.

***
inode号码

每个`inode`都有一个号码,操作系统用`inode`号码来识别不同的文件.

这里值得重复一遍,`Unix/Linux`系统内部不使用文件名,而使用`inode`号码来识别文件.对于系统来说,文件名只是`inode`号码便于识别的别称或者绰号.
表面上,用户通过文件名,打开文件.实际上,系统内部这个过程分成三步:首先,系统找到这个文件名对应的`inode`号码; 其次,通过`inode`号码,获取`inode`信息;
最后,根据`inode`信息,找到文件数据所在的`block`,读出数据.

使用`ls -i`命令,可以看到文件名对应的`inode`号码:

```bash
ls -i example.txt
```

***
目录文件

`Unix/Linux`系统中,目录(directory)也是一种文件.打开目录,实际上就是打开目录文件.

目录文件的结构非常简单,就是一系列目录项(`dirent`)的列表.每个目录项,由两部分组成: 所包含文件的文件名,以及该文件名对应的 `inode` 号码.

`ls`命令只列出目录文件中的所有文件名:

```bash
ls /etc
```

ls -i命令列出整个目录文件,即文件名和`inode`号码:

```bash
ls -i /etc
```

如果要查看文件的详细信息,就必须根据`inode`号码,访问`inode`节点,读取信息.`ls -l`命令列出文件的详细信息.

```bash
ls -l /etc
```

***
硬链接

一般情况下,文件名和`inode`号码是"一一对应"关系,每个`inode`号码对应一个文件名.但是,Unix/Linux系统允许,多个文件名指向同一个`inode`号码.
这意味着,可以用不同的文件名访问同样的内容; 对文件内容进行修改,会影响到所有文件名; 但是,删除一个文件名,不影响另一个文件名的访问.这种情况就被称为"硬链接"(`hard link`).

`ln`命令可以创建硬链接:

```bash
ln 源文件 目标文件
```

运行上面这条命令以后,源文件与目标文件的inode号码相同,都指向同一个`inode`.`inode`信息中有一项叫做"链接数",记录指向该`inode`的文件名总数,这时就会增加1.反过来,删除一个文件名,就会使得`inode`节点中的"链接数"减1.当这个值减到0,表明没有文件名指向这个`inode`,系统就会回收这个`inode`号码,以及其所对应`block`区域.

这里顺便说一下目录文件的"链接数".创建目录时,默认会生成两个目录项:"."和"..".前者的`inode`号码就是当前目录的`inode`号码,等同于当前目录的"硬链接"; 后者的`inode`号码就是当前目录的父目录的`inode`号码,等同于父目录的"硬链接".所以,任何一个目录的"硬链接"总数,总是等于`2`加上它的子目录总数(含隐藏目录),这里的`2`是父目录对其的"硬链接"和当前目录下的".硬链接".

***
软链接

除了硬链接以外,还有一种特殊情况.文件`A`和文件`B`的`inode`号码虽然不一样,但是文件`A`的内容是文件`B`的路径.
读取文件`A`时,系统会自动将访问者导向文件`B`.因此,无论打开哪一个文件,最终读取的都是文件`B`.这时,文件`A`就称为文件`B`的"软链接"(soft link)或者"符号链接(symbolic link).

这意味着,文件`A`依赖于文件`B`而存在,如果删除了文件`B`,打开文件`A`就会报错:"No such file or directory".
这是软链接与硬链接最大的不同:文件`A`指向文件`B`的文件名,而不是文件`B`的`inode`号码,文件`B`的`inode`"链接数"不会因此发生变化.

`ln -s`命令可以创建软链接.

```bash
ln -s 源文文件或目录 目标文件或目录
```

***
inode的特殊作用

由于`inode`号码与文件名分离,这种机制导致了一些`Unix/Linux`系统特有的现象.

+ 有时,文件名包含特殊字符,无法正常删除. 这时,直接删除`inode`节点,就能起到删除文件的作用.
+ 移动文件或重命名文件,只是改变文件名,不影响`inode`号码.
+ 打开一个文件以后,系统就以`inode`号码来识别这个文件,不再考虑文件名.因此,通常来说,系统无法从`inode`号码得知文件名.

第`3`点使得软件更新变得简单,可以在不关闭软件的情况下进行更新,不需要重启.
因为系统通过`inode`号码,识别运行中的文件,不通过文件名.更新的时候,新版文件以同样的文件名,生成一个新的`inode`,不会影响到运行中的文件.
等到下一次运行这个软件的时候,文件名就自动指向新版文件,旧版文件的`inode`则被回收.

## shebang 脚本开头

[Shebang](https://bash.cyberciti.biz/guide/Shebang)

大多数`Linux shell`和`perl`/`python`脚本以以下行开头:

```bash
#!/bin/bash
#!/usr/bin/perl
#!/usr/bin/python
#!/usr/bin/python3
#!/usr/bin/env bash
```

这称为`shebang`或`bang`行.

`shebang`(意思为这一切)其实就是`Bash`解释器的绝对路径. 几乎所有的`bash`脚本通常都以`#!/bin/bash`开头(假设`Bash`已安装在`/bin`中).
这样可以确保即使在另一个`shell`下执行脚本, 也可以使用`Bash`来解释该脚本.
`Shebang`是由Dennis Ritchie在第7版和8版`Unix`之间在`Bell`实验室推出的.  然后, 它也被添加到Berkeley的`BSD`中.

`/usr/bin/env`在修改后的环境中运行`bash`之类的程序.  它使您的`bash`脚本具有可移植性.
`#!/usr/bin/env bash`的优点是, 它将使用运行用户的`$PATH`变量中最先出现的`bash`可执行文件.

## 日志文件

[linux系统日志在哪? ](https://www.php.cn/linux-435716.html)
[linux日志介绍](https://zhuanlan.zhihu.com/p/26428150)

```bash
sudo tail -f /var/log/messages
# or in ubuantu
sudo tail -f /var/log/kern.log
```

`linux`日志大多是以明文存储, 一般存储在`/var/log`目录中, linux系统中主要有三个日志子系统: 连接时间日志, 进程统计日志, 错误日志.

+ `assess-log` 记录和`HTTP/web`的传输
+ `secure` 记录登录系统存取资料的消息
+ `btmp` 记录失败的消息
+ `lastlog` 记录最近几次成功登录的事件和最后一次不成功的登录
+ `messages`: 包括整体系统信息, 其中也包含系统启动期间的日志. 此外, 还包括`mail`, `cron`, `daemon`, `kern`和`auth`等内容
+ `sudolog` 记录`sudo`发出的命令
+ `sulog` 记录使用`su`命令的使用
+ `utmp` 记录当前登录的每个用户
+ `wtmp` 一个用户每次登录进入和退出的的永久记录
+ `syslog`: 它和`/etc/log/messages`日志文件不同, 它只记录警告信息, 常常是系统出问题的信息.
+ `xferlog` 记录了FTP会话
+ `user.log`: 记录所有等级用户信息的日志.
+ `auth.log`: 包含系统授权信息, 包括用户登录和使用的权限机制等.
+ `daemon.log`: 包含各种系统后台守护进程日志信息.
+ `kern.log`: 包含内核产生的日志, 有助于在定制内核时解决问题.

连接时间日志是有多个程序执行的, 把日志记录到`/var/log/wtmp`, `/var/run/utmp`,`/var/log/lastlog` 三个文件中, 这三个文件记录了用户登录系统和退出的有关信息,
`utmp`保存了当前用户的每个用户的信息, `wtmp`记录了每个用户登录注销和系统的启动, 关机的事件, `lastlog`记录了每个用户最后登录的信息记录.

`wtmp`和`utmp`文件都是二进制, 不能使用`cat`和`tail`命令查看, 但是可以使用`who`, `w`,`users`,`last` 等命令查看着两个文件的信息

***
`who [参数]`

+ `-a` 显示全部信息
+ `-m` 只显示当前终端的登录用户信息
+ `-q` 只显示当前登录到系统中的用户名称和数量, 和其他参数共同使用的时候, 其他参数将被忽略

***
`Systemd`统一管理所有 `Unit` 的启动日志. 带来的好处就是可以只用`journalctl`一个命令, 查看所有日志(内核日志和 应用日志).

语法格式:  `journalctl [参数]`

+ `-k,--dmesg`: 查看内核日志
+ `-b`:  查看系统本次启动的日志
+ `-u`:  查看指定服务的日志
+ `-n`:  指定日志条数
+ `-f`:  追踪日志
+ `--disk-usage`:  查看当前日志占用磁盘的空间的总大小

`-S, -since =, -U, -until =`
显示比指定日期更早或更晚的日志. 日期格式为`"2012-10-30 18:17:16"`.如果省略时间部分, 则假定为`00:00:00`.如果只有秒部分是省略, 则假定为`:00`.
如果省略日期部分, 则假定为当前日期.  也可以使用字符串`yesterday`, `"today"`, `"tomorrow"`.`now`是指当前时间.
是指当日, 当日或当日后一天的00:00:00. 最后, 可以用`-`或`+`为前缀指定相对时间. 有关完整的时间和日期规范, 请参见`systemd.time(7).`.
`--output=short-full`将完全按照此格式打印时间戳.

`x, --catalog`:
用信息目录中的解释文本来增加日志行. 这将为输出中的日志信息添加解释性的帮助文本, 如果这是可用的.
这些简短的帮助文本将解释错误或日志事件的背景, 可能的解决方案, 以及指向支持论坛, 开发人员文档和任何其他相关手册的指针.
请注意, 帮助文本不是对所有的信息都可用, 而只是对选定的 的帮助文本. 关于消息目录的更多信息, 请参考消息目录开发者文档[5].

注意: 当把 `journalctl` 输出附加到错误报告时, 请不要使用 `-x`.

`-e, --pager-end`:

立即跳到默认的分页工具的日志末尾. 这意味着`-n1000`, 以保证`pager`不会缓冲无限制大小的日志.
可以用一个明确的`-n数值`来覆盖, 而`-nall`将禁用这个上限. 注意, 这个选项只支持`less(1)`pager.

参考实例

+ 查看所有日志:  `journalctl`
+ 查看httpd的日志:  `journalctl -u httpd`
+ 查看最近发生的20条日志:  `journalctl -n 20`
+ 追踪日志:  `journalctl -f`

## Linux 安装时的分区

[UEFI/GPT 示例](https://wiki.archlinux.org/title/Parted_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#UEFI/GPT_%E7%A4%BA%E4%BE%8B)
[manjaro_user_guide 也有分区的例子](https://manjaro.org/support/userguide/)
[Arch boot process](https://wiki.archlinux.org/title/Arch_boot_process_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%90%AF%E5%8A%A8%E5%8A%A0%E8%BD%BD%E5%99%A8)

以`UEFI`主板为例, 参考`manjaro_user_guide`. 主要设置三个分区:

文件系统格式, 挂载点, 建议大小, `flags`

+ `fat32`,`/boot/efi`,`512MiB`, `boot,esp`, `esp`即 `UEFI System Partition`, GPT 中它是`boot`的别名.
+ `linuxswap`,不需要挂载, $\sqrt{\text{内存大小}} \sim 2* \text{内存大小}$
+ `ext4`,`/`,最简单的, 可以把剩下空间全部分配给`/`,文件系统也可以选别的, 比如`Btrfs`

强迫症可以把分区调整成`EFI`,`root`,`swap`的顺序, 可能有玄学加成.

## 交换分区 swap

[ubnuntu SwapFaq](https://help.ubuntu.com/community/SwapFaq)

**
如果想要确认是否有`swap`分区, 使用`parted`查看所有分区

```bash
sudo parted --list
```

如果有`swap`, 在输出中可以看到类似下面的内容

    5      236GB   256GB   20.0GB  linux-swap(v1)

***
增加交换分区大小并将其用于休眠

+ 创建交换分区
+ 激活交换分区
+ 使新的交换分区适用于休眠状态(可选)

***
创建交换分区

使用`Ubuntu`安装介质如U盘开机, 选择`立即运行Ubuntu`.
打开`GParted`分区编辑器: 删除原先的`swap`, `resize`主分区大小, 留出合适的空白空间用作`swap`.可以直接在`"free space following"`一项中设置想要的`swap`分区大小.
在新的空白空间中, 选择`new`, 输入`linux-swap`, 如果喜欢,可以起个名`swap`到`Partition name`中, 然后点击`Apply`应用.  完成后, 重新启动原先硬盘上的`Ubuntu`系统.

***
激活交换分区

如果交换位于主硬盘驱动器上, 则无需在此处做任何事情.
现在, 您需要查找`swap`所在的`分区`及其`UUID`, Universally Unique IDentifier, 它是该分区的通用唯一身份, 即使由于添加磁盘, 重启之后分区挂载点改变, `UUID`也不会改变.

切换到`root`用户, 运行`gparted` . 右键单击交换分区, 然后选择`Information`.
应该可以看到`Path`和`UUID`. 保持打开状态以供进一步参考.
运行 `gedit /etc/fstab`, 查找其中带有`swap`的行. 它应该在第三列, 用空格或制表符分隔. 您可以使用路径或`UUID`来告诉`Linux`在哪里找到交换分区. 建议使用`UUID`, 因为即使您移动分区或磁盘之后, `sdb`变成`sda`, `UUID`也将保持不变.
如果您使用的是UUID, 则您的代码行应如下所示:

    UUID=41e86209-3802-424b-9a9d-d7683142dab7 none swap sw 0 0

或使用路径:

    /dev/sda2 none swap sw 0 0

保存更改.

使用此命令启用新的交换分区.

```bash
sudo swapon --all # 或者 sudo swapon --all --verbose
```

确认交换分区存在.

```bash
$ cat /proc/swaps
Filename                                Type            Size  Used    Priority
/dev/sda2                               partition      20971480       -1
```

接着可以重启查看交换分区能否被正确激活.

***
使交换分区用于休眠(可选)

运行`cat /proc/swaps`, 可以看到`swap`分区的路径. 找到它的`UUID`. 使用

    sudo gedit /etc/default/grub

修改`grub`(启动引导加载程序)的配置. 如果怕出问题可以先备份

    sudo cp -a /etc/default/grub /etc/default/grub.bak

查找`GRUB_CMDLINE_LINUX="xx"`行, 在后面添加上:

    GRUB_CMDLINE_LINUX="XXX resume=UUID=41e86209-3802-424b-9a9d-d7683142dab7"

并保存文件.`sudo update-grub`并等待其完成.

然后`sudo gedit /etc/initramfs-tools/conf.d/resume`, 确保内容类似下面这样:

    resume=UUID=41e86209-3802-424b-9a9d-d7683142dab7

如果不使用`swap`, 这里的设置应该是`resume=none`,保存文件.
 然后运行`sudo update-initramfs -u`. 重启. 现在应该可以休眠了.

***
启用未生效的交换分区: 如果你已经有交换分区, 则有几种启用它的方法. 首先查看`fstab`

    cat /etc/fstab

确保下面有`swap`的记录, 这样可以在启动时启用`swap`.

    /dev/sdb5       none            swap    sw              0       0

然后禁用所有`swap`, 再重新创建它, 然后使用以下命令重新启用

```bash
sudo swapoff -a
sudo /sbin/mkswap /dev/sdb5
sudo swapon -a
```

***
什么是`swappiness`, 我该如何更改?

`swappiness`参数控制内核使用`swap`的倾向. 因为磁盘要比`RAM`慢得多, 所以如果进程经常主动地移出内存, 可能导致系统和应用程序的响应时间变慢.

+ `swappiness` 的值可以在`0`到`100`之间
+ `swappiness=0`告诉内核尽可能避免将进程从物理内存中交换出来
+ `swappiness=100` 告诉内核积极地把进程从物理内存移动到`swap`缓存

`Ubuntu` 中的默认设置为`swappiness=60`. 降低 `swappiness` 的默认值可能会提高典型的`Ubuntu`桌面安装的整体性能.
建议将 `swappiness` 的值设置为`10`, 你也可以自己尝试. `Ubuntu`服务器对桌面系统的性能要求不同, 默认值`60`可能更合适.

检查`swappiness`值

    cat /proc/sys/vm/swappiness

临时更改 `swappiness` 值(重启之后丢失):

    sudo sysctl vm.swappiness=10

永久更改:

    sudo gedit /etc/sysctl.conf

搜索`vm.swappiness`根据需要更改其值. 如果`vm.swappiness`不存在则自己添加:

    vm.swappiness=10

保存文件并重新启动.

## 编码,字符集

[File name is garbled](https://wiki.archlinux.org/title/Localization/Simplified_Chinese)

避免乱码基本原则: 使用 `utf-8` 代替 `gbk/gb2312`.

`convmv`: 将文件名从一种编码转换到另一种编码, 例如:

```bash
convmv -f GBK -t UTF-8 --notest --nosmart file
```

`-f`指定原始编码, `-t`指定输出编码. 使用 `convmv --list` 可查询所有支持的编码.
`--notest` 表示进行实际操作, 而非测试, 如果不使用该参数只会打印出转换结果而不会实际转码. `--smart`表示如果已经是`UTF-8` 则忽略.

### 文件内容乱码

使用 `iconv` 命令转换格式. 示例:

```bash
iconv -f GBK -t UTF-8 -o new-file origin-file
```

`-f` 指定原始编码, `-t` 指定输出编码. 使用 `iconv -l` 可查询所有支持的编码. `-o` 指定输出文件.

### zip 压缩包乱码

避免方法: 非 `utf8` 编码环境下(一般 `windows` 下的中文环境即是)不使用 `zip` 进行压缩(建议使用 `7z`).
解决方案: 安装使用 `unzip-iconv` 或者 `unzip-natspec`取代原版的 `unzip` 来解压缩, 示例:

```bash
unzip -O gbk file.zip
```

`file.zip` 是压缩文件, `gbk` 是该文件的编码格式, 以 `-O` 指定(原版 `unzip` 无 `-O` 选项).

## 合并文件夹

[How to copy-merge two directories](https://unix.stackexchange.com/questions/149965/how-to-copy-merge-two-directories)

这是 `rsync` 的功能. 除非你想`移动` 文件而不是 `复制` 它们, 否则用 `shell` 循环手动做这个没有任何好处.

```bash
rsync -a /path/to/源/   /path/to/目标
```

在你的例子中

```bash
rsync -a /images2/  /images/
```

注意 `images2` 的尾部斜线, 否则 `rsync` 会复制到 `/images/images2`.

如果两个目录中都存在同名的图像, 上面的命令将用 `/images2/某些/文件` 覆盖 `/images/某些/文件`.
如果你想只替换较早的文件, 请添加选项 `-u`.
如果你想总是保留 `/images` 中的版本, 添加选项 `--ignore-existing`.

如果你想从 `/images2` 移动文件, 用 `rsync`, 你可以通过选项 `--remove-source-files`.
然后 `rsync` 依次复制所有的文件, 完成后再删除每个文件.
如果源目录和目的目录在同一个文件系统上, 这要比移动慢得多.
