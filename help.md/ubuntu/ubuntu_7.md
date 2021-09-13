# ubuntu_7

## 第十九章:归档和备份

计算机系统管理员的一个主要任务就是保护系统的数据安全,其中一种方法是通过定时备份系统文件,来保护数据.
即使你不是一名系统管理员,像做做拷贝或者在各个位置和设备之间移动大量的文件,通常也是很有帮助的.

在这一章中,我们将会看看几个经常用来管理文件集合的程序.它们就是文件压缩程序:

+ `gzip` – 压缩或者展开文件
+ `bzip2` – 块排序文件压缩器

归档程序:

+ `tar` – 磁带打包工具
+ `zip` – 打包和压缩文件

还有文件同步程序:

+ `rsync` – 同步远端文件和目录

### 压缩文件

纵观计算领域的发展历史,人们努力想把最多的数据存放到到最小的可用空间中,不管是内存,存储设备 还是网络带宽.
诸如便携式音乐播放器, 高清电视,或宽带网络之类的存在都应归功于高效的数据压缩技术.

数据压缩就是一个删除冗余数据的过程.

比方说我们有一张`100*100`像素的 纯黑的图片文件,假定每个像素占`24`位,或者`3`个字节,那么这张图像将会占用 `30,000` 个字节的存储空间:

```bash
100 * 100 * 3 = 30,000
```

一张单色图像包含的数据全是多余的.我们只要简单地描述这个事实,我们有3万个黑色的像素数据块.
所以,我们不存储包含`3万`个`0` (通常在图像文件中,黑色由`0`来表示)的数据块,
相反,我们把这些数据压缩为数字`30,000`, 后跟一个`0`.
这种数据压缩方案被称为游程编码,是一种最基本的压缩技术.

压缩算法(数学技巧被用来执行压缩任务)分为两大类,无损压缩和有损压缩.

无损压缩保留了原始文件的所有数据.还原出的文件与原文件一模一样.

而另一方面,有损压缩,执行压缩操作时会删除数据,允许更大的压缩.
有损文件被还原后, 与原文件不相匹配, 而是一个近似值.
有损压缩的例子有 `JPEG`(图像)文件和 `MP3`(音频)文件.

在我们的讨论中,我们将看看完全无损压缩,因为计算机中的大多数数据是不能容忍丢失任何数据的.

### gzip

`gzip` 程序被用来压缩一个或多个文件.当执行 `gzip` 命令时,原始文件的压缩版会替代原始文件.
相对应的 `gunzip` 程序用来把压缩文件复原.这里有个例子:

```bash
$ ls -l /etc > foo.txt
$ gzip foo.txt
$ gunzip foo.txt.gz
```

在这个例子里,我们创建了一个名为 `foo.txt` 的文本文件,其内容是一个目录的列表.
接下来,我们运行 `gzip` 命令,它会把原始文件替换为一个叫做 `foo.txt.gz` 的压缩文件.
在 `foo.*` 文件列表中,我们看到原始文件已经被压缩文件替代了,这个压缩文件大约是原始文件的十五分之一.
我们也能看到压缩文件与原始文件有着相同的权限和时间戳.

接下来,我们运行 `gunzip` 程序来解压缩文件.
随后,我们见到压缩文件已经被原始文件替代了, 同样地保留了相同的权限和时间戳.

`gzip` 命令有许多选项.这里列出了一些:
表19-1: gzip 选项
***
选项 说明

+ `-c` 把输出写入到标准输出,并且保留原始文件.也能用 `--stdout` 和 `--to-stdout` 选项来指定.
+ `-d` 解压缩.正如 `gunzip` 命令一样.也可以用`--decompress` 或者`--uncompress` 选项来指定.
+ `-f` 强制压缩,即使原始文件的压缩文件已经存在了,也要执行.也可以用`--force`选项来指定.
+ `-h` 显示用法信息.也可用`--help` 选项来指定.
+ `-l` 列出每个被压缩文件的压缩数据.也可用`--list` 选项.
+ `-r` 若命令的一个或多个参数是目录,则递归地压缩目录中的文件.也可用`--recursive` 选项来指定.
+ `-t` 测试压缩文件的完整性.也可用`--test`选项来指定.
+ `-v` 显示压缩过程中的信息.也可用`--verbose` 选项来指定.
+ `-number` 设置压缩指数.`number` 是一个在`1`(最快,最小压缩)到`9`(最慢,最大压缩)之间的整数.
数值`1`和`9`也可以用`--fast`和`--best` 选项来表示.默认值是整数6.

返回到我们之前的例子中:

```bash
$ gzip foo.txt
$ gzip -tv foo.txt.gz
foo.txt.gz: OK
$ gzip -d foo.txt.gz
```

这里,我们用压缩文件来替代文件 `foo.txt`,压缩文件名为 `foo.txt.gz`. 下一步,我们测试了压缩文件 的完整性,使用了`-t` 和 `-v` 选项.

```bash
$ ls -l /etc | gzip > foo.txt.gz
```

这个命令创建了一个目录列表的压缩文件.
`gunzip` 程序,会解压缩 `gzip` 文件,假定那些文件名的扩展名是`.gz`,所以没有必要指定它, 只要指定的名字与现有的未压缩文件不冲突就可以:

```bash
$ gunzip foo.txt
```

如果我们的目标只是为了浏览一下压缩文本文件的内容,我们可以这样做:

```bash
$ gunzip -c foo.txt | less
```

另外,对应于 `gzip` 还有一个程序,叫做 `zcat`,它等同于带有`-c` 选项的 `gunzip` 命令.
它可以被用来如 `cat` 命令作用于 `gzip` 压缩文件:

```bash
$ zcat foo.txt.gz | less
```

小贴士: 还有一个 `zless` 程序.它与上面的管道线有相同的功能.

### bzip2

`bzip2` 程序,由 Julian Seward 开发,与 `gzip` 程序相似,但使用了不同的压缩算法, 舍弃了压缩速度,从而实现了更高的压缩级别. 在大多数情况下,它的工作模式等同于 `gzip` . 
由 `bzip2` 压缩的文件,用扩展名 `.bz2`来表示:

```bash
$ ls -l /etc > foo.txt
$ ls -l foo.txt
$ bzip2 foo.txt
$ ls -l foo.txt.bz2
$ bunzip2 foo.txt.bz2
```

正如我们所看到的, `bzip2` 程序用起来和 `gzip` 程序一样.

我们之前讨论的 `gzip` 程序的所有选项(除了`-r`) ,`bzip2` 程序同样也支持.
注意,然而,压缩级别选项(`-number`)对于 `bzip2` 程序来说,含义有稍许不同.

伴随着 `bzip2` 程序,有 `bunzip2` 和 `bzcat` 程序用来解压缩文件.
还有 `bzip2recover` 程序,可以用来尝试恢复受损的 `.bz2` 文件.

#### 不要强迫性压缩

偶尔会有人试图用高效的压缩算法,来压缩一个已经被压缩过的文件:

```bash
$ gzip picture.jpg
```

不要这样,  你可能只是在浪费时间和空间.
如果你再次压缩已经压缩过的文件,实际上你会得到一个更大的文件.
这是因为所有的压缩技术都会涉及一些开销,文件中会被添加描述此次压缩过程的信息.
如果你试图压缩一个已经不包含多余信息的文件,那么再次压缩不会节省空间,以抵消额外的开销.

### 归档文件

一个常见的,与文件压缩结合一块使用的文件管理任务是归档.归档就是收集许多文件,并把它们 捆绑成一个大文件的过程.

归档经常作为系统备份的一部分来使用.当把旧数据从一个系统移到某种类型的长期存储设备中时,也会用到归档程序.

### tar

在类 `Unix` 的软件世界中,这个 `tar` 程序是用来归档文件的经典工具.它是 `tape archive` 的简称,揭示了它的起源是一款制作磁带备份的工具.
虽然仍被用来完成传统任务, 它也同样适用于其它类型的存储设备.

我们经常看到扩展名为 `.tar` 或者 `.tgz` 的文件,它们各自表示`普通` 的 `tar` 包和被 `gzip` 程序压缩过的 `tar` 包.
一个 `tar` 包可以由一组独立的文件,一个或者多个目录,或者 两者混合体组成. 命令语法如下:

```bash
tar mode[options] pathname...
```

这里的 `mode` 是指以下操作模式之一(这里只展示了一部分,查看 `tar` 的手册来得到完整列表):
***
表19-2: `tar` 模式
模式 说明

+ `c` 为文件和/或目录列表创建归档文件.
+ `x` 抽取归档文件.
+ `r` 追加具体的路径到归档文件的末尾.
+ `t` 列出归档文件的内容.

`tar` 命令使用了稍微有点奇怪的方式来表达它的选项,所以我们需要一些例子来展示它是怎样工作的.
首先,让我们重新创建之前我们用过的操练场:

```bash
$ mkdir -p playground/dir-{00{1..9},0{10..99},100}
$ touch playground/dir-{00{1..9},0{10..99},100}/file-{A..Z}
```

下一步,让我们创建整个操练场的 `tar` 包:

```bash
$ tar cf playground.tar playground
```

这个命令创建了一个名为 `playground.tar` 的 tar 包,其包含整个 `playground` 目录层次结果.
我们 可以看到模式 `c` 和选项 `f`,其被用来指定这个 `tar` 包的名字,模式和选项可以写在一起,而且不需要开头的短横线.

注意,必须首先指定模式,然后才是其它的选项. 要想列出归档文件的内容,我们可以这样做:

```bash
$ tar tf playground.tar
```

为了得到更详细的列表信息,我们可以添加选项 `v`:

```bash
$ tar tvf playground.tar
```

现在,抽取 `tar` 包 `playground` 到一个新位置.我们先创建一个名为 `foo` 的新目录,更改目录, 然后抽取 `tar` 包中的文件:

```bash
$ mkdir foo
$ cd foo
$ tar xf ../playground.tar
$ ls playground
```

如果我们检查 `~/foo/playground` 目录中的内容,会看到这个已经成功创建了原始文件的精确副本.
然而有个警告:除非你是超级用户,否则从归档文件中抽取的文件和目录的所有权在执行复原操作的用户手里,而不属于原始所有者.

***
`tar` 命令另一个有趣的行为是它处理归档文件路径名的方式.
默认情况下,路径名是相对的,而不是绝对路径.当创建归档文件的时候,`tar` 命令会简单地删除路径名开头的斜杠.
为了说明问题,我们将重新创建我们的归档文件,这次指定一个绝对路径:

```bash
$ cd
$ tar cf playground2.tar ~/playground
```

记住,当按下回车键后,`~/playground` 会展开成 `/home/me/playground`,所以我们会得到一个绝对路径.
接下来和之前一样, 我们抽取归档文件,观察发生了什么:

```bash
$ cd foo
$ tar xf ../playground2.tar
$ ls
home playground
$ ls home
me
$ ls home/me
playground
```

我们看到, 当抽取第二个归档文件时,它重新创建了 `home/me/playground` 目录. 
并且是相对于当前的工作目录`~/foo`,而不是相对于 `root`. 
这看起来似乎是一种奇怪的工作方式,但事实上这种方式很有用,因为这样就允许我们抽取文件到任意位置,
而不是强制地把抽取的文件放置到原始目录下.

加上 `verbose`(`v`)选项,重做 这个练习,将会展现更加详细的信息.

***
让我们展示一个`tar`命令的实际应用. 假定我们想要复制家目录及其内容到另一个系统中, 并且有一个大容量的 `USB` 硬盘,可以作为传输工具.

在现代 `Linux` 系统中, 这个硬盘会被`自动地`挂载到 `/media` 目录下. 我们也假定硬盘中有一个名为 `BigDisk` 的逻辑卷. 为了制作 `tar` 包,我们可以这样做:

```bash
$ sudo tar cf /media/BigDisk/home.tar /home
```

`tar` 包制作完成之后,我们卸载硬盘,然后把它连接到第二个计算机上.
再一次,此硬盘被挂载到`/media/BigDisk` 目录下.为了抽取归档文件,我们这样做:

```bash
$ cd /
$ sudo tar xf /media/BigDisk/home.tar
```

值得注意的一点是,因为归档文件中的所有路径名都是相对的,所以首先我们必须更改目录到根目录下, 这样抽取的文件路径是相对于根目录的.
当抽取一个归档文件时,可以限制从归档文件中抽取的内容. 例如抽取单个文件, 可以这样实现:

```bash
tar xf archive.tar pathname
```

通过给命令添加末尾的路径名,`tar` 命令就只会恢复指定的文件.可以指定多个路径名.
注意:路径名必须是精准的相对路径名,和存储在归档文件中的完全一样.

当指定路径名的时候, 通常不支持通配符.
然而,`GNU` 版本的 `tar` 命令(在 Linux 发行版中最常出现)可以通过 `--wildcards` 选项来支持通配符.
这个例子使用了之前的 `playground.tar` 文件:

```bash
$ cd foo
$ tar xf ../playground2.tar --wildcards 'home/me/playground/dir-*/file-A'
```

这个命令将只会抽取路径名匹配 `dir-*`的文件. 

`tar` 命令经常结合 `find` 命令一起来制作归档文件.
我们可以使用 `find`来产生一个文件集合,然后把它们加入到归档文件中.

```bash
$ find playground -name 'file-A' -exec tar rf playground.tar '{}' '+'
```

这里我们使用 `find` 命令来匹配 `playground` 目录中所有名为 `file-A` 的文件,
然后使用 `-exec` 行为,来唤醒带有追加模式(`r`)的 `tar` 命令,把匹配的文件添加到归档文件 `playground.tar` 中.

使用 `tar` 和 `find` 命令,来创建逐渐增加的目录树, 或者整个系统的备份,是个不错的方法.
通过 `find` 命令匹配新于某个时间戳的文件,我们就能够创建一个归档文件,
令其只包含新于上一个 `tar` 包的文件(假定这个时间戳文件恰好在每个归档文件创建之后被更新了).

`tar` 命令也可以利用标准输出和输入.这里是一个完整的例子:

```bash
$ cd
$ find playground -name 'file-A' | tar cf - --files-from=- | gzip > playground.tgz
```

在这个例子里面,我们使用 `find` 程序产生了一个匹配文件列表,然后把它们管道到 `tar` 命令中.

如果指定了文件名`-`,则其被看作是标准输入或标准输出.
在上面的例子中`tar cf -`中的是标准输出,`--files-from=-`中的是标准输入.
(顺便说一下,使用`-`来表示 标准输入/输出的惯例,也被大量的其它程序使用).

这个 `--file-from` 选项(也可以用 `-T` 来指定) 导致 `tar` 命令从一个文件而不是命令行来读入它的路径名列表.

最后,这个由 `tar` 命令产生的归档 文件被管道到 `gzip` 命令中,然后创建了压缩归档文件 `playground.tgz`.
`.tgz` 扩展名是被`gzip` 压缩过的 `tar` 文件的常规扩展名.有时候也会使用 `.tar.gz`这个扩展名.

### 顺便压缩

在这里我们手动调用了`gzip`来制作归档文件的压缩版本.
实际上现在 GUN 版本的 `tar` 命令 ,可以直接在归档的同时进行压缩,`gzip`压缩对应选项`z`, `bzip2` 压缩对应选项`j`.
之前的例子可以简化为:

```bash
# 创建一个由 gzip 压缩的归档文件,可以这样做:
$ find playground -name 'file-A' | tar czf playground.tgz -T -
# 创建一个由 bzip2 压缩的归档文件,可以这样做:
$ find playground -name 'file-A' | tar cjf playground.tbz -T -
```

***
另一个 `tar` 命令与标准输入和输出的有趣使用,涉及到在系统之间经过 网络传输文件.

假定我们有两台机器,每台都运行着类 Unix,且装备着 `tar` 和 `ssh` 工具的操作系统.
在这种情景下,我们可以把一个目录从远端系统(名为 `remote-sys`)传输到我们的本地系统中:

```bash
$ mkdir remote-stuff
$ cd remote-stuff
$ ssh remote-sys 'tar cf - Documents' | tar xf -
me@remote-sys' s password:
$ ls Documents
```

这里我们能够从远端系统 `remote-sys` 中复制目录 `Documents` 到本地系统名为 `remote-stuff` 目录中.

我们怎样做的呢?首先,通过使用 `ssh` 命令在远端系统中启动 `tar` 程序.
你可记得 `ssh` 允许我们在远程联网的计算机上执行程序,并将远端系统中产生的输出结果被发送到本地系统中查看.
然后在本地系统中,我们执行 `tar xf -` 命令, 抽取标准输出中的文件.

### zip

`zip` 程序既是压缩工具,也是一个打包工具. 它读取和写入 `.zip` 文件, `Windows` 用户比较熟悉这种文件格式.
然而,在 Linux 中 `gzip` 是主要的压缩程序, `bzip2`则排第二.

在 `zip` 命令的基本用法为: 

```bash
zip options zipfile file...
```

例如,制作一个 `playground` 的 `zip` 版本的文件包,这样做:

```bash
$ zip -r playground.zip playground
```

我们需要包含 `-r` 选项,不然只有 `playground` 目录(没有任何它的内容)被存储.
虽然程序会自动添加 `.zip` 扩展名,但为了清晰起见,我们还是手动加上.

在创建 `zip` 版本的文件包时,`zip` 命令通常会显示一系列的信息:

```bash
adding: playground/dir-020/file-Z (stored 0%)
adding: playground/dir-020/file-Y (stored 0%)
```

这些信息显示了添加到文件包中每个文件的状态.

`zip` 命令会选用两种存储方法之一,来添加文件到文件包中:
一种是`store`,没有经过压缩的文件,正如这里所示,
另一种是`deflate`, 执行压缩操作.
在存储方法之后显示的数值表明了压缩量.

因为我们的 `playground` 目录只包含空文件,没有对它的内容执行压缩操作.

***
使用 `unzip` 程序,来直接抽取一个 `zip` 文件的内容.

```bash
$ cd foo
$ unzip ../playground.zip
```

对于 `zip` 命令(与 `tar` 命令相反)要注意一点,就是如果指定了的文件包已经存在,那么它会被更新而不是被替代.
这意味着会保留此文件包,但是会添加新文件,同时替换匹配的文件.

可以列出 文件或者有选择地从一个 `zip` 文件包中抽取文件,只要给 `unzip` 命令指定文件名:

```bash
$ unzip -l playground.zip playground/dir-087/file-Z
$ cd foo
$ unzip ./playground.zip playground/dir-087/file-Z
```

使用 `-l` 选项,导致 `unzip` 命令只是列出文件包中的内容而没有抽取文件.

如果没有指定文件, `unzip` 程序将会列出文件包中的所有文件.
添加这个 `-v` 选项会增加列表的冗余信息.注意当抽取的文件与已经存在的文件冲突时,会在替代此文件之前提醒用户.

像 `tar` 命令一样,`zip` 命令能够利用标准输入和输出,虽然它的实施不大有用.
通过`-@`选项,有可能把一系列的文件名管道到 `zip` 命令.

```bash
$ cd
$ find playground -name "file-A" | zip -@ file-A.zip
```

这里我们使用 `find` 命令产生一系列与`file-A`相匹配的文件列表,并且把此列表管道到 `zip` 命令,然后创建包含所选文件的文件包 `file-A.zip`.

`zip` 命令也支持把它的输出写入到标准输出,但是它的使用是有限的,因为很少的程序能利用输出.
不幸地是,`unzip` 程序不接受标准输入.这就阻止了 `zip` 和 `unzip` 一块使用,像 `tar` 命令那样, 来复制网络上的文件.

然而`zip` 命令可以接受标准输入,所以它可以被用来压缩其它程序的输出:

```bash
$ ls -l /etc/ | zip ls-etc.zip -
adding: - (deflated 80%)
```

在这个例子里,我们把 `ls` 命令的输出管道到 `zip` 命令.
像 `tar` 命令,`zip` 命令把末尾的`横杠`解释为 `使用标准输入作为输入文件`

通过指定`-p`选项,`unzip` 程序允许把它的结果发送到标准输出:

```bash
$ unzip -p ls-etc.zip | less
```

我们讨论了一些 `zip`/`unzip` 可以完成的基本操作.
它们两个都有许多选项,其增加了 命令的灵活性,虽然一些选项只针对于特定的平台.

`zip` 和 `unzip` 命令的说明手册都相当不错, 并且包含了有用的实例.
然而,这些程序的主要用途是为了和 `Windows` 系统交换文件,
而不是在 `Linux` 系统中执行压缩和打包操作,`tar` 和 `gzip` 程序在Linux 系统中更受欢迎.

### 7z解压缩

支持的格式

`LZMA2`, `XZ`, `ZIP`, `Zip64`, `CAB`, `RAR` (如果安装了 non-free `p7zip-rar`包),
`ARJ`,  `GZIP`, `BZIP2`, `TAR`, `CPIO`, `RPM`, `ISO`

用法: `7z <command> [<switches>...] <archive_name> [<file_names>...] [<@listfiles...>]`

解压缩,输入密码,并保持目录结构:

`7z x -p1234 filename`

压缩单个文件

`7z a -t7z archive_name filename`

压缩txt中的文件

`7z a -t7z configrc.win.7z @tom.rc_list.win`
`7z a -t7z configrc.linux.7z @tom.rc_list.linux`

`<Commands>`

+ `a` : 添加文件到归档中
+ `b` : Benchmark
+ `d` : 从归档中删除
+ `e` : 从归档中提取(不使用目录名)
+ `h` : 计算文件的 hash 值
+ `i` : 展示支持的格式
+ `l` : 列出归档的内容
+ `rn` : 重命名归档中的文件
+ `t` : 检查归档的完整性
+ `u` : 把文件更新到归档
+ `x` : 提取文件, 使用全路径(也就是保持文件结构)

`<Switches>`

+ `--` : Stop switches parsing
+ `-o{Directory}` : 设置输出目录
+ `-p{Password}` : 设置密码
+ `-r[-|0]` : 递归子目录
+ `-y` : 所有 queries 回答 yes
+ `-t{Type}`设置归档的 type

### 同步文件和目录

维护系统备份的常见策略是保持一个或多个目录与另一个本地系统(通常是某种可移动的存储设备), 或者远端系统中的目录(或多个目录)同步.
例如我们有一个在开发中的网站, 需要经常将它的本地备份与远端网络服务器保持同步. 在类 `Unix` 系统的世界里,能完成此任务且备受人们喜爱的工具是 `rsync`.

这个程序能同步本地与远端的目录,通过使用 `rsync` 远端更新协议,此协议允许 `rsync` 快速地检测两个目录的差异,执行最小量的复制来达到目录间的同步.
比起其它种类的复制程序, 这就使得`rsync` 命令非常快速和高效.唤醒`rsync`: 

```bash
rsync 选项 src dest
```

这里 `src` 和 `dest` 是下列选项之一:

+ 本地文件或目录一个远端文件或目录,以 `[user@]host:path` 的形式存在
+ 远端 `rsync` 服务器,由 `rsync://[user@]host[:port]/path` 指定

注意 `源` 和 `目标` 两者之一必须是本地文件,`rsync` 不支持远端到远端的复制.

让我们试着对一些本地文件使用 `rsync` 命令.首先,清空我们的 `foo` 目录:

```bash
$ rm -rf foo/*
```

下一步,我们同步 `playground` 目录和它在 `foo` 目录中对应的副本

```bash
$ rsync -av playground foo
```

我们使用了`-a` 选项(`递归`和`保护文件属性`)和 `-v` 选项(冗余输出), 从而同步`playground` 目录的内容到 `foo` 目录.
当这个命令执行的时候, 我们将会看到一系列的文件和目录被复制.在最后,我们将看到一条像这样的总结信息:

```bash
sent 135759 bytes received 57870 bytes 387258.00 bytes/sec
total size is 3230 speedup is 0.02
...
```

它说明了复制的数量.

如果再次运行这个命令,我们将会看到不同的结果:

```bash
$ rsync -av playgound foo
building file list ... done
sent 22635 bytes received 20 bytes
total size is 3230 speedup is 0.14
45310.00 bytes/sec
```

注意到这一次的输出没有文件列表.
这是因为 `rsync` 程序检测到目录`~/playground` 和 `~/foo/playground` 之间不存在差异,因此它不需要复制任何数据.
如果我们在 `playground` 目录中修改一个文件,然后 再次运行 `rsync` 命令:

```bash
$ touch playground/dir-099/file-Z
$ rsync -av playground foo
building file list ... done
playground/dir-099/file-Z
```

我们看到 `rsync` 命令检测到更改,并且只复制了更新的文件.

考虑之前`tar` 命令中的例子, 我们再次把此硬盘连接到系统,
它被挂载到`/media/BigDisk` 目录下,我们可以执行一个有用的系统备份了.

首先在外部硬盘上创建一个目录,名为`/backup`,然后使用 `rsync` 程序从我们的系统中复制最重要的数据到外部硬盘上:

```bash
$ mkdir /media/BigDisk/backup
$ sudo rsync -av --delete /etc /home /usr/local /media/BigDisk/backup
```

在这个例子里,我们把`/etc`,`/home`,和`/usr/local` 目录从我们的系统中复制到外部硬盘的`/media/BigDisk/backup`目录上.

我们使用了`--delete` 这个选项,来删除可能在备份设备中已经存在但却不再存在于源设备中的文件, 
(这与我们第一次创建备份无关,但是会在随后的复制操作中发挥作用).

挂载外部驱动器,运行 `rsync` 命令,不断重复这个过程,是一个不错的系统(虽然不理想)备份方式.
当然,别名会对这个操作更有帮助些.我们创建一个别名,并把它添加到`.bashrc` 文件中, 来提供这个特性:

```bash
alias backup='sudo rsync -av --delete /etc /home /usr/local /media/BigDisk/backup'
```

现在我们只需要连接外部驱动器,然后运行 `backup` 命令来完成工作.

### 在网络间使用 rsync 命令

`rsync` 程序的真正好处之一,是它可以被用来在网络间复制文件.毕竟,`rsync` 中的`r`象征着`remote`.
远程复制可以通过两种方法完成.

***
第一个方法要求另一个系统已经安装了 `rsync` 程序,还安装了远程 `shell` 程序,比如 `ssh`.
比方说我们本地网络中的某系统有大量可用的硬盘空间,我们用这个远程系统代替外部驱动器,来执行文件备份操作.

假定远程系统中有一个名为`/backup` 的目录, 其用来存放我们传送的文件,我们这样做:

```bash
$ sudo rsync -av --delete --rsh=ssh /etc /home /usr/local remote-sys:/backup
```

我们对命令做了两处修改,来方便网络间文件复制.

+ 首先,我们添加了`--rsh=ssh` 选项,其指示 `rsync` 使用 `ssh`程序作为它的远程 `shell`.
这样我们就能使用 `ssh` 加密通道,来把数据安全地传送到远程主机中.
+ 其次,在目标路径名前面指定了远端主机的名字(此例中远端主机名为 `remote-sys`),.

***
第二种方式是通过使用 `rsync` 服务器.
`rsync` 可以被配置为一个 守护进程,监听即将到来的同步请求.
这样做通常是为了方便一个远程系统的镜像.

例如, `Red Hat` 软件中心为它的 `Fedora` 发行版,维护着一个巨大的开发中软件包的仓库.
对于软件测试人员, 在发行周期的测试阶段,镜像这些软件集合是非常有帮助的.
因为仓库中的这些文件会频繁地 (通常每天不止一次)改动,定期同步本地镜像更加合理, 而不是大量地拷贝软件仓库.

这些软件库之一被维护在 `Georgia Tech`.
我们可以使用本地`rsync` 程序和`Georgia Tech`的 `rsync` 服务器来镜像它.

```bash
$ mkdir fedora-devel
$ rsync -av -delete rsync://rsync.gtlib.gatech.edu/fedora-linux-core/development/i386/os fedora-devel
```

在这个例子里,我们使用了远端 `rsync` 服务器的 `URI`.
`URI`由协议(`rsync://`),远端主机名(`rsync.gtlib.gatech.edu`),和软件仓库的路径名组成.

拓展阅读

在这里讨论的所有命令的手册文档都相当清楚明白,并且包含了有用的例子. 
另外, `GNU` 版本的 `tar` 命令有一个不错的[在线文档](http://www.gnu.org/software/tar/manual/index.html).

### rsync 帮助页面

#### 说明

        rsync [选项...] 源... [目标]

+ 通过远程 shell 访问. 

         推送: rsync [选项...] [用户@]主机:源... [目标]
         收取: rsync [选项...] 源... [用户@]主机:目标

+ 通过 `rsync` 守护程序访问. 

        推送: rsync [选项...] [用户@]主机::源... [目标]
                    rsync [选项...] rsync://[用户@]主机[:PORT]/源... [目标]
        收取: rsync [选项...] 源... [用户@]主机::目标
                    rsync [选项...] 源... rsync://[用户@]主机[:PORT]/目标

只有一个 `SRC` 参数而没有 `DEST` 参数的用法将列出源文件而不是复制. 

#### 描述

`Rsync` 是一个快速的, 非常通用的文件复制工具.  它可以在本地复制, 通过任何远程 `shell` 同步到/从另一个主机. 
或者从远程`rsync`守护进程复制.   它提供了大量的选项, 允许非常灵活地指定要复制的文件集.  
它因`delta-transfer`算法而闻名, 它只发送源文件和目的地文件之间的差异, 来减少网络数据的使用量. 
`Rsync` 被广泛用于备份和镜像, 并可作为进阶的复制命令用于日常. 

`Rsync` 使用 `quick check` 算法(默认情况下)寻找需要传输的文件, 该算法寻找在体积或最后修改时间上有变化的文件. 
当快速检查表明文件的数据不需要更新时, 指定追踪的其他文件属性若有变化(根据选项), 会直接应用到目的地文件上.

`rsync`的一些附加功能是:

+ 支持复制`links`, `devices`, `owners`, `groups`和`permissions`
+ 类似于`GNU tar` 的`exclude`和`exclude-from`选项
+ `CVS exclude` 模式, 忽略和 `CVS` 相同的文件
+ 可以使用任何透明的远程 `shell`, 包括 `ssh` 或 `rsh`
+ 不需要`超级用户`权限
+ 文件传输的管道化, 以减少延迟
+ 支持匿名或需要认证的`rsync`守护程序(非常适合做镜像)

#### general

`Rsync`可以复制文件到远程主机, 或从远程主机复制文件, 或在当前主机上复制文件(它不支持在两个远程主机之间复制文件). 

有两种不同的方式让`rsync`连接远程系统: 使用远程`shell`程序作为传输方式(如 `ssh` 或 `rsh`), 或通过`TCP`直连`rsync`守护进程.  
当源文件或目标文件的路径含有一个冒号(`:`)的分隔符时, 就会使用远程`shell`传输.  
当源文件或目标文件路径包含双冒号(`::`), 或指定`rsync:// URL`时, 直连`rsync`守护进程. 
参见 `USING RSYNC-DAEMON FEATURES VIA A REMOTE-SHELL CONNECTION`一节, 了解后一条规则的例外情况).

+ 作为特殊情况, 如果指定了单一的源参数而没有目的地, 文件将以类似于 `ls -l` 的输出格式列出. 
+ 正如预期的那样, 如果源路径和目标路径都没有指定一个远程主机, 那么复制就发生在本地(也见 `--list-only`选项). 
+ `Rsync` 将本地端称为 `客户端`, 将远程端称为 `服务器`.  不要把 `服务器` 和 `rsync守护进程`混淆起来
守护进程总是一个`服务器`, 但`服务器`既可以是`守护进程`也可以是远程`shell`生成的`进程`. 

#### 安装设置

参见 `README` 文件中的安装说明. 
你可以通过使用 `-e` 选项或设置 `RSYNC_RSH` 环境变量来指定你喜欢的任何远程 `shell`. 
注意`rsync`必须同时安装在源机器和目的机器上. 

#### 使用方法

使用`rsync`的方式与使用`rcp`相同. 你需要指定一个源和一个目的地, 其中一个可能是远程的. 也许用一些例子来解释最好. 

```bash
rsync -t *.c foo:src/
```

这将把所有与模式 `*.c` 匹配的文件从当前目录转移到机器`foo`上的目录`src`. 
如果文件已经存在于远程系统中, 那么将使用`rsync`远程更新协议, 只发送数据的差异.  
请注意, 通配符(`*.c`)的展开是由`shell`控制的, 发生在运行`rsync`, 而不是由`rsync`本身处理(与所有其他`posix`风格的程序完全一样). 

```bash
rsync -avz foo:src/bar /data/tmp
```

以上递归地将`foo`机器上`src/bar`目录下的所有文件传输到本地机器上的`/data/tmp/bar`目录. 
文件是以 `归档`模式传输的, 这就确保了`符号链接`, `设备`, `属性`, `权限`, `所有权`等在传输过程中被保留下来.  此外, 压缩被用来减少传输的数据部分的大小. 

```bash
rsync -avz foo:src/bar/ /data/tmp
```

源文件上的尾部`/`会改变这一行为, 阻止在`目的地`创建额外的目录级别.  你可以把源文件上的尾部`/`看作是 `复制这个目录的内容`, 而不是 "按名称复制目录", 
但在这两种情况下, `包裹目录`的属性都被转移到目的地的`包裹目录`中. 换句话说, 下面两个命令以同样的方式复制文件, 包括对`/dest/foo`的属性设置. 

```bash
rsync -av /src/foo /dest
rsync -av /src/foo/ /dest/foo
```

还要注意的是, 主机和`module`引用, 不需要尾部的`/`来指定复制默认目录的内容.  例如, 以下都将远程目录的内容复制到`/dest`. 

```bash
rsync -av host: /dest
rsync -av host::module /dest
```

+ 你也可以在纯本地模式下使用`rsync`, 即源文件和目标文件的名称中都没有`:`. 在这种情况下, 它就像增强的拷贝命令. 
+ 最后, 你可以通过省略`module`名称来列出某个`rsync`守护进程中所有可用的`模块`. 

```bash
rsync somehost.mydomain.com::
```

更多细节请看下面的章节. 

#### 举例说明

下面是一些我如何使用`rsync`的例子:

+ 为了备份我妻子的`home`目录, 其中包括许多`MS Word`文件和邮件文件夹, 我建立一个`cron` 任务, 它执行 :

```bash
rsync -Cavz . arvidsjaur:backup
# -C:cvs 忽略规则, a : 归档模式, -v: 增加详细程度, -z: 传输过程中压缩
```

每天晚上它通过`PPP`连接到我机器`arvidsjaur`上的一个备份目录 .

+ 为了同步我的`samba`源文件树, 我使用下面的`Makefile` targets:

```bash
get:
    rsync -avuzb --exclude '*~' samba:samba/ . 
    # -u, --update ; 跳过接收方较新的文件, -b, --backup ; 进行备份
put:
    rsync -Cavuzb . samba:samba/
sync: get put
```

它将与连接另一端的`CVS`目录同步. 然后我在远程机器上进行`CVS`操作, 这节省了很多时间, 因为远程`CVS`协议不是很有效. 

+ 在我的 `旧` 和 `新` ftp 站点之间镜像目录:

```bash
rsync -az -e ssh --delete ~ftp/pub/samba nimbus:"~ftp/pub/tridge"
# --delete: 删除源中不存在的文件. -e: 指定要使用的远程 shell
```

每隔几个小时它从`cron`启动. 

#### 高级用法

从远程主机请求多个文件的语法, 是通过指定额外的远程主机参数来完成的, 样式与第一个相同, 但可以省略主机名.  例如, 以下这些都可以

```bash
rsync -av host:file1 :file2 host:file{3,4} /dest/
rsync -av host::modname/file{1,2} host::modname/file3 /dest/
rsync -av host::modname/file1 ::modname/file{3,4}
```

旧版本的`rsync`要求在`SRC`中使用带引号的空格, 比如这些例子:

```bash
rsync -av host:'dir1/file1 dir2/file2' /dest
rsync host::'modname/dir1/file1 modname/dir2/file2' /dest
```

这种分词法在最新的`rsync`中仍然有效(默认情况下), 但不像第一种方法那样容易使用. 

如果你需要传输一个含有空格的文件名, 你可以指定 `--protect-args` (`-s`) 选项, 或者你需要以远程`shell`能够理解的方式来转义空格.  比如说

```bash
rsync -av host:'file\ name\ with\ spaces' /dest
```

#### 连接到一个rsync守护进程

也可以使用`rsync`而不使用远程`shell`作为传输工具.  在这种情况下, 你将直接连接到一个远程`rsync`守护进程, 通常使用`TCP 873`端口.  
(这显然需要在远程系统上运行守护进程, 所以请参考下面的`STARTING AN RSYNC DAEMON TO ACCEPT CONNECTIONS`一节来了解这方面的信息). 

以这种方式使用`rsync`, 与在远程`shell`使用`rsync`是一样的, 除了:

+ 使用双冒号`::` 而不是单冒号来分隔主机名和路径, 或者使用`rsync:// URL`. 
+ `路径` 的第一个词实际上是`模块`的名字. 
+ 当你连接时, 远程守护进程可能会打印当天的信息. 
+ 如果您在远程守护进程中没有指定路径名称, 那么将显示守护进程中的可访问路径列表. 
+ 如果你没有指定本地目标, 那么将打印指定的远程文件列表. 
+ 你不能使用`--rsh` (`-e`) 选项. 

拷贝远程模块 `src` 中所有文件的例子. 

```bash
rsync -av host::src /dest
```

远程守护程序上的某些模块可能需要认证. 如果是这样, 你在连接时将会收到一个密码提示. 
您可以通过设置环境变量 `RSYNC_PASSWORD` 为密码来省略密码提示.或使用`--password-file`选项来避免密码提示. 
这在编写`rsync`脚本时可能很有用. 

警告: 在某些系统中, 环境变量对所有用户都是可见的. 在这些系统中, 建议使用`--password-file`. 

+ 你可以通过设置环境变量`RSYNC_PROXY`指向你的网络代理的`主机名:端口`对来建立连接.  注意, 你的网络代理的配置必须支持代理连接到`873`端口. 
+ 你也可以通过设置环境变量`RSYNC_CONNECT_PROG`为你想运行的命令, 使用程序作为代理来建立连接, 以代替`socket`直连. 
这个字符串可以包含转义字符 `%H` 来表示在`rsync`命令中指定的主机名(所以如果你需要在字符串中使用单个`%`, 就使用`%%`).  比如说

```bash
export RSYNC_CONNECT_PROG='ssh proxyhost nc %H 873'
rsync -av targethost1::module/src/ /dest/
rsync -av rsync:://targethost2/module/src/ /dest/
```

以上命令使用`ssh`在`proxyhost`上运行`nc`(`netcat`), `nc`将所有数据转发到目标主机(`%H`)上的`873`端口(`rsync`守护程序). 

#### 通过远程shell连接使用rsync-daemon功能

有时使用`rsync`守护进程的各种功能(如命名模块)而不允许任何新的`socket`连接到系统中是很有用的(除了允许远程`shell` 访问). 
`Rsync`支持使用远程`shell`连接到主机, 然后生成一次性的 `daemon`服务器, 并期望在远程用户的家目录中读取其配置文件. 

如果你想加密一个`守护进程式`的传输数据, 但由于`守护进程`是由远程用户重新启动的, 你可能无法使用`chroot` 或改变守护进程的`UID`等功能,.  
(对于另一种加密`守护进程`的方法, 考虑使用`ssh`隧道将`本地端口`连接到`远程主机`. 并在`远程主机`上配置普通的`rsync`守护进程, 只允许`rsync`连接到 `localhost`). 

从用户的角度来看, 通过`远程shell`连接进行的`守护进程`传输, 使用的命令行语法与普通 `rsync-daemon传输`几乎相同, 
唯一的例外是你需要用选项 `--rsh=COMMAND` 明确地设置远程`shell`程序.  (在环境中设置`RSYNC_RSH`不会开启这个功能).  比如说

```bash
rsync -av --rsh=ssh host::module /dest
```

如果你需要指定不同的`远程shell`用户, 请在`host`前面使用`user@`前缀来指定`rsync-user`(对于需要使用用户认证的`模块`).  
这意味着你必须在指定`远程shell`时给`ssh`提供`-l user`选项, 例如下面的例子, 其中使用`--rsh`选项的缩写`-e`

```bash
rsync -av -e "ssh -l ssh-user" rsync-user@host::module /dest
```

`ssh-user` 将在`ssh`级别使用；`rsync-user`将用于登录 `模块`. 

#### 启动rsync守护进程以接受连接

为了连接到`rsync`守护进程, 远程系统需要有一个已经运行的守护进程
(或者它需要配置像`inetd`这样的东西来生成`rsync`守护进程, 以便为特定端口上传入的连接 spawn `rsync`守护进程).

关于如何启动处理 incoming `socket`连接的守护进程的完整信息, 请参见`rsyncd.conf(5)`手册页 -- 它是守护进程的配置文件, 
它包含如何运行守护进程的全部细节(包括`独立的`和`inetd`的配置). 

如果你使用其中远程`shell`传输工具进行传输, 就不需要手动启动`rsync`守护进程. 

#### 排序的传输顺序

`Rsync`总是将指定的文件名排序到它的内部传输列表.  这可以处理同名目录内容的合并, 使其容易删除重复的文件名. 
当文件的传输顺序与命令行上给出的顺序不同时, 可能会使某人感到困惑. 

如果你需要一个特定的文件先于其他文件传输, 可以使用独立的`rsync`调用, 也可以考虑使用`--delay-updates`.
这不会影响排序的传输顺序, 但会使最后的`文件更新`阶段更快发生). 

#### 选项总结

这里是`rsync`中可用选项的简短总结. 请参考下面的详细说明以获得完整的描述. 

+ `-v, --verbose` ;  增加详细程度.
  + `--info=FLAGS` ; 详细的, 较多信息的 verbosity.
  + `--debug=FLAGS`; 详细的, debug verbosity.
  + `--msgs2stderr` ; 用于调试的特殊输出处理
+ `--q, --quiet` ; 抑制非错误信息
  + `--no-motd` ; 抑制守护进程模式的`MOTD`(见注意事项). 
+ `-c, --checksum` ;  根据校验和跳过, 而不是根据`mod-time`和`size`. 
+ `-a, --archive` ; 归档模式；等于`-rlptgoD`(没有`-H`,`-A`,`-X`)
  + `--no-OPTION` 关掉一个隐含的`OPTION`(例如: `--no-D`)

`-r`; 递归. `-l`; 符号连接. `-p`; 保留权限. `-t`; 保留修改时间. `-go`; 组和所有者. `-D`; 设备文件和特殊文件.

+ `-r, --recursive` ; 递归到目录中去.
+ `-R, --relative` ; 使用相对路径名
  + `--no-implied-dirs` ; 不发送带有`--relative`的隐含目录. 
+ `-b, --backup`; 进行备份 (见 `--suffix` & `--backup-dir`). 
  + `--backup-dir=DIR` ; 在`DIR`的基础上进行备份, 使其成为层次结构. 
  + `--suffix=SUFFIX` ; 备份后缀. (默认`~` without `--backup-dir`). 
+ `-u, --update` ; 跳过`接收方`较新的文件
  + `--inplace` ; 就地更新目标文件
  + `--append` ; 将数据附加到较短的文件上
  + `--append-verify` ; --append w/old data in file checksum.

+ `-d, --dirs` ; 传输目录, 不进行递归.
+ `-l, --links` ; 将`符号链接`复制为`符号链接`
+ `-L, --copy-links` ; 将符号链接转换成指向的`文件`/`目录`
  + `--copy-unsafe-links` ; 只有 `不安全` 的符号链接转换成文件. 
  + `--safe-links` ; 忽略指向树外的`符号链接`
  + `--munge-links` ; 对符号链接进行整合, 使其更加安全. 
+ `-k, --copy-dirlinks` ; 将指向`dir`的符号链接转化为`dir`.
+ `-K, --keep-dirlinks` ; 将`接收方`的符号链接`dir`视为真实的`dir`. 
+ `-H, --hard-links` ; 保留硬链接
+ `-p, --perms` ; 保留权限
+ `-E, --executability` ; 保留可执行性
  + `--chmod=CHMOD` 影响文件和/或目录权限
+ ` -A, --acls` ; 保留 `ACLs` (暗示 `-p`)
+ `-X, --xattrs` ; 保留扩展属性
+ `-o, --owner` ; 保留所有者(仅超级用户). 
+ `-g, --group` ;  保留组
  + `--devices` ; 保留设备文件(仅超级用户)
  + `--specials` ; 保留特殊文件
+ `-D` ; 与`--devices --specials`相同
+ `-t, --times` ; 保留修改时间
+ `-O, --omit-dir-times` ; 从 `--times` 中省略目录.
+ `-J, --omit-link-times` ; 从`--times`中省略符号链接
  + `--super` ; 接收方尝试超级用户活动
  + `--fake-super` 使用`xattrs`存储/恢复特权属性
+ `-S, --sparse` ; 将`null`序列转换成`稀疏块`
  + `--preallocate` ; 在写入前分配目标文件
+ `-n, --dry-run` ; 执行试运行, 不做任何修改

+ `--W, --whole-file` ; 完整复制文件(不采用`delta-xfer`算法). 
  + `--checksum-choice=STR` ; 选择校验算法
+ `-x, --one-file-system` ; 不跨越文件系统的边界
+ `-B, --block-size=SIZE` ; 强制采用固定的校验块大小

+ `-e, --rsh=COMMAND` ; 指定要使用的远程 `shell`
  + `--rsync-path=PROGRAM` ; 指定要在远程机器上运行的`rsync`
  + `--existing` ; 跳过在接收器上创建新文件
  + `--ignore-existing` ; 跳过更新接收方存在的文件
  + `--remove-source-files` ; 发送方删除同步过的文件(非目录)
  + `--del` ; 是 `--delete-during` 的别名
  + `--delete` ; 删除目的地目录中不相干的文件
  + `--delete-before` ; 接收方在发送前删除, 而不是在发送过程中删除. 
  + `--delete-during` ; 接收者在传输过程中删除文件
  + `--delete-delay` ; 在传输过程中找出删除, 在传输后删除
  + `--delete-after` ; 接收者在转移后删除, 而不是在转移过程中. 
  + `--delete-excluded` ; 从目的地目录中删除被排除的文件
  + `--ignore-missing-args` ; 忽略丢失的源参数, 不报错
  + `--delete-missing-args` ; 从目的地删除缺失的源文件`args`
  + `--ignore-errors` ; 即使有`I/O`错误, 也删除. 
  + `--force` ; 强制删除目录, 即使不是空的. 
  + `--max-delete=NUM` ; 不删除超过`NUM`的文件
  + `--max-size=SIZE` ; 不传输任何大于`SIZE`的文件
  + `--min-size=SIZE` ; 不传输任何小于`SIZE`的文件
  + `--partial` ; 保留部分传输的文件
  + `--partial-dir=DIR` ; 将部分传输的文件放入`DIR`中
  + `--delay-updates` ; 最后再把所有更新的文件放到地方

+ `--m, --prune-empty-dirs` ; 从文件列表中删去空目录链
  + `--numeric-ids` ; 不按`用户`/`组`名映射`uid`/`gid`值. 
  + `--usermap=STRING` ; 自定义用户名映射
  + `--groupmap=STRING` ; 自定义组名映射
  + `--chown=USER:GROUP` ; 简单的用户名/组名映射
  + `--timeout=SECONDS` ; 设置`I/O`超时, 单位为`秒`
  + `--contimeout=SECONDS` ; 设置守护程序连接超时, 单位为`秒`
+ `--I, --ignore-times` ; 不跳过大小和时间相符的文件
  + `--size-only` ; 跳过大小一致的文件
+ `-@, --modify-window=NUM` ;  设置`mod-time`比较的准确性
+ `-T, --temp-dir=DIR` 在目录DIR中创建临时文件
+ `-y, --fuzzy` ; 在没有目标文件的情况下为`basis`找到类似的文件
  + `--compare-dest=DIR` ; also compare received files relative to `DIR`.
  + `--copy-dest=DIR` ; ... and include copies of unchanged files
  + `--link-dest=DIR` ; 当未改变时, 硬链接到`DIR`中的文件

+ `-z, --compress` ; 在传输过程中压缩文件数据
  + `--compress-level=NUM` ; 明确设置压缩级别
  + `--skip-compress=LIST` ; 跳过压缩后缀为`LIST`的文件
+ `--C, --cvs-exclude` ; 以`CVS`的方式自动忽略文件
+ `-f, --filter=RULE` ; 添加一个文件过滤规则.
+ `-F` ; 与 `--filter='dir-merge /.rsync-filter'`相同
  + `repeated`: `--filter='- .rsync-filter'`.
  + `--exclude=PATTERN` ; 排除匹配`PATTERN`的文件
  + `--exclude-from=FILE` ; 从`FILE`读取排除模式
  + `--include=PATTERN` ; 不排除匹配`PATTERN`的文件
  + `--include-from=FILE` ; 从`FILE`中读取包含模式
  + `--files-from=FILE` ; 从`FILE`中读取源文件名的列表

+ `-0, --from0` ; 所有的`*from/filter`文件都以`0`为界. 
+ `-s, --protect-args` ; 不按空格分词；只有通配符. 
+ `--address=ADDRESS` ; 绑定出站`socket`的地址给守护进程. 
  + `--port=PORT` ; 指定双冒号的备用端口号
  + `--sockopts=OPTIONS` ; 指定自定义`TCP`选项
  + `--blocking-io` ; 对远程`shell`使用阻塞式`I/O`
  + `--outbuf=N|L|B` ; 设置输出缓冲为`无`, `行`或`块`. 
  + `--stats` ; 提供一些文件传输的统计信息.

+ `--8, --8-bit-output` ; 在输出中不对高位字符进行转义. 
+ `-h,--human-readable` ; 以人类可读的格式输出数字
  + `--progress` ; 显示传输过程中的进度
+ `-P` ; 与 `--partial --progress` 相同
+ `-i, --itemize-changes`; 输出所有更新的变化摘要
+ `-M, --remote-option=OPTION` ; 只向远端发送`OPTION`
  + `--out-format=FORMAT` ; 使用指定的`FORMAT`输出更新信息
  + `--log-file=FILE` ; 在`FILE`中记录我们正在做的事情
  + `--log-file-format=FMT` ; 使用指定的`FMT`记录更新信息
  + `--password-file=FILE` ; 从`FILE`读取 daemon-access 的访问密码
  + `--list-only` ; 列出文件而不是复制它们
  + `--bwlimit=RATE` ; 限制 socket `I/O`带宽
  + `--stop-at=y-m-dTh:m` ; 在`year-month-dayThour:minute`停止`rsync`
  + `--time-limit=MINS` ; 在`MINS`分钟后停止`rsync`的运行
  + `--write-batch=FILE` ; 将批量更新写入`FILE`中
  + `--only-write-batch=FILE` ; 类似`--write-batch` 但不更新`dest`
  + `--read-batch=FILE` ; 从`FILE`中读取一个批处理的更新
  + `--protocol=NUM` ; 强制使用一个较早的协议版本
  + `--iconv=CONVERT_SPEC` ; 要求转换文件名的字符集
  + `--checksum-seed=NUM` ; 设置`块`/`文件`checksum seed(高级). 
  + `--noatime` ; 打开源文件时不改变`atime`

+ `--4, --ipv4` ; 偏好`IPv4`
+ `-6, --ipv6` ; 偏好`IPv6`
+ `--version` ;  打印版本号
+ `(-h) --help` 显示此帮助(关于`-h`的注释见下文). 

` `也可以作为一个守护进程运行, 在这种情况下, 可以接受以下选项. 

+ `--daemon` ; 作为`rsync`守护进程运行
  + `--address=ADDRESS` ; 绑定到指定地址
  + `--bwlimit=RATE` ; 限制 socket `I/O` 带宽
  + `--config=FILE` ; 指定备用的`rsyncd.conf `文件
+ `--M, --dparam=OVERRIDE` ; 覆盖全局守护进程配置参数
  + `--no-detach` ; 不从parent中分离出来
  + `--port=PORT` ; 在备用端口号上监听
  + `--log-file=FILE` ; 覆盖 `日志文件` 设置
  + `--log-file-format=FMT` ; 覆盖 `日志格式` 的设置
  + `--sockopts=OPTIONS` ; 指定自定义`TCP`选项

+ `-v, --verbose` ; 增加 verbosity
+ `-4, --ipv4` ; 优先选择`IPv4`
+ `-6, --ipv6` ; 偏好`IPv6`
+ `-h, --help` ; 显示此帮助(如果在 `--daemon` 之后使用). 

#### 选项

`Rsync`接受长(双破折号+字)和短(单破折号+字母)选项.  可用选项的完整列表在下面描述.  
如果`选项`可以用多种方式指定, 则选择应该用`逗号`隔开.   有些选项只有`长选项`, 没有`短选项`.  

如果选项需要参数, 则参数说明只列在长选项后面, 尽管也必须为短选项指定.  当指定一个参数时, 你可以使用 `--option=param` 的形式, 或者用空格代替`=`.  
该参数可能需要以某种方式加`引号`, 以避免被`shell`命令行解析掉.   
请记住文件名的领头`~`会被你的`shell`替换掉, 所以`--option=~/foo`不会把`tilde`改成你的家目录(要想跳转到家目录的话, 请去掉`=`). 

`--help` 打印一个简短的帮助页面, 描述`rsync`中可用的选项并退出.  为了向后兼容旧版本的`rsync`, 如果你使用`-h`选项而不使用任何其他参数, 也会输出帮助. 

+ `--delete`;

这告诉`rsync`从接收端删除不相干的文件(那些不在发送端上的文件), 但只针对被同步的目录.  
你必须要求`rsync`发送整个目录(例如 `dir`或 `dir/`), 而不是用目录内容的通配符(例如 `dir/*`), 
因为通配符会被`shell`展开, 因此`rsync`会得到一个传输单个文件的请求, 而不是文件的父目录. 

被排除在传输之外的文件也不会被删除, 除非你使用 `--delete-excluded` 选项或将规则标记为只在发送方匹配(见`FILTER RULES`部分的`include/exclude modifiers`). 

在`rsync 2.6.7`之前, 除非启用 `--recursive`, 否则该选项没有任何作用.  
从`2.6.7`开始, 当启用`--dirs` (`-d`)时, 删除也会发生, 但只对内容被复制的目录被复制的目录. 
如果使用不当, 这个选项可能会很危险!  首先尝试使用 `--dry-run` 选项(`-n`)进行运行, 看看哪些文件将被删除. 

如果发送方检测到任何`I/O`错误, 那么将自动禁止删除目的地的任何文件. 这是为了防止发送方的临时文件系统故障(如`NFS`错误)导致目的地的文件被大量删除. 
 你可以用`--ignore-errors`选项来覆盖这一点. 

`--delete`选项可以和`--delete-WHEN`选项以及`--delete-excluded`选项结合使用, 不会发生冲突.  
然而, 如果没有指定`--delete-WHEN`选项, 当使用`rsync 3.0.0`或更新的版本时, `rsync`将选择`--delete-during`算法, 
而使用较早版本的`rsync`时, 将选择`--delete-before`算法.  

## 第二十章:正则表达式

追溯到 `Unix` 刚刚开发的时候,它只知道 `ASCII` 字符. 在 `ASCII` 中,前`32`个字符 (数字`0-31`)都是控制码(如 `tabs`,`backspaces`,和`回车`). 
随后的`32`个字符(`32-63`)包含可打印的字符, 包括大多数的标点符号和数字`0`到`9`. 
再随后的`32`个字符(`64-95`)包含大写字符和一些更多的标点符号.  最后的`31`个字符(`96-127`)包含小写字母和更多的标点符号. 
基于这种安排方式,系统使用这种排序规则 的 `ASCII`:

```bash
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
```

而普通的字典顺序像这样:

```bash
aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
```

随着 `Unix` 系统的知名度在美国之外的国家传播开来,就需要支持不在 U.S.英语范围内的字符.  
于是就扩展了`ASCII` 字符表,使用了整个`8`位,添加了字符(数字`128-255`),这样就 容纳了更多的语言. 
为了支持这种能力,`POSIX` 标准介绍了一种叫做 `locale` 的概念,其可以被调整,来为某个特殊的区域, 选择所需的字符集. 通过使用下面这个命令,我们能够查看到我们系统的语言设置:

```bash
[me@linuxbox ~]$ echo $LANG
en_US.UTF-8
```

通过这个设置,`POSIX` 相容的应用程序将会使用字典排列顺序而不是 `ASCII` 顺序. 当`[A-Z]`字符区域按照字典顺序解释的时候,包含除了小写字母`a`之外的所有字母. 
为了部分地解决这个问题,`POSIX` 标准包含了大量的字符集,其提供了有用的字符区域.  下表中描述了它们:

表20-2: POSIX 字符集
字符集 说明

+ `[:alnum:]`:   字母数字字符. 在 `ASCII` 中,等价于:`[A-Za-z0-9]`
+ `[:word:]`:   与`[:alnum:]`相同, 但增加了下划线字符. 
+ `[:alpha:]`:   字母字符. 在 `ASCII` 中,等价于:`[A-Za-z]`
+ `[:blank:]`:   包含`空格`和 `tab` 字符. 
+ `[:cntrl:]`:   `ASCII` 的控制码. 包含了`0`到`31`,和`127`的 `ASCII` 字符. 
+ `[:digit:]`:   数字`0`到`9`
+ `[:graph:]`: 可视字符. 在 `ASCII` 中,它包含`33`到`126`的字符. 
+ `[:lower:]`:   小写字母. 
+ `[:punct:]`:   标点符号字符. 在 `ASCII` 中
+ `[:print:]`:   可打印的字符. 在`[:graph:]`中的所有字符,再加上`空格`字符. 
+ `[:space:]`:   空白字符,包括`空格`,`tab`,`回车`,`换行`,`vertical tab`, 和 `form feed`.在`ASCII` 中, 等价于:`[ \t\r\n\v\f]`
+ `[:upper:]`:   大写字母. 
+ `[:xdigit:]`: 用来表示十六进制数字的字符. 在`ASCII` 中,等价于:`[0-9A-Fa-f]`

### 用 find 筛选不规范文件名

`find` 命令支持一个基于正则表达式的测试. 
当某一行包含的字符串匹配上了一个表达式的时候,`grep` 命令会打印出这一行,然而 `find` 命令要求路径名精确地匹配这个正则表达式. 
在下面的例子里面,我们将使用带有一个正则表达式的 `find` 命令,来查找每个路径名,其包含的任意字符都不是以下字符集中的一员. 

```bash
[-\_./0-9a-zA-Z]
```

这样一种扫描会发现包含空格和其它潜在不规范字符的路径名:

```bash
[me@linuxbox ~]$ find . -regex '.*[^-\_./0-9a-zA-Z].*'
```

由于要精确地匹配整个路径名,所以我们在表达式的两端使用了`.*`来匹配零个或多个字符.  在表达式中间,我们使用了否定的中括号表达式,其包含了我们一系列可接受的路径名字符. 

### 用 locate 查找文件

`locate` 程序支持基本的(`--regexp` 选项)和扩展的(`--regex` 选项)正则表达式. 

```bash
[me@linuxbox ~]$ locate --regex 'bin/(bz|gz|zip)'
```

通过使用 `alternation`,我们搜索包含 `bin/bz`,`bin/gz`,或`/bin/zip` 字符串的路径名. 

## 第二十一章:文本处理

### cat

`cat` 程序具有许多有趣的选项. 其中许多选项用来帮助更好的可视化文本内容. 
一个例子是`cat -A`可以用来显示文本中的非打印字符.
`tab` 字符在我们的文本中由 `^I` 字符来表示. 这是一种常见的表示方法,意思是`Control-I`,结果证明,它和 `tab`字符是一样的

`cat` 程序也包含用来修改文本的选项. 
最著名的两个选项是`-n`,其给文本行添加行号. 以及`-s`, 禁止输出多行空白. 

### sort

`sort` 程序对标准输入或命令行中指定的一个或多个文件进行排序,然后把排序结果发送到标准输出. 
`sort` 程序能接受命令行中的多个文件作为参数,所以有可能把多个文件合并成一个有序的文件. 例如:

```bash
sort file1.txt file2.txt file3.txt > final_sorted_list.txt
```

`sort` 程序有几个有趣的选项. 这里只是一部分列表:
表21-1: 常见的 sort 程序选项
选项 长选项 描述

`-b`,`--ignore-leading-blanks`:默认情况下,排序从每行的第一个字符开始. 这个选项导致 `sort` 程序忽略空格,从第一个非空白字符开始排序. 
`-f`,`--ignore-case`:让排序不区分大小写. 
`-n`:`--numeric-sort`:默认基于字符串的长度来排序. 使用此选项允许根据数字值执行排序,而不是字母值. 
`-r`,`--reverse`:  按相反顺序排序. 结果按照降序排列,而不是升序. 
`-k`,`--key=field1[,field2]` 对从 `field1` 到 `field2` 之间的字符排序,而不是整个文本行. 看下面的讨论. 
`-m`,`--merge`: 把每个参数看作是一个预先排好序的文件. 把多个文件合并成一个排好序的文件,而没有执行额外的排序. 
`-o`,`--output=file`: 把排好序的输出结果发送到文件,而不是标准输出. 
`-t`,`--field-separator=char`: 定义域分隔字符. 默认情况下,域由空格或制表符分隔. 

首先,让我们看一下 `-n` 选项,被用做数值排序. 通过这个选项,可以基于数值进行排序. `du` 命令可以确定最大的磁盘空间用户. 通常 `du` 命令列出的输出结果按照路径名来排序:

```bash
[me@linuxbox ~]$ du -s /usr/share/* | head
68K  /usr/share/a11y-profile-manager
28K  /usr/share/accounts
40K  /usr/share/accountsservice
...
```

我们能够产生一个按数值排序的列表,来显示 `10` 个最大的空间占据者:

```bash
[me@linuxbox ~]$ du -s /usr/share/* | sort -nr | head
1020K  /usr/share/speech-dispatcher
1008K  /usr/share/rubygems-integration
972K  /usr/share/pandoc
...
```

通过使用`-nr` 选项,我们产生了一个反向的数值排序,最大数值排列在第一位. 这种排序起作用是因为数值出现在每行的开头. 
但是如果我们想要基于文件行中的某个数值排序,又会怎样呢? 例如,命令 `ls -l` 的输出结果:

```bash
[me@linuxbox ~]$ ls -lh /usr/bin | head
-rwxr-xr-x 1 root   root      59K 9月   5  2019 [
-rwxr-xr-x 1 root   root       96 3月   8 21:02 2to3-2.7
-rwxr-xr-x 1 root   root     9.9K 4月  23  2016 411toppm
...
```

此刻,忽略 `ls` 程序也能按照文件大小排序,我们使用 `sort` 程序来完成此任务:

```bash
[me@linuxbox ~]$ ls -l /usr/bin | sort -nr -k 5 | head
-rwxr-xr-x 1 root   root    1023K 3月  26  2020 x86_64-linux-gnu-gcc-7
-rwxr-xr-x 1 root   root     1019 4月  20  2020 wine-stable
-rwxr-xr-x 1 root   root     1007 8月  22  2020 dvipdf
...
```

`sort` 程序的许多用法都涉及到处理表格数据,例如上面 `ls` 命令的输出结果. 
如果我们把数据库的术语应用到上面的表格中,每行是一条记录,并且每条记录由多个字段组成, 例如文件属性,链接数,文件名,文件大小等等. 
`sort` 程序能够处理独立的字段. 在数据库术语中, 我们能够指定一个或者多个关键字段,来作为排序的关键值. 
在上面的例子中,我们指定 `n` 和 `r` 选项来执行相反的数值排序,并且指定 `-k 5`,让 `sort` 程序使用第五字段作为排序的关键值. 

`sort` 程序允许多个 `-k` 选项的实例,所以可以指定多个排序关键值. 下面是多键值排序的语法:

```bash
[me@linuxbox ~]$ sort --key=1,1 --key=2n distros.txt
```

虽然为了清晰,我们使用了选项的长格式,但是 `-k 1,1 -k 2n` 格式是等价的. `1,1`意味着始于并且结束于第一个字段. 
在第二个实例中,我们指定了 `2n`,表示按第二个字段排序, 并且按照数值排序. 键值说明符的末尾可以附上一个选项字母, 用来指定排序的种类. 
这些选项字母和 `sort` 程序的全局选项一样:`b`(忽略开头的空格),`n`(数值排序),`r`(逆向排序),等等. 

对于日期格式, 在计算机中,日期通常设置为 `YYYY-MM-DD` 格式, 这样按时间顺序排序变得容易,但有时我们的日期为`MM/DD/YYYY`的格式. 
幸运地是, `key` 选项允许在字段中指定偏移量,所以我们能在字段中定义键值. 

[me@linuxbox ~]$ sort -k 3.7nbr -k 3.1nbr -k 3.4nbr distros.txt

通过指定 `-k 3.7`,我们指示 `sort` 程序使用一个排序键值,其始于第三个字段中的第七个字符,对应于年的开头. 
同样地,我们指定 `-k 3.1` 和 `-k 3.4` 来分离日期中的月和日.  我们也添加了 `n` 和 `r` 选项来实现一个逆向的数值排序. 
`b` 选项用来删除日期字段中开头的空格( 行与行之间的空格数迥异,因此会影响 `sort` 程序的输出结果). 

一些文件不会使用 `tabs` 和`空格`做为字段界定符;例如, `/etc/passwd` 文件.`sort` 程序提供 了一个 `-t` 选项来定义分隔符. 
按照第七个字段(帐户的默认 `shell`)来排序此 `passwd` 文件,我们可以这样做:

```bash
[me@linuxbox ~]$ sort -t ':' -k 7 /etc/passwd | head
```

### uniq

与 `sort` 程序相比,`uniq` 程序是个轻量级程序.  `uniq` 执行一个看似琐碎的认为. 当给定一个排好序的文件(包括标准输出),`uniq` 会删除任意重复行,并且把结果发送到标准输出.  
它常常和 `sort` 程序一块使用,来清理重复的输出. 
`uniq` 程序是一个传统的 `Unix` 工具,经常与 `sort` 程序一块使用, `GNU` 版本的 `sort` 程序支持一个 `-u` 选项,其可以从排好序的输出结果中删除重复行.  `uniq` 只会删除相邻的重复行.

表21-2: 常用的 `uniq` 选项
选项 说明

+ `-c` 输出所有的重复行,并且每行开头显示重复的次数. 
+ `-d` 只输出重复行,而不是特有的文本行. 
+ `-f n` 忽略每行开头的 `n` 个字段,字段之间由空格分隔, 不同于 `sort` 程序, `uniq` 没有选项来设置备用的字段分隔符. 
+ `-i` 在比较文本行的时候忽略大小写. 
+ `-s n` 跳过(忽略)每行开头的 `n` 个字符. 
+ `-u` 只是输出独有的文本行. 这是默认的

### 切片和切块

下面我们将要讨论的三个程序用来从文件中获得文本列,并且以有用的方式重组它们. 

### cut

`cut` 程序被用来从文本行中抽取文本,并把其输出到标准输出. 它能够接受多个文件参数或者 标准输入. 从文本行中指定要抽取的文本有些麻烦,使用以下选项:

表21-3: cut 程序选择项
选项 说明

+ `-c char_list` 从文本行中抽取由 `char_list` 定义的文本. 这个列表由一个或多个逗号分隔开的数值区间组成. 
+ `-f field_list` 从文本行中抽取一个或多个由 `field_list`定义的字段. 列表包括一个或多个字段,或由逗号分隔开的字段区间. 
+ `-d delim_char` 当指定`-f` 选项之后,使用 `delim_char`做为字段分隔符. 默认情况下, 字段之间必须由单个 `tab` 字符分隔开. 
+ `--complement` 抽取整个文本行,除了那些由`-c` 和/或`-f` 选项指定的文本. 

正如我们所看到的,`cut` 程序抽取文本的方式相当不灵活. `cut` 命令最好用来从其它程序产生的文件中抽取文本,而不是从人们直接输入的文本中抽取, 例子为:

```bash
cut -f 3 distros.txt #抽取第三个字段
cut -f 3 distros.txt | cut -c 7-10 # 再抽取位置为7到10的字符
```

`Coreutils` 软件包有一个工具`expand`可以将`tab`转换成相应数目的空格, `unexpand`把空格替换成`tab`. 使用 `-d` 选项,我们能够指定冒号做为字段分隔符. 

```bash
[me@linuxbox ~]$ cut -d ':' -f 1 /etc/passwd | head
```

### paste

`paste` 命令的功能正好与 `cut` 相反. 它会添加一个或多个文本列到文件中. 它通过读取多个文件,然后把每个文件中顺序对应的行合并, 写入标准输出. 

### join

在某些方面,`join` 命令类似于 `paste`,它会往文件中添加列,但是它使用了独特的方法来完成.  
`join`会合并具有相同`key`的条目. 例如下面两个文件

```bash
[me@linuxbox ~]$ head distros-key-names.txt
11/25/2008 Fedora
10/30/2008 Ubuntu
06/19/2008 SUSE

[me@linuxbox ~]$ head distros-key-vernums.txt
11/25/2008 10
10/30/2008 8.10
06/19/2008 11.0
```

使用`join`可以把它们连接起来:

```bash
[me@linuxbox ~]$ join distros-key-names.txt distros-key-vernums.txt | head
11/25/2008 Fedora 10
10/30/2008 Ubuntu 8.10
06/19/2008 SUSE 11.0
```

现在我们有两个具有共享`key`( `发行日期` )的文件. 有必要指出,为了使 `join` 命令 能正常工作, 所有文件必须按照`key`排序. 
默认情况下,`join` 命令使用空白字符做为输入字段的界定符,`一个空格`作为输出字段的界定符.

### 比较文本

通常比较文本文件的版本很有帮助. 对于系统管理员和软件开发者来说,这个尤为重要.  一名系统管理员可能需要拿现有的配置文件与先前的版本做比较,来诊断一个系统错误.  
同样的,一名程序员经常需要查看程序的修改. 

### comm

`comm` 程序会比较两个文本文件,并且会显示每个文件特有的文本行和共有的文本行. 

```bash
[me@linuxbox ~]$ comm file1.txt file2.txt
```

`comm` 命令产生三列输出. 第一列包含第一个文件独有的文本行;第二列是第二个文件独有的;第三列包含两个文件共有的文本行. 
`comm` 支持 `-n` 形式的选项,这里 `n` 代表 `1`,`2` 或 `3`. 这些选项使用的时候,指定了要隐藏的列. 
例如,如果我们只想输出两个文件共享的文本行, 我们将隐藏第一列和第二列的输出结果:

```bash
[me@linuxbox ~]$ comm -12 file1.txt file2.txt
```

### diff

类似于 `comm` 程序,`diff` 程序被用来监测文件之间的差异. 然而,`diff` 是一款更加复杂的工具,它支持许多输出格式,并且一次能处理许多文本文件. 
软件开发员经常使用 `diff` 程序来检查不同程序源码版本之间的更改,`diff` 能够递归地检查源码目录,经常称之为源码树. 
`diff` 程序的一个常见用例是创建 `diff` 文件或者补丁,它会被其它程序使用,例如 `patch` 程序(我们一会儿讨论),来把文件从一个版本转换为另一个版本. 
如果我们使用 `diff` 程序,来查看我们之前的文件实例:

```bash
[me@linuxbox ~]$ diff file1.txt file2.txt
1d0
< a
4a4
> e
```

我们看到 `diff` 程序的默认输出风格:对两个文件之间差异的简短描述. 在默认格式中, 每组的更改之前都是一个更改命令,其形式为 `range operation range` , 
用来描述要求更改的位置和类型,从而把第一个文件转变为第二个文件:

表21-4: `diff` 更改命令
改变 说明

+ `r1ar2`: `add`, 把第二个文件中位置 `r2` 处的文件行添加到第一个文件中的 `r1` 处, 
+ `r1cr2`: `cover`, 用第二个文件中位置 `r2` 处的文本行覆盖位置 `r1` 处的文本行
+ `r1dr2`:`delete`, 删除第一个文件中位置 `r1` 处的文本行,这些文本行对应第二个文件中位置 `r2` 处

在这种格式中,一个范围就是由逗号分隔开的开头行和结束行的列表. 
然这种格式是默认情况(主要是为了服从 `POSIX` 标准且向后与传统的 `Unix diff` 命令兼容), 但是它并不像其它可选格式一样被广泛地使用. 
最流行的两种格式是上下文模式和统一模式. 当使用上下文模式(带上` -c` 选项),我们将看到这些:

```bash
[me@linuxbox ~]$ diff -c file1.txt file2.txt
*** file1.txt 2008-12-23 06:40:13.000000000 -0500
--- file2.txt 2008-12-23 06:40:34.000000000 -0500
...
```

这个输出结果以两个文件名和它们的时间戳开头. 第一个文件用`星号`做标记,第二个文件用`短横线`做标记.  
这些标记将象征它们各自代表的文件. 下一步,我们看到几组修改, 包括默认的周围上下文行数. 在第一组中,我们看到:
`*** 1,4 ***`其表示第一个文件中从第一行到第四行的文本行. `--- 1,4 ---`这表示第二个文件中从第一行到第四行的文本行. 
在更改组内,文本行以四个指示符之一开头:

表21-5: `diff` 上下文模式更改指示符
指示符 意思

+ `blank`:  上下文显示行, 表示没有差异
+ `-`:  删除行. 第一个文件中有, 第二个没有. 
+ `+`:  添加行. 第一个文件中没有, 第二个有. 这一行将会出现在第二个文件内,而不是第一个文件中. 
+ `!`:  更改行. 两个文件的本行的内容不同. 

统一模式相似于上下文模式,但是更加简洁. 通过 `-u` 选项来指定它:

```bash
[me@linuxbox ~]$ diff -u file1.txt file2.txt
--- file1.txt 2008-12-23 06:40:13.000000000 -0500
+++ file2.txt 2008-12-23 06:40:34.000000000 -0500
@@ -1,4 +1,4 @@
```

上下文模式和统一模式之间最显著的差异就是重复上下文的消除,这就使得统一模式的输出结果要比上下文模式的输出结果简短. 
在我们上述实例中,我们看到类似于上下文模式中的文件时间戳,其紧紧跟随字符串 `@@ -1,4+1,4 @@`. 
这行字符串表示了在更改组中描述的第一个文件中的文本行和第二个文件中的文本行.  这行字符串之后就是文本行本身,与三行默认的上下文. 每行以可能的三个字符中的一个开头:

表21-6: `diff` 统一模式更改指示符
字符 意思

+ `空格`:  两个文件都包含这一行.
+ `-`:  在第一个文件中删除这一行.
+ `+`:  添加这一行到第一个文件中.

### patch

`patch` 程序被用来把更改应用到文本文件中. 它接受从 `diff` 程序的输出,并且通常被用来把较老的文件版本转变为较新的文件版本. 

让我们考虑一个著名的例子. `Linux` 内核是由一个 大型的,组织松散的贡献者团队开发而成,这些贡献者会提交固定的少量更改到源码包中.  
`Linux` 内核由几百万行代码组成,虽然每个贡献者每次所做的修改相当少. 对于一个贡献者来说,每做一个修改就给每个开发者发送整个的内核源码树,这是没有任何意义的. 
相反, 提交一个 diff 文件. 一个 `diff` 文件包含先前的内核版本与带有贡献者修改的新版本之间的差异.  
然后一个接受者使用 `patch` 程序,把这些更改应用到他自己的源码树中. 使用 `diff/patch` 组合提供了 两个重大优点:

1. 一个 diff 文件非常小,与整个源码树的大小相比较而言. 
2. 一个 diff 文件简洁地显示了所做的修改,从而允许程序补丁的审阅者能快速地评估它. 

当然,`diff/patch` 能工作于任何文本文件,不仅仅是源码文件. 它同样适用于配置文件或任意其它文本. 
准备一个 `diff` 文件供 `patch` 程序使用,`GNU` 文档(查看下面的拓展阅读部分)建议这样使用 `diff` 命令:

```bash
diff -Naur 旧文件 新文件 > 补丁文件
```

`旧文件` 和 `新文件` 可以是单个文件也可以是包含文件的目录. `r` 选项支持递归目录树. 一旦创建了 `diff` 文件,我们就能应用它,把旧文件修补成新文件. 

```bash
patch < 补丁文件
```

我们不需要指定要修补的目标文件,因为 `diff` 文件(在统一模式中)已经 在标题行中包含了文件名. 
`patch` 程序有大量的选项,而且还有额外的实用程序可以被用来分析和编辑补丁. 

+ `-N, --new-file`:  认为缺失文件为空. 
+ `-a, --text`: 将所有文件当成文本格式
+ `-u, -U NUM, --unified[=NUM]`: 在统一模式下, 共同内容的行数(默认为`3`)
+ `-r, --recursive`:  递归地比较子文件夹

### tr

一般的文本编辑器是交互式的,意思是我们手动移动光标,然后输入我们的修改.  然而,也有非交互式的方法来编辑文本. 有可能,例如,通过单个命令把一系列修改应用到多个文件中. 

`tr` 程序被用来更改字符. 我们可以把它看作是一种基于字符的查找和替换操作. 换字是一种把字符从一个字母转换为另一个字母的过程. 例如,把小写字母转换成大写字母就是换字. 
我们可以通过 `tr` 命令来执行这样的转换,如下所示:

```bash
[me@linuxbox ~]$ echo "lowercase letters" | tr a-z A-Z
LOWERCASE LETTERS
```

正如我们所见,`tr` 命令操作标准输入,并把结果输出到标准输出. `tr` 命令接受两个参数:要被转换的字符集以及相对应的转换后的字符集. 字符集可以用三种方式来表示:

1. 一个枚举列表. 例如, `ABCDEFGHIJKLMNOPQRSTUVWXYZ`
2. 一个字符域. 例如,`A-Z` . 注意这种方法有时候面临与其它命令相同的问题,归因于语系的排序规则,因此应该谨慎使用. 
3. `POSIX` 字符类. 例如,`[:upper:]`

大多数情况下,两个字符集应该长度相同;然而,有可能第一个集合大于第二个,尤其如果我们想要把多个字符转换为单个字符:

```bash
[me@linuxbox ~]$ echo "lowercase letters" | tr [:lower:] A
AAAAAAAAA AAAAAAA
```

除了换字之外,`tr` 命令能允许字符从输入流中简单地被删除. 在之前的章节中,我们讨论了转换 `MS-DOS` 文本文件为 `Unix` 风格文本的问题. 
为了执行这个转换,每行末尾的回车符需要被删除.  这个可以通过 `tr` 命令来执行,如下所示:

```bash
tr -d '\r' < dos_file > unix_file
```

这里的 `dos_file` 是需要被转换的文件,`unix_file` 是转换后的结果. 这种形式的命令使用转义序列 `\r `来代表回车符. 查看 `tr` 命令所支持地完整的转义序列和字符类别列表,试试下面的命令:

```bash
[me@linuxbox ~]$ tr --help
```

***
ROT13: 不那么秘密的加密

`tr` 命令的一个有趣的用法是执行 `ROT13` 文本编码. 把 ROT13称为`加密`有点夸张, `文本模糊处理`更准确些. 
这个方法就是简单地把每个字符在字母表中向前移动`13`位. 因为移动的位数是可能的`26`个字符的一半, 所以对文本再次执行这个算法,就恢复到了它最初的形式. 
通过 `tr` 命令来执行这种编码:

```bash
echo  "secret text"  | tr a-zA-Z n-za-mN-ZA-M |
frperg grkg
```

再次执行相同的过程,得到翻译结果:

```bash
echo "frperg grkg" | tr a-zA-Z n-za-mN-ZA-M+ |
secret text
```

`tr` 也可以完成另一个技巧. 使用 `-s` 选项,`tr` 命令能“挤压”(删除)重复的字符实例:

```bash
[me@linuxbox ~]$ echo "aaabbbccc" | tr -s ab
abccc
```

这里我们有一个包含重复字符的字符串. 通过给 `tr` 命令指定字符集`ab`,我们能够消除字符集中字母的重复实例,对字符集之外字符(`c`)不作用.
注意重复的字符必须是相邻的.  如果它们不相邻,那么挤压会没有效果. 

### sed

`sed` 是 `stream editor`(流编辑器)的简称. 它对文本流进行编辑,或是一系列指定的文件,或是标准输入. 
`sed` 是一款强大的,并且有些复杂的程序(有整本内容都是关于 `sed` 程序的书籍), 所以在这里我们不会详尽的讨论它. 
总之, `sed` 的工作方式可以是给出单个编辑命令(在命令行中), 也可以是包含多个命令的脚本文件名, 然后它就按行来执行这些命令. 这里有一个非常简单的 `sed` 实例:

```bash
[me@linuxbox ~]$ echo "front" | sed 's/front/back/'
back
```

在这个例子中,我们使用 `echo` 命令产生了一个单词的文本流,然后把它管道给 `sed` 命令. `sed`依次对流文本执行指令 `s/front/back/`,随后输出`back`. 
我们也能够把这个命令认为是相似于 `vi` 中的`替换` (查找和替代)命令. 

`sed` 中的命令开始于单个字符. 在上面的例子中,这个替换命令由字母 `s` 来代表,其后跟着`查找`和`替代`字符串,`斜杠`字符做为分隔符. 
分隔符的选择是随意的. 按照惯例,经常使用斜杠字符, 但是 `sed` 将会接受紧随命令之后的任意字符做为分隔符. 我们可以按照这种方式来执行相同的命令:

```bash
[me@linuxbox ~]$ echo "front" | sed 's_front_back_'
back
```

通过紧跟命令之后使用`下划线`字符,则它变成界定符. `sed` 可以设置界定符的能力,使命令的可读性更强, 正如我们将看到的.
`sed` 中的大多数命令之前都会带有一个地址,其指定了输入流中要被编辑的文本行. 如果省略了地址, 然后会对输入流的每一行执行编辑命令. 
最简单的地址形式是一个行号. 我们能够添加一个地址 到我们例子中:

```bash
[me@linuxbox ~]$ echo "front" | sed '1s/front/back/'
back
```

给我们的命令添加地址 `1`,就导致只对仅有一行文本的输入流的第一行执行替换操作. 如果我们指定另一 个数字:

```bash
[me@linuxbox ~]$ echo "front" | sed '2s/front/back/'
front
```

我们看到没有执行这个编辑命令,因为我们的输入流没有第二行. 地址可以用许多方式来表达. 这里是最常用的:

表21-7: `sed` 地址表示法
地址 说明

+ `n`: 行号,`n` 是一个正整数. 
+ `$`: 最后一行. 
+ `/regexp/`: 所有匹配一个 `POSIX` 基本正则表达式的文本行, 表达式通过斜杠界定. 正则表达式可以有备用字符界定,例如`\cregexpc`,这里 `c` 就是一个备用的字符. 
+ `addr1,addr2`: 从 `addr1` 到 `addr2` 范围内的文本行,包含地址 `addr2` 在内. 地址可能是上述任意单独的地址形式. 
+ `first~step`: 匹配由数字 `first` 代表的文本行,然后随后的每个在 `step` 间隔处的文本行. 例如 `1~2` 是指每个位于奇数行号的文本行,`5~5` 则指第五行和之后每五行位置的文本行. 
+ `addr1,+n`: 匹配地址 `addr1` 和随后的 `n` 个文本行. 
+ `addr!`: 匹配所有的文本行,除了 `addr` 之外,`addr` 可能是上述任意的地址形式. 

例如打印`1`到`5`行, 

```bash
[me@linuxbox ~]$ sed -n '1,5p' distros.txt
```

一般为了高效,我们必须指定选项 `-n`(不自动打印选项), 让 `sed` 不要默认地打印每一行, `p` 命令, 其就是简单地把匹配的文本行打印出来. 

```bash
$ sed -n '/SUSE/p' distros.txt
```

通过包含由斜杠界定的正则表达式 `/SUSE/`,我们能够孤立出包含它的文本行,和 `grep` 程序的功能是相同的. 
最后,我们将试着否定上面的操作,通过给这个地址添加一个感叹号:

```bash
[me@linuxbox ~]$ sed -n '/SUSE/!p' distros.txt
```

目前为止,我们已经知道了两个 sed 的编辑命令,`s` 和 `p`. 这里是一个更加全面的基本编辑命令列表:

表21-8: `sed` 基本编辑命令
命令 说明

+ `=`: 输出当前的行号. 
+ `a`: 在当前行之后追加文本. 
+ `d`: 删除当前行. 
+ `i`: 在当前行之前插入文本. 
+ `p`: 打印当前行. 默认情况下,`sed` 程序打印每一行. 通过指定 `-n` 选项,这个默认的行为能够被忽略. 
+ `q`: 退出 `sed`,不再处理更多的文本行. 如果不指定 `-n` 选项,输出当前行. 
+ `Q`: 退出 `sed`,不再处理更多的文本行. 
+ `s/regexp/replacement/`: 只要找到一个 `regexp` 匹配项,就替换为 `replacement` 的内容. `replacement`可以是`&`, 就是`regexp`匹配到的文本. 
另外, `replacement` 可能包含序列 `\1`到`\9`,也就是反向引用. 在 `replacement` 末尾的斜杠之后,可以指定一个 可选的标志,来修改 `s` 命令的行为. 
+ `y/set1/set2`:  执行字符转写操作,通过把 `set1` 中的字符转变为相对应的 `set2` 中的字符. 注意不同于 `tr` 程序,`sed` 要求两个字符集合具有相同的长度. 

到目前为止,`s` 命令是最常使用的编辑命令. 例如 文件中的日期格式是`MM/DD/YYYY`,但如果格式是 `YYYY-MM-DD` 会更好一些(利于排序). 
手动修改 日期格式不仅浪费时间而且易出错,但是有了 `sed`,只需一步就能完成修改:

```bash
[me@linuxbox ~]$ sed 's/\([0-9]\{2\}\)\/\([0-9]\{2\}\)\/\([0-9]\{4\}\)$/\3-\1-\2/' distros.txt
```

哇!这个命令看起来很丑陋. 但是它起作用了. 仅用一步,我们就更改了文件中的日期格式.  它也是一个例子: 有时候我们会开玩笑地把正则表达式称为是"只写"的. 
我们能写正则表达式,但是有时候我们不能读它们. 在我们恐惧地忍不住要逃离此命令之前,让我们看一下 怎样来构建它. 

因为日期是 `MM/DD/YYYY` 格式,并且出现在文本行的末尾,我们可以使用这样的表达式:

```bash
([0-9]{2})/([0-9]{2})/([0-9]{4})$
```

此表达式匹配两位数字,一个斜杠,两位数字,一个斜杠,四位数字,以及行尾. 圆括号包围的是子表达式, 稍后可以使用反向引用,现在我们就可以构建`replacement`,如下所示:

```bash
\3-\1-\2
```

此表达式给出了`年份-月份-日期`.合起来就是 `sed 's/([0-9]{2})/([0-9]{2})/([0-9]{4})$/\3-\1-\2/' distros.txt`. 

我们还有两个问题. 第一个是在我们表达式中额外的斜杠将会迷惑 `sed`.第二个是在默认情况下,`sed`只接受基本的正则表达式,在表达式中的几个字符会 被当作文字字面值,而不是元字符. 
我们能够通过反斜杠的自由应用来转义令人不快的字符:`sed 's/\([0-9]\{2\}\)\/\([0-9]\{2\}\)\/\([0-9]\{4\}\)$/\3-\1-\2/' distros.txt`. 你掌握了吧!

`s` 命令的另一个功能是使用可选标志,其跟随替代字符串. 一个最重要的可选标志是 `g` 标志,其 指示 `sed` 对某个文本行全范围地执行查找和替代操作,不仅仅是对第一个实例,例如: 

```bash
[me@linuxbox ~]$ echo "aaabbbccc" | sed 's/b/B/'
aaaBbbccc
```

我们看到虽然执行了替换操作,但是只针对第一个字母 `b` 实例,然而剩余的实例没有更改. 

```bash
[me@linuxbox ~]$ echo "aaabbbccc" | sed 's/b/B/g'
aaaBBBccc
```

目前为止,通过命令行我们只让 `sed` 执行单个命令. 使用 `-f` 选项,也有可能在一个脚本文件中构建更加复杂的命令.  
我们需要编写一个脚本,所以我们将打开文本编辑器,然后输入以下文字:

```bash
# sed script to produce Linux distributions report
1 i\
\
Linux Distributions Report\
s/\([0-9]\{2\}\)\/\([0-9]\{2\}\)\/\([0-9]\{4\}\)$/\3-\1-\2/
y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/
```

我们将把 `sed` 脚本保存为 `distros.sed` 文件,然后像这样运行它:

```bash
[me@linuxbox ~]$ sed -f distros.sed distros.txt
```

让我们分析一下脚本文件. 我们将使用 `cat` 来给每行文本编号:

```bash
[me@linuxbox ~]$ cat -n distros.sed
1 # sed script to produce Linux distributions report
2
3 1 i\
4 \
5 Linux Distributions Report\
6
7 s/\([0-9]\{2\}\)\/\([0-9]\{2\}\)\/\([0-9]\{4\}\)$/\3-\1-\2/
8 y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/
```

我们脚本文件的第一行是一条注释. 如同 `Linux` 系统中的许多配置文件和编程语言一样,注释以`#`字符开始, 然后是人类可读的文本. 
第二行是一个空行. 正如注释一样,添加空白行是为了提高程序的可读性. 

许多 `sed` 命令支持行地址. 这些行地址被用来指定对输入文本的哪一行执行操作. 行地址可能被 表示为单独的行号,行号范围,以及特殊的行号`$`,它表示输入文本的最后一行. 
从第三行到第六行所包含地文本要被插入到地址 `1` 处,也就是输入文本的第一行中. 这个 `i` 命令 之后是反斜杠回车符,来产生一个转义的回车符,或者就是所谓的连行符. 
这个序列能够 被用在许多环境下,包括 `shell` 脚本,从而允许把回车符嵌入到文本流中,而没有通知解释器(在这是指 `sed` 解释器)已经到达了文本行的末尾. 
`i` 命令,同样地,命令 `a`(追加文本, 而不是插入文本)和 `c`(取代文本)命令都允许多个文本行,只要每个文本行,除了最后一行,以一个 连行符结束. 
实际上,脚本的第六行是插入文本的末尾,它以一个普通的回车符结尾,而不是一个 连行符,通知解释器 `i` 命令结束了. 

注意:一个连行符由一个`斜杠字符`其后紧跟一个`回车符`组成. 它们之间不允许有空白字符. 

第七行是我们的查找和替代命令. 因为命令之前没有添加地址,所以输入流中的每一行文本 都得服从它的操作. 
第八行执行小写字母到大写字母的字符替换操作. 注意不同于 `tr` 命令,这个 `sed` 中的 `y` 命令不 支持字符区域(例如,`[a-z]`),也不支持 `POSIX` 字符集. 
因为 `y` 命令之前不带地址, 所以它会操作输入流的每一行. 

`sed` 是一款非常强大的程序,它能够针对文本流完成相当复杂的编辑任务. 它最常用于简单的行任务,而不是长长的脚本. 
许多用户喜欢使用其它工具,来执行较大的工作.  在这些工具中最著名的是 `awk` 和 `perl` . 
它们不仅仅是工具,像这里介绍的程序,且延伸到 完整的编程语言领域. 特别是 `perl`,经常被用来代替 `shell`脚本,来完成许多系统管理任务, 同时它也是一款非常流行网络开发语言. 
`awk` 更专用一些. 其具体优点是其操作表格数据的能力.  虽然关于 `awk` 和 `perl` 的内容都超出了本书所讨论的范围, 但是对于 Linux 命令行用户来说,它们都是非常好的技能. 

### aspell

我们要查看的最后一个工具是 `aspell`,一款交互式的拼写检查器. `aspell` 程序是早先 `ispell` 程序的继承者,大多数情况下,它可以被用做一个替代品. 
虽然 `aspell` 程序大多被其它需要拼写检查能力的 程序使用,但它也可以作为一个独立的命令行工具使用. 
它能够智能地检查各种类型的文本文件, 包括 `HTML` 文件,`C/C++`程序,电子邮件和其它种类的专业文本. 拼写检查一个包含简单的文本文件,可以这样使用 `aspell`:

```bash
aspell check textfile
```

这里的 `textfile` 是要检查的文件名. 作为一个实际例子,让我们创建一个简单的文本文件,叫做 `foo.txt`, 包含一些故意的拼写错误:

```bash
[me@linuxbox ~]$ cat > foo.txt
The quick brown fox jimped over the laxy dog.
```

下一步我们将使用 `aspell` 来检查文件:

```bash
[me@linuxbox ~]$ aspell check foo.txt
```

`aspell` 在检查模式下是交互的,在显示屏的顶部,我们看到我们的文本中有一个拼写可疑且高亮显示的单词. 在中间部分,我们看到 十个拼写建议,序号从 `0` 到 `9`,然后是一系列其它可能的操作. 
最后,在最底部,我们看到一个提示符, 准备接受我们的选择. 如果我们按下 `1` 按键,`aspell` 会用单词 `jumped` 代替错误单词,然后移动到下一个拼写错的单词,就是`laxy`. 
一旦 aspell 结束操作,我们 可以检查我们的文件,会看到拼写错误的单词已经更正了. 

[me@linuxbox ~]$ cat foo.txt
The quick brown fox jumped over the lazy dog.

除非由命令行选项 `--dont-backup` 告诉 `aspell`,否则通过追加扩展名`.bak` 到文件名中, `aspell` 会创建一个包含原始文本的备份文件. 

对于`HTML`文件, 使用`-H(HTML)`检查模式选项,能够避免`aspell`认为`<HTML>`这样的标签是错误拼写. 

### 总结归纳

在这一章中,我们已经查看了一些操作文本的命令行工具. 我们将在随后的章节中发现这些工具组成 了解决实际问题的基本工具箱. 
这将是确定无疑的,当我们学习 shell 脚本的时候, 到时候这些工具将真正体现出它们的价值. 

还有一些更有趣的文本操作命令:`split`(把文件分割成碎片), `csplit`(基于上下文把文件分割成碎片),和 `sdiff`(并排合并文件差异). 
