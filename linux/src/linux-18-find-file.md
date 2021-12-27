# 第十八章:查找文件

因为我们已经浏览了 Linux 系统,所以一件事已经变得非常清楚:一个典型的 Linux 系统包含很多文件!
这就引发了一个问题,`我们怎样查找东西?`.

虽然我们已经知道 Linux 文件系统良好的组织结构,是源自 类 Unix的操作系统代代传承的习俗.
但是仅文件数量就会引起可怕的问题.

在这一章中,我们将察看 两个用来在系统中查找文件的工具.这些工具是:

+ `locate` – 通过名字来查找文件
+ `find` – 在目录层次结构中搜索文件

我们也将看一个经常与文件搜索命令一起使用的命令,它用来处理搜索到的文件列表:

+ `xargs` – 从标准输入生成和执行命令行

另外,我们将介绍两个命令来协助我们探索:

+ `touch` – 更改文件时间
+ `stat` – 显示文件或文件系统状态

### locate - 查找文件的简单方法

这个 `locate` 程序快速搜索路径名数据库,并且输出每个与给定字符串相匹配的文件名.
比如说, 例如,我们想要找到所有名字以`zip`开头的程序.
因为我们正在查找程序,可以假定包含 匹配程序的目录以`bin/`结尾.
因此,我们试着以这种方式使用 `locate` 命令,来找到我们的文件:

```bash
$ locate bin/zip
```

`locate` 命令将会搜索它的路径名数据库,输出任一个包含字符串`bin/zip`的路径名:

```bash
/usr/bin/zip
/usr/bin/zipcloak
...
```

如果搜索要求没有这么简单,`locate` 可以结合其它工具,比如说 `grep` 命令,来设计更加 有趣的搜索:

```bash
$ locate zip | grep bin
/bin/bunzip2
/bin/bzip2
...
```

这个 `locate` 程序已经存在了很多年了,它有几个不同的变体被普遍使用着.

在现在 Linux 发行版中发现的两个最常见的变体是 `slocate` 和 `mlocate`,但是通常它们被名为 `locate` 的 符号链接访问.
不同版本的 locate 命令拥有重复的选项集合.
一些版本包括正则表达式 匹配(我们会在下一章中讨论)和通配符支持.
查看 locate 命令的手册,从而确定安装了 哪个版本的 locate 程序.

### locate 数据库来自何方

你可能注意到了,在一些发行版中,仅仅在系统安装之后,`locate` 不能工作, 但是如果你第二天再试一下,它就工作正常了.

怎么回事呢?`locate` 数据库由另一个叫做 `updatedb` 的程序创建.
通常,这个程序作为一个 `cron` 工作例程周期性运转;也就是说,一个任务 在特定的时间间隔内被 `cron` 守护进程执行.
大多数装有`locate` 的系统会每隔一天运行一回 `updatedb` 程序.
因为数据库不能被持续地更新,所以当使用 `locate` 时,你会发现 目前最新的文件不会出现.
为了克服这个问题,可以手动运行 `updatedb` 程序, 更改为超级用户身份,在提示符下运行 `updatedb` 命令.

### find 查找文件

`locate` 程序只能依据文件名来查找文件,而 `find` 程序能基于各种各样的属性, 搜索一个给定目录(以及它的子目录),来查找文件.
我们将要花费大量的时间学习 `find` 命令,因为 它有许多有趣的特性,当我们开始在随后的章节里面讨论编程概念的时候,我们将会重复看到这些特性.
`find` 命令的最简单使用是,搜索一个或多个目录.例如,输出我们的家目录列表.

```bash
$ find ~
```

对于最活跃的用户帐号,这将产生一张很大的列表.因为这张列表被发送到标准输出, 我们可以把这个列表管道到其它的程序中.
让我们使用 `wc` 程序来计算出文件的数量:

```bash
$ find ~ | wc -l
47068
```

哇,我们一直很忙!`find` 命令的美丽所在就是它能够被用来识别符合特定标准的文件.
它通过 (有点奇怪)应用选项,测试条件,和操作来完成搜索.我们先看一下测试条件.

### find 测试

比如说我们想要目录列表.我们可以添加以下测试条件:

```bash
$ find ~ -type d | wc -l
1695
```

添加测试条件 `-type d` 限制了只搜索目录.相反地,我们使用这个测试条件来限定搜索普通文件:

```bash
$ find ~ -type f | wc -l
38737
```

这里是 `find` 命令支持的普通文件类型测试条件:
***
表18-1: find 文件类型
文件类型 描述

+ `b` 块设备文件
+ `c` 字符设备文件
+ `d` 目录
+ `f` 普通文件
+ `l` 符号链接

我们也可以通过加入一些额外的测试条件,根据文件大小和文件名来搜索:
让我们查找所有文件名匹配 通配符模式`*.JPG`和文件大小大于`1M` 的文件:

```bash
$ find ~ -type f -iname "\*.JPG" -size +1M | wc -l
840
```

在这个例子里面,我们加入了 `-iname` 测试条件,后面跟通配符模式.
注意,我们把它用双引号引起来, 从而阻止 shell 展开路径名.

紧接着,我们加入 `-size` 测试条件,后跟字符串`+1M`.
开头的加号表明 我们正在寻找文件大小大于指定数的文件.
若字符串以减号开头,则意味着查找小于指定数的文件.
若没有符号意味着`精确匹配这个数`.
结尾字母`M`表明测量单位是兆字节.

下面的字符可以 被用来指定测量单位:
***
表18-2: find 大小单位
字符 单位

+ `b` `512` 个字节块.如果没有指定单位,则这是默认值.
+ `c` 字节
+ `w` 两个字节的字
+ `k` 千字节(`1024`个字节单位)
+ `M` 兆字节(`1048576`个字节单位)
+ `G` 千兆字节(`1073741824`个字节单位)

`find` 命令支持大量不同的测试条件.下表是列出了一些常见的测试条件.
请注意,在需要数值参数的情况下,可以应用以上讨论的`+`和`-`符号表示法:
***
表18-3: `find` 测试条件
测试条件 描述

+ `-cmin n` ; 匹配的文件和目录的内容或属性最后修改时间正好在 `n` 分钟之前. 指定少于`n` 分钟之前,使用 `-n`,指定多于 `n` 分钟之前,使用 `+n`.
+ `-cnewer file` ; 匹配的文件和目录的内容或属性最后修改时间早于那些文件.
+ `-ctime n` ; 匹配的文件和目录的内容和属性最后修改时间在 `n*24` 小时之前.
+ `-empty` ; 匹配空文件和目录.
+ `-group name` ; 匹配的文件和目录属于一个组.组可以用组名或组 ID 来表示.
+ `-iname pattern` ; 就像`-name` 测试条件,但是不区分大小写.
+ `-inum n` ; 匹配的文件的 `inode` 号是 `n`.这对于找到某个特殊 `inode` 的所有硬链接很有帮助.
+ `-mmin n` ; 匹配的文件或目录的内容被修改于 `n` 分钟之前.
+ `-mtime n` ; 匹配的文件或目录的内容被修改于`n*24`小时之前.
+ `-name pattern` ; 用指定的通配符模式匹配的文件和目录.
+ `-newer file` ; 匹配的文件和目录的内容早于指定的文件.当编写 `shell` 脚本,做文件备份时,非常有帮助.
每次你制作一个备份,更新文件(比如说日志),然后使用 `find` 命令来决定自从上次更新,哪一个文件已经更改了.
+ `-nouser` ; 匹配的文件和目录不属于一个有效用户.这可以用来查找 属于删除帐户的文件或监测攻击行为.
+ `-nogroup` ; 匹配的文件和目录不属于一个有效的组.
+ `-perm mode` ; 匹配的文件和目录的权限已经设置为指定的 `mode`.`mode` 可以用 八进制或符号表示法.
+ `-samefile name` ; 相似于`-inum` 测试条件.匹配和文件 `name` 享有同样 `inode` 号的文件.
+ `-size n` ; 匹配的文件大小为 `n`. `+n`大于, `-n`小于
+ `-type c` ; 匹配的文件类型是 `c`.
+ `-user name` ; 匹配的文件或目录属于某个用户.这个用户可以通过用户名或用户 ID 来表示.
+ `-maxdepth 0 ` 表示只应用在`开始点`列表本身.
+ `-mindepth  1` 表示排除开始点列表,测试其余

这不是一个完整的列表.`find` 命令手册有更详细的说明.

+ `-size n[cwbkMG]`: 文件占据的体积, 可以大于, 小于, 和精确等于, `n`个单位的空间, 向上取整.  后缀可以是:
    + `b` ;   for 512-byte blocks (this is the default if no suffix is used)
    + `c` ;   for bytes
    + `w` ;   for two-byte words
    + `k` ;   for kibibytes (KiB, units of 1024 bytes)
    + `M` ;   for mebibytes (MiB, units of 1024 * 1024 = 1048576 bytes)
    + `G` ;   for gibibytes (GiB, units of 1024 * 1024 * 1024 = 1073741824 bytes)

### find 操作符

即使拥有了 `find` 命令提供的所有测试条件,我们还需要一个更好的方式来描述测试条件之间的逻辑关系.

例如,如果我们需要确定是否一个目录中的所有的文件和子目录拥有安全权限,怎么办呢?
我们可以查找权限不是`0600`的文件和权限不是`0700`的目录.

幸运地是,`find` 命令提供了 一种方法来结合测试条件,通过使用逻辑操作符来创建更复杂的逻辑关系.
为了表达上述的测试条件,我们可以这样做:

```bash
$ find ~ \( -type f -not -perm 0600 \) -or \( -type d -not -perm 0700 \)
```

呀!这的确看起来很奇怪.这些是什么东西?实际上,这些操作符没有那么复杂,一旦你知道了它们的原理.
这里是操作符列表:
***
表18-4: find 命令的逻辑操作符

+ `-and` 如果操作符两边的测试条件都是真,则匹配.可以简写为 `-a`. 注意若没有使用操作符,则默认使用 `-and`.
+ `-or` 若操作符两边的任一个测试条件为真,则匹配.可以简写为 `-o`.
+ `-not` 若操作符后面的测试条件是真,则匹配.可以简写为一个感叹号(`!`).
+ `()` 把测试条件和操作符组合起来形成更大的表达式.这用来控制逻辑计算的优先级.

默认情况下,`find` 命令按照从左到右的顺序计算.经常有必要重写默认的求值顺序,以得到期望的结果.
即使没有必要,有时候括住组合起来的字符,对提高命令的可读性是很有帮助的.

注意 因为圆括号`()`字符对于 shell 来说有特殊含义,所以在命令行中使用它们的时候,它们必须 用引号引起来,才能作为实参传递给 `find` 命令. 通常反斜杠字符被用来转义圆括号字符.

通过这张操作符列表,我们重建 `find` 命令.从最外层看,我们看到测试条件被分为两组,由一个 `-or` 操作符分
开:

```bash
( expression 1 ) -or ( expression 2 )
```

这很有意义,因为我们正在搜索具有不同权限集合的文件和目录.
如果我们文件和目录两者都查找, 那为什么要用 `-or` 来代替 `-and` 呢?
因为 `find` 命令扫描文件和目录时,会计算每一个对象,看看它是否 匹配指定的测试条件.

我们想要知道它是具有错误权限的文件还是有错误权限的目录.
它不可能同时符合这 两个条件.所以如果展开组合起来的表达式,我们能这样解释它:

```bash
( file with bad perms ) -or ( directory with bad perms )
```

下一个挑战是怎样来检查`错误权限`,这个怎样做呢?我们不从这个角度做.

我们将测试 `不是正确权限`,因为我们知道什么是`正确权限`.
对于文件,我们定义正确权限为`0600`, 目录则为`0711`.测试具有`不正确`权限的文件表达式为:

```bash
-type f -and -not -perms 0600
```

对于目录,表达式为:

```bash
-type d -and -not -perms 0700
```

正如上述操作符列表中提到的,这个`-and` 操作符能够被安全地删除,因为它是默认使用的操作符.

所以如果我们把这两个表达式连起来,就得到最终的命令:

```bash
find ~ \( -type f -not -perms 0600 \) -or \( -type d -not -perms 0700 \)
```

然而,因为圆括号对于 shell 有特殊含义,我们必须转义它们,来阻止 shell 解释它们.
在圆括号字符 之前加上一个反斜杠字符来转义它们.

逻辑操作符的另一个特性要重点理解.比方说我们有两个由逻辑操作符分开的表达式:

```bash
expr1 -operator expr2
```

在所有情况下,总会执行表达式 `expr1`;然而由操作符来决定是否执行表达式 `expr2`.
这里 列出了它是怎样工作的:
***
表18-5: find AND/OR 逻辑
`expr1` 的结果 操作符 `expr2` is...

+ `真` `-and` 总要执行
+ `假` `-and` 从不执行
+ `真` `-or` 从不执行
+ `假` `-or` 总要执行

为什么这会发生呢?这样做是为了提高性能.
以 `-and` 为例,我们知道表达式 `expr1 -and expr2` 不能为真,如果表达式 `expr1` 的结果为假,所以没有必要执行 `expr2`.

为什么这个很重要? 它很重要是因为我们能依靠这种行为来控制怎样来执行操作.我们会很快看到...

### find 预定义的操作

从 `find` 命令得到的结果列表很有用处,但是我们真正想要做的事情是操作列表中的某些条目.
幸运地是,`find` 命令允许基于搜索结果来执行操作.有许多预定义的操作和几种方式来 应用用户定义的操作.

首先,让我们看一下几个预定义的操作:
***
表18-6: 几个预定义的 `find` 命令操作

+ `-delete` 删除当前匹配的文件.
+ `-ls` 对匹配的文件执行等同的 `ls -dils` 命令.并将结果发送到标准输出.
+ `-print` 把匹配文件的全路径名输送到标准输出.如果没有指定其它操作,这是 默认操作.
+ `-quit` 一旦找到一个匹配,退出.

和测试条件一样,还有更多的操作.

查看 `find` 命令手册得到更多细节.在第一个例子里, 我们这样做:

```bash
find ~
```

这个命令输出了我们家目录中包含的每个文件和子目录.
它会输出一个列表,因为会默认使用`-print` 操作 ,如果没有指定其它操作的话.
因此我们的命令也可以这样表述:

```bash
find ~ -print
```

我们可以使用 `find` 命令来删除符合一定条件的文件.
例如,来删除扩展名为`.BAK`(这通常用来指定备份文件) 的文件,我们可以使用这个命令:

```bash
find ~ -type f -name '*.BAK' -delete
```

在这个例子里面,用户家目录(和它的子目录)下搜索每个以`.BAK` 结尾的文件名.
当找到后,就删除它们.警告:当使用` -delete` 操作时,不用说,你应该格外小心.
首先测试一下命令, 用 `-print` 操作代替` -delete`,来确认搜索结果.

在我们继续之前,让我们看一下逻辑运算符是怎样影响操作的.考虑以下命令:

```bash
find ~ -type f -name '*.BAK' -print
```

正如我们所见到的,这个命令会查找每个文件名以`.BAK` (`-name '*.BAK' `) 结尾的普通文件 (`-type f`), 并把每个匹配文件的相对路径名输出到标准输出 (`-print`).

然而,此命令按这个方式执行的原因,是 由每个测试和操作之间的逻辑关系决定的.
记住,在每个测试和操作之间会默认应用 `-and` 逻辑运算符. 我们也可以这样表达这个命令,使逻辑关系更容易看出:

```bash
find ~ -type f -and -name '*.BAK' -and -print
```

当命令被充分表达之后,让我们看看逻辑运算符是如何影响其执行的:

`-print` 只有 `-type f and -name '*.BAK'`为真的时候
`-name *.BAK` 只有` -type f `为真的时候
`-type f `总是被执行,因为它是与 `-and` 关系中的第一个测试/行为.

因为测试和行为之间的逻辑关系决定了哪一个会被执行,我们知道测试和行为的顺序很重要.
例如, 如果我们重新安排测试和行为之间的顺序,让 `-print` 行为是第一个,那么这个命令执行起来会截然不同:

```bash
find ~ -print -and -type f -and -name '*.BAK'
```

这个版本的命令会打印出每个文件(`-print` 行为总是为真),然后测试文件类型和指定的文件扩展名.

### find 自定义操作

除了预定义的行为之外,我们也可以唤醒随意的命令.传统方式是通过 `-exec` 行为.这个行为像这样工作:`-exec command {} ;`

这里的 `command` 就是指一个命令的名字,`{}`是当前路径名的符号表示,`分号`是要求的界定符表明命令结束.
这里是一个使用 `-exec` 行为的例子,其作用如之前讨论的 `-delete` 行为:

```bash
-exec rm '{}' ';'
```

重述一遍,因为花括号`{}`和分号`;`对于 `shell` 有特殊含义,所以它们必须被引起来或被转义.
也有可能交互式地执行一个用户定义的行为.通过使用 `-ok` 行为来代替 `-exec`,在执行每个指定的命令之前, 会提示用户:

```bash
find ~ -type f -name 'foo*' -ok ls -l '{}' ';'
< ls ... /home/me/bin/foo > ? y
...
```

在这个例子里面,我们搜索以字符串`foo`开头的文件名,并且对每个匹配的文件执行 `ls -l` 命令.
使用 `-ok `行为,会在 `ls` 命令执行之前提示用户.

### find 提高效率

当 `-exec` 行为被使用的时候,若每次找到一个匹配的文件,它会启动一个新的指定命令的实例.我们可能更愿意把所有的搜索结果结合起来,再运行一个命令的实例.例如,而不是像这样执行命令:

```bash
ls -l file1
ls -l file2
```

我们更喜欢这样执行命令:

```bash
ls -l file1 file2
```

这样就导致命令只被执行一次而不是多次.有两种方法可以这样做.传统方式是使用外部命令 `xargs`,另一种方法是,使用 `find` 命令自己的一个新功能.我们先讨论第二种方法.

通过把末尾的分号改为加号,就激活了 `find` 命令的一个功能,把搜索结果结合为一个参数列表, 然后执行一次所期望的命令.

```bash
find ~ -type f -name 'foo*' -exec ls -l '{}' +
-rwxr-xr-x 1 me
```

虽然我们得到一样的结果,但是系统只需要执行一次 `ls` 命令.

## find 选项

### 全局选项

`find` 有一些称为`Global option`(全局选项)的选项,对于出现在前面的Test,它们仍然会产生影响.
如果你把它放在别的位置,`find`会报出警告. 它应该被放在`star points...`后面,也就是文件列表的后面.

>`...`表示一个参数可以有多个

诸如 `-maxdepth levels`,`-mindepth levels`都是全局参数.

+ `-maxdepth 0 ` 表示只应用在`开始点`列表本身
+ `-mindepth  1` 表示排除开始点列表,测试其余

### -print0

因为经常遇到文件名中包含空格,所以有一个常用操作 `-print0`.

`-print0`: `True`;在标准输出上打印完整的文件名, 后面是一个`null`字符(而不是`-print`使用的`newline`字符).
这使得包含换行符或其他类型的空白的文件名, 能够被处理`find`输出的程序正确解释.  这个选项与`xargs`的`-0`选项相对应.

```bash
find ./ -mindepth 1 -maxdepth 1 -type f -iname '*.jpg' -print0 | xargs --null ls -l
```

通过指定分隔符为`null`(ASCII`0`),来构建参数列表

### -printf

`man find`中有一个例子,解释了`-printf`操作的`％f`和`％h`格式指令:

```bash
$ find . .. / /tmp /tmp/TRACE compile compile/64/tests/find -maxdepth 0 -printf '[%h][%f]\n'
[.][.]
[.][..]
[][/]
[][tmp]
[/tmp][TRACE]
[.][compile]
[compile/64/tests][find]
```

+ 只打印文件名称:

    ```bash
    find ./opentype ./truetype -mindepth 1 -printf '%f\n'
    ```

    即可列出`./opentype ./truetype`两个目录下所有文件的名称, 不包括前面的路径.

    ```bash
    gfind /usr/local/texlive/2020/texmf-dist/fonts/{opentype/,truetype/} -mindepth 1 '(' -iname '*.ttf' -or -iname '*.otf' ')' > ~/test/fontname.txt
    ```

+ 按照 `fontname.txt` 文件中给出的地址, 批量生成符号链接.

    ```bash
    for font in $(cat fontname.txt); do
      ln -s  ${font} ${font##*/}
    done
    ```

### -regex pattern

要求 `文件名` 符合 `正则表达式模式`. 它是对 `整个路径` 的匹配, 而不是部分匹配(search).
例如, 为匹配文件 `./fubar3`, 你可以使用正则表达式 `.*bar.` 或 `.*b.*3`, 但不能使用 `f.*r3`.
`find` 所理解的正则表达式, 默认是 Emacs Regular Expressions (除了`.` 匹配换行),但这可以通过 `-regextype` 选项来改变.

find . -mindepth 1 -maxdepth 1 -type f -regextyep gnu-awk -regex '.*\{\w{8}\-\w{4}-\w{4}-\w{4}-\w{12}\}'

### 不寻常的文件名

查找的许多`操作`会导致打印其他用户使用的数据.  这包括文件名, 大小, 修改时间等等.  文件名是一个潜在的问题, 因为它们可以包含任何字符, 除了`\0`和`/`.
文件名中的不寻常的字符可以对你的终端做意想不到的事情, 而且往往是不可取的(例如, 改变你在某些终端上的功能键的设置).
不同的`操作`对不寻常的字符有不同的处理方式, 如下所述.

+ `-print0, -fprint0`:始终打印准确的文件名, 不改变, 即使输出到终端.
+ `-ls, -fls`:不寻常的字符总是被转义.  `White space`, 反斜杠和双引号字符使用C-风格的转义来打印(例如`\f`,`\"`).
 其他不寻常的字符使用八进制转义来打印. 其他可打印的字符(对于`-ls`和`-fls`, 这些是八进制`041`和`0176`之间的字符)按原样打印.

+ `-printf, -fprintf`:如果输出不进入终端, 则按原样打印.
否则, 其结果取决于使用的是哪条指令.  指令`%D`,` %F`, `%g`, `%G`, `%H`, `%Y`, 和` %y`要展开的值不受文件所有者的控制, 所以会按原样打印.
指令`%a`,`%b`, `%c`, `%d`, `%i`, `%k`, `%m`, `%M`, `%n`, `%s`, `%t`, `%u`和`%U`的值在文件所有者的控制之下, 但不能用来向终端发送任意数据,因此这些数据被原样打印.
指令`%f`, `%h`, `%l`, `%p`和`%P`用引号括起来.  这种加引号的方式与GNU `ls`的方式相同, 这与用于`-ls`和`-fls`的机制不同.
如果你能决定对`find`的输出使用什么格式, 那么通常使用`0`作为结束符比使用换行符更好, 因为文件名可以包含空白和换行符.
`LC_CTYPE`环境变量的设置被用来决定哪些字符需要被引号.

+ `-print, -fprint`: `Quoting`的处理方式与`-printf`和`-fprintf`的处理方式相同.
如果你在脚本中使用`find`, 或者在匹配的文件可能有任意名称的情况下, 你应该考虑使用`-print0`而不是而不是`-print`.

+ `-ok`和`-okdir`动作是按原样打印当前文件名.  这在未来的版本中可能会改变.

## xargs

这个 `xargs` 命令会执行一个有趣的函数.它从标准输入接受输入,并把输入转换为一个特定命令的 参数列表.
对于我们的例子,我们可以这样使用它:

```bash
find ~ -type f -name 'foo\*' -print | xargs ls -l
-rwxr-xr-x 1 me
```

这里我们看到 `find` 命令的输出被管道到 `xargs` 命令,反过来,`xargs` 会为 `ls` 命令构建 参数列表,然后执行 `ls` 命令.

注意:当被放置到命令行中的参数个数相当大时,参数个数是有限制的.
有可能创建的命令 太长以至于 shell 不能接受.
当命令行超过系统支持的最大长度时,`xargs` 会执行带有最大 参数个数的指定命令,然后重复这个过程直到耗尽标准输入.
执行带有 `–show–limits` 选项 的 `xargs` 命令,来查看命令行的最大值.

[Shell中去掉文件中的换行符简单方法](https://blog.csdn.net/Jerry_1126/article/details/85009615)

```bash
cat FileName | xargs echo -n   # 连文件末尾换行符也去掉
# 或者
cat FileName | xargs           # 会保留文件末尾的换行符
```

### 处理古怪的文件名

[Shell 截取文件名和后缀](https://segmentfault.com/a/1190000023219453).
类 Unix 的系统允许在文件名中嵌入空格(甚至换行符).

这就给一些程序,如为其它 程序构建参数列表的`xargs` 程序,造成了问题.
一个嵌入的空格会被看作是一个界定符,生成的 命令会把每个空格分离的单词解释为单独的参数.
为了解决这个问题,`find` 命令和 `xarg` 程序 允许可选择的使用一个 `null` 字符作为参数分隔符.

一个 `null` 字符被定义在 `ASCII` 码中,由数字 `0`来表示(相反的,例如,`空格字符`在 `ASCII` 码中由数字`32`表示).
`find` 命令提供的 `-print0` 行为, 则会产生由 `null` 字符分离的输出,并且 `xargs` 命令有一个 `--null` 选项,这个选项会接受由 `null` 字符 分离的输入.这里有一个例子:

```bash
find ~ -iname '*.jpg' -print0 | xargs --null ls -l
```

使用这项技术,我们可以保证所有文件,甚至那些文件名中包含空格的文件,都能被正确地处理.
例如在latex 编译脚本里,可以这么用

```
find . -iname '*.tex' -printf '%f\0' | xargs --null echo
```

其中 `-printf '%f\0'` 指定输出格式为 不带路径的文件名,然后用ASCII`null`分隔.

所有可以用的格式指定为

\a     Alarm bell.

\b     Backspace.
\c     Stop printing from this format immediately and flush the output.
\f     Form feed.
\n     Newline.
\r     Carriage return.
\t     Horizontal tab.
\v     Vertical tab.
\0     ASCII NUL.
\\     A literal backslash (`\').
\NNN   The character whose ASCII code is NNN (octal).
A `\' character followed by any other character is treated as an ordinary character, so they both are printed.
%%     A literal percent sign.
%a     File's last access time in the format returned by the C `ctime' function.
%Ak    File's last access time in the format specified by k, which is either `@' or a directive for the C `strftime' function.  The possible values for  k
                     are listed below; some of them might not be available on all systems, due to differences in `strftime' between systems.
%c     File's last status change time in the format returned by the C `ctime' function.
%Ck    File's last status change time in the format specified by k, which is the same as for %A.
%d     File's depth in the directory tree; 0 means the file is a starting-point.
%D     The device number on which the file exists (the st_dev field of struct stat), in decimal.
%f     File's name with any leading directories removed (only the last element).
%F     Type of the filesystem the file is on; this value can be used for -fstype.
%g     File's group name, or numeric group ID if the group has no name.
%G     File's numeric group ID.
%h     Leading directories of file's name (all but the last element).  If the file name contains no slashes (since it is in the current directory) the  %h
       specifier expands to `.'.
%H     Starting-point under which file was found.
%i     File's inode number (in decimal).
%k     The  amount of disk space used for this file in 1K blocks.  Since disk space is allocated in multiples of the filesystem block size this is usually
       greater than %s/1024, but it can also be smaller if the file is a sparse file.
%l     Object of symbolic link (empty string if file is not a symbolic link).
%m     File's permission bits (in octal).  This option uses the `traditional' numbers which most Unix implementations use, but if your  particular  imple‐
                     mentation  uses an unusual ordering of octal permissions bits, you will see a difference between the actual value of the file's mode and the output
                     of %m.   Normally you will want to have a leading zero on this number, and to do this, you should use the # flag (as in, for example, `%#m').
%M     File's permissions (in symbolic form, as for ls).  This directive is supported in findutils 4.2.5 and later.
%n     Number of hard links to file.
%p     File's name.
%P     File's name with the name of the starting-point under which it was found removed.
%s     File's size in bytes.
%S     File's sparseness.  This is calculated as (BLOCKSIZE*st_blocks / st_size).  The exact value you will get for an ordinary file of a  certain  length
       is  system-dependent.  However, normally sparse files will have values less than 1.0, and files which use indirect blocks may have a value which is
       greater than 1.0.   The value used for BLOCKSIZE is system-dependent, but is usually 512 bytes.   If the file size is zero, the  value  printed  is
       undefined.  On systems which lack support for st_blocks, a file's sparseness is assumed to be 1.0.
%t     File's last modification time in the format returned by the C `ctime' function.
%Tk    File's last modification time in the format specified by k, which is the same as for %A.
%u     File's user name, or numeric user ID if the user has no name.
%U     File's numeric user ID.
%y     File's type (like in ls -l), U=unknown type (shouldn't happen)
%Y     File's type (like %y), plus follow symlinks: L=loop, N=nonexistent
%Z     (SELinux only) file's security context.

如果要使用通配符,需要用括号包住,或者进行转义(escape),否则shell 会将路径名展开,find 会接受到错误的参数列表.

比如`find . -name *.c  -print`会被shell 展开为类似于: `find . -name frcode.c locate.c word_io.c -print`
这将会使`find`报错.
 Instead of doing things this way, you should enclose the pattern in quotes or escape the wildcard:

+ `$ find . -name '*.c' -print`
+ `$ find . -name \*.c -print`

`escape`:逃脱,逃离,避开,即避免`shell`对提供的字符串进行各种处理.

***
例子: 抽取所有子文件,即把子目录的所有文件复制到当前目录下

`find ./  -type f -print0 | xargs -0 cp -t . --backup=t `

### 返回操练场

到实际使用 `find` 命令的时候了.我们将会创建一个操练场,来实践一些我们所学到的知识.

首先,让我们创建一个包含许多子目录和文件的操练场:

```bash
$ mkdir -p playground/dir-{00{1..9},0{10..99},100}
$ touch playground/dir-{00{1..9},0{10..99},100}/file-{A..Z}
```

惊叹于命令行的强大功能!只用这两行,我们就创建了一个包含一百个子目录,每个子目录中 包含了`26`个空文件的操练场.
试试用 `GUI` 来创建它!

我们用来创造这个奇迹的方法中包含一个熟悉的命令(`mkdir`),一个奇异的 `shell` 扩展(大括号) 和一个新命令,`touch`.
通过结合 `mkdir` 命令和`-p` 选项(导致 `mkdir` 命令创建指定路径的父目录),以及 大括号展开,我们能够创建一百个目录.

这个 `touch` 命令通常被用来设置或更新文件的访问,更改,和修改时间.
然而,如果一个文件名参数是一个 不存在的文件,则会创建一个空文件.

在我们的操练场中,我们创建了一百个名为 `file-A` 的文件实例.让我们找到它们:

```bash
$ find playground -type f -name 'file-A'
```

注意不同于 `ls` 命令,`find` 命令的输出结果是无序的.其顺序由存储设备的布局决定.
为了确定实际上 我们拥有一百个此文件的实例,我们可以用这种方式来确认:

```bash
$ find playground -type f -name 'file-A' | wc -l
```

下一步,让我们看一下基于文件的修改时间来查找文件.
当创建备份文件或者以年代顺序来 组织文件的时候,这会很有帮助.为此,首先我们将创建一个参考文件,我们将与其比较修改时间:

```bash
$ touch playground/timestamp
```

这个创建了一个空文件,名为 `timestamp`,并且把它的修改时间设置为当前时间.
我们能够验证 它通过使用另一个方便的命令,`stat`,是一款加大马力的 `ls` 命令版本.
这个 stat 命令会展示系统对 某个文件及其属性所知道的所有信息:

```bash
$ stat playground/timestamp
File: 'playground/timestamp'
Size: 0 Blocks: 0 IO Block: 4096 regular empty file
...
```

下一步,让我们使用 find 命令来更新一些操练场中的文件:

```bash
$ find playground -type f -name 'file-B' -exec touch '{}' ';'
```

这会更新操练场中所有名为 `file-B` 的文件.
接下来我们会使用 find 命令来识别已更新的文件, 通过把所有文件与参考文件 `timestamp` 做比较:

```bash
$ find playground -type f -newer playground/timestamp
```

搜索结果包含所有一百个文件 `file-B` 的实例.
因为我们在更新了文件 `timestamp` 之后, `touch` 了操练场中名为 `file-B` 的所有文件,
所以现在它们"新于"`timestamp` 文件,因此能被用 `-newer` 测试条件识别出来.

最后,让我们回到之前那个错误权限的例子中,把它应用于操练场里:

```bash
$ find playground \( -type f -not -perm 0600 \) -or \( -type d -not -perm 0700 \)
```

这个命令列出了操练场中所有一百个目录和二百六十个文件(还有 `timestamp` 和操练场本身,共 `2702` 个) ,
因为没有一个符合我们`正确权限`的定义.

通过对运算符和行为知识的了解,我们可以给这个命令添加行为,对实战场中的文件和目录应用新的权限.

```bash
$ find playground \( -type f -not -perm 0600 -exec chmod 0600 '{}' ';' \) -or \( -type d -not -perm 0711 -exec chmod 0700 '{}' ';' \)
```

在日常的基础上,我们可能发现运行两个命令会比较容易一些,一个操作目录,另一个操作文件,
而不是这一个长长的复合命令,但是很高兴知道,我们能这样执行命令.

这里最重要的一点是要 理解怎样把操作符和行为结合起来使用,来执行有用的任务.

### 选项

最后,我们有这些选项.这些选项被用来控制 `find` 命令的搜索范围.
当构建 `find` 表达式的时候, 它们可能被其它的测试条件和行为包含:
***
表 18-7: find 命令选项
选项 描述

+ `-depth` 指导 `find` 程序先处理目录中的文件,再处理目录自身.当指定`-delete` 行为时,会自动 应用这个选项.
+ `-maxdepth levels` 当执行测试条件和行为的时候,设置 `find` 程序陷入目录树的最大级别数
+ `-mindepth levels` 在应用测试条件和行为之前,设置 `find`程序陷入目录数的最小级别数.
+ `-mount 指导 find` 程序不要搜索挂载到其它文件系统上的目录.
+ `-noleaf 指导 find` 程序不要基于搜索类 Unix 的文件系统做出的假设,来优化它的搜索.

拓展阅读

程序 locate,`updatedb`,find 和 xargs 都是 GNU 项目 findutils 软件包的一部分.
这个 GUN 项目提供了大量的在线文档,这些文档相当出色,如果你在高安全性的 环境中使用这些程序,你应该读读这些文档.
[http://www.gnu.org/software/findutils/](http://www.gnu.org/software/findutils/)
