# shell 脚本

## 第二十五章: 编写第一个shell脚本

在前面的章节中, 我们已经装备了一个命令行工具的武器库.
虽然这些工具能够解决许多种计算问题,  但是我们仍然局限于在命令行中手动地一个一个使用它们.
难道不是很棒, 如果我们能够让 shell 来完成更多的工作?  我们可以的.
通过把我们的工具一起放置到我们自己设计的程序中, 然后 shell 就会自己来执行这些复杂的任务序
列.  通过编写 shell 脚本, 我们让 shell 来做这些事情.

## 什么是 Shell 脚本?

最简单的解释, 一个 shell 脚本就是一个包含一系列命令的文件.
shell 读取这个文件, 然后执行 文件中的所有命令, 就好像这些命令已经直接被输入到了命令行中一样.

Shell 有些独特, 因为它不仅是一个功能强大的命令行接口,也是一个脚本语言解释器.

我们将会看到,  大多数能够在命令行中完成的任务也能够用脚本来实现, 同样地, 大多数能用脚本实现的操作也能够 在命令行中完成.
虽然我们已经介绍了许多 shell 功能, 但只是集中于那些经常直接在命令行中使用的功能.
Shell 也提供了一些通常(但不总是)在编写程序时才使用的功能.

## 怎样编写一个 Shell 脚本

为了成功地创建和运行一个 shell 脚本, 我们需要做三件事情:

1. 编写一个脚本.  Shell 脚本就是普通的文本文件. 所以我们需要一个文本编辑器来书写它们.
最好的文本 编辑器都会支持语法高亮, 这样我们就能够看到一个脚本关键字的彩色编码视图.
语法高亮会帮助我们查看某种常见 错误. 为了编写脚本文件, vim, gedit, kate, 和许多其它编辑器都是不错的候选者.
2. 使脚本文件可执行.  系统会相当挑剔不允许任何旧的文本文件被看作是一个程序, 并且有充分的理由!
所以我们需要设置脚本文件的权限来允许其可执行.
3. 把脚本放置到 shell 能够找到的地方 当没有指定可执行文件明确的路径名时, shell 会自动地搜索某些目录,来查找此可执行文件.
为了最大程度的方便, 我们会把脚本放到这些目录当中.

### 脚本文件格式

为了保持编程传统, 我们将创建一个 "hello world" 程序来说明一个极端简单的脚本.
所以让我们启动 我们的文本编辑器, 然后输入以下脚本:

```bash
#!/bin/bash
# This is our first script.
echo 'Hello World!'
```

对于脚本中的最后一行, 我们应该是相当的熟悉, 仅仅是一个带有一个字符串参数的 echo 命令.
对于第二行也很熟悉. 它看起来像一个注释, 我们已经在许多我们检查和编辑过的配置文件中 看到过.
关于 shell 脚本中的注释, 它们也可以出现在文本行的末尾, 像这样:

    echo 'Hello World!' # This is a comment too

文本行中, # 符号之后的所有字符都会被忽略.
类似于许多命令, 这也在命令行中起作用:

```bash
$ echo 'Hello World!' # This is a comment too
Hello World!
```

虽然很少在命令行中使用注释, 但它们也能起作用.我们脚本中的第一行文本有点儿神秘.

它看起来它应该是一条注释, 因为它起始于一个`#`符号, 但是 它看起来太有意义, 以至于不仅仅是注释.
事实上, 这个#!字符序列是一种特殊的结构叫做 shebang.
这个 shebang 被用来告诉操作系统将执行此脚本所用的解释器的名字.
每个 shell 脚本都应该把这一文本行 作为它的第一行. 让我们把此脚本文件保存为 hello_world.

### 可执行权限

下一步我们要做的事情是让我们的脚本可执行. 使用 chmod 命令, 这很容易做到:

```bash
$ ls -l hello_world
-rw-r--r-- 1 me  me 63 2009-03-07 10:10 hello_world
$ chmod 755 hello_world
$ ls -l hello_world
-rwxr-xr-x 1 me me 63 2009-03-07 10:10 hello_world
```

对于脚本文件, 有两个常见的权限设置;
权限为755的脚本, 则每个人都能执行, 和权限为700的 脚本, 只有文件所有者能够执行.
注意为了能够执行脚本, 脚本必须是可读的.

### 脚本文件位置

当设置了脚本权限之后, 我们就能执行我们的脚本了:

```bash
$ ./hello_world
Hello World!
```

为了能够运行此脚本, 我们必须指定脚本文件明确的路径. 如果我们没有那样做, 我们会得到这样的提示:

```bash
$ hello_world
bash: hello_world: command not found
```

为什么会这样呢? 什么使我们的脚本不同于其它的程序? 结果证明, 什么也没有.
我们的 脚本没有问题. 是脚本存储位置的问题.
回到第12章, 我们讨论了 PATH 环境变量及其它在系统 查找可执行程序方面的作用.
回顾一下, 如果没有给出可执行程序的明确路径名, 那么系统每次都会 搜索一系列的目录, 来查找此可执行程序.
这个/bin 目录就是其中一个系统会自动搜索的目录.  这个目录列表被存储在一个名为 PATH 的环境变量中.
这个PATH 变量包含一个由冒号分隔开的目录列表.  我们可以查看 PATH 的内容:

```bash
$ echo $PATH
/home/me/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:
/bin:/usr/games
```

这里我们看到了我们的目录列表.
如果我们的脚本驻扎在此列表中任意目录下, 那么我们的问题将 会被解决. 注意列表中的第一个目录, /home/me/bin.
大多数的 Linux 发行版会配置 PATH 变量, 让其包含 一个位于用户家目录下的 bin 目录, 从而允许用户能够执行他们自己的程序.
所以如果我们创建了 一个 bin 目录, 并把我们的脚本放在这个目录下, 那么这个脚本就应该像其它程序一样开始工作了:

```bash
$ mkdir bin
$ mv hello_world bin
$ hello_world
Hello World!
```

它的确工作了.
如果这个 PATH 变量不包含这个目录, 我们能够轻松地添加它, 通过在我们的.bashrc 文件中包含下面 这一行文本:

```bash
export PATH=~/bin:"$PATH"
```

当做了这个修改之后, 它会在每个新的终端会话中生效.
为了把这个修改应用到当前的终端会话中,  我们必须让shell 重新读取这个 .bashrc 文件.
这可以通过 "sourcing".bashrc 文件来完成:

```bash
$ . .bashrc
```

这个点(.)命令是 source 命令的同义词, 一个 shell 内部命令, 用来读取一个指定的 shell 命令文件,  并把它看作是从键盘中输入的一样.
注意: 在 Ubuntu 系统中, 如果存在 ~/bin 目录, 当执行用户的 .bashrc 文件时,  Ubuntu 会自动地添加这个~/bin 目录到 PATH 变量中.
所以在 Ubuntu 系统中, 如果我们创建 了这个 ~/bin 目录, 随后退出, 然后再登录, 一切会正常运行.

### 脚本文件的好去处

这个 ~/bin 目录是存放为个人所用脚本的好地方.
如果我们编写了一个脚本, 系统中的每个用户都可以使用它,那么这个脚本的传统位置是 /usr/local/bin.
系统管理员使用的脚本经常放到 /usr/local/sbin 目录下.
大多数情况下, 本地支持的软件, 不管是脚本还是编译过的程序, 都应该放到 /usr/local 目录下,  而不是在 /bin 或/usr/bin 目录下.
这些目录都是由 Linux 文件系统层次结构标准指定, 只包含由 Linux 发行商 所提供和维护的文件.

## 更多的格式技巧

严肃认真的脚本书写, 一个关键目标是为了维护方便;
也就是说, 一个脚本可以轻松地被作者或其它 用户修改,使它适应变化的需求. 使脚本容易阅读和理解是一种方便维护的方法.

### 长选项名称

我们学过的许多命令都以长短两种选项名称为特征. 例如, 这个 ls 命令有许多选项既可以用短形式也 可以用长形式来表示. 例如:

```bash
$ ls -ad
# 和:
$ ls --all --directory
```

是等价的命令. 为了减少输入, 当在命令行中输入选项的时候, 短选项更受欢迎, 但是当书写脚本的时候,  长选项能提供可读性.

### 缩进和行继续符

当使用长命令的时候, 通过把命令在几个文本行中展开, 可以提高命令的可读性.  在第十八章中, 我们看到了一个特别长的 find 命令实例:

```bash
$ find playground \( -type f -not -perm 0600 -exec
chmod 0600 "{}" ";" \) -or \( -type d -not -perm 0711 -exec chmod
0711 "{}" ";" \)
```

显然, 这个命令有点儿难理解, 当第一眼看到它的时候. 在脚本中, 这个命令可能会比较容易 理解, 如果这样书写它:

```bash
find playground \
\( \
-type f \
-not -perm 0600 \
-exec chmod 0600 "{}" ";" \
\) \
-or \
\( \
-type d \
-not -perm 0711 \
-exec chmod 0711 "{}" ";" \
\)
```

通过使用行继续符(反斜杠-回车符序列)和缩进, 这个复杂命令的逻辑性更清楚地描述给读者.
这个技巧在命令行中同样生效, 虽然很少使用它, 因为输入和编辑这个命令非常麻烦.
脚本和 命令行的一个区别是, 脚本可能使用 tab 字符拉实现缩进, 然而命令行却不能, 因为 tab 字符被用来 激活自动补全功能.

### 为书写脚本配置 vim

这个 vim 文本编辑器有许多许多的配置设置. 有几个常见的选项能够有助于脚本书写:

```conf
:syntax on
```

打开语法高亮. 通过这个设置, 当查看脚本的时候, 不同的 shell 语法元素会以不同的颜色 显示.
这对于识别某些编程错误很有帮助. 并且它看起来也很酷.
注意为了这个功能起作用, 你 必须安装了一个完整的 vim 版本, 并且你编辑的文件必须有一个 shebang, 来说明这个文件是 一个 shell 脚本.
如果对于上面的命令, 你遇到了困难, 试试 `:set syntax=sh`.

```conf
:set hlsearch
```

打开这个选项是为了高亮查找结果. 比如说我们查找单词"echo".
通过设置这个选项, 这个 单词的每个实例会高亮显示.

```conf
:set tabstop=4
```

设置一个 tab 字符所占据的列数. 默认是8列. 把这个值设置为4(一种常见做法),  从而让长文本行更容易适应屏幕.

    :set autoindent

打开 "auto indent" 功能. 这导致 vim 能对新的文本行缩进与刚输入的文本行相同的列数.
对于许多编程结构来说, 这就加速了输入. 停止缩进, 输入 Ctrl-d.

通过把这些命令(没有开头的冒号字符)添加到你的 ~/.vimrc 文件中, 这些改动会永久生效.

### 总结归纳

在这脚本编写的第一章中, 我们已经看过怎样编写脚本, 怎样让它们在我们的系统中轻松地执行.
我们也知道了怎样使用各种格式技巧来提高脚本的可读性(可维护性).
在以后的各章中, 轻松维护 会作为编写好脚本的中心法则一次又一次地出现.

### 拓展阅读

查看各种各样编程语言的"Hello World"程序和实例:
http://en.wikipedia.org/wiki/Hello_world
这篇 Wikipedia 文章讨论了更多关于 shebang 机制的内容:
http://en.wikipedia.org/wiki/Shebang_(Unix)

## 第二十六章: 启动一个项目

从这一章开始, 我们将建设一个项目.
这个项目的目的是为了了解怎样使用各种各样的 shell 功能来 创建程序,更重要的是, 创建好程序.

我们将要编写的程序是一个报告生成器.
它会显示系统的各种统计数据和它的状态, 并将产生 HTML 格式的报告,
所以我们能通过网络浏览器, 比如说 Firefox 或者 Konqueror, 来查看这个报告.
通常, 创建程序要经过一系列阶段, 每个阶段会添加新的特性和功能.
我们程序的第一个阶段将会 产生一个非常小的 HTML 网页, 其不包含系统信息.
随后我们会添加这些信息.

### 第一阶段: 最小的文档

首先我们需要知道的事是一个规则的 HTML 文档的格式. 它看起来像这样:

```html
<HTML>
<HEAD>
<TITLE>Page Title</TITLE>
</HEAD>
<BODY>
Page body.
</BODY>
</HTML>
```

如果我们将这些内容输入到文本编辑器中, 并把文件保存为 foo.html,
然后我们就能在 Firefox 中 使用下面的URL 来查看文件内容:

```html
file:///home/username/foo.html
```

程序的第一个阶段将这个 HTML 文件输出到标准输出. 我们可以编写一个程序, 相当容易地完成这个任务.
启动我们的文本编辑器, 然后创建一个名为 ~/bin/sys_info_page 的新文件:

```bash
$ vim ~/bin/sys_info_page
```

随后输入下面的程序:

```bash
#!/bin/bash
# Program to output a system information page
echo "<HTML>"

echo "<HEAD>"
echo "<TITLE>Page Title</TITLE>"
echo "</HEAD>"
echo "<BODY>"
echo "Page body."
echo "</BODY>"
echo "</HTML>"
```

我们第一次尝试解决这个问题, 程序包含了一个 shebang, 一条注释(总是一个好主意)和一系列的 echo 命令,
每个命令负责输出一行文本. 保存文件之后, 我们将让它成为可执行文件, 再尝试运行它:

```bash
$ chmod 755 ~/bin/sys_info_page
$ sys_info_page
```

当程序运行的时候, 我们应该看到 HTML 文本在屏幕上显示出来, 因为脚本中的 echo 命令会输出 发送到标准输出.
我们再次运行这个程序, 把程序的输出重定向到文件 sys_info_page.html 中,  从而我们可以通过网络浏览器来查看输出结果:

```bash
$ sys_info_page > sys_info_page.html
$ firefox sys_info_page.html
```

到目前为止, 一切顺利.
在编写程序的时候, 尽量做到简单明了, 这总是一个好主意.
当一个程序易于阅读和理解的时候,  维护它也就更容易, 更不用说, 通过减少键入量, 可以使程序更容易书写了.
我们当前的程序版本 工作正常, 但是它可以更简单些.
实际上, 我们可以把所有的 echo 命令结合成一个 echo 命令, 当然 这样能更容易地添加更多的文本行到程序的输出中. 那么, 把我们的程序修改为:

```bash
#!/bin/bash
# Program to output a system information page
echo "<HTML>
<HEAD>
<TITLE>Page Title</TITLE>
</HEAD>
<BODY>
Page body.
</BODY>
</HTML>"
```

一个带引号的字符串可能包含换行符, 因此可以包含多个文本行. Shell 会持续读取文本直到它遇到 右引号.
它在命令行中也是这样工作的:

```bash
$ echo "<HTML>
> <HEAD>
>       <TITLE>Page Title</TITLE>
> </HEAD>
> <BODY>
> Page body.
> </BODY>
></HTML>"
```

开头的 ">" 字符是包含在 PS2shell 变量中的 shell 提示符.
每当我们在 shell 中键入多行语句的时候,  这个提示符就会出现.
现在这个功能有点儿晦涩, 但随后, 当我们介绍多行编程语句时, 它会派上大用场.

## 第二阶段: 添加一点儿数据

现在我们的程序能生成一个最小的文档, 让我们给报告添加些数据吧. 为此, 我们将做 以下修改:

```bash
#!/bin/bash
# Program to output a system information page
echo "<HTML>
<HEAD>
<TITLE>System Information Report</TITLE>
</HEAD>
<BODY>
<H1>System Information Report</H1>
</BODY>
</HTML>"
```

我们增加了一个网页标题, 并且在报告正文部分加了一个标题.

### 变量和常量

然而, 我们的脚本存在一个问题. 请注意字符串 "System Information Report" 是怎样被重复使用的?
对于这个微小的脚本而言, 它不是一个问题, 但是让我们设想一下,  我们的脚本非常冗长, 并且我们有许多这个字符串的实例.
如果我们想要更换一个标题, 我们必须 对脚本中的许多地方做修改, 这会是很大的工作量.
如果我们能整理一下脚本, 让这个字符串只 出现一次而不是多次, 会怎样呢? 这样会使今后的脚本维护工作更加轻松.
我们可以这样做:

```bash
#!/bin/bash
# Program to output a system information page
title="System Information Report"
echo "<HTML>
<HEAD>
<TITLE>$title</TITLE>
</HEAD>
<BODY>
<H1>$title</H1>
</BODY>
</HTML>"
```

通过创建一个名为 title 的变量,
并把 "System Information Report" 字符串赋值给它, 我们就可以利用参数展开功能, 把这个字符串放到文件中的多个位置.
那么, 我们怎样来创建一个变量呢? 很简单, 我们只管使用它.
当 shell 碰到一个变量的时候, 它会 自动地创建它. 这不同于许多编程语言, 它们中的变量在使用之前, 必须显式的声明或是定义.
关于 这个问题, shell 要求非常宽松, 这可能会导致一些问题. 例如, 考虑一下在命令行中发生的这种情形:

```bash
$ foo="yes"
$ echo $foo
yes
$ echo $fool
$
```

首先我们把 "yes" 赋给变量 foo, 然后用 echo 命令来显示变量值.
接下来, 我们显示拼写错误的变量名"fool" 的变量值, 然后得到一个空值.
这是因为 shell 很高兴地创建了变量 fool, 当 shell 遇到 fool 的时候,并且赋给 fool 一个空的默认值.
因此, 我们必须小心谨慎地拼写! 同样理解实例中究竟发生了什么事情也 很重要.
从我们以前学习 shell 执行展开操作, 我们知道这个命令:

```bash
$ echo $foo
# 经历了参数展开操作, 然后得到:
$ echo yes
# 然而这个命令:
$ echo $fool
# 展开为:
$ echo
```

这个空变量展开值为空! 对于需要参数的命令来说, 这会引起混乱. 下面是一个例子:

```bash
$ foo=foo.txt
$ foo1=foo1.txt
$ cp $foo $fool
cp: missing destination file operand after `foo.txt'
Try `cp --help' for more information.
```

我们给两个变量赋值, foo 和 foo1. 然后我们执行 cp 操作, 但是拼写错了第二个参数的名字.
参数展开之后,这个 cp 命令只接受到一个参数, 虽然它需要两个.
有一些关于变量名的规则:

1. 变量名可由字母数字字符(字母和数字)和下划线字符组成.
2. 变量名的第一个字符必须是一个字母或一个下划线.
3. 变量名中不允许出现空格和标点符号.

单词 "variable" 意味着可变的值, 并且在许多应用程序当中, 都是以这种方式来使用变量的.
然而,  我们应用程序中的变量, title, 被用作一个常量. 常量有一个名字且包含一个值, 在这方面就 像是变量.
不同之处是常量的值是不能改变的.
在执行几何运算的应用程序中, 我们可以把 PI 定义为 一个常量, 并把 3.1415 赋值给它, 用它来代替数字字面值.
shell 不能辨别变量和常量; 它们大多数情况下 是为了方便程序员. 一个常用惯例是指定大写字母来表示常量, 小写字母表示真正的变量.
我们 将修改我们的脚本来遵从这个惯例:

```bash
#!/bin/bash
# Program to output a system information page
TITLE="System Information Report For $HOSTNAME"
echo "<HTML>
<HEAD>
<TITLE>$title</TITLE>
</HEAD>
<BODY>
<H1>$title</H1>
</BODY>
</HTML>"
```

我们亦借此机会, 通过在标题中添加 shell 变量名 HOSTNAME, 让标题变得活泼有趣些.
这个变量名是这台机器的网络名称.

注意: 实际上, shell 确实提供了一种方法, 通过使用带有-r(只读)选项的内部命令 declare,
来强制常量的不变性. 如果我们给 TITLE 这样赋值:

那么 shell 会阻止之后给 TITLE 的任意赋值. 这个功能极少被使用, 但为了很早之前的脚本,  它仍然存在.

### 给变量和常量赋值

这里是我们真正开始使用参数扩展知识的地方. 正如我们所知道的, 这样给变量赋值:

    variable=value

这里的variable是变量的名字, value是一个字符串.
不同于一些其它的编程语言, shell 不会 在乎变量值的类型; 它把它们都看作是字符串.
通过使用带有-i 选项的 declare 命令, 你可以强制 shell 把 赋值限制为整型, 但是, 正如像设置变量为只读一样, 极少这样做.

注意在赋值过程中, 变量名, 等号和变量值之间必须没有空格.
那么, 这些值由什么组成呢?  可以展开成字符串的任意值:

```bash
a=z                                              # Assign the string "z" to variable a.
b="a string"                            # Embedded spaces must be within quotes.
c="a string and $b"         # Other expansions such as variables can be expanded into the assignment.

d=$(ls -l foo.txt)          # Arithmetic expansion.
e=$((5 * 7))        # Escape sequences such as tabs and newlines.
f="\t\ta string\n" # Results of a command.
```

可以在同一行中对多个变量赋值:

    a=5 b="a string"

在参数展开过程中, 变量名可能被花括号 "{}" 包围着.
由于变量名周围的上下文, 其变得不明确的情况下,这会很有帮助.
这里, 我们试图把一个文件名从 myfile 改为 myfile1, 使用一个变量:

```bash
$ filename="myfile"
$ touch $filename
$ mv $filename $filename1
mv: missing destination file operand after `myfile'
Try `mv --help' for more information.
```

这种尝试失败了, 因为 shell 把 mv 命令的第二个参数解释为一个新的(并且空的)变量.
通过这种方法 可以解决这个问题:

```bash
$ mv $filename ${filename}1
```

通过添加花括号, shell 不再把末尾的1解释为变量名的一部分.

我们将利用这个机会来添加一些数据到我们的报告中, 即创建包括的日期和时间, 以及创建者的用户名:

```bash
#!/bin/bash
# Program to output a system information page
TITLE="System Information Report For $HOSTNAME"
CURRENT_TIME=$(date +"%x %r %Z")
TIME_STAMP="Generated $CURRENT_TIME, by $USER"
echo "<HTML>
<HEAD>
<TITLE>$TITLE</TITLE>
</HEAD>
<BODY>
<H1>$TITLE</H1>
<P>$TIME_STAMP</P>
</BODY>
</HTML>"
```

## Here Documents

我们已经知道了两种不同的文本输出方法, 两种方法都使用了 echo 命令.
还有第三种方法, 叫做 here document 或者 here script.
一个 here document 是另外一种 I/O 重定向形式, 我们 在脚本文件中嵌入正文文本, 然后把它发送给一个命令的标准输入. 它这样工作:

```bash
command << token
text
token
```

这里的 command 是一个可以接受标准输入的命令名, token 是一个用来指示嵌入文本结束的字符串.
我们将修改我们的脚本, 来使用一个 here document:

```bash
#!/bin/bash
# Program to output a system information page
TITLE="System Information Report For $HOSTNAME"
CURRENT_TIME=$(date +"%x %r %Z")
TIME_STAMP="Generated $CURRENT_TIME, by $USER"
cat << _EOF_
<HTML>
<HEAD>
<TITLE>$TITLE</TITLE>
</HEAD>
<BODY>
<H1>$TITLE</H1>
<P>$TIME_STAMP</P>
</BODY>
</HTML>
_EOF_
```

取代 echo 命令, 现在我们的脚本使用 cat 命令和一个 here document.
这个字符串_EOF_(意思是"文件结尾",  一个常见用法)被选作为 token, 并标志着嵌入文本的结尾.
注意这个 token 必须在一行中单独出现,并且文本行中 没有末尾的空格.

那么使用一个 here document 的优点是什么呢? 它很大程度上和 echo 一样,
除了默认情况下, heredocuments 中的单引号和双引号会失去它们在 shell 中的特殊含义.
这里有一个命令中的例子:

```bash
$ foo="some text"
$ cat << _EOF_
> $foo
> "$foo"
> '$foo'
> \$foo
> _EOF_
some text
"some text"
'some text'
$foo
```

正如我们所见到的, shell 根本没有注意到引号.
它把它们看作是普通的字符. 这就允许我们 在一个 here document 中可以随意的嵌入引号.
 对于我们的报告程序来说, 这将是非常方便的. Here documents 可以和任意能接受标准输入的命令一块使用.
在这个例子中, 我们使用了 一个 here document 将一系列的命令传递到这个 ftp 程序中, 为的是从一个远端 FTP 服务器中得到一个文件:

```bash
#!/bin/bash
# Script to retrieve a file via FTP
FTP_SERVER=ftp.nl.debian.org
FTP_PATH=/debian/dists/lenny/main/installer-i386/current/images/cdrom
REMOTE_FILE=debian-cd_info.tar.gz
ftp -n << _EOF_
open $FTP_SERVER
user anonymous me@linuxbox
cd $FTP_PATH
hash
get $REMOTE_FILE
bye
_EOF_
ls -l $REMOTE_FILE
```

如果我们把重定向操作符从 "<<" 改为 "<<-", shell 会忽略在此 here document 中开头的 tab 字符.
这就能缩进一个 here document, 从而提高脚本的可读性:

```bash
#!/bin/bash
# Script to retrieve a file via FTP
FTP_SERVER=ftp.nl.debian.org
FTP_PATH=/debian/dists/lenny/main/installer-i386/current/images/cdrom
REMOTE_FILE=debian-cd_info.tar.gz
ftp -n <<- _EOF_
open $FTP_SERVER
user anonymous me@linuxbox
cd $FTP_PATH
hash
get $REMOTE_FILE
bye
_EOF_
ls -l $REMOTE_FILE
```

### 总结归纳

在这一章中, 我们启动了一个项目, 其带领我们领略了创建一个成功脚本的整个过程.
同时我们介绍了变量和常量的概念, 以及怎样使用它们. 它们是我们将找到的众多参数展开应用程序中的第一批实例.
我们也知道了怎样从我们的脚本文件中产生输出, 及其各种各样嵌入文本块的方法.

## 拓展阅读

关于 HTML 的更多信息, 查看下面的文章和教材:

http://en.wikipedia.org/wiki/Html
http://en.wikibooks.org/wiki/HTML_Programming
http://html.net/tutorials/html/
Bash 手册包括一节"HERE DOCUMENTS"的内容, 其详细的讲述了这个功能.

## 第二十七章: 自顶向下设计

随着程序变得更加庞大和复杂, 设计, 编码和维护它们也变得更加困难.
对于任意一个大项目而言,  把繁重, 复杂的任务分割为细小且简单的任务, 往往是一个好主意.
想象一下, 我们试图描述 一个平凡无奇的工作, 一位火星人要去市场买食物.
我们可能通过下面一系列步骤来形容整个过程:

+ 上车
+ 开车到市场
+ 停车
+ 买食物
+ 回到车中
+ 开车回家
+ 回到家中

然而, 火星人可能需要更详细的信息. 我们可以进一步细化子任务"停车"为这些步骤:

+ 找到停车位
+ 开车到停车位
+ 关闭引擎
+ 拉紧手刹
+ 下车
+ 锁车

这个"关闭引擎"子任务可以进一步细化为这些步骤, 包括"关闭点火装置", "移开点火匙"等等,
直到 已经完整定义了要去市场买食物整个过程的每一个步骤.

这种先确定上层步骤, 然后再逐步细化这些步骤的过程被称为自顶向下设计.
这种技巧允许我们 把庞大而复杂的任务分割为许多小而简单的任务.
自顶向下设计是一种常见的程序设计方法,  尤其适合 shell 编程.
在这一章中, 我们将使用自顶向下的设计方法来进一步开发我们的报告产生器脚本.

### Shell 函数

目前我们的脚本执行以下步骤来产生这个 HTML 文档:

+ 打开网页
+ 打开网页标头
+ 设置网页标题
+ 关闭网页标头
+ 打开网页主体部分
+ 输出网页标头
+ 输出时间戳
+ 关闭网页主体
+ 关闭网页

为了下一阶段的开发, 我们将在步骤7和8之间添加一些额外的任务. 这些将包括:
系统正常运行时间和负载.  这是自上次关机或重启之后系统的运行时间,
以及在几个时间间隔内当前运行在处理 中的平均任务量. 磁盘空间. 系统中存储设备的总使用量. 家目录空间. 每个用户所使用的存储空间数量.

如果对于每一个任务, 我们都有相应的命令, 那么通过命令替换, 我们就能很容易地把它们添加到我们的脚本中:

```bash
#!/bin/bash
# Program to output a system information page
TITLE="System Information Report For $HOSTNAME"
CURRENT_TIME=$(date +"%x %r %Z")
TIME_STAMP="Generated $CURRENT_TIME, by $USER"
cat << _EOF_
<HTML>
<HEAD>
<TITLE>$TITLE</TITLE>
</HEAD>
<BODY>
<H1>$TITLE</H1>
<P>$TIME_STAMP</P>
$(report_uptime)
$(report_disk_space)
$(report_home_space)
</BODY>
</HTML>
_EOF_
```

我们能够用两种方法来创建这些额外的命令.
我们可以分别编写三个脚本, 并把它们放置到 环境变量 PATH 所列出的目录下, 或者我们也可以把这些脚本作为 shell 函数嵌入到我们的程序中.
我们之前已经提到过, shell 函 数是位于其它脚本中的"微脚本", 作为自主程序.
Shell 函数有两种语法形式:

```bash
function name {
commands
return
}
and
name () {
commands
return
}
```

这里的 name 是函数名, commands 是一系列包含在函数中的命令.
两种形式是等价的, 可以交替使用. 下面我们将查看一个说明 shell 函数使用方法的脚本:

```bash
1    #!/bin/bash

3    # Shell function demo

5    function funct {
6    echo "Step 2"
7    return
8    }
9
10    # Main program starts here
11
12    echo "Step 1"
13    funct
14    echo "Step 3"
```

随着 shell 读取这个脚本, 它会跳过第1行到第11行的代码, 因为这些文本行由注释和函数定义组成.
从第12行 代码开始执行, 有一个 echo 命令.
第13行会调用 shell 函数 funct, 然后 shell 会执行这个函数,  就如执行其它命令一样.
这样程序控制权会转移到第六行, 执行第二个 echo 命令. 然后再执行第7行.

这个 return 命令终止这个函数, 并把控制权交给函数调用之后的代码(第14行), 从而执行最后一个 echo 命令.
注意为了使函数调用被识别出是 shell 函数, 而不是被解释为外部程序的名字,
所以在脚本中 shell 函数定义必须出现在函数调用之前.

我们将给脚本添加最小的 shell 函数定义:

```bash
#!/bin/bash
# Program to output a system information page
TITLE="System Information Report For $HOSTNAME"
CURRENT_TIME=$(date +"%x %r %Z")
TIME_STAMP="Generated $CURRENT_TIME, by $USER"
report_uptime () {
return
}
report_disk_space () {
return
}
report_home_space () {
return
}
cat << _EOF_
<HTML>
<HEAD>
<TITLE>$TITLE</TITLE>
</HEAD>
<BODY>
<H1>$TITLE</H1>
<P>$TIME_STAMP</P>
$(report_uptime)
$(report_disk_space)
$(report_home_space)
</BODY>
</HTML>
_EOF_
```

Shell 函数的命名规则和变量一样, 一个函数必须至少包含一条命令. 
这条 return 命令(是可选的)满足要求.

### 局部变量

目前我们所写的脚本中, 所有的变量(包括常量)都是全局变量. 全局变量在整个程序中保持存在.  对于许多事
情来说, 这很好, 但是有时候它会使 shell 函数的使用变得复杂. 在 shell 函数中, 经常期望 会有局部变量. 局
部变量只能在定义它们的 shell 函数中使用, 并且一旦 shell 函数执行完毕, 它们就不存在了.
拥有局部变量允许程序员使用的局部变量名, 可以与已存在的变量名相同, 这些变量可以是全局变量,  或者是其
它 shell 函数中的局部变量, 却不必担心潜在的名字冲突.
这里有一个实例脚本, 其说明了怎样来定义和使用局部变量:

```bash
#!/bin/bash
# local-vars: script to demonstrate local variables
foo=0 # global variable foo
funct_1 () {
local foo # variable foo local to funct_1
foo=1
echo "funct_1: foo = $foo"
}
funct_2 () {
local foo # variable foo local to funct_2
foo=2
echo "funct_2: foo = $foo"
}
echo "global: foo = $foo"
funct_1
echo "global: foo = $foo"
funct_2
echo "global: foo = $foo"
```

正如我们所看到的, 通过在变量名之前加上单词 local, 来定义局部变量. 
这就创建了一个只对其所在的 shell 函数起作用的变量. 
在这个 shell 函数之外, 这个变量不再存在. 当我们运行这个脚本的时候,  我们会看到这样的结果:

```bash
$ local-vars
global: foo = 0
funct_1: foo = 1
global: foo = 0
funct_2: foo = 2
global: foo = 0
```

我们看到对两个 shell 函数中的局部变量 foo 赋值, 不会影响到在函数之外定义的变量 foo 的值.
这个功能就允许 shell 函数能保持各自以及与它们所在脚本之间的独立性. 
这非常有价值, 因为它帮忙 阻止了 程序各部分之间的相互干涉.
这样 shell 函数也可以移植. 也就是说, 按照需求,  shell 函数可以在脚本之间进行剪切和粘贴.

## 保持脚本运行

当开发程序的时候, 保持程序的可执行状态非常有用. 
这样做, 并且经常测试, 我们就可以在程序 开发过程的早期检测到错误. 这将使调试问题容易多了. 
例如, 如果我们运行这个程序, 做一个小的修改,  然后再次执行这个程序, 最后发现一个问题, 非常有可能这个最新的修改就是问题的来源. 
通过添加空函数,  程序员称之为占位符, 我们可以在早期阶段证明程序的逻辑流程. 
当构建一个占位符的时候,  能够包含一些为程序员提供反馈信息的代码是一个不错的主意, 这些信息展示了正在执行的逻辑流程.  
现在看一下我们脚本的输出结果:

```bash
$ sys_info_page
<HTML>
<HEAD>
<TITLE>System Information Report For twin2</TITLE>
</HEAD>
<BODY>
<H1>System Information Report For linuxbox</H1>
<P>Generated 03/19/2009 04:02:10 PM EDT, by me</P>
</BODY>
</HTML>
```

我们看到时间戳之后的输出结果中有一些空行, 但是我们不能确定这些空行产生的原因.
如果我们 修改这些函数, 让它们包含一些反馈信息:

```bash
report_uptime () {
echo "Function report_uptime executed."
return
}
report_disk_space () {
echo "Function report_disk_space executed."
return
}
report_home_space () {
echo "Function report_home_space executed."
return
}
```

然后再次运行这个脚本:

```bash
$ sys_info_page
<HTML>
<HEAD>
<TITLE>System Information Report For linuxbox</TITLE>
</HEAD>
<BODY>
<H1>System Information Report For linuxbox</H1>
<P>Generated 03/20/2009 05:17:26 AM EDT, by me</P>
Function report_uptime executed.
Function report_disk_space executed.
Function report_home_space executed.
</BODY>
</HTML>
```

现在我们看到, 事实上, 执行了三个函数.
我们的函数框架已经各就各位并且能工作, 是时候更新一些函数代码了. 首先, 是 report_uptime 函数:

```bash
report_uptime () {
cat <<- _EOF_
<H2>System Uptime</H2>
<PRE>$(uptime)</PRE>
_EOF_
return
}
```

这些代码相当直截了当. 
我们使用一个 here 文档来输出标题和 uptime 命令的输出结果, 命令结果被 标签包围,  为的是保持命令的输出格式.
这个 report_disk_space 函数类似:

```bash
report_disk_space () {
cat <<- _EOF_
<H2>Disk Space Utilization</H2>
<PRE>$(df -h)</PRE>
_EOF_
return
}
```

这个函数使用 df -h 命令来确定磁盘空间的数量. 最后, 我们将建造 report_home_space 函数:

```bash
report_home_space () {
cat <<- _EOF_
<H2>Home Space Utilization</H2>
<PRE>$(du -sh /home/*)</PRE>
_EOF_
return
}
```

我们使用带有 -sh 选项的 du 命令来完成这个任务. 然而, 这并不是此问题的完整解决方案.
虽然它会 在一些系统(例如 Ubuntu)中起作用, 但是在其它系统中它不工作. 
这是因为许多系统会设置家目录的 权限, 以此阻止其它用户读取它们, 这是一个合理的安全措施. 
在这些系统中, 这个 report_home_space 函数,  只有用超级用户权限执行我们的脚本时, 才会工作.
一个更好的解决方案是让脚本能根据用户的使用权限来 调整自己的行为.
我们将在下一章中讨论这个问题.

>你的 .bashrc 文件中的 shell 函数
>
>Shell 函数是更为完美的别名替代物, 实际上是创建较小的个人所用命令的首选方法. 
>别名 非常局限于命令的种类和它们支持的 shell 功能, 然而 shell 函数允许任何可以编写脚本的东西.  
>例如, 如果我们喜欢 为我们的脚本开发的这个 report_disk_space shell 函数, 
>我们可以为我们的 .bashrc 文件 创建一个相似的名为 ds 的函数:
>
>```bash
>ds () {
>echo "Disk Space Utilization For $HOSTNAME"
>df -h
>}
>```

这一章中, 我们介绍了一种常见的程序设计方法, 叫做自顶向下设计, 
并且我们知道了怎样 使用 shell 函数按照要求来完成逐步细化的任务.
我们也知道了怎样使用局部变量使 shell 函数 独立于其它函数, 以及其所在程序的其它部分.
这就有可能使 shell 函数以可移植的方式编写,  并且能够重复使用, 通过把它们放置到多个程序中; 节省了大量的时间.

### 拓展阅读

Wikipedia 上面有许多关于软件设计原理的文章. 这里是一些好文章:
http://en.wikipedia.org/wiki/Top-down_design
http://en.wikipedia.org/wiki/Subroutines

## 第二十八章: 流程控制 if分支结构

在上一章中, 我们遇到一个问题. 怎样使我们的报告生成器脚本能适应运行此脚本的用户的权限?  
这个问题的解决方案要求我们能找到一种方法, 在脚本中基于测试条件结果, 来"改变方向".  
用编程术语表达, 就是我们需要程序可以分支. 
让我们考虑一个简单的用伪码表示的逻辑实例,  伪码是一种模拟的计算机语言, 为的是便于人们理解:

```bash
X=5
If X = 5, then:
Say "X equals 5."
Otherwise:
Say "X is not equal to 5."
```

这就是一个分支的例子. 
根据条件, "Does X = 5?" 做一件事情, "Say X equals 5," 否则, 做另一件事情, "Say X is not equal to 5."

### if

使用 shell, 我们可以编码上面的逻辑, 如下所示:

```bash
x=5
if [ $x = 5 ]; then
echo "x equals 5."
else
echo "x does not equal 5."
fi
```

或者我们可以直接在命令行中输入以上代码(略有缩短):

```bash
$ x=5
$ if [ $x = 5 ]; then echo "equals 5"; else echo "does not equal 5"; fi
equals 5
$ x=0
$ if [ $x = 5 ]; then echo "equals 5"; else echo "does not equal 5"; fi
does not equal 5
```

在这个例子中, 我们执行了两次这个命令. 第一次是, 把 x 的值设置为5, 从而导致输出字符串"equals 5", 
第二次是, 把 x 的值设置为0, 从而导致输出字符串"does not equal 5".
这个 if 语句语法如下:

```bash
if commands; then
commands
[elif commands; then
commands...]
[else
commands]
fi
```

这里的 commands 是指一系列命令. 第一眼看到会有点儿困惑. 
但是在我们弄清楚这些语句之前, 我们 必须看一下 shell 是如何评判一个命令的成功与失败的.

## 退出状态

当命令执行完毕后, 命令(包括我们编写的脚本和 shell 函数)会给系统发送一个值, 叫做退出状态.  
这个值是一个 0 到 255 之间的整数, 说明命令执行成功或是失败. 
按照惯例, 一个零值说明成功, 其它所有值说明失败.
Shell 提供了一个参数, 我们可以用它检查退出状态. 用具体实例看一下:

```bash
$ ls -d /usr/bin
/usr/bin
$ echo $?
0
$ ls -d /bin/usr
ls: cannot access /bin/usr: No such file or directory
$ echo $?
2
```

在这个例子中, 我们执行了两次 ls 命令. 第一次, 命令执行成功. 
如果我们显示参数 $? 的值, 我们 看到它是零. 我们第二次执行 ls 命令的时候, 产生了一个错误, 并再次查看参数 $? . 
这次它包含一个 数字 2, 表明这个命令遇到了一个错误. 
有些命令使用不同的退出值, 来诊断错误, 而许多命令当 它们执行失败的时候, 会简单地退出并发送一个数字1.
手册页中经常会包含一章标题为"退出状态"的内容,  描述了使用的代码. 然而, 一个零总是表明成功. 
这个 shell 提供了两个极其简单的内部命令, 它们不做任何事情, 除了以一个零或1退出状态来终止执行.  
True 命令总是执行成功, 而 false 命令总是执行失败:

```bash
$ true
$ echo $?
0

$ false
$ echo $?
1
```

我们能够使用这些命令, 来看一下 if 语句是怎样工作的. If 语句真正做的事情是计算命令执行成功或失败:

```bash
$ if true; then echo "It's true."; fi
It's true.
$ if false; then echo "It's true."; fi
$
```

当 if 之后的命令执行成功的时候, 命令 echo "It"s true." 将会执行, 否则此命令不执行.  
如果 if 之后跟随一系列命令, 则将计算列表中的最后一个命令:

```bash
$ if false; true; then echo "It's true."; fi
It's true.
$ if true; false; then echo "It's true."; fi
$
```

## 测试

到目前为止, 经常与 if 一块使用的命令是 test. 这个 test 命令执行各种各样的检查与比较.  
它有两种等价模式:

    test expression

比较流行的格式是:
    
    [ expression ]

这里的 expression 是一个表达式, 其执行结果是 true 或者是 false. 
当表达式为真时, 这个 test 命令返回一个零 退出状态, 当表达式为假时, test 命令退出状态为1.

## 文件表达式

以下表达式被用来计算文件状态:

表28-1: 测试文件表达式
表达式 如果为真

+ `file1 -ef file2` ;   `file1` 和 file2 拥有相同的索引号(通过硬链接两个文件名指向相同的文件).
+ `file1 -nt file2` ;   `file1` 新于` file2`.
+ `file1 -ot file2` ;   `file1` 早于 `file2`.
+ `-b file` ;   `file` 存在并且是一个块(设备)文件.
+ `-c file` ;   `file` 存在并且是一个字符(设备)文件.
+ `-d file` ;   `file` 存在并且是一个目录.
+ `-e file` ;   `file` 存在.
+ `-f file` ;   `file` 存在并且是一个普通文件.
+ `-g file` ;   `file` 存在并且设置了组 ID.
+ `-G file` ;   `file` 存在并且由有效组 ID 拥有.
+ `-k file` ;   `file` 存在并且设置了它的"sticky bit".
+ `-L file` ;   `file` 存在并且是一个符号链接.
+ `-O file` ;   `file` 存在并且由有效用户 ID 拥有.
+ `-p file` ;   `file` 存在并且是一个命名管道.
+ `-r file` ;   `file` 存在并且可读(有效用户有可读权限).
+ `-s file` ;   `file` 存在且其长度大于零.
+ `-S file` ;   `file` 存在且是一个网络 socket.
+ `-t fd`   ;   `fd` 是一个定向到终端／从终端定向的文件描述符 .  这可以被用来决定是否重定向了标准输入／输出错误.
+ `-u file` ;    `file` 存在并且设置了 setuid 位.
+ `-w file` ;   `file` 存在并且可写(有效用户拥有可写权限).
+ `-x file` ;   `file` 存在并且可执行(有效用户有执行／搜索权限).

这里我们有一个脚本说明了一些文件表达式:

```bash
#!/bin/bash
# test-file: Evaluate the status of a file
FILE=~/.bashrc
if [ -e "$FILE" ]; then
if [ -f "$FILE" ]; then
echo "$FILE is a regular file."
fi
if [ -d "$FILE" ]; then
echo "$FILE is a directory."
fi
if [ -r "$FILE" ]; then
echo "$FILE is readable."
fi
if [ -w "$FILE" ]; then
echo "$FILE is writable."
fi
if [ -x "$FILE" ]; then
echo "$FILE is executable/searchable."
fi
else
echo "$FILE does not exist"
exit 1
fi
exit
```

这个脚本会计算赋值给常量 FILE 的文件, 并显示计算结果. 
对于此脚本有两点需要注意. 第一个,  在表达式中参数 $FILE 是怎样被引用的. 
引号并不是必需的, 但这是为了防范空参数. 

如果 $FILE 的参数展开 是一个空值, 就会导致一个错误(操作符将会被解释为非空的字符串而不是操作符). 
用引号把参数引起来就 确保了操作符之后总是跟随着一个字符串, 即使字符串为空. 
第二个, 注意脚本末尾的 exit 命令.  这个 exit 命令接受一个单独的, 可选的参数, 其成为脚本的退出状态. 
当不传递参数时, 退出状态默认为零.  

以这种方式使用 exit 命令, 则允许此脚本提示失败如果 $FILE 展开成一个不存在的文件名. 
这个 exit 命令 出现在脚本中的最后一行, 是一个当一个脚本"运行到最后"(到达文件末尾), 不管怎样,  默认情况下它以退出状态零终止.

类似地, 通过带有一个整数参数的 return 命令, shell 函数可以返回一个退出状态. 
如果我们打算把 上面的脚本转变为一个 shell 函数, 为了在更大的程序中包含此函数, 
我们用 return 语句来代替 exit 命令,  则得到期望的行为:

```bash
test_file () {
# test-file: Evaluate the status of a file
FILE=~/.bashrc
if [ -e "$FILE" ]; then
if [ -f "$FILE" ]; then
echo "$FILE is a regular file."
fi
if [ -d "$FILE" ]; then
echo "$FILE is a directory."
fi
if [ -r "$FILE" ]; then
echo "$FILE is readable."
fi
if [ -w "$FILE" ]; then
echo "$FILE is writable."
fi
if [ -x "$FILE" ]; then
echo "$FILE is executable/searchable."
fi
else
echo "$FILE does not exist"
return 1
fi
}
```

## 字符串表达式

以下表达式用来计算字符串:

表28-2: 测试字符串表达式
表达式 如果为真...

+ `string` ; `string` 不为 `null`.
+ `-n string`  ; 字符串 string 的长度大于零.
+ `-z string` ; 字符串 string 的长度为零.
+ `string1 = string2, string1 == string2` ; `string1` 和 `string2` 相同. 单或双等号都可以, 不过双等号更受欢迎.
+ `string1 != string2` ; `string1` 和 string2 不相同.
+ `string1 > string2` ; `sting1` 排列在 string2 之后.
+ `string1 < string2` ; `string1` 排列在 string2 之前.

警告: 这个 > 和 <表达式操作符必须用引号引起来(或者是用反斜杠转义),  当与 test 一块使用的时候. 
如果不这样, 它们会被 shell 解释为重定向操作符, 造成潜在地破坏结果.  
同时也要注意虽然 bash 文档声明排序遵从当前语系的排列规则, 但并不这样. 
将来的 bash 版本, 包含 4.0,  使用 ASCII(POSIX)排序规则. 这是一个演示这些问题的脚本:

```bash
#!/bin/bash
# test-string: evaluate the value of a string
ANSWER=maybe
if [ -z "$ANSWER" ]; then
echo "There is no answer." >&2
exit 1
fi
if [ "$ANSWER" = "yes" ]; then
echo "The answer is YES."
elif [ "$ANSWER" = "no" ]; then
echo "The answer is NO."
elif [ "$ANSWER" = "maybe" ]; then
echo "The answer is MAYBE."
else
echo "The answer is UNKNOWN."
fi
```

在这个脚本中, 我们计算常量 ANSWER. 我们首先确定是否此字符串为空. 
如果为空, 我们就终止 脚本, 并把退出状态设为零. 注意这个应用于 echo 命令的重定向操作. 

其把错误信息 "There is no answer." 重定向到标准错误, 这是处理错误信息的"合理"方法. 
如果字符串不为空, 我们就计算 字符串的值, 看看它是否等于"yes," "no," 或者"maybe". 
为此使用了 elif, 它是 "else if" 的简写.  通过使用 elif, 我们能够构建更复杂的逻辑测试.

## 整型表达式

下面的表达式用于整数:

表28-3: 测试整数表达式
表达式 如果为真...

+ `integer1 -eq integer2`   ;   `integer1` 等于 `integer2`.
+ `integer1 -ne integer2`   ;   `integer1` 不等于 `integer2`.
+ `integer1 -le integer2`   ;   `integer1` 小于或等于 `integer2`.
+ `integer1 -lt integer2`   ;   `integer1` 小于 `integer2`.
+ `integer1 -ge integer2`   ;   `integer1` 大于或等于 `integer2`.
+ `integer1 -gt integer2`   ;   `integer1` 大于 `integer2`.

这里是一个演示以上表达式用法的脚本:

```bash
#!/bin/bash
# test-integer: evaluate the value of an integer.
INT=-5
if [ -z "$INT" ]; then
echo "INT is empty." >&2
exit 1
fi
if [ $INT -eq 0 ]; then
echo "INT is zero."
else
if [ $INT -lt 0 ]; then
echo "INT is negative."
else
echo "INT is positive."
fi
if [ $((INT % 2)) -eq 0 ]; then
echo "INT is even."
else
echo "INT is odd."
fi
fi
```

这个脚本中有趣的地方是怎样来确定一个整数是偶数还是奇数. 
通过用模数2对数字执行求模操作,  就是用数字来除以2, 并返回余数, 从而知道数字是偶数还是奇数.

### 更现代的测试版本

目前的 bash 版本包括一个复合命令, 作为加强的 test 命令替代物. 它使用以下语法:

    [[ expression ]]

这里, 类似于 test, expression 是一个表达式, 其计算结果为真或假. 
这个 [[ ]] 命令非常 相似于 test 命令(它支持所有的表达式), 但是增加了一个重要的新的字符串表达式:

    string1 =~ regex

其返回值为真, 如果 string1匹配扩展的正则表达式 regex. 这就为执行比如数据验证等任务提供了许多可能性.  
在我们前面的整数表达式示例中, 如果常量 INT 包含除了整数之外的任何数据, 脚本就会运行失败. 
这个脚本 需要一种方法来证明此常量包含一个整数. 
使用 [[ ]] 和 =~ 字符串表达式操作符, 我们能够这样来改进脚本:

```bash
#!/bin/bash
# test-integer2: evaluate the value of an integer.
INT=-5
if [[ "$INT" =~ ^-?[0-9]+$ ]]; then
if [ $INT -eq 0 ]; then
echo "INT is zero."
else
if [ $INT -lt 0 ]; then
echo "INT is negative."
else
echo "INT is positive."
fi
if [ $((INT % 2)) -eq 0 ]; then
echo "INT is even."
else
echo "INT is odd."
fi
fi
else
echo "INT is not an integer." >&2
exit 1
fi
```

通过应用正则表达式, 我们能够限制 INT 的值只是字符串, 其开始于一个可选的减号, 随后是一个或多个数字.
这个表达式也消除了空值的可能性. 

[[ ]] 添加的另一个功能是 == 操作符支持类型匹配, 正如路径名展开所做的那样. 例如:

```bash
$ FILE=foo.bar
$ if [[ $FILE == foo.* ]]; then
> echo "$FILE matches pattern 'foo.*'"
> fi
foo.bar matches pattern 'foo.*'
```

这就使 [[ ]] 有助于计算文件和路径名.

### (( )) - 为整数设计

除了 [[ ]] 复合命令之外, bash 也提供了(( )) 复合命名, 其有利于操作整数. 
它支持一套 完整的算术计算, 我们将在第35章中讨论这个主题.

(( )) 被用来执行算术真测试. 如果算术计算的结果是非零值, 则一个算术真测试值为真.

```bash
$ if ((1)); then echo "It is true."; fi
It is true.
$ if ((0)); then echo "It is true."; fi
$
```

使用 (( )) , 我们能够略微简化 test-integer2脚本, 像这样:

```bash
#!/bin/bash
# test-integer2a: evaluate the value of an integer.
INT=-5
if [[ "$INT" =~ ^-?[0-9]+$ ]]; then
if ((INT == 0)); then
echo "INT is zero."
else
if ((INT < 0)); then
echo "INT is negative."
else
echo "INT is positive."
fi
if (( ((INT % 2)) == 0)); then
echo "INT is even."
else
echo "INT is odd."
fi
fi
else
echo "INT is not an integer." >&2
exit 1
fi
```

注意我们使用小于和大于符号, 以及==用来测试是否相等. 这是使用整数较为自然的语法了. 
也要 注意, 因为复合命令 (( )) 是 shell 语法的一部分, 而不是一个普通的命令, 而且它只处理整数, 
所以它能够通过名字识别出变量, 而不需要执行展开操作.
我们将在第35中进一步讨论 (( )) 命令 和相关的算术展开操作.

## 结合表达式

也有可能把表达式结合起来创建更复杂的计算.
通过使用逻辑操作符来结合表达式. 我们 在第18章中已经知道了这些, 当我们学习 find 命令的时候. 
它们是用于 test 和 [[ ]] 三个逻辑操作.  它们是 AND, OR, 和 NOT. 
test 和 [[ ]] 使用不同的操作符来表示这些操作:

表28-4: 逻辑操作符

`操作符`    ;   `test`  ;   `[[ ]] and (( ))`

+ `AND` ;   `-a`    ;   `&&`
+ `OR`  ;   `-o`    ;   `||`
+ `NOT` ;   `!`    ;   `!`

这里有一个 AND 操作的示例. 下面的脚本决定了一个整数是否属于某个范围内的值:

```bash
#!/bin/bash
# test-integer3: determine if an integer is within a
# specified range of values.
MIN_VAL=1
MAX_VAL=100
INT=50
if [[ "$INT" =~ ^-?[0-9]+$ ]]; then
if [[ INT -ge MIN_VAL && INT -le MAX_VAL ]]; then
echo "$INT is within $MIN_VAL to $MAX_VAL."
else
echo "$INT is out of range."
fi
else
echo "INT is not an integer." >&2
exit 1
fi
```

我们也可以对表达式使用圆括号, 为的是分组. 
如果不使用括号, 那么否定只应用于第一个 表达式, 而不是两个组合的表达式. 用 test 可以这样来编码:

```bash
if [ ! \( $INT -ge $MIN_VAL -a $INT -le $MAX_VAL \) ]; then
echo "$INT is outside $MIN_VAL to $MAX_VAL."
else
echo "$INT is in range."
fi
```

因为 test 使用的所有的表达式和操作符都被 shell 看作是命令参数（不像 [[ ]] 和 (( )) ）,  对于
bash 有特殊含义的字符, 比如说 `(`和`)`, 必须引起来或者是转义。

知道了 test 和 [[ ]] 基本上完成相同的事情, 哪一个更好呢？test 更传统（是 POSIX 的一部分）,  然而
[[ ]] 特定于 bash。知道怎样使用 test 很重要, 因为它被非常广泛地应用, 但是显然 [[ ]] 更 有助
于, 并更易于编码。

>可移植性是头脑狭隘人士的心魔
>
>如果你和"真正的"Unix 用户交谈, 你很快就会发现他们大多数人不是非常喜欢 Linux. 
>他们 认为 Linux 肮脏且不干净. Unix 追随者的一个宗旨是, 一切都应"可移植的". 
>这意味着你编写 的任意一个脚本都应当无需修改, 就能运行在任何一个类 Unix 的系统中.
>Unix 用户有充分的理由相信这一点. 
>在 POSIX 之前, Unix 用户已经看到了命令的专有扩展以及 shell 对Unix 世界的所做所为, 他们自然会警惕 Linux 对他们心爱系统的影响.
>但是可移植性有一个严重的缺点. 它防碍了进步. 
>它要求做事情要遵循"最低常见标准".  在 shell 编程这种情况下, 它意味着一切要与 sh 兼容, 最初的 Bourne shell.
>这个缺点是一个借口, 专有软件供应商用它来证明他们的专利扩展, 只有他们称他们为"创新".  
>但是他们只是为他们的客户锁定设备.
>
>GNU 工具, 比如说 bash, 就没有这些限制. 他们通过支持标准和普遍地可用性来鼓励可移植性. 
>你几乎可以 在所有类型的系统中安装 bash 和其它的 GNU 工具, 甚至是 Windows, 而没有损失. 
>所以就感觉可以自由的使用 bash 的所有功能. 它是真正的可移植.

## 控制操作符: 分支的另一种方法

bash 支持两种可以执行分支任务的控制操作符. 
这个&&(AND) 和 ||(OR) 操作符作用如同 复合命令[[ ]] 中的逻辑操作符. 这是语法:

```bash
command1 && command2
# 和
command1 || command2
```

理解这些操作很重要. 
对于 && 操作符, 先执行 command1, 如果并且只有如果 command1 执行成功后,  才会执行 command2. 
对于 || 操作符, 先执行 command1, 如果并且只有如果 command1 执行失败后,  才会执行 command2.

在实际中, 它意味着我们可以做这样的事情:

```bash
$ mkdir temp && cd temp
```

这会创建一个名为 temp 的目录, 并且若它执行成功后, 当前目录会更改为 temp. 
第二个命令会尝试 执行只有当 mkdir 命令执行成功之后. 同样地, 一个像这样的命令:

```bash
$ [ -d temp ] || mkdir temp
```

会测试目录 temp 是否存在, 并且只有测试失败之后, 才会创建这个目录. 
这种构造类型非常有助于在 脚本中处理错误, 这个主题我们将会在随后的章节中讨论更多. 
例如, 我们在脚本中可以这样做:

```bash
[ -d temp ] || exit 1
```

如果这个脚本要求目录 temp, 且目录不存在, 然后脚本会终止, 并返回退出状态1.

## 总结

这一章开始于一个问题. 我们怎样使 sys_info_page 脚本来检测是否用户拥有权限来读取所有的 家目录?
根据我们的 if 知识, 我们可以解决这个问题, 通过把这些代码添加到 report_home_space 函数中:

```bash
report_home_space () {
if [[ $(id -u) -eq 0 ]]; then
cat <<- _EOF_
<H2>Home Space Utilization (All Users)</H2>
<PRE>$(du -sh /home/*)</PRE>
_EOF_
else
cat <<- _EOF_
<H2>Home Space Utilization ($USER)</H2>
<PRE>$(du -sh $HOME)</PRE>
_EOF_
fi
return
}
```

我们计算 id 命令的输出结果. 
通过带有 -u 选项的 id 命令, 输出有效用户的数字用户 ID 号.  
超级用户总是零, 其它每个用户是一个大于零的数字. 
知道了这点, 我们能够构建两种不同的 here 文档,  一个利用超级用户权限, 另一个限制于用户拥有的家目录.
我们将暂别 sys_info_page 程序, 但不要着急. 它还会回来.
同时, 当我们继续工作的时候,  将会讨论一些我们需要的话题.

## 拓展阅读

bash 手册页中有几部分对本章中涵盖的主题提供了更详细的内容:

Lists ( 讨论控制操作符 || 和 && )
Compound Commands ( 讨论 [[ ]] , (( )) 和 if )
CONDITIONAL EXPRESSIONS （条件表达式）
SHELL BUILTIN COMMANDS ( 讨论 test 

进一步, Wikipedia 中有一篇关于伪代码概念的好文章：
http://en.wikipedia.org/wiki/Pseudocode
