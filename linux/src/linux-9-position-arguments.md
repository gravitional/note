# shell

## 第三十三章: 位置参数
现在我们的程序还缺少一种本领, 就是接收和处理命令行选项和参数的能力. 在这一章中, 我们将探究一些能 让
程序访问命令行内容的 shell 性能.
访问命令行
shell 提供了一个称为位置参数的变量集合, 这个集合包含了命令行中所有独立的单词. 这些变量按照从0到9给
予命名.  可以以这种方式讲明白:

```bash
#!/bin/bash
# posit-param: script to view command line parameters
echo "
\$0 = $0
\$1 = $1
\$2 = $2
\$3 = $3
\$4 = $4
\$5 = $5
\$6 = $6
\$7 = $7
\$8 = $8
\$9 = $9
"
```

一个非常简单的脚本, 显示从 $0 到 $9 所有变量的值. 当不带命令行参数执行该脚本时, 输出结果如下:

```bash
$ posit-param
$0 = /home/me/bin/posit-param
$1 =
$2 =
$3 =
$4 =
$5 =
$6 =
$7 =
$8 =
$9 =
```

即使不带命令行参数, 位置参数 $0 总会包含命令行中出现的第一个单词, 也就是已执行程序的路径名.  当带参
数执行脚本时, 我们看看输出结果:

$ posit-param a b c d
$0 = /home/me/bin/posit-param
$1 = a
$2 = b
$3 = c
$4 = d
$5 =
$6 =
$7 =
$8 =
$9 =
注意:  实际上通过参数展开方式你可以访问的参数个数多于9个. 只要指定一个大于9的数字, 用花括号把该数字
括起来就可以.  例如 ${10},  ${55},  ${211}, 等等.
确定参数个数
另外 shell 还提供了一个名为 $#, 可以得到命令行参数个数的变量:

```bash
#!/bin/bash
# posit-param: script to view command line parameters
echo "
Number of arguments: $#
\$0 = $0
\$1 = $1
\$2 = $2
\$3 = $3
\$4 = $4
\$5 = $5
\$6 = $6
\$7 = $7
\$8 = $8
\$9 = $9
"
```

结果是:

```bash
$ posit-param a b c d
Number of arguments: 4
$0 = /home/me/bin/posit-param
$1 = a
$2 = b
$3 = c
$4 = d
$5 =
$6
$7
$8
$9
=
=
=
=
```

shift - 访问多个参数的利器

但是如果我们给一个程序添加大量的命令行参数, 会怎么样呢?  正如下面的例子:
$ posit-param *
Number of arguments: 82
$0 = /home/me/bin/posit-param
$1 = addresses.ldif
$2 = bin
$3 = bookmarks.html
$4 = debian-500-i386-netinst.iso
$5 = debian-500-i386-netinst.jigdo
$6 = debian-500-i386-netinst.template
$7 = debian-cd_info.tar.gz
$8 = Desktop
$9 = dirlist-bin.txt
在这个例子运行的环境下, 通配符 * 展开成82个参数. 我们如何处理那么多的参数?  为此, shell 提供了一种方
法, 尽管笨拙, 但可以解决这个问题. 执行一次 shift 命令,  就会导致所有的位置参数 "向下移动一个位置".
事实上, 用 shift 命令也可以 处理只有一个参数的情况(除了其值永远不会改变的变量 $0):

```bash
#!/bin/bash
# posit-param2: script to display all arguments
count=1
while [[ $# -gt 0 ]]; do
echo "Argument $count = $1"
count=$((count + 1))
shift
done
```

每次 shift 命令执行的时候, 变量 $2 的值会移动到变量 $1 中, 变量 $3 的值会移动到变量 $2 中, 依次类推.
变量 $# 的值也会相应的减1.
在该 posit-param2 程序中, 我们编写了一个计算剩余参数数量, 只要参数个数不为零就会继续执行的 while 循
环.  我们显示当前的位置参数, 每次循环迭代变量 count 的值都会加1, 用来计数处理的参数数量,  最后, 执行
shift 命令加载 $1, 其值为下一个位置参数的值. 这里是程序运行后的输出结果:

[me@linuxbox
Argument 1 =
Argument 2 =
Argument 3 =
Argument 4 =
~]$ posit-param2 a b c d
a
b
c
d
简单应用
即使没有 shift 命令, 也可以用位置参数编写一个有用的应用. 举例说明, 这里是一个简单的输出文件信息的程
序:

```bash
#!/bin/bash
# file_info: simple file information program
PROGNAME=$(basename $0)
if [[ -e $1 ]]; then
echo -e "\nFile Type:"
file $1
echo -e "\nFile Status:"
stat $1
else
echo "$PROGNAME: usage: $PROGNAME file" >&2
exit 1
fi
```

这个程序显示一个具体文件的文件类型(由 file 命令确定)和文件状态(来自 stat 命令). 该程序一个有意思
的特点是 PROGNAME 变量. 它的值就是 basename $0 命令的执行结果. 这个 basename 命令清除 一个路径
名的开头部分, 只留下一个文件的基本名称. 在我们的程序中, basename 命令清除了包含在 $0 位置参数 中的
路径名的开头部分, $0 中包含着我们示例程序的完整路径名. 当构建提示信息正如程序结尾的使用信息的时候,
basename $0 的执行结果就很有用处. 按照这种方式编码, 可以重命名该脚本, 且程序信息会自动调整为 包含
相应的程序名称.
Shell 函数中使用位置参数
正如位置参数被用来给 shell 脚本传递参数一样, 它们也能够被用来给 shell 函数传递参数. 为了说明这一点,
我们将把 file_info 脚本转变成一个 shell 函数:

```bash
file_info () {
# file_info: function to display file information
if [[ -e $1 ]]; then
echo -e "\nFile Type:"
file $1
echo -e "\nFile Status:"

stat $1
else
echo "$FUNCNAME: usage: $FUNCNAME file" >&2
return 1
fi
}
```

现在, 如果一个包含 shell 函数 file_info 的脚本调用该函数, 且带有一个文件名参数, 那这个参数会传递给
file_info 函数.
通过此功能, 我们可以写出许多有用的 shell 函数, 这些函数不仅能在脚本中使用, 也可以用在 .bashrc 文件
中.
注意那个 PROGNAME 变量已经改成 shell 变量 FUNCNAME 了. shell 会自动更新 FUNCNAME 变量, 以便
跟踪当前执行的 shell 函数. 注意位置参数 $0 总是包含命令行中第一项的完整路径名(例如, 该程序的名字),
但不会包含这个我们可能期望的 shell 函数的名字.
处理集体位置参数
有时候把所有的位置参数作为一个集体来管理是很有用的. 例如, 我们可能想为另一个程序编写一个 "包裹程
序".  这意味着我们会创建一个脚本或 shell 函数, 来简化另一个程序的执行. 包裹程序提供了一个神秘的命令
行选项 列表, 然后把这个参数列表传递给下一级的程序.
为此 shell 提供了两种特殊的参数. 他们二者都能扩展成完整的位置参数列表, 但以相当微妙的方式略有不同.
它们是:
表 32-1: * 和 @ 特殊参数
| 参数 | 描述 |
| $* | 展开成一个从1开始的位置参数列表. 当它被用双引号引起来的时候, 展开成一个由双引号引起来 的字符
串, 包含了所有的位置参数, 每个位置参数由 shell 变量 IFS 的第一个字符(默认为一个空格)分隔开.  |
| $@ | 展开成一个从1开始的位置参数列表. 当它被用双引号引起来的时候,  它把每一个位置参数展开成一个由
双引号引起来的分开的字符串.  |
下面这个脚本用程序中展示了这些特殊参数:

```bash
#!/bin/bash
# posit-params3 : script to demonstrate $* and $@
print_params () {
echo "\$1 = $1"
echo "\$2 = $2"
echo "\$3 = $3"
echo "\$4 = $4"
}
pass_params () {
echo -e "\n" '$* :';
print_params
$*

echo -e "\n" '"$*" :';
echo -e "\n" '$@ :';
echo -e "\n" '"$@" :';
print_params
print_params
print_params
"$*"
$@
"$@"
}
pass_params "word" "words with spaces"
在这个相当复杂的程序中, 我们创建了两个参数:  "word" 和 "words with spaces", 然后把它们 传递给
pass_params 函数. 这个函数, 依次, 再把两个参数传递给 print_params 函数,  使用了特殊参数 $* 和 $@ 提
供的四种可用方法. 脚本运行后, 揭示了这两个特殊参数存在的差异:
$ posit-param3
$* :
$1 = word
$2 = words
$3 = with
$4 = spaces
"$*" :
$1 = word words with spaces
$2 =
$3 =
$4 =
$@ :
$1 = word
$2 = words
$3 = with
$4 = spaces
"$@" :
$1 = word
$2 = words with spaces
$3 =
$4 =
通过我们的参数, $* 和 $@ 两个都产生了一个有四个词的结果:
word words with spaces
"$*" produces a one word result:
"word words with spaces"
"$@" produces a two word result:
"word" "words with spaces"
```

这个结果符合我们实际的期望. 我们从中得到的教训是尽管 shell 提供了四种不同的得到位置参数列表的方法,
但到目前为止,  "$@" 在大多数情况下是最有用的方法, 因为它保留了每一个位置参数的完整性.

一个更复杂的应用
经过长时间的间断, 我们将恢复程序 sys_info_page 的工作. 我们下一步要给程序添加如下几个命令行选项:
输出文件.  我们将添加一个选项, 以便指定一个文件名, 来包含程序的输出结果.  选项格式要么是 -f file,
要么是 --file file
交互模式. 这个选项将提示用户输入一个输出文件名, 然后判断是否指定的文件已经存在了. 如果文件存
在,  在覆盖这个存在的文件之前会提示用户. 这个选项可以通过 -i 或者 --interactive 来指定.
帮助. 指定 -h 选项 或者是 --help 选项, 可导致程序输出提示性的使用信息.
这里是处理命令行选项所需的代码:

```bash
usage () {
echo "$PROGNAME: usage: $PROGNAME [-f file | -i]"
return
}
# process command line options
interactive=
filename=
while [[ -n $1 ]]; do
case $1 in
-f | --file)
shift
filename=$1
;;
-i | --interactive)
interactive=1
;;
-h | --help)
usage
exit
;;
*)
usage >&2
exit 1
;;
esac
shift
done
```

首先, 我们添加了一个叫做 usage 的 shell 函数, 以便显示帮助信息, 当启用帮助选项或敲写了一个未知选项的
时候.
下一步, 我们开始处理循环. 当位置参数 $1 不为空的时候, 这个循环会持续运行. 在循环的底部, 有一个 shift
命令,  用来提升位置参数, 以便确保该循环最终会终止. 在循环体内, 我们使用了一个 case 语句来检查当前位
置参数的值,  看看它是否匹配某个支持的选项. 若找到了匹配项, 就会执行与之对应的代码. 若没有, 就会打印
出程序使用信息,  该脚本终止且执行错误.
处理 -f 参数的方式很有意思. 当监测到 -f 参数的时候, 会执行一次 shift 命令, 从而提升位置参数 $1 为 伴随着

-f 选项的 filename 参数.
我们下一步添加代码来实现交互模式:

```bash
# interactive mode
if [[ -n $interactive ]]; then
while true; do
read -p "Enter name of output file: " filename
if [[ -e $filename ]]; then
read -p "'$filename' exists. Overwrite? [y/n/q] > "
case $REPLY in
Y|y)
break
;;
Q|q)
echo "Program terminated."
exit
;;
*)
continue
;;
esac
elif [[ -z $filename ]]; then
continue
else
break
fi
done
fi
```

若 interactive 变量不为空, 就会启动一个无休止的循环, 该循环包含文件名提示和随后存在的文件处理代码.
如果所需要的输出文件已经存在, 则提示用户覆盖, 选择另一个文件名, 或者退出程序. 如果用户选择覆盖一个
已经存在的文件, 则会执行 break 命令终止循环. 注意 case 语句是怎样只检测用户选择了覆盖还是退出选项.
其它任何选择都会导致循环继续并提示用户再次选择.
为了实现这个输出文件名的功能, 首先我们必须把现有的这个写页面(page-writing)的代码转变成一个 shell
函数,  一会儿就会明白这样做的原因:

```bash
write_html_page () {
cat <<- _EOF_
<HTML>
<HEAD>
<TITLE>$TITLE</TITLE>
</HEAD>
<BODY>
<H1>$TITLE</H1>
<P>$TIMESTAMP</P>
$(report_uptime)
$(report_disk_space)

$(report_home_space)
</BODY>
</HTML>
_EOF_
return
}
# output html page
if [[ -n $filename ]]; then
if touch $filename && [[ -f $filename ]]; then
write_html_page > $filename
else
echo "$PROGNAME: Cannot write file '$filename'" >&2
exit 1
fi
else
write_html_page
fi
```

解决 -f 选项逻辑的代码出现在以上程序片段的末尾. 在这段代码中, 我们测试一个文件名是否存在, 若文件名存
在,  则执行另一个测试看看该文件是不是可写文件. 为此, 会运行 touch 命令, 紧随其后执行一个测试, 来决
定 touch 命令 创建的文件是否是个普通文件. 这两个测试考虑到了输入是无效路径名(touch 命令执行失
败), 和一个普通文件已经存在的情况.
正如我们所看到的, 程序调用 write_html_page 函数来生成实际的网页. 函数输出要么直接定向到 标准输出
(若 filename 变量为空的话)要么重定向到具体的文件中.
总结
伴随着位置参数的加入, 现在我们能编写相当具有功能性的脚本. 例如, 重复性的任务, 位置参数使得编写 非常
有用的, 可以放置在一个用户的 .bashrc 文件中的 shell 函数成为可能.
我们的 sys_info_page 程序日渐精进. 这里是一个完整的程序清单, 最新的更改用高亮显示:

```bash
#!/bin/bash
# sys_info_page: program to output a system information page
PROGNAME=$(basename $0)
TITLE="System Information Report For $HOSTNAME"
CURRENT_TIME=$(date +"%x %r %Z")
TIMESTAMP="Generated $CURRENT_TIME, by $USER"
report_uptime() {
    cat <<-_EOF_
<H2>System Uptime</H2>
<PRE>$(uptime)</PRE>
_EOF_
    return
}
report_disk_space() {
    cat <<-_EOF_
<H2>Disk Space Utilization</H2>
<PRE>$(df -h)</PRE>
_EOF_
    return
}
report_home_space() {
    if [[ $(id -u) -eq 0 ]]; then
        cat <<-_EOF_
<H2>Home Space Utilization (All Users)</H2>
<PRE>$(du -sh /home/*)</PRE>
_EOF_
    else
        cat <<-_EOF_
<H2>Home Space Utilization ($USER)</H2>
<PRE>$(du -sh $HOME)</PRE>
_EOF_
    fi
    return
}
usage() {
    echo "$PROGNAME: usage: $PROGNAME [-f file | -i]"
    return
}
write_html_page() {
    cat <<-_EOF_
<HTML>
<HEAD>
<TITLE>$TITLE</TITLE>
</HEAD>
<BODY>
<H1>$TITLE</H1>
<P>$TIMESTAMP</P>
$(report_uptime)
$(report_disk_space)
$(report_home_space)
</BODY>
</HTML>
_EOF_
    return
}
# process command line options
interactive=
filename=
while [[ -n $1 ]]; do
    case $1 in
    -f | --file)
        shift
        filename=$1
        ;;
    -i | --interactive)
        interactive=1
        ;;
    -h | --help)
        usage
        exit
        ;;
    *)
        usage >&2
        exit 1
        ;;
    esac
    shift
done
# interactive mode
if [[ -n $interactive ]]; then
    while true; do
        read -p "Enter name of output file: " filename
        if [[ -e $filename ]]; then
            read -p "'$filename' exists. Overwrite? [y/n/q] > "
            case $REPLY in
            Y | y)
                break
                ;;
            Q | q)
                echo "Program terminated."
                exit
                ;;
            *)
                continue
                ;;
            esac
        fi
    done
fi
# output html page
if [[ -n $filename ]]; then
    if touch $filename && [[ -f $filename ]]; then
        write_html_page >$filename
    else
        echo "$PROGNAME: Cannot write file '$filename'" >&2
        exit 1
    fi
else
    write_html_page
fi
```

我们还没有完成. 仍然还有许多事情我们可以做, 可以改进.

## 拓展阅读

Bash Hackers Wiki 上有一篇不错的关于位置参数的文章:
http://wiki.bash-hackers.org/scripting/posparams

Bash 的参考手册有一篇关于特殊参数的文章, 包括 $* 和 $@:
http://www.gnu.org/software/bash/manual/bashref.html#Special-Parameters
除了本章讨论的技术之外, bash 还包含一个叫做 getopts 的内部命令, 此命令也可以用来处理命令行参
数.  bash 参考页面的 SHELL BUILTIN COMMANDS 一节介绍了这个命令, Bash Hackers Wiki 上也有对
它的描述:
http://wiki.bash-hackers.org/howto/getopts_tutorial

## 第三十四章: 流程控制 for循环

在这关于流程控制的最后一章中, 我们将看看另一种 shell 循环构造. for 循环不同于 while 和 until 循环, 因为
在循环中, 它提供了一种处理序列的方式. 这证明在编程时非常有用. 因此在 bash 脚本中, for 循环是非常流行
的构造.
实现一个 for 循环, 很自然的, 要用 for 命令. 在现代版的 bash 中, 有两种可用的 for 循环格式.
for: 传统 shell 格式
原来的 for 命令语法是:
for variable [in words]; do
commands
done
这里的 variable 是一个变量的名字, 这个变量在循环执行期间会增加, words 是一个可选的条目列表,  其值会
按顺序赋值给 variable, commands 是在每次循环迭代中要执行的命令.
在命令行中 for 命令是很有用的. 我们可以很容易的说明它是如何工作的:
$ for i in A B C D; do echo $i; done
A
B
C
D
在这个例子中, for 循环有一个四个单词的列表: "A", "B", "C", 和 "D". 由于这四个单词的列表,
for 循环会执行四次.  每次循环执行的时候, 就会有一个单词赋值给变量 i. 在循环体内, 我们有一个 echo 命令
会显示 i 变量的值, 来演示赋值结果.  正如 while 和 until 循环, done 关键字会关闭循环.
for 命令真正强大的功能是我们可以通过许多有趣的方式创建 words 列表. 例如, 通过花括号展开:
$ for i in {A..D}; do echo $i; done
A
B
C
D
或者路径名展开:

$ for i in distros*.txt; do echo $i; done
distros-by-date.txt
distros-dates.txt
distros-key-names.txt
distros-key-vernums.txt
distros-names.txt
distros.txt
distros-vernums.txt
distros-versions.txt
或者命令替换:

```bash
#!/bin/bash
# longest-word : find longest string in a file
while [[ -n $1 ]]; do
if [[ -r $1 ]]; then
max_word=
max_len=0
for i in $(strings $1); do
len=$(echo $i | wc -c)
if (( len > max_len )); then
max_len=$len
max_word=$i
fi
done
echo "$1: '$max_word' ($max_len characters)"
fi
shift
done
```

在这个示例中, 我们要在一个文件中查找最长的字符串. 当在命令行中给出一个或多个文件名的时候,  该程序会
使用 strings 程序(其包含在 GNU binutils 包中), 为每一个文件产生一个可读的文本格式的 "words" 列
表.  然后这个 for 循环依次处理每个单词, 判断当前这个单词是否为目前为止找到的最长的一个. 当循环结束的
时候, 显示出最长的单词.
如果省略掉 for 命令的可选项 words 部分, for 命令会默认处理位置参数.  我们将修改 longest-word 脚本, 来
使用这种方式:

```bash
#!/bin/bash
# longest-word2 : find longest string in a file
for i; do
if [[ -r $i ]]; then
max_word=
max_len=0

for j in $(strings $i); do
len=$(echo $j | wc -c)
if (( len > max_len )); then
max_len=$len
max_word=$j
fi
done
echo "$i: '$max_word' ($max_len characters)"
fi
done
```

正如我们所看到的, 我们已经更改了最外围的循环, 用 for 循环来代替 while 循环. 通过省略 for 命令的 words
列表,  用位置参数替而代之. 在循环体内, 之前的变量 i 已经改为变量 j. 同时 shift 命令也被淘汰掉了.
为什么是 i?
你可能已经注意到上面所列举的 for 循环的实例都选择 i 作为变量. 为什么呢?  实际上没有具体原因, 除了
传统习惯.  for 循环使用的变量可以是任意有效的变量, 但是 i 是最常用的一个, 其次是 j 和 k.
这一传统的基础源于 Fortran 编程语言. 在 Fortran 语言中, 以字母 I, J, K, L 和 M 开头的未声明变量的
类型 自动设为整形, 而以其它字母开头的变量则为实数类型(带有小数的数字). 这种行为导致程序员使用
变量 I, J, 和 K 作为循环变量,  因为当需要一个临时变量(正如循环变量)的时候, 使用它们工作量比较
少. 这也引出了如下基于 fortran 的俏皮话:
"神是真实的, 除非是声明的整数. "
for: C 语言格式
最新版本的 bash 已经添加了第二种格式的 for 命令语法, 该语法相似于 C 语言中的 for 语法格式.  其它许多编
程语言也支持这种格式:
for (( expression1; expression2; expression3 )); do
commands
done
这里的 expression1, expression2, 和 expression3 都是算术表达式, commands 是每次循环迭代时要执行的
命令.  在行为方面, 这相当于以下构造形式:
(( expression1 ))
while (( expression2 )); do
commands
(( expression3 ))
done

expression1 用来初始化循环条件, expression2 用来决定循环结束的时间, 还有在每次循环迭代的末尾会执行
expression3.
这里是一个典型应用:

```bash
#!/bin/bash
# simple_counter : demo of C style for command
for (( i=0; i<5; i=i+1 )); do
echo $i
done
```

脚本执行之后, 产生如下输出:
$ simple_counter
0
1
2
3
4

在这个示例中, expression1 初始化变量 i 的值为0, expression2 允许循环继续执行只要变量 i 的值小于5,  还
有每次循环迭代时, expression3 会把变量 i 的值加1.
C 语言格式的 for 循环对于需要一个数字序列的情况是很有用处的. 我们将在接下来的两章中看到几个这样的应
用实例.
总结
学习了 for 命令的知识, 现在我们将对我们的 sys_info_page 脚本做最后的改进.  目前, 这个
report_home_space 函数看起来像这样:

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
下一步, 我们将重写它, 以便提供每个用户家目录的更详尽信息, 并且包含用户家目录中文件和目录的总个数:
report_home_space () {
local format="%8s%10s%10s\n"
local i dir_list total_files total_dirs total_size user_name
if [[ $(id -u) -eq 0 ]]; then
dir_list=/home/*
user_name="All Users"
else
dir_list=$HOME
user_name=$USER
fi
echo "<H2>Home Space Utilization ($user_name)</H2>"
for i in $dir_list; do
total_files=$(find $i -type f | wc -l)
total_dirs=$(find $i -type d | wc -l)
total_size=$(du -sh $i | cut -f 1)
echo "<H3>$i</H3>"
echo "<PRE>"
printf "$format" "Dirs" "Files" "Size"
printf "$format" "----" "-----" "----"
printf "$format" $total_dirs $total_files $total_size
echo "</PRE>"
done
return
}
```

这次重写应用了目前为止我们学过的许多知识. 我们仍然测试超级用户(superuser), 但是我们在 if 语句块内
设置了一些随后会在 for 循环中用到的变量, 来取代在 if 语句块内执行完备的动作集合. 我们添加了给 函数添加
了几个本地变量, 并且使用 printf 来格式化输出.

### 拓展阅读

`<高级 Bash 脚本指南>` 有一章关于循环的内容, 其中列举了各种各样的 for 循环实例:
http://tldp.org/LDP/abs/html/loops1.html

`<Bash 参考手册>` 描述了循环复合命令, 包括了 for 循环:
http://www.gnu.org/software/bash/manual/bashref.html#Looping-Constructs

## 第三十五章: 字符串和数字

所有的计算机程序都是用来和数据打交道的. 在过去的章节中, 我们专注于处理文件级别的数据.  然而, 许多程
序问题需要使用更小的数据单位来解决, 比方说字符串和数字.
在这一章中, 我们将查看几个用来操作字符串和数字的 shell 功能. shell 提供了各种执行字符串操作的参数展开
功能.  除了算术展开(在第七章中接触过), 还有一个常见的命令行程序叫做 bc, 能执行更高级别的数学运
算.
参数展开
尽管参数展开在第七章中出现过, 但我们并没有详尽地介绍它, 因为大多数的参数展开会用在脚本中, 而不是命
令行中.  我们已经使用了一些形式的参数展开; 例如, shell 变量. shell 提供了更多方式.
基本参数
最简单的参数展开形式反映在平常使用的变量上.
例如:
$a
当 $a 展开后, 会变成变量 a 所包含的值. 简单参数也可能用花括号引起来:
${a}
虽然这对展开没有影响, 但若该变量 a 与其它的文本相邻, 可能会把 shell 搞糊涂了. 在这个例子中, 我们试图
创建一个文件名, 通过把字符串 "_file" 附加到变量 a 的值的后面.
$ a="foo"
$ echo "$a_file"
如果我们执行这个序列, 没有任何输出结果, 因为 shell 会试着展开一个称为 a_file 的变量, 而不是 a. 通过 添
加花括号可以解决这个问题:
$ echo "${a}_file"
foo_file
我们已经知道通过把数字包裹在花括号中, 可以访问大于9的位置参数. 例如, 访问第十一个位置参数, 我们可
以这样做:
${11}
管理空变量的展开

几种用来处理不存在和空变量的参数展开形式. 这些展开形式对于解决丢失的位置参数和给参数指定默认值的情
况很方便.
${parameter:-word}
若 parameter 没有设置(例如, 不存在)或者为空, 展开结果是 word 的值. 若 parameter 不为空, 则展开结
果是 parameter 的值.
$
$
if unset
substitute value
$
$
$
bar
$
bar
foo=
echo ${foo:-"substitute value if unset"}
echo $foo
foo=bar
echo ${foo:-"substitute value if unset"}
echo $foo
${parameter:=word}
若 parameter 没有设置或为空, 展开结果是 word 的值. 另外, word 的值会赋值给 parameter.  若
parameter 不为空, 展开结果是 parameter 的值.
$
$
default value if
$
default value if
$
$
bar
$
bar
foo=
echo ${foo:="default value if unset"}
unset
echo $foo
unset
foo=bar
echo ${foo:="default value if unset"}
echo $foo
注意:  位置参数或其它的特殊参数不能以这种方式赋值.
${parameter:?word}
若 parameter 没有设置或为空, 这种展开导致脚本带有错误退出, 并且 word 的内容会发送到标准错误. 若
parameter 不为空,  展开结果是 parameter 的值.
$ foo=
$ echo ${foo:?"parameter is empty"}

bash: foo: parameter is empty
$ echo $?
1
$ foo=bar
$ echo ${foo:?"parameter is empty"}
bar
$ echo $?
0
${parameter:+word}
若 parameter 没有设置或为空, 展开结果为空. 若 parameter 不为空,  展开结果是 word 的值会替换掉
parameter 的值; 然而, parameter 的值不会改变.
$ foo=
$ echo ${foo:+"substitute value if set"}
$ foo=bar
$ echo ${foo:+"substitute value if set"}
substitute value if set
返回变量名的参数展开
shell 具有返回变量名的能力. 这会用在一些相当独特的情况下.
${!prefix*}
${!prefix@}
这种展开会返回以 prefix 开头的已有变量名. 根据 bash 文档, 这两种展开形式的执行结果相同.  这里, 我们列
出了所有以 BASH 开头的环境变量名:
$ echo ${!BASH*}
BASH BASH_ARGC BASH_ARGV BASH_COMMAND BASH_COMPLETION
BASH_COMPLETION_DIR BASH_LINENO BASH_SOURCE BASH_SUBSHELL
BASH_VERSINFO BASH_VERSION
字符串展开
有大量的展开形式可用于操作字符串. 其中许多展开形式尤其适用于路径名的展开.
${#parameter}
展开成由 parameter 所包含的字符串的长度. 通常, parameter 是一个字符串; 然而, 如果 parameter 是 @

或者是 * 的话,  则展开结果是位置参数的个数.
$ foo="This string is long."
$ echo "'$foo' is ${#foo} characters long."
'This string is long.' is 20 characters long.
${parameter:offset}
${parameter:offset:length}
这些展开用来从 parameter 所包含的字符串中提取一部分字符. 提取的字符始于 第 offset 个字符(从字符串开
头算起)直到字符串的末尾, 除非指定提取的长度.
$ foo="This string is long."
$ echo ${foo:5}
string is long.
$ echo ${foo:5:6}
string
若 offset 的值为负数, 则认为 offset 值是从字符串的末尾开始算起, 而不是从开头. 注意负数前面必须有一个
空格,  为防止与 ${parameter:-word} 展开形式混淆. length, 若出现, 则必须不能小于零.
如果 parameter 是 @, 展开结果是 length 个位置参数, 从第 offset 个位置参数开始.
$ foo="This string is long."
$ echo ${foo: -5}
long.
$ echo ${foo: -5:2}
lo
${parameter#pattern}
${parameter##pattern}
这些展开会从 paramter 所包含的字符串中清除开头一部分文本, 这些字符要匹配定义的 patten. pattern 是 通
配符模式, 就如那些用在路径名展开中的模式. 这两种形式的差异之处是该 # 形式清除最短的匹配结果,  而该## 模式清除最长的匹配结果.

$ foo=file.txt.zip
$ echo ${foo#*.}
txt.zip
$ echo ${foo##*.}
zip

${parameter%pattern}
${parameter%%pattern}
这些展开和上面的 # 和 ## 展开一样, 除了它们清除的文本从 parameter 所包含字符串的末尾开始, 而不是开
头.
$ foo=file.txt.zip
$ echo ${foo%.*}
file.txt
$ echo ${foo%%.*}
file
${parameter/pattern/string}
${parameter//pattern/string}
${parameter/#pattern/string}
${parameter/%pattern/string}
这种形式的展开对 parameter 的内容执行查找和替换操作. 如果找到了匹配通配符 pattern 的文本,  则用
string 的内容替换它. 在正常形式下, 只有第一个匹配项会被替换掉. 在该 // 形式下, 所有的匹配项都会被替换
掉.  该 /# 要求匹配项出现在字符串的开头, 而 /% 要求匹配项出现在字符串的末尾. /string 可能会省略掉, 这
样会 导致删除匹配的文本.
[me@linuxbox~]$ foo=JPG.JPG
$ echo ${foo/JPG/jpg}
jpg.JPG
[me@linuxbox~]$ echo ${foo//JPG/jpg}
jpg.jpg
[me@linuxbox~]$ echo ${foo/#JPG/jpg}
jpg.JPG
[me@linuxbox~]$ echo ${foo/%JPG/jpg}
JPG.jpg
知道参数展开是件很好的事情. 字符串操作展开可以用来替换其它常见命令比方说 sed 和 cut.  通过减少使用外
部程序, 展开提高了脚本的效率. 举例说明, 我们将修改在之前章节中讨论的 longest-word 程序,  用参数展开
${#j} 取代命令 $(echo $j | wc -c) 及其 subshell , 像这样:

```bash
#!/bin/bash
# longest-word3 : find longest string in a file
for i; do

if [[ -r $i ]]; then
max_word=
max_len=
for j in $(strings $i); do
len=${#j}
if (( len > max_len )); then
max_len=$len
max_word=$j
fi
done
echo "$i: '$max_word' ($max_len characters)"
fi
shift
done
```

下一步, 我们将使用 time 命令来比较这两个脚本版本的效率:
$ time longest-word2 dirlist-usr-bin.txt
dirlist-usr-bin.txt: 'scrollkeeper-get-extended-content-list' (38
characters)
real 0m3.618s
user 0m1.544s
sys 0m1.768s
$ time longest-word3 dirlist-usr-bin.txt
dirlist-usr-bin.txt: 'scrollkeeper-get-extended-content-list' (38
characters)
real 0m0.060s
user 0m0.056s
sys 0m0.008s
原来的脚本扫描整个文本文件需耗时3.168秒, 而该新版本, 使用参数展开, 仅仅花费了0.06秒  --  一个非常巨
大的提高.
大小写转换
最新的 bash 版本已经支持字符串的大小写转换了. bash 有四个参数展开和 declare 命令的两个选项来支持大小
写转换.
那么大小写转换对什么有好处呢?  除了明显的审美价值, 它在编程领域还有一个重要的角色.  让我们考虑一个
数据库查询的案例. 假设一个用户已经敲写了一个字符串到数据输入框中,  而我们想要在一个数据库中查找这个
字符串. 该用户输入的字符串有可能全是大写字母或全是小写或是两者的结合.  我们当然不希望把每个可能的大
小写拼写排列填充到我们的数据库中. 那怎么办?
解决这个问题的常见方法是规范化用户输入. 也就是, 在我们试图查询数据库之前, 把用户的输入转换成标准

化.  我们能做到这一点, 通过把用户输入的字符全部转换成小写字母或大写字母, 并且确保数据库中的条目 按
同样的方式规范化.
这个 declare 命令可以用来把字符串规范成大写或小写字符. 使用 declare 命令, 我们能强制一个 变量总是包含
所需的格式, 无论如何赋值给它.

```bash
#!/bin/bash
# ul-declare: demonstrate case conversion via declare
declare -u upper
declare -l lower
if [[ $1 ]]; then
upper="$1"
lower="$1"
echo $upper
echo $lower
fi
```

在上面的脚本中, 我们使用 declare 命令来创建两个变量, upper 和 lower. 我们把第一个命令行参数的值(位
置参数1)赋给 每一个变量, 然后把变量值在屏幕上显示出来:
$ ul-declare aBc
ABC
abc
正如我们所看到的, 命令行参数("aBc")已经规范化了.
有四个参数展开, 可以执行大小写转换操作:
表 35-1: 大小写转换参数展开

格式 结果
${parameter,,} 把 parameter 的值全部展开成小写字
母.
${parameter,} 仅仅把 parameter 的第一个字符展开
成小写字母.
${parameter^^} 把 parameter 的值全部转换成大写字
母.
${parameter^} 仅仅把 parameter 的第一个字符转换
成大写字母(首字母大写).
这里是一个脚本, 演示了这些展开格式:

```bash
#!/bin/bash
# ul-param - demonstrate case conversion via parameter expansion
if [[ $1 ]]; then
echo ${1,,}
echo ${1,}
echo ${1^^}
echo ${1^}
fi
```

这里是脚本运行后的结果:
$ ul-param aBc
abc
aBc
ABC
ABc
再次, 我们处理了第一个命令行参数, 输出了由参数展开支持的四种变体. 尽管这个脚本使用了第一个位置参
数,  但参数可以是任意字符串, 变量, 或字符串表达式.
算术求值和展开
我们在第七章中已经接触过算术展开了. 它被用来对整数执行各种算术运算. 它的基本格式是:
$((expression))
这里的 expression 是一个有效的算术表达式.
这个与复合命令 (( )) 有关, 此命令用做算术求值(真测试), 我们在第27章中遇到过.
在之前的章节中, 我们看到过一些类型的表达式和运算符. 这里, 我们将看到一个更完整的列表.
数基
回到第9章, 我们看过八进制(以8为底)和十六进制(以16为底)的数字. 在算术表达式中, shell 支持任意进
制的整形常量.
表 35-2: 指定不同的数基

表示法 描述
number 默认情况下, 没有任何表示法的数字被
看做是十进制数(以10为底).

0number 在算术表达式中, 以零开头的数字被认
为是八进制数.
0xnumber 十六进制表示法
base#number number 以 base 为底

一些例子:
$ echo $((0xff))
255
$ echo $((2#11111111))
255
在上面的示例中, 我们打印出十六进制数 ff(最大的两位数)的值和最大的八位二进制数(以2为底).
一元运算符
有两个二元运算符, + 和 -, 它们被分别用来表示一个数字是正数还是负数. 例如, -5.
简单算术
下表中列出了普通算术运算符:
表 35-3: 算术运算符

运算符 描述
+ 加
- 减
* 乘
/ 整除
** 乘方
% 取模(余数)

其中大部分运算符是不言自明的, 但是整除和取模运算符需要进一步解释一下.
因为 shell 算术只操作整形, 所以除法运算的结果总是整数:
$ echo $(( 5 / 2 ))
2
这使得确定除法运算的余数更为重要:

$ echo $(( 5 % 2 ))
1
通过使用除法和取模运算符, 我们能够确定5除以2得数是2, 余数是1.
在循环中计算余数是很有用处的. 在循环执行期间, 它允许某一个操作在指定的间隔内执行. 在下面的例子中,
我们显示一行数字, 并高亮显示5的倍数:

```bash
#!/bin/bash
# modulo : demonstrate the modulo operator
for ((i = 0; i <= 20; i = i + 1)); do
remainder=$((i % 5))
if (( remainder == 0 )); then
printf "<%d> " $i
else
printf "%d " $i
fi
done
printf "\n"
```

当脚本执行后, 输出结果看起来像这样:
$ modulo
<0> 1 2 3 4 <5> 6 7 8 9 <10> 11 12 13 14 <15> 16 17 18 19 <20>
赋值运算符
尽管它的使用不是那么明显, 算术表达式可能执行赋值运算. 虽然在不同的上下文中, 我们已经执行了许多次赋
值运算.  每次我们给变量一个值, 我们就执行了一次赋值运算. 我们也能在算术表达式中执行赋值运算:
[me@linuxbox
[me@linuxbox
[me@linuxbox
It is true.
[me@linuxbox
5
~]$ foo=
~]$ echo $foo
~]$ if (( foo = 5 ));then echo "It is true."; fi
~]$ echo $foo
在上面的例子中, 首先我们给变量 foo 赋了一个空值, 然后验证 foo 的确为空. 下一步, 我们执行一个 if 复合
命令 (( foo = 5 )).  这个过程完成两件有意思的事情: 1)它把5赋值给变量 foo, 2)它计算测试条件为真, 因

为 foo 的值非零.
注意:  记住上面表达式中 = 符号的真正含义非常重要. 单个 = 运算符执行赋值运算. foo = 5 是说"使得 foo
等于5",  而 == 运算符计算等价性. foo == 5 是说"是否 foo 等于5? ". 这会让人感到非常迷惑, 因为 test
命令接受单个 = 运算符 来测试字符串等价性. 这也是使用更现代的 [[ ]] 和 (( )) 复合命令来代替 test 命令的另一
个原因.
除了 = 运算符, shell 也提供了其它一些表示法, 来执行一些非常有用的赋值运算:
表35-4: 赋值运算符

表示法 描述
parameter = value 简单赋值. 给 parameter 赋值.
parameter += value 加. 等价于 parameter = parameter
+ value.
parameter -= value 减. 等价于 parameter = parameter
– value.
parameter *= value 乘. 等价于 parameter = parameter *
value.
parameter /= value 整除. 等价于 parameter =
parameter / value.
parameter %= value 取模. 等价于 parameter =
parameter % value.
parameter++ 后缀自增变量. 等价于 parameter =
parameter + 1 (但, 要看下面的讨
论).
parameter-- 后缀自减变量. 等价于 parameter =
parameter - 1.
++parameter 前缀自增变量. 等价于 parameter =
parameter + 1.
--parameter 前缀自减变量. 等价于 parameter =
parameter - 1.

这些赋值运算符为许多常见算术任务提供了快捷方式. 特别关注一下自增(++)和自减(--)运算符, 它们会把
它们的参数值加1或减1.  这种风格的表示法取自C 编程语言并且被其它几种编程语言吸收, 包括 bash.
自增和自减运算符可能会出现在参数的前面或者后面. 然而它们都是把参数值加1或减1, 这两个位置有个微小的
差异.  若运算符放置在参数的前面, 参数值会在参数返回之前增加(或减少). 若放置在后面, 则运算会在参数
返回之后执行.  这相当奇怪, 但这是它预期的行为. 这里是个演示的例子:
$ foo=1

$ echo $((foo++))
1
$ echo $foo
2
如果我们把1赋值给变量 foo, 然后通过把自增运算符 ++ 放到参数名 foo 之后来增加它, foo 返回1.  然而,
如果我们第二次查看变量 foo 的值, 我们看到它的值增加了1. 若我们把 ++ 运算符放到参数 foo 之前,  我们得
到更期望的行为:
$ foo=1
$ echo $((++foo))
2
$ echo $foo
2
对于大多数 shell 应用来说, 前缀运算符最有用.
自增 ++ 和 自减 -- 运算符经常和循环操作结合使用. 我们将改进我们的 modulo 脚本, 让代码更紧凑些:

```bash
#!/bin/bash
# modulo2 : demonstrate the modulo operator
for ((i = 0; i <= 20; ++i )); do
if (((i % 5) == 0 )); then
printf "<%d> " $i
else
printf "%d " $i
fi
done
printf "\n"
```

位运算符
位运算符是一类以不寻常的方式操作数字的运算符. 这些运算符工作在位级别的数字. 它们被用在某类底层的任
务中,  经常涉及到设置或读取位标志.
表35-5: 位运算符

运算符 描述
~ 按位取反. 对一个数字所有位取反.
<< 位左移. 把一个数字的所有位向左移
动.
>> 位右移. 把一个数字的所有位向右移
>> 动.
& 位与. 对两个数字的所有位执行一个
AND 操作.
^ 位异或. 对两个数字的所有位执行一个
异或操作.

注意除了按位取反运算符之外, 其它所有位运算符都有相对应的赋值运算符(例如, <<=).
这里我们将演示产生2的幂列表的操作, 使用位左移运算符:

```bash
$ for ((i=0;i<8;++i)); do echo $((1<<i)); done
1
2
4
8
16
32
64
128
```

逻辑运算符
正如我们在第27章中所看到的, 复合命令 (( )) 支持各种各样的比较运算符. 还有一些可以用来计算逻辑运算.
这里是比较运算符的完整列表:

表35-6: 比较运算符

运算符 描述
<= 小于或相等
>= 大于或相等
< 小于
> 大于
== 相等
!= 不相等
&& 逻辑与
expr1?expr2:expr3 条件(三元)运算符. 若表达式 expr1
的计算结果为非零值(算术真), 则 执
行表达式 expr2, 否则执行表达式
expr3.

当表达式用于逻辑运算时, 表达式遵循算术逻辑规则; 也就是, 表达式的计算结果是零, 则认为假, 而非零表达
式认为真.  该 (( )) 复合命令把结果映射成 shell 正常的退出码:
$ if ((1)); then echo "true"; else echo "false"; fi
true
$ if ((0)); then echo "true"; else echo "false"; fi
false
最陌生的逻辑运算符就是这个三元运算符了. 这个运算符(仿照于 C 编程语言里的三元运算符)执行一个单独的
逻辑测试.  它用起来类似于 if/then/else 语句. 它操作三个算术表达式(字符串不会起作用), 并且若第一个表
达式为真(或非零),  则执行第二个表达式. 否则, 执行第三个表达式. 我们可以在命令行中实验一下:
[me@linuxbox~]$
[me@linuxbox~]$
[me@linuxbox~]$
1
[me@linuxbox~]$
[me@linuxbox~]$
0
a=0
((a<1?++a:--a))
echo $a
((a<1?++a:--a))
echo $a
这里我们看到一个实际使用的三元运算符. 这个例子实现了一个切换. 每次运算符执行的时候, 变量 a 的值从零
变为1, 或反之亦然.
请注意在表达式内执行赋值却并非易事.
当企图这样做时, bash 会声明一个错误:
$ a=0
$ ((a<1?a+=1:a-=1))
bash: ((: a<1?a+=1:a-=1: attempted assignment to non-variable (error token is "
-=1")
通过把赋值表达式用括号括起来, 可以解决这个错误:
$ ((a<1?(a+=1):(a-=1)))
下一步, 我们看一个使用算术运算符更完备的例子, 该示例产生一个简单的数字表格:

```bash
#!/bin/bash
# arith-loop: script to demonstrate arithmetic operators
finished=0

a=0
printf "a\ta**2\ta**3\n"
printf "=\t====\t====\n"
until ((finished)); do
b=$((a**2))
c=$((a**3))
printf "%d\t%d\t%d\n" $a $b $c
((a<10?++a:(finished=1)))
done
```

在这个脚本中, 我们基于变量 finished 的值实现了一个 until 循环. 首先, 把变量 finished 的值设为零(算术
假),  继续执行循环之道它的值变为非零. 在循环体内, 我们计算计数器 a 的平方和立方. 在循环末尾, 计算计
数器变量 a 的值.  若它小于10(最大迭代次数), 则 a 的值加1, 否则给变量 finished 赋值为1, 使得变量
finished 算术为真,  从而终止循环. 运行该脚本得到这样的结果:

```bash
$ arith-loop

```

bc - 一种高精度计算器语言
我们已经看到 shell 是可以处理所有类型的整形算术的, 但是如果我们需要执行更高级的数学运算或仅使用浮点
数, 该怎么办?  答案是, 我们不能这样做. 至少不能直接用 shell 完成此类运算. 为此, 我们需要使用外部程
序.  有几种途径可供我们采用. 嵌入的 Perl 或者 AWK 程序是一种可能的方案, 但是不幸的是, 超出了本书的
内容大纲.  另一种方式就是使用一种专业的计算器程序. 这样一个程序叫做 bc, 在大多数 Linux 系统中都可以
找到.
该 bc 程序读取一个用它自己的类似于 C 语言的语法编写的脚本文件. 一个 bc 脚本可能是一个分离的文件或者
是读取 标准输入. bc 语言支持相当少的功能, 包括变量, 循环和程序员定义的函数. 这里我们不会讨论整个 bc
语言,  仅仅体验一下. 查看 bc 的手册页, 其文档整理非常好.
让我们从一个简单的例子开始. 我们将编写一个 bc 脚本来执行2加2运算:

/* A very simple bc script */
2 + 2
脚本的第一行是一行注释. bc 使用和 C 编程语言一样的注释语法. 注释, 可能会跨越多行, 开始于
/*
结束
于 */ .
使用 bc
如果我们把上面的 bc 脚本保存为 foo.bc, 然后我们就能这样运行它:
$ bc foo.bc
bc 1.06.94
Copyright 1991-1994, 1997, 1998, 2000, 2004, 2006 Free Software
Foundation, Inc.
This is free software with ABSOLUTELY NO WARRANTY.
For details type `warranty'.
4
如果我们仔细观察, 我们看到算术结果在最底部, 版权信息之后. 可以通过 -q(quiet)选项禁止这些版权信
息.  bc 也能够交互使用:
$ bc -q
2 + 2
4
quit
当使用 bc 交互模式时, 我们简单地输入我们希望执行的运算, 结果就立即显示出来. bc 的 quit 命令结束交互
会话.
也可能通过标准输入把一个脚本传递给 bc 程序:
$ bc < foo.bc
4
这种接受标准输入的能力, 意味着我们可以使用 here 文档, here字符串, 和管道来传递脚本. 这里是一个使用
here 字符串的例子:
$ bc <<< "2+2"
4

一个脚本实例
作为一个真实世界的例子, 我们将构建一个脚本, 用于计算每月的还贷金额. 在下面的脚本中,  我们使用了
here 文档把一个脚本传递给 bc:

```bash
#!/bin/bash
# loan-calc : script to calculate monthly loan payments
PROGNAME=$(basename $0)
usage () {
cat <<- EOF
Usage: $PROGNAME PRINCIPAL INTEREST MONTHS
Where:
PRINCIPAL is the amount of the loan.
INTEREST is the APR as a number (7% = 0.07).
MONTHS is the length of the loan's term.
EOF
}
if (($# != 3)); then
usage
exit 1
fi
principal=$1
interest=$2
months=$3
bc <<- EOF
scale = 10
i = $interest / 12
p = $principal
n = $months
a = p * ((i * ((1 + i) ^ n)) / (((1 + i) ^ n) - 1))
print a, "\n"
EOF
```

当脚本执行后, 输出结果像这样:
$ loan-calc 135000 0.0775 180
475
1270.7222490000
若贷款 135,000 美金, 年利率为 7.75%, 借贷180个月(15年), 这个例子计算出每月需要还贷的金额.  注意
这个答案的精确度. 这是由脚本中变量 scale 的值决定的. bc 的手册页提供了对 bc 脚本语言的详尽描述.  虽然

bc 的数学符号与 shell 的略有差异(bc 与 C 更相近), 但是基于目前我们所学的内容,  大多数符号是我们相当
熟悉的.
总结
在这一章中, 我们学习了很多小东西, 在脚本中这些小零碎可以完成"真正的工作". 随着我们编写脚本经验的
增加,  能够有效地操作字符串和数字的能力将具有极为重要的价值. 我们的 loan-calc 脚本表明,  甚至可以创建
简单的脚本来完成一些真正有用的事情.
额外加分
虽然该 loan-calc 脚本的基本功能已经很到位了, 但脚本还远远不够完善. 为了额外加分, 试着 给脚本 loan-
calc 添加以下功能:
完整的命令行参数验证
用一个命令行选项来实现"交互"模式, 提示用户输入本金, 利率和贷款期限
输出格式美化

拓展阅读

<Bash Hackers Wiki>对参数展开有一个很好的论述:
http://wiki.bash-hackers.org/syntax/pe
<Bash 参考手册>也介绍了这个:
http://www.gnu.org/software/bash/manual/bashref.html#Shell-Parameter-Expansion
Wikipedia 上面有一篇很好的文章描述了位运算:
http://en.wikipedia.org/wiki/Bit_operation
和一篇关于三元运算的文章:
http://en.wikipedia.org/wiki/Ternary_operation
还有一个对计算还贷金额公式的描述, 我们的 loan-calc 脚本中用到了这个公式:
http://en.wikipedia.org/wiki/Amortization_calculator
