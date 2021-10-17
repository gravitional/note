# shell

## 第三十六章: 数组
在上一章中, 我们查看了 shell 怎样操作字符串和数字的. 目前我们所见到的数据类型在计算机科学圈里被 成为
标量变量; 也就是说, 只能包含一个值的变量.
在本章中, 我们将看看另一种数据结构叫做数组, 数组能存放多个值. 数组几乎是所有编程语言的一个特性.
shell 也支持它们, 尽管以一个相当有限的形式. 即便如此, 为解决编程问题, 它们是非常有用的.
什么是数组?
数组是一次能存放多个数据的变量. 数组的组织结构就像一张表. 我们拿电子表格举例. 一张电子表格就像是一
个 二维数组. 它既有行也有列, 并且电子表格中的一个单元格, 可以通过单元格所在的行和列的地址定位它的位
置.  数组行为也是如此. 数组有单元格, 被称为元素, 而且每个元素会包含数据.  使用一个称为索引或下标的
地址可以访问一个单独的数组元素.
大多数编程语言支持多维数组. 一个电子表格就是一个多维数组的例子, 它有两个维度, 宽度和高度.  许多语言
支持任意维度的数组, 虽然二维和三维数组可能是最常用的.
Bash 中的数组仅限制为单一维度. 我们可以把它们看作是只有一列的电子表格. 尽管有这种局限, 但是有许多
应用使用它们.  对数组的支持第一次出现在 bash 版本2中. 原来的 Unix shell 程序, sh, 根本就不支持数组.
创建一个数组
数组变量就像其它 bash 变量一样命名, 当被访问的时候, 它们会被自动地创建. 这里是一个例子:
[me@linuxbox ~]$ a[1]=foo
[me@linuxbox ~]$ echo ${a[1]}
foo
这里我们看到一个赋值并访问数组元素的例子. 通过第一个命令, 把数组 a 的元素1赋值为 "foo".  第二个命
令显示存储在元素1中的值. 在第二个命令中使用花括号是必需的,  以便防止 shell 试图对数组元素名执行路径
名展开操作.
也可以用 declare 命令创建一个数组:
[me@linuxbox ~]$ declare -a a
使用 -a 选项, declare 命令的这个例子创建了数组 a.
数组赋值

有两种方式可以给数组赋值. 单个值赋值使用以下语法:
name[subscript]=value
这里的 name 是数组的名字, subscript 是一个大于或等于零的整数(或算术表达式). 注意数组第一个元素的
下标是0,  而不是1. 数组元素的值可以是一个字符串或整数.
多个值赋值使用下面的语法:
name=(value1 value2 ...)
这里的 name 是数组的名字, value...  是要按照顺序赋给数组的值, 从元素0开始. 例如, 如果我们希望 把星期
几的英文简写赋值给数组 days, 我们可以这样做:
[me@linuxbox ~]$ days=(Sun Mon Tue Wed Thu Fri Sat)
还可以通过指定下标, 把值赋给数组中的特定元素:
[me@linuxbox ~]$ days=([0]=Sun [1]=Mon [2]=Tue [3]=Wed [4]=Thu [5]=Fri [6]=Sat)
访问数组元素
那么数组对什么有好处呢?  就像许多数据管理任务一样, 可以用电子表格程序来完成, 许多编程任务则可以用数
组完成.
让我们考虑一个简单的数据收集和展示的例子. 我们将构建一个脚本, 用来检查一个特定目录中文件的修改次
数.  从这些数据中, 我们的脚本将输出一张表, 显示这些文件最后是在一天中的哪个小时被修改的. 这样一个脚
本 可以被用来确定什么时段一个系统最活跃. 这个脚本, 称为 hours, 输出这样的结果:
[me@linuxbox ~]$ hours .
Hour Files Hour Files
---- ----- ---- ----
00
0
12
11
01
1
13
7
02
0
14
1
03
0
15
7
04
1
16
6
04
1
17
5
06
6
18
4

07
3
19
4
08
1
20
1
09
14
21
0
10
2
22
0
11
5
23
0
Total files = 80
当执行该 hours 程序时, 指定当前目录作为目标目录. 它打印出一张表显示一天(0-23小时)每小时内,  有多
少文件做了最后修改. 程序代码如下所示:
#!/bin/bash
# hours : script to count files by modification time
usage () {
echo "usage: $(basename $0) directory" >&2
}
# Check that argument is a directory
if [[ ! -d $1 ]]; then
usage
exit 1
fi
# Initialize array
for i in {0..23}; do hours[i]=0; done
# Collect data
for i in $(stat -c %y "$1"/* | cut -c 12-13); do
j=${i/#0}
((++hours[j]))
((++count))
done
# Display data
echo -e "Hour\tFiles\tHour\tFiles"
echo -e "----\t-----\t----\t-----"
for i in {0..11}; do
j=$((i + 12))
printf "%02d\t%d\t%02d\t%d\n" $i ${hours[i]} $j ${hours[j]}
done
printf "\nTotal files = %d\n" $count
这个脚本由一个函数(名为 usage), 和一个分为四个区块的主体组成. 在第一部分, 我们检查是否有一个命令
行参数,  且该参数为目录. 如果不是目录, 会显示脚本使用信息并退出.
第二部分初始化一个名为 hours 的数组. 给每一个数组元素赋值一个0. 虽然没有特殊需要在使用之前准备数
组, 但是 我们的脚本需要确保没有元素是空值. 注意这个循环构建方式很有趣. 通过使用花括号展开
({0..23}), 我们能 很容易为 for 命令产生一系列的数据(words).
接下来的一部分收集数据, 对目录中的每一个文件运行 stat 程序. 我们使用 cut 命令从结果中抽取两位数字的小

时字段.  在循环里面, 我们需要把小时字段开头的零清除掉, 因为 shell 将试图(最终会失败)把从 "00" 到
"09" 的数值解释为八进制(见表35-1).  下一步, 我们以小时为数组索引, 来增加其对应的数组元素的值.
最后, 我们增加一个计数器的值(count), 记录目录中总共的文件数目.
脚本的最后一部分显示数组中的内容. 我们首先输出两行标题, 然后进入一个循环产生两栏输出. 最后, 输出总
共的文件数目.
数组操作
有许多常见的数组操作. 比方说删除数组, 确定数组大小, 排序, 等等. 有许多脚本应用程序.
输出整个数组的内容
下标 * 和 @ 可以被用来访问数组中的每一个元素. 与位置参数一样, @ 表示法在两者之中更有用处.  这里是一
个演示:
[me@linuxbox ~]$ animals=("a dog" "a cat" "a fish")
[me@linuxbox ~]$ for i in ${animals[*]}; do echo $i; done
a
dog
a
cat
a
fish
[me@linuxbox ~]$ for i in ${animals[@]}; do echo $i; done
a
dog
a
cat
a
fish
[me@linuxbox ~]$ for i in "${animals[*]}"; do echo $i; done
a dog a cat a fish
[me@linuxbox ~]$ for i in "${animals[@]}"; do echo $i; done
a dog
a cat
a fish
我们创建了数组 animals, 并把三个含有两个字的字符串赋值给数组. 然后我们执行四个循环看一下对数组内容
进行分词的效果.  表示法 ${animals[*]} 和 ${animals[@]}的行为是一致的直到它们被用引号引起来.
确定数组元素个数
使用参数展开, 我们能够确定数组元素的个数, 与计算字符串长度的方式几乎相同. 这里是一个例子:

[me@linuxbox ~]$ a[100]=foo
[me@linuxbox ~]$ echo ${#a[@]} # number of array elements
1
[me@linuxbox ~]$ echo ${#a[100]} # length of element 100
3
我们创建了数组 a, 并把字符串 "foo" 赋值给数组元素100. 下一步, 我们使用参数展开来检查数组的长度,
使用 @ 表示法.  最后, 我们查看了包含字符串 "foo" 的数组元素 100 的长度. 有趣的是, 尽管我们把字符串
赋值给数组元素100,  bash 仅仅报告数组中有一个元素. 这不同于一些其它语言的行为, 数组中未使用的元素
(元素0-99)会初始化为空值,  并把它们计入数组长度.
找到数组使用的下标
因为 bash 允许赋值的数组下标包含 "间隔", 有时候确定哪个元素真正存在是很有用的. 为做到这一点,  可以
使用以下形式的参数展开:
${!array[*]}
${!array[@]}
这里的 array 是一个数组变量的名字. 和其它使用符号 * 和 @ 的展开一样, 用引号引起来的 @ 格式是最有用
的,  因为它能展开成分离的词.
[me@linuxbox ~]$ foo=([2]=a [4]=b [6]=c)
[me@linuxbox ~]$ for i in "${foo[@]}"; do echo $i; done
a
b
c
[me@linuxbox ~]$ for i in "${!foo[@]}"; do echo $i; done
2
4
6
在数组末尾添加元素
如果我们需要在数组末尾附加数据, 那么知道数组中元素的个数是没用的, 因为通过 * 和 @ 表示法返回的数值
不能 告诉我们使用的最大数组索引. 幸运地是, shell 为我们提供了一种解决方案. 通过使用 += 赋值运算符,
我们能够自动地把值附加到数组末尾. 这里, 我们把三个值赋给数组 foo, 然后附加另外三个.
[me@linuxbox~]$
[me@linuxbox~]$
a b c
[me@linuxbox~]$
[me@linuxbox~]$
foo=(a b c)
echo ${foo[@]}
foo+=(d e f)
echo ${foo[@]}

a b c d e f
数组排序
就像电子表格, 经常有必要对一列数据进行排序. Shell 没有这样做的直接方法, 但是通过一点儿代码, 并不难
实现.
#!/bin/bash
# array-sort : Sort an array
a=(f e d c b a)
echo "Original array: ${a[@]}"
a_sorted=($(for i in "${a[@]}"; do echo $i; done | sort))
echo "Sorted array: ${a_sorted[@]}"
当执行之后, 脚本产生这样的结果:
[me@linuxbox ~]$ array-sort
Original array: f e d c b a
Sorted array:
a b c d e f
脚本运行成功, 通过使用一个复杂的命令替换把原来的数组(a)中的内容复制到第二个数组(a_sorted)中.
通过修改管道线的设计, 这个基本技巧可以用来对数组执行各种各样的操作.
删除数组
删除一个数组, 使用 unset 命令:
[me@linuxbox
[me@linuxbox
a b c d e f
[me@linuxbox
[me@linuxbox
[me@linuxbox
~]$ foo=(a b c d e f)
~]$ echo ${foo[@]}
~]$ unset foo
~]$ echo ${foo[@]}
~]$
也可以使用 unset 命令删除单个的数组元素:
[me@linuxbox~]$ foo=(a b c d e f)
[me@linuxbox~]$ echo ${foo[@]}
a b c d e f

[me@linuxbox~]$ unset 'foo[2]'
[me@linuxbox~]$ echo ${foo[@]}
a b d e f
在这个例子中, 我们删除了数组中的第三个元素, 下标为2. 记住, 数组下标开始于0, 而不是1! 也要注意数组
元素必须 用引号引起来为的是防止 shell 执行路径名展开操作.
有趣地是, 给一个数组赋空值不会清空数组内容:
[me@linuxbox ~]$ foo=(a b c d e f)
[me@linuxbox ~]$ foo=
[me@linuxbox ~]$ echo ${foo[@]}
b c d e f
任何引用一个不带下标的数组变量, 则指的是数组元素0:
[me@linuxbox~]$
[me@linuxbox~]$
a b c d e f
[me@linuxbox~]$
[me@linuxbox~]$
A b c d e f
foo=(a b c d e f)
echo ${foo[@]}
foo=A
echo ${foo[@]}
关联数组
现在最新的 bash 版本支持关联数组了. 关联数组使用字符串而不是整数作为数组索引.  这种功能给出了一种有
趣的新方法来管理数据. 例如, 我们可以创建一个叫做 "colors" 的数组, 并用颜色名字作为索引.
declare -A colors
colors["red"]="#ff0000"
colors["green"]="#00ff00"
colors["blue"]="#0000ff"
不同于整数索引的数组, 仅仅引用它们就能创建数组, 关联数组必须用带有 -A 选项的 declare 命令创建.
访问关联数组元素的方式几乎与整数索引数组相同:
echo ${colors["blue"]}

在下一章中, 我们将看一个脚本, 很好地利用关联数组, 生产出了一个有意思的报告.
总结
如果我们在 bash 手册页中搜索单词 "array"的话, 我们能找到许多 bash 在哪里会使用数组变量的实例. 其中
大部分相当晦涩难懂,  但是它们可能在一些特殊场合提供临时的工具. 事实上, 在 shell 编程中, 整套数组规则
利用率相当低, 很大程度上归咎于这样的事实,  传统 Unix shell 程序(比如说 sh)缺乏对数组的支持. 这样缺
乏人气是不幸的, 因为数组广泛应用于其它编程语言,  并为解决各种各样的编程问题, 提供了一个强大的工具.
数组和循环有一种天然的姻亲关系, 它们经常被一起使用. 该
for ((expr; expr; expr))
形式的循环尤其适合计算数组下标.
拓展阅读
Wikipedia 上面有两篇关于在本章提到的数据结构的文章:
http://en.wikipedia.org/wiki/Scalar_(computing)
http://en.wikipedia.org/wiki/Associative_array

## 第三十七章: 奇珍异宝
在我们 bash 学习旅程中的最后一站, 我们将看一些零星的知识点. 当然我们在之前的章节中已经 涵盖了很多方
面, 但是还有许多 bash 特性我们没有涉及到. 其中大部分特性相当晦涩, 主要对 那些把 bash 集成到 Linux 发
行版的程序有用处. 然而还有一些特性, 虽然不常用,  但是对某些程序问题是很有帮助的. 我们将在这里介绍它
们.
组命令和子 shell
bash 允许把命令组合在一起. 可以通过两种方式完成; 要么用一个 group 命令, 要么用一个子 shell.  这里是
每种方式的语法示例:
组命令:
{ command1; command2; [command3; ...] }
子 shell:
(command1; command2; [command3;...])
这两种形式的不同之处在于, 组命令用花括号把它的命令包裹起来, 而子 shell 用括号. 值得注意的是, 鉴于
bash 实现组命令的方式,  花括号与命令之间必须有一个空格, 并且最后一个命令必须用一个分号或者一个换行
符终止.
那么组命令和子 shell 命令对什么有好处呢?  尽管它们有一个很重要的差异(我们马上会接触到), 但它们都是
用来管理重定向的.  让我们考虑一个对多个命令执行重定向的脚本片段.
ls -l > output.txt
echo "Listing of foo.txt" >> output.txt
cat foo.txt >> output.txt
这些代码相当简洁明了. 三个命令的输出都重定向到一个名为 output.txt 的文件中.  使用一个组命令, 我们可
以重新编 写这些代码, 如下所示:
{ ls -l; echo "Listing of foo.txt"; cat foo.txt; } > output.txt

使用一个子 shell 是相似的:
(ls -l; echo "Listing of foo.txt"; cat foo.txt) > output.txt
使用这样的技术, 我们为我们自己节省了一些打字时间, 但是组命令和子 shell 真正闪光的地方是与管道线相结
合.  当构建一个管道线命令的时候, 通常把几个命令的输出结果合并成一个流是很有用的.  组命令和子 shell 使
这种操作变得很简单:
{ ls -l; echo "Listing of foo.txt"; cat foo.txt; } | lpr
这里我们已经把我们的三个命令的输出结果合并在一起, 并把它们用管道输送给命令 lpr 的输入, 以便产生一个
打印报告.
在下面的脚本中, 我们将使用组命令, 看几个与关联数组结合使用的编程技巧. 这个脚本, 称为 array-2, 当给
定一个目录名, 打印出目录中的文件列表,  伴随着每个文件的文件所有者和组所有者. 在文件列表的末尾, 脚本
打印出属于每个所有者和组的文件数目.  这里我们看到的结果(缩短的, 为简单起见), 是给定脚本的目录为
/usr/bin 的时候:
[me@linuxbox ~]$ array-2 /usr/bin
/usr/bin/2to3-2.6
/usr/bin/2to3
/usr/bin/a2p
/usr/bin/abrowser
/usr/bin/aconnect
/usr/bin/acpi_fakekey
/usr/bin/acpi_listen
/usr/bin/add-apt-repository
.
/usr/bin/zipgrep
/usr/bin/zipinfo
/usr/bin/zipnote
/usr/bin/zip
/usr/bin/zipsplit
/usr/bin/zjsdecode
/usr/bin/zsoelim
root
root
root
root
root
root
root
root root
root
root
root
root
root
root
root
root
root
root
root
root
root
root root
root
root
root
root
root
root
File owners:
daemon : 1 file(s)
root
: 1394 file(s) File group owners:
crontab : 1 file(s)
daemon : 1 file(s)
lpadmin : 1 file(s)
mail
: 4 file(s)

mlocate
root
shadow
ssh
tty
utmp
:
:
:
:
:
:
1 file(s)
1380 file(s)
2 file(s)
1 file(s)
2 file(s)
2 file(s)
这里是脚本代码列表(带有行号):

```bash
#!/bin/bash
# array-2: Use arrays to tally file owners
declare -A files file_group file_owner groups owners
if [[ ! -d "$1" ]]; then
    echo "Usage: array-2 dir" >&2
    exit 1
fi
for i in "$1"/*; do
    owner=$(stat -c %U "$i")
    group=$(stat -c %G "$i")
    files["$i"]="$i"
    file_owner["$i"]=$owner
    file_group["$i"]=$group
    ((++owners[$owner]))
    ((++groups[$group]))
done
# List the collected files
{ for i in "${files[@]}"; do
    printf "%-40s %-10s %-10s\n" \
        "$i" ${file_owner["$i"]} ${file_group["$i"]}
done; } | sort
echo
# List owners
echo "File owners:"
{ for i in "${!owners[@]}"; do
    printf "%-10s: %5d file(s)\n" "$i" ${owners["$i"]}
done; } | sort
echo
# List groups
echo "File group owners:"
{ for i in "${!groups[@]}"; do
    printf "%-10s: %5d file(s)\n" "$i" ${groups["$i"]}
done; } | sort
```

让我们看一下这个脚本的运行机制:
行5:  关联数组必须用带有 -A 选项的 declare 命令创建. 在这个脚本中我们创建了如下五个数组:
files 包含了目录中文件的名字, 按文件名索引
file_group 包含了每个文件的组所有者, 按文件名索引
file_owner 包含了每个文件的所有者, 按文件名索引
groups 包含了属于索引的组的文件数目
owners 包含了属于索引的所有者的文件数目
行7-10: 查看是否一个有效的目录名作为位置参数传递给程序. 如果不是, 就会显示一条使用信息, 并且脚本退
出, 退出状态为1.
行12-20: 循环遍历目录中的所有文件. 使用 stat 命令, 行13和行14抽取文件所有者和组所有者,  并把值赋给
它们各自的数组(行16, 17), 使用文件名作为数组索引. 同样地, 文件名自身也赋值给 files 数组.
行18-19: 属于文件所有者和组所有者的文件总数各自加1.
行22-27: 输出文件列表. 为做到这一点, 使用了 "${array[@]}" 参数展开, 展开成整个的数组元素列表,  并
且每个元素被当做是一个单独的词. 从而允许文件名包含空格的情况. 也要注意到整个循环是包裹在花括号中,
从而形成了一个组命令. 这样就允许整个循环输出会被管道输送给 sort 命令的输入. 这是必要的, 因为 展开的
数组元素是无序的.
行29-40: 这两个循环与文件列表循环相似, 除了它们使用 "${!array[@]}" 展开, 展开成数组索引的列表 而不
是数组元素的.
进程替换
虽然组命令和子 shell 看起来相似, 并且它们都能用来在重定向中合并流, 但是两者之间有一个很重要的不同.
然而, 一个组命令在当前 shell 中执行它的所有命令, 而一个子 shell(顾名思义)在当前 shell 的一个 子副本中
执行它的命令. 这意味着运行环境被复制给了一个新的 shell 实例. 当这个子 shell 退出时, 环境副本会消失,
所以在子 shell 环境(包括变量赋值)中的任何更改也会消失. 因此, 在大多数情况下, 除非脚本要求一个子
shell,  组命令比子 shell 更受欢迎. 组命令运行很快并且占用的内存也少.
我们在第20章中看到过一个子 shell 运行环境问题的例子, 当我们发现管道线中的一个 read 命令 不按我们所期
望的那样工作的时候. 为了重现问题, 我们构建一个像这样的管道线:
echo "foo" | read
echo $REPLY
该 REPLY 变量的内容总是为空, 是因为这个 read 命令在一个子 shell 中执行, 所以它的 REPLY 副本会被毁掉,
当该子 shell 终止的时候. 因为管道线中的命令总是在子 shell 中执行, 任何给变量赋值的命令都会遭遇这样的

问题.  幸运地是, shell 提供了一种奇异的展开方式, 叫做进程替换, 它可以用来解决这种麻烦. 进程替换有两
种表达方式:
一种适用于产生标准输出的进程:
<(list)
另一种适用于接受标准输入的进程:
>(list)
这里的 list 是一串命令列表:
为了解决我们的 read 命令问题, 我们可以雇佣进程替换, 像这样:
read < <(echo "foo")
echo $REPLY
进程替换允许我们把一个子 shell 的输出结果当作一个用于重定向的普通文件. 事实上, 因为它是一种展开形
式, 我们可以检验它的真实值:
[me@linuxbox ~]$ echo <(echo "foo")
/dev/fd/63
通过使用 echo 命令, 查看展开结果, 我们看到子 shell 的输出结果, 由一个名为 /dev/fd/63 的文件提供.
进程替换经常被包含 read 命令的循环用到. 这里是一个 read 循环的例子, 处理一个目录列表的内容, 内容创建
于一个子 shell:
#!/bin/bash
# pro-sub : demo of process substitution
while read attr links owner group size date time filename; do
cat <<- EOF
Filename:
$filename
Size:
$size
Owner:
$owner
Group:
$group
Modified:
$date $time
Links:
$links
Attributes:
$attr

EOF
done < <(ls -l | tail -n +2)
这个循环对目录列表的每一个条目执行 read 命令. 列表本身产生于该脚本的最后一行代码. 这一行代码把从进
程替换得到的输出 重定向到这个循环的标准输入. 这个包含在管道线中的 tail 命令, 是为了消除列表的第一行文
本, 这行文本是多余的.
当脚本执行后, 脚本产生像这样的输出:
[me@linuxbox ~]$ pro_sub | head -n 20
Filename: addresses.ldif
Size: 14540
Owner: me
Group: me
Modified: 2009-04-02 11:12
Links:
1
Attributes: -rw-r--r--
Filename: bin
Size: 4096
Owner: me
Group: me
Modified: 2009-07-10 07:31
Links: 2
Attributes: drwxr-xr-x
Filename: bookmarks.html
Size: 394213
Owner: me
Group: me

陷阱
在第10章中, 我们看到过程序是怎样响应信号的. 我们也可以把这个功能添加到我们的脚本中. 然而到目前为
止,  我们所编写过的脚本还不需要这种功能(因为它们运行时间非常短暂, 并且不创建临时文件), 大且更复杂
的脚本 可能会受益于一个信息处理程序.
当我们设计一个大的, 复杂的脚本的时候, 若脚本仍在运行时, 用户注销或关闭了电脑, 这时候会发生什么, 考
虑到这一点非常重要.  当像这样的事情发生了, 一个信号将会发送给所有受到影响的进程. 依次地, 代表这些进
程的程序会执行相应的动作, 来确保程序 合理有序的终止. 比方说, 例如, 我们编写了一个会在执行时创建临时
文件的脚本. 在一个好的设计流程, 我们应该让脚本删除创建的 临时文件, 当脚本完成它的任务之后. 若脚本接
收到一个信号, 表明该程序即将提前终止的信号,  此时让脚本删除创建的临时文件, 也会是很精巧的设计.
为满足这样需求, bash 提供了一种机制, 众所周知的 trap. 陷阱由被恰当命令的内部命令 trap 实现.  trap 使
用如下语法:

trap argument signal [signal...]
这里的 argument 是一个字符串, 它被读取并被当作一个命令, signal 是一个信号的说明, 它会触发执行所要解
释的命令.
这里是一个简单的例子:
#!/bin/bash
# trap-demo : simple signal handling demo
trap "echo 'I am ignoring you.'" SIGINT SIGTERM
for i in {1..5}; do
echo "Iteration $i of 5"
sleep 5
done
这个脚本定义一个陷阱, 当脚本运行的时候, 这个陷阱每当接受到一个 SIGINT 或 SIGTERM 信号时, 就会执行
一个 echo 命令.  当用户试图通过按下 Ctrl-c 组合键终止脚本运行的时候, 该程序的执行结果看起来像这样:
[me@linuxbox ~]$ trap-demo
Iteration 1 of 5
Iteration 2 of 5
I am ignoring you.
Iteration 3 of 5
I am ignoring you.
Iteration 4 of 5
Iteration 5 of 5
正如我们所看到的, 每次用户试图中断程序时, 会打印出这条信息.
构建一个字符串形成一个有用的命令序列是很笨拙的, 所以通常的做法是指定一个 shell 函数作为命令. 在这个
例子中,  为每一个信号指定了一个单独的 shell 函数来处理:

```bash
#!/bin/bash
# trap-demo2 : simple signal handling demo
exit_on_signal_SIGINT () {
echo "Script interrupted." 2>&1
exit 0
}
exit_on_signal_SIGTERM () {
echo "Script terminated." 2>&1
exit 0
}
trap exit_on_signal_SIGINT SIGINT
trap exit_on_signal_SIGTERM SIGTERM
for i in {1..5}; do
echo "Iteration $i of 5"
sleep 5
done
```

这个脚本的特色是有两个 trap 命令, 每个命令对应一个信号. 每个 trap, 依次, 当接受到相应的特殊信号时,
会执行指定的 shell 函数. 注意每个信号处理函数中都包含了一个 exit 命令. 没有 exit 命令,  信号处理函数执
行完后, 该脚本将会继续执行.
当用户在这个脚本执行期间, 按下 Ctrl-c 组合键的时候, 输出结果看起来像这样:
[me@linuxbox ~]$ trap-demo2
Iteration 1 of 5
Iteration 2 of 5
Script interrupted.

## 临时文件
把信号处理程序包含在脚本中的一个原因是删除临时文件, 在脚本执行期间, 脚本可能会创建临时文件来存
放中间结果.  命名临时文件是一种艺术. 传统上, 在类似于 unix 系统中的程序会在 /tmp 目录下创建它们的
临时文件, /tmp 是 一个服务于临时文件的共享目录. 然而, 因为这个目录是共享的, 这会引起一定的安全
顾虑, 尤其对那些用 超级用户特权运行的程序. 除了为暴露给系统中所有用户的文件设置合适的权限, 这一
明显步骤之外,  给临时文件一个不可预测的文件名是很重要的. 这就避免了一种为大众所知的 temp race 攻
击.  一种创建一个不可预测的(但是仍有意义的)临时文件名的方法是, 做一些像这样的事情:
tempfile=/tmp/$(basename $0).$.$RANDOM
这将创建一个由程序名字, 程序进程的 ID(PID)文件名, 和一个随机整数组成. 注意, 然而, 该
$RANDOM shell 变量 只能返回一个范围在1-32767内的整数值, 这在计算机术语中不是一个很大的范围,
所以一个单一的该变量实例是不足以克服一个坚定的攻击者的.
一个比较好的方法是使用 mktemp 程序(不要和 mktemp 标准库函数相混淆)来命名和创建临时文件.  这
个 mktemp 程序接受一个用于创建文件名的模板作为参数. 这个模板应该包含一系列的 "X" 字符,  随后
这些字符会被相应数量的随机字母和数字替换掉. 一连串的 "X" 字符越长, 则一连串的随机字符也就越
长.  这里是一个例子:
tempfile=$(mktemp /tmp/foobar.$.XXXXXXXXXX)
这里创建了一个临时文件, 并把临时文件的名字赋值给变量 tempfile. 因为模板中的 "X" 字符会被随机字
母和 数字代替, 所以最终的文件名(在这个例子中, 文件名也包含了特殊参数 $$ 的展开值, 进程的 PID)
可能像这样:

/tmp/foobar.6593.UOZuvM6654
对于那些由普通用户操作执行的脚本, 避免使用 /tmp 目录, 而是在用户家目录下为临时文件创建一个目
录,  通过像这样的一行代码:
[[ -d $HOME/tmp ]] || mkdir $HOME/tmp
异步执行
有时候需要同时执行多个任务. 我们已经知道现在所有的操作系统若不是多用户的但至少是多任务的.  脚本也可
以构建成多任务处理的模式.
通常这涉及到启动一个脚本, 依次, 启动一个或多个子脚本来执行额外的任务, 而父脚本继续运行. 然而, 当一
系列脚本 以这种方式运行时, 要保持父子脚本之间协调工作, 会有一些问题. 也就是说, 若父脚本或子脚本依赖
于另一方, 并且 一个脚本必须等待另一个脚本结束任务之后, 才能完成它自己的任务, 这应该怎么办?
bash 有一个内置命令, 能帮助管理诸如此类的异步执行的任务. wait 命令导致一个父脚本暂停运行, 直到一个
特定的进程(例如, 子脚本)运行结束.
等待
首先我们将演示一下 wait 命令的用法. 为此, 我们需要两个脚本, 一个父脚本:
#!/bin/bash
# async-parent : Asynchronous execution demo (parent)
echo "Parent: starting..."
echo "Parent: launching child script..."
async-child &
pid=$!
echo "Parent: child (PID= $pid) launched."
echo "Parent: continuing..."
sleep 2
echo "Parent: pausing to wait for child to finish..."
wait $pid
echo "Parent: child is finished. Continuing..."
echo "Parent: parent is done. Exiting."
和一个子脚本:
#!/bin/bash
# async-child : Asynchronous execution demo (child)
echo "Child: child is running..."
sleep 5
echo "Child: child is done. Exiting."

在这个例子中, 我们看到该子脚本是非常简单的. 真正的操作通过父脚本完成. 在父脚本中, 子脚本被启动,  并
被放置到后台运行. 子脚本的进程 ID 记录在 pid 变量中, 这个变量的值是 $! shell 参数的值, 它总是 包含放到
后台执行的最后一个任务的进程 ID 号.
父脚本继续, 然后执行一个以子进程 PID 为参数的 wait 命令. 这就导致父脚本暂停运行, 直到子脚本退出,  意
味着父脚本结束.
当执行后, 父子脚本产生如下输出:
[me@linuxbox ~]$ async-parent
Parent: starting...
Parent: launching child script...
Parent: child (PID= 6741) launched.
Parent: continuing...
Child: child is running...
Parent: pausing to wait for child to finish...
Child: child is done. Exiting.
Parent: child is finished. Continuing...
Parent: parent is done. Exiting.
命名管道
在大多数类似也 Unix 的操作系统中, 有可能创建一种特殊类型的饿文件, 叫做命名管道. 命名管道用来在 两个
进程之间建立连接, 也可以像其它类型的文件一样使用. 虽然它们不是那么流行, 但是它们值得我们去了解.
有一种常见的编程架构, 叫做客户端-服务器, 它可以利用像命名管道这样的通信方式,  也可以使用其它类型的
进程间通信方式, 比如网络连接.
最为广泛使用的客户端-服务器系统类型是, 当然, 一个 web 浏览器与一个 web 服务器之间进行通信.  web 浏
览器作为客户端, 向服务器发出请求, 服务器响应请求, 并把对应的网页发送给浏览器.
命令管道的行为类似于文件, 但实际上形成了先入先出(FIFO)的缓冲. 和普通(未命令的)管道一样,  数据从
一端进入, 然后从另一端出现. 通过命令管道, 有可能像这样设置一些东西:
process1 > named_pipe
和
process2 < named_pipe
表现出来就像这样:

process1 | process2
设置一个命名管道
首先, 我们必须创建一个命名管道. 使用 mkfifo 命令能够创建命令管道:
[me@linuxbox
[me@linuxbox
prw-r--r-- 1
me
0 2009-07-17
~]$ mkfifo pipe1
~]$ ls -l pipe1
me
06:41 pipe1
这里我们使用 mkfifo 创建了一个名为 pipe1 的命名管道. 使用 ls 命令, 我们查看这个文件,  看到位于属性字
段的第一个字母是 "p", 表明它是一个命名管道.
使用命名管道
为了演示命名管道是如何工作的, 我们将需要两个终端窗口(或用两个虚拟控制台代替).  在第一个终端中, 我
们输入一个简单命令, 并把命令的输出重定向到命名管道:
[me@linuxbox ~]$ ls -l > pipe1
我们按下 Enter 按键之后, 命令将会挂起. 这是因为在管道的另一端没有任何接受数据. 当这种现象发生的时
候,  据说是管道阻塞了. 一旦我们绑定一个进程到管道的另一端, 该进程开始从管道中读取输入的时候, 这种情
况会消失.  使用第二个终端窗口, 我们输入这个命令:
[me@linuxbox ~]$ cat < pipe1
然后产自第一个终端窗口的目录列表出现在第二个终端中, 并作为来自 cat 命令的输出. 在第一个终端 窗口中的
ls 命令一旦它不再阻塞, 会成功地结束.
总结
嗯, 我们已经完成了我们的旅程. 现在剩下的唯一要做的事就是练习, 练习, 再练习.  纵然在我们的长途跋涉
中, 我们涉及了很多命令, 但是就命令行而言, 我们只是触及了它的表面.  仍留有成千上万的命令行程序, 需要
去发现和享受. 开始挖掘 /usr/bin 目录吧, 你将会看到!
拓展阅读

bash 手册页的 "复合命令" 部分包含了对组命令和子 shell 表示法的详尽描述.
bash 手册也的 EXPANSION 部分包含了一小部分进程替换的内容:
<高级 Bash 脚本指南>也有对进程替换的讨论:
http://tldp.org/LDP/abs/html/process-sub.html
<Linux 杂志>有两篇关于命令管道的好文章. 第一篇, 源于1997年9月:
http://www.linuxjournal.com/article/2156
和第二篇, 源于2009年3月:
http://www.linuxjournal.com/content/using-named-pipes-fifos-bash
