# shell 脚本

## 第二十九章: 读取键盘输入
到目前为止我们编写的脚本都缺乏一项在大多数计算机程序中都很常见的功能－交互性. 也就是,  程序与用户进
行交互的能力. 虽然许多程序不必是可交互的, 但一些程序却得到益处, 能够直接 接受用户的输入. 以这个前面
章节中的脚本为例:
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
每次我们想要改变 INT 数值的时候, 我们必须编辑这个脚本. 如果脚本能请求用户输入数值, 那 么它会更加有
用处. 在这个脚本中, 我们将看一下我们怎样给程序增加交互性功能.
read - 从标准输入读取数值
这个 read 内部命令被用来从标准输入读取单行数据. 这个命令可以用来读取键盘输入, 当使用 重定向的时候,
读取文件中的一行数据. 这个命令有以下语法形式:
read [-options] [variable...]
这里的 options 是下面列出的可用选项中的一个或多个, 且 variable 是用来存储输入数值的一个或多个变量
名.  如果没有提供变量名, shell 变量 REPLY 会包含数据行.
基本上, read 会把来自标准输入的字段赋值给具体的变量. 如果我们修改我们的整数求值脚本, 让其使用 read

, 它可能看起来像这样:
#!/bin/bash
# read-integer: evaluate the value of an integer.
echo -n "Please enter an integer -> "
read int
if [[ "$int" =~ ^-?[0-9]+$ ]]; then
if [ $int -eq 0 ]; then
echo "$int is zero."
else
if [ $int -lt 0 ]; then
echo "$int is negative."
else
echo "$int is positive."
fi
if [ $((int % 2)) -eq 0 ]; then
echo "$int is even."
else
echo "$int is odd."
fi
fi
else
echo "Input value is not an integer." >&2
exit 1
fi
我们使用带有 -n 选项(其会删除输出结果末尾的换行符)的 echo 命令, 来显示提示信息,  然后使用 read 来
读入变量 int 的数值. 运行这个脚本得到以下输出:
[me@linuxbox ~]$ read-integer
Please enter an integer -> 5
5 is positive.
5 is odd.
read 可以给多个变量赋值, 正如下面脚本中所示:
#!/bin/bash
# read-multiple: read multiple values from keyboard
echo -n "Enter one or more values > "
read var1 var2 var3 var4 var5
echo "var1 = '$var1'"
echo "var2 = '$var2'"
echo "var3 = '$var3'"
echo "var4 = '$var4'"
echo "var5 = '$var5'"

在这个脚本中, 我们给五个变量赋值并显示其结果. 注意当给定不同个数的数值后, read 怎样操作:
[me@linuxbox ~]$ read-multiple
Enter one or more values > a b c d e
var1 = 'a'
var2 = 'b'
var3 = 'c'
var4 = 'd'
var5 = 'e'
[me@linuxbox ~]$ read-multiple
Enter one or more values > a
var1 = 'a'
var2 = ''
var3 = ''
var4 = ''
var5 = ''
[me@linuxbox ~]$ read-multiple
Enter one or more values > a b c d e f g
var1 = 'a'
var2 = 'b'
var3 = 'c'
var4 = 'd'
var5 = 'e f g'
如果 read 命令接受到变量值数目少于期望的数字, 那么额外的变量值为空, 而多余的输入数据则会 被包含到最
后一个变量中. 如果 read 命令之后没有列出变量名, 则一个 shell 变量, REPLY, 将会包含 所有的输入:
#!/bin/bash
# read-single: read multiple values into default variable
echo -n "Enter one or more values > "
read
echo "REPLY = '$REPLY'"
这个脚本的输出结果是:
[me@linuxbox ~]$ read-single
Enter one or more values > a b c d
REPLY = 'a b c d'
选项
read 支持以下选送:
表29-1: read 选项

选项 说明
-a array 把输入赋值到数组 array 中, 从索引号
零开始. 我们 将在第36章中讨论数组
问题.
-d delimiter 用字符串 delimiter 中的第一个字符指
示输入结束, 而不是一个换行符.
-e 使用 Readline 来处理输入. 这使得与
命令行相同的方式编辑输入.
-n num 读取 num 个输入字符, 而不是整行.
-p prompt 为输入显示提示信息, 使用字符串
prompt.
-r Raw mode. 不把反斜杠字符解释为转
义字符.
-s Silent mode. 不会在屏幕上显示输入的
字符. 当输入密码和其它确认信息的时
候, 这会很有帮助.
-t seconds 超时. 几秒钟后终止输入. read 会返回
一个非零退出状态, 若输入超时.
-u fd 使用文件描述符 fd 中的输入, 而不是
标准输入.

使用各种各样的选项, 我们能用 read 完成有趣的事情. 例如, 通过-p 选项, 我们能够提供提示信息:
#!/bin/bash
# read-single: read multiple values into default variable
read -p "Enter one or more values > "
echo "REPLY = '$REPLY'"
通过 -t 和 -s 选项, 我们可以编写一个这样的脚本, 读取"秘密"输入, 并且如果在特定的时间内 输入没有完
成, 就终止输入.
#!/bin/bash
# read-secret: input a secret pass phrase
if read -t 10 -sp "Enter secret pass phrase > " secret_pass; then
echo -e "\nSecret pass phrase = '$secret_pass'"
else
echo -e "\nInput timed out" >&2
exit 1
if
这个脚本提示用户输入一个密码, 并等待输入10秒钟. 如果在特定的时间内没有完成输入,  则脚本会退出并返回
一个错误. 因为包含了一个 -s 选项, 所以输入的密码不会出现在屏幕上.
IFS
通常, shell 对提供给 read 的输入按照单词进行分离. 正如我们所见到的, 这意味着多个由一个或几个空格 分离
开的单词在输入行中变成独立的个体, 并被 read 赋值给单独的变量. 这种行为由 shell 变量__IFS__ (内部字符
分隔符)配置. IFS 的默认值包含一个空格, 一个 tab, 和一个换行符, 每一个都会把 字段分割开.
我们可以调整 IFS 的值来控制输入字段的分离. 例如, 这个 /etc/passwd 文件包含的数据行 使用冒号作为字段
分隔符. 通过把 IFS 的值更改为单个冒号, 我们可以使用 read 读取 /etc/passwd 中的内容, 并成功地把字段分
给不同的变量. 这个就是做这样的事情:
#!/bin/bash
# read-ifs: read fields from a file
FILE=/etc/passwd
read -p "Enter a user name > " user_name
file_info=$(grep "^$user_name:" $FILE)
if [ -n "$file_info" ]; then
IFS=":" read user pw uid gid name home shell <<< "$file_info"
echo "User = '$user'"
echo "UID = '$uid'"
echo "GID = '$gid'"
echo "Full Name = '$name'"
echo "Home Dir. = '$home'"
echo "Shell = '$shell'"
else
echo "No such user '$user_name'" >&2
exit 1
fi
这个脚本提示用户输入系统中一个帐户的用户名, 然后显示在文件 /etc/passwd/ 文件中关于用户记录的 不同字
段. 这个脚本包含两个有趣的文本行.  第一个是:
file_info=$(grep "^$user_name:" $FILE)
这一行把 grep 命令的输入结果赋值给变量 file_info. grep 命令使用的正则表达式 确保用户名只会在
/etc/passwd 文件中匹配一个文本行.
第二个有意思的文本行是:
IFS=":" read user pw uid gid name home shell <<< "$file_info"
这一行由三部分组成: 一个变量赋值, 一个带有一串参数的 read 命令, 和一个奇怪的新的重定向操作符.  我们
首先看一下变量赋值.

Shell 允许在一个命令之前立即发生一个或多个变量赋值. 这些赋值为跟随着的命令更改环境变量.  这个赋值的
影响是暂时的; 只是在命令存在期间改变环境变量. 在这种情况下, IFS 的值改为一个冒号.  另外, 我们也可以
这样编码:
OLD_IFS="$IFS"
IFS=":"
read user pw uid gid name home shell <<< "$file_info"
IFS="$OLD_IFS"
我们先存储 IFS 的值, 然后赋给一个新值, 再执行 read 命令, 最后把 IFS 恢复原值. 显然, 完成相同的任务,
在命令之前放置变量名赋值是一种更简明的方式.
这个
<<<
操作符指示一个 here 字符串. 一个 here 字符串就像一个 here 文档, 只是比较简短, 由 单个字符
串组成. 在这个例子中, 来自 /etc/passwd 文件的数据发送给 read 命令的标准输入.  我们可能想知道为什么选
择这种相当晦涩的方法而不是:
echo "$file_info" | IFS=":" read user pw uid gid name home shell
你不能管道 read
虽然通常 read 命令接受标准输入, 但是你不能这样做:
echo "foo" | read
我们期望这个命令能生效, 但是它不能. 这个命令将显示成功, 但是 REPLY 变量 总是为空. 为什么会这样?
答案与 shell 处理管道线的方式有关系. 在 bash(和其它 shells, 例如 sh)中, 管道线 会创建子 shell. 它
们是 shell 的副本, 且用来执行命令的环境变量在管道线中.  上面示例中, read 命令将在子 shell 中执行.
在类 Unix 的系统中, 子 shell 执行的时候, 会为进程创建父环境的副本. 当进程结束 之后, 环境副本就会被
破坏掉. 这意味着一个子 shell 永远不能改变父进程的环境. read 赋值变量,  然后会变为环境的一部分. 在
上面的例子中, read 在它的子 shell 环境中, 把 foo 赋值给变量 REPLY,  但是当命令退出后, 子 shell 和它
的环境将被破坏掉, 这样赋值的影响就会消失.
使用 here 字符串是解决此问题的一种方法. 另一种方法将在37章中讨论.
校正输入
从键盘输入这种新技能, 带来了额外的编程挑战, 校正输入. 很多时候, 一个良好编写的程序与 一个拙劣程序之
间的区别就是程序处理意外的能力. 通常, 意外会以错误输入的形式出现. 在前面 章节中的计算程序, 我们已经
这样做了一点儿, 我们检查整数值, 甄别空值和非数字字符. 每次 程序接受输入的时候, 执行这类的程序检查非
常重要, 为的是避免无效数据. 对于 由多个用户共享的程序, 这个尤为重要. 如果一个程序只使用一次且只被作
者用来执行一些特殊任务,  那么为了经济利益而忽略这些保护措施, 可能会被原谅. 即使这样, 如果程序执行危
险任务, 比如说 删除文件, 所以最好包含数据校正, 以防万一.

这里我们有一个校正各种输入的示例程序:
#!/bin/bash
# read-validate: validate input
invalid_input () {
echo "Invalid input '$REPLY'" >&2
exit 1
}
read -p "Enter a single item > "
# input is empty (invalid)
[[ -z $REPLY ]] && invalid_input
# input is multiple items (invalid)
(( $(echo $REPLY | wc -w) > 1 )) && invalid_input
# is input a valid filename?
if [[ $REPLY =~ ^[-[:alnum:]\._]+$ ]]; then
echo "'$REPLY' is a valid filename."
if [[ -e $REPLY ]]; then
echo "And file '$REPLY' exists."
else
echo "However, file '$REPLY' does not exist."
fi
# is input a floating point number?
if [[ $REPLY =~ ^-?[[:digit:]]*\.[[:digit:]]+$ ]]; then
echo "'$REPLY' is a floating point number."
else
echo "'$REPLY' is not a floating point number."
fi
# is input an integer?
if [[ $REPLY =~ ^-?[[:digit:]]+$ ]]; then
echo "'$REPLY' is an integer."
else
echo "'$REPLY' is not an integer."
fi
else
echo "The string '$REPLY' is not a valid filename."
fi
这个脚本提示用户输入一个数字. 随后, 分析这个数字来决定它的内容. 正如我们所看到的, 这个脚本 使用了许
多我们已经讨论过的概念, 包括 shell 函数,  [[ ]] ,  (( )) , 控制操作符
&& , 以及
if
和 一些
正则表达式.
菜单
一种常见的交互类型称为菜单驱动. 在菜单驱动程序中, 呈现给用户一系列选择, 并要求用户选择一项.  例如,
我们可以想象一个展示以下信息的程序:

Please Select:
1.Display System Information
2.Display Disk Space
3.Display Home Space Utilization
0.Quit
Enter selection [0-3] >
使用我们从编写 sys_info_page 程序中所学到的知识, 我们能够构建一个菜单驱动程序来执行 上述菜单中的任
务:
#!/bin/bash
# read-menu: a menu driven system information program
clear
echo "
Please Select:
1\.
2\.
3\.
0\.
Display System Information
Display Disk Space
Display Home Space Utilization
Quit
"
read -p "Enter selection [0-3] > "
if [[ $REPLY =~ ^[0-3]$ ]]; then
if [[ $REPLY == 0 ]]; then
echo "Program terminated."
exit
fi
if [[ $REPLY == 1 ]]; then
echo "Hostname: $HOSTNAME"
uptime
exit
fi
if [[ $REPLY == 2 ]]; then
df -h
exit
fi
if [[ $REPLY == 3 ]]; then
if [[ $(id -u) -eq 0 ]]; then
echo "Home Space Utilization (All Users)"
du -sh /home/*
else
echo "Home Space Utilization ($USER)"
du -sh $HOME
fi
exit
fi

else
echo "Invalid entry." >&2
exit 1
fi
The presence of multiple ｀exit｀ points in a program is generally a bad idea (it makes
从逻辑上讲, 这个脚本被分为两部分. 第一部分显示菜单和用户输入. 第二部分确认用户反馈, 并执行 选择的行
动. 注意脚本中使用的 exit 命令. 在这里, 在一个行动执行之后,  exit 被用来阻止脚本执行不必要的代码.  通
常在程序中出现多个 exit 代码是一个坏想法(它使程序逻辑较难理解), 但是它在这个脚本中起作用.
总结归纳
在这一章中, 我们向着程序交互性迈出了第一步; 允许用户通过键盘向程序输入数据. 使用目前 已经学过的技
巧, 有可能编写许多有用的程序, 比如说特定的计算程序和容易使用的命令行工具 前端. 在下一章中, 我们将继
续建立菜单驱动程序概念, 让它更完善.
友情提示
仔细研究本章中的程序, 并对程序的逻辑结构有一个完整的理解, 这是非常重要的, 因为即将到来的 程序会日益
复杂. 作为练习, 用 test 命令而不是 [[ ]] 复合命令来重新编写本章中的程序.  提示: 使用 grep 命令来计
算正则表达式及其退出状态. 这会是一个不错的实践.
拓展阅读
Bash 参考手册有一章关于内部命令的内容, 其包括了 read 命令:
http://www.gnu.org/software/bash/manual/bashref.html#Bash-Builtins

第三十章: 流程控制 while/until 循环
在前面的章节中, 我们开发了菜单驱动程序, 来产生各种各样的系统信息. 虽然程序能够运行,  但它仍然存在重
大的可用问题. 它只能执行单一的选择, 然后终止. 更糟糕地是, 如果做了一个 无效的选择, 程序会以错误终
止, 而没有给用户提供再试一次的机会. 如果我们能构建程序,  以致于程序能够重复显示菜单, 而且能一次由一
次的选择, 直到用户选择退出程序, 这样的程序会更好一些.
在这一章中, 我们将看一个叫做循环的程序概念, 其可用来使程序的某些部分重复. shell 为循环提供了三个复合
命令.  本章我们将查看其中的两个命令, 随后章节介绍第三个命令.
循环
日常生活中充满了重复性的活动. 每天去散步, 遛狗, 切胡萝卜, 所有任务都要重复一系列的步骤.  让我们以切
胡萝卜为例. 如果我们用伪码表达这种活动, 它可能看起来像这样:
1. 准备切菜板
2. 准备菜刀
3. 把胡萝卜放到切菜板上
4. 提起菜刀
5. 向前推进胡萝卜
6. 切胡萝卜
7. 如果切完整个胡萝卜, 就退出, 要不然回到第四步继续执行
从第四步到第七步形成一个循环. 重复执行循环内的动作直到满足条件"切完整个胡萝卜".
while
bash 能够表达相似的想法. 比方说我们想要按照顺序从1到5显示五个数字. 可如下构造一个 bash 脚本:
#!/bin/bash
# while-count: display a series of numbers
count=1
while [ $count -le 5 ]; do
echo $count
count=$((count + 1))
done
echo "Finished."
当执行的时候, 这个脚本显示如下信息:
[me@linuxbox ~]$ while-count

1
2
3
4
5
Finished.
while 命令的语法是:
while commands; do commands; done
和 if 一样,  while 计算一系列命令的退出状态. 只要退出状态为零, 它就执行循环内的命令.  在上面的脚本
中, 创建了变量 count , 并初始化为1.  while 命令将会计算 test 命令的退出状态.  只要 test 命令返回退出状
态零, 循环内的所有命令就会执行. 每次循环结束之后, 会重复执行 test 命令.  第六次循环之后,  count 的数
值增加到6,  test 命令不再返回退出状态零, 且循环终止.  程序继续执行循环之后的语句.
我们可以使用一个 while 循环, 来提高前面章节的 read-menu 程序:
#!/bin/bash
# while-menu: a menu driven system information program
DELAY=3 # Number of seconds to display results
while [[ $REPLY != 0 ]]; do
clear
cat <<- _EOF_
Please Select:
1\. Display System Information
2\. Display Disk Space
3\. Display Home Space Utilization
0\. Quit
_EOF_
read -p "Enter selection [0-3] > "
if [[ $REPLY =~ ^[0-3]$ ]]; then
if [[ $REPLY == 1 ]]; then
echo "Hostname: $HOSTNAME"
uptime
sleep $DELAY
fi
if [[ $REPLY == 2 ]]; then
df -h
sleep $DELAY
fi
if [[ $REPLY == 3 ]]; then
if [[ $(id -u) -eq 0 ]]; then
echo "Home Space Utilization (All Users)"
du -sh /home/*
else
echo "Home Space Utilization ($USER)"
du -sh $HOME
fi
sleep $DELAY
fi
else
echo "Invalid entry."
sleep $DELAY
fi
done
echo "Program terminated."

通过把菜单包含在 while 循环中, 每次用户选择之后, 我们能够让程序重复显示菜单. 只要 REPLY 不 等
于"0", 循环就会继续, 菜单就能显示, 从而用户有机会重新选择. 每次动作完成之后, 会执行一个 sleep 命
令, 所以在清空屏幕和重新显示菜单之前, 程序将会停顿几秒钟, 为的是能够看到选项输出结果.  一旦 REPLY
等于"0", 则表示选择了"退出"选项, 循环就会终止, 程序继续执行 done 语句之后的代码.
跳出循环
bash 提供了两个内部命令, 它们可以用来在循环内部控制程序流程. 这个 break 命令立即终止一个循环,  且程
序继续执行循环之后的语句. 这个 continue 命令导致程序跳过循环中剩余的语句, 且程序继续执行 下一次循
环. 这里我们看看采用了 break 和 continue 两个命令的 while-menu 程序版本:
#!/bin/bash
# while-menu2: a menu driven system information program
DELAY=3 # Number of seconds to display results
while true; do
clear
cat <<- _EOF_
Please Select:
1\. Display System Information
2\. Display Disk Space
3\. Display Home Space Utilization
0\. Quit
_EOF_
read -p "Enter selection [0-3] > "
if [[ $REPLY =~ ^[0-3]$ ]]; then
if [[ $REPLY == 1 ]]; then
echo "Hostname: $HOSTNAME"
uptime
sleep $DELAY
continue
fi
if [[ $REPLY == 2 ]]; then
df -h
sleep $DELAY
continue
fi
if [[ $REPLY == 3 ]]; then
if [[ $(id -u) -eq 0 ]]; then
echo "Home Space Utilization (All Users)"
du -sh /home/*
else
echo "Home Space Utilization ($USER)"
du -sh $HOME
fi
sleep $DELAY
continue
fi
if [[ $REPLY == 0 ]]; then
break
fi
else
echo "Invalid entry."
sleep $DELAY
fi
done
echo "Program terminated."

在这个脚本版本中, 我们设置了一个无限循环(就是自己永远不会终止的循环), 通过使用 true 命令 为 while
提供一个退出状态. 因为 true 的退出状态总是为零, 所以循环永远不会终止. 这是一个 令人惊讶的通用脚本编
程技巧. 因为循环自己永远不会结束, 所以由程序员在恰当的时候提供某种方法来跳出循环.  此脚本, 当选
择"0"选项的时候, break 命令被用来退出循环. continue 命令被包含在其它选择动作的末尾,  为的是更加高
效执行. 通过使用 continue 命令, 当一个选项确定后, 程序会跳过不需要的代码. 例如,  如果选择了选
项"1", 则没有理由去测试其它选项.
until
这个 until 命令与 while 非常相似, 除了当遇到一个非零退出状态的时候,  while 退出循环,  而 until 不退出.
一个 until 循环会继续执行直到它接受了一个退出状态零. 在我们的 while-count 脚本中,  我们继续执行循环直
到 count 变量的数值小于或等于5. 我们可以得到相同的结果, 通过在脚本中使用 until 命令:
#!/bin/bash
# until-count: display a series of numbers
count=1
until [ $count -gt 5 ]; do
echo $count
count=$((count + 1))
done

echo "Finished."
通过把 test 表达式更改为 $count -gt 5 ,  until 会在正确的时间终止循环. 决定使用 while 循环 还是 until 循
环, 通常是选择一个 test 可以编写地很清楚的循环.
使用循环读取文件
while 和 until 能够处理标准输入. 这就可以使用 while 和 until 处理文件. 在下面的例子中,  我们将显示在前
面章节中使用的 distros.txt 文件的内容:
#!/bin/bash
# while-read: read lines from a file
while read distro version release; do
printf "Distro: %s\tVersion: %s\tReleased: %s\n" \
$distro \
$version \
$release
done < distros.txt
为了重定向文件到循环中, 我们把重定向操作符放置到 done 语句之后. 循环将使用 read 从重定向文件中读取
字段. 这个 read 命令读取每个文本行之后, 将会退出, 其退出状态为零, 直到到达文件末尾. 到时候, 它的 退
出状态为非零数值, 因此终止循环. 也有可能把标准输入管道到循环中.
#!/bin/bash
# while-read2: read lines from a file
sort -k 1,1 -k 2n distros.txt | while read distro version release; do
printf "Distro: %s\tVersion: %s\tReleased: %s\n" \
$distro \
$version \
$release
done
这里我们接受 sort 命令的标准输出, 然后显示文本流. 然而, 因为管道将会在子 shell 中执行 循环, 当循环终
止的时候, 循环中创建的任意变量或赋值的变量都会消失, 记住这一点很重要.
总结
通过引入循环, 和我们之前遇到的分支, 子例程和序列, 我们已经介绍了程序流程控制的主要类型.  bash 还有
一些锦囊妙计, 但它们都是关于这些基本概念的完善.

拓展阅读

Linux 文档工程中的 Bash 初学者指南一书中介绍了更多的 while 循环实例:
http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_02.html
Wikipedia 中有一篇关于循环的文章, 其是一篇比较长的关于流程控制的文章中的一部分:
http://en.wikipedia.org/wiki/Control_flow#Loops

第三十一章: 疑难排解
随着我们的脚本变得越来越复杂, 当脚本运行错误, 执行结果出人意料的时候, 我们就应该查看一下原因了.  在
这一章中, 我们将会看一些脚本中出现地常见错误类型, 同时还会介绍几个可以跟踪和消除问题的有用技巧.
语法错误
一个普通的错误类型是语法. 语法错误涉及到一些 shell 语法元素的拼写错误. 大多数情况下, 这类错误 会导致
shell 拒绝执行此脚本.
在以下讨论中, 我们将使用下面这个脚本, 来说明常见的错误类型:
#!/bin/bash
# trouble: script to demonstrate common errors
number=1
if [ $number = 1 ]; then
echo "Number is equal to 1."
else
echo "Number is not equal to 1."
fi
参看脚本内容, 我们知道这个脚本执行成功了:
[me@linuxbox ~]$ trouble
Number is equal to 1.
丢失引号
如果我们编辑我们的脚本, 并从跟随第一个 echo 命令的参数中, 删除其末尾的双引号:
#!/bin/bash
# trouble: script to demonstrate common errors
number=1
if [ $number = 1 ]; then
echo "Number is equal to 1.
else
echo "Number is not equal to 1."
fi
观察发生了什么:

[me@linuxbox ~]$ trouble
/home/me/bin/trouble: line 10: unexpected EOF while looking for
matching `"'
/home/me/bin/trouble: line 13: syntax error: unexpected end of file
这个脚本产生了两个错误. 有趣地是, 所报告的行号不是引号被删除的地方, 而是程序中后面的文本行.  我们能
知道为什么, 如果我们跟随丢失引号文本行之后的程序. bash 会继续寻找右引号, 直到它找到一个,  其就是这
个紧随第二个 echo 命令之后的引号. 找到这个引号之后, bash 变得很困惑, 并且 if 命令的语法 被破坏了, 因
为现在这个 fi 语句在一个用引号引起来的(但是开放的)字符串里面.
在冗长的脚本中, 此类错误很难找到. 使用带有语法高亮的编辑器将会帮助查找错误. 如果安装了 vim 的完整
版,  通过输入下面的命令, 可以使语法高亮生效:
:syntax on
丢失或意外的标记
另一个常见错误是忘记补全一个复合命令, 比如说 if 或者是 while. 让我们看一下, 如果 我们删除 if 命令中测试
之后的分号, 会出现什么情况:
#!/bin/bash
# trouble: script to demonstrate common errors
number=1
if [ $number = 1 ] then
echo "Number is equal to 1."
else
echo "Number is not equal to 1."
fi
结果是这样的:
[me@linuxbox ~]$ trouble
/home/me/bin/trouble: line 9: syntax error near unexpected token
`else'
/home/me/bin/trouble: line 9: `else'
再次, 错误信息指向一个错误, 其出现的位置靠后于实际问题所在的文本行. 所发生的事情真是相当有意思. 我
们记得,  if 能够接受一系列命令, 并且会计算列表中最后一个命令的退出代码. 在我们的程序中, 我们打算这个
列表由 单个命令组成, 即 [, 测试的同义词. 这个 [ 命令把它后面的东西看作是一个参数列表. 在我们这种情况

下,  有三个参数:  $number, =, 和 ]. 由于删除了分号, 单词 then 被添加到参数列表中, 从语法上讲,  这是
合法的. 随后的 echo 命令也是合法的. 它被解释为命令列表中的另一个命令, if 将会计算命令的 退出代码. 接
下来遇到单词 else, 但是它出局了, 因为 shell 把它认定为一个 保留字(对于 shell 有特殊含义的单词), 而不
是一个命令名, 因此报告错误信息.
预料不到的展开
可能有这样的错误, 它们仅会间歇性地出现在一个脚本中. 有时候这个脚本执行正常, 其它时间会失败,  这是因
为展开结果造成的. 如果我们归还我们丢掉的分号, 并把 number 的数值更改为一个空变量, 我们 可以示范一
下:
#!/bin/bash
# trouble: script to demonstrate common errors
number=
if [ $number = 1 ]; then
echo "Number is equal to 1."
else
echo "Number is not equal to 1."
fi
运行这个做了修改的脚本, 得到以下输出:
[me@linuxbox ~]$ trouble
/home/me/bin/trouble: line 7: [: =: unary operator expected
Number is not equal to 1.
我们得到一个相当神秘的错误信息, 其后是第二个 echo 命令的输出结果. 这问题是由于 test 命令中 number
变量的展开结果造成的. 当此命令:
[ $number = 1 ]
经过展开之后, number 变为空值, 结果就是这样:
[
= 1 ]
这是无效的, 所以就产生了错误. 这个 = 操作符是一个二元操作符(它要求每边都有一个数值), 但是第一个数
值是缺失的,  这样 test 命令就期望用一个一元操作符(比如 -z)来代替. 进一步说, 因为 test 命令运行失败了
(由于错误),  这个 if 命令接收到一个非零退出代码, 因此执行第二个 echo 命令.
通过为 test 命令中的第一个参数添加双引号, 可以更正这个问题:
[ "$number" = 1 ]
然后当展开操作发生地时候, 执行结果将会是这样:
[ "" = 1 ]
其得到了正确的参数个数. 除了代表空字符串之外, 引号应该被用于这样的场合, 一个要展开 成多单词字符串的
数值, 及其包含嵌入式空格的文件名.
逻辑错误
不同于语法错误, 逻辑错误不会阻止脚本执行. 虽然脚本会正常运行, 但是它不会产生期望的结果,  归咎于脚本
的逻辑问题. 虽然有不计其数的可能的逻辑错误, 但下面是一些在脚本中找到的最常见的 逻辑错误类型:
1. 不正确的条件表达式. 很容易编写一个错误的 if/then/else 语句, 并且执行错误的逻辑.  有时候逻辑会被颠
倒, 或者是逻辑结构不完整.
2. "超出一个值"错误. 当编写带有计数器的循环语句的时候, 为了计数在恰当的点结束, 循环语句 可能要求
从 0 开始计数, 而不是从 1 开始, 这有可能会被忽视. 这些类型的错误要不导致循环计数太多, 而"超出范
围",  要不就是过早的结束了一次迭代, 从而错过了最后一次迭代循环.
3. 意外情况. 大多数逻辑错误来自于程序碰到了程序员没有预见到的数据或者情况. 这也 可以包括出乎意料的
展开, 比如说一个包含嵌入式空格的文件名展开成多个命令参数而不是单个的文件名.
防错编程
当编程的时候, 验证假设非常重要. 这意味着要仔细得计算脚本所使用的程序和命令的退出状态.  这里有个实
例, 基于一个真实的故事. 为了在一台重要的服务器中执行维护任务, 一位不幸的系统管理员写了一个脚本.  这
个脚本包含下面两行代码:
cd $dir_name
rm *
从本质上来说, 这两行代码没有任何问题, 只要是变量 dir_name 中存储的目录名字存在就可以. 但是如果不是
这样会发生什么事情呢? 在那种情况下, cd 命令会运行失败,  脚本会继续执行下一行代码, 将会删除当前工作
目录中的所有文件. 完成不是期望的结果!  由于这种设计策略, 这个倒霉的管理员销毁了服务器中的一个重要部

分.
让我们看一些能够提高这个设计的方法. 首先, 在 cd 命令执行成功之后, 再运行 rm 命令, 可能是明智的选
择.
cd $dir_name && rm *
这样, 如果 cd 命令运行失败后, rm 命令将不会执行. 这样比较好, 但是仍然有可能未设置变量 dir_name 或其
变量值为空, 从而导致删除了用户家目录下面的所有文件. 这个问题也能够避免, 通过检验变量 dir_name 中包
含的目录名是否真正地存在:
[[ -d $dir_name ]] && cd $dir_name && rm *
通常, 当某种情况(比如上述问题)发生的时候, 最好是终止脚本执行, 并对这种情况提示错误信息:
if [[ -d $dir_name ]]; then
if cd $dir_name; then
rm *
else
echo "cannot cd to '$dir_name'" >&2
exit 1
fi
else
echo "no such directory: '$dir_name'" >&2
exit 1
fi
这里, 我们检验了两种情况, 一个名字, 看看它是否为一个真正存在的目录, 另一个是 cd 命令是否执行成功.
如果任一种情况失败, 就会发送一个错误说明信息到标准错误, 然后脚本终止执行, 并用退出状态 1 表明脚本执
行失败.
验证输入
一个良好的编程习惯是如果一个程序可以接受输入数据, 那么这个程序必须能够应对它所接受的任意数据. 这 通
常意味着必须非常仔细地筛选输入数据, 以确保只有有效的输入数据才能被程序用来做进一步地处理. 在前面章
节 中我们学习 read 命令的时候, 我们遇到过一个这样的例子. 一个脚本中包含了下面一条测试语句,  用来验证
一个选择菜单:
[[ $REPLY =~ ^[0-3]$ ]]

这条测试语句非常明确. 只有当用户输入是一个位于 0 到 3 范围内(包括 0 和 3)的数字的时候,  这条语句才
返回一个 0 退出状态. 而其它任何输入概不接受. 有时候编写这类测试条件非常具有挑战性,  但是为了能产出一
个高质量的脚本, 付出还是必要的.
设计是时间的函数
当我还是一名大学生, 在学习工业设计的时候, 一位明智的教授说过一个项目的设计程度是由 给定设计师的
时间量来决定的. 如果给你五分钟来设计一款能够 "杀死苍蝇" 的产品, 你会设计出一个苍蝇拍. 如果给你
五个月的时间, 你可能会制作出激光制导的 "反苍蝇系统".
同样的原理适用于编程. 有时候一个 "快速但粗糙" 的脚本就可以解决问题,  但这个脚本只能被其作者使用
一次. 这类脚本很常见, 为了节省气力也应该被快速地开发出来.  所以这些脚本不需要太多的注释和防错检
查. 相反, 如果一个脚本打算用于生产使用, 也就是说,  某个重要任务或者多个客户会不断地用到它, 此时
这个脚本就需要非常谨慎小心地开发了.
测试
在各类软件开发中(包括脚本), 测试是一个重要的环节. 在开源世界中有一句谚语, "早发布, 常发布", 这
句谚语就反映出这个事实(测试的重要性).  通过提早和经常发布, 软件能够得到更多曝光去使用和测试. 经验
表明如果在开发周期的早期发现 bug, 那么这些 bug 就越容易定位, 而且越能低成本 的修复.
在之前的讨论中, 我们知道了如何使用 stubs 来验证程序流程. 在脚本开发的最初阶段, 它们是一项有价值的技
术 来检测我们的工作进度.
让我们看一下上面的文件删除问题, 为了轻松测试, 看看如何修改这些代码. 测试原本那个代码片段将是危险
的, 因为它的目的是要删除文件,  但是我们可以修改代码, 让测试安全:
if [[ -d $dir_name ]]; then
if cd $dir_name; then
echo rm * # TESTING
else
echo "cannot cd to '$dir_name'" >&2
exit 1
fi
else
echo "no such directory: '$dir_name'" >&2
exit 1
fi
exit # TESTING
因为在满足出错条件的情况下代码可以打印出有用信息, 所以我们没有必要再添加任何额外信息了.  最重要的改

动是仅在 rm 命令之前放置了一个 echo 命令,  为的是把 rm 命令及其展开的参数列表打印出来, 而不是执行实
际的 rm 命令语句. 这个改动可以安全的执行代码.  在这段代码的末尾, 我们放置了一个 exit 命令来结束测试,
从而防止执行脚本其它部分的代码.  这个需求会因脚本的设计不同而变化.
我们也在代码中添加了一些注释, 用来标记与测试相关的改动. 当测试完成之后, 这些注释可以帮助我们找到并
删除所有的更改.
测试案例
为了执行有用的测试, 开发和使用好的测试案例是很重要的. 这个要求可以通过谨慎地选择输入数据或者运行边
缘案例和极端案例来完成.  在我们的代码片段中(是非常简单的代码), 我们想要知道在下面的三种具体情况下
这段代码是怎样执行的:
1. dir_name 包含一个已经存在的目录的名字
2. dir_name 包含一个不存在的目录的名字
3. dir_name 为空
通过执行以上每一个测试条件, 就达到了一个良好的测试覆盖率.
正如设计, 测试也是一个时间的函数. 不是每一个脚本功能都需要做大量的测试. 问题关键是确定什么功能是最
重要的. 因为 测试若发生故障会存在如此潜在的破坏性, 所以我们的代码片在设计和测试段期间都应值得仔细推
敲.
调试
如果测试暴露了脚本中的一个问题, 那下一步就是调试了. "一个问题"通常意味着在某种情况下, 这个脚本的
执行 结果不是程序员所期望的结果. 若是这种情况, 我们需要仔细确认这个脚本实际到底要完成什么任务, 和为
什么要这样做.  有时候查找 bug 要牵涉到许多监测工作. 一个设计良好的脚本会对查找错误有帮助. 设计良好
的脚本应该具备防卫能力,  能够监测异常条件, 并能为用户提供有用的反馈信息.  然而有时候, 出现的问题相
当稀奇, 出人意料, 这时候就需要更多的调试技巧了.
找到问题区域
在一些脚本中, 尤其是一些代码比较长的脚本, 有时候隔离脚本中与出现的问题相关的代码区域对查找问题很有
效.  隔离的代码区域并不总是真正的错误所在, 但是隔离往往可以深入了解实际的错误原因. 可以用来隔离代码
的一项 技巧是"添加注释". 例如, 我们的文件删除代码可以修改成这样, 从而决定注释掉的这部分代码是否导
致了一个错误:
if [[ -d $dir_name ]]; then
if cd $dir_name; then
rm *
else
echo "cannot cd to '$dir_name'" >&2

exit 1
fi
# else
#
echo "no such directory: '$dir_name'" >&2
#
exit 1
fi
通过给脚本中的一个逻辑区块内的每条语句的开头添加一个注释符号, 我们就阻止了这部分代码的执行. 然后可
以再次执行测试,  来看看清除的代码是否影响了错误的行为.
追踪
在一个脚本中, 错误往往是由意想不到的逻辑流导致的. 也就是说, 脚本中的一部分代码或者从未执行, 或是以
错误的顺序,  或在错误的时间给执行了. 为了查看真实的程序流, 我们使用一项叫做追踪(tracing)的技术.
一种追踪方法涉及到在脚本中添加可以显示程序执行位置的提示性信息. 我们可以添加提示信息到我们的代码片
段中:
echo "preparing to delete files" >&2
if [[ -d $dir_name ]]; then
if cd $dir_name; then
echo "deleting files" >&2
rm *
else
echo "cannot cd to '$dir_name'" >&2
exit 1
fi
else
echo "no such directory: '$dir_name'" >&2
exit 1
fi
echo "file deletion complete" >&2
我们把提示信息输出到标准错误输出, 让其从标准输出中分离出来. 我们也没有缩进包含提示信息的语句, 这样
想要删除它们的时候, 能比较容易找到它们.
当这个脚本执行的时候, 就可能看到文件删除操作已经完成了:
[me@linuxbox ~]$ deletion-script
preparing to delete files
deleting files
file deletion complete
[me@linuxbox ~]$

bash 还提供了一种名为追踪的方法, 这种方法可通过 -x 选项和 set 命令加上 -x 选项两种途径实现.  拿我们之
前的 trouble 脚本为例, 给该脚本的第一行语句添加 -x 选项, 我们就能追踪整个脚本.
#!/bin/bash -x
# trouble: script to demonstrate common errors
number=1
if [ $number = 1 ]; then
echo "Number is equal to 1."
else
echo "Number is not equal to 1."
fi
当脚本执行后, 输出结果看起来像这样:
[me@linuxbox ~]$ trouble
+ number=1
+ '[' 1 = 1 ']'
+ echo 'Number is equal to 1.'
Number is equal to 1.
追踪生效后, 我们看到脚本命令展开后才执行. 行首的加号表明追踪的迹象, 使其与常规输出结果区分开来.  加
号是追踪输出的默认字符. 它包含在 PS4(提示符4)shell 变量中. 可以调整这个变量值让提示信息更有意义.
这里, 我们修改该变量的内容, 让其包含脚本中追踪执行到的当前行的行号. 注意这里必须使用单引号是为了防
止变量展开, 直到 提示符真正使用的时候, 就不需要了.
[me@linuxbox ~]$ export PS4='$LINENO + '
[me@linuxbox ~]$ trouble
5 + number=1
7 + '[' 1 = 1 ']'
8 + echo 'Number is equal to 1.'
Number is equal to 1.
我们可以使用 set 命令加上 -x 选项, 为脚本中的一块选择区域, 而不是整个脚本启用追踪.
#!/bin/bash
# trouble: script to demonstrate common errors
number=1
set -x # Turn on tracing

if [ $number = 1 ]; then
echo "Number is equal to 1."
else
echo "Number is not equal to 1."
fi
set +x # Turn off tracing
我们使用 set 命令加上 -x 选项来启动追踪, +x 选项关闭追踪. 这种技术可以用来检查一个有错误的脚本的多个
部分.
执行时检查数值
伴随着追踪, 在脚本执行的时候显示变量的内容, 以此知道脚本内部的工作状态, 往往是很用的.  使用额外的
echo 语句通常会奏效.
#!/bin/bash
# trouble: script to demonstrate common errors
number=1
echo "number=$number" # DEBUG
set -x # Turn on tracing
if [ $number = 1 ]; then
echo "Number is equal to 1."
else
echo "Number is not equal to 1."
fi
set +x # Turn off tracing
在这个简单的示例中, 我们只是显示变量 number 的数值, 并为其添加注释, 随后利于其识别和清除.  当查看
脚本中的循环和算术语句的时候, 这种技术特别有用.
总结
在这一章中, 我们仅仅看了几个在脚本开发期间会出现的问题. 当然, 还有很多. 这章中描述的技术对查找 大多
数的常见错误是有效的. 调试是一种艺术, 可以通过开发经验, 在知道如何避免错误(整个开发过程中不断测试)
以及在查找 bug(有效利用追踪)两方面都会得到提升.
拓展阅读
Wikipedia 上面有两篇关于语法和逻辑错误的短文:
http://en.wikipedia.org/wiki/Syntax_error
http://en.wikipedia.org/wiki/logic_error
网上有很多关于技术层面的 bash 编程的资源:

http://mywiki.wooledge.org/BashPitfalls
http://tldp.org/LDP/abs/html/gotchas.html
http://www.gnu.org/software/bash/manual/html_node/Reserved-Word-Index.html
想要学习从编写良好的 Unix 程序中得知的基本概念, 可以参考 Eric Raymond 的<Unix 编程的艺术>这本
伟大的著作. 书中的许多想法都能适用于 shell 脚本:
http://www.faqs.org/docs/artu/
http://www.faqs.org/docs/artu/ch01s06.html
对于真正的高强度的调试, 参考这个 Bash Debugger:
http://bashdb.sourceforge.net/

## 第三十二章: 流程控制 case分支
在这一章中, 我们将继续看一下程序的流程控制. 在第28章中, 我们构建了一些简单的菜单并创建了用来 应对各
种用户选择的程序逻辑. 为此, 我们使用了一系列的 if 命令来识别哪一个可能的选项已经被选中.  这种类型的构
造经常出现在程序中, 出现频率如此之多, 以至于许多编程语言(包括 shell) 专门为多选决策提供了一种流程
控制机制.
case
Bash 的多选复合命令称为 case. 它的语法规则如下所示:
case word in
[pattern [| pattern]...) commands ;;]...
esac
如果我们看一下第28章中的读菜单程序, 我们就知道了用来应对一个用户选项的逻辑流程:
#!/bin/bash
# read-menu: a menu driven system information program
clear
echo "
Please Select:
1\. Display System Information
2\. Display Disk Space
3\. Display Home Space Utilization
0\. Quit
"
read -p "Enter selection [0-3] > "
if [[ $REPLY =~ ^[0-3]$ ]]; then
if [[ $REPLY == 0 ]]; then
echo "Program terminated."
exit
fi
if [[ $REPLY == 1 ]]; then
echo "Hostname: $HOSTNAME"
uptime
exit
fi
if [[ $REPLY == 2 ]]; then
df -h
exit
fi
if [[ $REPLY == 3 ]]; then
if [[ $(id -u) -eq 0 ]]; then

echo "Home Space Utilization (All Users)"
du -sh /home/*
else
echo "Home Space Utilization ($USER)"
du -sh $HOME
fi
exit
fi
else
echo "Invalid entry." >&2
exit 1
fi
使用 case 语句, 我们可以用更简单的代码替换这种逻辑:
#!/bin/bash
# case-menu: a menu driven system information program
clear
echo "
Please Select:
1\. Display System Information
2\. Display Disk Space
3\. Display Home Space Utilization
0\. Quit
"
read -p "Enter selection [0-3] > "
case $REPLY in
0) echo "Program terminated."
exit
;;
1) echo "Hostname: $HOSTNAME"
uptime
;;
2) df -h
;;
3) if [[ $(id -u) -eq 0 ]]; then
echo "Home Space Utilization (All Users)"
du -sh /home/*
else
echo "Home Space Utilization ($USER)"
du -sh $HOME
fi
;;
*) echo "Invalid entry" >&2
exit 1
;;
esac

case 命令检查一个变量值, 在我们这个例子中, 就是 REPLY 变量的变量值, 然后试图去匹配其中一个具体的模
式.  当与之相匹配的模式找到之后, 就会执行与该模式相关联的命令. 若找到一个模式之后, 就不会再继续寻
找.
模式
这里 case 语句使用的模式和路径展开中使用的那些是一样的. 模式以一个 ")" 为终止符. 这里是一些有效的
模式.
表32-1: case 模式实例

模式 描述
a) 若单词为 "a", 则匹配
[[:alpha:]]) 若单词是一个字母字符, 则匹配
???) 若单词只有3个字符, 则匹配
*.txt) 若单词以 ".txt" 字符结尾, 则匹配
*) 匹配任意单词. 把这个模式做为 case
命令的最后一个模式, 是一个很好的做
法,  可以捕捉到任意一个与先前模式不
匹配的数值; 也就是说, 捕捉到任何可
能的无效值.

这里是一个模式使用实例:
#!/bin/bash
read -p "enter word > "
case $REPLY in
[[:alpha:]])
echo "is a single alphabetic character." ;;
[ABC][0-9])
echo "is A, B, or C followed by a digit." ;;
???)
echo "is three characters long." ;;
*.txt)
echo "is a word ending in '.txt'" ;;
*)
echo "is something else." ;;
esac
还可以使用竖线字符作为分隔符, 把多个模式结合起来. 这就创建了一个 "或" 条件模式. 这对于处理诸如大
小写字符很有用处. 例如:
#!/bin/bash
# case-menu: a menu driven system information program
clear
echo "

Please Select:
A. Display System Information
B. Display Disk Space
C. Display Home Space Utilization
Q. Quit
"
read -p "Enter selection [A, B, C or Q] > "
case $REPLY in
q|Q) echo "Program terminated."
exit
;;
a|A) echo "Hostname: $HOSTNAME"
uptime
;;
b|B) df -h
;;
c|C) if [[ $(id -u) -eq 0 ]]; then
echo "Home Space Utilization (All Users)"
du -sh /home/*
else
echo "Home Space Utilization ($USER)"
du -sh $HOME
fi
;;
*)
echo "Invalid entry" >&2
exit 1
;;
esac
这里, 我们更改了 case-menu 程序的代码, 用字母来代替数字做为菜单选项. 注意新模式如何使得大小写字母
都是有效的输入选项.
执行多个动作
早于版本号4.0的 bash, case 语法只允许执行与一个成功匹配的模式相关联的动作.  匹配成功之后, 命令将会
终止. 这里我们看一个测试一个字符的脚本:
#!/bin/bash
# case4-1: test a character
read -n 1 -p "Type a character > "
echo
case $REPLY in
[[:upper:]])
echo "'$REPLY'
[[:lower:]])
echo "'$REPLY'
[[:alpha:]])
echo "'$REPLY'
[[:digit:]])
echo "'$REPLY'
[[:graph:]])
echo "'$REPLY'
is
is
is
is
is
upper case." ;;
lower case." ;;
alphabetic." ;;
a digit." ;;
a visible character." ;;

[[:punct:]])
[[:space:]])
[[:xdigit:]])
echo "'$REPLY' is a punctuation symbol." ;;
echo "'$REPLY' is a whitespace character." ;;
echo "'$REPLY' is a hexadecimal digit." ;;
esac
运行这个脚本, 输出这些内容:
[me@linuxbox ~]$ case4-1
Type a character > a
'a' is lower case.
大多数情况下这个脚本工作是正常的, 但若输入的字符不止与一个 POSIX 字符集匹配的话, 这时脚本就会出错.
例如, 字符 "a" 既是小写字母, 也是一个十六进制的数字. 早于4.0的 bash, 对于 case 语法绝不能匹配 多个
测试条件. 现在的 bash 版本, 添加 ";;&" 表达式来终止每个行动, 所以现在我们可以做到这一点:
#!/bin/bash
# case4-2: test a character
read -n 1 -p "Type a character > "
echo
case $REPLY in
[[:upper:]])
echo "'$REPLY'
[[:lower:]])
echo "'$REPLY'
[[:alpha:]])
echo "'$REPLY'
[[:digit:]])
echo "'$REPLY'
[[:graph:]])
echo "'$REPLY'
[[:punct:]])
echo "'$REPLY'
[[:space:]])
echo "'$REPLY'
[[:xdigit:]])
echo "'$REPLY'
esac
is
is
is
is
is
is
is
is
upper case." ;;&
lower case." ;;&
alphabetic." ;;&
a digit." ;;&
a visible character." ;;&
a punctuation symbol." ;;&
a whitespace character." ;;&
a hexadecimal digit." ;;&
当我们运行这个脚本的时候, 我们得到这些:
[me@linuxbox ~]$ case4-2
Type a character > a
'a' is lower case.
'a' is alphabetic.
'a' is a visible character.
'a' is a hexadecimal digit.
添加的 ";;&" 的语法允许 case 语句继续执行下一条测试, 而不是简单地终止运行.

总结
case 命令是我们编程技巧口袋中的一个便捷工具. 在下一章中我们将看到,  对于处理某些类型的问题来说,
case 命令是一个完美的工具.
拓展阅读
Bash 参考手册的条件构造一节详尽的介绍了 case 命令:
http://tiswww.case.edu/php/chet/bash/bashref.html#SEC21
高级 Bash 脚本指南提供了更深一层的 case 应用实例:
http://tldp.org/LDP/abs/html/testbranch.html

