# awk-2

[awk命令](https://man.linuxde.net/awk)

`awk`是一种编程语言, 用于在`linux/unix`下对文本和数据进行处理. 数据可以来自标准输入(stdin), 一个或多个文件, 或其它命令的输出.
它支持用户自定义函数和动态正则表达式等先进功能, 是`linux/unix`下的一个强大编程工具.
它在命令行中使用, 但更多是作为脚本来使用.

`awk`有很多内建的功能, 比如数组, 函数等, 这是它和`C`语言的相同之处, 灵活性是`awk`最大的优势.

## awk命令格式和选项

我们可以直接通过命令行的方式为 `AWK` 程序提供 `AWK` 命令, 也可以使用包括 `AWK` 命令的脚本文件.

***
语法形式

```bash
awk [options] 'script' var=value file(s)
awk [options] -f scriptfile var=value file(s)
```

***
常用命令选项

+ `-F fs`   fs指定输入分隔符, `fs`可以是字符串或正则表达式, 如`-F:`
+ `-v var=value`   赋值一个用户定义变量, 将外部变量传递给`awk`
+ `-f scripfile`  从脚本文件中读取`awk`命令
+ `-m[fr] val`   对`val`值设置内在限制, `-mf`选项限制分配给`val`的最大块数目；`-mr`选项限制记录的最大数目. 这两个功能是Bell实验室版awk的扩展功能, 在标准awk中不适用.
+ `--dump-variables[=file] 选项`: 将全局变量及相应值按序输出到指定文件中, 默认的输出文件名是 `awkvars.out`.

***
`--lint[=fatal]` 选项

这个选项用于检查程序的可移植情况以及代码中的可疑部分. 如果提供了参数 `fatal`, AWK 会将所有的警告信息当作错误信息处理. 下面这个简单的示例说明了 `lint` 选项的用法：

`awk --lint '' /bin/ls`

***
`--posix` 选项

这个选项会打开严格 `POSIX` 兼容性审查.  如此, 所有共同的以及 GAWK 特定的扩展将被设置为无效.

***
`--profile[=file]` 选项

这个选项会将程序文件以一种很优美的方式输出(译注：用于格式化 awk 脚本文件). 默认输出文件是 `awkprof.out`. 示例如下：

```bash
awk --profile 'BEGIN{printf"---|Header|--\n"} {print} END{printf"---|Footer|---\n"}' marks.txt > /dev/null
cat awkprof.out
```

***
`--traditional` 选项

此选项用于禁止 GAWK 相关的扩展

### awk模式和操作

awk脚本是由模式和操作组成的.

***
模式

模式可以是以下任意一个：

+ /正则表达式/：使用通配符的扩展集.
+ 关系表达式：使用运算符进行操作, 可以是字符串或数字的比较测试.
+ 模式匹配表达式：用运算符`~`(匹配)和`~!`(不匹配).
+ `BEGIN`语句块, `pattern`语句块, `END`语句块：参见awk的工作原理

***
操作

操作由一个或多个命令, 函数, 表达式组成, 之间由换行符或分号隔开, 并位于大括号内, 主要部分是：

+ 变量或数组赋值
+ 输出命令
+ 内置函数
+ 控制流语句

### awk脚本基本结构

```bash
awk 'BEGIN{ print "start" } pattern{ commands } END{ print "end" }' file
```

一个`awk`脚本通常由：`BEGIN`语句块, 能够使用模式匹配的通用语句块, `END`语句块3部分组成, 这三个部分是可选的.
任意一个部分都可以不出现在脚本中, 脚本通常在单引号或双引号中, 例如：

```bash
awk 'BEGIN{ i=0 } { i++ } END{ print i }' filename
awk "BEGIN{ i=0 } { i++ } END{ print i }" filename
```

### awk的工作原理

```bash
awk 'BEGIN{ commands } pattern{ commands } END{ commands }'
```

+ 第一步：执行`BEGIN{ commands }`语句块中的语句；
+ 第二步：从文件或标准输入(stdin)读取一行, 然后执行`pattern{ commands }`语句块, 它逐行扫描文件, 从第一行到最后一行重复这个过程, 直到文件全部被读取完毕.
+ 第三步：当读至输入流末尾时, 执行`END{ commands }`语句块.

`BEGIN`语句块在`awk`开始从输入流中读取行之前被执行,
这是一个可选的语句块, 比如变量初始化, 打印输出表格的表头等语句通常可以写在`BEGIN`语句块中.

`END`语句块在`awk`从输入流中读取完所有的行之后即被执行,
比如打印所有行的分析结果这类信息汇总都是在`END`语句块中完成, 它也是一个可选语句块.

`pattern`语句块中的通用命令是最重要的部分, 它也是可选的. 如果没有提供`pattern`语句块, 则默认执行`{ print }`, 即打印每一个读取到的行, `awk`读取的每一行都会执行该语句块.

示例

```bash
echo -e "A line 1\nA line 2" | awk 'BEGIN{ print "Start" } { print } END{ print "End" }'
```

当使用不带参数的`print`时, 它就打印当前行, 当`print`的参数是以逗号进行分隔时, 打印时则以空格作为定界符.
在`awk`的`print`语句块中双引号是被当作拼接符使用, 例如：

```bash
echo | awk '{ var1="v1"; var2="v2"; var3="v3"; print var1,var2,var3; }'
v1 v2 v3
```

双引号拼接使用：

```bash
echo | awk '{ var1="v1"; var2="v2"; var3="v3"; print var1"="var2"="var3; }'
v1=v2=v3
```

`{ }`类似一个循环体, 会对文件中的每一行进行迭代,
通常变量初始化语句(如：`i=0`)以及打印文件头部的语句放入`BEGIN`语句块中, 将打印的结果等语句放在`END`语句块中.

### awk内置变量

内置变量, 也就是预定义变量

说明：`[A][N][P][G]`表示第一个支持变量的工具, `[A]=awk`, `[N]=nawk`, `[P]=POSIXawk`, `[G]=gawk`

`$n`: 当前记录的第`n`个字段, 比如`n`为`1`表示第一个字段, `n`为`2`表示第二个字段.
`$0`: 这个变量包含执行过程中当前行的文本内容.
`ARGC`: (`[N]` ) 命令行参数的数目.
`ARGIND`: (`[G]` ) 命令行中当前文件的位置(从0开始算).
`ARGV`: (`[N]` ) 包含命令行参数的数组.
`CONVFMT`: (`[G]` ) 数字转换格式(默认值为%.6g).
`ENVIRON`: (`[P]` ) 环境变量关联数组.
`ERRNO`: (`[N]` ) 最后一个系统错误的描述.
`FIELDWIDTHS`: (`[G]` ) 字段宽度列表(用空格键分隔).
`FILENAME`: (`[A]` ) 当前输入文件的名.
`FNR`: (`[P]` ) 同NR, 但相对于当前文件.
`FS`: (`[A]` ) 字段分隔符(默认是任何空格).
`IGNORECASE`: (`[G]` ) 如果为真, 则进行忽略大小写的匹配.
`NF`: (`[A]` ) 表示字段数, 在执行过程中对应于当前的字段数.
`NR`: (`[A]` ) 表示记录数, 在执行过程中对应于当前的行号.
`OFMT`: (`[A]` ) 数字的输出格式(默认值是%.6g).
`OFS`: (`[A]` ) 输出字段分隔符(默认值是一个空格).
`ORS`: (`[A]` ) 输出记录分隔符(默认值是一个换行符).
`RS`: (`[A]` ) 记录分隔符(默认是一个换行符).
`RSTART`: (`[N]` ) 由match函数所匹配的字符串的第一个位置.
`RLENGTH`: (`[N]` ) 由match函数所匹配的字符串的长度.
`SUBSEP`: (`[N]` ) 数组下标分隔符(默认值是34).

### 内建变量示例

***
简单查询

```bash
echo -e "line1 f2 f3\nline2 f4 f5\nline3 f6 f7" | awk '{print "行数:"NR, "字段总数:"NF, "字段0="$0, "字段1="$1, "字段2="$2, "字段3="$3}'
```

使用`print $NF`可以打印出一行中的最后一个字段, 使用`$(NF-1)`则是打印倒数第二个字段, 其他以此类推：

```bash
echo -e "line1 f2 f3\n line2 f4 f5" | awk '{print $NF}'
echo -e "line1 f2 f3\n line2 f4 f5" | awk '{print $(NF-1)}'
```

打印每一行的第二和第三个字段：

```bash
awk '{ print $2,$3 }' filename
```

统计文件中的行数：

```bash
awk 'END{ print NR }' filename
```

以上命令只使用了`END`语句块, 在读入每一行的时, `awk`会将`NR`更新为对应的行号,
当到达最后一行`NR`的值就是最后一行的行号, 所以`END`语句块中的`NR`就是文件的行数.

一个每一行中第一个字段值累加的例子：

```bash
seq 5 | awk 'BEGIN{ sum=0; print "总和：" } { print $1"+"; sum+=$1 } END{ print "等于"; print sum }'
```

`seq` - print a sequence of numbers

### 将外部变量值传递给awk

借助`-v`选项, 可以将外部值(并非来自`stdin`)传递给`awk`：

```bash
VAR=10000
echo | awk -v VARIABLE=$VAR '{ print VARIABLE }'
```

另一种传递外部变量方法：

```bash
var1="aaa"
var2="bbb"
echo | awk '{ print v1,v2 }' v1=$var1 v2=$var2
```

当输入来自于文件时使用：

`awk '{ print v1,v2 }' v1=$var1 v2=$var2 filename`

以上方法中, 变量之间用空格分隔作为`awk`的命令行参数跟随在`BEGIN`, `{}`和`END`语句块之后.

## awk运算与判断

作为一种程序设计语言所应具有的特点之一, `awk`支持多种运算, 这些运算与C语言提供的基本相同.
`awk`还提供了一系列内置的运算函数(如`log`, `sqr`, `cos`, `sin`等)和一些用于对字符串进行操作(运算)的函数(如`length`, `substr`等等).

这些函数的引用大大的提高了`awk`的运算功能. 作为对条件转移指令的一部分, 关系判断是每种程序设计语言都具备的功能, `awk`也不例外, `awk`中允许进行多种测试, 作为样式匹配, 还提供了模式匹配表达式`~`(匹配)和`~!`(不匹配).
作为对测试的一种扩充, `awk`也支持用逻辑运算符.

***
算术运算符

+ `+ -`  加, 减
+ `* / &`  乘, 除与求余
+ `+ - !`  一元加, 减和逻辑非
+ `^ ***`  求幂
+ `++ --`  增加或减少, 作为前缀或后缀

例：

```bash
awk 'BEGIN{a="b";print a++,++a;}'
```

注意：所有用作算术运算符进行操作, 操作数自动转为数值, 所有非数值都变为0

### 赋值运算符

`= += -= *= /= %= ^= **=  赋值语句`

例：

`a+=5; 等价于：a=a+5; 其它同类`

### 逻辑运算符

+ `||`  逻辑或
+ `&& ` 逻辑与

例：

```bash
awk 'BEGIN{a=1;b=2;print (a>5 && b<=2),(a>5 || b<=2);}'
```

### 正则运算符

`~ ~!`  匹配正则表达式和不匹配正则表达式

例：

```bash
awk 'BEGIN{a="100testa";if(a ~ /^100*/){print "ok";}}'
ok
```

### 关系运算符

`< <= > >= != ==`  关系运算符

例：

```bash
awk 'BEGIN{a=11;if(a >= 9){print "ok";}}'
ok
```

注意：`>`,` < `可以作为字符串比较, 也可以用作数值比较, 关键看操作数(operand).
如果是字符串就会转换为字符串比较, 两个都为数字才转为数值比较. 字符串比较：按照`ASCII`码顺序比较.

### 其它运算符

+ `$`  字段引用
+ `空格`  字符串连接符
+ `?:`  C条件表达式
+ `in`  **数组中**是否存在某**键值**

例：

```bash
awk 'BEGIN{a="b";print a=="b"?"ok":"err";}'
awk 'BEGIN{a="b";arr[0]="b";arr[1]="c";print (a in arr);}'
awk 'BEGIN{a="b";arr[0]="b";arr["b"]="c";print (a in arr);}'
```

### 运算级优先级表

级别越高越优先
级别越高越优先

## awk高级输入输出

### 读取下一条记录

`awk`中`next`语句使用：在循环逐行匹配, 如果遇到`next`, 就会跳过当前行, 直接忽略下面语句. 而进行下一行匹配.
`next`语句一般用于多行合并：

```bash
cat text.txt
a
b
c
d
e
awk 'NR%2==1{next}{print NR,$0;}' text.txt
```

当记录行号除以`2`余`1`, 就跳过当前行. 下面的`print NR,$0`也不会执行.
下一行开始, 程序有开始判断`NR%2`值. 这个时候记录行号是：`2` , 就会执行下面语句块：`printNR,$0`

***
例子

将包含有`web`的行与下面的行合并：

```bash
cat text.txt
web01[192.168.2.100]
httpd            ok
tomcat               ok
sendmail               ok
web02[192.168.2.101]
httpd            ok
postfix               ok
web03[192.168.2.102]
mysqld            ok
httpd               ok
```

```bash
awk '/^web/{T=$0;next;}{print T":\t"$0;}' marks.txt
```

### 简单地读取一条记录

`awk` `getline`用法：输出重定向需用到`getline`函数.

`getline`从标准输入, 管道或者当前正在处理的文件之外的其他输入文件获得输入.
它负责从输入获得下一行的内容, 并给`NF`,`NR`和`FNR`等内建变量赋值.
如果得到一条记录, `getline`函数返回`1`, 如果到达文件的末尾就返回`0`, 如果出现错误, 例如打开文件失败, 就返回`-1`.

`getline`语法：`getline var`, 变量`var`包含了特定行的内容.

awk `getline`从整体上来说, 用法说明：

当其左右无重定向符`|`或`<`时：`getline`作用于当前文件, 读入当前文件的第一行给其后跟的变量`var`或`$0`(无变量时候),
应该注意到, 由于`awk`在处理`getline`之前已经读入了一行, 所以`getline`得到的返回结果是隔行的.

当其左右有重定向符`|`或`<`时：`getline`则作用于定向输入文件,
由于该文件是刚打开, 并没有被`awk`读入一行, 只是`getline`读入, 那么`getline`返回的是该文件的第一行, 而不是隔行.

示例：

执行`linux`的`date`命令, 并通过管道输出给`getline`, 然后再把输出赋值给自定义变量`out`, 并打印它：

```bash
awk 'BEGIN{ "date" | getline out; print out }' test
```

执行`shell`的`date`命令, 并通过管道输出给`getline`, 然后`getline`从管道中读取并将输入赋值给`out`, `split`函数把变量`out`转化成数组`mon`, 然后打印数组`mon`的第二个元素：

```bash
awk 'BEGIN{ "date" | getline out; split(out,mon); print mon[2] }' test
```

命令`ls`的输出传递给`geline`作为输入, 循环使`getline`从`ls`的输出中读取一行, 并把它打印到屏幕.
这里没有输入文件, 因为`BEGIN`块在打开输入文件前执行, 所以可以忽略输入文件.

```bash
awk 'BEGIN{ while( "ls" | getline) print }'
```

### 关闭文件

`awk`中允许在程序中关闭一个**输入或输出**文件, 方法是使用`awk`的`close`语句.

`close("filename")`

`filename`可以是`getline`打开的文件, 也可以是`stdin`, 包含文件名的变量, 或者`getline`使用的确切命令.
或一个输出文件, 可以是`stdout`, 包含文件名的变量或使用管道的确切命令.

### 输出到一个文件

`awk`中允许用如下方式将结果输出到一个文件：

`echo | awk '{printf("hello word!n") > "datafile"}'`
或
`echo | awk '{printf("hello word!n") >> "datafile"}'`

## 设置字段定界符

默认的字段定界符是空格, 可以使用`-F "定界符"` 明确指定一个定界符：

`awk -F: '{ print $NF }' /etc/passwd`
或
`awk 'BEGIN{ FS=":" } { print $NF }' /etc/passwd`

在`BEGIN`语句块中则可以用`OFS=“定界符”`设置输出字段的定界符.

## 流程控制语句

`linux awk`的`while`, `do-while`和`for`语句允许使用`break`,`continue`语句来控制流程走向, 也允许使用`exit`这样的语句来退出.

`break`中断当前正在执行的循环并跳到循环外执行下一条语句. `if `是流程选择用法.
`awk`中, 流程控制语句, 语法结构, 与c语言类似. 有了这些语句, 其实很多`shell`程序都可以交给`awk`, 而且性能是非常快的.
下面是各个语句用法.

### 条件判断语句

```bash
if(表达式)
  {语句1}
else
  {语句2}
```

格式中`语句1`可以是多个语句, 为了方便判断和阅读, 最好将多个语句用`{}`括起来. `awk`分枝结构允许嵌套, 其格式为：

```bash
if(表达式)
  {语句1}
else if(表达式)
  {语句2}
else
  {语句3}
```

示例：

```bash
awk 'BEGIN{
test=100;
if(test>90){
  print "very good";
  }
  else if(test>60){
    print "good";
  }
  else{
    print "no pass";
  }
}'
```

每条命令语句后面可以用`;`分号结尾.

### 循环语句

`while`语句

```bash
while(表达式)
  {语句}
```

示例：

```bash
awk 'BEGIN{
test=100;
total=0;
while(i<=test){
  total+=i;
  i++;
}
print total;
}'
```

***
`for`循环

`for`循环有两种格式：

格式1：

```bash
for(变量 in 数组)
  {语句}
```

示例：

```bash
awk 'BEGIN{
for(k in ENVIRON){
  print k"="ENVIRON[k];
}
}'
```

注：`ENVIRON`是`awk`常量, 是字典型数组.

格式2：

```bash
for(变量;条件;表达式)
  {语句}
```

示例：

```bash
awk 'BEGIN{
total=0;
for(i=0;i<=100;i++){
  total+=i;
}
print total;
}'
```

***
`do`循环

```bash
do
{语句} while(条件)
```

例子：

```bash
awk 'BEGIN{
total=0;
i=0;
do {total+=i;i++;} while(i<=100)
  print total;
}'
```

### 其他语句

+ `break` 当 `break` 语句用于 `while` 或 `for` 语句时, 导致退出程序循环.
+ `continue` 当 `continue` 语句用于 `while` 或 `for` 语句时, 使程序循环移动到下一个迭代.
+ `next` 能能够导致读入下一个输入行, 并返回到脚本的顶部. 这可以避免对当前输入行执行其他的操作过程.
+ `exit` 语句使主输入循环退出并将控制转移到`END`,如果`END`存在的话. 如果没有定义`END`规则, 或在`END`中应用`exit`语句, 则终止脚本的执行.

## 数组应用

数组是`awk`的灵魂, 处理文本中最不能少的就是它的数组处理.
`awk`的数组索引(下标)可以是**数字**或者**字符串**, 所以`awk`的数组叫做关联数组(associative arrays).
`awk` 中的数组不必提前声明, 也不必声明大小. 数组元素用`0`或**空字符串**来初始化, 这根据上下文而定.

### 数组的定义

数字做数组索引(下标)：

```bash
Array[1]="sun"
Array[2]="kai"
```

字符串做数组索引(下标)：

```bash
Array["first"]="www"
Array["last"]="name"
Array["birth"]="1987"
```

使用中`print Array[1]`会打印出`sun`；使用`print Array[2]`会打印出`kai`；使用`print["birth"]`会得到`1987`.

读取数组的值

```bash
{ for(item in array) {print array[item]}; }       #输出的顺序是随机的
{ for(i=1;i<=len;i++) {print array[i]}; }         #Len是数组的长度
```

### 数组相关函数

得到数组长度：

```bash
awk 'BEGIN{info="it is a test";lens=split(info,tA," ");print length(tA),"length is:"lens;}'
```

`length`返回字符串以及数组长度, `split`进行分割字符串为数组, 也会返回分割得到数组长度.

```bash
awk 'BEGIN{info="it is a test";split(info,tA," ");print asort(tA);for (k in tA){print k,tA[k];}}'
```

`asort`对数组进行排序, 返回数组长度.

输出数组内容(无序, 有序输出)：

```bash
awk 'BEGIN{info="it is a test";split(info,tA," ");for(k in tA){print k,tA[k];}}'
```

`for…in`输出, 因为数组是关联数组, 默认是无序的. 所以通过`for…in`得到是无序的数组. 如果需要得到有序数组, 需要通过下标获得.

```bash
awk 'BEGIN{info="it is a test";tlen=split(info,tA," ");for(k=1;k<=tlen;k++){print k,tA[k];}}'
```

注意：数组下标是从`1`开始, 与`C`数组不一样.

判断键值存在以及删除键值：

```bash
#错误的判断方法：
awk 'BEGIN{tB["a"]="a1";tB["b"]="b1";if(tB["c"]!="1"){print "no found";};for(k in tB){print k,tB[k];}}'
no found
a a1
b b1
c
```

以上出现奇怪问题, `tB["c"]`没有定义, 但是循环时候, 发现已经存在该键值, 它的值为空.
这里需要注意, `awk`数组是关联数组, 只要通过数组引用它的`key`, 就会自动创建改序列.

```bash
#正确判断方法：
awk 'BEGIN{tB["a"]="a1";tB["b"]="b1";if( "c" in tB){print "ok";};for(k in tB){print k,tB[k];}}'
a a1
b b1
```

`if(key in array)`通过这种方法判断数组中是否包含`key`键值.

```bash
#删除键值：
[chengmo@localhost ~]$ awk 'BEGIN{tB["a"]="a1";tB["b"]="b1";delete tB["a"];for(k in tB){print k,tB[k];}}'
b b1
```

`delete array[key]`可以删除, 对应数组`key`的, 序列值.

### 二维, 多维数组使用

`awk`的多维数组在本质上是一维数组, 更确切一点, `awk`在存储上并不支持多维数组. `awk`提供了逻辑上模拟二维数组的访问方式.
例如, `array[2,4]=1`这样的访问是允许的. `awk`使用一个特殊的字符串`SUBSEP`作为分割字段, 在上面的例子中, 关联数组array存储的键值实际上是`2SUBSEP4`.

类似一维数组的成员测试, 多维数组可以使用`if ( (i,j) in array)`这样的语法, 但是下标必须放置在圆括号中.

```bash
awk 'BEGIN{
for(i=1;i<=4;i++){
  for(j=1;j<=4;j++){
    tarr[i,j]=i*j;
  }
}
if ((4,5) in tarr)
{print tarr[4,5]}
else
{print "does not exist"}

if ((4,4) in tarr)
{print tarr[4,4]}
else
{print "does not exist"}
}'

类似一维数组的循环访问, 多维数组使用`for ( item in array )`这样的语法遍历数组.

```bash
awk 'BEGIN{
for(i=1;i<=4;i++){
  for(j=1;j<=4;j++){
    tarr[i,j]=i*j;
  }
}
for(m in tarr){
  print m,tarr[m];
}
}'
```

与一维数组不同的是, 多维数组必须使用`split()`函数来访问单独的下标分量.

```bash
awk 'BEGIN{
for(i=1;i<=9;i++){
  for(j=1;j<=9;j++){
    tarr[i,j]=i*j; print i,"*",j,"=",tarr[i,j];
  }
}
}'
```

可以通过`array[k,k2]`引用获得数组内容.

另一种方法：

```bash
awk 'BEGIN{
for(i=1;i<=9;i++){
  for(j=1;j<=9;j++){
    tarr[i,j]=i*j;
  }
}
for(m in tarr){
  split(m,tarr2,SUBSEP); print tarr2[1],"*",tarr2[2],"=",tarr[m];
}
}'
```

## 内置函数

`awk`内置函数, 主要分以下`3`种类似：算数函数, 字符串函数, 其它一般函数, 时间函数.

### 算术函数

+ `atan2(y, x)`  返回 y/x 的反正切.
+ `cos(x)`  返回 `x` 的余弦；`x` 是弧度.
+ `sin(x)`  返回 `x` 的正弦；`x` 是弧度.
+ `exp(x)`  返回 `x` 幂函数.
+ `log(x)`  返回 `x` 的自然对数.
+ `sqrt(x)`  返回 `x` 平方根.
+ `int(x)`  返回 `x` 的截断至整数的值.
+ `rand( )`  返回任意数字 `n`, 其中 `0 <= n < 1`.
+ `srand( [expr])`  将 `rand` 函数的种子值设置为 `Expr` 参数的值, 或如果省略 `Expr` 参数则使用某天的时间. 返回先前的种子值.

举例说明：

```bash
awk 'BEGIN{OFMT="%.3f";fs=sin(1);fe=exp(10);fl=log(10);fi=int(3.1415);print fs,fe,fl,fi;}'
0.841 22026.466 2.303 3
```

`OFMT` 设置输出数据格式是保留`3`位小数.

获得随机数：

```bash
awk 'BEGIN{srand();fr=int(100*rand());print fr;}'
awk 'BEGIN{srand();fr=int(100*rand());print fr;}'
awk 'BEGIN{srand();fr=int(100*rand());print fr;}'
```

### 字符串函数

+ `gsub(Ere, Repl, [In] )`  和`sub`函数类似, 只不过进行所有可能的替换.
+ `sub(Ere, Repl, [In] )`  匹配`In`中由 `Ere` 指定的字符串(扩展正则表达式), 并用 `Repl`参数替换, 只替换第一个具体值. `sub`函数返回替换的数量. 用`&`来进行匹配结果的引用. 如果未指定 `In` 参数, 缺省值是整个记录(`$0` 记录变量).
+ `index(str1,str2)`  返回`str2`在`str1`中的位置, 从 1 开始编号. 如果 `str2`参数不在`str1`中出现, 则返回`0`(零).
+ `length [(str)]`  返回 `str` 参数指定的字符串的长度(字符形式). 如果未给出 `str`, 则返回整个记录的长度(`$0`的长度).
+ `blength [(str)]`  返回 `str` 参数指定的字符串的长度(以**字节**为单位). 如果未给出 `str` 参数, 则返回整个记录的长度(`$0`的长度).
+ `substr(str,M,[N])`  返回`str`中长度为`N`的字符子串. 子串从 `M`指定的位置开始.  `str` 中的第一个字符编号为 `1`. 如果未指定 `N` 参数, 则默认取到 `str` 的末尾.
+ `match(str,Ere)`  返回 `str`中`Ere`的位置(字符形式), 从`1` 开始编号. 如果 Ere 参数不出现, 则返回 `0`. `RSTART` 特殊变量记录返回值. `RLENGTH` 特殊变量记录匹配字符串的长度, 或如果未找到任何匹配, 则值为 `-1`.
+ `split(str,A,[Ere])`  将 `str` 分割为数组 `A[1]`, `A[2]`, `. . .`, `A[n]`, 并返回`n`(数组的长度). 分隔符为`Ere`指定的扩展正则表达式. 如果没有给出 `Ere` 参数, 则为当前字段分隔符(`FS` 特殊变量).
除非上下文指明特定的元素为数字值, 否则 `A` 中的元素为字符串.
+ `tolower(str)`  返回 `str` 的小写形式, 大写和小写的映射由当前语言环境的 `LC_CTYPE` 范畴定义.
+ `toupper(str)`  返回 `str` 的大写形式, 大写和小写的映射由当前语言环境的`LC_CTYPE` 范畴定义.
+ `sprintf(Format, Expr, Expr, . . . )`  根据 `Format` 参数指定的 `printf` 格式字输出 `Expr` 参数指定的表达式, 并返回最后生成的字符串.

注：`Ere`都可以是正则表达式.

***
`gsub`,`sub`

```bash
awk 'BEGIN{info="this is a test2010test!";gsub(/[0-9]+/,"AAA",info);print info}'
```

在`info`中查找`/[0-9]+/ `,并用`"AAA"`替换, 并将替换后的值, 赋给`info`.
如果未给出`info`参数, 则默认为`$0`.

***
查找字符串`index`

```bash
awk 'BEGIN{info="this is a test2010test!";print index(info,"test")?"ok":"no found";}'
## or
awk 'BEGIN{info="this is a test2010test!";
if(index(info,"test"))
{print "Ok";}
else{print "not found";}
}'
## or
awk 'BEGIN{info="this is a test2010test!";
if("test" in info)
{print "Ok";}
else{print "not found";}
}'
```

***
正则表达式匹配查找`match`

```bash
awk 'BEGIN{info="this is a test2010test!";print match(info,/[0-9]+/)?"ok":"no found";}'
```

***
截取字符串`substr`

```bash
awk 'BEGIN{info="this is a test2010test!";print substr(info,4,10);}'

```

从第 4个 字符开始, 截取10个长度字符串

***
字符串分割`split`

awk 'BEGIN{info="this is a test";split(info,tA," ");print length(tA);for(k in tA){print k,tA[k];}}'

分割`info`, 动态创建数组`tA`. `awk`中的`for …in`循环, 是一个无序的循环.
并不是按照数组下标`1…n`循环 , 因此使用时候需要注意.

***
格式化字符串输出`sprintf`

格式化的字符串包括两部分内容(内容和格式)：
一部分是正常字符, 这些字符将按原样输出;
另一部分是格式控制字符, 以`"%"`开始, 后跟一个或几个规定字符,用来确定输出内容格式.

格式  描述

+ `%d`  十进制有符号整数
+ `%u`  十进制无符号整数
+ `%f`  浮点数
+ `%s`  字符串
+ `%c`  单个字符
+ `%p`  指针的值
+ `%e`  指数形式的浮点数
+ `%x`  `%X` 无符号以十六进制表示的整数
+ `%o`  无符号以八进制表示的整数
+ `%g`  自动选择合适的表示法

```bash
awk 'BEGIN{n1=124.113;n2=-1.224;n3=1.2345; printf("%.2f,%.2u,%.2g,%X,%o\n",n1,n2,n3,n1,n1);}'
```

### 一般函数

格式  描述

+ `close(Expression)`  用同一个 `Expression`参数(值为字符串)来关闭文件或管道. 它们由 `print`或`printf` 语句或`getline` 函数打开.
如果文件或管道成功关闭, 则返回`0`；其它情况下返回非零值.
如果打算写一个文件, 并稍后在同一个程序中读取文件, 则`close`语句是必需的.
+ `system(command)`  执行 `Command` 参数指定的命令, 并返回退出状态. 等同于 `system` 子例程.
+ `Expression | getline [Variable]`  将 `Expression`的值当作命令执行, 然后从管道传送的流中读取一个输入记录, 并将该记录的值赋给`Variable`. 如果当前不存在执行`Expression`得到的流, 则创建一个.
创建的流等同于调用 `popen` 子例程, 此时 `Command` 参数取 `Expression` 的值且 `Mode` 为`r`.
只要流保留打开且`Expression`不变, 则`getline`函数继续读取下一个记录. 如果未指定 `Variable` 参数, 则使用 `$0` 和`NF`存储记录.
+ `getline [Variable] < Expression` 从`Expression`指定的文件读取下一个记录, 并将 `Variable`设置为该记录的值. 只要流保留打开且`Expression`的值不变, 则`getline`函数继续往下读取记录.
如果未指定 `Variable` 参数, 则使用 `$0` 和`NF`存储记录.
+ `getline [Variable]`  将 `Variable` 设置为下一个输入记录. 如果未指定 `Variable` 参数, 则使用`$0`,`NF`, `NR` 和 `FNR` 特殊变量.

### 打开外部文件(close用法)

```bash
awk 'BEGIN{while("cat /etc/passwd"|getline){print $0;};close("/etc/passwd");}'
```

逐行读取外部文件

```bash
awk 'BEGIN{while(getline < "/etc/passwd"){print $0;};close("/etc/passwd");}'
```

```bash
awk 'BEGIN{print "Enter your name:";getline name;print name;}'
```

调用外部应用程序

```bash
awk 'BEGIN{b=system("ls -al");print b;}'
```

### 时间函数

函数名  说明

+ `mktime( YYYY MM dd HH MM ss[ DST])` 生成时间格式
+ `strftime([format [, timestamp]])` 格式化时间输出, 将时间戳转为时间字符串, 具体格式见下表.
+ `systime()`  得到时间戳,返回从`1970年1月1日`开始到当前时间(不计闰年)的整秒数

建指定时间(mktime使用)

```bash
awk 'BEGIN{tstamp=mktime("2001 01 01 12 12 12");print strftime("%c",tstamp);}'
```

```bash
awk 'BEGIN{tstamp1=mktime("2001 01 01 12 12 12");tstamp2=mktime("2001 02 01 0 0 0");print tstamp2-tstamp1;}'
```

***
strftime日期和时间格式说明符

格式  描述

+ `%a`  星期几的缩写(Sun)
+ `%A`  星期几的完整写法(Sunday)
+ `%b`  月名的缩写(Oct)
+ `%B`  月名的完整写法(October)
+ `%c`  本地日期和时间
+ `%d`  十进制日期
+ `%D`  日期 `08/20/99`
+ `%e`  日期, 如果只有一位会补上一个空格
+ `%H`  用十进制表示24小时格式的小时
+ `%I`  用十进制表示12小时格式的小时
+ `%j`  从`1`月`1`日起一年中的第几天
+ `%m`  十进制表示的月份
+ `%M`  十进制表示的分钟
+ `%p`  12小时表示法(AM/PM)
+ `%S`  十进制表示的秒
+ `%U`  十进制表示的一年中的第几个星期(星期天作为一个星期的开始)
+ `%w`  十进制表示的星期几(星期天是0)
+ `%W`  十进制表示的一年中的第几个星期(星期一作为一个星期的开始)
+ `%x`  重新设置本地日期(08/20/99)
+ `%X`  重新设置本地时间(12：00：00)
+ `%y`  两位数字表示的年(99)
+ `%Y`  当前月份
+ `%Z`  时区(PDT)
+ `%%`  百分号(%)

## 一些示例

### 分隔文件

下面这个例子, 是按第`6`例分隔文件, 相当的简单(其中的`NR!=1`表示不处理表头).

`awk 'NR!=1{print > $6}' netstat.txt`

你也可以把指定的列输出到文件：

`awk 'NR!=1{print $4,$5 > $6}' netstat.txt`

再复杂一点：(注意其中的`if-else-if`语句, 可见`awk`其实是个脚本解释器)

```bash
$ awk 'NR!=1{if($6 ~ /TIME|ESTABLISHED/) print > "1.txt";
else if($6 ~ /LISTEN/) print > "2.txt";
else print > "3.txt" }' netstat.txt
```

### 统计

下面的命令计算所有的`C`文件, `CPP`文件和`H`文件的文件大小总和.

```bash
$ ls -l  *.cpp *.c *.h | awk '{sum+=$5} END {print sum}'
```

***
注：如果你要指定多个分隔符, 你可以这样来：

awk -F '[;:]'

***
如果我们需要表头的话, 我们可以引入内建变量NR：

```bash
awk '$3==0 && $6=="LISTEN" || NR==1 ' netstat.txt
```

### 环境变量

即然说到了脚本, 我们来看看怎么和环境变量交互：(使用`-v`参数和`ENVIRON`, 使用`ENVIRON`的环境变量需要`export`)

```bash
$ x=5
$ y=10
$ export y
$ echo $x $y
5 10
$ awk -v val=$x '{print $1, $2, $3, $4+val, $5+ENVIRON["y"]}' OFS="\t" score.txt
```

### 几个花活

[AWK 简明教程](https://coolshell.cn/articles/9070.html)

```bash
#从file文件中找出长度大于80的行
awk 'length>80' file
#按连接数查看客户端IP
netstat -ntu | awk '{print $5}' | cut -d: -f1 | sort | uniq -c | sort -nr
#打印99乘法表
seq 9 | sed 'H;g' | awk -v RS='' '{for(i=1;i<=NF;i++)printf("%dx%d=%d%s", i, NR, i*NR, i==NR?"\n":"\t")}'
```
