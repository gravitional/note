# bash 语法

## shell 变量

[Shell变量:Shell变量的定义,赋值和删除](http://c.biancheng.net/view/743.html)

脚本语言在定义变量时通常不需要指明类型,直接赋值就可以,`Shell` 变量也遵循这个规则.

在 `Bash shell` 中,每一个变量的值都是**字符串**,无论你给变量赋值时有没有使用引号,值都会以字符串的形式存储.

这意味着,`Bash shell` 在默认情况下不会区分变量类型,即使你将整数和小数赋值给变量,它们也会被视为字符串,这一点和大部分的编程语言不同.

例如在C语言或者 C++ 中,变量分为整数,小数,字符串,布尔等多种类型.

当然,如果有必要,你也可以使用 `Shell declare` 关键字显式定义变量的类型,但在一般情况下没有这个需求,Shell 开发者在编写代码时自行注意值的类型即可.

### 定义变量

`Shell` 支持以下三种定义变量的方式:

```bash
variable=value
variable='value'
variable="value"
```

`variable` 是变量名,`value` 是赋给变量的值.如果 `value` 不包含任何空白符(例如空格,`Tab` 缩进等),那么可以不使用引号;
如果 `value` 包含了空白符,那么就必须使用引号包围起来.使用单引号和使用双引号也是有区别的,稍后我们会详细说明.

注意,赋值号`=`的周围不能有空格,这可能和你熟悉的大部分编程语言都不一样.

`Shell` 变量的命名规范和大部分编程语言都一样:

+ 变量名由数字,字母,下划线组成;
+ 必须以字母或者下划线开头;
+ 不能使用 `Shell` 里的关键字(通过 `help` 命令可以查看保留关键字).

变量定义举例:

```bash
url=http://c.biancheng.net/shell/
echo $url
name='C语言中文网'
echo $name
author="严长生"
echo $author
```

### 使用变量

使用一个定义过的变量,只要在变量名前面加美元符号`$`即可,如:

```bash
author="严长生"
echo $author
echo ${author}
```

变量名外面的花括号`{ }`是可选的,加不加都行,
加花括号是为了帮助解释器识别变量的边界,比如下面这种情况:

```bash
skill="Java"
echo "I am good at ${skill}Script"
```

如果不给 `skill` 变量加花括号,写成`echo "I am good at $skillScript"`,解释器就会把 `$skillScript` 当成一个变量(其值为空),代码执行结果就不是我们期望的样子了.

推荐给所有变量加上花括号`{ }`,这是个良好的编程习惯.

### 修改变量的值

已定义的变量,可以被重新赋值,如:

```bash
url="http://c.biancheng.net"
echo ${url}
url="http://c.biancheng.net/shell/"
echo ${url}
```

第二次对变量赋值时不能在变量名前加`$`,只有在使用变量时才能加`$`.

### 单引号和双引号的区别

定义变量时,变量的值可以由单引号`' '`包围,也可以由双引号`" "`包围,
它们的区别以下面的代码为例来说明:

``` bash
#!/bin/bash
url="http://c.biancheng.net"
website1='C语言中文网:${url}'
website2="C语言中文网:${url}"
echo $website1
echo $website2
运行结果:
C语言中文网:${url}
C语言中文网:http://c.biancheng.net
```

以单引号`' '`包围变量的值时,单引号里面是什么就输出什么,即使内容中有变量和命令(命令需要`反引`起来)也会把它们原样输出.
这种方式比较适合定义显示纯字符串的情况,即不希望解析变量,命令等的场景.

以双引号`" "`包围变量的值时,输出时会先解析里面的变量和命令,而不是把双引号中的变量名和命令原样输出.
这种方式比较适合字符串中附带有变量和命令并且想将其解析后再输出的变量定义.

我的建议:
如果变量的内容是数字,那么可以不加引号;
如果真的需要原样输出就加单引号;
其他没有特别要求的字符串等最好都加上双引号,定义变量时加双引号是最常见的使用场景.

### 将命令的结果赋值给变量

`Shell` 也支持将命令的执行结果赋值给变量,常见的有以下两种方式:

```bash
variable=`command`
variable=$(command)
```

第一种方式把命令用反引号` `(位于 `Esc` 键的下方)包围起来,反引号和单引号非常相似,容易产生混淆,所以不推荐使用这种方式;
第二种方式把命令用`$()`包围起来,区分更加明显,所以推荐使用这种方式.

例如,我在 `demo` 目录中创建了一个名为 `log.txt` 的文本文件,用来记录我的日常工作.
下面的代码中,使用 `cat` 命令将 `log.txt` 的内容读取出来,并赋值给一个变量,然后使用 `echo` 命令输出.

```bash
[mozhiyan@localhost ~]$ cd demo
[mozhiyan@localhost demo]$ log=$(cat log.txt)
[mozhiyan@localhost demo]$ echo $log
严长生正在编写Shell教程,教程地址:http://c.biancheng.net/shell/
[mozhiyan@localhost demo]$ log=`cat log.txt`
[mozhiyan@localhost demo]$ echo $log
严长生正在编写Shell教程,教程地址:http://c.biancheng.net/shell/
```

### 只读变量

使用 `readonly` 命令可以将变量定义为只读变量,只读变量的值不能被改变.

下面的例子尝试更改只读变量,结果报错:

```bash
#!/bin/bash
myUrl="http://c.biancheng.net/shell/"
readonly myUrl
myUrl="http://c.biancheng.net/shell/"

运行脚本,结果如下:
bash: myUrl: This variable is read only.
```

### 删除变量

使用 `unset` 命令可以删除变量.语法:

```bash
unset variable_name
```

变量被删除后不能再次使用; `unset` 命令不能删除只读变量.

举个例子:

```bash
#!/bin/sh
myUrl="http://c.biancheng.net/shell/"
unset myUrl
echo $myUrl
```

上面的脚本没有任何输出.

### 变量类型

运行`shell`时,会同时存在三种变量:

1. **局部变量** 局部变量在脚本或命令中定义,仅在当前shell实例中有效,其他shell启动的程序不能访问局部变量.
2. **环境变量** 所有的程序,包括shell启动的程序,都能访问环境变量,有些程序需要环境变量来保证其正常运行.必要的时候shell脚本也可以定义环境变量.
3. **shell变量** shell变量是由shell程序设置的特殊变量.shell变量中有一部分是环境变量,有一部分是局部变量,这些变量保证了shell的正常运行

## Shell 字符串

字符串是`shell`编程中最常用最有用的数据类型(除了数字和字符串,也没啥其它类型好用了),字符串可以用单引号,也可以用双引号,也可以不用引号.单双引号的区别跟PHP类似.

### 单引号

```bash
str='this is a string'
```

单引号字符串的限制:

+ 单引号里的任何字符都会原样输出,单引号字符串中的变量是无效的;
+ 单引号字串中不能出现单独一个的单引号(对单引号使用转义符后也不行),但可成对出现,作为字符串拼接使用.

### 双引号

```bash
your_name='runoob'
str="Hello, I know you are \"$your_name\"! \n"
echo -e $str
out: Hello, I know you are "runoob"!
```

双引号的优点:

+ 双引号里可以有变量
+ 双引号里可以出现转义字符

### 拼接字符串

```bash
your_name="runoob"
# 使用双引号拼接
greeting="hello, "$your_name" !"
greeting_1="hello, ${your_name} !"
echo $greeting  $greeting_1
out: hello, runoob ! hello, runoob !
# 使用单引号拼接
greeting_2='hello, '$your_name' !'
greeting_3='hello, ${your_name} !'
echo $greeting_2  $greeting_3
out: hello, runoob ! hello, ${your_name} !
```

### 获取字符串长度

```bash
string="abcd"
echo ${#string} #输出 4
```

### 提取子字符串

以下实例从字符串第 `2` 个字符开始截取 `4` 个字符:

```bash
string="runoob is a great site"
echo ${string:1:4} # 输出 unoo
```

注意:第一个字符的索引值为 `0`.

### 查找子字符串

查找字符 `i` 或 `o` 的位置(哪个字母先出现就计算哪个):

```bash
string="runoob is a great site"
echo $(expr index "$string" io)  # 输出 4
```

### Shell 数组

[Bash 数组](https://www.w3cschool.cn/bashshell/bashshell-2ynf37ls.html)

`bash`支持一维数组(不支持多维数组),并且没有限定数组的大小.

类似于 `C` 语言,数组元素的下标由 `0` 开始编号.
获取数组中的元素要利用下标,下标可以是整数或算术表达式,其值应大于或等于 `0`.
定义数组

在 `Shell` 中,用括号来表示数组,数组元素用"空格"符号分割开.定义数组的一般形式为:

```bash
数组名=(值1 值2 ... 值n)
```

例如:

```bash
array_name=(value0 value1 value2 value3)
```

或者

```bash
array_name=(
value0
value1
value2
value3
)
```

还可以单独定义数组的各个分量:

```bash
array_name[0]=value0
array_name[1]=value1
array_name[n]=valuen
```

可以不使用连续的下标,而且下标的范围没有限制.

### 读取数组

读取数组元素值的一般格式是:

```bash
${数组名[下标]}
```

例如:

```bash
valuen=${array_name[n]}
```

使用 `@` 符号可以获取数组中的所有元素,例如:

```bash
echo ${array_name[@]}
```

### 获取数组的长度

获取数组长度的方法与获取字符串长度的方法相同,例如:

```bash
# 取得数组元素的个数
length=${#array_name[@]}
# 或者
length=${#array_name[*]}
# 取得数组单个元素的长度
lengthn=${#array_name[n]}
```

### Shell 注释

以 `#` 开头的行就是注释,会被解释器忽略.

通过每一行加一个 `#` 号设置多行注释,像这样:

```bash
#--------------------------------------------
# 这是一个注释
# author:菜鸟教程
# site:www.runoob.com
# slogan:学的不仅是技术,更是梦想!
#--------------------------------------------
##### 用户配置区 开始 #####
#
#
# 这里可以添加脚本描述信息
#
#
##### 用户配置区 结束  #####
```

如果在开发过程中,遇到大段的代码需要临时注释起来,过一会儿又取消注释,怎么办呢?

每一行加个`#`符号太费力了,可以把这一段要注释的代码用一对花括号括起来,定义成一个函数,
没有地方调用这个函数,这块代码就不会执行,达到了和注释一样的效果.

### 多行注释

多行注释还可以使用以下格式:

```bash
:<<EOF
注释内容...
注释内容...
注释内容...
EOF

EOF 也可以使用其他符号:

:<<'
注释内容...
注释内容...
注释内容...
'

:<<!
注释内容...
注释内容...
注释内容...
!
```

## Shell 传递参数

传入脚本的参数,要用双引号保护起来`"args"`,防止变量的自动分字(word splitting)
也就是双层引号可以避免分字

与`mathematica`配合的时候,尽量把长参数放到 `mathematica`  脚本中,把短参数放到调用的`shell`中,结构化成一个关联的形式.

## Shell 数组

### shell中的数组作为参数传递

[shell中的数组作为参数传递](https://blog.csdn.net/brouse8079/article/details/6417836)

`./test.sh  "${atest[@]}"` 简而言之,需要把数组参数用引号括起来.

其中 `$0` 为执行的文件名(包含文件路径)

```bash
#!/bin/bash

echo $1
echo $2
...
echo ${10}
```

***
构造数组

```bash
atest=("a" "bb cc" "dd ee ff" "gg hh ii jj")
```

***
测试

`atest`为数组.此时若把这个数组的内容作为参数调用另一个shell脚本时,写法很关键.

第一种写法:`./test.sh ${atest[@]}`

执行结果:

```bash
a
...
a0
```

此时传递的参数为`a bb cc dd ee ff gg hh ii jj`.把数组的内容组成了一个字符串,已经破坏了原来数组的结构.

第二种写法:`./test.sh  "${atest[@]}"`

执行结果:

```bash
a
bb cc
dd ee ff
gg hh ii jj
a0
```

把数组用双引号括起来,此时传递到`test.sh`中的参数仍然为数组的原来结构.

## 环境变量 path

[ubuntu 修改环境变量(PATH)](https://www.cnblogs.com/crave/p/11818594.html)

在Linux中,在执行命令时,系统会按照`PATH`的设置,去每个`PATH`定义的路径下搜索执行文件,先搜索到的文件先执行.

当我们在执行一个指令癿时候,举例来说"ls"好了,系统会依照PATH的设定去每个PATH定义的目录下搜寻文件名为ls 的可执行文件, 如果在PATH定义的目录中含有多个文件名为ls 的可执行文件,那么先搜寻到癿同名指令先被执行!

***
如何改变PATH

1. 直接修改`$PATH`值:

生效方法:立即生效
有效期限:临时改变,只能在当前的终端窗口中有效,当前窗口关闭后就会恢复原有的`path`配置
用户局限:仅对当前用户

`echo $PATH //查看当前PATH的配置路径`

`export PATH=$PATH:/xxx/xxx //将需配置路径加入$PATH  等号两边一定不能有空格`

配置完后可以通过第一句命令查看配置结果.

### 通过修改.bashrc文件

有效期限:永久有效
用户局限:仅对当前用户

`.bashrc`文件在根目录下

```bash
vi .bashrc  //编辑.bashrc文件
//在最后一行添上:
export PATH=$PATH:/xxx/xxx  ///xxx/xxx位需要加入的环境变量地址 等号两边没空格
```

生效方法:(有以下两种)

+ 关闭当前终端窗口,重新打开一个新终端窗口就能生效
+ 输入`source .bashrc`命令,立即生效

### 通过修改profile文件:(profile文件在/etc目录下)

生效方法:系统重启
有效期限:永久有效
用户局限:对所有用户

```bash
vi /etc/profile //编辑profile文件
//在最后一行添上:
export PATH=$PATH:/xxx/xxx
```

### 通过修改environment文件

生效方法:系统重启
有效期限:永久有效
用户局限:对所有用户

environment文件在`/etc`目录下

```bash
vi /etc/profile //编辑profile文件
在PATH=/......中加入`:/xxx/xxx`
```

## 运算符与括号用法

[shell 中 && || () {} 用法](https://www.jianshu.com/p/617c1ee1e46e)
[shell中$(( )) 与 $( ) 还有${ }的区别](https://www.cnblogs.com/xunbu7/p/6187017.html)
[小括号和大括号用法](http://c.biancheng.net/view/954.html)
[How To Do Calculation in Bash](https://www.shell-tips.com/bash/math-arithmetic-calculation/)
[Shell 中的中括号用法总结](https://www.runoob.com/w3cnote/shell-summary-brackets.html)
[linux shell 中判断文件,目录是否存在](https://blog.csdn.net/yifeng4321/article/details/70232436)

### shell 算术逻辑运算符

+ `id++, id–`: 运算后, 变量自增自减运算符
+ `++id, –id`:运算前, 变量自增自减运算符
+ `-, +`:  加减一元运算符
+ `!, ~`:  逻辑和按位取反
+ `**`: 指数
+ `*, /, %`: 乘除余数
+ `+, -`:  加减
+ `«, » `: 左右移位
+ `<=, >=, <, >`:  比较运算
+ `==, !=`:  等于, 不等于
+ `&`:  按位 AND
+ `^`:  按位 XOR
+ `|`:  按位 OR
+ `&&`:  逻辑 AND
+ `||`:  逻辑 OR
+ `expr1 ? expr2 : expr3`:  条件运算
+ `=, *=, /=, %=, +=, -=, «=, »=, &=, ^=, |=`: 赋值

使用`pstree`可以查看进程树. 知道了`父Shell` 和`子Shell`, 我们接着解释小括号和大括号的区别. 如果用于一串命令的执行, 那么小括号和大括号主要区别在于:

`{}`是`shell`的保留字.

+ `()` 在子Shell执行代码块, `{}`在`当前Shell`执行代码块.
+ `()` 和 `{}` 都用`;`隔开命令. `{}`与命令之间需要用空格分隔, 而`()`开头结尾不需要空格.
+ `{}` 最末条命令要用分号结束, `()` 的最末命令不需要分号.
+ `()` 和 `{}` 内部某个命令的重定向只影响该命令, 但括号外部的重定向则会影响到括号内的所有命令.

### 括号总结

***
命令成组, 用 `()` 或者 `{  }`

```bash
> A=1; echo $A; { A=2 }; echo $A
1
2
> A=1; echo $A; (A=2); echo $A
1
1
```

在使用`{  }`时,`{  }`与命令之间必须使用一个`空格`

***
`$( )`命令替换

`$( )` 与 \`\` (反引号) 都是用来做命令替换

***
`${ }` 变量替换,各种字符串功能

`${ }` 用来作变量替换,把括号里的变量代入值.

***
`$(( ))`整数算术

在 bash 中,`$(( ))` 的整数运算符号大致有这些:

+ `+ - * /` :分别为 "加,减,乘,除".
+ `%` :余数运算
+ `& | ^ !`:分别为 "`AND`,`OR`,`XOR`,`NOT`" 运算.

### 逻辑运算符

***
&&运算符语法格式如下:

```bash
command1 && command2 && command3 ...
```

`command1  && command2`: `command1`返回真, 才执行`command2`, 如果`command1`为假, 不执行`command2`.

示例1中,`cp`命令首先从`root`的家目录复制文件文件`anaconda-ks.cfg`到 `/data`目录下;
执行成功后,使用 `rm` 命令删除源文件; 如果删除成功则输出提示信息"`SUCCESS`".

`cp anaconda-ks.cfg /data/ && rm -f anaconda-ks.cfg && echo "SUCCESS"`

***
`||`运算符:

```bash
command1 || command2
```

`||`则与`&&`相反.如果`command1`未执行成功,那么就执行`command2`; 如果`command1`执行成功, 就不执行`command2`
这和C语言中的逻辑或语法功能相同,即实现短路逻辑或操作.只要有一个命令返回真(命令返回值 `$? == 0`),后面的命令就不会被执行.

例如: 若`dir`目录不存在,将输出提示信息`fail` .

```bash
ls dir &>/dev/null || echo "fail"
```

如果 `dir` 目录存在,将输出 `success` 提示信息; 否则输出 `fail` 提示信息.

```bash
ls dir &>/dev/null && echo "fail" || echo "fail"
```

***
`&>` 的意思是重定向标准输出和错误到 同一个地方,如

```bash
ls -l /bin/usr &> ls-output.txt
```

利用`/dev/null`处理不需要的输出,这个文件是系统设备,叫做位存储桶,它可以 接受输入,并且对输入不做任何处理.

***
下面是一个`shell`脚本中常用的`||`组合示例

```bash
echo $BASH | grep -q 'bash' || { exec bash "$0" "$@" || exit 1; }
```

系统调用`exec`是以新的进程去代替原来的进程,但进程的`PID`保持不变.
因此,可以这样认为,`exec`系统调用并没有创建新的进程,只是替换了原来进程上下文的内容. 原进程的代码段,数据段,堆栈段被新的进程所代替.

### (){}代码段

如果希望把几个命令合在一起执行, `shell` 提供了两种方法. 既可以在`当前shell`, 也可以在`子shell`中执行.

格式:

```bash
(命令1;命令2;命令3....)        # 命令用;分隔, 或者每个命令放到单独的一行. 在子shell中执行, 不影响当前shell的变量
{ command1;command2;command3...  } #在当前shell执行, 会影响当前shell的变量
```

`()` 表示在`子shell `中将多个命令作为一个整体执行. 例如 `(cd ~/Downloads)` 执行完毕后, 当前shell的目录不会切换到`~/Downloads`.
而`{ cd ~/Downloads }`会切换到`~/Downloads`, 注意`{}`中, 命令开头和结尾必须用空格隔开.

命令组合常和命令执行控制结合起来使用.比如如果目录`dir`不存在,则执行命令组合.

```bash
ls dir &>/dev/null || (cd /home/; ls -lh; echo "success")
ls dir &>/dev/null || { cd /home/; ls -lh; echo "success" }
```

另外,`{}`可以用来做花括号展开,例如:

```bash
echo Front-{A,B,C}-Back
echo Number_{1..5}
echo {Z..A}
echo aa{A{1,2},B{3,4}}bb
```

开头称为报头,结尾称为附言,中间包含由逗号分开的字符串或整数列表,不能包含空白. 还可以使用范围, 可以嵌套.

### $( ) 命令替换

`$( )` 与 两个反引号(backtick), 均可以用来做命令替换, 但一般不建议用反引号, 也就是ESC下面那个按键.

`$( )` 的优点: 反引号容易与单引号搞混,尤其对于初学者来说; 在多层次的复合替换中,`backtick` 须要额外的跳脱处理,而 `$( )` 则比较直观.
`$( )` 的不足: 反引号基本上可用在全部的 `unix shell` 中使用,若写成 `shell script` ,其移植性比较高.

### ${} 花括号展开

`${ }` 用来作变量替换,把括号里的变量代入值.

以上的理解在于, 你一定要分清楚 `unset` 与 `null` 及 `non-null` 这三种赋值状态.
一般而言, `:` 与 `null` 有关, 若不带 `:` 的话, null 不受影响, 若带 `:` 则连 null 也受影响.

还有哦,`${#var}` 可计算出变量值的长度:
`${#file}` 可得到 `27` ,因为 `/dir1/dir2/dir3/my.file.txt` 刚好是 `27` 个字节...

***
接下来,再为大家介稍一下 `bash` 的组数(`array`)处理方法.

一般而言,`A="a b c def"` 这样的变量只是将 `$A` 替换为一个单一的字符串,
但是改为 `A=(a b c def) `,则是将 `$A` 定义为组数...

bash 的组数替换方法可参考如下方法:

+ `${A[@]}` 或 `${A[*]}` 可得到 `a b c def` (全部组数)
+ `${A[0]}` 可得到 `a` (第一个组数),${A[1]} 则为第二个组数...
+ `${#A[@]}` 或 `${#A[*]}` 可得到 `4` (全部组数数量)
+ `${#A[0]}` 可得到 `1` (即第一个组数(`a`)的长度),`${#A[3]}` 可得到 `3` (第四个组数(def)的长度)
+ `A[3]=xyz` 则是将第四个组数重新定义为 `xyz` ...

### (()) 算术展开

`$(( 算术表达式 ))`称为算术展开, `((...))`称为复合命令, 它返回最后一个表达式的值, 它只能计算整数算术. 括号`(())`和`==`等操作符周围都不需要空格.

+ `+ - * /` :分别为加,减,乘,除.
+ `%` :余数运算
+ `<<`: 左移
+ `>>`: 右移
+ `&`: 按位与
+ `|`: 按位或
+ `~`: 按位非
+ `^`: 按位异或
+ `& | ^ !`:分别为 `AND`,`OR`,`XOR`,`NOT` 的位运算.

其中`XOR`表示`exclusive OR`:一样为`0`,不一样为`1`,相当于不考虑进位的加法. 例:

```bash
$ a=5; b=7; c=2
$ echo $(( a+b*c ))
19
$ echo $(( (a+b)/c ))
6
$ echo $(( (a*b)%c))
1
```

`$(( ))` 中可以做变量替换,  `$变量名称`将替换成变量的值, 可以省略`$`, 如:`$(( $a + $b * $c))` 也可得到 `19` 的结果.
此外,`$(( ))` 还可作不同进位(如二进制,八进位,十六进制)作运算,只是,输出结果皆为十进制:

```bash
echo $((16#2a)) 结果为 42 (16进位转十进制)
```

以一个实用的例子来看看吧:

假如当前的   `umask` 是 `022` ,那么新建文件的权限即为:

```bash
$ umask 022
$ echo "obase=8;$(( 8#666 & (8#777 ^ 8#$(umask)) ))" | bc
644
```

事实上,单纯用 `(( ))` 也可重定义变量值,或作 `testing`:

+ `a=5; ((a++))` 可将 `$a` 重定义为 `6`
+ `a=5; ((a–))` 则为 `a=4`
+ `a=5; b=7; ((a < b))` 会得到   `0` (`true`) 的返回值.

用于 `(( ))` 的比较运算符:

+ `<`:小于
+ `>`:大于
+ `<=`:小于或等于
+ `>=`:大于或等于
+ `==`:等于
+ `!=`:不等于

### [],文件系统测试,bash test

`test`和`[`基本是同一个 `shell` 内置命令. `test`  检查文件系统, 或者比较两个值的关系(数字或者字符串)

+ 算术比较.比如一个变量是否为`0`, `[ $var -eq 0 ]`.
+ 文件属性测试. 比如一个文件是否存在,`[ -e $var ]`, 是否是目录,`[ -d $var ]`.
+ 字符串比较. 比如两个字符串是否相同, `[[ $var1 = $var2 ]]`.

语法:

```bash
test 表达式
test
[ 表达式 ]
[ ]
[ OPTION
```

需要注意的是 `[` 与 `]` 与操作数之间一定要有一个空格,否则会报错.比如下面这样就会报错: `[$var -eq 0 ]`  或 `[ $var -ne 0]`.

`Shell` 还提供了与`-a` , 或`-o`, 非`!`三个逻辑操作符用于将测试条件连接起来, 其优先级为:  `!` 最高,  `-a` 次之,  `-o` 最低.  例如:

`[ $var1 -ne 0 -a $var2 -gt 2 ]`  # 使用逻辑与 `-a`
`[ $var1 -ne 0 -o $var2 -gt 2 ]`  # 使用逻辑或 `-o`

#### 各种测试

如果省略了`EXPRESSION`, 则默认为`false`.  否则, `EXPRESSION为`真或假, 并设置返回状态.

+ `( EXPRESSION )`:EXPRESSION为真
+ `! EXPRESSION`: EXPRESSION 为假
+ `EXPRESSION1 -a EXPRESSION2`: EXPRESSION1和EXPRESSION2都是真.
+ `EXPRESSION1 -o EXPRESSION2`: EXPRESSION1或EXPRESSION2为真.

+ `[[-n STRING]]`: `STRING` 的长度为非零. `STRING` 相当于`-n STRING`, 可以用来判断环境变量是否存在
+ `[[-z STRING]]`: 表示 `STRING` 的长度为零
+ `[[string1 = string2]]`: 字符串是相等的
+ `[[string1 != string2]]`: 字符串不相等

+ `[INTEGER1 -eq INTEGER2]`: `INTEGER1`等于`INTEGER2`
+ `[INTEGER1 -ge INTEGER2]`: 大于或等于
+ `[INTEGER1 -gt INTEGER2]`: 大于
+ `[INTEGER1 -le INTEGER2]`: 小于或等于
+ `[INTEGER1 -lt INTEGER2]`:小于
+ `[INTEGER1 -ne INTEGER2]`:不等于

+ `[FILE1 -ef FILE2]`: `FILE1`和`FILE2`有相同的设备号和节点号
+ `[FILE1 -nt FILE2]`: `FILE1`比`FILE2`要新(修改日期).
+ `[FILE1 -ot FILE2]`: `FILE1` 比`FILE2`老

+ `[-b FILE]`: `FILE`存在并且是块类型
+ `[-c FILE]`: `FILE`存在, 并且是字符类型
+ `[-d FILE]`: `FILE`存在并且是一个目录
+ `[-e FILE]`: `FILE`存在.
+ `[-f FILE]`: `FILE`存在并且是一个普通文件.
+ `[-g FILE]`:`FILE`存在并且是`set-group-ID`.
+ `[-G FILE]`: `FILE`存在并且被有效的组ID所拥有
+ `[-h FILE]`: `FILE`存在并且是一个符号链接(与-L相同).
+ `[-k FILE]`:`FILE`存在, 并且其`sticky bit `被设置.
+ `[-L FILE]`:`FILE`存在并且是一个符号链接(与-h相同).
+ `[-O FILE]`: `FILE`存在并且被有效的用户`ID`所拥有
+ `[-p FILE]`:`FILE`存在, 并且是一个命名的管道
+ `[-r FILE]`:`FILE`存在并且被授予读取权限
+ `[-s FILE]`:`FILE`存在并且大小大于`0`
+ `[-S FILE]`:`FILE`存在并且是一个套接字
+ `[-t FD]`: 文件描述符`FD`在一个终端上被打开.
+ `[-u FILE]`:`FILE`存在并且其`set-user-ID`位被设置
+ `[-w FILE]`:`FILE`存在并且被授予写权限
+ `[-x FILE]`:`FILE`存在, 并且被授予执行(或搜索)权限

除了`-h`和`-L`之外, 所有与`FILE`相关的测试都会对符号链接`dereference`.
注意, 对于`shells`来说, 小括号需要被转义(`\(  \)`).  `INTEGER`也可以是`-l STRING`, 它被运算为`STRING`的长度.

注意: 二元的`-a`和`-o`本身有歧义.  使用`test EXPR1 && test EXPR2`或`test EXPR1 || test EXPR2`代替.
注意: `[` 具有 `--help` 和 `--version` 选项, 但 `test` 不具有. `test` 对这些选项的处理与对其他非空字符串的处理相同.
注意: 你的shell可能有自己的`test`或`[`的版本, 它通常取代这里描述的版本.  关于它所支持的选项的细节, 请参考你的shell的文档.

例子:

```bash
# 如果 filename 存在,则为真
if [ -e filename ]; then echo "true";fi
# 如果存在某文件,则删除
if [ -f trials ]; then rm ${result_path}trials; fi
# 如果没有文件夹,则创建
if [ ! -d $result_name ];then mkdir -p $result_name;fi
```

>注意: `bash`在变量代入时比较粗犷, 最好用双引号把`变量`裹住, 例如:

```bash
abc="hello xx"; if test "hello" != "$abc"; then  echo "Your word is not 'hello'."; fi
```

变量 `abc` 的值为`hello xx`, 在字符串中间有个空格. 如果不用引号保护起来, `bash`解释上面的命令时, 会将`test` 命令解释成:

```bash
test "hello" != hello xx
```

这不是一个合法的 `test` 命令, 所以脚本执行时就会报错.
其实不光是空格, 包含在 `$IFS `(internal field separator)中的其它字符, 以及空变量, 都会造成语法错误.
所以使用双引号包裹变量是一种保护机制, 可以提高脚本的健壮性. 但是在`zsh`中可以不用引号包裹, `zsh`和`bash`的分词机制不同.

### [[ ]]字符串比较

在进行字符串比较时,最好使用双中括号 `[[ ]]`. 因为单中括号可能会导致一些错误, 因此最好避开它们.

检查两个字符串是否相同:

```bash
[[ $str1 == $str2 ]]
```

当 `str1` 等于 `str2` 时,返回真.也就是说, `str1` 和 `str2` 包含的文本是一样的.
其中的双等于号也可以写成单等于号,也就是说,上面的字符串比较等效于 `[[ $str1 = $str2 ]]`.
注意 `=` 前后有一个`空格`,如果忘记加空格, 就变成了赋值语句,而非比较关系了.
字符串比较,`[[`,`]]`,`==`周围必须都有空格,中括号比较时,变量必须写成如`$a`的形式.

字符串的其他比较情况:

+ `[[ $str1 != $str2 ]]`   如果 `str1` 与 `str2` 不相同,则返回真
+ `[[ -z $str1 ]]`   如果 `str1` 是`null`字符串,则返回真
+ `[[ -n $str1 ]]`   如果 `str1` 是非`null`字符串,则返回真

使用逻辑运算符 `&&` 和 `||` 可以轻松地将多个条件组合起来, 比如:

```bash
str1="Not empty"
str2=""
if [[ -n $str1 ]] && [[ -z $str2 ]];
then
  echo str1 is nonempty and str2 is empty string.
fi
```

`test` 命令也可以从来执行条件检测,用 `test` 可以避免使用过多的括号,`[]` 中的测试条件同样可以通过 `test` 来完成.

```bash
if [ $var -eq 0 ]; then echo "True"; fi
# 等价于:
if test $var -eq 0; then echo "True"; fi
```

### declare

[Bash 数组](https://www.w3cschool.cn/bashshell/bashshell-2ynf37ls.html)

```bash
declare: declare [-aAfFgilnrtux] [-p] [name[=value] ...]
```

`declare`用来设置变量的值和属性. 声明变量并赋予其属性.  如果没有给出`NAME`,  显示所有变量的属性和值.

选项:

+ `-f`:  只将动作或显示作用在函数类的名称和定义上
+ `-F`: 限制只显示函数名(调试时加上行号和源文件)
+ `-g`: 用于`shell`函数时, 创建局变量; 否则忽略此选项
+ `-p`:  显示每个`NAME`的属性和值

设置属性的选项:

+ `-a`: 使`NAME`成为索引数组(如果支持)
+ `-A`: 使`NAME`成为关联数组(如果支持)
+ `-i`: 使`NAME`具有 "整数 "属性
+ `-l`: 在赋值时将每个`NAME`的值转换为小写.
+ `-n`: 以值命名的变量,把`NAME`当成它的引用
+ `-r`: 使`NAME`成为只读变量
+ `-t`: 使`NAME`具有 `trace`属性
+ `-u`: 在赋值时将每个`NAME`的值转换为大写字母
+ `-x`: 使`NAME`输出.

因为命令行选项用`-`开头, 跟通常相反, 使用`+`关闭指定的属性.
带有整数属性的变量, 在赋值的时候会先进行算术计算(见`let`命令).
当在一个函数中使用时, `declare`使`NAME`成为局部的, 就像`local`命令那样. `-g`选项抑制这种行为.

***

+ `-a`选项指定索引数组, 索引从0开始. `=`两侧无空格.
+ `-A`选项指定关联数组.
+ `${Array_Name[index]}`: 获取数组元素. 使用`@`或`*`作为索引,可以得到数组中的所有元素.
+ `${!Array_Name[index]}`: 打印数组中的键使用
+ `${#ARRAY_NAME[@]}`: 获取数组长度.
+ `for i in ${arr[@]}; do echo $i;done`: 遍历数组的值
+ `for i in "${!arr[@]}";  do   echo $i;    done`: 遍历数组的键.
+ `arr[4]="JavaScript"`: 增加元素.
+ `unset arr[1]`: 删除数组元素
+ `unset arr` :删除数组
+ ` arr2=("${arr[@]:m:n}")`: 获取数组切片

举例:

```bash
declare -a x=(a b c d e);declare -a y=([0]=a [2]=c [3]=f [4]=k);
declare -p x y;echo ${x[2]};
echo '删除数组'; declare -a arr=( "Java" "Python" "HTML" "CSS" "JavaScript" );unset arr;echo ${!arr[@]};
echo '数组的键'; declare -a arr=( "Welcome" "To" "W3Cschool" );echo "${!arr[@]}";
echo '数组的长度';echo ${#arr[@]};echo ${#arr[*]};
echo '遍历数组'; for i in "${!arr[@]}"; do echo The key value of element "${arr[$i]}" is "$i"; done;
echo '数组切片';arr=( "Java" "Python" "HTML" "CSS" "JavaScript" );arr2=("${example_array[@]:1:3}"); declare -p arr2;
```

***
`@`和` *`两者的区别:

```bash
declare -a arr=("welcome" "to" "W3Cschool")
echo "@";for i in "${arr[@]}";do echo "$i";done;
echo "*";for i in "${arr[*]}";do echo "$i";done;
```

### local

```bash
local: local [option] name[=value] ...
```

`local`定义局部变量. 它的选项和`declare`相同. `local`变量只在定义的函数内可用, 它只对此函数和子函数可见.
