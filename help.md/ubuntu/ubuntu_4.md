# ubuntu-4

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

`variable` 是变量名,`value` 是赋给变量的值.如果 `value` 不包含任何空白符(例如空格,`Tab` 缩进等),那么可以不使用引号；
如果 `value` 包含了空白符,那么就必须使用引号包围起来.使用单引号和使用双引号也是有区别的,稍后我们会详细说明.

注意,赋值号`=`的周围不能有空格,这可能和你熟悉的大部分编程语言都不一样.

`Shell` 变量的命名规范和大部分编程语言都一样:

+ 变量名由数字,字母,下划线组成；
+ 必须以字母或者下划线开头；
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
如果变量的内容是数字,那么可以不加引号；
如果真的需要原样输出就加单引号；
其他没有特别要求的字符串等最好都加上双引号,定义变量时加双引号是最常见的使用场景.

### 将命令的结果赋值给变量

`Shell` 也支持将命令的执行结果赋值给变量,常见的有以下两种方式:

```bash
variable=`command`
variable=$(command)
```

第一种方式把命令用反引号` `(位于 `Esc` 键的下方)包围起来,反引号和单引号非常相似,容易产生混淆,所以不推荐使用这种方式；
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

变量被删除后不能再次使用；`unset` 命令不能删除只读变量.

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

+ 单引号里的任何字符都会原样输出,单引号字符串中的变量是无效的；
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
# slogan:学的不仅是技术,更是梦想！
#--------------------------------------------
##### 用户配置区 开始 #####
#
#
# 这里可以添加脚本描述信息
#
#
##### 用户配置区 结束  #####
```

如果在开发过程中,遇到大段的代码需要临时注释起来,过一会儿又取消注释,怎么办呢？

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

与`mathematica`配合的时候,尽量把长参数放到mathematica 脚本中,把短参数放到调用的`shell`中,结构化成一个关联的形式.

### 实例

## Shell 数组

### 实例

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

当我们在执行一个指令癿时候,举例来说"ls"好了,系统会依照PATH的设定去每个PATH定义的目录下搜寻文件名为ls 的可执行文件, 如果在PATH定义的目录中含有多个文件名为ls 的可执行文件,那么先搜寻到癿同名指令先被执行！

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

使用`pstree`可以查看进程树. 知道了`父Shell` 和`子Shell`, 我们接着解释小括号和大括号的区别. 如果用于一串命令的执行, 那么小括号和大括号主要区别在于：

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
执行成功后,使用 `rm` 命令删除源文件；如果删除成功则输出提示信息"`SUCCESS`".

`cp anaconda-ks.cfg /data/ && rm -f anaconda-ks.cfg && echo "SUCCESS"`

***
`||`运算符:

```bash
command1 || command2
```

`||`则与`&&`相反.如果`command1`未执行成功,那么就执行`command2`；如果`command1`执行成功, 就不执行`command2`
这和C语言中的逻辑或语法功能相同,即实现短路逻辑或操作.只要有一个命令返回真(命令返回值 `$? == 0`),后面的命令就不会被执行.

例如: 若`dir`目录不存在,将输出提示信息`fail` .

```bash
ls dir &>/dev/null || echo "fail"
```

如果 `dir` 目录存在,将输出 `success` 提示信息；否则输出 `fail` 提示信息.

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
{ command1;command2;command3… } #在当前shell执行, 会影响当前shell的变量
```

`()` 表示在`子shell `中将多个命令作为一个整体执行. 例如 `(cd ~/Downloads)` 执行完毕后, 当前shell的目录不会切换到`~/Downloads`.
而`{ cd ~/Downloads }`会切换到`~/Downloads`, 注意`{}`中, 命令开头和结尾必须用空格隔开. 

命令组合常和命令执行控制结合起来使用.比如如果目录`dir`不存在,则执行命令组合.

```bash
ls dir &>/dev/null || (cd /home/; ls -lh; echo "success")
ls dir &>/dev/null || { cd /home/; ls -lh; echo "success" }
```

另外,`{}`可以用来做花括号展开,例如：

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
`${#file}` 可得到 `27` ,因为 `/dir1/dir2/dir3/my.file.txt` 刚好是 `27` 个字节…

***
接下来,再为大家介稍一下 `bash` 的组数(`array`)处理方法.

一般而言,`A="a b c def"` 这样的变量只是将 `$A` 替换为一个单一的字符串,
但是改为 `A=(a b c def) `,则是将 `$A` 定义为组数…

bash 的组数替换方法可参考如下方法:

+ `${A[@]}` 或 `${A[*]}` 可得到 `a b c def` (全部组数)
+ `${A[0]}` 可得到 `a` (第一个组数),${A[1]} 则为第二个组数…
+ `${#A[@]}` 或 `${#A[*]}` 可得到 `4` (全部组数数量)
+ `${#A[0]}` 可得到 `1` (即第一个组数(`a`)的长度),`${#A[3]}` 可得到 `3` (第四个组数(def)的长度)
+ `A[3]=xyz` 则是将第四个组数重新定义为 `xyz` …

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

### []文件系统测试

`test`和`[`基本是同一个shell内置命令. `test`  检查文件系统, 或者比较两个值的关系(数字或者字符串)

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

`Shell` 还提供了与`-a` , 或`-o`, 非`!`三个逻辑操作符用于将测试条件连接起来, 其优先级为： `!` 最高,  `-a` 次之,  `-o` 最低.  例如：

`[ $var1 -ne 0 -a $var2 -gt 2 ]`  # 使用逻辑与 `-a`
`[ $var1 -ne 0 -o $var2 -gt 2 ]`  # 使用逻辑或 `-o`

#### 各种测试

如果省略了`EXPRESSION`, 则默认为`false`.  否则, `EXPRESSION为`真或假, 并设置返回状态.  

+ `( EXPRESSION )`:EXPRESSION为真
+ `! EXPRESSION`: EXPRESSION 为假
+ `EXPRESSION1 -a EXPRESSION2`: EXPRESSION1和EXPRESSION2都是真. 
+ `EXPRESSION1 -o EXPRESSION2`: EXPRESSION1或EXPRESSION2为真. 
  
+ `-n STRING`: STRING的长度为非零. STRING相当于`-n STRING`
+ `-z STRING`: 表示STRING的长度为零
+ `string1 = string2`: 字符串是相等的
+ `string1 != string2`: 字符串不相等
  
+ `INTEGER1 -eq INTEGER2`: `INTEGER1`等于`INTEGER2`
+ `INTEGER1 -ge INTEGER2`: 大于或等于
+ `INTEGER1 -gt INTEGER2`: 大于
+ `INTEGER1 -le INTEGER2`: 小于或等于
+ `INTEGER1 -lt INTEGER2`:小于
+ `INTEGER1 -ne INTEGER2`:不等于
+ `FILE1 -ef FILE2`: `FILE1`和`FILE2`有相同的设备号和节点号
+ `FILE1 -nt FILE2`: FILE1比FILE2要新(修改日期). 
+ `FILE1 -ot FILE2`: FILE1比FILE2老

+ `-b FILE`: `FILE`存在并且是块类型
+ `-c FILE`: `FILE`存在, 并且是字符类型
+ `-d FILE`: `FILE`存在并且是一个目录
+ `-e FILE`: `FILE`存在.
+ `-f FILE`: `FILE`存在并且是一个普通文件
+ `-g FILE`:`FILE`存在并且是`set-group-ID`. 
+ `-G FILE`: `FILE`存在并且被有效的组ID所拥有
+ `-h FILE`: `FILE`存在并且是一个符号链接(与-L相同). 
+ `-k FILE`:`FILE`存在, 并且其`sticky bit `被设置. 
+ `-L FILE`:`FILE`存在并且是一个符号链接(与-h相同). 
+ `-O FILE`: `FILE`存在并且被有效的用户`ID`所拥有
+ `-p FILE`:`FILE`存在, 并且是一个命名的管道
+ `-r FILE`:`FILE`存在并且被授予读取权限
+ `-s FILE`:`FILE`存在并且大小大于`0`
+ `-S FILE`:`FILE`存在并且是一个套接字
+ `-t FD`: 文件描述符`FD`在一个终端上被打开.
+ `-u FILE`:`FILE`存在并且其`set-user-ID`位被设置
+ `-w FILE`:`FILE`存在并且被授予写权限
+ `-x FILE`:`FILE`存在, 并且被授予执行(或搜索)权限

除了`-h`和`-L`之外, 所有与`FILE`相关的测试都会对符号链接`dereference`.  
注意, 对于`shells`来说, 小括号需要被转义(`\(  \)`).  `INTEGER`也可以是`-l STRING`, 它被评估为`STRING`的长度. 

注意：二元的`-a`和`-o`本身有歧义.  使用`test EXPR1 && test EXPR2`或`test EXPR1 || test EXPR2`代替. 
注意: `[` 具有 `--help` 和 `--version` 选项, 但 `test` 不具有. `test` 对这些选项的处理与对其他非空字符串的处理相同. 
注意：你的shell可能有自己的`test`或`[`的版本, 它通常取代这里描述的版本.  关于它所支持的选项的细节, 请参考你的shell的文档. 

***
如果 `filename` 存在,则为真

```bash
if [ -e filename ]; then echo "true";fi
```

如果存在某文件,则删除

```bash
if [ -f trials ]; then rm ${result_path}trials; fi
```

如果没有文件夹,则创建

```bash
if [ ! -d $result_name ];then mkdir -p $result_name;fi
```

***
`bash`在变量替换方面比较粗犷, 有时需要用双引号把变量裹住, 例如：

```bash
abc="hello xx"; if test "hello" != "$abc"; then  echo "Your word is not 'hello'."; fi
```

变量 `abc` 的值为`hello xx`, 在字符串中间有个空格. 如果不用引号保护起来, `bash`解释上面的命令时, 会将`test` 命令解释成：

```bash
test "hello" != hello xx
```

这不是一个合法的 `test` 命令, 所以脚本执行时就会报错. 其实不光是空格, 包含在 `$IFS `(internal field separator)中的其它字符, 以及空变量, 都会造成语法错误. 
所以使用双引号包裹变量是一种保护机制, 可以提高脚本的健壮性. 但是在`zsh`中可以不用引号包裹, `zsh`和`bash`的分词机制不同.

### [[ ]]字符串比较

在进行字符串比较时,最好使用双中括号 `[[ ]]`. 因为单中括号可能会导致一些错误,因此最好避开它们.

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
```

等价于:

```bash
if test $var -eq 0; then echo "True"; fi
```

### declare

[Bash 数组](https://www.w3cschool.cn/bashshell/bashshell-2ynf37ls.html)

```bash
declare: declare [-aAfFgilnrtux] [-p] [name[=value] ...]
```

`declare`用来设置变量的值和属性. 声明变量并赋予其属性.  如果没有给出`NAME`,  显示所有变量的属性和值. 

选项：

+ `-f`:  只将动作或显示作用在函数类的名称和定义上
+ `-F`: 限制只显示函数名(调试时加上行号和源文件)
+ `-g`: 用于`shell`函数时, 创建局变量；否则忽略此选项
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

举例：

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

## 字符串和数字

commandline chapter 35

在这一章中,我们将查看几个用来操作字符串和数字的 shell 功能.
shell 提供了各种执行字符串操作的参数展开功能.
除了算术展开(在第七章中接触过),还有一个常见的命令行程序叫做 `bc`,能执行更高级别的数学运算.

### 字符串总结

parameter 前面可能出现的保留字,`!`, `#`
parameter 后面可能接的保留字,`:` `#` `%` `/`

+ 返回变量名的参数展开
+ `${!prefix*}` :这种展开会返回以 `prefix` 开头的已有变量名
+ `${#parameter}` :展开成由 parameter 所包含的字符串的长度.
+ 空变量的展开
+ `${parameter:-word}` :若 parameter 没有设置(例如,不存在)或者为空,展开结果是 word 的值
+ `${parameter:=word}` :若 parameter 没有设置或为空,展开结果是 word 的值.另外,word 的值会赋值给 parameter.
+ `${parameter:?word}`:若 parameter 没有设置或为空,这种展开导致脚本带有错误退出,并且 word 的内容会发送到标准错误.
+ `${parameter:+word}`:若 parameter 为空,结果为空.若 parameter 不为空, word 的值; parameter 的值不改变.
+ 字符串展开
+ `${parameter:offset}` :从 `parameter` 所包含的字符串中提取一部分字符,到结尾
+ `${parameter:offset:length}` :从 `parameter` 所包含的字符串中提取一部分字符,`length`制定长度
+ 字符串修剪
+ `${parameter#pattern}` :从 `paramter` 所包含的字符串中清除开头的`pattern`
+ `${parameter##pattern}` :## 模式清除最长的匹配结果.
+ `${parameter%pattern}` :清除 `parameter` 末尾所包含的`pattern`
+ `${parameter%%pattern}` :%% 模式清除最长的匹配结果.
+ 字符串查找和替换操作 `parameter`必须是一个变量 `pattern` 和 `string` 可以不加引号
+ `${parameter/pattern/string}` :如果找到了匹配通配符 `pattern` 的文本, 则用 `string` 的内容替换它.
+ `${parameter//pattern/string}` : `//` 形式下,所有的匹配项都会被替换掉
+ `${parameter/#pattern/string}` :` /# `要求匹配项出现在字符串的开头,
+ `${parameter/%pattern/string}` :`/%` 要求匹配项出现在字符串的末尾

### 参数展开

尽管参数展开在第七章中出现过,但我们并没有详尽地介绍它,因为大多数的参数展开会用在脚本中,而不是命
令行中. 我们已经使用了一些形式的参数展开;例如,shell 变量.shell 提供了更多方式.

### 基本参数展开

最简单的参数展开形式反映在平常使用的变量上.

在这个例子中,我们试图创建一个文件名,通过把字符串 `_file` 附加到变量 `a` 的值的后面.

```bash
[me@linuxbox ~]$ a="foo"
[me@linuxbox ~]$ echo "$a_file"
```

如果我们执行这个序列,没有任何输出结果,因为 `shell` 会试着展开一个称为 `a_file` 的变量,而不是 `a`.通过添加花括号可以解决这个问题:

```bash
[me@linuxbox ~]$ echo "${a}_file"
foo_file
```

我们已经知道通过把数字包裹在花括号中,可以访问大于`9`的位置参数.例如,访问第十一个位置参数,我们可以这样做: `${11}`

### 管理空变量的展开

`null`
`undefined`
`defined`

几种用来处理不存在和空变量的参数展开形式.
这些展开形式对于解决丢失的位置参数和给参数指定默认值的情况很方便.

```bash
${parameter:-word}
```

若 parameter 没有设置(例如,不存在)或者为空,展开结果是 `word` 的值.
若 parameter 不为空,则展开结果是 parameter 的值.

```bash
foo=
echo ${foo:-"substitute value if unset"}
echo $foo
foo=bar
echo ${foo:-"substitute value if unset"}
echo $foo
```

```bash
${parameter:=word}
```

若 `parameter` 没有设置或为空,展开结果是 `word` 的值.另外,`word` 的值会赋值给 `parameter`.
若 `parameter` 不为空,展开结果是 `parameter` 的值.

```bash
foo=
echo ${foo:="default value if unset"}
unset
echo $foo
unset
foo=bar
echo ${foo:="default value if unset"}
echo $foo
```

注意: 位置参数或其它的特殊参数不能以这种方式赋值.

```bash
${parameter:?word}
```

若 parameter 没有设置或为空,这种展开导致脚本带有错误退出,并且 word 的内容会发送到标准错误.
若parameter 不为空, 展开结果是 parameter 的值.

```bash
foo=
echo ${foo:?"parameter is empty"
echo $?
foo=bar
echo ${foo:?"parameter is empty"}
echo $?
```

```bash
${parameter:+word}
```

若 parameter 没有设置或为空,展开结果为空.
若 parameter 不为空, 展开结果是 word 的值会替换掉parameter 的值;然而,parameter 的值不会改变.

```bash
foo=
echo ${foo:+"substitute value if set"}
foo=bar
echo ${foo:+"substitute value if set"}
```

### 返回变量名的参数展开

`shell` 具有返回变量名的能力.这会用在一些相当独特的情况下.

```bash
${!prefix*}
${!prefix@}
```

这种展开会返回以 `prefix` 开头的已有变量名.根据 bash 文档,这两种展开形式的执行结果相同.
这里,我们列出了所有以 `BASH` 开头的环境变量名:

```bash
[me@linuxbox ~]$ echo ${!BASH*}
BASH BASH_ARGC BASH_ARGV BASH_COMMAND BASH_COMPLETION
...
```

### 字符串展开

有大量的展开形式可用于操作字符串.其中许多展开形式尤其适用于路径名的展开.

```bash
${#parameter}
```

展开成由 `parameter` 所包含的字符串的长度.

通常,`parameter` 是一个字符串;然而,如果 `parameter` 是 `@`或者是 `*` 的话, 则展开结果是位置参数的个数.

```bash
[me@linuxbox ~]$ foo="This string is long."
[me@linuxbox ~]$ echo "'$foo' is ${#foo} characters long."
'This string is long.' is 20 characters long.
```

***

```bash
${parameter:offset}
${parameter:offset:length}
```

这些展开用来从 `parameter` 所包含的字符串中提取一部分字符.
提取的字符始于第 `offset` 个字符(从字符串开头算起)直到字符串的末尾,除非指定提取的长度.

```bash
[me@linuxbox ~]$ foo="This string is long."
[me@linuxbox ~]$ echo ${foo:5}
string is long.
[me@linuxbox ~]$ echo ${foo:5:6}
string
```

若 `offset` 的值为负数,则认为 `offset` 值是从字符串的末尾开始算起,而不是从开头.
注意负数前面必须有一个空格, 为防止与 `${parameter:-word}` 展开形式混淆.`length`,若出现,则必须不能小于零.
如果 `parameter` 是 `@`,展开结果是 `length` 个位置参数,从第 offset 个位置参数开始.

```bash
[me@linuxbox ~]$ foo="This string is long."
[me@linuxbox ~]$ echo ${foo: -5}
long.
[me@linuxbox ~]$ echo ${foo: -5:2}
lo
```

***

```bash
${parameter#pattern}
${parameter##pattern}
```

这些展开会从 `paramter` 所包含的字符串中清除开头一部分文本,这些字符要匹配定义的 `patten` .
pattern 是通配符模式,就如那些用在路径名展开中的模式.
这两种形式的差异之处是该 `#` 形式清除最短的匹配结果, 而该`##` 模式清除最长的匹配结果.

```bash
[me@linuxbox ~]$ foo=file.txt.zip
[me@linuxbox ~]$ echo ${foo#*.}
txt.zip
[me@linuxbox ~]$ echo ${foo##*.}
zip
```

***

```bash
${parameter%pattern}
${parameter%%pattern}
```

这些展开和上面的 `#` 和 `##` 展开一样,除了它们清除的文本从 `parameter` 所包含字符串的末尾开始,而不是开头.

```bash
[me@linuxbox ~]$ foo=file.txt.zip
[me@linuxbox ~]$ echo ${foo%.*}
file.txt
[me@linuxbox ~]$ echo ${foo%%.*}
file
```

***

```bash
${parameter/pattern/string}
${parameter//pattern/string}
${parameter/#pattern/string}
${parameter/%pattern/string}
```

这种形式的展开对 `parameter` 的内容执行查找和替换操作.

如果找到了匹配通配符 `pattern` 的文本, 则用 `string` 的内容替换它.
在正常形式下,只有第一个匹配项会被替换掉.在 `//` 形式下,所有的匹配项都会被替换掉.
` /# `要求匹配项出现在字符串的开头,而 `/%` 要求匹配项出现在字符串的末尾.
`/string` 可能会省略掉,这样会导致删除匹配的文本.

```bash
[me@linuxbox~]$ foo=JPG.JPG
[me@linuxbox ~]$ echo ${foo/JPG/jpg}
jpg.JPG
[me@linuxbox~]$ echo ${foo//JPG/jpg}
jpg.jpg
[me@linuxbox~]$ echo ${foo/#JPG/jpg}
jpg.JPG
[me@linuxbox~]$ echo ${foo/%JPG/jpg}
JPG.jpg
```

知道参数展开是件很好的事情.字符串操作展开可以用来替换其它常见命令比方说 `sed` 和 `cut`.通过减少使用外部程序,展开提高了脚本的效率.

举例说明,我们将修改在之前章节中讨论的 `longest-word` 程序,
用参数展开`${#j}` 取代命令 `$(echo $j | wc -c)` 及其`subshell` ,像这样:

`wc`- print newline, word, and byte counts for each file

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

下一步,我们将使用 `time` 命令来比较这两个脚本版本的效率:

```bash
[me@linuxbox ~]$ time longest-word2 dirlist-usr-bin.txt
dirlist-usr-bin.txt: 'scrollkeeper-get-extended-content-list' (38 characters)
real 0m3.618s
user 0m1.544s
sys 0m1.768s
[me@linuxbox ~]$ time longest-word3 dirlist-usr-bin.txt
dirlist-usr-bin.txt: 'scrollkeeper-get-extended-content-list' (38 characters)
real 0m0.060s
user 0m0.056s
sys 0m0.008s
```

原来的脚本扫描整个文本文件需耗时`3.168`秒,而该新版本,使用参数展开,仅仅花费了`0.06`秒 —— 一个非常巨
大的提高.

### formfactor bash 脚本

```bash
curveopacity=1
markers="Bands"
markopacity=0.1
expr_marker=3
expr_opacity=1

wolframscript -print "all" -file ./f.figure.series-full.rencon3.strange.baryons-all.band.wl "full" 0.90 1.50 $curveopacity $markers $markopacity $expr_marker $expr_opacity
```

### 通配符/Wildcard/glob

[Shell中的通配符](https://www.jianshu.com/p/25f3d0cd5fdc)

`glob()`, glob: 一滴 一团

`glob()`函数根据`shell`使用的规则搜索所有与模式匹配的路径名 (请参阅`glob(7)`)
没有`tilde expansion`或`parameter substitution`； 如果需要这些,请使用`wordexp(3)`.

`globfree()`函数释放先前调用`glob()`时,动态分配的存储空间 .
`man 7 glob()` see glob(7)

在 `Shell` 中命令中,通常会使用通配符表达式来匹配一些文件,如以下命令可以查找当前目录下所有后缀为 `.xml` 的文件

```bash
find . -name "*.xml" 
```

Shell 中可以使用的通配符如下:

| 通配符| 含义| 实例|
|--|--|--|
|`*`|  匹配 `0` 或多个字符   | `a*b`,`a`与`b`之间可以有任意长度的任意字符, 也可以一个也没有, 如 `aabcb`, `axyzb`, `a012b`, `ab`|
| `?`| 匹配任意单个字符   |`a?b`,`a`与`b`之间有且只有一个字符, 可以是任意字符,如 `aab`, `abb`, `acb`, `a0b`|
| `[list]`|  匹配 `list` 中的任意单个字符 | `a[xyz]b`,`a`与`b`之间必须也只能有一个字符, 但只能是 `x` 或 `y` 或 `z`, 如 `axb`, `ayb`, `azb`.|
|`[!list]`|  匹配除 `list` 中的任意单一字符 |  `a[!0-9]b`,`a`与`b`之间必须也只能有一个字符, 但不能是阿拉伯数字, 如 `axb,` `aab`, `a-b`.|
|`[c1-c2]`| 匹配 `c1-c2` 中的任意单一字符 | `a[0-9]b`,匹配`0`与`9`之间其中一个字符,如 `a0b`, `a1b`... `a9b`|
| `{s1,s2,...}` | 匹配 `s1` 或 `s2` (或更多)中的一个字符串 |`a{abc,xyz,123}b`,`a`与`b`之间只能是`abc`或`xyz`或`123`这三个字符串之一|
| `[[:class:]]`| 匹配任意一个属于指定字符类中的字符 | `*[[:lower:]123]`,以小写字母开头,或者以`1`,`2`,`3`结尾的文件 |

常用字符类

+ `[:alnum:]` : 匹配任意一个字母或数字
+ `[:alpha:]` :  匹配任意一个字母
+ `[:digit:]` : 匹配任意一个数字
+ `[:lower:]` : 匹配任意一个小写字母
+ `[:upper:]` : 匹配任意一个大写字母

### 转义字符

有的时候,我们匹配的内容里面会存在 `*`,`?`,`[`等通配符中的符号.
为了表示他们原来的意思,我们需要使用转义字符 `\`,如 `a\[ac\]c` 表示匹配 `a[a]c` 或 `a[c]c`.

`\ `本身用` \\` 表示.

### 字符切割

分字 word splitting

[Shell_Linux Shell 中实现字符串切割的几种方法](ttps://blog.csdn.net/u010003835/article/details/80750003)
[refs1](https://blog.csdn.net/u010003835/article/details/80749220)
[refs2](https://blog.csdn.net/whuslei/article/details/7187639)

***
`shell` 的 `for` 参数可以是一个连续的字符串,用`IFS`分割

```bash
#!/bin/bash
string="hello shell split test"  ; for var in ${string[@]}; do echo -e "$var EOF" ; done
####
echo test2
string="hello shell split test"
for var in ${string}
do
   echo -e "$var EOF"
done
```

***
我们在 `shell` 脚本编程中,经常需要用到字符串切割,即将字符串切割为一个数组,
类似 `java` 中的`split`函数,下面对几种常见的方式做一个总结.

+ 利用 `shell` 中 变量 的字符串替换
+ 设置分隔符,通过 `IFS` 变量
+ 利用`tr` 指令实现字符替换  (!只能针对单个分隔符)

***
方法一:利用 `shell` 中变量的字符串替换

示例:

```bash
#!/bin/bash
string="hello,shell,split,test"
array=(${string//,/ })

for var in ${array[@]}
do
   echo -e "$var \n"
done
```

***
方法二: 设置分隔符,通过 `IFS `变量

原理:自定义IFS变量, 改变分隔符, 对字符串进行切分

`IFS` 介绍

`Shell` 脚本中有个变量叫 `IFS`(Internal Field Seprator) ,**内部域分隔符**.

`Shell` 的环境变量分为 `set`, `env` 两种,其中 `set` 变量可以通过 `export` 工具导入到 `env` 变量中.
其中,`set` 是显示设置 `shell` 变量,仅在本 `shell` 中有效；`env` 是显示设置用户环境变量 ,仅在当前会话中有效.
换句话说,`set` 变量里包含了 `env` 变量,但 `set` 变量不一定都是 `env` 变量.
这两种变量不同之处在于变量的作用域不同.显然,`env` 变量的作用域要大些,它可以在 `subshell` 中使用.

而 `IFS` 是一种 `set` 变量,当 `shell` 处理"命令替换"和"参数替换"时, `shell` 根据 `IFS` 的值,默认是 `space`, `tab`, `newline` 来拆解读入的变量,然后对特殊字符进行处理,最后重新组合赋值给该变量.

***
`IFS` 简单实例

查看变量 `IFS` 的值.

```bash
$ echo $IFS

$ echo "$IFS" | od -b
0000000 040 011 012 012
0000004
```

直接输出IFS是看不到的,把它转化为二进制就可以看到了, `040`是空格,`011`是`Tab`,`012`是换行符`\n` .
最后一个 `012` 是 `echo`输出的(`echo` 默认会换行的).

```bash
SAVEIFS=$(echo -en "\0040\0011\0012"); # 默认的IFS
```

示例

```bash
#!/bin/bash

string="hello,shell,split,test"

#对IFS变量 进行替换处理
OLD_IFS="$IFS"
IFS=","
array=($string)
IFS="$OLD_IFS"

for var in ${array[@]}
do
   echo -e $var\n
done
```

***
方法三: 利用`tr`指令实现字符替换

原理: 由于只是对单个字符进行的替换,则可以用  `echo args |   tr "oldSpilt" "newSpilt"`  的方式实现.

`tr` 指令讲解: `tr`命令可以对来自标准输入的字符进行替换,压缩和删除.

语法:`tr(选项)(参数)`

选项

+ `-c`或`--complerment`:取代所有不属于第一字符集的字符；
+ `-d`或`--delete`:删除所有属于第一字符集的字符；
+ `-s`或`--squeeze-repeats`:把连续重复的字符以单独一个字符表示；
+ `-t`或`--truncate-set1`:先删除第一字符集较第二字符集多出的字符.

参数

+ `字符集1`:指定要转换或删除的原字符集.当执行转换操作时,必须使用参数`字符集2`指定转换的目标字符集.
但执行删除操作时,不需要参数`字符集2`；
+ `字符集2`:指定要转换成的目标字符集.

示例:

```bash
#!/bin/bash

string="hello,shell,split,test"
array=(`echo $string | tr ',' ' '` )

for var in ${array[@]}
do
   echo -e $var
done
```

cmdline 第七章 重定向

## 重定向

+ `cat` --连接文件
+ `sort`--排序文本行
+ `uniq`--报道或忽略重复行
+ `grep`--打印匹配行
+ `wc`--打印文件中换行府, 字, 和字节个数
+ `head`--输出文件第一部分
+ `tail`--输出文件最后一部分

### 标准输入, 输出和错误

Unix`一切皆文件`, 程序, 比如说`ls`, 把它们的运行结果输送到一个叫做标准输出(`stdout`)的特殊文件, 
把它们的状态信息送到另一个叫做标准错误(`stderr`)的文件. 默认情况下, 标准输出和标准错误都连接到屏幕, 而不是保存到磁盘文件. 

此外, 许多程序从一个叫做标准输入(`stdin`)的设备得到输入. 默认情况下, 标准输入连接到键盘. 

`I/O` 重定向允许我们可以更改输出走向和输入来向. 
一般地,输出送到屏幕,输入来自键盘, 但是通过 `I/O` 重定向,我们可以改变输入输出方向. 

### 重定向标准输出

`I/O` 重定向允许我们来重定义标准输出送到哪里. 

重定向标准输出到另一个文件,我们使用 `>` 重定向符,其后跟着文件名. 
把重定向结果追加到文件内容后面,而不是从开头重写文件, 我们使用`>>`重定向符,像这样:

```bash
ls -l /usr/bin >> ls-output.txt
```

### 重定向标准错误

重定向标准错误缺乏专用的重定向操作符. 重定向标准错误,我们必须参考它的文件描述符.  
一个程序可以在几个编号的文件流中的任一个上产生输出. 
然而我们必须把这些文件流的前三个看作标准输入,输出和错误, shell内部给它们的文件描述符分别为`0`,`1`和`2`. 

可能有这种情况,我们希望捕捉一个命令的所有输出到一个文件. 
为了完成这个,我们 必须同时重定向标准输出和标准错误. 有两种方法. 

第一个,传统的方法, 在旧版本 shell 中也有效:

```bash
ls -l /bin/usr > ls-output.txt 2>&1
```

使用这种方法,我们完成两个重定向. 
首先重定向标准输出到文件 `ls-output.txt`,然后 重定向文件描述符`2`(标准错误)到文件描述符`1`(标准输出)使用表示法`2>&1`. 注意重定向的顺序安排非常重要. 标准错误的重定向必须总是出现在标准输出重定向之后,要不然它不起作用. 

现在的 bash 版本提供了第二种方法,更精简合理的方法来执行这种联合的重定向. 

```bash
ls -l /bin/usr &> ls-output.txt
```

在这个例子里面,我们使用单单一个表示法 `&>` 来重定向标准输出和错误到文件 `ls-output.txt`. 

### 处理不需要的输出

有时候`沉默是金`,我们不想要一个命令的输出结果,只想把它们扔掉. 

系统为我们提供了解决问题的方法,通过重定向输出结果 到一个特殊的叫做`/dev/null`的文件. 
这个文件是系统设备,叫做位存储桶,它可以接受输入,并且对输入不做任何处理. 
为了隐瞒命令错误信息,我们这样做:

```bash
ls -l /bin/usr 2> /dev/null
```

>Unix 文化中的/dev/null
>位存储桶是个古老的 Unix 概念,由于它的普遍性,它的身影出现在 Unix 文化的 许多部分. 
>当有人说他/她正在发送你的评论到`/dev/null`,现在你应该知道那是 什么意思了. 
>更多的例子,可以阅读 Wikipedia 关于`/dev/null`的文章. 

### 重定向标准输入

`cat - 连接文件`

`cat `命令读取一个或多个文件,然后复制它们到标准输出,就像这样:`cat [file]`

`cat` 经常被用来显示简短的文本文件. 因为 `cat` 可以 接受不只一个文件作为参数,所以它也可以用来把文件连接在一起. 

比方说我们下载了一个大型文件,这个文件被分离成多个部分(`USENET` 中的多媒体文件经常以这种方式分离), 我们想把它们连起来. 

```bash
cat movie.mpeg.0* > movie.mpeg
```

如果 `cat` 没有给出任何参数,它会从标准输入读入数据,因为标准输入,默认情况下,连接到键盘. 
 它正在等待我们输入数据!试试这个:

```bash
cat
The quick brown fox jumped over the lazy dog.
```

下一步,输入 `Ctrl-d`(按住 `Ctrl` 键同时按下`d`),来告诉 `cat`,在标准输入中, 它已经到达文件末尾(EOF):
由于文件名参数的缺席,`cat` 复制标准输入到标准输出,所以我们看到文本行重复出现.  
我们可以使用这种行为来创建简短的文本文件. 比方说,我们想创建一个叫做`lazy_dog.txt` 的文件,这个文件包含例子中的文本. 
我们这样做:

```bash
cat > lazy_dog.txt
The quick brown fox jumped over the lazy dog.
```

输入命令,其后输入要放入文件中的文本. 记住,最后输入`Ctrl-d`. 通过使用这个命令,我们实现了世界上最低能的文字处理器!

现在我们知道怎讲接受标准输入:

```bash
cat < lazy_dog.txt
The quick brown fox jumped over the lazy dog.
```

使用`<`重定向操作符,我们把标准输入源从键盘改到文件 `lazy_dog.txt`. 

`2.05b`版本以后, `bash`可以用下列语法重定向标准输入至字符串(称为here string)：

```bash
command <<< "string to be read as standard input"
```

如果字符串包括空格就需要用引号包裹字符串

### 管道线

命令可以从**标准输入**读取数据,然后再把数据输送到**标准输出**,
命令的这种能力被 一个 `shell` 特性所利用,这个特性叫做管道线. 

使用管道操作符`|`(竖杠),一个命令的标准输出可以管道到另一个命令的标准输入:

```bash
command1 | command2
```

我们用 less 来一页一页地显示任何命令的输出,命令把它的运行结果输送到标准输出:

```bash
ls -l /usr/bin | less
```

这极其方便!使用这项技术,我们可以方便地检测会产生标准输出的任一命令的运行结果. 

### 过滤器

管道线经常用来对数据完成复杂的操作. 有可能会把几个命令放在一起组成一个管道线.  
通常,以这种方式使用的命令被称为过滤器. 过滤器接受输入,以某种方式改变它,然后 输出它. 

#### sort

第一个我们想试验的过滤器是 `sort`. 

```bash
ls /bin /usr/bin | sort | less
```

因为我们指定了两个目录`(/bin` 和`/usr/bin`),`ls` 命令的输出结果由有序列表组成, 各自针对一个目录. 
通过在管道线中包含 `sort`,我们改变输出数据,从而产生一个 有序列表. 

### uniq - 报道或忽略重复行

`uniq` 命令经常和 `sort` 命令结合在一起使用. 
`uniq` 从标准输入或单个文件名参数接受数据有序列表(详情查看`uniq`手册页),默认情况下,从数据列表中删除任何重复行. 

```bash
ls /bin /usr/bin | sort | uniq | less
```

在这个例子中,我们使用 `uniq` 从 `sort` 命令的输出结果中,来删除任何重复行. 

如果我们想看到 重复的数据列表,让 uniq 命令带上`-d`选项,就像这样:

```bash
ls /bin /usr/bin | sort | uniq -d | less
```

### wc - 打印行,字和字节数

`wc`(字计数)命令是用来显示文件所包含的行,字和字节数. 例如:

```bash
wc ls-output.txt
7902 64566 503634 ls-output.txt
```

`wc` 打印出来三个数字:包含在文件 `ls-output.txt` 中的行数,单词数和字节数,.
正如我们先前的命令,如果 `wc` 不带命令行参数,它接受标准输入. 
`-l`选项限制命令输出只能报道行数. 

添加 `wc` 到管道线来统计数据,是个很便利的方法. 

```bash
ls /bin /usr/bin | sort | uniq | wc -l
2728
```

### grep - 打印匹配行

grep 是个很强大的程序,用来找到文件中的匹配文本. 这样使用 `grep` 命令:

```bash
grep pattern [file...]
```

当 grep 遇到一个文件中的匹配`模式`,它会打印出包含这个类型的行. 
grep 能够匹配的模式可以很复杂(正则表达式),但是现在我们把注意力集中在简单文本匹配上面. 

比如说,我们想在我们的程序列表中,找到文件名中包含单词`zip`的所有文件. 
这样一个搜索, 可能让我们了解系统中的一些程序与文件压缩有关系. 这样做:

```bash
ls /bin /usr/bin | sort | uniq | grep zip
```

grep 有一对方便的选项:
`-i`导致 `grep` 忽略大小写当执行搜索时(通常,搜索是大小写 敏感的),
`-v`选项会告诉 `grep` 只打印不匹配的行. 

### head/tail - 打印文件开头部分/结尾部分

有时候你不需要一个命令的所有输出. 可能你只想要前几行或者后几行的输出内容. 
`head` 命令打印文件的前十行,而 `tail` 命令打印文件的后十行. 
默认情况下,两个命令 都打印十行文本,但是可以通过`-n`选项来调整命令打印的行数. 

```bash
head -n 5 ls-output.txt
tail -n 5 ls-output.txt
```

它们也能用在管道线中:

```bash
ls /usr/bin | tail -n 5
```

`tail` 有一个选项允许你实时的浏览文件. 当观察日志文件的进展时,这很有用,因为它们同时在被写入. 

在以下的例子里,我们要查看目录`/var/log`里面的信息文件. 
在 一些 Linux 发行版中,要求有超级用户权限才能阅读这些文件,因为文件`/var/log/messages`可能包含安全信息. 

```bash
tail -f /var/log/messages
Feb 8 13:40:05 twin4 dhclient: DHCPACK from 192.168.1.1
```

使用`-f`选项,`tail`命令继续监测这个文件,当新的内容添加到文件后,它们会立即出现在屏幕上. 
这会一直继续下去直到你输入`Ctrl-c`. 

### tee - 从 Stdin 读取数据,并同时输出到 Stdout 和文件

为了和我们的管道隐喻保持一致,Linux 提供了一个叫做 `tee` 的命令,这个命令制造了 一个`tee`,安装到我们的管道上. 

`tee` 程序从标准输入读入数据,并且同时复制数据到**标准输出**(相当于允许数据继续随着管道线流动)和一个或多个文件. 
当在某个中间处理阶段来捕捉一个管道线的内容时,这很有帮助. 

这里,我们重复执行一个先前的例子, 这次包含 `tee` 命令,在 `grep` 过滤管道线的内容之前,来捕捉整个目录列表到文件 ls.txt:

```bash
ls /usr/bin | tee ls.txt | grep zip
bunzip2
bzip2
....
```

## 从shell眼中看世界

### 字符展开

传递到 echo 命令的任一个参数都会在(屏幕上)显示出来.  让我们试一个例子:

```bash
echo *
Desktop Documents ls-output.txt Music Pictures Public Templates Videos
```

为什么 `echo` 不打印` * `呢?答案就是在 `echo` 命令被执行前, shell 把` * `展开成了另外的东西(在这种情况下,就是在当前工作目录下的文件名字). 

当回车键被按下时,`shell` 在命令被执行前在命令行上自动展开任何符合条件的字符, 所以 `echo` 命令从不会发现` * `,只把它展开成结果. 
知道了这个以后,我们能看到 `echo` 执行的结果和我们想象的一样. 

### 路径名展开

这种通配符工作机制叫做路径名展开. 我们能够执行以下参数展开模式:

```bash
echo D*
Desktop Documents
```

和:

```bash
echo *s
Documents Pictures Templates Videos
```

```bash
甚至是:
echo [[:upper:]]*
Desktop Documents Music Pictures Public Templates Videos
```

查看家目录之外的目录:

```bash
echo /usr/*/share
/usr/kerberos/share /usr/local/share
```

### 隐藏文件路径名展开

正如我们知道的,以圆点字符开头的文件名是隐藏文件. 路径名展开也尊重这种 行为. 像这样的展开:

```bash
echo *
```

不会显示隐藏文件. 要是展开模式以一个圆点开头,我们就能够在展开模式中包含隐藏文件, 
而且隐藏文件可能会出现在第一位置,就像这样:

```bash
echo .*
```

然而,如果我们仔细检查一下输出结果,我们会看到名字`.` 和`..`也出现在结果中. 
因为这些名字是指当前工作目录和它的父目录,使用这种 模式可能会产生不正确的结果. 
我们能看到这样的结果,如果我们试一下这个命令:
`ls -d .* | less`
为了在这种情况下正确地完成路径名展开,我们应该使用一个更精确些的模式.  这个模式会正确地工作:
`ls -d .[!.]?*`
这种模式展开成为文件名,每个文件名以圆点开头,第二个字符不包含圆点,再包含至少一个字符, 并且这
个字符之后紧接着任意多个字符. 

这将列出大多数的隐藏文件 (但仍将不能包含以多个圆点开头的文件名)

这个带有 `-A` 选项(`几乎所有`)的`ls`命令能够提供一份正确的隐藏文件清单:

```bash
ls -A
```

### 波浪线展开

可能你从我们对 cd 命令的介绍中回想起来,波浪线字符(` ~ `)有特殊的意思. 
当它用在 一个单词的开头时,它会展开成指定用户的家目录名,如果没有指定用户名,则是当前用户的家目录:

```bash
echo ~
/home/me
```

如果有用户`foo`这个帐号,然后:

```bash
echo ~foo
/home/foo
```

### 算术表达式展开

`shell` 允许算术表达式通过展开来执行. 这允许我们把 `shell` 提示当作计算器来使用:

```bash
echo $((2 + 2))
4
```

算术表达式展开使用这种格式:`$((expression))`

算术表达式只支持整数(全部是数字,不带小数点),但是能执行很多不同的操作. 
这里是 一些它支持的操作符:

***
操作符 说明

+ `+` 加
+ `-` 减
+ `*` 乘
+ `/` 除(但是记住,因为展开只是支持整数除法,所以结果是整数. )
+ `%` 取余,只是简单的意味着,`余数`
+ `**` 取幂

在算术表达式中空格并不重要,并且表达式可以嵌套. 例如,5的平方乘以3:

```
echo $(($((5**2)) * 3))
```

一对括号可以用来把多个子表达式括起来. 通过这个技术,我们可以重写上面的例子:

```bash
echo $(((5**2) * 3))
```

这是一个使用除法和取余操作符的例子. 注意整数除法的结果:

```bash
echo Five divided by two equals $((5/2))
Five divided by two equals 2
echo with $((5%2)) left over.
with 1 left over.
```

在35章会更深入的讨论算术表达式的内容. 

### 花括号展开

可能最奇怪的展开是花括号展开. 通过它,你可以从一个包含花括号的模式中创建多个文本字符串. 例:

```bash
echo Front-{A,B,C}-Back
Front-A-Back Front-B-Back Front-C-Back
```

花括号展开模式可能包含一个开头部分叫做报头,一个结尾部分叫做附言. 
花括号表达式本身可能包含一个由逗号分开的字符串列表,或者一系列整数,或者单个的字符串. 
这种模式不能嵌入空白字符. 这个例题使用了一系列整数:

```bash
echo Number_{1..5}
```

一系列以倒序排列的字母:

```bash
echo {Z..A}
```

花括号展开可以嵌套(会自动展平):

```bash
echo a{A{1,2},B{3,4}}b
```

那么这对什么有好处呢?最普遍的应用是,创建一系列的文件或目录列表. 

```bash
mkdir {2007..2009}-0{1..9} {2007..2009}-{10..12}
```

### 参数展开

在这一章我们将会简单地介绍参数展开,只是皮毛而已. 
这个特性在 `shell` 脚本中比直接在命令行中更有用. 它的许多性能和系统存储小块数据,并给每块数据命名的能力有关系. 

许多像这样的小块数据, 更适当些应叫做变量,可以方便地检查它们. 
例如,叫做`USER`的变量包含你的用户名. 唤醒参数展开,揭示 `USER` 中的内容,可以这样做:

```bash
echo $USER
me
```

查看有效的变量列表,试试这个:

```bash
printenv | less
```

你可能注意到其它展开类型,如果你误输入一个模式,展开就不会发生. 
这时 `echo` 命令只简单地显示误键入的模式. 通过参数展开,如果你拼写错了一个变量名, 展开仍然会进行,只是展成一个空字符串:

```bash
echo $SUER
```

### 命令替换

命令替换允许我们把一个命令的输出作为一个展开模式来使用:

```bash
echo $(ls)
```

我最喜欢用的一行命令是像这样的:

```bash
ls -l $(which cp)
-rwxr-xr-x 1 root root 71516 2007-12-05 08:58 /bin/cp
```

这里我们把 `which cp` 的执行结果作为一个参数传递给 `ls` 命令,因此要想得到 `cp` 程序的输出列表,
不必知道它完整的路径名. 

我们不只限制于简单命令. 也可以使用整个管道线 (只展示部分输出):

```bash
file $(ls /usr/bin/* | grep zip)
....
```

在这个例子中,管道线的输出结果成为 `file` 命令的参数列表. 

在旧版 shell 程序中,有另一种语法也支持命令替换,可与刚提到的语法换使用. 
`bash` 也支持这种语法. 它使用倒引号来代替美元符号和括号:

```bash
ls -l `which cp`
```

我们已经知道 shell 有许多方式可以完成展开,现在是时候学习怎样来控制展开了.  

### 双引号

我们将要看一下引用的第一种类型,双引号. 如果你把文本放在双引号中, `shell` 使用的特殊字符,
除了`\`(反斜杠),`$` ,和 `` ` ``(倒引号)之外, 则失去它们的特殊含义,被当作普通字符来看待. 

这意味着

+ 单词分割, (`空格`)
+ 路径名展开, (`*``?`)
+ 波浪线展开,(`~`)
+ 和花括号展开(`{}`)

都被禁止,然而

+ 参数展开(`$USER`)
+ 算术展开(`$(())`)
+ 命令替换`$()`

仍然执行. 
使用双引号,我们可以处理包含空格的文件名. 比方说我们是不幸的名为 `two words.txt` 文件的受害者. 
如果我们试图在命令行中使用这个 文件,**单词分割机制**会导致这个文件名被看作两个独自的参数,而不是所期望的单个参数:

```bash
ls -l two words.txt
ls: cannot access two: No such file or directory
ls: cannot access words.txt: No such file or directory
```

使用双引号,我们可以阻止单词分割,得到期望的结果;进一步,我们甚至可以修复 破损的文件名. 

```bash
ls -l `two words.txt`
mv `two words.txt` two_words.txt
```

记住,在双引号中,**参数展开**,**算术表达式展开**,和**命令替换**仍然有效:

```bash
echo `${USER} $((2+2)) $(cal)`
me 4
```

在默认情况下,单词分割机制会在单词中寻找**空格**,**制表符**,和**换行符**,并把它们看作单词之间的界定符. 它们只作为分隔符使用. 

如果我们加上双引号,单词分割被禁止,内嵌的空格也不会被当作界定符,它们成为参数的一部分.  
一旦加上双引号,我们的命令行就包含一个带有一个参数的命令. 

考虑下面的例子:

```bash
echo $(cal)
echo `$(cal)`
```

在第一个实例中,没有引用的命令替换导致命令行包含`38`个参数. 
在第二个例子中, 命令行只有一个参数,参数中包括嵌入的**空格**和**换行符**. 

### 单引号

如果需要禁止所有的展开,我们使用单引号. 以下例子是无引用,双引号,和单引号的比较结果:

```bash
echo text ~/*.txt {a,b} $(echo foo) $((2+2)) $USER
echo `text ~/*.txt {a,b} $(echo foo) $((2+2)) $USER`
echo 'text ~/*.txt {a,b} $(echo foo) $((2+2)) $USER'
```

正如我们所看到的,随着引用程度加强,越来越多的展开被禁止. 

### 转义字符

有时候我们只想引用单个字符. 我们可以在字符之前加上一个反斜杠,在这个上下文中叫做转义字符. 
经常在双引号中使用转义字符,来有选择地阻止展开. 

```bash
echo `The balance for user $USER is: \$5.00`
The balance for user me is: $5.00
```

使用转义字符来消除文件名中一个字符的特殊含义,是很普遍的. 例如,在文件名中可能使用一些对于 `shell` 来说,有特殊含义的字符. 
这些字符包括`$`, `!`, `空格`等字符. 
注意在单引号中,反斜杠失去它的特殊含义,它 被看作普通字符. 

### 反斜杠转义字符序列

反斜杠除了作为转义字符外,反斜杠也是一种表示法的一部分,这种表示法代表某种特殊字符,叫做控制码. 
`ASCII`编码表中前`32`个字符被用来把命令转输到像电报机一样的设备. 

一些编码是众所周知的(制表符,退格符,换行符,和回车符),其它一些编码就不熟悉了(空值,传输结束码,和确认). 

***
转义序列 含义

+ `\a` : 响铃(`警告`-导致计算机嘟嘟响)
+ `\b` : 退格符
+ `\n` : 新的一行. 在类 Unix 系统中,产生换行. 
+ `\r` : 回车符
+ `\t` : 制表符

上表列出了一些常见的反斜杠转义字符. 
反斜杠表示法背后的思想来源于 C 编程语言, 许多其它语言也采用了这种表示方法,包括 shell. 

`echo` 命令带上 `-e` 选项,能够解释转义序列. 你可以把转义序列放在`$' '`里面.  

以下例子,使用 `sleep` 命令,一个简单的程序,它会等待指定的秒数,然后退出. 
我们可以创建一个简单的倒数计数器:

```bash
sleep 2; echo -e `Time's up\a`
```

我们也可以这样做:

```bash
sleep 2; echo `Time's up` /span>\a'
```

## 键盘高级操作技巧

以下命令将会露面：

+ `clear`
+ `history`

### 命令行编辑

Bash使用了一个名为`Readline`的库(共享的线程集合, 可以被不同的程序使用), 来实现命令行编辑. 

利用历史命令

```bash
history | grep /usr/bin
```

可以通过类似`!88`的形式, 引用历史命令. 
