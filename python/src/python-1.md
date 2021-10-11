# python-1.md

ref: [这是小白的Python新手教程](https://www.liaoxuefeng.com/wiki/1016959663602400)

## Python解释器

括号表达式可以断行, 普通断行用 `\`转义

当我们编写Python代码时, 我们得到的是一个包含`Python`代码的以`.py`为扩展名的文本文件. 要运行代码, 就需要`Python`解释器去执行`.py`文件. 

在命令行模式下敲命令`python`, 就看到类似如下的一堆文本输出, 然后就进入到`Python交互模式`, 它的提示符是`>>>`. 
在Python交互模式下输入`exit()`并回车, 就退出了`Python`交互模式, 并回到命令行模式

如果要让`Python`打印出指定的文字, 可以用`print()`函数, 然后把希望打印的文字用单引号或者双引号括起来, 但不能混用单引号和双引号：

```python
>>> print('hello, world')
hello, world
```

这种用单引号或者双引号括起来的文本在程序中叫字符串, 今后我们还会经常遇到. 
在命令行模式下, 可以执行`python`进入`Python`交互式环境, 也可以执行`python hello.py`运行一个`.py`文件. 
执行一个`.py`文件只能在命令行模式执行. 
想要输出结果, 必须自己用`print()`打印出来

能不能像`.exe`文件那样直接运行`.py`文件呢？在Windows上是不行的, 但是, 在Mac和Linux上是可以的, 方法是在`.py`文件的第一行加上一个特殊的注释：

**python env 设置**

```python
#!/usr/bin/env python3
print('hello, world')
```

然后, 通过命令给`hello.py`以执行权限：

```bash
$ chmod a+x hello.py
nothing
```

就可以直接运行`hello.py`了

### Python空行

[Python 基础语法][]

[Python 基础语法]: https://www.runoob.com/python/python-basic-syntax.html

**函数之间或类的方法之间用空行分隔, 表示一段新的代码的开始**. 类和函数入口之间也用一行空行分隔, 以突出函数入口的开始. 

空行与代码缩进不同, 空行并不是Python语法的一部分. 书写时不插入空行, Python解释器运行也不会出错. 但是空行的作用在于分隔两段不同功能或含义的代码, 便于日后代码的维护或重构. 

记住：空行也是程序代码的一部分. 

#### 行和缩进

学习 Python 与其他语言最大的区别就是, Python 的代码块不使用大括号 `{}` 来控制类, 函数以及其他逻辑判断. 
python 最具特色的就是用缩进来写模块. 

缩进的空白数量是可变的, 但是所有代码块语句必须包含相同的缩进空白数量, 这个必须严格执行. 

### 多行语句

Python语句中一般以新行作为语句的结束符. 

但是我们可以使用斜杠( `\`)将一行的语句分为多行显示, 如下所示：

```python
total = item_one + \
        item_two + \
        item_three
```

语句中包含 `[]`, `{}` 或 `()` 括号就不需要使用多行连接符. 如下实例：

```python
days = ['Monday', 'Tuesday', 'Wednesday',
        'Thursday', 'Friday']
```

### 输出

`print()`函数也可以接受多个字符串, 用逗号“,”隔开, 就可以连成一串输出：

```python
>>> print('The quick brown fox', 'jumps over', 'the lazy dog')
The quick brown fox jumps over the lazy dog
```

`print()`会依次打印每个字符串, 遇到逗号`,`会输出一个空格, 因此, 输出的字符串是这样拼起来的

### 输入

`Python`提供了一个`input()`, 可以让用户输入字符串, 并存放到一个变量里. 比如输入用户的名字：

```python
>>> name = input()
Michael
```

当你输入`name = input()`并按下回车后, `Python`交互式命令行就在等待你的输入了. 
这时, 你可以输入任意字符, 然后按回车后完成输入. 

输入完成后, 不会有任何提示, `Python`交互式命令行又回到`>>>`状态了. 那我们刚才输入的内容到哪去了？答案是存放到`name`变量里了. 可以直接输入`name`查看变量内容：

```python
>>> name
'Michael'
```

`input()`可以让你显示一个字符串来提示用户, 于是我们把代码改成：

```python
name = input('please enter your name: ')
print('hello,', name)
```

再次运行这个程序, 你会发现, 程序一运行, 会首先打印出`please enter your name`:, 这样, 用户就可以根据提示, 输入名字后, 得到`hello, xxx`的输出

## Python基础

`Python`的语法比较简单, 采用缩进方式, 写出来的代码就像下面的样子：

```python
# print absolute value of an integer:
a = 100
if a >= 0:
    print(a)
else:
    print(-a)
```

以`#`开头的语句是注释, 注释是给人看的, 可以是任意内容, 解释器会忽略掉注释. 
其他每一行都是一个语句, 当语句以冒号`:`结尾时, 缩进的语句视为代码块. 

缩进有利有弊. 好处是强迫你写出格式化的代码, 但没有规定缩进是几个空格还是`Tab`. 按照约定俗成的惯例, 应该始终坚持使用`4`个空格的缩进. 
缩进的另一个好处是强迫你写出缩进较少的代码, 你会倾向于把一段很长的代码拆分成若干函数, 从而得到缩进较少的代码. 
缩进的坏处就是“复制－粘贴”功能失效了, 这是最坑爹的地方. 
当你重构代码时, 粘贴过去的代码必须重新检查缩进是否正确. 
此外, IDE很难像格式化`Java`代码那样格式化`Python`代码. 
最后, 请务必注意, Python程序是大小写敏感的, 如果写错了大小写, 程序会报错. 

### 数据类型和变量

在Python中, 能够直接处理的数据类型有以下几种：

#### 整数

Python可以处理任意大小的整数, 当然包括负整数, 在程序中的表示方法和数学上的写法一模一样, 例如：`1`, `100`, `-8080`, `0`, 等等. 
计算机由于使用二进制, 所以, 有时候用十六进制表示整数比较方便, 十六进制用`0x`前缀和`0-9`, `a-f`表示, 例如：`0xff00`, `0xa5b4c3d2`, 等等. 

#### 浮点数

浮点数也就是小数, 之所以称为浮点数, 是因为按照科学记数法表示时, 一个浮点数的小数点位置是可变的, 比如, $1.23x10^9$和$12.3x10^8$是完全相等的. 
浮点数可以用数学写法, 如`1.23`, `3.14`, `-9.01`, 等等. 
但是对于很大或很小的浮点数, 就必须用科学计数法表示, 把`10`用`e`替代, $1.23x10^9$就是`1.23e9`, 或者`12.3e8`, `0.000012`可以写成`1.2e-5`, 等等. 

整数和浮点数在计算机内部存储的方式是不同的, 整数运算永远是精确的(除法难道也是精确的？是的！), 而浮点数运算则可能会有四舍五入的误差. 

#### 字符串

字符串是以单引号`'`或双引号`"`括起来的任意文本, 比如`'abc'`, `"xyz"`等等. 请注意, `''`或`""`本身只是一种表示方式, 不是字符串的一部分, 因此, 字符串`'abc'`只有`a`, `b`, `c`这3个字符. 
如果`'`本身也是一个字符, 那就可以用`""`括起来, 比如`"I'm OK"`包含的字符是`I`, `'`, `m`, `空格`, `O`, `K`这6个字符. 

如果字符串内部既包含`'`又包含`"`怎么办？可以用转义字符`\`来标识, 比如：
`'I\'m \"OK\"!'`
表示的字符串内容是：
`I'm "OK"!`

转义字符\可以转义很多字符, 比如`\n`表示换行, `\t`表示制表符, 字符`\`本身也要转义, 所以`\\`表示的字符就是`\`, 可以在Python的交互式命令行用`print()`打印字符串看看：

```python
>>> print('I\'m ok.')
I'm ok.
>>> print('I\'m learning\nPython.')
I'm learning
Python.
>>> print('\\\n\\')
\
\
```

如果字符串里面有很多字符都需要转义, 就需要加很多`\`, 为了简化, Python还允许用`r'text'`表示`text`字符串默认不转义, 可以自己试试：

```python
>>> print('\\\t\\')
\       \
>>> print(r'\\\t\\')
\\\t\\
```

如果字符串内部有很多换行, 用\n写在一行里不好阅读, 为了简化, Python允许用`'''...'''`的格式表示多行内容, 可以自己试试：

```python
>>> print('''line1
... line2
... line3''')
line1
line2
line3
```

上面是在交互式命令行内输入, 注意在输入多行内容时, 提示符由`>>>`变为`...`, 提示你可以接着上一行输入, 注意`...`是提示符, 不是代码的一部分：

当输入完结束符`'''`和括号`)`后, 执行该语句并打印结果. 

如果写成程序并存为`.py`文件, 就是：

```python
print('''line1
line2
line3''')
```

多行字符串`'''...'''`还可以在前面加上`r`使用

#### 布尔值

布尔值和布尔代数的表示完全一致, 一个布尔值只有`True`, `False`两种值, 
要么是`True`, 要么是`False`, 在Python中, 可以直接用`True`, `False`表示布尔值(请注意大小写), 也可以通过布尔运算计算出来：

```python
>>> True
True
>>> False
False
>>> 3 > 2
True
>>> 3 > 5
False
```

布尔值可以用`and`, `or`和`not`运算. 

```python
>>> True and True
True
>>> True and False
False
>>> False and False
False
>>> 5 > 3 and 3 > 1
True
```

布尔值经常用在条件判断中, 比如：

```python
if age >= 18:
    print('adult')
else:
    print('teenager')
```

##### 空值

空值是Python里一个特殊的值, 用`None`表示. `None`不能理解为`0`, 因为`0`是有意义的, 而`None`是一个特殊的空值. 
此外, Python还提供了列表, 字典等多种数据类型, 还允许创建自定义数据类型, 我们后面会继续讲到. 

### 变量

变量的概念基本上和初中代数的方程变量是一致的, 只是在计算机程序中, 变量不仅可以是数字, 还可以是任意数据类型. 
变量在程序中就是用一个变量名表示了, 变量名必须是大小写`英文`, `数字`和`_`的组合, 且不能用数字开头, 比如：

```python
Answer = True
```

在Python中, 等号`=`是赋值语句, 可以把任意数据类型赋值给变量, 
同一个变量可以反复赋值, 而且可以是不同类型的变量, 例如：

```python
a = 123 # a是整数
print(a)
a = 'ABC' # a变为字符串
print(a)
```

这种变量本身类型不固定的语言称之为动态语言, 与之对应的是静态语言. 
静态语言在定义变量时必须指定变量类型, 如果赋值的时候类型不匹配, 就会报错. 
例如Java是静态语言, 赋值语句如下(`//` 表示注释)：

```python
int a = 123; // a是整数类型变量
a = "ABC"; // 错误：不能把字符串赋给整型变量
```

和静态语言相比, 动态语言更灵活, 就是这个原因. 
请不要把赋值语句的等号等同于数学的等号. 

最后, 理解变量在计算机内存中的表示也非常重要. 当我们写：

```python
a = 'ABC'
```

时, Python解释器干了两件事情：

+ 在内存中创建了一个'ABC'的字符串；
+ 在内存中创建了一个名为a的变量, 并把它指向'ABC'. 

也可以把一个变量`a`赋值给另一个变量`b`, 这个操作实际上是把变量`b`指向变量`a`所指向的数据, 例如下面的代码：

#### 常量

所谓常量就是不能变的变量, 比如常用的数学常数$\pi$就是一个常量. 
在Python中, 通常用全部大写的变量名表示常量：

```python
PI = 3.14159265359
```

但事实上`PI`仍然是一个变量, Python根本没有任何机制保证PI不会被改变, 
所以, 用全部大写的变量名表示常量只是一个习惯上的用法, 如果你一定要改变变量`PI`的值, 也没人能拦住你. 

最后解释一下整数的除法为什么也是精确的. 在Python中, 有两种除法, 一种除法是`/`：

```python
>>> 10 / 3
3.3333333333333335
```

`/`除法计算结果是浮点数, 即使是两个整数恰好整除, 结果也是浮点数：

```python
>>> 9 / 3
3.0
```

还有一种除法是`//`, 称为地板除(floor), 两个整数的除法仍然是整数：

```python
>>> 10 // 3
3
```

你没有看错, 整数的地板除`//`永远是整数, 即使除不尽. 要做精确的除法, 使用`/`就可以. 

因为`//`除法只取结果的整数部分, 所以Python还提供一个余数运算, 可以得到两个整数相除的余数：

```python
>>> 10 % 3
1
```

无论整数做`//`除法还是取余数, 结果永远是整数, 所以, 整数运算结果永远是精确的. 

#### 变量小结

Python支持多种数据类型, 在计算机内部, 可以把任何数据都看成一个“对象”, 而变量就是在程序中用来指向这些数据对象的, 对变量赋值就是把数据和变量给关联起来. 

对变量赋值`x = y`是把变量`x`指向真正的对象, 该对象是变量`y`所指向的. 随后对变量`y`的赋值不影响变量`x`的指向. 
注意：Python的整数没有大小限制, 而某些语言的整数根据其存储长度是有大小限制的, 例如`Java`对32位整数的范围限制在`-2147483648-2147483647`. 

`Python`的浮点数也没有大小限制, 但是超出一定范围就直接表示为`inf`(无限大). 

### 字符串和编码

#### 字符编码

字符串比较特殊的是还有一个编码问题

因为计算机只能处理数字, 如果要处理文本, 就必须先把文本转换为数字才能处理. 最早的计算机在设计时采用`8`个比特(`bit`)作为一个字节(`byte`), 所以, 一个字节能表示的最大的整数就是`255`(二进制`11111111`=十进制255), 如果要表示更大的整数, 就必须用更多的字节. 比如两个字节可以表示的最大整数是`65535`, 4个字节可以表示的最大整数是`4294967295`. 

由于计算机是美国人发明的, 因此, 最早只有`127`个字符被编码到计算机里, 也就是大小写英文字母, 数字和一些符号, 这个编码表被称为`ASCII`编码, 比如大写字母`A`的编码是`65`, 小写字母`z`的编码是`122`

因此, `Unicode`应运而生. `Unicode`把所有语言都统一到一套编码里, 这样就不会再有乱码问题了. 

`Unicode`标准也在不断发展, 但最常用的是用两个字节表示一个字符(如果要用到非常偏僻的字符, 就需要4个字节). 现代操作系统和大多数编程语言都直接支持`Unicode`. 

现在, 捋一捋ASCII编码和`Unicode`编码的区别：`ASCII`编码是1个字节, 而`Unicode`编码通常是`2`个字节. 
字母`A`用`ASCII`编码是十进制的`65`, 二进制的`01000001`；
字符`0`用`ASCII`编码是十进制的`48`, 二进制的`00110000`, 注意字符`'0'`和整数`0`是不同的；
汉字`中`已经超出了`ASCII`编码的范围, 用`Unicode`编码是十进制的`20013`, 二进制的`01001110 00101101`. 
你可以猜测, 如果把`ASCII`编码的`A`用`Unicode`编码, 只需要在前面补`0`就可以, 因此, `A`的`Unicode`编码是`00000000 01000001`. 
但是, 如果你写的文本基本上全部是英文的话, 用`Unicode`编码比`ASCII`编码需要多一倍的存储空间, 在存储和传输上就十分不划算. 

所以, 本着节约的精神, 又出现了把`Unicode`编码转化为“可变长编码”的`UTF-8`编码. `UTF-8`编码把一个`Unicode`字符根据不同的数字大小编码成`1-6`个字节, 常用的英文字母被编码成`1`个字节, 汉字通常是`3`个字节, 只有很生僻的字符才会被编码成`4-6`个字节. 如果你要传输的文本包含大量英文字符, 用`UTF-8`编码就能节省空间：

搞清楚了`ASCII`, `Unicode`和`UTF-8`的关系, 我们就可以总结一下现在计算机系统通用的字符编码工作方式：
在计算机内存中, 统一使用Unicode编码, 当需要保存到硬盘或者需要传输的时候, 就转换为UTF-8编码. 
用记事本编辑的时候, 从文件读取的UTF-8字符被转换为Unicode字符到内存里, 编辑完成后, 保存的时候再把Unicode转换为UTF-8保存到文件：

```file
rw-file-utf-8
```

浏览网页的时候, 服务器会把动态生成的Unicode内容转换为UTF-8再传输到浏览器：
所以你看到很多网页的源码上会有类似`<meta charset="UTF-8" />`的信息, 表示该网页正是用的UTF-8编码. 

#### Python的字符串

搞清楚了令人头疼的字符编码问题后, 我们再来研究Python的字符串. 
在最新的Python 3版本中, 字符串是以Unicode编码的, 也就是说, Python的字符串支持多语言, 例如：

```python
>>> print('包含中文的str')
包含中文的str
```

对于单个字符的编码, Python提供了`ord()`函数获取字符的整数表示, `chr()`函数把编码转换为对应的字符：

```python
>>> ord('A')
65
>>> ord('中')
20013
>>> chr(66)
'B'
>>> chr(25991)
'文'
```

如果知道字符的整数编码, 还可以用十六进制这么写`str`：

```python
>>> '\u4e2d\u6587'
'中文'
```

两种写法完全是等价的. 

由于Python的字符串类型是`str`, 在内存中以`Unicode`表示, 一个字符对应若干个字节. 
如果要在网络上传输, 或者保存到磁盘上, 就需要把`str`变为以字节为单位的`bytes`. 

Python对`bytes`类型的数据用带`b`前缀的单引号或双引号表示：

```python
x = b'ABC'
```

要注意区分`'ABC'`和`b'ABC'`, 前者是`str`, 后者虽然内容显示得和前者一样, 但`b'ABC'`的每个字符都只占用一个字节. 

以`Unicode`表示的`str`通过`encode()`方法可以编码为指定的`bytes`, 例如：

```python
>>> 'ABC'.encode('ascii')
b'ABC'
>>> '中文'.encode('utf-8')
b'\xe4\xb8\xad\xe6\x96\x87'
>>> '中文'.encode('ascii')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
UnicodeEncodeError: 'ascii' codec can't encode characters in position 0-1: ordinal not in range(128)
```

纯英文的`str`可以用`ASCII`编码为`bytes`, 内容是一样的, 含有中文的`str`可以用`UTF-8`编码为`bytes`. 
含有中文的`str`无法用`ASCII`编码, 因为中文编码的范围超过了`ASCII`编码的范围, `Python`会报错. 

在`bytes`中, 无法显示为`ASCII`字符的字节, 用`\x##`显示. 

反过来, 如果我们从网络或磁盘上读取了字节流, 那么读到的数据就是`bytes`. 
要把`bytes`变为`str`, 就需要用`decode()`方法：

```python
>>> b'ABC'.decode('ascii')
'ABC'
>>> b'\xe4\xb8\xad\xe6\x96\x87'.decode('utf-8')
'中文'
```

如果`bytes`中包含无法解码的字节, decode()方法会报错：

```python
>>> b'\xe4\xb8\xad\xff'.decode('utf-8')
Traceback (most recent call last):
  ...
UnicodeDecodeError: 'utf-8' codec can't decode byte 0xff in position 3: invalid start byte
```

如果`bytes`中只有一小部分无效的字节, 可以传入`errors='ignore'`忽略错误的字节：

```python
>>> b'\xe4\xb8\xad\xff'.decode('utf-8', errors='ignore')
'中'
```

要计算`str`包含多少个字符, 可以用`len()`函数：

```python
>>> len('ABC')
3
>>> len('中文')
2
```

`len()`函数计算的是`str`的字符数, 如果换成`bytes`, `len()`函数就计算字节数：

```python
>>> len(b'ABC')
3
>>> len(b'\xe4\xb8\xad\xe6\x96\x87')
6
>>> len('中文'.encode('utf-8'))
6
```

可见, `1`个中文字符经过`UTF-8`编码后通常会占用`3`个字节, 而`1`个英文字符只占用`1`个字节. 
在操作字符串时, 我们经常遇到`str`和`bytes`的互相转换. 为了避免乱码问题, 应当始终坚持使用`UTF-8`编码对`str`和`bytes`进行转换. 

由于Python源代码也是一个文本文件, 所以, 当你的源代码中包含中文的时候, 在保存源代码时, 就需要务必指定保存为`UTF-8`编码. 当Python解释器读取源代码时, 为了让它按`UTF-8`编码读取, 我们通常在文件开头写上这两行：

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
```

第一行注释是为了告诉`Linux/OS X`系统, 这是一个`Python`可执行程序, `Windows`系统会忽略这个注释；
第二行注释是为了告诉Python解释器, 按照`UTF-8`编码读取源代码, 否则, 你在源代码中写的中文输出可能会有乱码. 
申明了`UTF-8`编码并不意味着你的`.py`文件就是`UTF-8`编码的, 必须并且要确保文本编辑器正在使用`UTF-8 without BOM`编码：

如果`.py`文件本身使用`UTF-8`编码, 并且也申明了`# -*- coding: utf-8 -*-`, 打开命令提示符测试就可以正常显示中文：

#### 格式化

最后一个常见的问题是如何输出格式化的字符串. 我们经常会输出类似`'亲爱的xxx你好！你xx月的话费是xx, 余额是xx'`之类的字符串, 而`xxx`的内容都是根据变量变化的, 所以, 需要一种简便的格式化字符串的方式.

在Python中, 采用的格式化方式和C语言是一致的, 用%实现, 举例如下：

```python
>>> 'Hello, %s' % 'world'
'Hello, world'
>>> 'Hi, %s, you have $%d.' % ('Michael', 1000000)
'Hi, Michael, you have $1000000.'
```

你可能猜到了, `%`运算符就是用来格式化字符串的. 在字符串内部, `%s`表示用字符串替换, `%d`表示用整数替换, 有几个`%?`占位符, 后面就跟几个变量或者值, 顺序要对应好. 如果只有一个`%?`, 括号可以省略. 

常见的占位符有：

| 占位符 | 替换内容 |
| -----  | -----  |
| `%d` | 整数 |
| `%f` | 浮点数 |
| `%s` | 字符串 |
| `%x` | 十六进制整数 |

如果你不太确定应该用什么, `%s`永远起作用, 它会把任何数据类型转换为字符串：

```python
>>> 'Age: %s. Gender: %s' % (25, True)
'Age: 25. Gender: True'
```

有些时候, 字符串里面的`%`是一个普通字符怎么办？这个时候就需要转义, 用`%%`来表示一个`%`：

```python
>>> 'growth rate: %d %%' % 7
'growth rate: 7 %'
```

#### format()

另一种格式化字符串的方法是使用字符串的`format()`方法, 它会用传入的参数依次替换字符串内的占位符`{0}`, `{1}`……, 不过这种方式写起来比`%`要麻烦得多：

```python
>>> 'Hello, {0}, 成绩提升了 {1:.1f}%'.format('小明', 17.125)
'Hello, 小明, 成绩提升了 17.1%'
```

### 字符串和编码小结

`Python3`的字符串使用`Unicode`, 直接支持多语言. 

当`str`和`bytes`互相转换时, 需要指定编码. 最常用的编码是`UTF-8`. `Python`当然也支持其他编码方式, 比如把`Unicode`编码成`GB2312`：

```python
>>> '中文'.encode('gb2312')
b'\xd6\xd0\xce\xc4'
```

但这种方式纯属自找麻烦, 如果没有特殊业务要求, 请牢记仅使用`UTF-8`编码. 
格式化字符串的时候, 可以用Python的交互式环境测试, 方便快捷. 

### 使用list和tuple

#### list 列表

Python内置的一种数据类型是列表：`list`. `list`是一种有序的集合, 可以随时添加和删除其中的元素. 
比如, 列出班里所有同学的名字, 就可以用一个`list`表示：

```python
>>> classmates = ['Michael', 'Bob', 'Tracy']
>>> classmates
['Michael', 'Bob', 'Tracy']
```

变量`classmates`就是一个list. 用`len()`函数可以获得list元素的个数：

```python
>>> len(classmates)
3
```

用索引来访问list中每一个位置的元素, 记得索引是从`0`开始的：

```python
>>> classmates[0]
'Michael'
>>> classmates[3]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
IndexError: list index out of range
```

当索引超出了范围时, Python会报一个`IndexError`错误, 所以, 要确保索引不要越界, 记得最后一个元素的索引是`len(classmates) - 1`. 

如果要取最后一个元素, 除了计算索引位置外, 还可以用`-1`做索引, 直接获取最后一个元素：

```python
>>> classmates[-1]
'Tracy'
```

以此类推, 可以获取倒数第2个, 倒数第3个：

```python
>>> classmates[-2]
'Bob'
>>> classmates[-3]
'Michael'
```

当然, 倒数第4个就越界了. 

`list`是一个可变的有序表, 所以, 可以往`list`中追加元素到末尾：

```python
>>> classmates.append('Adam')
>>> classmates
['Michael', 'Bob', 'Tracy', 'Adam']
```

也可以把元素插入到指定的位置, 比如索引号为`1`的位置：

```python
>>> classmates.insert(1, 'Jack')
>>> classmates
['Michael', 'Jack', 'Bob', 'Tracy', 'Adam']
```

要删除list末尾的元素, 用`pop()`方法：

```python
>>> classmates.pop()
'Adam'
>>> classmates
['Michael', 'Jack', 'Bob', 'Tracy']
```

要删除指定位置的元素, 用`pop(i)`方法, 其中`i`是索引位置：

```python
>>> classmates.pop(1)
'Jack'
>>> classmates
['Michael', 'Bob', 'Tracy']
```

要把某个元素替换成别的元素, 可以直接赋值给对应的索引位置：

```python
>>> classmates[1] = 'Sarah'
>>> classmates
['Michael', 'Sarah', 'Tracy']
```

`list`里面的元素的数据类型也可以不同, 比如：

```python
>>> L = ['Apple', 123, True]
```

`list`元素也可以是另一个`list`, 比如：

```python
>>> s = ['python', 'java', ['asp', 'php'], 'scheme']
>>> len(s)
4
```

要注意`s`只有`4`个元素, 其中`s[2]`又是一个`list`, 如果拆开写就更容易理解了：

```python
>>> p = ['asp', 'php']
>>> s = ['python', 'java', p, 'scheme']
```

要拿到`'php'`可以写`p[1]`或者`s[2][1]`, 因此`s`可以看成是一个二维数组, 类似的还有三维, 四维……数组, 不过很少用到. 

如果一个`list`中一个元素也没有, 就是一个`空的``list`, 它的长度为`0`：

```python
>>> L = []
>>> len(L)
0
```

#### tuple 元组

另一种有序列表叫元组：`tuple`. tuple和list非常类似, 但是tuple一旦初始化就不能修改, 比如同样是列出同学的名字：

```python
>>> classmates = ('Michael', 'Bob', 'Tracy')
```

现在, `classmates`这个`tuple`不能变了, 它也没有`append()`, `insert()`这样的方法. 其他获取元素的方法和list是一样的, 你可以正常地使用`classmates[0]`, `classmates[-1]`, 但不能赋值成另外的元素. 

不可变的tuple有什么意义？因为tuple不可变, 所以代码更安全. 如果可能, 能用tuple代替list就尽量用tuple. 

tuple的陷阱：当你定义一个tuple时, 在定义的时候, tuple的元素就必须被确定下来, 比如：

```python
>>> t = (1, 2)
>>> t
(1, 2)
```

如果要定义一个空的tuple, 可以写成`()`：

```python
>>> t = ()
>>> t
()
```

但是, 要定义一个只有1个元素的tuple, 如果你这么定义：

```python
>>> t = (1)
>>> t
1
```

定义的不是tuple, 是`1`这个数！这是因为括号`()`既可以表示tuple, 又可以表示数学公式中的`小括号`, 这就产生了歧义, 因此, Python规定, 这种情况下, 按小括号进行计算, 计算结果自然是`1`. 

所以, 只有1个元素的tuple定义时必须加一个逗号`,`, 来消除歧义：

```python
>>> t = (1,)
>>> t
(1,)
```

Python在显示只有1个元素的tuple时, 也会加一个逗号`,`, 以免你误解成数学计算意义上的括号. 

最后来看一个“可变的”tuple：

```python
>>> t = ('a', 'b', ['A', 'B'])
>>> t[2][0] = 'X'
>>> t[2][1] = 'Y'
>>> t
('a', 'b', ['X', 'Y'])
```

这个tuple定义的时候有3个元素, 分别是`'a'`, `'b'`和一个`list`. 不是说tuple一旦定义后就不可变了吗？怎么后来又变了？

tuple所谓的“不变”是说, tuple的每个元素, 指向永远不变. 即指向`'a'`, 就不能改成指向`'b'`, 指向一个list, 就不能改成指向其他对象, 但指向的这个list本身是可变的！

理解了“指向不变”后, 要创建一个内容也不变的tuple怎么做？那就必须保证tuple的每一个元素本身也不能变. 

### 条件判断

计算机之所以能做很多自动化的任务, 因为它可以自己做条件判断. 

比如, 输入用户年龄, 根据年龄打印不同的内容, 在Python程序中, 用if语句实现：

```python
age = 20
if age >= 18:
    print('your age is', age)
    print('adult')
```

根据Python的缩进规则, 如果`if`语句判断是`True`, 
就把缩进的两行`print`语句执行了, **否则, 什么也不做**. 

Python程序语言指定任何非`0`和非空(`null`)值为`True`, `0` 或者 `null`为`false`. 

```python
>>>bool('sadfasdf')
True
```

由于 `python` 并不支持 `switch` 语句, 所以多个条件判断, 只能用 `elif` 来实现, 如果判断需要多个条件需同时判断时, 可以使用 `or` (或), 表示两个条件有一个成立时判断条件成功；使用 `and` (与)时, 表示只有两个条件同时成立的情况下, 判断条件才成功. 

也可以给`if`添加一个`else`语句, 意思是, 如果`if`判断是`False`, 不要执行`if`的内容, 去把`else`执行了：

```python
age = 3
if age >= 18:
    print('your age is', age)
    print('adult')
else:
    print('your age is', age)
    print('teenager')
```

注意不要少写了冒号`:`. 

当然上面的判断是很粗略的, 完全可以用`elif`做更细致的判断：

```python
age = 3
if age >= 18:
    print('adult')
elif age >= 6:
    print('teenager')
else:
    print('kid')
```

`elif`是`else if`的缩写, 完全可以有多个`elif`, 所以`if`语句的完整形式就是：

```python
if <条件判断1>:
    <执行1>
elif <条件判断2>:
    <执行2>
elif <条件判断3>:
    <执行3>
else:
    <执行4>
```

`if`语句执行有个特点, 它是从上往下判断, 如果在某个判断上是`True`, 把该判断对应的语句执行后, 就忽略掉剩下的`elif`和`else`, 所以, 请测试并解释为什么下面的程序打印的是`teenager`：

```python
age = 20
if age >= 6:
    print('teenager')
elif age >= 18:
    print('adult')
else:
    print('kid')
```

`if`判断条件还可以简写, 比如写：

```python
if x:
    print('True')
```

只要x是非零数值, 非空字符串, 非空list等, 就判断为`True`, 否则为`False`. 

#### 再议-input

最后看一个有问题的条件判断. 很多同学会用`input()`读取用户的输入, 这样可以自己输入, 程序运行得更有意思：

```python
birth = input('birth: ')
if birth < 2000:
    print('00前')
else:
    print('00后')
```

输入`1982`, 结果报错：

```python
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unorderable types: str() > int()
```

这是因为`input()`返回的数据类型是`str`, `str`不能直接和整数比较, 必须先把`str`转换成整数. Python提供了`int()`函数来完成这件事情：

```python
s = input('birth: ')
birth = int(s)
if birth < 2000:
    print('00前')
else:
    print('00后')
```

再次运行, 就可以得到正确地结果. 但是, 如果输入`abc`呢？又会得到一个错误信息：

```python
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ValueError: invalid literal for int() with base 10: 'abc'
```

原来`int()`函数发现一个字符串并不是合法的数字时就会报错, 程序就退出了. 

如何检查并捕获程序运行期的错误呢？后面的错误和调试会讲到.

### 循环

为了让计算机能计算成千上万次的重复运算, 我们就需要循环语句. 

Python的循环有两种, 一种是`for...in`循环, 依次把`list`或`tuple`中的每个元素迭代出来, 看例子：

```python
names = ['Michael', 'Bob', 'Tracy']
for name in names:
    print(name)
```

执行这段代码, 会依次打印`names`的每一个元素：

```python
Michael
Bob
Tracy
```

所以`for x in ...`循环就是把每个元素代入变量`x`, 然后执行缩进块的语句. 

再比如我们想计算`1-10`的整数之和, 可以用一个`sum`变量做累加：

```python
sum = 0
for x in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
    sum = sum + x
print(sum)
```

Python提供一个`range()`函数, 可以生成一个整数序列, 再通过`list()`函数可以转换为`list`. 比如`range(5)`生成的序列是从`0`开始小于`5`的整数：

```python
>>> list(range(5))
[0, 1, 2, 3, 4]
```

`range(101)`就可以生成`0-100`的整数序列, 计算如下：

```python
sum = 0
for x in range(101):
    sum = sum + x
print(sum)
```

第二种循环是`while`循环, 只要条件满足, 就不断循环, 条件不满足时退出循环. 
比如我们要计算`100`以内所有奇数之和, 可以用`while`循环实现：

```python
sum = 0
n = 99
while n > 0:
    sum = sum + n
    n = n - 2
print(sum)
```

在循环内部变量`n`不断自减, 直到变为`-1`时, 不再满足`while`条件, 循环退出. 

#### break

在循环中, `break`语句可以提前退出循环. 例如, 本来要循环打印`1～100`的数字：

```python
n = 1
while n <= 100:
    print(n)
    n = n + 1
print('END')
```

上面的代码可以打印出1~100. 

如果要提前结束循环, 可以用`break`语句：

```python
n = 1
while n <= 100:
    if n > 10: # 当n = 11时, 条件满足, 执行break语句
        break # break语句会结束当前循环
    print(n)
    n = n + 1
print('END')
```

执行上面的代码可以看到, 打印出`1~10`后, 紧接着打印`END`, 程序结束. 

可见`break`的作用是提前结束循环. 

#### continue

在循环过程中, 也可以通过`continue`语句, 跳过当前的这次循环, 直接开始下一次循环. 

```python
n = 0
while n < 10:
    n = n + 1
    if n % 2 == 0: # 如果n是偶数, 执行continue语句
        continue # continue语句会直接继续下一轮循环, 后续的print()语句不会执行
    print(n)
```

执行上面的代码可以看到, 打印的不是`1～10`, 而是`1, 3, 5, 7, 9`. 

可见`continue`的作用是提前结束本轮循环, 并直接开始下一轮循环. 

小结

循环是让计算机做重复任务的有效的方法. 

`break`语句可以在循环过程中直接退出循环, 而`continue`语句可以提前结束本轮循环, 并直接开始下一轮循环. 这两个语句通常都必须配合`if`语句使用. 

要特别注意, 不要滥用`break`和`continue`语句. `break`和`continue`会造成代码执行逻辑分叉过多, 容易出错. 大多数循环并不需要用到`break`和`continue`语句, 上面的两个例子, 都可以通过改写循环条件或者修改循环逻辑, 去掉`break`和`continue`语句. 

有些时候, 如果代码写得有问题, 会让程序陷入“死循环”, 也就是永远循环下去. 这时可以用`Ctrl+C`退出程序, 或者强制结束Python进程. 

### 使用dict和set

#### dict

Python内置了字典：`dict`的支持, dict全称dictionary, 在其他语言中也称为`map`, 使用 `键-值`(key-value)存储, 具有极快的查找速度. 

举个例子, 假设要根据同学的名字查找对应的成绩, 如果用list实现, 需要两个list：

```python
names = ['Michael', 'Bob', 'Tracy']
scores = [95, 75, 85]
```

给定一个名字, 要查找对应的成绩, 就先要在names中找到对应的位置, 再从scores取出对应的成绩, list越长, 耗时越长. 

如果用dict实现, 只需要一个“名字”-“成绩”的对照表, 直接根据名字查找成绩, 无论这个表有多大, 查找速度都不会变慢. 用Python写一个dict如下：

```python
>>> d = {'Michael': 95, 'Bob': 75, 'Tracy': 85}
>>> d['Michael']
95
```

为什么dict查找速度这么快？因为dict的实现原理和查字典是一样的. 
假设字典包含了1万个汉字, 我们要查某一个字, 一个办法是把字典从第一页往后翻, 直到找到我们想要的字为止, 这种方法就是在list中查找元素的方法, list越大, 查找越慢. 

第二种方法是先在字典的索引表里(比如部首表)查这个字对应的页码, 然后直接翻到该页, 找到这个字. 
无论找哪个字, 这种查找速度都非常快, 不会随着字典大小的增加而变慢. 

dict就是第二种实现方式, 给定一个名字, 比如`'Michael'`, dict在内部就可以直接计算出`Michael`对应的存放成绩的“页码”, 也就是`95`这个数字存放的内存地址, 直接取出来, 所以速度非常快. 

你可以猜到, 这种`key-value`存储方式, 在放进去的时候, 必须根据`key`算出`value`的存放位置, 这样, 取的时候才能根据`key`直接拿到`value`. 

把数据放入dict的方法, 除了初始化时指定外, 还可以通过key放入：

```python
>>> d['Adam'] = 67
>>> d['Adam']
67
```

由于一个key只能对应一个value, 所以, 多次对一个key放入value, 后面的值会把前面的值冲掉：

```python
>>> d['Jack'] = 90
>>> d['Jack']
90
>>> d['Jack'] = 88
>>> d['Jack']
88
```

如果key不存在, dict就会报错：

```python
>>> d['Thomas']
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'Thomas'
```

要避免key不存在的错误, 有两种办法, 一是通过`in`判断key是否存在：

```python
>>> 'Thomas' in d
False
```

二是通过dict提供的`get()`方法, 如果`key`不存在, 可以返回`None`, 或者自己指定的`value`：

```python
>>> d.get('Thomas')
>>> d.get('Thomas', -1)
-1
```

注意：返回`None`的时候Python的交互环境**不显示结果**. 

要删除一个`key`, 用`pop(key)`方法, 对应的`value`也会从`dict`中删除：

```python
>>> d.pop('Bob')
75
>>> d
{'Michael': 95, 'Tracy': 85}
```

请务必注意, `dict`内部存放的顺序和`key`放入的顺序是没有关系的. 

和list比较, dict有以下几个特点：

+ 查找和插入的速度极快, 不会随着key的增加而变慢；
+ 需要占用大量的内存, 内存浪费多. 

而list相反：

+ 查找和插入的时间随着元素的增加而增加；
+ 占用空间小, 浪费内存很少. 

所以, dict是用空间来换取时间的一种方法. 

dict可以用在需要高速查找的很多地方, 在Python代码中几乎无处不在, 正确使用dict非常重要, 需要牢记的第一条就是dict的key必须是不可变对象. 

这是因为dict根据key来计算value的存储位置, 如果每次计算相同的key得出的结果不同, 那dict内部就完全混乱了. 这个通过key计算位置的算法称为哈希算法(`Hash`). 

要保证hash的正确性, 作为key的对象就不能变. 在Python中, `字符串`, `整数`等都是不可变的, 因此, 可以放心地作为`key`. 而`list`是可变的, 就不能作为key：

```python
>>> key = [1, 2, 3]
>>> d[key] = 'a list'
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unhashable type: 'list'
```

#### set

`set`和`dict`类似, 也是一组key的集合, 但不存储value. 由于key不能重复, 所以, 在set中, **没有重复的key**. 

要创建一个set, 需要提供一个list作为输入集合：

```python
>>> s = set([1, 2, 3])
>>> s
{1, 2, 3}
```

注意, 传入的参数`[1, 2, 3]`是一个list, 而显示的`{1, 2, 3}`只是告诉你这个set内部有1, 2, 3这3个元素, **显示的顺序也不表示set是有序的**. 

重复元素在set中自动被过滤：

```python
>>> s = set([1, 1, 2, 2, 3, 3])
>>> s
{1, 2, 3}
```

通过`add(key)`方法可以添加元素到set中, 可以重复添加, 但不会有效果：

```python
>>> s.add(4)
>>> s
{1, 2, 3, 4}
>>> s.add(4)
>>> s
{1, 2, 3, 4}
```

通过`remove(key)`方法可以删除元素：

```python
>>> s.remove(4)
>>> s
{1, 2, 3}
```

set可以看成数学意义上的无序和无重复元素的集合, 因此, 两个set可以做数学意义上的交集, 并集等操作：

```python
>>> s1 = set([1, 2, 3])
>>> s2 = set([2, 3, 4])
>>> s1 & s2
{2, 3}
>>> s1 | s2
{1, 2, 3, 4}
```

set和dict的唯一区别仅在于没有存储对应的value, 但是, set的原理和dict一样, 
所以, 同样不可以放入可变对象, 因为无法判断两个可变对象是否相等, 也就无法保证set内部“不会有重复元素”. 
试试把list放入set, 看看是否会报错. 

#### 再议不可变对象

上面我们讲了, `str`是不变对象, 而`list`是可变对象. 

对于可变对象, 比如list, 对list进行操作, list内部的内容是会变化的, 比如：

```python
>>> a = ['c', 'b', 'a']
>>> a.sort()
>>> a
['a', 'b', 'c']
```

而对于不可变对象, 比如`str`, 对`str`进行操作呢：

```python
>>> a = 'abc'
>>> a.replace('a', 'A')
'Abc'
>>> a
'abc'
```

虽然字符串有个`replace()`方法, 也确实变出了`'Abc'`, 但变量`a`最后仍是`'abc'`, 应该怎么理解呢？

我们先把代码改成下面这样：

```python
>>> a = 'abc'
>>> b = a.replace('a', 'A')
>>> b
'Abc'
>>> a
'abc'
```

要始终牢记的是, `a`是变量, 而`'abc'`才是字符串对象！有些时候, 我们经常说, 对象a的内容是`'abc'`, 但其实是指, `a`本身是一个变量, 它指向的对象的内容才是`'abc'`：

当我们调用`a.replace('a', 'A')`时, 实际上调用方法`replace`是作用在字符串对象`'abc'`上的, 而这个方法虽然名字叫`replace`, 但却没有改变字符串`'abc'`的内容. 相反, `replace`方法创建了一个新字符串`'Abc'`并返回, 如果我们用变量`b`指向该新字符串, 就容易理解了, 变量`a`仍指向原有的字符串`'abc'`, 但变量`b`却指向新字符串`'Abc'`了：

所以, 对于不变对象来说, 调用对象自身的任意方法, 也不会改变该对象自身的内容. 相反, 这些方法会创建新的对象并返回, 这样, 就保证了不可变对象本身永远是不可变的. 

小结

使用`key-value`存储结构的dict在Python中非常有用, 选择不可变对象作为`key`很重要, 最常用的`key`是字符串. 

`tuple`虽然是不变对象, 但试试把`(1, 2, 3)`和`(1, [2, 3])`放入dict或set中, 并解释结果. 

## 函数

我们知道圆的面积计算公式为：

$S=\pi r^2$

有了函数, 我们就不再每次写`s = 3.14 * x * x`, 
而是写成更有意义的函数调用`s = area_of_circle(x)`, 

Python不但能非常灵活地定义函数, 而且本身内置了很多有用的函数, 可以直接调用. 

### 抽象

抽象是数学中非常常见的概念. 举个例子：

计算数列的和, 比如：`1 + 2 + 3 + ... + 100`, 写起来十分不方便, 于是数学家发明了求和符号$\sum$, 可以把`1 + 2 + 3 + ... + 100`记作：

$\sum\limits_1^100 n$

这种抽象记法非常强大, 因为我们看$\sum$ 就可以理解成求和, 而不是还原成低级的加法运算. 
而且, 这种抽象记法是可扩展的, 比如：

$\sum\limits_1^100 n^2+1$

还原成加法运算就变成了：
`(1 x 1 + 1) + (2 x 2 + 1)  + ... + (100 x 100 + 1)`
可见, 借助抽象, 我们才能不关心底层的具体计算过程, 而直接在更高的层次上思考问题. 

写计算机程序也是一样, 函数就是最基本的一种代码抽象的方式. 

### 调用函数

Python内置了很多有用的函数, 我们可以直接调用. 

要调用一个函数, 需要知道函数的名称和参数, 比如求绝对值的函数`abs`, 只有一个参数. 可以直接从[Python的官方网站查看文档][] ：

[Python的官方网站查看文档]: http://docs.python.org/3/library/functions.html#abs

也可以在交互式命令行通过`help(abs)`查看`abs`函数的帮助信息. 

调用`abs`函数：

```python
>>> abs(100)
100
>>> abs(-20)
20
>>> abs(12.34)
12.34
```

调用函数的时候, 如果传入的参数数量不对, 会报`TypeError`的错误, 并且`Python`会明确地告诉你：`abs()`有且仅有`1`个参数, 但给出了两个：

```python
>>> abs(1, 2)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: abs() takes exactly one argument (2 given)
```

如果传入的参数数量是对的, 但参数类型不能被函数所接受, 也会报`TypeError`的错误, 并且给出错误信息：`str`是错误的参数类型：

```python
>>> abs('a')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: bad operand type for abs(): 'str'
```

而`max`函数`max()`可以接收任意多个参数, 并返回最大的那个：

```python
>>> max(1, 2)
2
>>> max(2, 3, 1, -5)
3
```

#### 数据类型转换

Python内置的常用函数还包括数据类型转换函数, 比如`int()`函数可以把其他数据类型转换为整数：

```python
>>> int('123')
123
>>> int(12.34)
12
>>> float('12.34')
12.34
>>> str(1.23)
'1.23'
>>> str(100)
'100'
>>> bool(1)
True
>>> bool('')
False
```

函数名其实就是指向一个函数对象的引用, 完全可以把函数名赋给一个变量, 相当于给这个函数起了一个“别名”：

```python
>>> a = abs # 变量a指向abs函数
>>> a(-1) # 所以也可以通过a调用abs函数
1
```

### 定义函数

在Python中, 定义一个函数要使用`def`语句, 
依次写出函数名, 括号, 括号中的参数和冒号`:`, 然后, 在缩进块中编写函数体, 函数的返回值用`return`语句返回. 

我们以自定义一个求绝对值的my_abs函数为例

```python
# -*- coding: utf-8 -*-
def my_abs(x):
    if x >= 0:
        return x
    else:
        return -x

print(my_abs(-99))
```

请注意, 函数体内部的语句在执行时, 一旦执行到`return`时, 函数就执行完毕, 并将结果返回. 因此, 函数内部通过条件判断和循环可以实现非常复杂的逻辑. 

如果没有`return`语句, 函数执行完毕后也会返回结果, 只是结果为`None`. 
`return None`可以简写为`return`. 

在Python交互环境中定义函数时, 注意`Python`会出现`...`的提示. 
函数定义结束后需要**按两次回车**重新回到`>>>`提示符下：

如果你已经把`my_abs()`的函数定义保存为`abstest.py`文件了, 
那么, 可以在该文件的当前目录下启动Python解释器, 用`from abstest import my_abs`来导入`my_abs()`函数, 注意`abstest`是文件名(不含`.py`扩展名)：

`import`的用法在后续模块一节中会详细介绍. 

#### 空函数

如果想定义一个什么事也不做的空函数, 可以用`pass`语句：

```python
def nop():
    pass
```

`pass`语句什么都不做, 那有什么用？
实际上`pass`可以用来作为占位符, 比如现在还没想好怎么写函数的代码, 就可以先放一个`pass`, 让代码能运行起来. 

`pass`还可以用在其他语句里, 比如：

```python
if age >= 18:
    pass
```

缺少了`pass`, 代码运行就会有语法错误. 

#### 参数检查

调用函数时, 如果参数个数不对, Python解释器会自动检查出来, 并抛出`TypeError`：

```python
>>> my_abs(1, 2)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: my_abs() takes 1 positional argument but 2 were given
```

但是如果参数类型不对, Python解释器就无法帮我们检查. 
试试`my_abs`和内置函数`abs`的差别：

```python
>>> my_abs('A')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 2, in my_abs
TypeError: unorderable types: str() >= int()
>>> abs('A')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: bad operand type for abs(): 'str'
```

当传入了不恰当的参数时, 内置函数`abs`会检查出参数错误, 而我们定义的`my_abs`没有参数检查, 会导致`if`语句出错, 出错信息和`abs`不一样. 所以, 这个函数定义不够完善. 

让我们修改一下`my_abs`的定义, 对参数类型做检查, 只允许整数和浮点数类型的参数. 
数据类型检查可以用内置函数`isinstance()`实现：

```python
def my_abs(x):
    if not isinstance(x, (int, float)):
        raise TypeError('bad operand type')
    if x >= 0:
        return x
    else:
        return -x
```

添加了参数检查后, 如果传入错误的参数类型, 函数就可以抛出一个错误：

```python
>>> my_abs('A')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 3, in my_abs
TypeError: bad operand type
```

错误和异常处理将在后续讲到. 

#### 返回多个值

函数可以返回多个值吗？答案是肯定的. 

比如在游戏中经常需要从一个点移动到另一个点, 给出坐标, 位移和角度, 就可以计算出新的坐标：

```python
import math

def move(x, y, step, angle=0):
    nx = x + step * math.cos(angle)
    ny = y - step * math.sin(angle)
    return nx, ny
```

`import math`语句表示导入`math`包, 并允许后续代码引用`math`包里的`sin`, `cos`等函数. 

然后, 我们就可以同时获得返回值：

```python
>>> x, y = move(100, 100, 60, math.pi / 6)
>>> print(x, y)
151.96152422706632 70.0
```

但其实这只是一种假象, Python函数返回的仍然是单一值：

```python
>>> r = move(100, 100, 60, math.pi / 6)
>>> print(r)
(151.96152422706632, 70.0)
```

原来返回值是一个`tuple`！
但是, 在语法上, 返回一个`tuple`可以省略括号, 
而多个变量可以同时接收一个`tuple`, 按位置赋给对应的值, 
所以, Python的函数返回多值其实就是返回一个`tuple`, 但写起来更方便. 

#### 函数小结

+ 定义函数时, 需要确定函数名和参数个数；
+ 如果有必要, 可以先对参数的数据类型做检查；
+ 函数体内部可以用`return`随时返回函数结果；
+ 函数执行完毕也没有`return`语句时, 自动`return None`. 
+ 函数可以同时返回多个值, 但其实就是一个`tuple`. 

#### 练习-定义函数

请定义一个函数`quadratic(a, b, c)`, 接收`3`个参数, 
返回一元二次方程$a x^2+b x+c=0$的两个解. 

提示：

一元二次方程的求根公式为：

$x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}$

计算平方根可以调用`math.sqrt()`函数：

```python
>>> import math
>>> math.sqrt(2)
1.4142135623730951
```

```python
# -*- coding: utf-8 -*-

import math

def quadratic(a, b, c):
    delta = b*b-4*a*c
    if delta >= 0:
        r1=(-b+math.sqrt(delta))/(2*a)
        r2=(-b-math.sqrt(delta))/(2*a)
        return (r1,r2)
     else:
        return no real root

# 测试:
print('quadratic(2, 3, 1) =', quadratic(2, 3, 1))
print('quadratic(1, 3, -4) =', quadratic(1, 3, -4))

if quadratic(2, 3, 1) != (-0.5, -1.0):
    print('测试失败')
elif quadratic(1, 3, -4) != (1.0, -4.0):
    print('测试失败')
else:
    print('测试成功')
```

### 函数的参数

定义函数的时候, 我们把参数的名字和位置确定下来, 函数的接口定义就完成了. 
对于函数的调用者来说, 只需要知道如何传递正确的参数, 以及函数将返回什么样的值就够了, 函数内部的复杂逻辑被封装起来, 调用者无需了解. 

Python的函数定义非常简单, 但灵活度却非常大. 
除了正常定义的必选参数外, 还可以使用默认参数, 可变参数和关键字参数, 使得函数定义出来的接口, 不但能处理复杂的参数, 还可以简化调用者的代码. 

#### 位置参数

我们先写一个计算$x^2$的函数：

```python
def power(x):
    return x * x
```

对于`power(x)`函数, 参数`x`就是一个位置参数. 
当我们调用`power`函数时, 必须传入有且仅有的一个参数`x`：

```python
>>> power(5)
25
>>> power(15)
225
```

我们不可能定义无限多个函数. 
你也许想到了, 可以把`power(x)`修改为`power(x, n)`, 用来计算$x^n$, 说干就干：

```python
def power(x, n):
    s = 1
    while n > 0:
        n = n - 1
        s = s * x
    return s
```

对于这个修改后的`power(x, n)`函数, 可以计算任意`n`次方：

```python
>>> power(5, 2)
25
>>> power(5, 3)
125
```

修改后的`power(x, n)`函数有两个参数：`x`和`n`, 这两个参数都是位置参数, 
调用函数时, 传入的两个值按照位置顺序依次赋给参数`x`和`n`. 

#### 默认参数

新的`power(x, n)`函数定义没有问题, 但是, 旧的调用代码失败了, 原因是我们增加了一个参数, 导致旧的代码因为缺少一个参数而无法正常调用：

```python
>>> power(5)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: power() missing 1 required positional argument: 'n'
```

Python的错误信息很明确：调用函数`power()`缺少了一个位置参数`n`. 

这个时候, 默认参数就排上用场了. 由于我们经常计算$x^2$, 所以, 完全可以把第二个参数`n`的默认值设定为`2`：

```python
def power(x, n=2):
    s = 1
    while n > 0:
        n = n - 1
        s = s * x
    return s
```

这样, 当我们调用`power(5)`时, 相当于调用`power(5, 2)`：

```python
>>> power(5)
25
>>> power(5, 2)
25
```

而对于`n > 2`的其他情况, 就必须明确地传入`n`, 比如`power(5, 3)`. 

从上面的例子可以看出, 默认参数可以简化函数的调用. 设置默认参数时, 有几点要注意：

+ 必选参数在前, 默认参数在后, 否则Python的解释器会报错
+ 如何设置默认参数. 

当函数有多个参数时, 把变化大的参数放前面, 变化小的参数放后面. 
变化小的参数就可以作为默认参数. 

使用默认参数有什么好处？最大的好处是能降低调用函数的难度. 

举个例子, 我们写个一年级小学生注册的函数, 需要传入`name`和`gender`两个参数：

```python
def enroll(name, gender):
    print('name:', name)
    print('gender:', gender)
```

这样, 调用`enroll()`函数只需要传入两个参数：

```python
>>> enroll('Sarah', 'F')
name: Sarah
gender: F
```

如果要继续传入年龄, 城市等信息怎么办？这样会使得调用函数的复杂度大大增加. 

我们可以把年龄和城市设为默认参数：

```python
def enroll(name, gender, age=6, city='Beijing'):
    print('name:', name)
    print('gender:', gender)
    print('age:', age)
    print('city:', city)
```

这样, 大多数学生注册时不需要提供年龄和城市, 只提供必须的两个参数：

```python
>>> enroll('Sarah', 'F')
name: Sarah
gender: F
age: 6
city: Beijing
```

只有与默认参数不符的学生才需要提供额外的信息：

```python
enroll('Bob', 'M', 7)
enroll('Adam', 'M', city='Tianjin')
```

可见, 默认参数降低了函数调用的难度, 而一旦需要更复杂的调用时, 又可以传递更多的参数来实现. 无论是简单调用还是复杂调用, 函数只需要定义一个. 

有多个默认参数时, 调用的时候, 既可以按顺序提供默认参数, 比如调用`enroll('Bob', 'M', 7)`, 意思是, 除了`name`, `gender`这两个参数外, 最后1个参数应用在参数`age`上, `city`参数由于没有提供, 仍然使用默认值. 

也可以不按顺序提供部分默认参数. 当不按顺序提供部分默认参数时, 需要把参数名写上. 
比如调用`enroll('Adam', 'M', city='Tianjin')`, 意思是, `city`参数用传进去的值, 其他默认参数继续使用默认值. 

默认参数很有用, 但使用不当, 也会掉坑里. 默认参数有个最大的坑, 演示如下：
先定义一个函数, 传入一个`list`, 添加一个`END`再返回：

```python
def add_end(L=[]):
    L.append('END')
    return L
```

当你正常调用时, 结果似乎不错：

```python
>>> add_end([1, 2, 3])
[1, 2, 3, 'END']
>>> add_end(['x', 'y', 'z'])
['x', 'y', 'z', 'END']
```

当你使用默认参数调用时, 一开始结果也是对的：

```python
>>> add_end()
['END']
```

但是, 再次调用add_end()时, 结果就不对了：

```python
>>> add_end()
['END', 'END']
>>> add_end()
['END', 'END', 'END']
```

很多初学者很疑惑, 默认参数是`[]`, 但是函数似乎每次都“记住了”上次添加了`'END'`后的list. 

原因解释如下：

Python函数在定义的时候, 默认参数`L`的值就被计算出来了, 即`[]`, 因为默认参数`L`也是一个变量, 它指向对象`[]`, 每次调用该函数, 如果改变了`L`的内容, 则下次调用时, 默认参数的内容就变了, 不再是函数定义时的`[]`了. 

>定义默认参数要牢记一点：默认参数必须指向不变对象！

要修改上面的例子, 我们可以用`None`这个不变对象来实现：

```python
def add_end(L=None):
    if L is None:
        L = []
    L.append('END')
    return L
```

现在, 无论调用多少次, 都不会有问题：

```python
>>> add_end()
['END']
>>> add_end()
['END']
```

为什么要设计`str`, `None`这样的**不变对象**呢？
因为不变对象一旦创建, 对象内部的数据就不能修改, 这样就减少了由于修改数据导致的错误. 
此外, 由于对象不变, 多任务环境下同时读取对象不需要加锁, 同时读一点问题都没有. 我们在编写程序时, 如果可以设计一个不变对象, 那就尽量设计成不变对象. 

#### 可变参数

在Python函数中, 还可以定义可变参数. 顾名思义, 可变参数就是传入的参数个数是可变的, 可以是1个, 2个到任意个, 还可以是0个. 

我们以数学题为例子, 给定一组数字`a`, `b`, `c`……, 请计算`a^2 + b^2 + c^2 + ……`. 

要定义出这个函数, 我们必须确定输入的参数. 由于参数个数不确定, 我们首先想到可以把`a, b, c……`作为一个list或tuple传进来, 这样, 函数可以定义如下：

```python
def calc(numbers):
    sum = 0
    for n in numbers:
        sum = sum + n * n
    return sum
```

但是调用的时候, 需要先组装出一个list或tuple：

```python
>>> calc([1, 2, 3])
14
>>> calc((1, 3, 5, 7))
84
```

如果利用可变参数, 调用函数的方式可以简化成这样：

```python
>>> calc(1, 2, 3)
14
>>> calc(1, 3, 5, 7)
84
```

所以, 我们把函数的参数改为可变参数：

```python
def calc(*numbers):
    sum = 0
    for n in numbers:
        sum = sum + n * n
    return sum
```

定义可变参数和定义一个list或tuple参数相比, 仅仅在参数前面加了一个`*`号. 
在函数内部, 参数numbers接收到的是一个tuple, 因此, 函数代码完全不变. 
但是, 调用该函数时, 可以传入任意个参数, 包括`0`个参数：

```python
>>> calc(1, 2)
5
>>> calc()
0
```

如果已经有一个list或者tuple, 要调用一个可变参数怎么办？可以这样做：

```python
>>> nums = [1, 2, 3]
>>> calc(nums[0], nums[1], nums[2])
14
```

这种写法当然是可行的, 问题是太繁琐, 所以Python允许你在list或tuple前面加一个`*`号, 把list或tuple的元素变成可变参数传进去：

```python
>>> nums = [1, 2, 3]
>>> calc(*nums)
14
```

`*nums`表示把`nums`这个`list`的所有元素作为可变参数传进去. 
这种写法相当有用, 而且很常见. 

#### 关键字参数

可变参数允许你传入`0`个或任意个参数, 这些可变参数在函数调用时自动组装为一个`tuple`. 
而关键字参数允许你传入`0`个或任意个含参数名的参数, 这些关键字参数在函数内部自动组装为一个`dict`. 请看示例：

```python
def person(name, age, **kw):
    print('name:', name, 'age:', age, 'other:', kw)
```

函数`person`除了必选参数`name`和`age`外, 还接受关键字参数`kw`. 在调用该函数时, 可以只传入必选参数：

```python
>>> person('Michael', 30)
name: Michael age: 30 other: {}
```

也可以传入任意个数的关键字参数：

```python
>>> person('Bob', 35, city='Beijing')
name: Bob age: 35 other: {'city': 'Beijing'}
>>> person('Adam', 45, gender='M', job='Engineer')
name: Adam age: 45 other: {'gender': 'M', 'job': 'Engineer'}
```

关键字参数有什么用？它可以扩展函数的功能. 比如, 在`person`函数里, 我们保证能接收到`name`和`age`这两个参数, 但是, 如果调用者愿意提供更多的参数, 我们也能收到. 
试想你正在做一个用户注册的功能, 除了用户名和年龄是必填项外, 其他都是可选项, 利用关键字参数来定义这个函数就能满足注册的需求. 

和可变参数类似, 也可以先组装出一个`dict`, 然后, 把该`dict`转换为关键字参数传进去：

```python
>>> extra = {'city': 'Beijing', 'job': 'Engineer'}
>>> person('Jack', 24, city=extra['city'], job=extra['job'])
name: Jack age: 24 other: {'city': 'Beijing', 'job': 'Engineer'}
```

当然, 上面复杂的调用可以用简化的写法：

```python
>>> extra = {'city': 'Beijing', 'job': 'Engineer'}
>>> person('Jack', 24, **extra)
name: Jack age: 24 other: {'city': 'Beijing', 'job': 'Engineer'}
```

`**extra`表示把`extra`这个`dict`的所有`key-value`用关键字参数传入到函数的`**kw`参数, 
`kw`将获得一个`dict`, 注意`kw`获得的`dict`是`extra`的一份拷贝, 对`kw`的改动不会影响到函数外的`extra`. 

#### 命名关键字参数

对于关键字参数, 函数的调用者可以传入任意不受限制的关键字参数. 至于到底传入了哪些, 就需要在函数内部通过`kw`检查. 

仍以`person()`函数为例, 我们希望检查是否有`city`和`job`参数：

```python
def person(name, age, **kw):
    if 'city' in kw:
        # 有city参数
        pass
    if 'job' in kw:
        # 有job参数
        pass
    print('name:', name, 'age:', age, 'other:', kw)
```

但是调用者仍可以传入不受限制的关键字参数：

```python
>>> person('Jack', 24, city='Beijing', addr='Chaoyang', zipcode=123456)
```

如果要限制关键字参数的名字, 就可以用命名关键字参数, 例如, 只接收`city`和`job`作为关键字参数. 这种方式定义的函数如下：

```python
def person(name, age, *, city, job):
    print(name, age, city, job)
```

和关键字参数`**kw`不同, 命名关键字参数需要一个特殊分隔符`*`, `*`后面的参数被视为命名关键字参数. 

调用方式如下：

```python
>>> person('Jack', 24, city='Beijing', job='Engineer')
Jack 24 Beijing Engineer
```

如果函数定义中已经有了一个可变参数, 后面跟着的命名关键字参数就不再需要一个特殊分隔符*了：

```python
def person(name, age, *args, city, job):
    print(name, age, args, city, job)
```

命名关键字参数必须传入参数名, 这和位置参数不同. 如果没有传入参数名, 调用将报错：

```python
>>> person('Jack', 24, 'Beijing', 'Engineer')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: person() takes 2 positional arguments but 4 were given
```

由于调用时缺少参数名`city`和`job`, Python解释器把这4个参数均视为位置参数, 但`person()`函数仅接受2个位置参数. 

命名关键字参数可以有缺省值, 从而简化调用：

```python
def person(name, age, *, city='Beijing', job):
    print(name, age, city, job)
```

由于命名关键字参数`city`具有默认值, 调用时, 可不传入`city`参数：

```python
>>> person('Jack', 24, job='Engineer')
Jack 24 Beijing Engineer
```

使用命名关键字参数时, 要特别注意, 如果没有可变参数, 就必须加一个`*`作为特殊分隔符. 如果缺少`*`, Python解释器将无法识别位置参数和命名关键字参数：

```python
def person(name, age, city, job):
    # 缺少 *, city和job被视为位置参数
    pass
```

#### 参数组合

在Python中定义函数, 可以用

+ 必选参数(`位置参数`)
+ 默认参数(`a=b`,`默认参数`)
+ 可变参数(`*`, `不定参数`)
+ 关键字参数(`**`, `不定字典`)
+ 命名关键字参数, (`字典参数`)

这5种参数都可以组合使用. 

其中`默认参数`与`字典参数`的区别在于, `字典参数`位于`可变参数`(或`*`)后面

但是请注意, 参数定义的顺序必须是：

1. 位置参数
2. 默认参数
3. 不定参数
4. 不定字典参数
5. 字典参数

***
可以这么理解, 参数一共有两大类, 位置参数(无名参数)和字典参数(署名参数). 想象python解释器去解释函数的参数列表：

1. **位置参数**和**字典参数**初看起来, 形式上是一样的, 单纯从各自的形式上, 将无法区分, 
所以需要约定一个分割符, 左边的是**位置参数**和**默认参数**, 然后右边是**不定字典参数**和**字典参数**
这个分隔符就懒省事儿地选成`*`, 而且也可以用来表示**不定参数**. 
2. 确定长度(长度为`1`)的变量放在参数列表两端, 如**位置参数**和**字典参数**, 不确定长度的变量(**不定参数**和**不定字典参数**)放在中间. 
因为长度可变的参数如果放在两边, 由于它可以自由伸缩占据参数位置, 就会把别的变量位置掩盖. 所以要在两端用固定长度的变量封住. 

由此可以确定五种类型参数的次序

```python
(A, B=25, *C, **D,E,)
(位置参数A, 默认参数B, 不定参数*C, 不定字典**D, 字典参数E)
```

不定在中间, 定长在两边. 
左边是位置, 右边是字典. 
何处是分隔, `*`号来体现. 

***
比如定义一个函数, 包含上述若干种参数：

```python
def f1(a, b, c=0, *args, **kw):
    print('a =', a, 'b =', b, 'c =', c, 'args =', args, 'kw =', kw)

def f2(a, b, c=0, *, d, **kw):
    print('a =', a, 'b =', b, 'c =', c, 'd =', d, 'kw =', kw)
```

在函数调用的时候, Python解释器自动按照参数位置和参数名把对应的参数传进去. 

```python
>>> f1(1, 2)
a = 1 b = 2 c = 0 args = () kw = {}
>>> f1(1, 2, c=3)
a = 1 b = 2 c = 3 args = () kw = {}
>>> f1(1, 2, 3, 'a', 'b')
a = 1 b = 2 c = 3 args = ('a', 'b') kw = {}
>>> f1(1, 2, 3, 'a', 'b', x=99)
a = 1 b = 2 c = 3 args = ('a', 'b') kw = {'x': 99}
>>> f2(1, 2, d=99, ext=None)
a = 1 b = 2 c = 0 d = 99 kw = {'ext': None}
```

最神奇的是通过一个`tuple`和`dict`, 你也可以调用上述函数：

```python
>>> args = (1, 2, 3, 4)
>>> kw = {'d': 99, 'x': '#'}
>>> f1(*args, **kw)
a = 1 b = 2 c = 3 args = (4,) kw = {'d': 99, 'x': '#'}
>>> args = (1, 2, 3)
>>> kw = {'d': 88, 'x': '#'}
>>> f2(*args, **kw)
a = 1 b = 2 c = 3 d = 88 kw = {'x': '#'}
```

所以, 对于任意函数, 都可以通过类似`func(*args, **kw)`的形式调用它, 无论它的参数是如何定义的. 

#### 函数的参数小结

Python的函数具有非常灵活的参数形态, 既可以实现简单的调用, 又可以传入非常复杂的参数. 

默认参数一定要用不可变对象, 如果是可变对象, 程序运行时会有逻辑错误！
要注意定义`可变参数`和`关键字参数`的语法：

`*args`是可变参数, `args`接收的是一个`tuple`；
`**kw`是关键字参数, `kw`接收的是一个`dict`. 

以及调用函数时如何传入可变参数和关键字参数的语法：

可变参数既可以直接传入：`func(1, 2, 3)`, 又可以先组装list或`tuple`, 再通过`*args`传入：`func(*(1, 2, 3))`；

关键字参数既可以直接传入：`func(a=1, b=2)`, 又可以先组装`dict`, 再通过`**kw传入`：`func(**{'a': 1, 'b': 2})`. 

使用`*args`和`**kw`是Python的习惯写法, 当然也可以用其他参数名, 但最好使用习惯用法. 

命名的关键字参数是为了限制调用者可以传入的参数名, 同时可以提供默认值. 

定义命名的关键字参数在没有可变参数的情况下不要忘了写分隔符`*`, 
否则定义的将是位置参数. 

#### 练习-函数的参数

以下函数允许计算两个数的乘积, 请稍加改造, 变成可接收一个或多个数并计算乘积：

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
def product(x, y):
    return x * y
```

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
def product(x, *args):
    if  not isinstance(x, (int, float)):
        raise TypeError('bad operand type')
    prod=x
    for element in args:
        prod=prod*element
    return prod
```

```python
# 测试
print('product(5) =', product(5))
print('product(5, 6) =', product(5, 6))
print('product(5, 6, 7) =', product(5, 6, 7))
print('product(5, 6, 7, 9) =', product(5, 6, 7, 9))
if product(5) != 5:
    print('测试失败!')
elif product(5, 6) != 30:
    print('测试失败!')
elif product(5, 6, 7) != 210:
    print('测试失败!')
elif product(5, 6, 7, 9) != 1890:
    print('测试失败!')
else:
    try:
        product()
        print('测试失败!')
    except TypeError:
        print('测试成功!')
```

### 递归函数

在函数内部, 可以调用其他函数. 如果一个函数在内部调用自身本身, 这个函数就是递归函数. 

举个例子, 我们来计算阶乘`n! = 1 x 2 x 3 x ... x n`, 
用函数`fact(n)`表示, 可以看出：`fact(n) = fact(n-1) x n`
只有`n=1`时需要特殊处理. 
于是, `fact(n)`用递归的方式写出来就是：

```python
def fact(n):
    if n==1:
        return 1
    return n * fact(n - 1)
```

上面就是一个递归函数. 可以试试：

```python
>>> fact(1)
1
>>> fact(5)
120
>>> fact(100)
933262154439...
```

递归函数的优点是定义简单, 逻辑清晰. 
理论上, 所有的递归函数都可以写成循环的方式, 但循环的逻辑不如递归清晰. 

使用递归函数需要注意防止栈溢出. 
在计算机中, 函数调用是通过栈(`stack`)这种数据结构实现的, 每当进入一个函数调用, 栈就会加一层栈帧, 每当函数返回, 栈就会减一层栈帧. 由于栈的大小不是无限的, 所以, 递归调用的次数过多, 会导致栈溢出. 可以试试`fact(1000)`：

```python
>>> fact(1000)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 4, in fact
  ...
  File "<stdin>", line 4, in fact
RuntimeError: maximum recursion depth exceeded in comparison
```

解决递归调用栈溢出的方法是通过**尾递归**优化, 事实上尾递归和循环的效果是一样的, 
所以, 把循环看成是一种特殊的尾递归函数也是可以的. 

尾递归是指, 在函数返回的时候, 调用自身本身, 并且, `return`语句不能包含表达式. 
这样, 编译器或者解释器就可以把尾递归做优化, 使递归本身无论调用多少次, 都只占用一个栈帧, 不会出现栈溢出的情况. 

上面的`fact(n)`函数由于`return n * fact(n - 1)`引入了乘法表达式, 所以就不是尾递归了. 要改成尾递归方式, 需要多一点代码, 主要是要把每一步的乘积传入到递归函数中：

```python
def fact(n):
    return fact_iter(n, 1)

def fact_iter(num, product):
    if num == 1:
        return product
    return fact_iter(num - 1, num * product)
```

可以看到, `return fact_iter(num - 1, num * product)`仅返回递归函数本身, `num - 1和num * product`在函数调用前就会被计算, 不影响函数调用. 

尾递归调用时, 如果做了优化, 栈不会增长, 因此, 无论多少次调用也不会导致栈溢出. 
遗憾的是, 大多数编程语言没有针对尾递归做优化, Python解释器也没有做优化, 所以, 即使把上面的`fact(n)`函数改成尾递归方式, 也会导致栈溢出. 

#### 练习-递归函数

汉诺塔的移动可以用递归函数非常简单地实现. 

请编写`move(n, a, b, c)`函数, 它接收参数`n`, 表示`3`个柱子`A`, `B`, `C`中第`1`个柱子`A`的盘子数量, 
然后打印出把所有盘子从`A`借助`B`移动到`C`的方法, 例如：

```python
# -*- coding: utf-8 -*-
def move(n, a, b, c):
    if n == 1:
        return str(a)+'-->'+str(c)
    else:
        return (move(n-1,a,c,b),print(a, '-->', c),move(n-1,b,a,c))

f=move(4,'A','B','C')
f
len(f)

# 期待输出:
# A --> C
# A --> B
# C --> B
# A --> C
# B --> A
# B --> C
# A --> C
```

## 高级特性

掌握了Python的数据类型, 语句和函数, 基本上就可以编写出很多有用的程序了. 

比如构造一个`1, 3, 5, 7, ..., 99`的列表, 可以通过循环实现：

```python
L = []
n = 1
while n <= 99:
    L.append(n)
    n = n + 2
```

取list的前一半的元素, 也可以通过循环实现. 

但是在Python中, 代码不是越多越好, 而是越少越好. 
代码不是越复杂越好, 而是越简单越好. 

基于这一思想, 我们来介绍Python中非常有用的高级特性, 1行代码能实现的功能, 决不写5行代码. 
请始终牢记, 代码越少, 开发效率越高. 

### 切片

取一个list或tuple的部分元素是非常常见的操作. 比如, 一个list如下：

```python
>>> L = ['Michael', 'Sarah', 'Tracy', 'Bob', 'Jack']
```

取前3个元素, 应该怎么做？

笨办法：

>>> [L[0], L[1], L[2]]
['Michael', 'Sarah', 'Tracy']

对这种经常取指定索引范围的操作, 用循环十分繁琐, 因此, Python提供了切片(`Slice`)操作符, 能大大简化这种操作. 

对应上面的问题, 取前3个元素, 用一行代码就可以完成切片：

```python
>>> L[0:3]
['Michael', 'Sarah', 'Tracy']
```

`L[0:3]`表示, 从索引`0`开始取, 直到索引`3`为止, 但不包括索引`3`. 即索引`0`, `1`, `2`, 正好是3个元素. 

如果第一个索引是`0`, 还可以省略：

```python
>>> L[:3]
['Michael', 'Sarah', 'Tracy']
```

也可以从索引`1`开始, 取出`2`个元素出来：

```python
>>> L[1:3]
['Sarah', 'Tracy']
```

类似的, 既然Python支持`L[-1]`取倒数第一个元素, 那么它同样支持倒数切片, 试试：

```python
>>> L[-2:]
['Bob', 'Jack']
>>> L[-2:-1]
['Bob']
```

记住倒数第一个元素的索引是`-1`. 

切片操作十分有用. 我们先创建一个`0-99`的数列：

```python
>>> L = list(range(100))
>>> L
[0, 1, 2, 3, ..., 99]
```

可以通过切片轻松取出某一段数列. 比如前`10`个数：

```python
>>> L[:10]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

后`10`个数：

```python
>>> L[-10:]
[90, 91, 92, 93, 94, 95, 96, 97, 98, 99]
```

前`11-20`个数：

```python
>>> L[10:20]
[10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
```

前`10`个数, 每两个取一个：

```python
>>> L[:10:2]
[0, 2, 4, 6, 8]
```

所有数, 每5个取一个：

```python
>>> L[::5]
[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95]
```

甚至什么都不写, 只写`[:]`就可以原样复制一个`list`：

```python
>>> L[:]
[0, 1, 2, 3, ..., 99]
```

tuple也是一种list, 唯一区别是tuple不可变. 因此, tuple也可以用切片操作, 只是操作的结果仍是tuple：

```python
>>> (0, 1, 2, 3, 4, 5)[:3]
(0, 1, 2)
```

字符串`'xxx'`也可以看成是一种`list`, 每个元素就是一个字符. 
因此, 字符串也可以用切片操作, 只是操作结果仍是字符串：

```python
>>> 'ABCDEFG'[:3]
'ABC'
>>> 'ABCDEFG'[::2]
'ACEG'
```

在很多编程语言中, 针对字符串提供了很多各种截取函数(例如, `substring`), 其实目的就是对字符串切片. Python没有针对字符串的截取函数, 只需要切片一个操作就可以完成, 非常简单. 

#### 练习-切片

利用切片操作, 实现一个`trim()`函数, 去除字符串首尾的空格, 
注意不要调用`str`的`strip()`方法：

```python
def trim(s):
    while (s !="" and s[0] == ' '):
        s=s[1:]
    while (s !="" and s[-1] == ' '):
        s=s[:-1]
    return s
```

当 `s=''` 时,  `s[0]` or `s[-1]` 会报错, 但`s[:1]`却不会报错, 
应该是slice 的内部处理, 所以也可以用

```python
def trim(s):
    while s[:1] == ' ':
        s = s[1:]
    while s[-1:] == ' ':
        s = s[:-1]
    return s
```

### 迭代

如果给定一个list或tuple, 我们可以通过`fo`r循环来遍历这个`list`或`tuple`, 这种遍历我们称为迭代(`Iteration`). 

在Python中, 迭代是通过`for ... in`来完成的, 而很多语言比如C语言, 迭代list是通过下标完成的, 比如Java代码：

```python
for (i=0; i<list.length; i++) {
    n = list[i];
}
```

可以看出, Python的`for`循环抽象程度要高于C的`for`循环, 因为Python的`for`循环不仅可以用在`list`或`tuple`上, 还可以作用在其他可迭代对象上. 

`list`这种数据类型虽然有下标, 但很多其他数据类型是没有下标的, 但是, 只要是可迭代对象, 无论有无下标, 都可以迭代, 比如dict就可以迭代：

```python
>>> d = {'a': 1, 'b': 2, 'c': 3}
>>> for key in d:
...     print(key)
...
a
c
b
```

因为`dict`的存储不是按照list的方式顺序排列, 所以, 迭代出的结果顺序很可能不一样. 

默认情况下, `dict`迭代的是`key`. 如果要迭代`value`, 
可以用`for value in d.values()`, 
如果要同时迭代`key`和`value`, 可以用`for k, v in d.items()`.

此处必须用 `values()`,`items()` 是类的属性

由于字符串也是可迭代对象, 因此, 也可以作用于`for`循环：

```python
>>> for ch in 'ABC':
...     print(ch)
...
A
B
C
```

所以, 当我们使用`for`循环时, 只要作用于一个可迭代对象, `for`循环就可以正常运行, 而我们不太关心该对象究竟是list还是其他数据类型. 

那么, 如何判断一个对象是可迭代对象呢？方法是通过`collections`模块的`Iterable`类型判断：

```python
>>> from collections import Iterable
>>> isinstance('abc', Iterable) # str是否可迭代
True
>>> isinstance([1,2,3], Iterable) # list是否可迭代
True
>>> isinstance(123, Iterable) # 整数是否可迭代
False
```

最后一个小问题, 如果要对`list`实现类似Java那样的下标循环怎么办？
Python内置的`enumerate`函数可以把一个`list`变成`索引-元素对`, 
这样就可以在`for`循环中同时迭代索引和元素本身：

```python
>>> for i, value in enumerate(['A', 'B', 'C']):
...     print(i, value)
...
0 A
1 B
2 C
```

上面的`for`循环里, 同时引用了两个变量, 在Python里是很常见的, 比如下面的代码：

```python
>>> for x, y in [(1, 1), (2, 4), (3, 9)]:
...     print(x, y)
...
1 1
2 4
3 9
```

#### 练习-迭代

请使用迭代查找一个list中最小和最大值, 并返回一个tuple：

```python
# -*- coding: utf-8 -*-

def findMinAndMax(L):
    if L == []:
        return (None, None)
    else:
        min = L[0]
        max = L[0]
        for i in L:
            if min > i:
                min=i
        for i in L:
            if max < i:
                max=i
        return (min, max)
```

```python
# 测试
if findMinAndMax([]) != (None, None):
    print('测试失败!')
elif findMinAndMax([7]) != (7, 7):
    print('测试失败!')
elif findMinAndMax([7, 1]) != (1, 7):
    print('测试失败!')
elif findMinAndMax([7, 1, 3, 9, 5]) != (1, 9):
    print('测试失败!')
else:
    print('测试成功!')
```

#### 小结-迭代

任何可迭代对象都可以作用于for循环, 包括我们自定义的数据类型, 只要符合迭代条件, 就可以使用for循环. 

### 列表生成式

列表生成式即`List Comprehensions`, 是Python内置的非常简单却强大的可以用来创建list的生成式. 

举个例子, 要生成`list [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
可以用`list(range(1, 11))`：

```python
>>> list(range(1, 11))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

但如果要生成`[1x1, 2x2, 3x3, ..., 10x10]`怎么做？方法一是循环：

```python
>>> L = []
>>> for x in range(1, 11):
...    L.append(x * x)
...
>>> L
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

但是循环太繁琐, 而列表生成式则可以用一行语句代替循环生成上面的list：

```python
>>> [x * x for x in range(1, 11)]
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

写列表生成式时, 把要生成的元素`x * x`放到前面, 后面跟`for`循环, 
就可以把list创建出来, 十分有用, 多写几次, 很快就可以熟悉这种语法. 

`for`循环后面还可以加上`if`判断, 这样我们就可以筛选出仅偶数的平方：

```python
>>> [x * x for x in range(1, 11) if x % 2 == 0]
[4, 16, 36, 64, 100]
```

还可以使用两层循环, 可以生成全排列：

```python
>>> [m + n for m in 'ABC' for n in 'XYZ']
['AX', 'AY', 'AZ', 'BX', 'BY', 'BZ', 'CX', 'CY', 'CZ']
```

三层和三层以上的循环就很少用到了. 

运用列表生成式, 可以写出非常简洁的代码. 例如, 列出当前目录下的所有文件和目录名, 可以通过一行代码实现：

```python
>>> import os # 导入os模块, 模块的概念后面讲到
>>> [d for d in os.listdir('.')] # os.listdir可以列出文件和目录
['.emacs.d', '.ssh', '.Trash', 'Adlm', 'Applications', 'Desktop', 'Documents', 'Downloads', 'Library', 'Movies', 'Music', 'Pictures', 'Public', 'VirtualBox VMs', 'Workspace', 'XCode']
```

`for`循环其实可以同时使用两个甚至多个变量, 
比如`dict`的`items()`可以同时迭代`key`和`value`：

```python
>>> d = {'x': 'A', 'y': 'B', 'z': 'C' }
>>> for k, v in d.items():
...     print(k, '=', v)
...
y = B
x = A
z = C
```

因此, 列表生成式也可以使用两个变量来生成list：

```python
>>> d = {'x': 'A', 'y': 'B', 'z': 'C' }
>>> [k + '=' + v for k, v in d.items()]
['y=B', 'x=A', 'z=C']
```

最后把一个list中所有的字符串变成小写：

```python
>>> L = ['Hello', 'World', 'IBM', 'Apple']
>>> [s.lower() for s in L]
['hello', 'world', 'ibm', 'apple']
```

#### 练习-列表生成器

如果list中既包含字符串, 又包含整数, 由于非字符串类型没有`lower()`方法, 所以列表生成式会报错：

```python
>>> L = ['Hello', 'World', 18, 'Apple', None]
>>> [s.lower() for s in L]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 1, in <listcomp>
AttributeError: 'int' object has no attribute 'lower'
```

使用内建的`isinstance`函数可以判断一个变量是不是字符串：

```python
>>> x = 'abc'
>>> y = 123
>>> isinstance(x, str)
True
>>> isinstance(y, str)
False
```

请修改列表生成式, 通过添加`if`语句保证列表生成式能正确地执行：

```python
# -*- coding: utf-8 -*-
L1 = ['Hello', 'World', 18, 'Apple', None]
L2=[s.lower() for s in L1 if isinstance(s,str)]

# 测试:
print(L2)
if L2 == ['hello', 'world', 'apple']:
    print('测试通过!')
else:
    print('测试失败!')
```

### 生成器

通过列表生成式, 我们可以直接创建一个列表. 但是, 受到内存限制, 列表容量肯定是有限的. 而且, 创建一个包含100万个元素的列表, 不仅占用很大的存储空间, 如果我们仅仅需要访问前面几个元素, 那后面绝大多数元素占用的空间都白白浪费了. 

所以, 如果列表元素可以按照某种算法推算出来, 那我们是否可以在循环的过程中不断推算出后续的元素呢？这样就不必创建完整的list, 从而节省大量的空间. 在Python中, 这种一边循环一边计算的机制, 称为生成器：`generator`. 

要创建一个generator, 有很多种方法. 第一种方法很简单, 只要把一个列表生成式的`[]`改成`()`, 就创建了一个generator：

```python
>>> L = [x * x for x in range(10)]
>>> L
[0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
>>> g = (x * x for x in range(10))
>>> g
<generator object <genexpr> at 0x1022ef630>
```

创建`L`和`g`的区别仅在于最外层的`[]`和`()`, 
L是一个list, 而g是一个generator. 

我们可以直接打印出list的每一个元素, 但我们怎么打印出generator的每一个元素呢？
如果要一个一个打印出来, 可以通过`next()`函数获得generator的下一个返回值：

```python
>>> next(g)
0
>>> next(g)
1
>>> next(g)
4
...
>>> next(g)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
StopIteration
```

我们讲过, generator保存的是算法, 每次调用`next(g)`, 
就计算出`g`的下一个元素的值, 直到计算到最后一个元素, 
没有更多的元素时, 抛出`StopIteration`的错误. 

当然, 上面这种不断调用`next(g)`实在是太变态了, 正确的方法是使用`for`循环, 因为generator也是可迭代对象：

```python
>>> g = (x * x for x in range(10))
>>> for n in g:
...     print(n)
...
0
1
4
...
81
```

所以, 我们创建了一个generator后, 基本上永远不会调用`next()`, 
而是通过`for`循环来迭代它, 并且不需要关心`StopIteration`的错误. 

`generator`非常强大. 如果推算的算法比较复杂, 
用类似列表生成式的`for`循环无法实现的时候, 还可以用函数来实现. 
比如, 著名的斐波拉契数列(`Fibonacci`), 
除第一个和第二个数外, 任意一个数都可由前两个数相加得到：

```python
1, 1, 2, 3, 5, 8, 13, 21, 34, ...
```

斐波拉契数列用列表生成式写不出来, 但是, 用函数把它打印出来却很容易：

```python
def fib(max):
    n, a, b = 0, 0, 1
    while n < max:
        print(b)
        a, b = b, a + b
        n = n + 1
    return 'done'
```

注意, 赋值语句：

```python
a, b = b, a + b
```

相当于：

```python
t = (b, a + b) # t是一个tuple
a = t[0]
b = t[1]
```

但不必显式写出临时变量`t`就可以赋值. 

上面的函数可以输出斐波那契数列的前`N`个数：

```python
>>> fib(6)
1
1
2
3
5
8
'done'
```

仔细观察, 可以看出, `fib`函数实际上是定义了斐波拉契数列的推算规则, 
可以从第一个元素开始, 推算出后续任意的元素, 这种逻辑其实非常类似generator. 

也就是说, 上面的函数和`generator`仅一步之遥. 要把fib函数变成`generator`, 
只需要把`print(b)`改为`yield b`就可以了：

```python
def fib(max):
    n, a, b = 0, 0, 1
    while n < max:
        yield b
        a, b = b, a + b
        n = n + 1
    return 'done'
```

这就是定义`generator`的另一种方法. 如果一个函数定义中包含`yield`关键字, 那么这个函数就不再是一个普通函数, 而是一个`generator`：

```python
>>> f = fib(6)
>>> f
<generator object fib at 0x104feaaa0>
```

这里, 最难理解的就是`generator`和函数的执行流程不一样. 
函数是顺序执行, 遇到`return`语句或者最后一行函数语句就返回. 
而变成`generator`的函数, 在每次调用`next()`的时候执行, 遇到`yield`语句返回, 再次执行时从上次返回的`yield`语句处继续执行. 

举个简单的例子, 定义一个generator, 依次返回数字`1, 3, 5`：

```python
def odd():
    print('step 1')
    yield 1
    print('step 2')
    yield(3)
    print('step 3')
    yield(5)
```

调用该generator时, 首先要生成一个generator对象, 
然后用`next()`函数不断获得下一个返回值：

```python
>>> o = odd()
>>> next(o)
step 1
1
>>> next(o)
step 2
...
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
StopIteration
```

可以看到, `odd`不是普通函数, 而是`generator`, 在执行过程中, 遇到`yield`就中断, 下次又继续执行. 
执行3次`yield`后, 已经没有`yield`可以执行了, 
所以, 第4次调用`next(o)`就报错. 

回到`fib`的例子, 我们在循环过程中不断调用`yield`, 就会不断中断. 当然要给循环设置一个条件来退出循环, 不然就会产生一个无限数列出来. 

同样的, 把函数改成`generator`后, 我们基本上从来不会用`next()`来获取下一个返回值, 而是直接使用`for`循环来迭代：

```python
>>> for n in fib(6):
...     print(n)
...
1
1
2
3
5
8
```

但是用`for`循环调用`generator`时, 发现拿不到`generator`的`return`语句的返回值. 
如果想要拿到返回值, 必须捕获`StopIteration`错误, 返回值包含在`StopIteration`的`value`中：

```python
>>> g = fib(6)
>>> while True:
...     try:
...         x = next(g)
...         print('g:', x)
...     except StopIteration as e:
...         print('Generator return value:', e.value)
...         break
...
g: 1
g: 1
g: 2
g: 3
g: 5
g: 8
Generator return value: done
```

关于如何捕获错误, 后面的错误处理还会详细讲解. 

杨辉三角定义如下：

```python
          1
         / \
        1   1
       / \ / \
      1   2   1
     / \ / \ / \
    1   3   3   1
   / \ / \ / \ / \
  1   4   6   4   1
 / \ / \ / \ / \ / \
1   5   10  10  5   1
```

把每一行看做一个list, 试写一个generator, 不断输出下一行的list：

```python
# -*- coding: utf-8 -*-
def factorial(s):
    i=1
    prod=1
    if s<=1:
        return 1
    else:
        while i <= s:
            prod=prod*i
            i=i+1
        return prod

def triangles():
    n=1
    while True:
        if n == 1:
            yield [1]
            n=n+1
        else:
            yield [factorial(n-1)//(factorial(m-1)*factorial(n-m))  for m in list(range(n+1))[1:]]
            n=n+1
    return 'done'

# 期待输出:

n = 0
results = []
for t in triangles():
    results.append(t)
    n = n + 1
    if n == 10:
        break

for t in results:
    print(t)

if results == [
    [1],
    [1, 1],
    [1, 2, 1],
    [1, 3, 3, 1],
    [1, 4, 6, 4, 1],
    [1, 5, 10, 10, 5, 1],
    [1, 6, 15, 20, 15, 6, 1],
    [1, 7, 21, 35, 35, 21, 7, 1],
    [1, 8, 28, 56, 70, 56, 28, 8, 1],
    [1, 9, 36, 84, 126, 126, 84, 36, 9, 1]
]:
    print('测试通过!')
else:
    print('测试失败!')
```

### 迭代器

我们已经知道, 可以直接作用于`for`循环的数据类型有以下几种：

一类是集合数据类型, 如list, tuple, dict, set, str等；
一类是generator, 包括生成器和带yield的generator function. 
这些可以直接作用于`for`循环的对象统称为可迭代对象：`Iterable`. 

可以使用`isinstance()`判断一个对象是否是Iterable对象：

```python
>>> from collections import Iterable
>>> isinstance([], Iterable)
True
>>> isinstance({}, Iterable)
True
>>> isinstance('abc', Iterable)
True
>>> isinstance((x for x in range(10)), Iterable)
True
>>> isinstance(100, Iterable)
False
```

而生成器不但可以作用于`for`循环, 
还可以被`next()`函数不断调用并返回下一个值, 
直到最后抛出`StopIteration`错误表示无法继续返回下一个值了. 
可以被`next()`函数调用并不断返回下一个值的对象称为迭代器：`Iterator`. 

可以使用`isinstance()`判断一个对象是否是`Iterator`对象：

```python
>>> from collections import Iterator
>>> isinstance((x for x in range(10)), Iterator)
True
>>> isinstance([], Iterator)
False
>>> isinstance({}, Iterator)
False
>>> isinstance('abc', Iterator)
False
```

生成器都是`Iterator`对象, 但list, dict, str虽然是`Iterable`, 却不是`Iterator`. 

把list, dict, str等Iterable变成`Iterator`可以使用`iter()`函数：

```python
>>> isinstance(iter([]), Iterator)
True
>>> isinstance(iter('abc'), Iterator)
True
```

你可能会问, 为什么list, dict, str等数据类型不是`Iterator`？

这是因为Python的Iterator对象表示的是一个数据流, Iterator对象可以被`next()`函数调用并不断返回下一个数据, 直到没有数据时抛出StopIteration错误. 
可以把这个数据流看做是一个有序序列, 但我们却不能提前知道序列的长度, 只能不断通过`next()`函数实现按需计算下一个数据, 所以Iterator的计算是惰性的, 只有在需要返回下一个数据时它才会计算. 

Iterator甚至可以表示一个无限大的数据流, 例如全体自然数. 而使用list是永远不可能存储全体自然数的. 

#### 小结-迭代器

凡是可作用于`for`循环的对象都是`Iterable`类型；
凡是可作用于`next()`函数的对象都是`Iterator`类型, 它们表示一个惰性计算的序列；

集合数据类型如list, dict, str等是Iterable但不是Iterator, 不过可以通过`iter()`函数获得一个`Iterator`对象. 

Python的`for`循环本质上就是通过不断调用`next()`函数实现的, 例如：

```python
for x in [1, 2, 3, 4, 5]:
    pass
```

实际上完全等价于：

```python
# 首先获得Iterator对象:
it = iter([1, 2, 3, 4, 5])
# 循环:
while True:
    try:# 获得下一个值:
        x = next(it)
    except StopIteration: # 遇到StopIteration就退出循环
        break
```
