# python-1.md

ref: [这是小白的Python新手教程](https://www.liaoxuefeng.com/wiki/1016959663602400)

## Python解释器

括号表达式可以断行, 普通断行用 `\`转义
当我们编写 `Python` 代码时, 我们得到的是包含`Python`代码的以`.py`为扩展名的文本文件.
要运行代码, 就需要`Python`解释器去执行`.py`文件.

在命令行模式下敲命令`python`, 就看到类似如下的一堆文本输出, 然后就进入到`Python交互模式`, 它的提示符是`>>>`.
在 `Python` 交互模式下输入`exit()`并回车, 就退出了`Python`交互模式, 并回到命令行模式

如果要让`Python`打印出指定的文字, 可以用`print()`函数,
然后把希望打印的文字用 `单引号` 或者 `双引号` 括起来, 但不能混用 `单引号` 和 `双引号`:

```python
>>> print('hello, world')
hello, world
```

这种用单引号或者双引号括起来的文本在程序中叫 `字符串`, 今后我们还会经常遇到.
在命令行模式下, 可以执行`python`进入`Python`交互式环境, 也可以执行`python hello.py`运行 `.py` 文件.
执行 `.py` 文件只能在命令行模式执行.
想要输出结果, 必须自己用`print()`打印出来

能不能像`.exe`文件那样直接运行`.py`文件呢?
在Windows上是不行的, 但是, 在Mac和Linux上是可以的, 方法是在`.py`文件的第一行加上 `特殊注释`:

**python env 设置**

```python
#!/usr/bin/env python3
print('hello, world')
```

然后, 通过命令给`hello.py`以执行权限:

```bash
$ chmod a+x hello.py
nothing
```

就可以直接运行`hello.py`了

### Python空行

[Python 基础语法](https://www.runoob.com/python/python-basic-syntax.html)

`函数`之间或 `类的方法` 之间用空行分隔, 表示一段新的代码的开始.
`类`和 `函数` 入口之间也用一行空行分隔, 以突出函数入口的开始.

空行与代码缩进不同, 空行并不是Python语法的一部分.
书写时不插入空行, Python解释器运行也不会出错.
但是空行的作用在于分隔两段不同功能或含义的代码, 便于日后代码的维护或重构.

记住: 空行也是程序代码的一部分.

#### 行和缩进

学习 Python 与其他语言最大的区别就是, Python 的代码块不使用大括号 `{}` 来控制类, 函数以及其他逻辑判断.
python 最具特色的就是用 `缩进` 来写模块.

缩进的空白数量是可变的, 但是所有代码块语句必须包含相同的缩进空白数量, 这个必须严格执行.

### 多行语句

Python 语句中一般以新行作为语句的结束符.

但是我们可以使用斜杠( `\`)将一行的语句分为多行显示, 如下所示:

```python
total = item_one + \
        item_two + \
        item_three
```

语句中包含 `[]`, `{}` 或 `()` 括号就不需要使用多行连接符. 如下实例:

```python
days = ['Monday', 'Tuesday', 'Wednesday',
        'Thursday', 'Friday']
```

### 输出

`print()`函数也可以接受多个字符串, 用逗号","隔开, 就可以连成一串输出:

```python
>>> print('The quick brown fox', 'jumps over', 'the lazy dog')
The quick brown fox jumps over the lazy dog
```

`print()`会依次打印每个字符串, 遇到逗号`,`会输出一个空格, 因此, 输出的字符串是这样拼起来的

### 输入

`Python`提供了 `input()`, 可以让用户输入字符串, 并存放到变量里. 比如输入用户的名字:

```python
>>> name = input()
Michael
```

当你输入`name = input()`并按下回车后, `Python`交互式命令行就在等待你的输入了.
这时, 你可以输入任意字符, 然后按回车后完成输入.

输入完成后, 不会有任何提示, `Python`交互式命令行又回到`>>>`状态了.
那我们刚才输入的内容到哪去了? 答案是存放到`name`变量里了.
可以直接输入`name`查看变量内容:

```python
>>> name
'Michael'
```

`input()`可以让你显示 `字符串` 来提示用户, 于是我们把代码改成:

```python
name = input('please enter your name: ')
print('hello,', name)
```

再次运行这个程序, 你会发现, 程序一运行, 会首先打印出`please enter your name:`,
这样, 用户就可以根据提示, 输入名字后, 得到`hello, xxx`的输出

## Python基础

`Python`的语法比较简单, 采用缩进方式, 写出来的代码就像下面的样子:

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

缩进有利有弊. 好处是强迫你写出格式化的代码, 但没有规定缩进是几个空格还是`Tab`.
按照约定俗成的惯例, 应该始终坚持使用`4`个空格的缩进.

缩进的另一个好处是强迫你写出缩进较少的代码,
你会倾向于把一段很长的代码拆分成若干函数, 从而得到缩进较少的代码.

缩进的坏处就是 "复制－粘贴" 功能失效了, 这是最坑爹的地方.
当你重构代码时, 粘贴过去的代码必须重新检查缩进是否正确.

此外, IDE很难像格式化 `Java` 代码那样格式化 `Python` 代码.
最后, 请务必注意, Python程序是大小写敏感的, 如果写错了大小写, 程序会报错.

### 数据类型和变量

在Python中, 能够直接处理的数据类型有以下几种:

#### 整数

Python可以处理任意大小的整数, 当然包括 `负整数`,
在程序中的表示方法和数学上的写法一模一样, 例如: `1`, `100`, `-8080`, `0`, 等等.

计算机由于使用二进制, 所以, 有时候用十六进制表示整数比较方便,
十六进制用`0x`前缀和`0-9`, `a-f`表示, 例如: `0xff00`, `0xa5b4c3d2`, 等等.

#### 浮点数

浮点数也就是小数, 之所以称为浮点数, 是因为按照科学记数法表示时,
`浮点数`的小数点位置是可变的, 比如, `1.23x10^9` 和 `12.3x10^8` 是完全相等的.

浮点数可以用数学写法, 如`1.23`, `3.14`, `-9.01`, 等等.
但是对于很大或很小的浮点数, 就必须用科学计数法表示,
把`10`用`e`替代, `1.23x10^9` 就是 `1.23e9`, 或者 `12.3e8`, `0.000012`可以写成`1.2e-5`, 等等.

整数和浮点数在计算机内部存储的方式是不同的,
整数运算永远是精确的(除法难道也是精确的? 是的! ), 而浮点数运算则可能会有四舍五入的误差.

#### 字符串

字符串是以单引号`'`或双引号`"`括起来的任意文本, 比如`'abc'`, `"xyz"`等等.
请注意, `''`或`""`本身只是一种表示方式, 不是字符串的一部分,
因此, 字符串`'abc'`只有`a`, `b`, `c`这3个字符.

如果`'`本身也是一个字符, 那就可以用`""`括起来,
比如 `"I'm OK"` 包含的字符是`I`, `'`, `m`, `空格`, `O`, `K`这6个字符.

如果字符串内部既包含`'`又包含`"`怎么办? 可以用转义字符`\`来标识, 比如:

    'I\'m \"OK\"!'

表示的字符串内容是:

    I'm "OK"!

转义字符\可以转义很多字符, 比如`\n`表示换行, `\t`表示制表符,
字符`\`本身也要转义, 所以`\\`表示的字符就是`\`,
可以在Python的交互式命令行用`print()`打印字符串看看:

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

如果字符串里面有很多字符都需要转义, 就需要加很多`\`,
为了简化, Python还允许用`r'text'`表示`text`字符串默认不转义, 可以自己试试:

```python
>>> print('\\\t\\')
\       \
>>> print(r'\\\t\\')
\\\t\\
```

如果字符串内部有很多换行, 用 `\n` 写在一行里不好阅读,
为了简化, Python允许用`'''...'''`的格式表示多行内容, 可以自己试试:

```python
>>> print('''line1
... line2
... line3''')
line1
line2
line3
```

上面是在交互式命令行内输入, 注意在输入多行内容时,
提示符由`>>>`变为`...`, 提示你可以接着上一行输入, 注意`...`是提示符, 不是代码的一部分:
当输入完结束符`'''`和括号`)`后, 执行该语句并打印结果.

如果写成程序并存为`.py`文件, 就是:

```python
print('''line1
line2
line3''')
```

多行字符串`'''...'''`还可以在前面加上`r`使用

#### 布尔值

`布尔值` 和布尔代数的表示完全一致, `布尔值` 只有`True`, `False`两种值, 要么是`True`, 要么是`False`,
在Python中, 可以直接用`True`, `False`表示布尔值(请注意 `大小写`), 也可以通过布尔运算计算出来:

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

布尔值经常用在条件判断中, 比如:

```python
if age >= 18:
    print('adult')
else:
    print('teenager')
```

##### 空值,None

`空值` 是Python里一个特殊的值, 用 `None` 表示.
`None`不能理解为`0`, 因为`0`是有意义的, 而`None`是一个特殊的空值.
此外, Python还提供了列表, 字典等多种数据类型, 还允许创建自定义数据类型, 我们后面会继续讲到.

### 变量

变量的概念基本上和初中代数的方程变量是一致的, 只是在计算机程序中, 变量不仅可以是数字, 还可以是任意数据类型.
变量在程序中就是用一个变量名表示了, 变量名必须是大小写`英文`, `数字`和`_`的组合, 且不能用数字开头, 比如:

```python
Answer = True
```

在Python中, 等号`=`是赋值语句, 可以把任意数据类型赋值给变量,
同一个变量可以反复赋值, 而且可以是不同类型的变量, 例如:

```python
a = 123 # a是整数
print(a)
a = 'ABC' # a变为字符串
print(a)
```

这种变量本身类型不固定的语言称之为动态语言, 与之对应的是静态语言.
静态语言在定义变量时必须指定变量类型, 如果赋值的时候类型不匹配, 就会报错.
例如Java是静态语言, 赋值语句如下(`//` 表示注释):

```python
int a = 123; // a是整数类型变量
a = "ABC"; // 错误: 不能把字符串赋给整型变量
```

和静态语言相比, 动态语言更灵活, 就是这个原因.
请不要把赋值语句的等号等同于数学的等号.

最后, 理解变量在计算机内存中的表示也非常重要. 当我们写:

```python
a = 'ABC'
```

时, `Python` 解释器干了两件事情:

+ 在内存中创建了一个 `'ABC'` 的字符串;
+ 在内存中创建了一个名为 `a` 的变量, 并把它指向 `'ABC'`.

也可以把变量`a`赋值给另一个变量`b`, 这个操作实际上是把变量`b`指向变量`a`所指向的数据, 例如下面的代码:

#### 常量

所谓常量就是不能变的变量, 比如常用的数学常数 `PI` 就是一个常量.
在Python中, 通常用全部大写的变量名表示常量:

```python
PI = 3.14159265359
```

但事实上`PI`仍然是变量, Python根本没有任何机制保证PI不会被改变,
所以, 用全部大写的变量名表示常量只是习惯上的用法, 如果你一定要改变变量`PI`的值, 也没人能拦住你.

最后解释一下整数的除法为什么也是精确的. 在 `Python` 中, 有两种除法, 一种除法是`/`:

```python
>>> 10 / 3
3.3333333333333335
```

`/`除法计算结果是浮点数, 即使是两个整数恰好整除, 结果也是浮点数:

```python
>>> 9 / 3
3.0
```

还有一种除法是`//`, 称为地板除(floor), 两个整数的除法仍然是整数:

```python
>>> 10 // 3
3
```

你没有看错, 整数的地板除`//`永远是整数, 即使除不尽. 要做精确的除法, 使用`/`就可以.

因为`//`除法只取结果的整数部分, 所以 Python 还提供 `余数` 运算, 可以得到两个整数相除的余数:

```python
>>> 10 % 3
1
```

无论整数做`//`除法还是取余数, 结果永远是整数, 所以, 整数运算结果永远是精确的.

#### 变量小结

Python支持多种数据类型, 在计算机内部, 可以把任何数据都看成一个"对象",
而变量就是在程序中用来指向这些数据对象的, 对变量赋值就是把数据和变量给关联起来.

对变量赋值`x = y`是把变量`x`指向真正的对象, 该对象是变量`y`所指向的. 随后对变量`y`的赋值不影响变量`x`的指向.
注意: Python的整数没有大小限制, 而某些语言的整数根据其存储长度是有大小限制的,
例如`Java`对 `32` 位整数的范围限制在`-2147483648-2147483647`.

`Python`的浮点数也没有大小限制, 但是超出一定范围就直接表示为`inf`(无限大).

### 字符串和编码

#### 字符编码

字符串比较特殊的是还有 `编码` 问题

因为计算机只能处理数字, 如果要处理文本, 就必须先把文本转换为数字才能处理.
最早的计算机在设计时采用`8`个比特(`bit`)作为一个字节(`byte`),
所以, 一个字节能表示的最大的整数就是`255`(二进制`11111111`=十进制255),
如果要表示更大的整数, 就必须用更多的字节.
比如两个字节可以表示的最大整数是`65535`, 4个字节可以表示的最大整数是`4294967295`.

由于计算机是美国人发明的, 因此, 最早只有`127`个字符被编码到计算机里,
也就是大小写英文字母, 数字和一些符号,
这个编码表被称为`ASCII`编码, 比如大写字母`A`的编码是`65`, 小写字母`z`的编码是`122`

因此, `Unicode`应运而生. `Unicode`把所有语言都统一到一套编码里, 这样就不会再有乱码问题了.

`Unicode`标准也在不断发展, 但最常用的是用两个字节表示一个字符(如果要用到非常偏僻的字符, 就需要4个字节).
现代操作系统和大多数编程语言都直接支持`Unicode`.

现在, 捋一捋ASCII编码和`Unicode`编码的区别: `ASCII`编码是1个字节, 而`Unicode`编码通常是`2`个字节.
字母`A`用`ASCII`编码是十进制的`65`, 二进制的`01000001`;
字符`0`用`ASCII`编码是十进制的`48`, 二进制的`00110000`, 注意字符`'0'`和整数`0`是不同的;
汉字`中`已经超出了`ASCII`编码的范围, 用`Unicode`编码是十进制的`20013`, 二进制的`01001110 00101101`.
你可以猜测, 如果把`ASCII`编码的`A`用`Unicode`编码, 只需要在前面补`0`就可以, 因此, `A`的`Unicode`编码是`00000000 01000001`.
但是, 如果你写的文本基本上全部是英文的话, 用`Unicode`编码比`ASCII`编码需要多一倍的存储空间, 在存储和传输上就十分不划算.

所以, 本着节约的精神, 又出现了把`Unicode`编码转化为"可变长编码"的`UTF-8`编码. `UTF-8`编码把一个`Unicode`字符根据不同的数字大小编码成`1-6`个字节, 常用的英文字母被编码成`1`个字节, 汉字通常是`3`个字节, 只有很生僻的字符才会被编码成`4-6`个字节. 如果你要传输的文本包含大量英文字符, 用`UTF-8`编码就能节省空间:

搞清楚了`ASCII`, `Unicode`和`UTF-8`的关系, 我们就可以总结一下现在计算机系统通用的字符编码工作方式:
在计算机内存中, 统一使用Unicode编码, 当需要保存到硬盘或者需要传输的时候, 就转换为UTF-8编码.
用记事本编辑的时候, 从文件读取的UTF-8字符被转换为Unicode字符到内存里, 编辑完成后, 保存的时候再把Unicode转换为UTF-8保存到文件:

```file
rw-file-utf-8
```

浏览网页的时候, 服务器会把动态生成的Unicode内容转换为UTF-8再传输到浏览器:
所以你看到很多网页的源码上会有类似`<meta charset="UTF-8" />`的信息, 表示该网页正是用的UTF-8编码.

#### Python的字符串

搞清楚了令人头疼的字符编码问题后, 我们再来研究Python的字符串.
在最新的Python 3版本中, 字符串是以Unicode编码的, 也就是说, Python的字符串支持多语言, 例如:

```python
>>> print('包含中文的str')
包含中文的str
```

对于单个字符的编码, Python提供了`ord()`函数获取字符的整数表示, `chr()`函数把编码转换为对应的字符:

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

如果知道字符的整数编码, 还可以用十六进制这么写`str`:

```python
>>> '\u4e2d\u6587'
'中文'
```

两种写法完全是等价的.

由于Python的字符串类型是`str`, 在内存中以`Unicode`表示, 一个字符对应若干个字节.
如果要在网络上传输, 或者保存到磁盘上, 就需要把`str`变为以字节为单位的`bytes`.

Python对`bytes`类型的数据用带`b`前缀的单引号或双引号表示:

```python
x = b'ABC'
```

要注意区分`'ABC'`和`b'ABC'`, 前者是`str`, 后者虽然内容显示得和前者一样, 但`b'ABC'`的每个字符都只占用一个字节.

以`Unicode`表示的`str`通过`encode()`方法可以编码为指定的`bytes`, 例如:

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
要把`bytes`变为`str`, 就需要用`decode()`方法:

```python
>>> b'ABC'.decode('ascii')
'ABC'
>>> b'\xe4\xb8\xad\xe6\x96\x87'.decode('utf-8')
'中文'
```

如果`bytes`中包含无法解码的字节, decode()方法会报错:

```python
>>> b'\xe4\xb8\xad\xff'.decode('utf-8')
Traceback (most recent call last):
  ...
UnicodeDecodeError: 'utf-8' codec can't decode byte 0xff in position 3: invalid start byte
```

如果`bytes`中只有一小部分无效的字节, 可以传入`errors='ignore'`忽略错误的字节:

```python
>>> b'\xe4\xb8\xad\xff'.decode('utf-8', errors='ignore')
'中'
```

要计算`str`包含多少个字符, 可以用`len()`函数:

```python
>>> len('ABC')
3
>>> len('中文')
2
```

`len()`函数计算的是`str`的字符数, 如果换成`bytes`, `len()`函数就计算字节数:

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

由于Python源代码也是一个文本文件, 所以, 当你的源代码中包含中文的时候, 在保存源代码时, 就需要务必指定保存为`UTF-8`编码. 当Python解释器读取源代码时, 为了让它按`UTF-8`编码读取, 我们通常在文件开头写上这两行:

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
```

第一行注释是为了告诉`Linux/OS X`系统, 这是一个`Python`可执行程序, `Windows`系统会忽略这个注释;
第二行注释是为了告诉Python解释器, 按照`UTF-8`编码读取源代码, 否则, 你在源代码中写的中文输出可能会有乱码.
申明了`UTF-8`编码并不意味着你的`.py`文件就是`UTF-8`编码的, 必须并且要确保文本编辑器正在使用`UTF-8 without BOM`编码:

如果`.py`文件本身使用`UTF-8`编码, 并且也申明了`# -*- coding: utf-8 -*-`, 打开命令提示符测试就可以正常显示中文:

#### 格式化

最后一个常见的问题是如何输出格式化的字符串. 我们经常会输出类似`'亲爱的xxx你好! 你xx月的话费是xx, 余额是xx'`之类的字符串, 而`xxx`的内容都是根据变量变化的, 所以, 需要一种简便的格式化字符串的方式.

在Python中, 采用的格式化方式和C语言是一致的, 用%实现, 举例如下:

```python
>>> 'Hello, %s' % 'world'
'Hello, world'
>>> 'Hi, %s, you have $%d.' % ('Michael', 1000000)
'Hi, Michael, you have $1000000.'
```

你可能猜到了, `%`运算符就是用来格式化字符串的. 在字符串内部, `%s`表示用字符串替换, `%d`表示用整数替换, 有几个`%?`占位符, 后面就跟几个变量或者值, 顺序要对应好. 如果只有一个`%?`, 括号可以省略.

常见的占位符有:

| 占位符 | 替换内容 |
| -----  | -----  |
| `%d` | 整数 |
| `%f` | 浮点数 |
| `%s` | 字符串 |
| `%x` | 十六进制整数 |

如果你不太确定应该用什么, `%s`永远起作用, 它会把任何数据类型转换为字符串:

```python
>>> 'Age: %s. Gender: %s' % (25, True)
'Age: 25. Gender: True'
```

有些时候, 字符串里面的`%`是一个普通字符怎么办? 这个时候就需要转义, 用`%%`来表示一个`%`:

```python
>>> 'growth rate: %d %%' % 7
'growth rate: 7 %'
```

#### format()

另一种格式化字符串的方法是使用字符串的`format()`方法, 它会用传入的参数依次替换字符串内的占位符`{0}`, `{1}`... ... , 不过这种方式写起来比`%`要麻烦得多:

```python
>>> 'Hello, {0}, 成绩提升了 {1:.1f}%'.format('小明', 17.125)
'Hello, 小明, 成绩提升了 17.1%'
```

### 字符串和编码小结

`Python3`的字符串使用`Unicode`, 直接支持多语言.

当`str`和`bytes`互相转换时, 需要指定编码. 最常用的编码是`UTF-8`. `Python`当然也支持其他编码方式, 比如把`Unicode`编码成`GB2312`:

```python
>>> '中文'.encode('gb2312')
b'\xd6\xd0\xce\xc4'
```

但这种方式纯属自找麻烦, 如果没有特殊业务要求, 请牢记仅使用`UTF-8`编码.
格式化字符串的时候, 可以用Python的交互式环境测试, 方便快捷.

### 使用list和tuple

#### list 列表

Python内置的一种数据类型是列表: `list`. `list`是一种有序的集合, 可以随时添加和删除其中的元素.
比如, 列出班里所有同学的名字, 就可以用一个`list`表示:

```python
>>> classmates = ['Michael', 'Bob', 'Tracy']
>>> classmates
['Michael', 'Bob', 'Tracy']
```

变量`classmates`就是一个list. 用`len()`函数可以获得list元素的个数:

```python
>>> len(classmates)
3
```

用索引来访问list中每一个位置的元素, 记得索引是从`0`开始的:

```python
>>> classmates[0]
'Michael'
>>> classmates[3]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
IndexError: list index out of range
```

当索引超出了范围时, Python会报一个`IndexError`错误, 所以, 要确保索引不要越界, 记得最后一个元素的索引是`len(classmates) - 1`.

如果要取最后一个元素, 除了计算索引位置外, 还可以用`-1`做索引, 直接获取最后一个元素:

```python
>>> classmates[-1]
'Tracy'
```

以此类推, 可以获取倒数第2个, 倒数第3个:

```python
>>> classmates[-2]
'Bob'
>>> classmates[-3]
'Michael'
```

当然, 倒数第4个就越界了.

`list`是一个可变的有序表, 所以, 可以往`list`中追加元素到末尾:

```python
>>> classmates.append('Adam')
>>> classmates
['Michael', 'Bob', 'Tracy', 'Adam']
```

也可以把元素插入到指定的位置, 比如索引号为`1`的位置:

```python
>>> classmates.insert(1, 'Jack')
>>> classmates
['Michael', 'Jack', 'Bob', 'Tracy', 'Adam']
```

要删除list末尾的元素, 用`pop()`方法:

```python
>>> classmates.pop()
'Adam'
>>> classmates
['Michael', 'Jack', 'Bob', 'Tracy']
```

要删除指定位置的元素, 用`pop(i)`方法, 其中`i`是索引位置:

```python
>>> classmates.pop(1)
'Jack'
>>> classmates
['Michael', 'Bob', 'Tracy']
```

要把某个元素替换成别的元素, 可以直接赋值给对应的索引位置:

```python
>>> classmates[1] = 'Sarah'
>>> classmates
['Michael', 'Sarah', 'Tracy']
```

`list`里面的元素的数据类型也可以不同, 比如:

```python
>>> L = ['Apple', 123, True]
```

`list`元素也可以是另一个`list`, 比如:

```python
>>> s = ['python', 'java', ['asp', 'php'], 'scheme']
>>> len(s)
4
```

要注意`s`只有`4`个元素, 其中`s[2]`又是一个`list`, 如果拆开写就更容易理解了:

```python
>>> p = ['asp', 'php']
>>> s = ['python', 'java', p, 'scheme']
```

要拿到`'php'`可以写`p[1]`或者`s[2][1]`, 因此`s`可以看成是一个二维数组, 类似的还有三维, 四维... ... 数组, 不过很少用到.

如果一个`list`中一个元素也没有, 就是一个`空的``list`, 它的长度为`0`:

```python
>>> L = []
>>> len(L)
0
```

#### tuple 元组

另一种有序列表叫元组: `tuple`.
`tuple` 和 `list` 非常类似, 但是 `tuple` 一旦初始化就不能修改, 比如同样是列出同学的名字:

```python
>>> classmates = ('Michael', 'Bob', 'Tracy')
```

现在, `classmates`这个`tuple`不能变了, 它也没有`append()`, `insert()`这样的方法.
其他获取元素的方法和list是一样的, 你可以正常地使用`classmates[0]`, `classmates[-1]`, 但不能赋值成另外的元素.

不可变的 `tuple` 有什么意义? 因为 `tuple` 不可变, 所以代码更安全. 如果可能, 能用 `tuple` 代替list就尽量用 `tuple` .

`tuple` 的陷阱: 当你定义 `tuple` 时, 在定义的时候, tuple的元素就必须被确定下来, 比如:

```python
>>> t = (1, 2)
>>> t
(1, 2)
```

如果要定义一个空的tuple, 可以写成`()`:

```python
>>> t = ()
>>> t
()
```

但是, 要定义一个只有1个元素的tuple, 如果你这么定义:

```python
>>> t = (1)
>>> t
1
```

定义的不是tuple, 是`1`这个数!
这是因为括号`()`既可以表示tuple, 又可以表示数学公式中的`小括号`, 这就产生了歧义.
因此, Python规定, 这种情况下, 按小括号进行计算, 计算结果自然是`1`.

所以, 只有 `1` 个元素的 `tuple` 定义时必须加逗号 `,`, 来消除歧义:

```python
>>> t = (1,)
>>> t
(1,)
```

Python在显示只有 `1` 个元素的tuple时, 也会加一个逗号`,`, 以免你误解成数学计算意义上的括号.

最后来看一个"可变的"tuple:

```python
>>> t = ('a', 'b', ['A', 'B'])
>>> t[2][0] = 'X'
>>> t[2][1] = 'Y'
>>> t
('a', 'b', ['X', 'Y'])
```

这个tuple定义的时候有3个元素, 分别是`'a'`, `'b'`和一个`list`. 不是说tuple一旦定义后就不可变了吗? 怎么后来又变了?

tuple所谓的"不变"是说, tuple的每个元素, 指向永远不变. 即指向`'a'`, 就不能改成指向`'b'`, 指向一个list, 就不能改成指向其他对象, 但指向的这个list本身是可变的!

理解了"指向不变"后, 要创建一个内容也不变的tuple怎么做? 那就必须保证tuple的每一个元素本身也不能变.

### 条件判断,if

计算机之所以能做很多自动化的任务, 因为它可以自己做条件判断.
比如, 输入用户年龄, 根据年龄打印不同的内容, 在Python程序中, 用 `if` 语句实现:

```python
age = 20
if age >= 18:
    print('your age is', age)
    print('adult')
```

根据 `Python` 的缩进规则, 如果 `if` 语句判断是`True`,
就把缩进的两行`print`语句执行了, 否则 **什么也不做**.

Python程序语言指定任何非 `0` 和非空(`null`)值为 `True`, `0` 或者 `null` 为`False`:

```python
>>>bool('sadfasdf')
True
```

由于 `python` 并不支持 `switch` 语句, 所以多个条件判断, 只能用 `elif` 来实现,
如果判断需要多个条件需同时判断时, 可以使用 `or` (或), 表示两个条件有一个成立时判断条件成功;
使用 `and` (与)时, 表示只有两个条件同时成立的情况下, 判断条件才成功.

也可以给`if`添加`else`语句, 意思是, 如果`if`判断是`False`, 不要执行`if`的内容, 去把`else`执行了:

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
当然上面的判断是很粗略的, 完全可以用`elif`做更细致的判断:

```python
age = 3
if age >= 18:
    print('adult')
elif age >= 6:
    print('teenager')
else:
    print('kid')
```

`elif`是`else if`的缩写, 完全可以有多个`elif`, 所以`if`语句的完整形式就是:

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

`if`语句执行有个特点, 它是从上往下判断, 如果在某个判断上是`True`, 把该判断对应的语句执行后,
就忽略掉剩下的`elif`和`else`, 所以请测试并解释为什么下面的程序打印的是`teenager`:

```python
age = 20
if age >= 6:
    print('teenager')
elif age >= 18:
    print('adult')
else:
    print('kid')
```

`if`判断条件还可以简写, 比如写:

```python
if x:
    print('True')
```

只要 `x` 是 `非零`数值, `非空`字符串, `非空`list等, 就判断为`True`, 否则为`False`.

#### 再议-input

最后看一个有问题的条件判断.
很多同学会用`input()`读取用户的输入, 这样可以自己输入, 程序运行得更有意思:

```python
birth = input('birth: ')
if birth < 2000:
    print('00前')
else:
    print('00后')
```

输入`1982`, 结果报错:

```python
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: unorderable types: str() > int()
```

这是因为`input()`返回的数据类型是`str`, `str`不能直接和 `整数` 比较, 必须先把`str`转换成 `整数`.
Python提供了`int()`函数来完成这件事情:

```python
s = input('birth: ')
birth = int(s)
if birth < 2000:
    print('00前')
else:
    print('00后')
```

再次运行, 就可以得到正确地结果. 但是, 如果输入`abc`呢? 又会得到一个错误信息:

```python
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ValueError: invalid literal for int() with base 10: 'abc'
```

原来`int()`函数发现字符串并不是合法的 `数字` 时就会报错, 程序就退出了.

如何检查并捕获程序运行期的错误呢? 后面的错误和调试会讲到.

### 循环

为了让计算机能计算成千上万次的重复运算, 我们就需要循环语句.

`Python` 的循环有两种, 一种是`for...in`循环, 依次把`list`或`tuple`中的每个元素迭代出来, 看例子:

```python
names = ['Michael', 'Bob', 'Tracy']
for name in names:
    print(name)
```

执行这段代码, 会依次打印`names`的每一个元素:

```python
Michael
Bob
Tracy
```

所以`for x in ...`循环就是把每个元素代入变量`x`, 然后执行缩进块的语句.
再比如我们想计算`1-10`的整数之和, 可以用`sum`变量做累加:

```python
sum = 0
for x in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
    sum = sum + x
print(sum)
```

Python提供`range()`函数, 可以生成 `整数序列`, 再通过`list()`函数可以转换为`list`.
比如`range(5)`生成的序列是从`0`开始小于`5`的整数:

```python
>>> list(range(5))
[0, 1, 2, 3, 4]
```

`range(101)`就可以生成`0-100`的整数序列, 计算如下:

```python
sum = 0
for x in range(101):
    sum = sum + x
print(sum)
```

第二种循环是`while`循环, 只要条件满足, 就不断循环, 条件不满足时退出循环.
比如我们要计算`100`以内所有奇数之和, 可以用`while`循环实现:

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

在循环中, `break`语句可以提前退出循环. 例如, 本来要循环打印`1~100`的数字:

```python
n = 1
while n <= 100:
    print(n)
    n = n + 1
print('END')
```

上面的代码可以打印出 `1~100`.

如果要提前结束循环, 可以用`break`语句:

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

#### 小结

循环是让计算机做重复任务的有效的方法.

`break`语句可以在循环过程中直接退出循环, 而`continue`语句可以提前结束本轮循环, 并直接开始下一轮循环.
这两个语句通常都必须配合`if`语句使用.

要特别注意, 不要滥用`break`和`continue`语句. `break`和`continue`会造成代码执行逻辑分叉过多, 容易出错.
大多数循环并不需要用到`break`和`continue`语句, 上面的两个例子, 都可以通过改写循环条件或者修改循环逻辑, 去掉`break`和`continue`语句.

有些时候, 如果代码写得有问题, 会让程序陷入"死循环", 也就是永远循环下去.
这时可以用`Ctrl+C`退出程序, 或者强制结束 `Python` 进程.
