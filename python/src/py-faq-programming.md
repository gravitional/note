# python 常见问题--编程

[编程常见问题](https://docs.python.org/zh-cn/3/faq/programming.html?highlight=split#sequences-tuples-lists)

### 函数形参列表中的 `斜杠` ( `/`)是什么意思?

函数参数列表中的斜杠表示在它之前的 `形参` 全都仅限 `位置形参`.
仅限位置形参没有可供外部使用的名称. 在调用仅接受位置形参的函数时, 实参只会根据位置映射到形参上. 假定 divmod() 是一个仅接受位置形参的函数.  它的帮助文档如下所示:
>>>

>>> help(divmod)
Help on built-in function divmod in module builtins:

divmod(x, y, /)
    Return the tuple (x//y, x%y).  Invariant: div*y + mod == x.

形参列表尾部的斜杠说明, 两个形参都是仅限位置形参. 因此, 用关键字参数调用 divmod() 将会引发错误:
>>>

>>> divmod(x=3, y=4)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: divmod() takes no keyword arguments

### 为什么 -22 // 10 会返回 -3 ?

这主要是为了让 `i % j` 的正负与 `j` 一致, 如果期望如此, 且期望如下等式成立:

```python
i == (i // j) * j + (i % j)
```

那么整除就必须返回向下取整的结果.
C 语言同样要求保持这种一致性, 于是编译器在截断 `i // j` 的结果时需要让 `i % j` 的正负与 `i` 一致.

对于 `i % j` 来说 `j` 为负值的应用场景实际上是非常少的.
而 `j` 为正值的情况则非常多, 并且实际上在所有情况下让 `i % j` 的结果为 `>= 0` 会更有用处.
如果现在时间为 `10` 时, 那么 `200` 小时前应是几时?  `-190 % 12 == 2` 是有用处的;
`-190 % 12 == -10` 则是会导致意外的漏洞.

### 如何获得 int 字面值的属性而不是SyntaxError?

试图以正常方式查询 `int` literal属性, 会出现语法错误, 因为句号被看作是小数点.

```python
>>> 1.__class__
  File "<stdin>", line 1
  1.__class__
   ^
SyntaxError: invalid decimal literal
```

解决办法是用空格或括号将 字面值 与 `句号` 分开:

```python
1 .__class__
<class 'int'>

(1).__class__
<class 'int'>
```

### 如何将字符串转换为数字?

对于整数, 可使用内置的 `int()` 类型构造器, 例如 `int('144') == 144`.
类似地, 可使用 `float()` 转换为浮点数, 例如 `float('144') == 144.0`.

默认情况下, 这些操作会将数字按十进制来解读,
因此 `int('0144') == 144` 为真值, 而 `int('0x144')` 会引发 `ValueError`.
`int(string, base)` 接受第二个可选参数指定转换的基数, 例如 `int( '0x144', 16) == 324`.
如果指定基数为 `0`, 则按 `Python` 规则解读数字: 前缀 `0o` 表示八进制, 而 `0x` 表示十六进制.

如果只是想把 `字符串` 转为 `数字`, 请不要使用内置函数 `eval()`.
`eval()` 的速度慢很多且存在安全风险: 别人可能会传入带有不良副作用的 `Python` 表达式.
比如可能会传入 `__import__('os').system("rm -rf $HOME")`, 这会把 `home` 目录给删了.

`eval()` 还有把数字解析为 `Python` 表达式的后果, 因此如 `eval('09')` 将会导致语法错误,
因为 `Python` 不允许十进制数带有前导 `0`(`0` 除外).

### 如何将数字转换为字符串?

比如要把数字 `144` 转换为字符串 `'144'`, 可使用内置类型构造器 `str()`.
如果要表示为 `十六进制` 或 `八进制` 数格式, 可使用内置函数 `hex()` 或 `oct()`.

更复杂的格式化方法请参阅 [格式字符串字面值][] 和 [格式字符串语法] 等章节,
比如 `"{:04d}".format(144)` 会生成 `'0144'` ,  `"{:.3f}".format(1.0/3.0)` 则会生成 `'0.333'`.

[格式字符串字面值]: https://docs.python.org/zh-cn/3/reference/lexical_analysis.html#f-strings
[格式字符串语法]: https://docs.python.org/zh-cn/3/library/string.html#formatstrings

### 是否有与Perl 的chomp() 等效的方法, 用于从字符串中删除尾随换行符?

可以使用 `S.rstrip("\r\n")` 从字符串 `S` 的末尾删除所有的换行符, 而不删除其他尾随空格.
如果字符串 `S` 表示多行, 且末尾有几个空行, 则将删除所有空行的换行符:

```python
>>> lines = ("line 1 \r\n"
...          "\r\n"
...          "\r\n")
>>> lines.rstrip("\n\r")
'line 1 '
```

由于通常只在一次读取一行文本时才需要这样做, 所以使用 `S.rstrip()` 这种方式工作得很好.

### 是否有 scanf() 或 sscanf() 的等价函数?

没有.

如果要对简单的输入进行解析, 最容易的做法通常是,
利用字符串对象的 `split()` 方法将一行按 `空白符` 分隔拆分为多个单词, 然后用 `int()` 或 `float()` 将 `十进制数字符串` 转换为 `数字值`.
`split()` 支持可选的 `sep` 形参, 适用于分隔符不用 `空白符` 的情况.

如果要对更复杂的输入进行解析, 那么 `正则表达式` 要比 `C` 语言的 `sscanf()` 更强大, 也更合适.

### 将多个字符串连接在一起的最有效方法是什么?

`str` 和 `bytes` 对象是不可变的, 因此连接多个字符串的效率会很低, 因为每次连接都会创建一个新的对象. 
一般情况下, 总耗时与字符串总长是二次方的关系.

如果要连接多个 `str` 对象, 通常推荐的方案是先全部放入 `列表`, 最后再调用 `str.join()` :

```python
chunks = []
for s in my_strings:
    chunks.append(s)
result = ''.join(chunks)
```

(还有一种合理高效的习惯做法, 就是利用 [io.StringIO][] )

如果要连接多个 `bytes` 对象, 推荐做法是用 `bytearray` 对象的原地连接操作( `+=` 运算符)追加数据:

```python
result = bytearray()
for b in my_bytes_objects:
    result += b
```

[io.StringIO]: https://docs.python.org/zh-cn/3/library/io.html#io.StringIO
