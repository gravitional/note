# python-lang-ref

## python 数据模型

`对象` 是 Python 中对数据的抽象.
Python 程序中的所有数据都是由对象或对象间关系来表示的.  (从某种意义上说, 按照冯·诺依曼的"存储程序计算机"模型, 代码本身也是由对象来表示的. )

`obj`(对象)具有`class`(类型), (类型本身也是对象).
`obj`具有`attribute`(属性)(`property`和`method`).
`attribute`本身也是`obj`. `attribute`又会有自己的`property`和`method`.

每个对象都有各自的`编号`, `类型`和`值`. 一个对象被创建后, 它的 编号 就绝不会改变;
你可以将其理解为该对象在内存中的地址.  `is` 运算符可以比较两个对象的编号是否相同;
`id()` 函数能返回一个代表其编号的整型数.

CPython implementation detail: 在 CPython 中, `id(x)` 就是存放 `x` 的内存的地址.

对象的`类型`决定该对象所支持的操作 (例如 "对象是否有长度属性? ") 并且定义了该类型的对象可能的取值.
`type()` 函数能返回一个对象的类型 (类型本身也是对象). 与编号一样, 一个对象的 `类型` 也是不可改变的.

有些对象的 `值` 可以改变. 值可以改变的对象被称为 可变的; 值不可以改变的对象就被称为 不可变的.
一个对象的可变性是由其类型决定的; 例如, 数字, 字符串和元组是不可变的, 而字典和列表是可变的.

(但是元组的元素可以是数组, 数组可以改变. 所以, 不可变并不严格等同于值不能改变, 只是映射关系不变的意思. )

对象绝不会被显式地销毁; 然而, 当无法访问时它们可能会被作为垃圾回收. 允许具体的实现推迟垃圾回收或完全省略此机制 --- 如何实现垃圾回收是实现的质量问题, 只要可访问的对象不会被回收即可.

python 中特殊的方法名为 `__xx__` 的形式

### 属性查询优先级

`obj.attr`

+ ` __getattribute__()`,  无条件调用
+ `数据描述符`: (`obj.attr`具有 `__set__` and `__get__` 方法, `obj.attr`对应的类被称描述类, `obj.attr`这样的`instance`被称为`Descriptors`描述器)
+ `实例对象的字典 obj.__dict__`(若与描述符对象同名, 会被覆盖)
+ `类的字典`
+ `非数据描述符` (`obj.attr`中只有`__get__`方法的类, 此对象仍然是描述器. )
+ `父类的字典`
+ ` __getattr__()` 方法

## python 词法分析

[词法分析¶][]

[词法分析¶]: https://docs.python.org/zh-cn/3/reference/lexical_analysis.html

### 逻辑行

Python 程序由一个 `解析器` 读取.
输入到解析器的是一个由 `词法分析器` 所生成的 `形符` (`tokens`)流, 本章将描述词法分析器是如何将一个文件拆分为一个个形符的.

一个 Python 程序可分为许多 `逻辑行`.

`逻辑行`的结束是以 `NEWLINE` 形符表示的.
语句不能跨越逻辑行的边界, 除非其语法允许包含 `NEWLINE` (例如复合语句可由多行子语句组成).
一个逻辑行可由一个或多个 `物理行` 按照明确或隐含的 行拼接 规则构成.

`物理行`是以一个`行终止序列`结束的字符序列.
在源文件和字符串中, 可以使用任何标准平台上的`行终止序列` - Unix 所用的 ASCII 字符 `LF` (换行), Windows 所用的 ASCII 字符序列 `CR LF` (回车加换行), 或者旧 Macintosh 所用的 ASCII 字符 `CR` (回车).
所有这些形式均可使用, 无论具体平台. 输入的结束也会被作为最后一个物理行的隐含终止标志.

当嵌入 Python 时, 源码字符串传入 Python API 应使用标准 C 的传统换行符 (即 `\n`, 表示 ASCII 字符 `LF` 作为行终止标志).

### 注释

一条注释以不包含在字符串字面值内的井号 (`#`) 开头, 并在物理行的末尾结束.  一条注释标志着逻辑行的结束, 除非存在隐含的行拼接规则.  注释在语法分析中会被忽略.

编码声明

如果一条注释位于 Python 脚本的第一或第二行, 并且匹配正则表达式 `coding[=:]\s*([-\w.]+)`, 这条注释会被作为编码声明来处理;

上述表达式的第一组指定了源码文件的编码. 编码声明必须独占一行.
如果它是在第二行, 则第一行也必须是注释. 推荐的编码声明形式如下

```python
# -*- coding: <encoding-name> -*-
```

这也是 GNU Emacs 认可的形式, 以及

```vim
# vim:fileencoding=<encoding-name>
```

这是 Bram Moolenaar 的 `VIM` 认可的形式.

如果没有编码声明, 则默认编码为 UTF-8. 此外, 如果文件的首字节为 UTF-8 字节顺序标志 (`b'\xef\xbb\xbf'`), 文件编码也声明为 `UTF-8` (这是 Microsoft 的 notepad 等软件支持的形式).

编码声明指定的编码名称必须是 Python 所认可的编码. 所有词法分析将使用此编码, 包括语义字符串, 注释和标识符.

### 行拼接

显式的行拼接

两个或更多个物理行可使用反斜杠字符 (`\`) 拼接为一个逻辑行, 规则如下: 当一个物理行以一个不在字符串或注释内的反斜杠结尾时, 它将与下一行拼接构成一个单独的逻辑行, 反斜杠及其后的换行符会被删除. 例如:

```python
if 1900 < year < 2100 and 1 <= month <= 12 \
   and 1 <= day <= 31 and 0 <= hour < 24 \
   and 0 <= minute < 60 and 0 <= second < 60:   # Looks like a valid date
        return 1
```

+ 以反斜杠结束的行不能带有注释.
+ 反斜杠不能用来拼接注释.
+ 反斜杠不能用来拼接`tokens`, 字符串除外 (即`literal string`以外的`tokens`不能用反斜杠分隔到两个物理行).
+ 不允许有`literal string`以外的反斜杠存在于物理行的其他位置(出现在用来拼接的`\`的前面).

***
隐式的行拼接

圆括号, 方括号或花括号以内的表达式允许分成多个物理行, 无需使用反斜杠. 总之, 括号以内可以断行.
例如:

```python
month_names = ['Januari', 'Februari', 'Maart',      # These are the
               'April',   'Mei',      'Juni',       # Dutch names
               'Juli',    'Augustus', 'September',  # for the months
               'Oktober', 'November', 'December']   # of the year
```

隐式的行拼接可以带有注释. 后续行的缩进不影响程序结构. 后续行也允许为空白行.
隐式拼接的行之间不会有 `NEWLINE` 形符.
隐式拼接的行也可以出现于`三引号`字符串中 (见下); 此情况下这些行不允许带有注释.

### 空白行

一个只包含`空格符`, `制表符`, `进纸符`或者`注释`的逻辑行会被忽略 (即不生成 `NEWLINE` 形符).
在交互模式输入语句时, 对空白行的处理可能会因读取-求值-打印循环的具体实现方式而存在差异.
在标准交互模式解释器中, 一个完全空白的逻辑行 (即连空格或注释都没有) 将会结束一条多行复合语句.

### 缩进

一个逻辑行开头处的空白 (空格符和制表符) 被用来计算该行的缩进等级, 以决定语句段落的组织结构.

制表符会被 (从左至右) 替换为一至八个空格, 这样缩进的空格总数为八的倍数 (这是为了与 Unix 所用的规则一致).
首个非空白字符之前的空格总数将确定该行的缩进层次.
一个缩进不可使用反斜杠进行多行拼接; 首个反斜杠之前的空格将确定缩进层次.

在一个源文件中如果混合使用制表符和空格符缩进, 并使得确定缩进层次需要依赖于制表符对应的空格数量设置, 则被视为不合规则;
此情况将会引发 `TabError`.

跨平台兼容性注释: 由于非 UNIX 平台上文本编辑器本身的特性, 在一个源文件中混合使用制表符和空格符是不明智的.
另外也要注意不同平台还可能会显式地限制最大缩进层级.

行首有时可能会有一个进纸符; 它在上述缩进层级计算中会被忽略.
处于行首空格内其他位置的进纸符的效果未定义 (例如它可能导致空格计数重置为零).

多个连续行各自的缩进层级将会被放入一个堆栈用来生成 `INDENT` 和 `DEDENT` 形符, 具体说明如下(省略)

以下示例显示了各种缩进错误:

```python
 def perm(l):                       # error: first line indented
for i in range(len(l)):             # error: not indented
    s = l[:i] + l[i+1:]
        p = perm(l[:i] + l[i+1:])   # error: unexpected indent
        for x in p:
                r.append(l[i:i+1] + x)
            return r                # error: inconsistent dedent
```

(实际上, 前三个错误会被解析器发现; 只有最后一个错误是由词法分析器发现的 --- `return r `的缩进无法匹配弹出栈的缩进层级. )

### 形符之间的空白

除非是在逻辑行的开头或字符串内, 空格符, 制表符和进纸符等空白符都同样可以用来分隔形符.
如果两个形符彼此相连会被解析为一个不同的形符, 则需要使用空白来分隔 (例如 `ab` 是一个形符, 而 `a b` 是两个形符).

### 其他形符

除了 `NEWLINE`, `INDENT` 和 `DEDENT`, 还存在以下类别的形符: `标识符`, `关键字`, `字面值`, `运算符` 以及 `分隔符`.

空白字符 (之前讨论过的行终止符除外) 不属于形符, 而是用来分隔形符.
如果存在二义性, 将从左至右读取尽可能长的合法字符串组成一个形符.

### 标识符和关键字

标识符 (或者叫做 `名称`) 由以下词法定义进行描述.

Python 中的标识符语法是基于 Unicode 标准附件 `UAX-31`, 并加入了下文所定义的细化与修改; 更多细节还可参见 PEP 3131 .

在 ASCII 范围内 (`U+0001..U+007F`), 可用于标识符的字符与 Python 2.x 一致: 大写和小写字母 `A` 至 `Z`, 下划线 `_` 以及数字 `0` 至 `9`, 但不可以数字打头.

Python 3.0 引入了 ASCII 范围以外的额外字符 (见 PEP 3131). 这些字符的分类使用包含于 `unicodedata` 模块中的 Unicode 字符数据库版本.

标识符的长度没有限制. 对大小写敏感.

所有标识符在解析时会被转换为规范形式 NFKC; 标识符的比较都是基于 NFKC.

Unicode 4.1 中所有可用的标识符字符列表可参见以下[非正式 HTML 文件][]

[非正式 HTML 文件]:  https://www.unicode.org/Public/13.0.0/ucd/DerivedCoreProperties.txt

### 关键字

以下标识符被作为语言的保留字或称 关键字, 不可被用作普通标识符. 关键字的拼写必须与这里列出的完全一致.

+ `False`   `await` `else`  `import`    `pass`
+ `None`    `break` `except`    `in`    `raise`
+ `True`    `class` `finally`   `is`    `return`
+ `and` `continue`  `for`   `lambda`    `try`
+ `as`  `def`   `from`  `nonlocal`  `while`
+ `assert`  `del`   `global`    `not`   `with`
+ `async`   `elif`  `if`    `or`    `yield`

### 保留的标识符类

某些标识符类 (除了关键字) 具有特殊的含义. 这些标识符类的命名模式是以下划线字符打头和结尾:

+ `_*`

不会被 `from module import *` 导入. 特殊标识符 `_` 在交互式解释器中被用来存放最近一次求值结果;
它保存在 `builtins` 模块中. 当不处于交互模式时, `_` 无特殊含义也没有预定义. 参见 `import` 语句.

注解: `_` 作为名称常用于连接国际化文本; 请参看 gettext 模块文档了解有关此约定的详情.

+ `__*__`

系统定义的名称, 在非正式场合下被叫做 "dunder" 名称.  这些名称是由解释器及其实现(包括标准库)定义的.  现有系统定义名称相关的讨论请参见 特殊方法名称 等章节.  未来的 Python 版本中还将定义更多此类名称.  任何情况下 任何 不遵循文档所显式指明的` __*__ `名称使用方式都可能导致无警告的错误.

+ `__*`

类的私有名称. 这种名称在类定义中使用时, 会以一种混合形式重写以避免在基类及派生类的 "私有" 属性之间出现名称冲突.
参见 标识符(名称).

### 字面值(literal:原义的)

字面值用于表示一些内置类型的常量.

***
字符串和字节串字面值

自然语言描述: 两种字面值都可以用成对单引号 (`''`) 或双引号 (`""`) 来标示首尾.
它们也可以用成对的`连续三个单引号或双引号`来标示首尾 (这通常被称为 `三引号字符串`).
反斜杠 (`\`) 字符被用来对特殊含义的字符进行转义, 例如换行, 反斜杠本身或是引号等字符.

`字节串`字面值总是带有前缀 `'b'` 或 `'B'`; 它们生成 `bytes` 类型而非 `str` 类型的实例.
它们只能包含 `ASCII` 字符; 字节对应数值在`128`及以上必须以转义形式来表示.

字符串和字节串字面值都可以带有前缀 `'r'` 或 `'R'`;
这种字符串被称为`原始字符串`其中的反斜杠会被当作其本身的字面字符来处理.
因此在原始字符串字面值中, `'\U'` 和 `'\u'` 转义形式不会被特殊对待.
由于 Python 2.x 的原始统一码字面值的特性与 Python 3.x 不一致, `'ur'` 语法已不再被支持.

包含 `'f'` 或 `'F'` 前缀的字符串字面值称为 格式化字符串字面值; 参见 格式化字符串字面值.
`'f'` 可与 `'r'` 连用, 但不能与 `'b'` 或 `'u'` 连用, 因此存在原始格式化字符串, 但不存在格式化字节串字面值.

在三引号字面值中, 允许存在未经转义的换行和引号 (并原样保留), 要输入三引号本身, 需要转义.

除非带有 `'r'` 或 `'R'` 前缀, 字符串和字节串字面值中的转义序列会基于类似标准 `C` 中的转义规则来解读.
可用的转义序列如下:

### 字符串字面值拼接

多个相邻的字符串或字节串字面值 (以空白符分隔), 所用的引号可以彼此不同, 其含义等同于全部拼接为一体.
因此,  `"hello" 'world'` 等同于 `"helloworld"`.
此特性可以减少反斜杠的使用, 以方便地将很长的字符串分成多个物理行, 甚至每部分字符串还可分别加注释, 例如:

```python
re.compile("[A-Za-z_]"       # letter or underscore
           "[A-Za-z0-9_]*"   # letter, digit or underscore
          )
```

注意此特性是在句法层面定义的, 但是在编译时实现. 在运行时拼接字符串表达式必须使用 `+` 运算符. (应该就是有表达式的话, 必须用`+`号)

还要注意字面值拼接时每个部分可以使用不同的引号风格 (甚至混合使用原始字符串和三引号字符串), 格式化字符串字面值也可与普通字符串字面值拼接.

### 格式化字符串字面值

3.6 新版功能.

格式化字符串字面值 或称 `f-string` 是带有 `'f'` 或 `'F'` 前缀的字符串字面值.
这种字符串可包含替换字段, 即以 `{}` 标示的表达式. 而其他字符串字面值总是一个常量,
>格式化字符串字面值实际上是会在运行时被求值的表达式.

转义序列会像在普通字符串字面值中一样被解码 (除非字面值还被标示为原始字符串). 解码之后, 字符串内容所用的语法如下:

```python
f_string          ::=  (literal_char | "{{" | "}}" | replacement_field)*
replacement_field ::=  "{" f_expression ["="] ["!" conversion] [":" format_spec] "}"
f_expression      ::=  (conditional_expression | "*" or_expr)
                         ("," conditional_expression | "," "*" or_expr)* [","]
                       | yield_expression
conversion        ::=  "s" | "r" | "a"
format_spec       ::=  (literal_char | NULL | replacement_field)*
literal_char      ::=  <any code point except "{", "}" or NULL>
```

字符串在花括号以外的部分按其字面值处理, 除了双重花括号 `'{{'` 或 `'}}'` 会被替换为相应的单个花括号.
单个左花括号 `{` 标示一个替换字段, 它以一个 Python 打头.

+ 要显示如`x=  'value of x'` 这样的形式, 可以在表达式后加一个等于号 `=`.
+ 之后可能带有一个以叹号 `!` 标示的转换字段.
+ 之后还可能带有一个以冒号 `:` 标示的格式说明符.
+ 替换字段以一个右花括号 `}` 作为结束.

格式化字符串字面值中的表达式会被当作包含在圆括号中的普通 Python 表达式一样处理, 但有少数例外.

`空表达式`不被允许, `lambda` 和赋值表达式 `:=` 必须显式地加上圆括号.
替换表达式可以包含换行(例如在三重引号字符串中), 但是不能包含注释.
每个表达式会在格式化字符串字面值所包含的位置按照从左至右的顺序被求值.

在提供了等于号 `=` 的时候, 输出将包含表达式文本, `=` 以及求值结果.
左花括号 `{` 之后包含在表达式中及 `=` 后的空格将在输出时被保留.
默认情况下, `=` 会导致表达式的 `repr()` 被使用, 除非专门指定了格式.
当指定了格式时默认会使用表达式的 `str()`, 除非声明了转换字段 `!r`.

3.8 新版功能: 等于号 `=` 在 Python 3.8 中加入.

如果指定了转换符, 表达式的求值结果会先转换再格式化.
转换符 `!s` 即对结果调用 `str()`, `!r` 为调用 `repr()`, 而 `!a` 为调用 `ascii()`.

在此之后结果会使用 `format()` 协议进行格式化. 格式说明符会被传入表达式或转换结果的 `__format__()` 方法.
如果省略格式说明符则会传入一个空字符串. 然后格式化结果会包含在整个字符串最终的值当中.

一些格式化字符串字面值的示例:

```python
>>> name = "Fred"
>>> f"He said his name is {name!r}."
"He said his name is 'Fred'."
>>> f"He said his name is {repr(name)}."  # repr() is equivalent to !r
"He said his name is 'Fred'."
>>> width = 10
>>> precision = 4
>>> value = decimal.Decimal("12.34567")
>>> f"result: {value:{width}.{precision}}"  # nested fields
'result:      12.35'
>>> today = datetime(year=2017, month=1, day=27)
>>> f"{today:%B %d, %Y}"  # using date format specifier
'January 27, 2017'
>>> f"{today=:%B %d, %Y}" # using date format specifier and debugging
'today=January 27, 2017'
>>> number = 1024
>>> f"{number:#0x}"  # using integer format specifier
'0x400'
>>> foo = "bar"
>>> f"{ foo = }" # preserves whitespace
" foo = 'bar'"
>>> line = "The mill's closed"
>>> f"{line = }"
'line = "The mill\'s closed"'
>>> f"{line = :20}"
"line = The mill's closed   "
>>> f"{line = !r:20}"
'line = "The mill\'s closed" '
```

要区分外层引号和内层引号

f"abc {a["x"]} def"    # error: outer string literal ended prematurely
f"abc {a['x']} def"    # workaround: use different quoting

格式表达式中不允许有反斜杠, 这会引发错误:

```python
f"newline: {ord('\n')}"  # raises SyntaxError
```

想包含需要用反斜杠转义的值, 可以创建一个临时变量.

```python
newline = ord('\n')

f"newline: {newline}"
'newline: 10'
```

格式化字符串字面值不可用作文档字符串, 即便其中没有包含表达式.

```python
>>> def foo():
...     f"Not a docstring"
...
>>> foo.__doc__ is None
True
```

另请参见 PEP 498 了解加入格式化字符串字面值的提议, 以及使用了相关的格式字符串机制的 `str.format()`.

### 数字字面值

数字字面值有三种类型: 整型数, 浮点数和虚数. 没有专门的复数字面值 (复数可由一个实数加一个虚数合成).

注意数字字面值并不包含正负号; `-1` 这样的负数实际上是由单目运算符 `'-'` 和字面值 `1` 合成的.

### 整型数字面值

整型数字面值由以下词法定义进行描述:

```python
integer      ::=  decinteger | bininteger | octinteger | hexinteger
decinteger   ::=  nonzerodigit (["_"] digit)* | "0"+ (["_"] "0")*
bininteger   ::=  "0" ("b" | "B") (["_"] bindigit)+
octinteger   ::=  "0" ("o" | "O") (["_"] octdigit)+
hexinteger   ::=  "0" ("x" | "X") (["_"] hexdigit)+
nonzerodigit ::=  "1"..."9"
digit        ::=  "0"..."9"
bindigit     ::=  "0" | "1"
octdigit     ::=  "0"..."7"
hexdigit     ::=  digit | "a"..."f" | "A"..."F"
```

整型数字面值的长度没有限制, 能一直大到占满可用内存.

在确定数字大小时字面值中的下划线会被忽略. 它们可用来将数码分组以提高可读性.
一个下划线可放在数码之间, 也可放在基数说明符例如 `0x` 之后.

注意非零的十进制数开头不允许有额外的零. 这是为了避免与 Python 在版本 `3.0` 之前所使用的 `C` 风格八进制字面值相混淆.

一些整型数字面值的示例如下:

```python
7     2147483647                        0o177    0b100110111
3     79228162514264337593543950336     0o377    0xdeadbeef
      100_000_000_000                   0b_1110_0101
```

在 3.6 版更改: 允许在字面值中使用下划线进行分组.

### 浮点数字面值

浮点数字面值由以下词法定义进行描述:

```python
floatnumber   ::=  pointfloat | exponentfloat
pointfloat    ::=  [digitpart] fraction | digitpart "."
exponentfloat ::=  (digitpart | pointfloat) exponent
digitpart     ::=  digit (["_"] digit)*
fraction      ::=  "." digitpart
exponent      ::=  ("e" | "E") ["+" | "-"] digitpart
```

注意整型数部分和指数部分在解析时总是以 10 为基数. 例如, 077e010 是合法的, 且表示的数值与 77e10 相同. 浮点数字面值允许的范围依赖于具体实现. 对于整型数字面值, 支持以下划线进行分组.

一些浮点数字面值的示例如下:

```python
3.14    10.    .001    1e100    3.14e-10    0e0    3.14_15_93
```

在 3.6 版更改: 允许在字面值中使用下划线进行分组.

### 虚数字面值

虚数字面值由以下词法定义进行描述:

```python
imagnumber ::=  (floatnumber | digitpart) ("j" | "J")
```

一个虚数字面值将生成一个实部为 `0.0` 的复数. 复数是以一对浮点数来表示的, 它们的取值范围相同.
要创建一个实部不为零的复数, 就加上一个浮点数, 例如 (`3+4j`). 一些虚数字面值的示例如下:

```python
3.14j   10.j    10j     .001j   1e100j   3.14e-10j   3.14_15_93j
```

### 运算符

以下形符属于运算符:

```python
+       -       *       **      /       //      %      @
<<      >>      &       |       ^       ~       :=
<       >       <=      >=      ==      !=
```

### 分隔符

以下形符在语法中归类为分隔符:

```python
(       )       [       ]       {       }
,       :       .       ;       @       =       ->
+=      -=      *=      /=      //=     %=      @=
&=      |=      ^=      >>=     <<=     **=
```

句点也可出现于浮点数和虚数字面值中. 连续三个句点有表示一个省略符的特殊含义.
以上列表的后半部分为增强赋值操作符, 在词法中作为分隔符, 但也起到运算作用.

以下可打印 ASCII 字符作为其他形符的组成部分时**具有**特殊含义, 或是对词法分析器有重要意义:

```python
'       "       #       \
```

以下可打印 ASCII 字符**不在** Python 词法中使用. 如果出现于字符串字面值和注释之外将无条件地引发错误:

```python
$       ?       `
```

## 内置函数

[内置函数](https://docs.python.org/zh-cn/3/library/functions.html)

### 类型转换

str()
int()
float()
bool()
chr()
bin()
oct()
hex()
class float([x])    返回从数字或字符串 x 生成的浮点数.

### 编码内存

ord()
ascii()

### 数学运算

abs()
min()
max()
round()
pow()
sum()
divmod(a, b)    它将两个(非复数)数字作为实参, 并在执行整数除法时返回一对商和余数

### 文件io

input([prompt])    如果存在 prompt 实参, 则将其写入标准输出, 末尾不带换行符. 接下来, 该函数从输入中读取一行, 将其转换为字符串(除了末尾的换行符)并返回. 当读取到 EOF 时, 则触发 EOFError

open(file, mode='r', buffering=-1, encoding=None, errors=None, newline=None, closefd=True, opener=None)

打开 file 并返回对应的 file object.  如果该文件不能被打开, 则引发 OSError.  请参阅 读写文件 获取此函数的更多用法示例.

print(*objects, sep=' ', end='\n', file=sys.stdout, flush=False)

将 objects 打印到 file 指定的文本流, 以 sep 分隔并在末尾加上 end.  sep, end, file 和 flush 如果存在, 它们必须以关键字参数的形式给出.

format(value[, format_spec])

将 value 转换为 format_spec 控制的"格式化"表示. format_spec 的解释取决于 value 实参的类型, 但是大多数内置类型使用标准格式化语法: 格式规格迷你语言.

repr()

### 序列/集合

class set([iterable])    返回一个新的 set 对象, 可以选择带有从 iterable 获取的元素.  set 是一个内置类型.
complex()
list()
tuple()
dict()
frozenset()

class range(stop)
class range(start, stop[, step])

虽然被称为函数, 但 range 实际上是一个不可变的序列类型, 参见在 range 对象 与 序列类型 --- list, tuple, range 中的文档说明.

***
序列操作

any(iterable)    如果 iterable 的任一元素为真值则返回 True.  如果可迭代对象为空, 返回 False.
all(iterable)   如果 iterable 的所有元素均为真值(或可迭代对象为空)则返回 True .
next()
slice()
sorted()
enumerate(iterable, start=0)    返回一个枚举对象. iterable 必须是一个序列, 或 iterator, 或其他支持迭代的对象.
`enumerate()` 返回的迭代器的 `__next__()` 方法返回一个元组, 里面包含一个计数值(从 start 开始, 默认为 0)和通过迭代 iterable 获得的值.

    class bytearray([source[, encoding[, errors]]])

返回新的 `bytes` 数组.  bytearray 类是一个可变序列, 包含范围为 0 <= x < 256 的整数

    len(s)

返回对象的长度(元素个数).
实参可以是序列(如 string, bytes, tuple, list 或 range 等)或集合(如 dictionary, set 或 frozen set 等)

### 迭代器

filter(function, iterable)

用 iterable 中函数 function 返回真的那些元素, 构建新的迭代器.
iterable 可以是序列, 支持迭代的容器, 或迭代器.
如果 `function` 是 `None` , 则会假设它是一个身份函数, 即 iterable 中所有返回假的元素会被移除

    class bytes([source[, encoding[, errors]]])

返回新的 `bytes` 对象,  是不可变序列, 包含范围为 `0 <= x < 256` 的整数.
`bytes` 是 `bytearray` 的不可变版本

    iter(object[, sentinel])

返回一个 iterator 对象. 根据是否存在第二个实参, 第一个实参的解释是非常不同的.

    zip()

`reversed(seq)` 返回反向的 `iterator`. `seq`
必须是一个具有 `__reversed__()` 方法的对象或者是支持该序列协议.
具有从 `0` 开始的整数类型参数的 `__len__()` 方法和 `__getitem__()` 方法.

### 面向对象

dir()

id(object)    返回对象的"标识值". 该值是一个整数, 在此对象的生命周期中保证是唯一且恒定的. 两个生命期不重叠的对象可能具有相同的 id() 值.
CPython implementation detail: This is the address of the object in memory.

class object     返回一个没有特征的新对象. object 是所有类的基类. 它具有所有 Python 类实例的通用方法. 这个函数不接受任何实参.

setattr(object, name, value)    此函数与 getattr() 两相对应.  其参数为一个对象, 一个字符串和一个任意值.  字符串指定一个现有属性或者新增属性

delattr(object, name)    setattr() 相关的函数. 实参是一个对象和一个字符串. 该字符串必须是对象的某个属性. 如果对象允许, 该函数将删除指定的属性. 例如 delattr(x, 'foobar') 等价于 del x.foobar .

hash(object)    返回该对象的哈希值(如果它有的话). 哈希值是整数. 它们在字典查找元素时用来快速比较字典的键. 相同大小的数字变量有相同的哈希值(即使它们类型不同, 如 1 和 1.0).

issubclass(class, classinfo)
如果 class 是 classinfo 的 (直接, 间接或 虚拟) 子类则返回 True.  类会被视作其自身的子类.

isinstance()

class memoryview(obj)返回由给定实参创建的"内存视图"对象

super([type[, object-or-type]])

返回一个代理对象, 它会将方法调用委托给 type 的父类或兄弟类.

callable(object)

如果参数 object 是可调用的就返回 True, 否则返回 False.  如果返回 True, 调用仍可能失败, 但如果返回 False, 则调用 object 将肯定不会成功.

class property(fget=None, fset=None, fdel=None, doc=None)

返回 property 属性.

class type(object)
class type(name, bases, dict)

传入一个参数时, 返回 object 的类型.  返回值是一个 type 对象, 通常与 `object.__class__` 所返回的对象相同.

@classmethod

把一个方法封装成类方法.
 一个类方法把类自己作为第一个实参, 就像一个实例方法把实例自己作为第一个实参. 请用以下习惯来声明类方法:

class C:
    @classmethod
    def f(cls, arg1, arg2, ...): ...

@staticmethod

    将方法转换为静态方法.

    静态方法不会接收隐式的第一个参数. 要声明一个静态方法, 请使用此语法

    class C:
        @staticmethod
        def f(arg1, arg2, ...): ...

    @staticmethod 这样的形式称为函数的 decorator -- 详情参阅 函数定义.

    静态方法的调用可以在类上进行 (例如 C.f()) 也可以在实例上进行 (例如 C().f()).

    Python中的静态方法与Java或C ++中的静态方法类似. 另请参阅 classmethod() , 用于创建备用类构造函数的变体.

    像所有装饰器一样, 也可以像常规函数一样调用 staticmethod , 并对其结果执行某些操作. 比如某些情况下需要从类主体引用函数并且您希望避免自动转换为实例方法. 对于这些情况, 请使用此语法:

    class C:
        builtin_open = staticmethod(open)

getattr(object, name[, default])

    返回对象命名属性的值. name 必须是字符串. 如果该字符串是对象的属性之一, 则返回该属性的值. 例如,  getattr(x, 'foobar') 等同于 x.foobar. 如果指定的属性不存在, 且提供了 default 值, 则返回它, 否则触发 AttributeError.

locals()

    更新并返回表示当前本地符号表的字典.  在函数代码块但不是类代码块中调用 locals() 时将返回自由变量.  请注意在模块层级上, locals() 和 globals() 是同一个字典.

globals()

返回表示当前全局符号表的字典. 这总是当前模块的字典(在函数或方法中, 不是调用它的模块, 而是定义它的模块).

hasattr(object, name)    该实参是一个对象和一个字符串. 如果字符串是对象的属性之一的名称, 则返回 True, 否则返回 False. (此功能是通过调用 getattr(object, name) 看是否有 AttributeError 异常来实现的. )

### 函数

@staticmethod   将方法转换为静态方法
eval()
exec()

vars([object])

返回模块, 类, 实例或任何其它具有 `__dict__` 属性的对象的 `__dict__` 属性.

map(function, iterable, ...)

返回一个将 function 应用于 iterable 中每一项并输出其结果的迭代器.  如果传入了额外的 iterable 参数, function 必须接受相同个数的实参并被应用于从所有可迭代对象中并行获取的项.  当有多个可迭代对象时, 最短的可迭代对象耗尽则整个迭代就将结束.  对于函数的输入已经是参数元组的情况, 请参阅 itertools.starmap().

### 调试

breakpoint(*args, **kws)    此函数会在调用时将你陷入调试器中

### 杂项

help([object])    启动内置的帮助系统(此函数主要在交互式中使用

compile(source, filename, mode, flags=0, dont_inherit=False, optimize=-1)

将 source 编译成代码或 AST 对象. 代码对象可以被 exec() 或 eval() 执行.
source 可以是常规的字符串, 字节字符串, 或者 AST 对象. 参见 ast 模块的文档了解如何使用 AST 对象.

    __import__(name, globals=None, locals=None, fromlist=(), level=0)

## 命名空间

在介绍类之前, 我首先要告诉你一些Python的作用域规则.
类定义对命名空间有一些巧妙的技巧, 你需要知道作用域和命名空间如何工作才能完全理解正在发生的事情.
顺便说一下, 关于这个主题的知识对任何高级Python程序员都很有用.

`namespace` (命名空间)是一个从名字到对象的映射.
大部分命名空间当前都由 Python 字典实现, 但一般情况下基本不会去关注它们(除了要面对性能问题时), 而且也有可能在将来更改.
下面是几个命名空间的例子:

+ 存放内置函数的集合(包含 `abs()` 这样的函数, 和内建的异常等);
+ 模块中的全局名称;
+ 函数调用中的局部名称.
+ 从某种意义上说, 对象的属性集合也是一种命名空间的形式.

关于命名空间的重要一点是, 不同命名空间中的名称之间绝对没有关系;
例如, 两个不同的模块都可以定义一个 maximize 函数而不会产生混淆 --- 模块的用户必须在其前面加上模块名称.

顺便说明一下, 我把任何跟在一个点号之后的名称都称为 `属性` --- 例如, 在表达式 `z.real` 中, `real` 是对象 `z` 的一个属性.

在不同时刻创建的命名空间拥有不同的生存期.

包含`内置名称`的`命名空间`是在 Python 解释器启动时创建的, 永远不会被删除.

一个`*.py`文件被称为一个`模块`, `模块`的`全局命名空间`在`模块`定义被读入时创建; 通常, 模块命名空间也会持续到解释器退出.
被解释器的顶层调用执行的语句, 从一个脚本文件读取或交互式地读取, 被认为是 `__main__ `模块调用的一部分, 因此它们拥有自己的全局命名空间. (`内置名称`实际上也存在于一个模块中; 这个模块称作 `builtins` . )

`global namespace` :

+ `builtins`
+ `module`(本`*.py`文件)

可以认为是 `module namespace`

一个函数的本地命名空间在这个函数被调用时创建, 并在函数返回或抛出一个不在函数内部处理的错误时被删除.
(事实上, 比起描述到底发生了什么, 忘掉它更好. )当然, 每次递归调用都会有它自己的本地命名空间.

`作用域`(`scope`): 在某一段程序中, 某个命名空间可以直接被访问. 就是变量名称的生效空间.

虽然作用域是静态地确定的, 但它们会被动态地使用.
在执行期间的任何时刻, 会有 3 或 4 个个命名空间可被直接访问的嵌套作用域:

+ the `innermost` scope, which is searched first, contains the `local` names
+ the scopes of any `enclosing` functions, which are searched starting with the nearest enclosing scope, contains `non-local`, but also `non-global` names
+ the next-to-last scope contains the current module's global names
+ the `outermost` scope (searched last) is the namespace containing `built-in` names

也就是

`local` : 本地名称
`non-local` : 包裹结构的名称, 但不包含`module`
`global` : `module`
`built-in` : 解释器从`bultin` 模块导入的命名空间

无论函数什么地方或以什么别名被调用, 作用域是按字面文本来确定的, `global`作用域指的就是`module`(本`*.py`文件).

Python 的一个特殊规定是这样的--如果不存在生效的 `global` 或 `nonlocal` 语句 -- 则对名称的赋值总是会进入最内层作用域.

赋值不会复制数据 --- 它们只是将名称绑定到对象.
删除也是如此: 语句 `del x` 会从局部作用域所引用的命名空间中移除对 `x` 的绑定.
事实上, 所有引入新名称的操作都是使用局部作用域: 特别地, `import `语句和函数定义会在局部作用域中绑定模块或函数名称.

`global` 语句可被用来表明特定变量生存于`global`作用域并且应当在其中被重新绑定;
`nonlocal` 语句表明特定变量生存于`outter`作用域中并且应当在其中被重新绑定.

### 作用域和命名空间示例

这个例子演示了如何引用不同作用域和名称空间, 以及 `global` 和 `nonlocal` 会如何影响变量绑定:

```python
def scope_test():
    def do_local():
        spam = "local spam"

    def do_nonlocal():
        nonlocal spam
        spam = "nonlocal spam"

    def do_global():
        global spam
        spam = "global spam"

    spam = "test spam"
    do_local()
    print("After local assignment:", spam)
    do_nonlocal()
    print("After nonlocal assignment:", spam)
    do_global()
    print("After global assignment:", spam)

scope_test()
print("In global scope:", spam)

### 示例代码的输出是:
After local assignment: test spam
After nonlocal assignment: nonlocal spam
After global assignment: nonlocal spam
In global scope: global spam
```

请注意 局部 赋值(这是默认状态)不会改变 `scope_test`(父函数)对 `spam` 的绑定.
`nonlocal` 赋值会改变 `scope_test` 对 `spam` 的绑定(在一个子函数中改变外层函数中的变量),
而 `global` 赋值会改变`module`中的绑定. (整个`*.py`文件中的定义)
