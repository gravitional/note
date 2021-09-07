# python-5.md

ref: [这是小白的Python新手教程][]

[这是小白的Python新手教程]: https://www.liaoxuefeng.com/wiki/1016959663602400

## 正则表达式

字符串是编程时涉及到的最多的一种数据结构, 对字符串进行操作的需求几乎无处不在. 比如判断一个字符串是否是合法的`Email`地址, 虽然可以编程提取`@`前后的子串, 再分别判断是否是单词和域名, 但这样做不但麻烦, 而且代码难以复用. 

正则表达式是一种用来匹配字符串的强有力的武器. 它的设计思想是用一种描述性的语言来给字符串定义一个规则, 凡是符合规则的字符串, 我们就认为它“匹配”了, 否则, 该字符串就是不合法的. 

所以我们判断一个字符串是否是合法的`Email`的方法是：

+ 创建一个匹配`Email`的正则表达式；
+ 用该正则表达式去匹配用户的输入来判断是否合法. 

因为正则表达式也是用字符串表示的, 所以, 我们要首先了解如何用字符来描述字符. 

在正则表达式中, 如果直接给出字符, 就是精确匹配. 用`\d`可以匹配一个数字, `\w`可以匹配一个字母或数字, 所以：

+ `'00\d'`可以匹配`'007'`, 但无法匹配`'00A'`；
+ `'\d\d\d'`可以匹配`'010'`；
+ `'\w\w\d'`可以匹配`'py3'`；

`.`可以匹配任意字符, 所以：

`'py.'`可以匹配`'pyc'`, `'pyo'`, `'py!'`等等. 

要匹配变长的字符, 在正则表达式中, 用`*`表示任意个字符(包括`0`个), 用`+`表示至少一个字符, 用`?`表示`0`个或`1`个字符, 用`{n}`表示`n`个字符, 用`{n,m}`表示`n-m`个字符：

来看一个复杂的例子：`\d{3}\s+\d{3,8`}. 
我们来从左到右解读一下：

+ `\d{3}`表示匹配3个数字, 例如`'010'`；
+ `\s`可以匹配一个空格(也包括`Tab`等空白符), 所以`\s+`表示至少有一个空格, 例如 `匹配`'  '`, `'    '`等；
+ `\d{3,8}`表示`3-8`个数字, 例如`'1234567'`. 

综合起来, 上面的正则表达式可以匹配以任意个空格隔开的带区号的电话号码. 

如果要匹配`'010-12345'`这样的号码呢？由于`'-'`是特殊字符, 在正则表达式中, 要用`'\'`转义, 所以, 上面的正则是`\d{3}\-\d{3,8}`. 

但是, 仍然无法匹配`'010 - 12345'`, 因为带有空格. 所以我们需要更复杂的匹配方式. 

### 进阶

要做更精确地匹配, 可以用`[]`表示范围, 比如：

+ `[0-9a-zA-Z\_]`可以匹配一个数字, 字母或者下划线；
+ `[0-9a-zA-Z\_]+`可以匹配至少由一个数字, 字母或者下划线组成的字符串, 比如`'a100'`, `'0_Z'`, `'Py3000'`等等；
+ `[a-zA-Z\_][0-9a-zA-Z\_]*`可以匹配由字母或下划线开头, 后接任意个由一个数字, 字母或者下划线组成的字符串, 也就是Python合法的变量；
+ `[a-zA-Z\_][0-9a-zA-Z\_]{0, 19}`更精确地限制了变量的长度是1-20个字符(前面1个字符+后面最多19个字符). 

+ `A|B`可以匹配`A`或`B`, 所以`(P|p)ython`可以匹配`'Python'`或者`'python'`. 
+ `^`表示行的开头, `^\d`表示必须以数字开头. 
+ `$`表示行的结束, `\d$`表示必须以数字结束. 

你可能注意到了, `py`也可以匹配`'python'`, 但是加上`^py$`就变成了整行匹配, 就只能匹配`'py'`了. 

### re模块

有了准备知识, 我们就可以在Python中使用正则表达式了. Python提供`re`模块, 包含所有正则表达式的功能. 由于Python的字符串本身也用`\`转义, 所以要特别注意：

```python
s = 'ABC\\-001' # Python的字符串
# 对应的正则表达式字符串变成：
# 'ABC\-001'
```

因此我们强烈建议使用Python的`r`前缀, 就不用考虑转义的问题了：

```python
s = r'ABC\-001' # Python的字符串
# 对应的正则表达式字符串不变：
# 'ABC\-001'
```

先看看如何判断正则表达式是否匹配：

```python
>>> import re
>>> re.match(r'^\d{3}\-\d{3,8}$', '010-12345')
<_sre.SRE_Match object; span=(0, 9), match='010-12345'>
>>> re.match(r'^\d{3}\-\d{3,8}$', '010 12345')
>>>
```

`match()`方法判断是否匹配, 如果匹配成功, 返回一个`Match`对象, 否则返回`None`. 
常见的判断方法就是：

```python
test = '用户输入的字符串'
if re.match(r'正则表达式', test):
    print('ok')
else:
    print('failed')
```

### 切分字符串

用正则表达式切分字符串比用固定的字符更灵活, 请看正常的切分代码：

```python
>>> 'a b   c'.split(' ')
['a', 'b', '', '', 'c']
```

嗯, 无法识别连续的空格, 用正则表达式试试：

```python
>>> re.split(r'\s+', 'a b   c')
['a', 'b', 'c']
```

无论多少个空格都可以正常分割. 加入,试试：

```python
>>> re.split(r'[\s\,]+', 'a,b, c  d')
['a', 'b', 'c', 'd']
```

再加入;试试：

```python
>>> re.split(r'[\s\,\;]+', 'a,b;; c  d')
['a', 'b', 'c', 'd']
```

如果用户输入了一组标签, 下次记得用正则表达式来把不规范的输入转化成正确的数组. 
分组

除了简单地判断是否匹配之外, 正则表达式还有提取子串的强大功能. 用()表示的就是要提取的分组(Group). 比如：

`^(\d{3})-(\d{3,8})$`分别定义了两个组, 可以直接从匹配的字符串中提取出区号和本地号码：

```python
>>> m = re.match(r'^(\d{3})-(\d{3,8})$', '010-12345')
>>> m
<_sre.SRE_Match object; span=(0, 9), match='010-12345'>
>>> m.group(0)
'010-12345'
>>> m.group(1)
'010'
>>> m.group(2)
'12345'
```

如果正则表达式中定义了组, 就可以在`Match`对象上用`group()`方法提取出子串来. 

注意到`group(0)`永远是原始字符串, `group(1)`, `group(2)`……表示第1, 2, ……个子串. 

提取子串非常有用. 来看一个更凶残的例子：

```python
>>> t = '19:05:30'
>>> m = re.match(r'^(0[0-9]|1[0-9]|2[0-3]|[0-9])\:(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]|[0-9])\:(0[0-9]|1[0-9]|2[0-9]|3[0-9]|4[0-9]|5[0-9]|[0-9])$', t)
>>> m.groups()
('19', '05', '30')
```

这个正则表达式可以直接识别合法的时间. 但是有些时候, 用正则表达式也无法做到完全验证, 比如识别日期：

'^(0[1-9]|1[0-2]|[0-9])-(0[1-9]|1[0-9]|2[0-9]|3[0-1]|[0-9])$'

对于`'2-30'`, `'4-31'`这样的非法日期, 用正则还是识别不了, 或者说写出来非常困难, 这时就需要程序配合识别了. 
贪婪匹配

最后需要特别指出的是, 正则匹配默认是贪婪匹配, 也就是匹配尽可能多的字符. 举例如下, 匹配出数字后面的0：

```python
>>> re.match(r'^(\d+)(0*)$', '102300').groups()
('102300', '')
```

由于`\d+`采用贪婪匹配, 直接把后面的`0`全部匹配了, 结果`0*`只能匹配空字符串了. 

必须让`\d+`采用非贪婪匹配(也就是尽可能少匹配), 才能把后面的`0`匹配出来, 加个`?`就可以让`\d+`采用非贪婪匹配：

```python
>>> re.match(r'^(\d+?)(0*)$', '102300').groups()
('1023', '00')
```

### 编译

当我们在Python中使用正则表达式时, `re`模块内部会干两件事情：

+ 编译正则表达式, 如果正则表达式的字符串本身不合法, 会报错；
+ 用编译后的正则表达式去匹配字符串. 

如果一个正则表达式要重复使用几千次, 出于效率的考虑, 我们可以预编译该正则表达式, 接下来重复使用时就不需要编译这个步骤了, 直接匹配：

```python
>>> import re
# 编译:
>>> re_telephone = re.compile(r'^(\d{3})-(\d{3,8})$')
# 使用：
>>> re_telephone.match('010-12345').groups()
('010', '12345')
>>> re_telephone.match('010-8086').groups()
('010', '8086')
```

编译后生成`Regular Expression`对象, 由于该对象自己包含了正则表达式, 所以调用对应的方法时不用给出正则字符串. 

### 小结-正则表达式

正则表达式非常强大, 要在短短的一节里讲完是不可能的. 要讲清楚正则的所有内容, 可以写一本厚厚的书了. 如果你经常遇到正则表达式的问题, 你可能需要一本正则表达式的参考书. 

### 练习

请尝试写一个验证Email地址的正则表达式. 版本一应该可以验证出类似的Email：

+ `someone@gmail.com`
+ `bill.gates@microsoft.com`

```python
re.match(r'^[\w\.]+?@[\w]+\.com$','bill.gates@microsoft.com')
```

```python
# -*- coding: utf-8 -*-
import re

def is_valid_email(addr):
    if re.match(r'^[\w\.]+?@[\w]+\.com$',str(addr)):
        return True
    else:
        return False

# 测试:
assert is_valid_email('someone@gmail.com')
assert is_valid_email('bill.gates@microsoft.com')
assert not is_valid_email('bob#example.com')
assert not is_valid_email('mr-bob@example.com')
print('ok')
```

版本二可以提取出带名字的Email地址：

+ `<Tom Paris> tom@voyager.org` => `Tom Paris`
+ `bob@example.com` => `bob`

```python
re.match(r'^(<([\w ]+)>)?\s*(\w+)@[\w]+\.(com|org)$', '<Tom Paris> tom@voyager.org').groups()
```

```python
re.match(r'^(<([\w ]+)>)?\s*(\w+)@[\w]+\.(com|org)$', 'bob@example.com').groups()
```

```python
# -*- coding: utf-8 -*-
import re

def name_of_email(addr):
    m=re.match(r'^(<([\w ]+)>)?\s*(\w+)@[\w]+\.(com|org)$', str(addr))
    if m[2]:
        return m[2]
    elif m[3]:
        return m[3]
    else:
        return 'no match names'

# 测试:
assert name_of_email('<Tom Paris> tom@voyager.org') == 'Tom Paris'
assert name_of_email('tom@voyager.org') == 'tom'
print('ok')
```

## 常用内建模块

Python之所以自称“batteries included”, 就是因为内置了许多非常有用的模块, 无需额外安装和配置, 即可直接使用. 

本章将介绍一些常用的内建模块. 

### datetime

`datetime`是Python处理日期和时间的标准库. 
获取当前日期和时间

我们先看如何获取当前日期和时间：

```python
>>> from datetime import datetime
>>> now = datetime.now() # 获取当前datetime
>>> print(now)
2015-05-18 16:28:07.198690
>>> print(type(now))
<class 'datetime.datetime'>
```

注意到`datetime`是模块, `datetime`模块还包含一个`datetime`类, 
通过`from datetime import datetime`导入的才是`datetime`这个类. 

如果仅导入`import datetime`, 则必须引用全名`datetime.datetime`. 

`datetime.now()`返回当前日期和时间, 其类型是`datetime`. 

#### 获取指定日期和时间

要指定某个日期和时间, 我们直接用参数构造一个`datetime`：

```python
>>> from datetime import datetime
>>> dt = datetime(2015, 4, 19, 12, 20) # 用指定日期时间创建datetime
>>> print(dt)
2015-04-19 12:20:00
```

#### datetime转换为timestamp

在计算机中, 时间实际上是用数字表示的. 
我们把1970年1月1日 `00:00:00 UTC+00:00`时区的时刻称为`epoch time`, 记为`0`(1970年以前的时间timestamp为负数), 当前时间就是相对于`epoch time`的秒数, 称为timestamp. 

你可以认为：

```python
timestamp = 0 = 1970-1-1 00:00:00 UTC+0:00
```

对应的北京时间是：

```python
timestamp = 0 = 1970-1-1 08:00:00 UTC+8:00
```

可见`timestamp`的值与时区毫无关系, 因为`timestamp`一旦确定, 其`UTC`时间就确定了, 转换到任意时区的时间也是完全确定的, 这就是为什么计算机存储的当前时间是以`timestamp`表示的, 因为全球各地的计算机在任意时刻的`timestamp`都是完全相同的(假定时间已校准). 

把一个`datetime`类型转换为`timestamp`只需要简单调用`timestamp`()方法：

```python
>>> from datetime import datetime
>>> dt = datetime(2015, 4, 19, 12, 20) # 用指定日期时间创建datetime
>>> dt.timestamp() # 把datetime转换为timestamp
1429417200.0
```

注意Python的`timestamp`是一个浮点数. 如果有小数位, 小数位表示毫秒数. 

某些编程语言(如Java和JavaScript)的`timestamp`使用整数表示毫秒数, 这种情况下只需要把`timestamp`除以1000就得到Python的浮点表示方法. 

#### timestamp转换为datetime

要把`timestamp`转换为`datetime`, 使用`datetime`提供的`fromtimestamp`()方法：

```python
>>> from datetime import datetime
>>> t = 1429417200.0
>>> print(datetime.fromtimestamp(t))
2015-04-19 12:20:00
```

注意到`timestamp`是一个浮点数, 它没有时区的概念, 
而`datetime`是有时区的. 上述转换是在`timestamp`和本地时间做转换. 

本地时间是指当前操作系统设定的时区. 例如北京时区是东8区, 则本地时间：

```python
2015-04-19 12:20:00
```

实际上就是UTC+8:00时区的时间：

```python
2015-04-19 12:20:00 UTC+8:00
```

而此刻的格林威治标准时间与北京时间差了8小时, 也就是UTC+0:00时区的时间应该是：

```python
2015-04-19 04:20:00 UTC+0:00
```

`timestamp`也可以直接被转换到UTC标准时区的时间：

```python
>>> from datetime import datetime
>>> t = 1429417200.0
>>> print(datetime.fromtimestamp(t)) # 本地时间
2015-04-19 12:20:00
>>> print(datetime.utcfromtimestamp(t)) # UTC时间
2015-04-19 04:20:00
```

#### str转换为datetime

很多时候, 用户输入的日期和时间是字符串, 要处理日期和时间, 
首先必须把`str`转换为`datetime`. 
转换方法是通过`datetime.strptime()`实现, 需要一个日期和时间的格式化字符串：
p should be parse

```python
>>> from datetime import datetime
>>> cday = datetime.strptime('2015-6-1 18:19:59', '%Y-%m-%d %H:%M:%S')
>>> print(cday)
2015-06-01 18:19:59
```

字符串`'%Y-%m-%d %H:%M:%S'`规定了日期和时间部分的格式. 
[详细的说明请参考Python文档][].

[详细的说明请参考Python文档]: https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior

注意转换后的`datetime`是没有时区信息的. 

#### datetime转换为str

如果已经有了`datetime`对象, 要把它格式化为字符串显示给用户, 就需要转换为str, 转换方法是通过`strftime()`实现的, 同样需要一个日期和时间的格式化字符串：

```python
>>> from datetime import datetime
>>> now = datetime.now()
>>> print(now.strftime('%a, %b %d %H:%M'))
Mon, May 05 16:28
```

#### datetime加减

对日期和时间进行加减实际上就是把`datetime`往后或往前计算, 得到新的`datetime`. 加减可以直接用+和-运算符, 不过需要导入timedelta这个类：

```python
>>> from datetime import datetime, timedelta
>>> now = datetime.now()
>>> now
datetime.datetime(2015, 5, 18, 16, 57, 3, 540997)
>>> now + timedelta(hours=10)
datetime.datetime(2015, 5, 19, 2, 57, 3, 540997)
>>> now - timedelta(days=1)
datetime.datetime(2015, 5, 17, 16, 57, 3, 540997)
>>> now + timedelta(days=2, hours=12)
datetime.datetime(2015, 5, 21, 4, 57, 3, 540997)
```

可见, 使用`timedelta`你可以很容易地算出前几天和后几天的时刻. 

#### 本地时间转换为UTC时间

本地时间是指系统设定时区的时间, 例如北京时间是`UTC+8:00`时区的时间, 
而`UTC`时间指`UTC+0:00`时区的时间. 

一个`datetime`类型有一个时区属性`tzinfo`, 但是默认为`None`, 
所以无法区分这个`datetime`到底是哪个时区, 除非强行给`datetime`设置一个时区：

```python
>>> from datetime import datetime, timedelta, timezone
>>> tz_utc_8 = timezone(timedelta(hours=8)) # 创建时区UTC+8:00
>>> now = datetime.now()
>>> now
datetime.datetime(2015, 5, 18, 17, 2, 10, 871012)
>>> dt = now.replace(tzinfo=tz_utc_8) # 强制设置为UTC+8:00
>>> dt
datetime.datetime(2015, 5, 18, 17, 2, 10, 871012, tzinfo=datetime.timezone(datetime.timedelta(0, 28800)))
```

如果系统时区恰好是`UTC+8:00`, 那么上述代码就是正确的, 
否则, 不能强制设置为`UTC+8:00`时区. 

#### 时区转换

我们可以先通过`utcnow()`拿到当前的`UTC`时间, 再转换为任意时区的时间：

```python
# 拿到UTC时间, 并强制设置时区为UTC+0:00:
>>> utc_dt = datetime.utcnow().replace(tzinfo=timezone.utc)
>>> print(utc_dt)
2015-05-18 09:05:12.377316+00:00
# astimezone()将转换时区为北京时间:
>>> bj_dt = utc_dt.astimezone(timezone(timedelta(hours=8)))
>>> print(bj_dt)
2015-05-18 17:05:12.377316+08:00
# astimezone()将转换时区为东京时间:
>>> tokyo_dt = utc_dt.astimezone(timezone(timedelta(hours=9)))
>>> print(tokyo_dt)
2015-05-18 18:05:12.377316+09:00
# astimezone()将bj_dt转换时区为东京时间:
>>> tokyo_dt2 = bj_dt.astimezone(timezone(timedelta(hours=9)))
>>> print(tokyo_dt2)
2015-05-18 18:05:12.377316+09:00
```

时区转换的关键在于, 拿到一个`datetime`时, 要获知其正确的时区, 
然后强制设置时区, 作为基准时间. 

利用带时区的`datetime`, 通过`astimezone()`方法, 可以转换到任意时区. 

注：不是必须从`UTC+0:00`时区转换到其他时区, 任何带时区的`datetime`都可以正确转换, 例如上述`bj_dt`到`tokyo_dt`的转换. 

#### 小结-datetime

`datetime`表示的时间需要时区信息才能确定一个特定的时间, 否则只能视为本地时间. 

如果要存储`datetime`, 最佳方法是将其转换为`timestamp`再存储, 因为`timestamp`的值与时区完全无关. 

#### 练习-datetime

假设你获取了用户输入的日期和时间如`'2015-1-21 9:01:30'`, 以及一个时区信息如`'UTC+5:00'`, 均是`str`, 请编写一个函数将其转换为`timestamp`：

```python
# -*- coding:utf-8 -*-

import re
from datetime import datetime, timezone, timedelta

def to_timestamp(dt_str, tz_str, timezone_local=8):
    # 本机时区为 UTC+8:00
    the_time = datetime.strptime(str(dt_str), '%Y-%m-%d %H:%M:%S')
    #解析输入的时间, 时区是UTC+8
    time_delta_hour=int(
        re.match(r'^UTC((?:\+|\-)\d+):\d+',str(tz_str)).group(1)
        )-timezone_local
        # 正则表达式抽出 输入的时区, 然后-8, 得到与本地时区的差
    the_time_local = the_time - timedelta(hours=time_delta_hour)
    # 得到输入时间在本地时区的表示
    return the_time_local.timestamp()
    #返回timestamp表示

# 测试:
t1 = to_timestamp('2015-6-1 08:10:30', 'UTC+7:00')
assert t1 == 1433121030.0, t1

t2 = to_timestamp('2015-5-31 16:10:30', 'UTC-09:00')
assert t2 == 1433121030.0, t2

print('ok')
```

### collections

collections是Python内建的一个集合模块, 提供了许多有用的集合类. 

#### namedtuple-命名元组

我们知道`tuple`可以表示不变集合, 例如, 一个点的二维坐标就可以表示成：

```python
>>> p = (1, 2)
```

但是, 看到`(1, 2)`, 很难看出这个`tuple`是用来表示一个坐标的. 

定义一个class又小题大做了, 这时, `namedtuple`就派上了用场：

```python
>>> from collections import namedtuple
>>> Point = namedtuple('Point', ['x', 'y'])
>>> p = Point(1, 2)
>>> p.x
1
>>> p.y
2
```

`namedtuple`是一个函数, 它用来创建一个自定义的`tuple`对象, 并且规定了`tuple`元素的个数, 并可以用属性而不是索引来引用`tuple`的某个元素. 

这样一来, 我们用`namedtuple`可以很方便地定义一种数据类型, 
它具备`tuple`的不变性, 又可以根据属性来引用, 使用十分方便. 

可以验证创建的Point对象是tuple的一种子类：

```python
>>> isinstance(p, Point)
True
>>> isinstance(p, tuple)
True
```

类似的, 如果要用坐标和半径表示一个圆, 也可以用`namedtuple`定义：

```python
# namedtuple('名称', [属性list]):
Circle = namedtuple('Circle', ['x', 'y', 'r'])
```

#### deque-双向列表

使用`list`存储数据时, 按索引访问元素很快, 但是插入和删除元素就很慢了, 因为list是线性存储, 数据量大的时候, 插入和删除效率很低. 

`deque`是为了高效实现插入和删除操作的双向列表, 适合用于队列和栈：

```python
>>> from collections import deque
>>> q = deque(['a', 'b', 'c'])
>>> q.append('x')
>>> q.appendleft('y')
>>> q
deque(['y', 'a', 'b', 'c', 'x'])
```

`deque`除了实现list的`append()`和`pop()`外, 
还支持`appendleft()`和`popleft()`, 这样就可以非常高效地往头部添加或删除元素. 

#### defaultdict-缺失key自定义

使用`dict`时, 如果引用的Key不存在, 就会抛出KeyError. 如果希望key不存在时, 返回一个默认值, 就可以用defaultdict：

```python
>>> from collections import defaultdict
>>> dd = defaultdict(lambda: 'N/A')
>>> dd['key1'] = 'abc'
>>> dd['key1'] # key1存在
'abc'
>>> dd['key2'] # key2不存在, 返回默认值
'N/A'
```

注意默认值是调用函数返回的, 而函数在创建`defaultdict`对象时传入. 

除了在Key不存在时返回默认值, `defaultdict`的其他行为跟dict是完全一样的. 

#### OrderedDict-有序字典

使用dict时, `Key`是无序的. 在对dict做迭代时, 我们无法确定`Key`的顺序. 

如果要保持`Key`的顺序, 可以用`OrderedDict`：

```python
>>> from collections import OrderedDict
>>> d = dict([('a', 1), ('b', 2), ('c', 3)])
>>> d # dict的Key是无序的
{'a': 1, 'c': 3, 'b': 2}
>>> od = OrderedDict([('a', 1), ('b', 2), ('c', 3)])
>>> od # OrderedDict的Key是有序的
OrderedDict([('a', 1), ('b', 2), ('c', 3)])
```

注意, `OrderedDict`的`Key`会按照插入的顺序排列, 不是`Key`本身排序：

```python
>>> od = OrderedDict()
>>> od['z'] = 1
>>> od['y'] = 2
>>> od['x'] = 3
>>> list(od.keys()) # 按照插入的Key的顺序返回
['z', 'y', 'x']
```

`OrderedDict`可以实现一个`FIFO`(先进先出)的`dict`, 当容量超出限制时, 先删除最早添加的`Key`：

```python
from collections import OrderedDict

class LastUpdatedOrderedDict(OrderedDict):

    def __init__(self, capacity):
        super(LastUpdatedOrderedDict, self).__init__()
        self._capacity = capacity

    def __setitem__(self, key, value):
        containsKey = 1 if key in self else 0
        if len(self) - containsKey >= self._capacity:
            last = self.popitem(last=False)
            print('remove:', last)
        if containsKey:
            del self[key]
            print('set:', (key, value))
        else:
            print('add:', (key, value))
        OrderedDict.__setitem__(self, key, value)
```

#### ChainMap-字典串联

`ChainMap`可以把一组`dict`串起来并组成一个逻辑上的`dict`. `ChainMap`本身也是一个`dict`, 但是查找的时候, 会按照顺序在内部的dict依次查找. 

什么时候使用`ChainMap`最合适？举个例子：应用程序往往都需要传入参数, 参数可以通过命令行传入, 可以通过环境变量传入, 还可以有默认参数. 
我们可以用`ChainMap`实现参数的优先级查找, 即先查命令行参数, 如果没有传入, 再查环境变量, 如果没有, 就使用默认参数. 

下面的代码演示了如何查找`user`和`color`这两个参数：

```python
from collections import ChainMap
import os, argparse

# 构造缺省参数:
defaults = {
    'color': 'red',
    'user': 'guest'
}

# 构造命令行参数:
parser = argparse.ArgumentParser()
parser.add_argument('-u', '--user')
parser.add_argument('-c', '--color')
namespace = parser.parse_args()
command_line_args = { k: v for k, v in vars(namespace).items() if v }

# 组合成ChainMap:
combined = ChainMap(command_line_args, os.environ, defaults)

# 打印参数:
print('color=%s' % combined['color'])
print('user=%s' % combined['user'])
```

没有任何参数时, 打印出默认参数：

```python
$ python3 use_chainmap.py
color=red
user=guest
```

当传入命令行参数时, 优先使用命令行参数：

```python
$ python3 use_chainmap.py -u bob
color=red
user=bob
```

同时传入命令行参数和环境变量, 命令行参数的优先级较高：

```python
$ user=admin color=green python3 use_chainmap.py -u bob
color=green
user=bob
```

#### Counter-统计频率

`Counter`是一个简单的计数器, 例如, 统计字符出现的个数：

```python
>>> from collections import Counter
>>> c = Counter()
>>> for ch in 'programming':
...     c[ch] = c[ch] + 1
...
>>> c
Counter({'g': 2, 'm': 2, 'r': 2, 'a': 1, 'i': 1, 'o': 1, 'n': 1, 'p': 1})
>>> c.update('hello') # 也可以一次性update
>>> c
Counter({'r': 2, 'o': 2, 'g': 2, 'm': 2, 'l': 2, 'p': 1, 'a': 1, 'i': 1, 'n': 1, 'h': 1, 'e': 1})
```

`Counter`实际上也是`dict`的一个子类, 上面的结果可以看出每个字符出现的次数. 

#### 小结-collections

`collections`模块提供了一些有用的集合类, 可以根据需要选用. 

### base64-二进制打印

`Base64`是一种用64个字符来表示任意二进制数据的方法. 

用记事本打开`exe`, `jpg`, `pdf`这些文件时, 我们都会看到一大堆乱码, 
因为二进制文件包含很多无法显示和打印的字符, 
所以, 如果要让记事本这样的文本处理软件能处理二进制数据, 
就需要一个二进制到字符串的转换方法. 
`Base64`是一种最常见的二进制编码方法. 

`Base64`的原理很简单, 首先, 准备一个包含64个字符的数组：

```python
['A', 'B', 'C', ... 'a', 'b', 'c', ... '0', '1', ... '+', '/']
```

然后, 对二进制数据进行处理, 每3个字节一组, 一共是`3x8=24bit`, 划为`4`组, 每组正好`6`个`bit`：

#### base64-encode

这样我们得到4个数字作为索引, 然后查表, 获得相应的4个字符, 就是编码后的字符串. 

所以, `Base64`编码会把`3`字节的二进制数据编码为`4`字节的文本数据, 长度增加`33%`, 好处是编码后的文本数据可以在邮件正文, 网页等直接显示. 

如果要编码的二进制数据不是`3`的倍数, 最后会剩下`1`个或`2`个字节怎么办？`Base64`用`\x00`字节在末尾补足后, 再在编码的末尾加上`1`个或`2`个`=`号, 表示补了多少字节, 解码的时候, 会自动去掉. 

Python内置的`base64`可以直接进行`base64`的编解码：

```python
>>> import base64
>>> base64.b64encode(b'binary\x00string')
b'YmluYXJ5AHN0cmluZw=='
>>> base64.b64decode(b'YmluYXJ5AHN0cmluZw==')
b'binary\x00string'
```

由于标准的`Base64`编码后可能出现字符`+`和`/`, 在`URL`中就不能直接作为参数, 所以又有一种"`url safe`"的`base64`编码, 其实就是把字符`+`和`/`分别变成`-`和`_`：

```python
>>> base64.b64encode(b'i\xb7\x1d\xfb\xef\xff')
b'abcd++//'
>>> base64.urlsafe_b64encode(b'i\xb7\x1d\xfb\xef\xff')
b'abcd--__'
>>> base64.urlsafe_b64decode('abcd--__')
b'i\xb7\x1d\xfb\xef\xff'
```

还可以自己定义`64`个字符的排列顺序, 这样就可以自定义`Base64`编码, 不过, 通常情况下完全没有必要. 

`Base64`是一种通过查表的编码方法, 不能用于加密, 即使使用自定义的编码表也不行. 
`Base64`适用于小段内容的编码, 比如数字证书签名, Cookie的内容等. 
由于=字符也可能出现在`Base64`编码中, 但`=`用在URL, Cookie里面会造成歧义, 所以, 很多`Base64`编码后会把`=`去掉：

```python
# 标准Base64:
'abcd' -> 'YWJjZA=='
# 自动去掉=:
'abcd' -> 'YWJjZA'
```

去掉`=`后怎么解码呢？因为`Base64`是把3个字节变为4个字节, 所以, `Base64`编码的长度永远是`4`的倍数, 因此, 需要加上`=`把`Base64`字符串的长度变为4的倍数, 就可以正常解码了. 

#### 小结-base64

Base64是一种任意二进制到文本字符串的编码方法, 常用于在URL, Cookie, 网页中传输少量二进制数据. 

#### 练习-base64

请写一个能处理去掉`=`的`base64`解码函数：

以`Unicode`表示的`str`通过`encode()`方法可以编码为指定的`bytes`, 例如：

```python
>>> 'ABC'.encode('ascii')
b'ABC'
>>> '中文'.encode('utf-8')
b'\xe4\xb8\xad\xe6\x96\x87'
>>> '中文'.encode('ascii')
```

```python
# -*- coding: utf-8 -*-
import base64

def safe_base64_decode(s):
    eq_length=4-(len(s)%4)
    full_byte=s+b'='*eq_length
    return base64.b64decode(full_byte)

# 测试:
assert b'abcd' == safe_base64_decode(b'YWJjZA=='), safe_base64_decode('YWJjZA==')
assert b'abcd' == safe_base64_decode(b'YWJjZA'), safe_base64_decode('YWJjZA')
print('ok')
```

### struct-二进制格式化

+ `0110110`->bit(比特)
+ 8`个比特(`bit`)作为一个字节(`byte`)
+ `Unicode`常用两个字节表示一个字符(如用到生僻字, 需要`4`个字节)

```python
>>> '中文'.encode('utf-8')
b'\xe4\xb8\xad\xe6\x96\x87'
```

准确地讲, Python没有专门处理字节的数据类型. 但由于`b'str'`可以表示字节, 所以, `字节数组＝二进制str`. 而在C语言中, 我们可以很方便地用`struct`, `union`来处理字节, 以及字节和`int`, `float`的转换. 

在Python中, 比方说要把一个`32`位无符号整数变成字节, 也就是`4`个长度的`bytes`, 你得配合位运算符这么写：

```python
>>> n = 10240099
>>> b1 = (n & 0xff000000) >> 24
>>> b2 = (n & 0xff0000) >> 16
>>> b3 = (n & 0xff00) >> 8
>>> b4 = n & 0xff
>>> bs = bytes([b1, b2, b3, b4])
>>> bs
b'\x00\x9c@c'
```

非常麻烦. 如果换成浮点数就无能为力了. 
好在Python提供了一个`struct`模块来解决`bytes`和其他二进制数据类型的转换. 
`struct`的`pack`函数把任意数据类型变成`bytes`：

```python
>>> import struct
>>> struct.pack('>I', 10240099)
b'\x00\x9c@c'
```

pack的第一个参数是处理指令, `'>I'`的意思是：
`>`表示字节顺序是`big-endian`, 也就是网络序, `I`表示`4字节无符号整数`. 
后面的参数个数要和处理指令一致. 

`unpack`把`bytes`变成相应的数据类型：

```python
>>> struct.unpack('>IH', b'\xf0\xf0\xf0\xf0\x80\x80')
(4042322160, 32896)
```

根据`>IH`的说明, 后面的`bytes`依次变为`I`：4字节无符号整数和
`H`：2字节无符号整数. 

所以, 尽管Python不适合编写底层操作字节流的代码, 但在对性能要求不高的地方, 利用struct就方便多了. 

struct模块定义的数据类型可以参考Python官方文档：

[数据类型可以参考Python官方文档][]

[数据类型可以参考Python官方文档]: https://docs.python.org/3/library/struct.html#format-characters

Windows的位图文件(`.bmp`)是一种非常简单的文件格式, 
我们来用`struct`分析一下. 

首先找一个`bmp`文件, 没有的话用“画图”画一个. 

读入前`30`个字节来分析：

```python
>>> s = b'\x42\x4d\x38\x8c\x0a\x00\x00\x00\x00\x00\x36\x00\x00\x00\x28\x00\x00\x00\x80\x02\x00\x00\x68\x01\x00\x00\x01\x00\x18\x00'
```

`BMP`格式采用小端方式存储数据, 文件头的结构按顺序如下：

两个字节：`'BM'`表示`Windows`位图, `'BA'`表示`OS/2`位图； 一个`4`字节整数：表示位图大小； 一个`4`字节整数：保留位, 始终为`0`； 一个`4`字节整数：实际图像的偏移量； 一个`4`字节整数：Header的字节数； 一个`4`字节整数：图像宽度； 一个`4`字节整数：图像高度； 一个`2`字节整数：始终为1； 一个`2`字节整数：颜色数. 

所以, 组合起来用`unpack`读取：

```python
>>> struct.unpack('<ccIIIIIIHH', s)
(b'B', b'M', 691256, 0, 54, 40, 640, 360, 1, 24)
```

结果显示, `b'B'`, `b'M'`说明是`Windows`位图, 
位图大小为`640x360`, 颜色数为24. 

请编写一个`bmpinfo.py`, 可以检查任意文件是否是位图文件, 如果是, 打印出图片大小和颜色数. 

```python
# -*- coding: utf-8 -*-

bmp_data = base64.b64decode('Qk1oAgAAAAAAADYAAAAoAAAAHAAAAAoAAAABABAAAAAAADICAAASCwAAEgsAA' +
                   'AAAAAAAAAAA/3//f/9//3//f/9//3//f/9//3//f/9//3//f/9//3//f/9//3//f/9//3/' +
                   '/f/9//3//f/9//3//f/9/AHwAfAB8AHwAfAB8AHwAfP9//3//fwB8AHwAfAB8/3//f/9/A' +
                   'HwAfAB8AHz/f/9//3//f/9//38AfAB8AHwAfAB8AHwAfAB8AHz/f/9//38AfAB8/3//f/9' +
                   '//3//fwB8AHz/f/9//3//f/9//3//f/9/AHwAfP9//3//f/9/AHwAfP9//3//fwB8AHz/f' +
                   '/9//3//f/9/AHwAfP9//3//f/9//3//f/9//38AfAB8AHwAfAB8AHwAfP9//3//f/9/AHw' +
                   'AfP9//3//f/9//38AfAB8/3//f/9//3//f/9//3//fwB8AHwAfAB8AHwAfAB8/3//f/9//' +
                   '38AfAB8/3//f/9//3//fwB8AHz/f/9//3//f/9//3//f/9/AHwAfP9//3//f/9/AHwAfP9' +
                   '//3//fwB8AHz/f/9/AHz/f/9/AHwAfP9//38AfP9//3//f/9/AHwAfAB8AHwAfAB8AHwAf' +
                   'AB8/3//f/9/AHwAfP9//38AfAB8AHwAfAB8AHwAfAB8/3//f/9//38AfAB8AHwAfAB8AHw' +
                   'AfAB8/3//f/9/AHwAfAB8AHz/fwB8AHwAfAB8AHwAfAB8AHz/f/9//3//f/9//3//f/9//' +
                   '3//f/9//3//f/9//3//f/9//3//f/9//3//f/9//3//f/9//3//f/9//38AAA==')

import base64, struct

def bmp_info(data):
    bmp_unpack=struct.unpack('<ccIIIIIIHH', bmp_data[0:30])
    if bmp_unpack[0:2] == (b'B',b'M'):
        return {
        'width': bmp_unpack[-4] ,
        'height': bmp_unpack[-3],
        'color': bmp_unpack[-1]
        }

# 测试
bi = bmp_info(bmp_data)
assert bi['width'] == 28
assert bi['height'] == 10
assert bi['color'] == 16
print('ok')
```

### hashlib-哈希算法

摘要算法简介

Python的hashlib提供了常见的摘要算法, 如`MD5`, `SHA1`等等. 

什么是摘要算法呢？摘要算法又称哈希算法, 散列算法. 它通过一个函数, 把任意长度的数据转换为一个长度固定的数据串(通常用16进制的字符串表示). 

举个例子, 你写了一篇文章, 内容是一个字符串`'how to use python hashlib - by Michael'`, 并附上这篇文章的摘要是`'2d73d4f15c0db7f5ecb321b6a65e5d6d'`. 如果有人篡改了你的文章, 并发表为`'how to use python hashlib - by Bob'`, 你可以一下子指出Bob篡改了你的文章, 因为根据`'how to use python hashlib - by Bob'`计算出的摘要不同于原始文章的摘要. 

可见, 摘要算法就是通过摘要函数f()对任意长度的数据`data`计算出固定长度的摘要`digest`, 目的是为了发现原始数据是否被人篡改过. 

摘要算法之所以能指出数据是否被篡改过, 就是因为摘要函数是一个单向函数, 计算f(`data`)很容易, 但通过`digest`反推`data`却非常困难. 而且, 对原始数据做一个`bit`的修改, 都会导致计算出的摘要完全不同. 

我们以常见的摘要算法MD5为例, 计算出一个字符串的`MD5`值：

```python
import hashlib

md5 = hashlib.md5()
md5.update('how to use md5 in python hashlib?'.encode('utf-8'))
print(md5.hexdigest())
```

计算结果如下：

```python
d26a53750bc40b38b65a520292f69306
```

如果数据量很大, 可以分块多次调用`update()`, 最后计算的结果是一样的：

```python
import hashlib

md5 = hashlib.md5()
md5.update('how to use md5 in '.encode('utf-8'))
md5.update('python hashlib?'.encode('utf-8'))
print(md5.hexdigest())
```

试试改动一个字母, 看看计算的结果是否完全不同. 

`MD5`是最常见的摘要算法, 速度很快, 生成结果是固定的`128 bit`字节, 通常用一个`32`位的`16`进制字符串表示. 

另一种常见的摘要算法是`SHA1`, 调用`SHA1`和调用MD5完全类似：

```python
import hashlib

sha1 = hashlib.sha1()
sha1.update('how to use sha1 in '.encode('utf-8'))
sha1.update('python hashlib?'.encode('utf-8'))
print(sha1.hexdigest())
```

`SHA1`的结果是`160 bit`字节, 通常用一个40位的16进制字符串表示. 
比`SHA1`更安全的算法是`SHA256`和`SHA512`, 不过越安全的算法不仅越慢, 而且摘要长度更长. 

有没有可能两个不同的数据通过某个摘要算法得到了相同的摘要？完全有可能, 因为任何摘要算法都是把无限多的数据集合映射到一个有限的集合中. 这种情况称为碰撞, 比如Bob试图根据你的摘要反推出一篇文章`'how to learn hashlib in python - by Bob'`, 并且这篇文章的摘要恰好和你的文章完全一致, 这种情况也并非不可能出现, 但是非常非常困难. 

#### 摘要算法应用

摘要算法能应用到什么地方？举个常用例子：

任何允许用户登录的网站都会存储用户登录的用户名和口令. 
如何存储用户名和口令呢？方法是存到数据库表中：

| name | password |
| ----- | ----- |
| michael | 123456 |
| bob | abc999 |
| alice | alice2008 |

如果以明文保存用户口令, 如果数据库泄露, 所有用户的口令就落入黑客的手里. 
此外, 网站运维人员是可以访问数据库的, 也就是能获取到所有用户的口令. 

正确的保存口令的方式是不存储用户的明文口令, 而是存储用户口令的摘要, 比如`MD5`：

| username | password |
| ----- | ----- |
| `michael` | `e10adc3949ba59abbe56e057f20f883e` |
| `bob` | `878ef96e86145580c38c87f0410ad153` |
| `alice` | `99b1c2188db85afee403b1536010c2c9` |

当用户登录时, 首先计算用户输入的明文口令的`MD5`, 
然后和数据库存储的MD5对比, 
如果一致, 说明口令输入正确, 如果不一致, 口令肯定错误. 

#### 练习-hashlib

根据用户输入的口令, 计算出存储在数据库中的`MD5`口令：

```python
def calc_md5(password):
    pass
```

存储`MD5`的好处是即使运维人员能访问数据库, 也无法获知用户的明文口令. 
设计一个验证用户登录的函数, 根据用户输入的口令是否正确, 返回`True`或`False`：

```python
# -*- coding: utf-8 -*-
import hashlib

db = {
    'michael': 'e10adc3949ba59abbe56e057f20f883e',
    'bob': '878ef96e86145580c38c87f0410ad153',
    'alice': '99b1c2188db85afee403b1536010c2c9'
}


def login(user, password):
    md5_new = hashlib.md5()
    md5_new.update(str(password).encode('utf-8'))
    return md5_new.hexdigest()==db[str(user)]

# 测试:
assert login('michael', '123456')
assert login('bob', 'abc999')
assert login('alice', 'alice2008')
assert not login('michael', '1234567')
assert not login('bob', '123456')
assert not login('alice', 'Alice2008')
print('ok')
```

采用`MD5`存储口令是否就一定安全呢？也不一定. 假设你是一个黑客, 已经拿到了存储`MD5`口令的数据库, 如何通过`MD5`反推用户的明文口令呢？暴力破解费事费力, 真正的黑客不会这么干. 

考虑这么个情况, 很多用户喜欢用`123456`, `888888`, `password`这些简单的口令, 于是, 黑客可以事先计算出这些常用口令的`MD5`值, 得到一个反推表：

```python
'e10adc3949ba59abbe56e057f20f883e': '123456'
'21218cca77804d2ba1922c33e0151105': '888888'
'5f4dcc3b5aa765d61d8327deb882cf99': 'password'
```

这样, 无需破解, 只需要对比数据库的`MD5`, 黑客就获得了使用常用口令的用户账号. 
对于用户来讲, 当然不要使用过于简单的口令. 但是, 我们能否在程序设计上对简单口令加强保护呢？
由于常用口令的`MD5`值很容易被计算出来, 所以, 要确保存储的用户口令不是那些已经被计算出来的常用口令的`MD5`, 这一方法通过对原始口令加一个复杂字符串来实现, 俗称“加盐”：

```python
def calc_md5(password):
    return get_md5(password + 'the-Salt')
```

经过`Salt`处理的MD5口令, 只要`Salt`不被黑客知道, 即使用户输入简单口令, 也很难通过`MD5`反推明文口令. 

但是如果有两个用户都使用了相同的简单口令比如`123456`, 在数据库中, 将存储两条相同的`MD5`值, 这说明这两个用户的口令是一样的. 有没有办法让使用相同口令的用户存储不同的`MD5`呢？

如果假定用户无法修改登录名, 就可以通过把登录名作为`Salt`的一部分来计算`MD5`, 从而实现相同口令的用户也存储不同的`MD5`. 

#### 练习-hashlib-2

根据用户输入的登录名和口令模拟用户注册, 计算更安全的`MD5`：

```python
db = {}

def register(username, password):
    db[username] = get_md5(password + username + 'the-Salt')
```

然后, 根据修改后的`MD5`算法实现用户登录的验证：

```python
# -*- coding: utf-8 -*-
import hashlib, random

def get_md5(s):
    return hashlib.md5(s.encode('utf-8')).hexdigest()

class User(object):
    def __init__(self, username, password):
        self.username = username
        self.salt = ''.join([chr(random.randint(48, 122)) for i in range(20)])
        self.password = get_md5(password + self.salt)
db = {
    'michael': User('michael', '123456'),
    'bob': User('bob', 'abc999'),
    'alice': User('alice', 'alice2008')
}
```

```python
# -*- coding: utf-8 -*-
import hashlib, random

def get_md5(s):
    return hashlib.md5(s.encode('utf-8')).hexdigest()

class User(object):
    def __init__(self, username, password):
        self.username = username
        self.salt = ''.join([chr(random.randint(48, 122)) for i in range(20)])
        self.password = get_md5(password + self.salt)

db = {
    'michael': User('michael', '123456'),
    'bob': User('bob', 'abc999'),
    'alice': User('alice', 'alice2008')
}

def login(username, password):
    user = db[username]
    return user.password == get_md5(password+user.salt)


# 测试:
assert login('michael', '123456')
assert login('bob', 'abc999')
assert login('alice', 'alice2008')
assert not login('michael', '1234567')
assert not login('bob', '123456')
assert not login('alice', 'Alice2008')
print('ok')
```

#### hmac-赋key哈希算法

通过哈希算法, 我们可以验证一段数据是否有效, 方法就是对比该数据的哈希值, 例如, 判断用户口令是否正确, 我们用保存在数据库中的password_md5对比计算md5(password)的结果, 如果一致, 用户输入的口令就是正确的. 

为了防止黑客通过彩虹表根据哈希值反推原始口令, 在计算哈希的时候, 不能仅针对原始输入计算, 需要增加一个salt来使得相同的输入也能得到不同的哈希, 这样, 大大增加了黑客破解的难度. 

如果salt是我们自己随机生成的, 通常我们计算MD5时采用md5(message + salt). 但实际上, 把salt看做一个“口令”, 加salt的哈希就是：计算一段message的哈希时, 根据不通口令计算出不同的哈希. 要验证哈希值, 必须同时提供正确的口令. 

这实际上就是Hmac算法：Keyed-Hashing for Message Authentication. 它通过一个标准算法, 在计算哈希的过程中, 把key混入计算过程中. 

和我们自定义的加salt算法不同, Hmac算法针对所有哈希算法都通用, 无论是MD5还是SHA-1. 采用Hmac替代我们自己的salt算法, 可以使程序算法更标准化, 也更安全. 

Python自带的hmac模块实现了标准的Hmac算法. 我们来看看如何使用hmac实现带key的哈希. 

我们首先需要准备待计算的原始消息message, 随机key, 哈希算法, 这里采用MD5, 使用hmac的代码如下：

```python
>>> import hmac
>>> message = b'Hello, world!'
>>> key = b'secret'
>>> h = hmac.new(key, message, digestmod='MD5')
>>> # 如果消息很长, 可以多次调用h.update(msg)
>>> h.hexdigest()
'fa4ee7d173f2d97ee79022d1a7355bcf'
```

可见使用hmac和普通hash算法非常类似. hmac输出的长度和原始哈希算法的长度一致. 需要注意传入的key和message都是bytes类型, str类型需要首先编码为bytes. 

#### 练习-hmac

将上一节的`salt`改为标准的`hmac`算法, 验证用户口令：

```python
# -*- coding: utf-8 -*-
import hmac, random

def hmac_md5(key, s):
    return hmac.new(key.encode('utf-8'), s.encode('utf-8'), 'MD5').hexdigest()

class User(object):
    def __init__(self, username, password):
        self.username = username
        self.key = ''.join([chr(random.randint(48, 122)) for i in range(20)])
        self.password = hmac_md5(self.key, password)

db = {
    'michael': User('michael', '123456'),
    'bob': User('bob', 'abc999'),
    'alice': User('alice', 'alice2008')
}

def login(username, password):
    user = db[username]
    return user.password == hmac_md5(user.key, password)

# 测试:
assert login('michael', '123456')
assert login('bob', 'abc999')
assert login('alice', 'alice2008')
assert not login('michael', '1234567')
assert not login('bob', '123456')
assert not login('alice', 'Alice2008')
print('ok')
```

#### 小结-hmac

Python内置的`hmac`模块实现了标准的`Hmac`算法, 它利用一个`key`对message计算“杂凑”后的`hash`, 使用`hmac`算法比标准`hash`算法更安全, 因为针对相同的message, 不同的key会产生不同的`hash`. 

### itertools-迭代器

Python的内建模块`itertools`提供了非常有用的用于操作迭代对象的函数. 

#### 无限迭代器

首先, 我们看看`itertools`提供的几个“无限”迭代器：

```python
>>> import itertools
>>> natuals = itertools.count(1)
>>> for n in natuals:
...     print(n)
...
1
2
3
...
```

因为`count()`会创建一个无限的迭代器, 所以上述代码会打印出自然数序列, 根本停不下来, 只能按`Ctrl+C`退出. 

`cycle()`会把传入的一个序列无限重复下去：

```python
>>> import itertools
>>> cs = itertools.cycle('ABC') # 注意字符串也是序列的一种
>>> for c in cs:
...     print(c)
...
'A'
'B'
'C'
'A'
'B'
'C'
...
```

同样停不下来. 

`repeat()`负责把一个元素无限重复下去, 不过如果提供第二个参数就可以限定重复次数：

```python
>>> ns = itertools.repeat('A', 3)
>>> for n in ns:
...     print(n)
...
A
A
A
```

无限序列只有在`for`迭代时才会无限地迭代下去, 如果只是创建了一个迭代对象, 它不会事先把无限个元素生成出来, 事实上也不可能在内存中创建无限多个元素. 

无限序列虽然可以无限迭代下去, 但是通常我们会通过`takewhile()`等函数根据条件判断来截取出一个有限的序列：

```python
>>> natuals = itertools.count(1)
>>> ns = itertools.takewhile(lambda x: x <= 10, natuals)
>>> list(ns)
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

`itertools`提供的几个迭代器操作函数更加有用：

##### chain()

chain()可以把一组迭代对象串联起来, 形成一个更大的迭代器：

```python
>>> for c in itertools.chain('ABC', 'XYZ'):
...     print(c)
# 迭代效果：'A' 'B' 'C' 'X' 'Y' 'Z'
```

#### groupby()

`groupby()`把迭代器中相邻的重复元素挑出来放在一起：

```python
>>> for key, group in itertools.groupby('AAABBBCCAAA'):
...     print(key, list(group))
...
A ['A', 'A', 'A']
B ['B', 'B', 'B']
C ['C', 'C']
A ['A', 'A', 'A']
```

实际上挑选规则是通过函数完成的, 只要作用于函数的两个元素返回的值相等, 这两个元素就被认为是在一组的, 而函数返回值作为组的key. 如果我们要忽略大小写分组, 就可以让元素'A'和'a'都返回相同的key：

```python
>>> for key, group in itertools.groupby('AaaBBbcCAAa', lambda c: c.upper()):
...     print(key, list(group))
...
A ['A', 'a', 'a']
B ['B', 'B', 'b']
C ['c', 'C']
A ['A', 'A', 'a']
```

#### 练习-itertools

计算圆周率可以根据公式：

利用Python提供的`itertools`模块, 我们来计算这个序列的前N项和：

```python
# -*- coding: utf-8 -*-
import itertools

def pi(N):
    ' 计算pi的值 '
    # step 1: 创建一个奇数序列: 1, 3, 5, 7, 9, ...

    # step 2: 取该序列的前N项: 1, 3, 5, 7, 9, ..., 2*N-1.

    # step 3: 添加正负符号并用4除: 4/1, -4/3, 4/5, -4/7, 4/9, ...

    # step 4: 求和:
    return 3.14

# 测试:
print(pi(10))
print(pi(100))
print(pi(1000))
print(pi(10000))
assert 3.04 < pi(10) < 3.05
assert 3.13 < pi(100) < 3.14
assert 3.140 < pi(1000) < 3.141
assert 3.1414 < pi(10000) < 3.1415
print('ok')
```

```python
import itertools,logging
logging.basicConfig(level=logging.INFO)

def pi(N):
    natuals = itertools.count(1)
    nums = itertools.cycle((4,-4))
    def pi_series():
        while True:
            yield (next(natuals),next(nums))
    new_pi_series=list(itertools.takewhile(
    lambda x: abs(x[0])<= N, pi_series()))
    logging.info('the calc series is = %s' % str(new_pi_series[:2])+'<head-tail>'+str(new_pi_series[-2:]))
    pi_finite=0
    for nat, num in new_pi_series:
        pi_finite=pi_finite+(num/(2*nat-1))
    logging.info('the calculated pi is %s' % pi_finite)
    return pi_finite

# 测试:
print(pi(10))
print(pi(100))
print(pi(1000))
print(pi(10000))
assert 3.04 < pi(10) < 3.05
assert 3.13 < pi(100) < 3.14
assert 3.140 < pi(1000) < 3.141
assert 3.1414 < pi(10000) < 3.1415
print('ok')
```

#### 小结-itertools

`itertools`模块提供的全部是处理迭代功能的函数, 它们的返回值不是`list`, 而是`Iterator`, 只有用`for`循环迭代的时候才真正计算. 

### contextlib-上下文管理

在Python中, 读写文件这样的资源要特别注意, 必须在使用完毕后正确关闭它们. 
正确关闭文件资源的一个方法是使用`try...finally`：

```python
try:
    f = open('/path/to/file', 'r')
    f.read()
finally:
    if f:
        f.close()
```

写`try...finally`非常繁琐. 
Python的`with`语句允许我们非常方便地使用资源, 而不必担心资源没有关闭, 所以上面的代码可以简化为：

```python
with open('/path/to/file', 'r') as f:
    f.read()
```

并不是只有`open()`函数返回的`fp`对象才能使用`with`语句. 实际上, 任何对象, 只要正确实现了上下文管理, 就可以用于with语句. 

实现上下文管理是通过`__enter__`和`__exit__`这两个方法实现的. 例如, 下面的`class`实现了这两个方法：

```python
class Query(object):

    def __init__(self, name):
        self.name = name

    def __enter__(self):
        print('Begin')
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        if exc_type:
            print('Error')
        else:
            print('End')

    def query(self):
        print('Query info about %s...' % self.name)
```

这样我们就可以把自己写的资源对象用于with语句：

```python
with Query('Bob') as q:
    q.query()
```

#### @contextmanager

编写`__enter__`和`__exit__`仍然很繁琐, 因此Python的标准库`contextlib`提供了更简单的写法, 上面的代码可以改写如下：

```python
from contextlib import contextmanager

class Query(object):

    def __init__(self, name):
        self.name = name

    def query(self):
        print('Query info about %s...' % self.name)

@contextmanager
def create_query(name):
    print('Begin')
    q = Query(name)
    yield q
    print('End')
```

`@contextmanager`这个`decorator`接受一个`generator`, 用`yield`语句把`with ... as var`把变量输出出去, 然后, `with`语句就可以正常地工作了：

```python
with create_query('Bob') as q:
    q.query()
```

很多时候, 我们希望在某段代码执行前后自动执行特定代码, 也可以用`@contextmanager`实现. 例如：

```python
@contextmanager
def tag(name):
    print("<%s>" % name)
    yield
    print("</%s>" % name)

with tag("h1"):
    print("hello")
    print("world")
```

上述代码执行结果为：

```python
<h1>
hello
world
</h1>
```

代码的执行顺序是：

+ `with`语句首先执行`yield`之前的语句, 因此打印出`<h1>`；
+ `yield`调用会执行`with`语句内部的所有语句, 因此打印出`hello`和`world`；
+ 最后执行`yield`之后的语句, 打印出`</h1>`. 

因此, `@contextmanager`让我们通过编写`generator`来简化上下文管理. 

#### @closing

如果一个对象没有实现上下文, 我们就不能把它用于`with`语句. 这个时候, 可以用`closing()`来把该对象变为上下文对象. 例如, 用`with`语句使用`urlopen()`：

```python
from contextlib import closing
from urllib.request import urlopen

with closing(urlopen('https://www.python.org')) as page:
    for line in page:
        print(line)
```

`closing`也是一个经过`@contextmanager`装饰的`generator`, 这个`generator`编写起来其实非常简单：

```python
@contextmanager
def closing(thing):
    try:
        yield thing
    finally:
        thing.close()
```

它的作用就是把任意对象变为上下文对象, 并支持`with`语句. 

`@contextlib`还有一些其他`decorator`, 便于我们编写更简洁的代码. 

>上下文管理器是：
>有一个特殊的语句块, 在执行这个语句块之前需要先执行一些准备动作；当语句块执行完成后, 需要继续执行一些收尾动作. 

### urllib-URL操作

`urllib`提供了一系列用于操作URL的功能. 

#### Get

`urllib`的`request`模块可以非常方便地抓取`URL`内容, 
也就是发送一个`GET`请求到指定的页面, 然后返回`HTTP`的响应：
例如, 对[豆瓣的一个URL][] 进行抓取, 并返回响应：

[豆瓣的一个URL]: https://api.douban.com/v2/book/2129650

```python
from urllib import request

with request.urlopen('https://api.douban.com/v2/book/2129650') as f:
    data = f.read()
    print('Status:', f.status, f.reason)
    for k, v in f.getheaders():
        print('%s: %s' % (k, v))
    print('Data:', data.decode('utf-8'))
```

可以看到`HTTP`响应的头和`JSON`数据：

```python
Status: 200 OK
Server: nginx
Date: Tue, 26 May 2015 10:02:27 GMT
Content-Type: application/json; charset=utf-8
Content-Length: 2049
Connection: close
Expires: Sun, 1 Jan 2006 01:00:00 GMT
Pragma: no-cache
Cache-Control: must-revalidate, no-cache, private
X-DAE-Node: pidl1
Data: {"rating":{"max":10,"numRaters":16,"average":"7.4","min":0},"subtitle":"","author":["廖雪峰编著"],"pubdate":"2007-6",...}
```

如果我们要想模拟浏览器发送`GET`请求, 就需要使用`Request`对象, 通过往`Request`对象添加`HTTP`头, 我们就可以把请求伪装成浏览器. 
例如, 模拟`iPhone 6`去请求豆瓣首页：

```python
from urllib import request

req = request.Request('http://www.douban.com/')
req.add_header('User-Agent', 'Mozilla/6.0 (iPhone; CPU iPhone OS 8_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/8.0 Mobile/10A5376e Safari/8536.25')
with request.urlopen(req) as f:
    print('Status:', f.status, f.reason)
    for k, v in f.getheaders():
        print('%s: %s' % (k, v))
    print('Data:', f.read().decode('utf-8'))
```

这样豆瓣会返回适合`iPhone`的移动版网页：

```python
    <meta name="viewport" content="width=device-width, user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0">
    <meta name="format-detection" content="telephone=no">
    <link rel="apple-touch-icon" sizes="57x57" href="http://img4.douban.com/pics/cardkit/launcher/57.png" />
```

#### Post

如果要以`POST`发送一个请求, 只需要把参数`data`以`bytes`形式传入. 

我们模拟一个微博登录, 先读取登录的邮箱和口令, 
然后按照`weibo.cn`的登录页的格式以`username=xxx&password=xxx`的编码传入：

```python
from urllib import request, parse

print('Login to weibo.cn...')
email = input('Email: ')
passwd = input('Password: ')
login_data = parse.urlencode([
    ('username', email),
    ('password', passwd),
    ('entry', 'mweibo'),
    ('client_id', ''),
    ('savestate', '1'),
    ('ec', ''),
    ('pagerefer', 'https://passport.weibo.cn/signin/welcome?entry=mweibo&r=http%3A%2F%2Fm.weibo.cn%2F')
])

req = request.Request('https://passport.weibo.cn/sso/login')
req.add_header('Origin', 'https://passport.weibo.cn')
req.add_header('User-Agent', 'Mozilla/6.0 (iPhone; CPU iPhone OS 8_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/8.0 Mobile/10A5376e Safari/8536.25')
req.add_header('Referer', 'https://passport.weibo.cn/signin/login?entry=mweibo&res=wel&wm=3349&r=http%3A%2F%2Fm.weibo.cn%2F')

with request.urlopen(req, data=login_data.encode('utf-8')) as f:
    print('Status:', f.status, f.reason)
    for k, v in f.getheaders():
        print('%s: %s' % (k, v))
    print('Data:', f.read().decode('utf-8'))
```

如果登录成功, 我们获得的响应如下：

```python
Status: 200 OK
Server: nginx/1.2.0
...
Set-Cookie: SSOLoginState=1432620126; path=/; domain=weibo.cn
...
Data: {"retcode":20000000,"msg":"","data":{...,"uid":"1658384301"}}
```

如果登录失败, 我们获得的响应如下：

```python
Data: {"retcode":50011015,"msg":"\u7528\u6237\u540d\u6216\u5bc6\u7801\u9519\u8bef","data":{"username":"example@python.org","errline":536}}
```

#### Handler

如果还需要更复杂的控制, 比如通过一个Proxy去访问网站, 我们需要利用`ProxyHandler`来处理, 示例代码如下：

```python
proxy_handler = urllib.request.ProxyHandler({'http': 'http://www.example.com:3128/'})
proxy_auth_handler = urllib.request.ProxyBasicAuthHandler()
proxy_auth_handler.add_password('realm', 'host', 'username', 'password')
opener = urllib.request.build_opener(proxy_handler, proxy_auth_handler)
with opener.open('http://www.example.com/login.html') as f:
    pass
```

#### 小结-urllib

`urllib`提供的功能就是利用程序去执行各种`HTTP`请求. 
如果要模拟浏览器完成特定功能, 需要把请求伪装成浏览器. 
伪装的方法是先监控浏览器发出的请求, 再根据浏览器的请求头来伪装, 
`User-Agent`头就是用来标识浏览器的. 

#### 练习-urllib

利用`urllib`读取`JSON`, 然后将`JSON`解析为`Python`对象：

```python
# -*- coding: utf-8 -*-
from urllib import request
import json

def fetch_data(url):
    with request.urlopen(str(url)) as f:
        data = f.read().decode('utf-8')
        return json.loads(data)


# 测试
URL = 'https://yesno.wtf/api'
data = fetch_data(URL)
print(data)
assert data['answer']== 'yes' and (data['forced']== False)
print('ok')
```

`dumps()`方法返回一个`str`, 内容就是标准的`JSON`. 类似的, `dump()`方法可以直接把`JSON`写入一个file-like Object. 

要把`JSON`反序列化为Python对象, 用`loads()`或者对应的`load()`方法, 前者把`JSON`的字符串反序列化, 后者从`file-like Object`中读取字符串并反序列化：

### XML

`XML`虽然比`JSON`复杂, 在`Web`中应用也不如以前多了, 不过仍有很多地方在用, 所以, 有必要了解如何操作`XML`. 

#### DOM vs SAX

操作`XML`有两种方法：`DOM`和`SAX`. `DOM`会把整个XML读入内存, 解析为树, 因此占用内存大, 解析慢, 优点是可以任意遍历树的节点. `SAX`是流模式, 边读边解析, 占用内存小, 解析快, 缺点是我们需要自己处理事件. 

正常情况下, 优先考虑`SAX`, 因为`DOM`实在太占内存. 

在Python中使用`SAX`解析`XML`非常简洁, 通常我们关心的事件是`start_element`, `end_element`和`char_data`, 准备好这`3`个函数, 然后就可以解析`xml`了. 

举个例子, 当`SAX`解析器读到一个节点时：

```python
<a href="/">python</a>
```

会产生3个事件：

+ `start_element`事件, 在读取`<a href="/">`时；
+ `char_data`事件, 在读取`python`时；
+ `end_element`事件, 在读取`</a>`时. 

用代码实验一下：

```python
from xml.parsers.expat import ParserCreate

class DefaultSaxHandler(object):
    def start_element(self, name, attrs):
        print('sax:start_element: %s, attrs: %s' % (name, str(attrs)))

    def end_element(self, name):
        print('sax:end_element: %s' % name)

    def char_data(self, text):
        print('sax:char_data: %s' % text)

xml = r'''<?xml version="1.0"?>
<ol>
    <li><a href="/python">Python</a></li>
    <li><a href="/ruby">Ruby</a></li>
</ol>
'''

handler = DefaultSaxHandler()
parser = ParserCreate()
parser.StartElementHandler = handler.start_element
parser.EndElementHandler = handler.end_element
parser.CharacterDataHandler = handler.char_data
parser.Parse(xml)
```

需要注意的是读取一大段字符串时, `CharacterDataHandler`可能被多次调用, 所以需要自己保存起来, 在`EndElementHandler`里面再合并. 

除了解析`XML`外, 如何生成`XML`呢？99%的情况下需要生成的`XML`结构都是非常简单的, 因此, 最简单也是最有效的生成`XML`的方法是拼接字符串：

```python
L = []
L.append(r'<?xml version="1.0"?>')
L.append(r'<root>')
L.append(encode('some & data'))
L.append(r'</root>')
return ''.join(L)
```

如果要生成复杂的`XML`呢？建议你不要用`XML`, 改成`JSON`. 

#### 小结-xml

解析`XML`时, 注意找出自己感兴趣的节点, 响应事件时, 把节点数据保存起来. 解析完毕后, 就可以处理数据. 

#### 练习-xml

请利用`SAX`编写程序解析`Yahoo`的`XML`格式的天气预报, 获取天气预报：

`https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20%3D%202151330&format=xml`

参数`woeid`是城市代码, 要查询某个城市代码, 可以在`weather.yahoo.com`搜索城市, 浏览器地址栏的`URL`就包含城市代码. 

```python
# -*- coding:utf-8 -*-

from xml.parsers.expat import ParserCreate
from urllib import request

def parseXml(xml_str):
    print(xml_str)
    return {
        'city': '?',
        'forecast': [
            {
                'date': '2017-11-17',
                'high': 43,
                'low' : 26
            },
            {
                'date': '2017-11-18',
                'high': 41,
                'low' : 20
            },
            {
                'date': '2017-11-19',
                'high': 43,
                'low' : 19
            }
        ]
    }

# 测试:
URL = 'https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20%3D%202151330&format=xml'

with request.urlopen(URL, timeout=4) as f:
    data = f.read()



result = parseXml(data.decode('utf-8'))
assert result['city'] == 'Beijing'
```

#### 总结-2

[看了三遍, 刚开始学xml, 大概有了点想法. ][]

[看了三遍, 刚开始学xml, 大概有了点想法. ]: https://www.liaoxuefeng.com/discuss/969955749132672/1303613376823330

首先, 如果看不明白他给的这个例子且不明白什么是`xml`的话, 可以先百度一下`xml`的格式和语法规则. 

可以看看这个教程：`https://blog.csdn.net/sinat_29830917/article/details/70241786`

我理解的是`ParserCreat()`返回一个`ParserCreat`对象, 他有`StartElementHandler`, `EndElementHandler`, `CharacterDataHandler`三个成员函数, 
但需要特别注意的是：这三个函数均为`CallBack`类型的, 
`ParserCreat`对象在创建时只是会提供三个空指针, 需要我们自己去绑定相应的具体函数. 

>函数调用一般分为两种, 一种是主调, 即编写代码者, 自己调用的函数, 还一种为`Callback`函数, 编码者写好, 但他自己却不主动调用, 而是在某些条件下(编写者并不清楚具体时间和流程), 由其他函数调用.
>简单的比如设备驱动, 操作系统提供了某组设备的函数指针, 比如LCD屏驱动, 由一组画点, 画线, 画块函数组成, 当更换LCD时, 只需要把操作系统开放的函数指针, 指向新的接口即可, 操作系统再需要时, 会自动调用新的驱动函数

我们通过`ParserCreat`对象中的`Parse()`函数传入一个`xml`, 然后就是上文中写到的：

当`SAX`解析器读到一个节点时：

```xml
<a href="/">python</a>
```

会产生3个事件：

+ start_element事件, 在读取`<a href="/">`时；
+ char_data事件, 在读取`python`时；
+ end_element事件, 在读取`</a>`时. 

SAX解析器会帮我们解析好这个`xml`语句, 并且准备好我们写的三个函数, 即`StartElementHandler`, `EndElementHandler`, `CharacterDataHandler`所需要的全部参数, 然后进行调用. 

其中, `start_element`事件中的参数`attrs`指的是标签的属性. 

把代码改一下或许可以舒服些：

```python
from xml.parsers.expat import ParserCreate

def start_element( name, attrs):
    print('sax:start_element: %s, attrs: %s' % (name, str(attrs)))

def end_element( name):
    print('sax:end_element: %s' % name)

def char_data( text):
    print('sax:char_data: %s' % text.replace(' ', '.').replace('\n', r'\n'))

xml = r'''<?xml version="1.0" encoding="utf-8"?>
<!--声明内部dTD-->
<!DOCTYPE students[
    <!ELEMENT students (student*) >
    <!ELEMENT students (name,school) >
    <!ATTLIST student id CDATA #REQUIRED >
    <!ELEMENT name (#PCDATA) >
    <!ELEMENT school (#PCDATA) >
]>
<students>
    <student id = "01">
        <name>CSL</name>
        <school>NUPT</school>
    </student>
    <student id = "02">
        <name>CSLB</name>
        <school>NUPT</school>
    </student>
</students>
'''
parser = ParserCreate()
parser.StartElementHandler = start_element
parser.EndElementHandler = end_element
parser.CharacterDataHandler = char_data
parser.Parse(xml)
```

输出：

```python
sax:start_element: students, attrs: {}
sax:char_data: \n
...
sax:end_element: students
[Finished in 0.2s]
```

### HTMLParser

如果我们要编写一个搜索引擎, 第一步是用爬虫把目标网站的页面抓下来, 第二步就是解析该`HTML`页面, 看看里面的内容到底是新闻, 图片还是视频. 

假设第一步已经完成了, 第二步应该如何解析`HTML`呢？

`HTML`本质上是XML的子集, 但是`HTML`的语法没有XML那么严格, 所以不能用标准的DOM或SAX来解析`HTML`. 

好在Python提供了`HTMLParser`来非常方便地解析`HTML`, 只需简单几行代码：

```python
from html.parser import HTMLParser
from html.entities import name2codepoint

class MyHTMLParser(HTMLParser):

    def handle_starttag(self, tag, attrs):
        print('<%s>' % tag)

    def handle_endtag(self, tag):
        print('</%s>' % tag)

    def handle_startendtag(self, tag, attrs):
        print('<%s/>' % tag)

    def handle_data(self, data):
        print(data)

    def handle_comment(self, data):
        print('<!--', data, '-->')

    def handle_entityref(self, name):
        print('&%s;' % name)

    def handle_charref(self, name):
        print('&#%s;' % name)

parser = MyHTMLParser()
parser.feed('''<html>
<head></head>
<body>
<!-- test html parser -->
    <p>Some <a href=\"#\">html</a> HTML&nbsp;tutorial...<br>END</p>
</body></html>''')
```

`feed()`方法可以多次调用, 也就是不一定一次把整个`HTML`字符串都塞进去, 可以一部分一部分塞进去. 

特殊字符有两种, 一种是英文表示的`&nbsp`(&nbsp);, 一种是数字表示的`&#1234`(&#1234);, 这两种字符都可以通过Parser解析出来. 

#### 小结-htmlparser

利用`HTMLParser`, 可以把网页中的文本, 图像等解析出来. 

#### 练习-htmlparser

找一个网页, 例如`https://www.python.org/events/python-events/`, 
用浏览器查看源码并复制, 然后尝试解析一下HTML, 
输出Python官网发布的会议时间, 名称和地点. 
