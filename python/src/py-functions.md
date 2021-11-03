# python-2.md

ref: [这是小白的Python新手教程](https://www.liaoxuefeng.com/wiki/1016959663602400)

## 函数式编程

函数是Python内建支持的一种封装, 我们通过把大段代码拆成函数, 通过一层一层的函数调用, 就可以把复杂任务分解成简单的任务, 这种分解可以称之为面向过程的程序设计. 函数就是面向过程的程序设计的基本单元.

而函数式编程(请注意多了一个"式"字)——Functional Programming,
虽然也可以归结到面向过程的程序设计, 但其思想更接近数学计算.

我们首先要搞明白计算机(Computer)和计算(Compute)的概念.

在计算机的层次上, CPU执行的是加减乘除的指令代码, 以及各种条件判断和跳转指令, 所以, 汇编语言是最贴近计算机的语言.

而计算则指数学意义上的计算, 越是抽象的计算, 离计算机硬件越远.

对应到编程语言, 就是越低级的语言, 越贴近计算机, 抽象程度低, 执行效率高, 比如C语言; 越高级的语言, 越贴近计算, 抽象程度高, 执行效率低, 比如Lisp语言.

函数式编程就是一种抽象程度很高的编程范式, 纯粹的函数式编程语言编写的函数没有变量, 因此, 任意一个函数, 只要输入是确定的, 输出就是确定的, 这种纯函数我们称之为没有副作用. 而允许使用变量的程序设计语言, 由于函数内部的变量状态不确定, 同样的输入, 可能得到不同的输出, 因此, 这种函数是有副作用的.

函数式编程的一个特点就是, 允许把函数本身作为参数传入另一个函数, 还允许返回一个函数!

Python对函数式编程提供部分支持.
由于Python允许使用变量, 因此, Python不是纯函数式编程语言.

### 高阶函数

高阶函数英文叫`Higher-order function`.
什么是高阶函数? 我们以实际代码为例子, 一步一步深入概念.

#### 变量可以指向函数

以Python内置的求绝对值的函数`abs()`为例, 调用该函数用以下代码:

```python
>>> abs(-10)
10
```

但是, 如果只写`abs`呢?

```python
>>> abs
<built-in function abs>
```

可见, `abs(-10)`是函数调用, 而`abs`是函数本身.

要获得函数调用结果, 我们可以把结果赋值给变量:

```python
>>> x = abs(-10)
>>> x
10
```

但是, 如果把函数本身赋值给变量呢?

```python
>>> f = abs
>>> f
<built-in function abs>
```

结论: 函数本身也可以赋值给变量, 即: 变量可以指向函数.

如果一个变量指向了一个函数, 那么, 可否通过该变量来调用这个函数? 用代码验证一下:

```python
>>> f = abs
>>> f(-10)
10
```

成功! 说明变量f现在已经指向了`abs`函数本身.
直接调用`abs()`函数和调用变量`f()`完全相同.

#### 函数名也是变量

那么函数名是什么呢? 函数名其实就是指向函数的变量!
对于`abs()`这个函数, 完全可以把函数名`abs`看成变量,
它指向一个可以计算绝对值的函数!

如果把`abs`指向其他对象, 会有什么情况发生?

```python
>>> abs = 10
>>> abs(-10)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'int' object is not callable
```

把`abs`指向`10`后, 就无法通过`abs(-10)`调用该函数了!
因为`abs`这个变量已经不指向求绝对值函数而是指向一个整数`10`!

当然实际代码绝对不能这么写, 这里是为了说明函数名也是变量.
要恢复`abs`函数, 请重启Python交互环境.

注: 由于`abs`函数实际上是定义在`import builtins`模块中的,
所以要让修改`abs`变量的指向在其它模块也生效, 要用`import builtins; builtins.abs = 10`.

#### 传入函数

既然变量可以指向函数, 函数的参数能接收变量,
那么一个函数就可以接收另一个函数作为参数, 这种函数就称之为高阶函数.

一个最简单的高阶函数:

```python
def add(x, y, f):
    return f(x) + f(y)
```

当我们调用`add(-5, 6, abs)`时,
参数`x`, `y`和f分别接收`-5`, `6`和`abs`,
根据函数定义, 我们可以推导计算过程为:

```python
x = -5
y = 6
f = abs
f(x) + f(y) ==> abs(-5) + abs(6) ==> 11
return 11
```

用代码验证一下:

```python
# -*- coding: utf-8 -*-

def add(x, y, f):
    return f(x) + f(y)

print(add(-5, 6, abs))
```

编写高阶函数, 就是让函数的参数能够接收别的函数.

#### 小结-高阶函数

把函数作为参数传入, 这样的函数称为高阶函数,
函数式编程就是指这种高度抽象的编程范式.

### map/reduce

Python内建了`map()`和`reduce()`函数.

如果你读过Google的那篇大名鼎鼎的论文"[MapReduce: Simplified Data Processing on Large Clusters][]", 你就能大概明白`map/reduce`的概念.

[MapReduce: Simplified Data Processing on Large Clusters]: research.google.com/archive/mapreduce.html

#### map

我们先看map. `map()`函数接收两个参数, 一个是函数, 一个是`Iterable`,
map将传入的函数依次作用到序列的每个元素, 并把结果作为新的`Iterator`返回.

举例说明, 比如我们有一个函数`f(x)=x2`,
要把这个函数作用在一个`list [1, 2, 3, 4, 5, 6, 7, 8, 9]`上,
就可以用`map()`实现,
我们用Python代码实现:

```python
>>> def f(x):
...     return x * x
...
>>> r = map(f, [1, 2, 3, 4, 5, 6, 7, 8, 9])
>>> list(r)
[1, 4, 9, 16, 25, 36, 49, 64, 81]
```

`map()`传入的第一个参数是`f`, 即函数对象本身. 由于结果`r`是一个`Iterator`, `Iterator`是惰性序列, 因此通过`list()`函数让它把整个序列都计算出来并返回一个`list`.

你可能会想, 不需要`map()`函数, 写一个循环, 也可以计算出结果:

```python
L = []
for n in [1, 2, 3, 4, 5, 6, 7, 8, 9]:
    L.append(f(n))
print(L)
```

的确可以, 但是, 从上面的循环代码, 能一眼看明白"把f(x)作用在list的每一个元素并把结果生成一个新的list"吗?

所以, `map()`作为高阶函数, 事实上它把运算规则抽象了, 因此, 我们不但可以计算简单的`f(x)=x2`, 还可以计算任意复杂的函数, 比如, 把这个`list`所有数字转为字符串:

```python
>>> list(map(str, [1, 2, 3, 4, 5, 6, 7, 8, 9]))
['1', '2', '3', '4', '5', '6', '7', '8', '9']
```

只需要一行代码.

#### reduce

再看`reduce`的用法. `reduce`把一个函数作用在一个序列`[x1, x2, x3, ...]`上, 这个函数必须接收两个参数,
`reduce`把结果继续和序列的下一个元素做累积计算, 其效果就是:

```python
reduce(f, [x1, x2, x3, x4]) = f(f(f(x1, x2), x3), x4)
```

比方说对一个序列求和, 就可以用`reduce`实现:

```python
>>> from functools import reduce
>>> def add(x, y):
...     return x + y
...
>>> reduce(add, [1, 3, 5, 7, 9])
25
```

当然求和运算可以直接用Python内建函数`sum()`, 没必要动用`reduce`.
但是如果要把序列`[1, 3, 5, 7, 9]`变换成整数`13579`, `reduce`就可以派上用场:

```python
>>> from functools import reduce
>>> def fn(x, y):
...     return x * 10 + y
...
>>> reduce(fn, [1, 3, 5, 7, 9])
13579
```

这个例子本身没多大用处, 但是, 如果考虑到字符串`str`也是一个序列, 对上面的例子稍加改动, 配合`map()`, 我们就可以写出把`str`转换为`int`的函数:

```python
>>> from functools import reduce
>>> def fn(x, y):
...     return x * 10 + y
...
>>> def char2num(s):
...     digits = {'0': 0, '1': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9}
...     return digits[s]
...
>>> reduce(fn, map(char2num, '13579'))
13579
```

整理成一个`str2int`的函数就是:

```python
from functools import reduce

DIGITS = {'0': 0, '1': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9}

def str2int(s):
    def fn(x, y):
        return x * 10 + y
    def char2num(s):
        return DIGITS[s]
    return reduce(fn, map(char2num, s))
```

还可以用`lambda`函数进一步简化成:

```python
from functools import reduce

DIGITS = {'0': 0, '1': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9}

def char2num(s):
    return DIGITS[s]

def str2int(s):
    return reduce(lambda x, y: x * 10 + y, map(char2num, s))
```

也就是说, 假设Python没有提供`int()`函数, 你完全可以自己写一个把字符串转化为整数的函数, 而且只需要几行代码!

`lambda`函数的用法在后面介绍.

#### 练习-map-reduce-1

利用`map()`函数, 把用户输入的不规范的英文名字, 变为首字母大写,
其他小写的规范名字.
输入: `['adam', 'LISA', 'barT']`, 输出: `['Adam', 'Lisa', 'Bart']`:

```python
# -*- coding: utf-8 -*-
def normalize(name):
    return s=s[0].upper()+s[1:].lower()

 # 测试:
L1 = ['adam', 'LISA', 'barT']
L2 = list(map(normalize, L1))
print(L2)
```

#### 练习-map-reduce-2

Python提供的`sum()`函数可以接受一个list并求和,
请编写一个`prod()`函数, 可以接受一个list并利用`reduce()`求积:

```python
# -*- coding: utf-8 -*-
from functools import reduce
def prod(L):
    def times(a,b):
        return a*b
    return reduce(times,L)

print('3 * 5 * 7 * 9 =', prod([3, 5, 7, 9]))
if prod([3, 5, 7, 9]) == 945:
    print('测试成功!')
else:
    print('测试失败!')
```

#### 练习-map-reduce-3

利用map和reduce编写一个`str2float`函数, 把字符串`'123.456'`转换成浮点数`123.456`:

```python
# -*- coding: utf-8 -*-
from functools import reduce
import math

def str2float(s):
    dgdict= {'0': 0, '1': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9}
    dot=len(s)-s.index('.')-1
    new_s=s.replace('.','')
    def fn(x, y):
        return x * 10 + y
    def char2num(s2):
        return dgdict[s2]
    s_int=reduce(fn, map(char2num, new_s))
    return s_int/(math.pow(10,dot))

# 测试
print('str2float(\'123.456\') =', str2float('123.456'))
if abs(str2float('123.456') - 123.456) < 0.00001:
    print('测试成功!')
else:
    print('测试失败!')
```

### filter

Python内建的`filter()`函数用于过滤序列.

和`map()`类似, `filter()`也接收一个函数和一个序列.
和map()不同的是, `filter()`把传入的函数依次作用于每个元素, 然后根据返回值是`True`还是`False`决定保留还是丢弃该元素.

`''` 空字符串被视为`False`

例如, 在一个list中, 删掉偶数, 只保留奇数, 可以这么写:

```python
def is_odd(n):
    return n % 2 == 1

list(filter(is_odd, [1, 2, 4, 5, 6, 9, 10, 15]))
# 结果: [1, 5, 9, 15]
```

把一个序列中的空字符串删掉, 可以这么写:

```python
def not_empty(s):
    return s and s.strip()

list(filter(not_empty, ['A', '', 'B', None, 'C', '  ']))
# 结果: ['A', 'B', 'C']
```

可见用`filter()`这个高阶函数, 关键在于正确实现一个"筛选"函数.

注意到`filter()`函数返回的是一个`Iterator`, 也就是一个惰性序列, 所以要强迫`filter()`完成计算结果, 需要用`list()`函数获得所有结果并返回list.
用filter求素数

计算素数的一个方法是埃氏筛法, 它的算法理解起来非常简单:

首先, 列出从2开始的所有自然数, 构造一个序列:
2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...

取序列的第一个数2, 它一定是素数, 然后用2把序列的2的倍数筛掉:
3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...

取新序列的第一个数3, 它一定是素数, 然后用3把序列的3的倍数筛掉:
5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...

不断筛下去, 就可以得到所有的素数.

用Python来实现这个算法, 可以先构造一个从3开始的奇数序列:

```python
def _odd_iter():
    n = 1
    while True:
        n = n + 2
        yield n
```

注意这是一个生成器, 并且是一个无限序列.

然后定义一个筛选函数:

```python
def _not_divisible(n):
    return lambda x: x % n > 0
```

lambda 表达式 即匿名函数

最后, 定义一个生成器, 不断返回下一个素数:

```python
def primes():
    yield 2
    it = _odd_iter() # 初始序列
    while True:
        n = next(it) # 返回序列的第一个数
        yield n
        it = filter(_not_divisible(n), it) # 构造新序列
```

这个生成器先返回第一个素数`2`, 然后, 利用`filter()`不断产生筛选后的新的序列.

由于`primes()`也是一个无限序列, 所以调用时需要设置一个退出循环的条件:

```python
# 打印1000以内的素数:
for n in primes():
    if n < 1000:
        print(n)
    else:
        break
```

注意到`Iterator`是惰性计算的序列, 所以我们可以用`Python`表示"全体自然数",
"全体素数"这样的序列, 而代码非常简洁.

练习-filter

回数是指从左向右读和从右向左读都是一样的数, 例如12321, 909. 请利用filter()筛选出回数:

```python
# -*- coding: utf-8 -*-

def is_palindrome(n):
    n_rev=''
    for i in str(n):
         n_rev=i+n_rev
    return str(n) == n_rev

# 测试:
output = filter(is_palindrome, range(1, 1000))
print('1~1000:', list(output))
if list(filter(is_palindrome, range(1, 200))) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 22, 33, 44, 55, 66, 77, 88, 99, 101, 111, 121, 131, 141, 151, 161, 171, 181, 191]:
    print('测试成功!')
else:
    print('测试失败!')
```

也可以用 字符 切片功能, 把间距取为`-1`, 会比较方便

```python
def is_palindrome(n):
    L=list(str(n))
    return L==L[::-1]
```

### sorted

排序算法

排序也是在程序中经常用到的算法. 无论使用冒泡排序还是快速排序, 排序的核心是比较两个元素的大小. 如果是数字, 我们可以直接比较, 但如果是字符串或者两个dict呢? 直接比较数学上的大小是没有意义的, 因此, 比较的过程必须通过函数抽象出来.

Python内置的`sorted()`函数就可以对list进行排序:

```python
>>> sorted([36, 5, -12, 9, -21])
[-21, -12, 5, 9, 36]
```

此外, `sorted()`函数也是一个高阶函数,
它还可以接收一个`key`函数来实现自定义的排序, 例如按绝对值大小排序:

```python
>>> sorted([36, 5, -12, 9, -21], key=abs)
[5, 9, -12, -21, 36]
```

`key`指定的函数将作用于`list`的每一个元素上,
并根据`key`函数返回的结果进行排序.
对比原始的`list`和经过`key=abs`处理过的`list`:

```python
list = [36, 5, -12, 9, -21]
keys = [36, 5,  12, 9,  21]
```

然后`sorted()`函数按照`keys`进行排序, 并按照对应关系返回list相应的元素:

我们再看一个字符串排序的例子:

```python
>>> sorted(['bob', 'about', 'Zoo', 'Credit'])
['Credit', 'Zoo', 'about', 'bob']
```

默认情况下, 对字符串排序, 是按照`ASCII`的大小比较的, 由于`'Z'< 'a'`, 结果, 大写字母Z会排在小写字母a的前面.

现在, 我们提出排序应该忽略大小写, 按照字母序排序. 要实现这个算法, 不必对现有代码大加改动, 只要我们能用一个key函数把字符串映射为忽略大小写排序即可. 忽略大小写来比较两个字符串, 实际上就是先把字符串都变成大写(或者都变成小写), 再比较.

这样, 我们给`sorted`传入`key`函数, 即可实现忽略大小写的排序:

```python
>>> sorted(['bob', 'about', 'Zoo', 'Credit'], key=str.lower)
['about', 'bob', 'Credit', 'Zoo']
```

要进行反向排序, 不必改动`key`函数, 可以传入第三个参数`reverse=True`:

```python
>>> sorted(
['bob', 'about', 'Zoo', 'Credit'],
key=str.lower,
reverse=True
)
['Zoo', 'Credit', 'bob', 'about']
```

从上述例子可以看出, 高阶函数的抽象能力是非常强大的, 而且, 核心代码可以保持得非常简洁.

#### 小结-sorted

`sorted()`也是一个高阶函数. 用`sorted()`排序的关键在于实现一个映射函数.

#### 练习-sorted

假设我们用一组`tuple`表示学生名字和成绩:

```python
L = [('Bob', 75), ('Adam', 92), ('Bart', 66), ('Lisa', 88)]
```

请用`sorted()`对上述列表分别按名字排序:

```python
# -*- coding: utf-8 -*-
L = [('Bob', 75), ('Adam', 92), ('Bart', 66), ('Lisa', 88)]

def by_name(t):
    return t[0]

L2 = sorted(L, key=by_name)
print(L2)
```

再按成绩从高到低排序:

```python
# -*- coding: utf-8 -*-

L = [('Bob', 75), ('Adam', 92), ('Bart', 66), ('Lisa', 88)]

def by_score(t):
    return t[1]

L2 = sorted(L, key=by_score)
print(L2)
```

### 返回函数

#### 函数作为返回值

高阶函数除了可以接受函数作为参数外, 还可以把函数作为结果值返回.

我们来实现一个可变参数的求和. 通常情况下, 求和的函数是这样定义的:

```python
def calc_sum(*args):
    ax = 0
    for n in args:
        ax = ax + n
    return ax
```

但是, 如果不需要立刻求和, 而是在后面的代码中, 根据需要再计算怎么办? 可以不返回求和的结果, 而是返回求和的函数:

```python
def lazy_sum(*args):
    def sum():
        ax = 0
        for n in args:
            ax = ax + n
        return ax
    return sum
```

对于`sum()`这个函数, 完全可以把函数名`sum`看成变量,
它指向一个可以计算累加的函数.

当我们调用`lazy_sum()`时, 返回的并不是求和结果, 而是求和函数:

```python
>>> f = lazy_sum(1, 3, 5, 7, 9)
>>> f
<function lazy_sum.<locals>.sum at 0x101c6ed90>
```

调用函数f时, 才真正计算求和的结果:

```python
>>> f()
25
```

在这个例子中, 我们在函数`lazy_sum`中又定义了函数`sum`,
并且, 内部函数`sum`可以引用外部函数`lazy_sum`的参数和局部变量,
当`lazy_sum`返回函数`sum`时, 相关参数和变量都保存在返回的函数中,
这种称为"闭包(`Closure`)"的程序结构拥有极大的威力.

请再注意一点, 当我们调用`lazy_sum()`时, **每次调用都会返回一个新的函数**,
即使传入相同的参数:

```python
>>> f1 = lazy_sum(1, 3, 5, 7, 9)
>>> f2 = lazy_sum(1, 3, 5, 7, 9)
>>> f1==f2
False
```

`f1()`和`f2()`的调用结果互不影响.

#### 闭包

注意到返回的函数在其定义内部引用了局部变量`args`, 所以, 当一个函数返回了一个函数后, 其内部的局部变量还被新函数引用, 所以, 闭包用起来简单, 实现起来可不容易.

另一个需要注意的问题是, 返回的函数并没有立刻执行, 而是直到调用了`f()`才执行. 我们来看一个例子:

```python
def count():
    fs = []
    for i in range(1, 4):
        def f():
             return i*i
        fs.append(f)
    return fs

f1, f2, f3 = count()
```

在上面的例子中, 每次循环, 都创建了一个新的函数, 然后, 把创建的`3`个函数都返回了.
你可能认为调用`f1()`, `f2()`和`f3()`结果应该是`1`, `4`, `9`, 但实际结果是:

```python
>>> f1()
9
>>> f2()
9
>>> f3()
9
```

全部都是`9`! 原因就在于返回的函数引用了变量`i`, 但它并非立刻执行.
等到`3`个函数都返回时, 它们所引用的变量i已经变成了`3`, 因此最终结果为`9`.

>返回闭包时牢记一点: 返回函数不要引用任何循环变量, 或者后续会发生变化的变量.

如果一定要引用循环变量怎么办? 方法是再创建一个函数, 用该函数的参数绑定循环变量当前的值, 无论该循环变量后续如何更改, 已绑定到函数参数的值不变:

```python
def count():
    def f(j):
        def g():
            return j*j
        return g
    fs = []
    for i in range(1, 4):
        fs.append(f(i)) # f(i)立刻被执行, 因此i的当前值被传入f()
    return fs
```

再看看结果:

```python
>>> f1, f2, f3 = count()
>>> f1()
1
>>> f2()
4
>>> f3()
9
```

缺点是代码较长, 可利用`lambda`函数缩短代码.

#### 练习-返回函数

利用闭包返回一个计数器函数, 每次调用它返回递增整数:

```python
# -*- coding: utf-8 -*-

def createCounter():
    def counter():
        return 1
    return counter
```

```python
# -*- coding: utf-8 -*-

def createCounter():
    num=(x for x in range(1,1000,1))
    def counter():
        return next(num)
    return counter

# 测试:
counterA = createCounter()
print(counterA(), counterA(), counterA(), counterA(), counterA()) # 1 2 3 4 5
counterB = createCounter()
if [counterB(), counterB(), counterB(), counterB()] == [1, 2, 3, 4]:
    print('测试通过!')
else:
    print('测试失败!')
```

### 命名空间

ref: [Python LEGB规则][]

[Python LEGB规则]: https://www.jianshu.com/p/3b72ba5a209c

```python
#!/usr/bin/env python
# encoding: utf-8

x = 1

def foo():
    x = 2
    def innerfoo():
        x = 3                  #此处改动: 注释掉
        print 'locals ', x
    innerfoo()
    print 'enclosing function locals ', x

foo()
print 'global ', x
```

在上述两个例子中, 从内到外, 依次形成四个命名空间:

+ `def innerfoo():`: Local,  即函数内部命名空间;
+ `def foo():`: Enclosing function locals; 外部嵌套函数的名字空间
+ `module(文件本身)`: Global(module); 函数定义所在模块(文件)的名字空间
+ `Python内置模块的名字空间`: Builtin

`x = 3` 属于函数内部命名空间, 当被注释掉之后, 函数`innerfoo`内部通过`print x` 使用`x`这个名称时, 触发了名称查找动作.
首先在`Local`命名空间查找, 没有找到, 然后到`Enclosing function locals`命名空间查找, 查找成功, 然后调用.

>如果函数接收到的是一个可变对象(dict, list), 就能修改对象的原始值,
>如果是不可变对象(num, str, tuple), 则不能直接修改对象

### 匿名函数

当我们在传入函数时, 有些时候, 不需要显式地定义函数, 直接传入匿名函数更方便.

在Python中, 对匿名函数提供了有限支持. 还是以`map()`函数为例,
计算`f(x)=x2`时, 除了定义一个`f(x)`的函数外, 还可以直接传入匿名函数:

```python
>>> list(map(lambda x: x * x, [1, 2, 3, 4, 5, 6, 7, 8, 9]))
[1, 4, 9, 16, 25, 36, 49, 64, 81]
```

通过对比可以看出, 匿名函数`lambda x: x * x`实际上就是:

```python
def f(x):
    return x * x
```

关键字`lambda`表示匿名函数, 冒号前面的`x`表示函数参数.

匿名函数有个限制, 就是只能有一个表达式, 不用写`return`, 返回值就是该表达式的结果.

用匿名函数有个好处, 因为函数没有名字, 不必担心函数名冲突.
此外, 匿名函数也是一个函数对象, 也可以把匿名函数赋值给一个变量,
再利用变量来调用该函数:

```python
>>> f = lambda x: x * x
>>> f
<function <lambda> at 0x101c6ef28>
>>> f(5)
25
```

同样, 也可以把匿名函数作为返回值返回, 比如:

```python
def build(x, y):
    return lambda: x * x + y * y
```

#### 练习-匿名函数

请用匿名函数改造下面的代码:

```python
# -*- coding: utf-8 -*-

def is_odd(n):
    return n % 2 == 1
L = list(filter(is_odd, range(1, 20)))
print(L)
```

```python
# -*- coding: utf-8 -*-
L = list(filter(lambda n : n%2==1, range(1, 20)))
print(L)
```

#### 小结-匿名函数

Python对匿名函数的支持有限, 只有一些简单的情况下可以使用匿名函数.

### 装饰器

由于函数也是一个对象, 而且函数对象可以被赋值给变量,
所以, 通过变量也能调用该函数.

```python
>>> def now():
...     print('2015-3-25')
...
>>> f = now
>>> f()
2015-3-25
```

函数对象有一个`__name__`属性, 可以拿到函数的名字:

```python
>>> now.__name__
'now'
>>> f.__name__
'now'
```

现在, 假设我们要增强`now()`函数的功能,
比如, 在函数调用前后自动打印日志, 但又不希望修改`now()`函数的定义, 这种在代码运行期间动态增加功能的方式, 称之为"装饰器"(Decorator).

本质上, `decorator`就是一个返回 函数 的高阶函数.
所以, 我们要定义一个能打印日志的`decorator`, 可以定义如下:

```python
def log(func):
    def wrapper(*args, **kw):
        print('call %s():' % func.__name__)
        return func(*args, **kw)
    return wrapper
```

观察上面的log, 因为它是一个decorator, 所以接受一个函数作为参数,
并返回一个函数. 我们要借助Python的`@`语法, 把decorator置于函数的定义处:

```python
@log
def now():
    print('2015-3-25')
```

调用`now()`函数, 不仅会运行`now()`函数本身, 还会在运行`now()`函数前打印一行日志:

```python
>>> now()
call now():
2015-3-25
```

把`@log`放到`now()`函数的定义处, 相当于执行了语句:

```python
now = log(now)
```

由于`log()`是一个decorator, 返回一个函数,
所以, 原来的`now()`函数仍然存在, 只是现在同名的`now`变量指向了新的函数,
于是调用`now()`将执行新函数, 即在`log()`函数中返回的`wrapper()`函数.

`wrapper()`函数的参数定义是`(*args, **kw)`, 因此, `wrapper()`函数可以接受任意参数的调用. 在`wrapper()`函数内, 首先打印日志, 再紧接着调用原始函数.

如果`decorator`本身需要传入参数, 那就需要编写一个返回`decorator`的高阶函数, 写出来会更复杂. 比如, 要自定义`log`的文本:

```python
def log(text):
    def decorator(func):
        def wrapper(*args, **kw):
            print('%s %s():' % (text, func.__name__))
            return func(*args, **kw)
        return wrapper
    return decorator
```

这个3层嵌套的decorator用法如下:

```python
@log('execute')
def now():
    print('2015-3-25')
```

执行结果如下:

```python
>>> now()
execute now():
2015-3-25
```

和两层嵌套的decorator相比, 3层嵌套的效果是这样的:

```python
>>> now = log('execute')(now)
```

我们来剖析上面的语句, 首先执行`log('execute')`, 返回的是decorator函数,
再调用返回的函数, 参数是`now`函数, 返回值最终是`wrapper`函数.

以上两种decorator的定义都没有问题, 但还差最后一步.
因为我们讲了函数也是对象, 它有`__name__`等属性,
但你去看经过`decorator`装饰之后的函数, 它们的`__name__`已经从原来的`'now'`变成了`'wrapper'`:

```python
>>> now.__name__
'wrapper'
```

因为返回的那个`wrapper()`函数名字就是`'wrapper'`, 所以, 需要把原始函数的`__name__`等属性复制到`wrapper()`函数中, 否则, 有些依赖函数签名的代码执行就会出错.

不需要编写`wrapper.__name__ = func.__name__`这样的代码, Python内置的`functools.wraps`就是干这个事的, 所以, 一个完整的`decorator`的写法如下:

```python
import functools

def log(func):
    @functools.wraps(func)
    def wrapper(*args, **kw):
        print('call %s():' % func.__name__)
        return func(*args, **kw)
    return wrapper
```

或者针对带参数的`decorator`:

```python
import functools

def log(text):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kw):
            print('%s %s():' % (text, func.__name__))
            return func(*args, **kw)
        return wrapper
    return decorator
```

`import functools`是导入`functools`模块. 模块的概念稍候讲解.
现在, 只需记住在定义`wrapper()`的前面加上`@functools.wraps(func)`即可.

#### 练习-装饰器

请设计一个decorator, 它可作用于任何函数上, 并打印该函数的执行时间:

```python
def metric(fn):
    print('%s executed in %s ms' % (fn.__name__, 10.24))
    return fn
```

```python
# -*- coding: utf-8 -*-
import time, functools

def metric(fn):
    @functools.wraps(fn)
    def wrapper(*args, **kw):
        start_time = time.time() # 记录程序开始运行时间
        fn(*args, **kw)
        end_time = time.time()  # 记录程序结束运行时间
        print('%s executed in %s ms' % (fn.__name__,
        (end_time - start_time)))
        return fn(*args, **kw)
    return wrapper

# 测试
@metric
def fast(x, y):
    time.sleep(0.0012)
    return x + y;

@metric
def slow(x, y, z):
    time.sleep(0.1234)
    return x * y * z;

f = fast(11, 22)
s = slow(11, 22, 33)

if f != 33:
    print('测试失败!')
elif s != 7986:
    print('测试失败!')
```

#### 小结-装饰器

在面向对象(OOP)的设计模式中, decorator被称为装饰模式.
OOP的装饰模式需要通过继承和组合来实现, 而Python除了能支持OOP的decorator外, 直接从语法层次支持decorator.
Python的decorator可以用函数实现, 也可以用类实现.

decorator可以增强函数的功能, 定义起来虽然有点复杂, 但使用起来非常灵活和方便.

请编写一个decorator, 能在函数调用的前后打印出'begin call'和'end call'的日志.

```python
import functools

def log(func):
    @functools.wraps(func)
    def wrapper(*args, **kw):
        print('begin call %s():' % func.__name__)
        func(*args, **kw)
        print('end call %s():' % func.__name__)
        return None
    return wrapper
```

再思考一下能否写出一个`@log`的decorator, 使它既支持:

```python
@log
def f():
    pass
```

又支持:

```python
@log('execute')
def f():
    pass
```

### 偏函数

Python的`functools`模块提供了很多有用的功能, 其中一个就是偏函数(`Partial function`). 要注意, 这里的偏函数和数学意义上的偏函数不一样.

在介绍函数参数的时候, 我们讲到, 通过设定参数的默认值, 可以降低函数调用的难度. 而偏函数也可以做到这一点. 举例如下:

`int()`函数可以把字符串转换为整数, 当仅传入字符串时, `int()`函数默认按十进制转换:

```python
>>> int('12345')
12345
```

但`int()`函数还提供额外的`base`参数, 默认值为`10`. 如果传入`base`参数, 就可以做`N`进制的转换:

```python
>>> int('12345', base=8)
5349
>>> int('12345', 16)
74565
```

假设要转换大量的二进制字符串, 每次都传入`int(x, base=2)`非常麻烦,
于是, 我们想到, 可以定义一个`int2()`的函数, 默认把`base=2`传进去:

```python
def int2(x, base=2):
    return int(x, base)
```

这样, 我们转换二进制就非常方便了:

```python
>>> int2('1000000')
64
>>> int2('1010101')
85
```

`functools.partial`就是帮助我们创建一个偏函数的,
不需要我们自己定义`int2()`, 可以直接使用下面的代码创建一个新的函数`int2`:

```python
>>> import functools
>>> int2 = functools.partial(int, base=2)
>>> int2('1000000')
64
>>> int2('1010101')
85
```

所以, 简单总结`functools.partial`的作用就是, 把一个函数的某些参数给固定住(也就是设置默认值), 返回一个新的函数, 调用这个新函数会更简单.

注意到上面的新的`int2`函数, 仅仅是把`base`参数重新设定默认值为`2`, 但也可以在函数调用时传入其他值:

```python
>>> int2('1000000', base=10)
1000000
```

最后, 创建偏函数时, 实际上可以接收`函数对象`, `*args`和`**kw`这`3`个参数, 当传入:

```python
int2 = functools.partial(int, base=2)
```

实际上固定了`int()`函数的**关键字参数**`base`, 也就是:

```python
int2('10010')
```

相当于:

```python
kw = { 'base': 2 }
int('10010', **kw)
```

当传入:

```python
max2 = functools.partial(max, 10)
```

实际上会把`10`作为`*args`的一部分自动加到左边, 也就是:

```python
max2(5, 6, 7)
```

相当于:

```python
args = (10, 5, 6, 7)
max(*args)
```

结果为`10`.

#### 小结-偏函数

当函数的参数个数太多, 需要简化时, 使用`functools.partial`可以创建一个新的函数, 这个新函数可以固定住原函数的部分参数, 从而在调用时更简单.

## 模块

在计算机程序的开发过程中, 随着程序代码越写越多, 在一个文件里代码就会越来越长, 越来越不容易维护.

为了编写可维护的代码, 我们把很多函数分组, 分别放到不同的文件里, 这样, 每个文件包含的代码就相对较少, 很多编程语言都采用这种组织代码的方式. 在Python中, 一个`.py`文件就称之为一个模块(Module).

使用模块有什么好处?

最大的好处是大大提高了代码的可维护性. 其次, 编写代码不必从零开始. 当一个模块编写完毕, 就可以被其他地方引用. 我们在编写程序的时候, 也经常引用其他模块, 包括Python内置的模块和来自第三方的模块.

使用模块还可以避免函数名和变量名冲突. 相同名字的函数和变量完全可以分别存在不同的模块中, 因此, 我们自己在编写模块时, 不必考虑名字会与其他模块冲突. 但是也要注意, 尽量不要与内置函数名字冲突. 点这里查看Python的所有内置函数.

你也许还想到, 如果不同的人编写的模块名相同怎么办? 为了避免模块名冲突, Python又引入了按目录来组织模块的方法, 称为包(Package).

举个例子, 一个`abc.py`的文件就是一个名字叫`abc`的模块, 一个`xyz.py`的文件就是一个名字叫`xyz`的模块.

现在, 假设我们的abc和xyz这两个模块名字与其他模块冲突了, 于是我们可以通过包来组织模块, 避免冲突. 方法是选择一个顶层包名, 比如mycompany, 按照如下目录存放:

```python
mycompany
├─ __init__.py
├─ abc.py
└─ xyz.py
```

引入了包以后, 只要顶层的包名不与别人冲突, 那所有模块都不会与别人冲突. 现在, `abc.py`模块的名字就变成了`mycompany.abc`, 类似的, `xyz.py`的模块名变成了`mycompany.xyz`.

请注意, 每一个包目录下面都会有一个`__init__.py`的文件, 这个文件是必须存在的, 否则, Python就把这个目录当成普通目录, 而不是一个包. `__init__.py`可以是空文件, 也可以有Python代码, 因为`__init__.py`本身就是一个模块, 而它的模块名就是`mycompany`.

类似的, 可以有多级目录, 组成多级层次的包结构. 比如如下的目录结构:

```python
mycompany
 ├─ web
 │  ├─ __init__.py
 │  ├─ utils.py
 │  └─ www.py
 ├─ __init__.py
 ├─ abc.py
 └─ utils.py
```

 文件`www.py`的模块名就是`mycompany.web.www`, 两个文件`utils.py`的模块名分别是`mycompany.utils`和`mycompany.web.utils`.

>自己创建模块时要注意命名, 不能和Python自带的模块名称冲突. 例如, 系统自带了`sys`模块, 自己的模块就不可命名为`sys.py`, 否则将无法导入系统自带的sys模块.

`mycompany.web`也是一个模块, 请指出该模块对应的`.py`文件.

总结

模块是一组Python代码的集合, 可以使用其他模块, 也可以被其他模块使用.

创建自己的模块时, 要注意:

+ 模块名要遵循Python变量命名规范, 不要使用中文, 特殊字符;
+ 模块名不要和系统模块名冲突, 最好先查看系统是否已存在该模块, 检查方法是在Python交互环境执行`import abc`, 若成功则说明系统存在此模块.

### 使用模块

Python本身就内置了很多非常有用的模块, 只要安装完毕, 这些模块就可以立刻使用.

我们以内建的`sys`模块为例, 编写一个`hello`的模块:

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

' a test module '

__author__ = 'Michael Liao'

import sys

def test():
    args = sys.argv
    if len(args)==1:
        print('Hello, world!')
    elif len(args)==2:
        print('Hello, %s!' % args[1])
    else:
        print('Too many arguments!')

if __name__=='__main__':
    test()
```

第1行和第2行是标准注释, 第1行注释可以让这个`hello.py`文件直接在Unix/Linux/Mac上运行, 第2行注释表示.py文件本身使用标准UTF-8编码;

第4行是一个字符串, 表示模块的文档注释, 任何模块代码的第一个字符串都被视为模块的文档注释;
第6行使用`__author__`变量把作者写进去, 这样当你公开源代码后别人就可以瞻仰你的大名;
以上就是Python模块的标准文件模板, 当然也可以全部删掉不写, 但是, 按标准办事肯定没错.

后面开始就是真正的代码部分.

你可能注意到了, 使用sys模块的第一步, 就是导入该模块:
`import sys`

导入`sys`模块后, 我们就有了变量`sys`指向该模块, 利用`sys`这个变量, 就可以访问`sys`模块的所有功能.

`sys`模块有一个`argv`变量, 用`list`存储了命令行的所有参数. `argv`至少有一个元素, 因为第一个参数永远是该`.py`文件的名称, 例如:

运行`python3 hello.py`获得的`sys.argv`就是`['hello.py']`;

运行`python3 hello.py Michael`获得的`sys.argv`就是`['hello.py', 'Michael]`.

最后, 注意到这两行代码:

```python
if __name__=='__main__':
    test()
```

当我们在命令行运行hello模块文件时, Python解释器把一个特殊变量`__name__`置为`__main__`, 而如果在其他地方导入该hello模块时, `if`判断将失败, 因此, 这种if测试可以让一个模块通过命令行运行时执行一些额外的代码, 最常见的就是运行测试.

我们可以用命令行运行hello.py看看效果:

```bash
$ python3 hello.py
Hello, world!
$ python hello.py Michael
Hello, Michael!
```

如果启动Python交互环境, 再导入hello模块:

```python
$ python3
Python 3.4.3 (v3.4.3:9b73f1c3e601, Feb 23 2015, 02:52:03)
[GCC 4.2.1 (Apple Inc. build 5666) (dot 3)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> import hello
>>>
```

导入时, 没有打印`Hello, word!`, 因为没有执行`test()`函数.

调用`hello.test()`时, 才能打印出Hello, word!:

```python
>>> hello.test()
Hello, world!
```

#### 作用域

在一个模块中, 我们可能会定义很多函数和变量, 但有的函数和变量我们希望给别人使用, 有的函数和变量我们希望仅仅在模块内部使用. 在Python中, 是通过`_`前缀来实现的.

正常的函数和变量名是公开的(`public`), 可以被直接引用, 比如: `abc`, `x123`, `PI`等;

类似`__xxx__`这样的变量是特殊变量, 可以被直接引用, 但是有特殊用途, 比如上面的`__author__`, `__name__`就是特殊变量, hello模块定义的文档注释也可以用特殊变量`__doc__`访问, 我们自己的变量一般不要用这种变量名;

类似`_xxx`和`__xxx`这样的函数或变量就是非公开的(private), 不应该被直接引用, 比如`_abc`, `__abc`等;

之所以我们说, private函数和变量"不应该"被直接引用, 而不是"不能"被直接引用, 是因为Python并没有一种方法可以完全限制访问private函数或变量, 但是, 从编程习惯上不应该引用private函数或变量.

private函数或变量不应该被别人引用, 那它们有什么用呢? 请看例子:

```python
def _private_1(name):
    return 'Hello, %s' % name

def _private_2(name):
    return 'Hi, %s' % name

def greeting(name):
    if len(name) > 3:
        return _private_1(name)
    else:
        return _private_2(name)
```

我们在模块里公开`greeting()`函数, 而把内部逻辑用private函数隐藏起来了, 这样, 调用`greeting()`函数不用关心内部的private函数细节, 这也是一种非常有用的代码封装和抽象的方法, 即:

外部不需要引用的函数全部定义成private, 只有外部需要引用的函数才定义为public.

### 安装第三方模块

在Python中, 安装第三方模块, 是通过包管理工具pip完成的.

如果你正在使用Mac或Linux, 安装pip本身这个步骤就可以跳过了.

如果你正在使用Windows, 请参考安装Python一节的内容, 确保安装时勾选了`pip`和`Add python.exe to Path`.

在命令提示符窗口下尝试运行`pip`, 如果Windows提示未找到命令, 可以重新运行安装程序添加pip.

注意: Mac或Linux上有可能并存Python 3.x和Python 2.x, 因此对应的pip命令是`pip3`.

例如, 我们要安装一个第三方库——`Python Imaging Library`, 这是Python下非常强大的处理图像的工具库. 不过, PIL目前只支持到Python 2.7, 并且有年头没有更新了, 因此, 基于PIL的`Pillow`项目开发非常活跃, 并且支持最新的Python 3.

一般来说, 第三方库都会在Python官方的`pypi.python.org`网站注册, 要安装一个第三方库, 必须先知道该库的名称, 可以在官网或者pypi上搜索, 比如`Pillow`的名称叫`Pillow`, 因此, 安装`Pillow`的命令就是:

```python
pip install Pillow
```

耐心等待下载并安装后, 就可以使用Pillow了.

#### 安装常用模块

在使用Python时, 我们经常需要用到很多第三方库, 例如, 上面提到的Pillow, 以及MySQL驱动程序, Web框架Flask, 科学计算Numpy等. 用pip一个一个安装费时费力, 还需要考虑兼容性. 我们推荐直接使用`Anaconda`, 这是一个基于Python的数据处理和科学计算平台, 它已经内置了许多非常有用的第三方库, 我们装上`Anaconda`, 就相当于把数十个第三方模块自动安装好了, 非常简单易用.

可以从`Anaconda`官网下载GUI安装包, 安装包有500~600M, 所以需要耐心等待下载. 网速慢的同学请移步国内镜像. 下载后直接安装, `Anaconda`会把系统Path中的python指向自己自带的Python, 并且, `Anaconda`安装的第三方模块会安装在Anaconda自己的路径下, 不影响系统已安装的Python目录.

安装好Anaconda后, 重新打开命令行窗口, 输入python, 可以看到Anaconda的信息:

```python
C:\> python
Python 3.6.3 |Anaconda, Inc.| ... on win32
Type "help", ... for more information.
```

可以尝试直接`import numpy`等已安装的第三方模块.
模块搜索路径

当我们试图加载一个模块时, Python会在指定的路径下搜索对应的`.py`文件, 如果找不到, 就会报错:

```python
>>> import mymodule
...
ImportError: No module named mymodule
```

默认情况下, Python解释器会搜索当前目录, 所有已安装的内置模块和第三方模块, 搜索路径存放在sys模块的path变量中:

```python
>>> import sys
>>> sys.path
['', '/Library/Frameworks/Python.framework/Versions/3.6/lib/python36.zip', '/Library/Frameworks/Python.framework/Versions/3.6/lib/python3.6', ..., '/Library/Frameworks/Python.framework/Versions/3.6/lib/python3.6/site-packages']
```

如果我们要添加自己的搜索目录, 有两种方法:

一是直接修改`sys.path`, 添加要搜索的目录:

```python
>>> import sys
>>> sys.path.append('/Users/michael/my_py_scripts')
```

这种方法是在运行时修改, 运行结束后失效.

第二种方法是设置环境变量`PYTHONPATH`, 该环境变量的内容会被自动添加到模块搜索路径中. 设置方式与设置`Path`环境变量类似. 注意只需要添加你自己的搜索路径, Python自己本身的搜索路径不受影响
