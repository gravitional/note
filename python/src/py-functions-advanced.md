# 高级特性

掌握了Python的数据类型, 语句和函数, 基本上就可以编写出很多有用的程序了.

比如构造一个`1, 3, 5, 7, ..., 99`的列表, 可以通过循环实现:

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

## 切片

取一个list或tuple的部分元素是非常常见的操作. 比如, 一个list如下:

```python
>>> L = ['Michael', 'Sarah', 'Tracy', 'Bob', 'Jack']
```

取前3个元素, 应该怎么做?

笨办法:

>>> [L[0], L[1], L[2]]
['Michael', 'Sarah', 'Tracy']

对这种经常取指定索引范围的操作, 用循环十分繁琐, 因此, Python提供了切片(`Slice`)操作符, 能大大简化这种操作.

对应上面的问题, 取前3个元素, 用一行代码就可以完成切片:

```python
>>> L[0:3]
['Michael', 'Sarah', 'Tracy']
```

`L[0:3]`表示, 从索引`0`开始取, 直到索引`3`为止, 但不包括索引`3`. 即索引`0`, `1`, `2`, 正好是3个元素.

如果第一个索引是`0`, 还可以省略:

```python
>>> L[:3]
['Michael', 'Sarah', 'Tracy']
```

也可以从索引`1`开始, 取出`2`个元素出来:

```python
>>> L[1:3]
['Sarah', 'Tracy']
```

类似的, 既然Python支持`L[-1]`取倒数第一个元素, 那么它同样支持倒数切片, 试试:

```python
>>> L[-2:]
['Bob', 'Jack']
>>> L[-2:-1]
['Bob']
```

记住倒数第一个元素的索引是`-1`.

切片操作十分有用. 我们先创建一个`0-99`的数列:

```python
>>> L = list(range(100))
>>> L
[0, 1, 2, 3, ..., 99]
```

可以通过切片轻松取出某一段数列. 比如前`10`个数:

```python
>>> L[:10]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

后`10`个数:

```python
>>> L[-10:]
[90, 91, 92, 93, 94, 95, 96, 97, 98, 99]
```

前`11-20`个数:

```python
>>> L[10:20]
[10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
```

前`10`个数, 每两个取一个:

```python
>>> L[:10:2]
[0, 2, 4, 6, 8]
```

所有数, 每5个取一个:

```python
>>> L[::5]
[0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95]
```

甚至什么都不写, 只写`[:]`就可以原样复制一个`list`:

```python
>>> L[:]
[0, 1, 2, 3, ..., 99]
```

tuple也是一种list, 唯一区别是tuple不可变. 因此, tuple也可以用切片操作, 只是操作的结果仍是tuple:

```python
>>> (0, 1, 2, 3, 4, 5)[:3]
(0, 1, 2)
```

字符串`'xxx'`也可以看成是一种`list`, 每个元素就是一个字符.
因此, 字符串也可以用切片操作, 只是操作结果仍是字符串:

```python
>>> 'ABCDEFG'[:3]
'ABC'
>>> 'ABCDEFG'[::2]
'ACEG'
```

在很多编程语言中, 针对字符串提供了很多各种截取函数(例如, `substring`), 其实目的就是对字符串切片. Python没有针对字符串的截取函数, 只需要切片一个操作就可以完成, 非常简单.

### 练习-切片

利用切片操作, 实现一个`trim()`函数, 去除字符串首尾的空格,
注意不要调用`str`的`strip()`方法:

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

## 迭代

如果给定一个list或tuple, 我们可以通过`fo`r循环来遍历这个`list`或`tuple`, 这种遍历我们称为迭代(`Iteration`).

在Python中, 迭代是通过`for ... in`来完成的, 而很多语言比如C语言, 迭代list是通过下标完成的, 比如Java代码:

```python
for (i=0; i<list.length; i++) {
    n = list[i];
}
```

可以看出, Python的`for`循环抽象程度要高于C的`for`循环, 因为Python的`for`循环不仅可以用在`list`或`tuple`上, 还可以作用在其他可迭代对象上.

`list`这种数据类型虽然有下标, 但很多其他数据类型是没有下标的, 但是, 只要是可迭代对象, 无论有无下标, 都可以迭代, 比如dict就可以迭代:

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

由于字符串也是可迭代对象, 因此, 也可以作用于`for`循环:

```python
>>> for ch in 'ABC':
...     print(ch)
...
A
B
C
```

所以, 当我们使用`for`循环时, 只要作用于一个可迭代对象, `for`循环就可以正常运行, 而我们不太关心该对象究竟是list还是其他数据类型.

那么, 如何判断一个对象是可迭代对象呢? 方法是通过`collections`模块的`Iterable`类型判断:

```python
>>> from collections import Iterable
>>> isinstance('abc', Iterable) # str是否可迭代
True
>>> isinstance([1,2,3], Iterable) # list是否可迭代
True
>>> isinstance(123, Iterable) # 整数是否可迭代
False
```

最后一个小问题, 如果要对`list`实现类似Java那样的下标循环怎么办?
Python内置的`enumerate`函数可以把一个`list`变成`索引-元素对`,
这样就可以在`for`循环中同时迭代索引和元素本身:

```python
>>> for i, value in enumerate(['A', 'B', 'C']):
...     print(i, value)
...
0 A
1 B
2 C
```

上面的`for`循环里, 同时引用了两个变量, 在Python里是很常见的, 比如下面的代码:

```python
>>> for x, y in [(1, 1), (2, 4), (3, 9)]:
...     print(x, y)
...
1 1
2 4
3 9
```

### 练习-迭代

请使用迭代查找一个list中最小和最大值, 并返回一个tuple:

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

### 小结-迭代

任何可迭代对象都可以作用于for循环, 包括我们自定义的数据类型, 只要符合迭代条件, 就可以使用for循环.

## 列表生成式

列表生成式即`List Comprehensions`, 是Python内置的非常简单却强大的可以用来创建list的生成式.

举个例子, 要生成`list [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
可以用`list(range(1, 11))`:

```python
>>> list(range(1, 11))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

但如果要生成`[1x1, 2x2, 3x3, ..., 10x10]`怎么做? 方法一是循环:

```python
>>> L = []
>>> for x in range(1, 11):
...    L.append(x * x)
...
>>> L
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

但是循环太繁琐, 而列表生成式则可以用一行语句代替循环生成上面的list:

```python
>>> [x * x for x in range(1, 11)]
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

写列表生成式时, 把要生成的元素`x * x`放到前面, 后面跟`for`循环,
就可以把list创建出来, 十分有用, 多写几次, 很快就可以熟悉这种语法.

`for`循环后面还可以加上`if`判断, 这样我们就可以筛选出仅偶数的平方:

```python
>>> [x * x for x in range(1, 11) if x % 2 == 0]
[4, 16, 36, 64, 100]
```

还可以使用两层循环, 可以生成全排列:

```python
>>> [m + n for m in 'ABC' for n in 'XYZ']
['AX', 'AY', 'AZ', 'BX', 'BY', 'BZ', 'CX', 'CY', 'CZ']
```

三层和三层以上的循环就很少用到了.

运用列表生成式, 可以写出非常简洁的代码. 例如, 列出当前目录下的所有文件和目录名, 可以通过一行代码实现:

```python
>>> import os # 导入os模块, 模块的概念后面讲到
>>> [d for d in os.listdir('.')] # os.listdir可以列出文件和目录
['.emacs.d', '.ssh', '.Trash', 'Adlm', 'Applications', 'Desktop', 'Documents', 'Downloads', 'Library', 'Movies', 'Music', 'Pictures', 'Public', 'VirtualBox VMs', 'Workspace', 'XCode']
```

`for`循环其实可以同时使用两个甚至多个变量,
比如`dict`的`items()`可以同时迭代`key`和`value`:

```python
>>> d = {'x': 'A', 'y': 'B', 'z': 'C' }
>>> for k, v in d.items():
...     print(k, '=', v)
...
y = B
x = A
z = C
```

因此, 列表生成式也可以使用两个变量来生成list:

```python
>>> d = {'x': 'A', 'y': 'B', 'z': 'C' }
>>> [k + '=' + v for k, v in d.items()]
['y=B', 'x=A', 'z=C']
```

最后把一个list中所有的字符串变成小写:

```python
>>> L = ['Hello', 'World', 'IBM', 'Apple']
>>> [s.lower() for s in L]
['hello', 'world', 'ibm', 'apple']
```

### 练习-列表生成器

如果list中既包含字符串, 又包含整数, 由于非字符串类型没有`lower()`方法, 所以列表生成式会报错:

```python
>>> L = ['Hello', 'World', 18, 'Apple', None]
>>> [s.lower() for s in L]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 1, in <listcomp>
AttributeError: 'int' object has no attribute 'lower'
```

使用内建的`isinstance`函数可以判断一个变量是不是字符串:

```python
>>> x = 'abc'
>>> y = 123
>>> isinstance(x, str)
True
>>> isinstance(y, str)
False
```

请修改列表生成式, 通过添加`if`语句保证列表生成式能正确地执行:

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

## 生成器

通过列表生成式, 我们可以直接创建一个列表. 但是, 受到内存限制, 列表容量肯定是有限的. 而且, 创建一个包含100万个元素的列表, 不仅占用很大的存储空间, 如果我们仅仅需要访问前面几个元素, 那后面绝大多数元素占用的空间都白白浪费了.

所以, 如果列表元素可以按照某种算法推算出来, 那我们是否可以在循环的过程中不断推算出后续的元素呢? 这样就不必创建完整的list, 从而节省大量的空间. 在Python中, 这种一边循环一边计算的机制, 称为生成器: `generator`.

要创建一个generator, 有很多种方法. 第一种方法很简单, 只要把一个列表生成式的`[]`改成`()`, 就创建了一个generator:

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

我们可以直接打印出list的每一个元素, 但我们怎么打印出generator的每一个元素呢?
如果要一个一个打印出来, 可以通过`next()`函数获得generator的下一个返回值:

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

当然, 上面这种不断调用`next(g)`实在是太变态了, 正确的方法是使用`for`循环, 因为generator也是可迭代对象:

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
除第一个和第二个数外, 任意一个数都可由前两个数相加得到:

```python
1, 1, 2, 3, 5, 8, 13, 21, 34, ...
```

斐波拉契数列用列表生成式写不出来, 但是, 用函数把它打印出来却很容易:

```python
def fib(max):
    n, a, b = 0, 0, 1
    while n < max:
        print(b)
        a, b = b, a + b
        n = n + 1
    return 'done'
```

注意, 赋值语句:

```python
a, b = b, a + b
```

相当于:

```python
t = (b, a + b) # t是一个tuple
a = t[0]
b = t[1]
```

但不必显式写出临时变量`t`就可以赋值.

上面的函数可以输出斐波那契数列的前`N`个数:

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
只需要把`print(b)`改为`yield b`就可以了:

```python
def fib(max):
    n, a, b = 0, 0, 1
    while n < max:
        yield b
        a, b = b, a + b
        n = n + 1
    return 'done'
```

这就是定义`generator`的另一种方法. 如果一个函数定义中包含`yield`关键字, 那么这个函数就不再是一个普通函数, 而是一个`generator`:

```python
>>> f = fib(6)
>>> f
<generator object fib at 0x104feaaa0>
```

这里, 最难理解的就是`generator`和函数的执行流程不一样.
函数是顺序执行, 遇到`return`语句或者最后一行函数语句就返回.
而变成`generator`的函数, 在每次调用`next()`的时候执行, 遇到`yield`语句返回, 再次执行时从上次返回的`yield`语句处继续执行.

举个简单的例子, 定义一个generator, 依次返回数字`1, 3, 5`:

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
然后用`next()`函数不断获得下一个返回值:

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

同样的, 把函数改成`generator`后, 我们基本上从来不会用`next()`来获取下一个返回值, 而是直接使用`for`循环来迭代:

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
如果想要拿到返回值, 必须捕获`StopIteration`错误, 返回值包含在`StopIteration`的`value`中:

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

杨辉三角定义如下:

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

把每一行看做一个list, 试写一个generator, 不断输出下一行的list:

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

## 迭代器

我们已经知道, 可以直接作用于`for`循环的数据类型有以下几种:

一类是集合数据类型, 如list, tuple, dict, set, str等;
一类是generator, 包括生成器和带yield的generator function.
这些可以直接作用于`for`循环的对象统称为可迭代对象: `Iterable`.

可以使用`isinstance()`判断一个对象是否是Iterable对象:

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
可以被`next()`函数调用并不断返回下一个值的对象称为迭代器: `Iterator`.

可以使用`isinstance()`判断一个对象是否是`Iterator`对象:

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

把list, dict, str等Iterable变成`Iterator`可以使用`iter()`函数:

```python
>>> isinstance(iter([]), Iterator)
True
>>> isinstance(iter('abc'), Iterator)
True
```

你可能会问, 为什么list, dict, str等数据类型不是`Iterator`?

这是因为Python的Iterator对象表示的是一个数据流, Iterator对象可以被`next()`函数调用并不断返回下一个数据, 直到没有数据时抛出StopIteration错误.
可以把这个数据流看做是一个有序序列, 但我们却不能提前知道序列的长度, 只能不断通过`next()`函数实现按需计算下一个数据, 所以Iterator的计算是惰性的, 只有在需要返回下一个数据时它才会计算.

Iterator甚至可以表示一个无限大的数据流, 例如全体自然数. 而使用list是永远不可能存储全体自然数的.

## 小结-迭代器

凡是可作用于`for`循环的对象都是`Iterable`类型;
凡是可作用于`next()`函数的对象都是`Iterator`类型, 它们表示一个惰性计算的序列;

集合数据类型如list, dict, str等是Iterable但不是Iterator, 不过可以通过`iter()`函数获得一个`Iterator`对象.

Python的`for`循环本质上就是通过不断调用`next()`函数实现的, 例如:

```python
for x in [1, 2, 3, 4, 5]:
    pass
```

实际上完全等价于:

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
