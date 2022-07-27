# python-4.md

ref: [这是小白的Python新手教程][https://www.liaoxuefeng.com/wiki/1016959663602400]

## 错误, 调试和测试

在程序运行过程中, 总会遇到各种各样的错误.

有的错误是程序编写有问题造成的, 比如本来应该输出整数结果输出了字符串, 这种错误我们通常称之为bug, bug是必须修复的.

有的错误是用户输入造成的, 比如让用户输入email地址, 结果得到一个空字符串, 这种错误可以通过检查用户输入来做相应的处理.

还有一类错误是完全无法在程序运行过程中预测的, 比如写入文件的时候, 磁盘满了, 写不进去了, 或者从网络抓取数据, 网络突然断掉了. 这类错误也称为异常, 在程序中通常是必须处理的, 否则, 程序会因为各种问题终止并退出.

Python内置了一套异常处理机制, 来帮助我们进行错误处理.

此外, 我们也需要跟踪程序的执行, 查看变量的值是否正确, 这个过程称为调试. Python的pdb可以让我们以单步方式执行代码.

最后, 编写测试也很重要. 有了良好的测试, 就可以在程序修改后反复运行, 确保程序输出符合我们编写的测试.

### 错误处理

在程序运行的过程中, 如果发生了错误, 可以事先约定返回一个错误代码, 这样, 就可以知道是否有错, 以及出错的原因.
在操作系统提供的调用中, 返回错误码非常常见. 比如打开文件的函数`open()`, 成功时返回文件描述符(就是一个整数), 出错时返回`-1`.

用错误码来表示是否出错十分不便, 因为函数本身应该返回的正常结果和错误码混在一起, 造成调用者必须用大量的代码来判断是否出错:

```python
def foo():
    r = some_function()
    if r==(-1):
        return (-1)
    # do something
    return r

def bar():
    r = foo()
    if r==(-1):
        print('Error')
    else:
        pass
```

一旦出错, 还要一级一级上报, 直到某个函数可以处理该错误(比如, 给用户输出一个错误信息).

所以高级语言通常都内置了一套`try...except...finally..`.的错误处理机制, Python也不例外.

#### try

让我们用一个例子来看看`try`的机制:

```python
try:
    print('custom try...')
    r = 10 / 0
    print('result:', r)
except ZeroDivisionError as e:
    print('custom except:', e)
finally:
    print('custom finally...')

print('END')
```

当我们认为某些代码可能会出错时, 就可以用`try`来运行这段代码, 如果执行出错, 则后续代码不会继续执行, 而是直接跳转至错误处理代码, 即`except`语句块, 执行完`except`后, 如果有`finally`语句块, 则执行`finally`语句块, 至此, 执行完毕.

上面的代码在计算`10 / 0`时会产生一个除法运算错误:

```python
custom try...
custom except: division by zero
custom finally...
END
```

从输出可以看到, 当错误发生时, 后续语句`print('result:', r)`不会被执行, `except`由于捕获到`ZeroDivisionError`, 因此被执行. 最后, finally语句被执行. 然后, 程序继续按照流程往下走.

如果把除数`0`改成`2`, 则执行结果如下:

```python
try...
result: 5
finally...
END
```

由于没有错误发生, 所以`except`语句块不会被执行, 但是`finally`如果有, 则一定会被执行(可以没有`finally`语句).

你还可以猜测, 错误应该有很多种类, 如果发生了不同类型的错误, 应该由不同的`except`语句块处理. 没错, 可以有多个`except`来捕获不同类型的错误:

```python
try:
    print('try...')
    r = 10 / int('a')
    print('result:', r)
except ValueError as e:
    print('ValueError:', e)
except ZeroDivisionError as e:
    print('ZeroDivisionError:', e)
finally:
    print('finally...')
print('END')
```

`int()`函数可能会抛出`ValueError`, 所以我们用一个except捕获`ValueError`, 用另一个`except`捕获`ZeroDivisionError`.

此外, 如果没有错误发生, 可以在`except`语句块后面加一个`else`, 当没有错误发生时, 会自动执行`else`语句:

```python
try:
    print('try...')
    r = 10 / int('2')
    print('result:', r)
except ValueError as e:
    print('ValueError:', e)
except ZeroDivisionError as e:
    print('ZeroDivisionError:', e)
else:
    print('no error!')
finally:
    print('finally...')
print('END')
```

Python的错误其实也是class, 所有的错误类型都继承自`BaseException`, 所以在使用`except`时需要注意的是, 它不但捕获该类型的错误, 还把其子类也"一网打尽". 比如:

```python
try:
    foo()
except ValueError as e:
    print('ValueError')
except UnicodeError as e:
    print('UnicodeError')
```

第二个`except`永远也捕获不到`UnicodeError`, 因为`UnicodeError`是`ValueError`的子类, 如果有, 也被第一个`except`给捕获了.

Python所有的错误都是从BaseException类派生的, 常见的错误类型和继承关系看这里:

[常见的错误类型和继承关系][]

[常见的错误类型和继承关系]: https://docs.python.org/3/library/exceptions.html#exception-hierarchy

使用`try...except`捕获错误还有一个巨大的好处, 就是可以跨越多层调用, 比如函数`main()`调用`foo()`, `foo()`调用`bar()`, 结果`bar()`出错了, 这时, 只要`main()`捕获到了, 就可以处理:

```python
def foo(s):
    return 10 / int(s)

def bar(s):
    return foo(s) * 2

def main():
    try:
        bar('0')
    except Exception as e:
        print('Error:', e)
    finally:
        print('finally...')
```

也就是说, 不需要在每个可能出错的地方去捕获错误, 只要在合适的层次去捕获错误就可以了. 这样一来, 就大大减少了写`try...except...finally`的麻烦.

#### 调用栈

如果错误没有被捕获, 它就会一直往上抛, 最后被Python解释器捕获, 打印一个错误信息, 然后程序退出. 来看看`err.py`:

```python
# err.py:
def foo(s):
    return 10 / int(s)

def bar(s):
    return foo(s) * 2

def main():
    bar('0')

main()
```

执行, 结果如下:

```python
$ python3 err.py
Traceback (most recent call last):
  File "err.py", line 11, in <module>
    main()
  File "err.py", line 9, in main
    bar('0')
  File "err.py", line 6, in bar
    return foo(s) * 2
  File "err.py", line 3, in foo
    return 10 / int(s)
ZeroDivisionError: division by zero
```

出错并不可怕, 可怕的是不知道哪里出错了. 解读错误信息是定位错误的关键. 我们从上往下可以看到整个错误的调用函数链:

错误信息第1行:

`Traceback (most recent call last):`

告诉我们这是错误的跟踪信息.

第2~3行:

```python
File "err.py", line 11, in <module>
    main()
```

调用`main()`出错了, 在代码文件err.py的第11行代码, 但原因是第9行:

```python
File "err.py", line 9, in main
    bar('0')
```

调用`bar('0')`出错了, 在代码文件err.py的第9行代码, 但原因是第6行:

```python
  File "err.py", line 6, in bar
    return foo(s) * 2
```

原因是`return foo(s) * 2`这个语句出错了, 但这还不是最终原因, 继续往下看:

 ```python
    File "err.py", line 3, in foo
    return 10 / int(s)
 ```

原因是`return 10 / int(s)`这个语句出错了, 这是错误产生的源头, 因为下面打印了:

```python
ZeroDivisionError: integer division or modulo by zero
```

根据错误类型`ZeroDivisionError`, 我们判断, `int(s)`本身并没有出错, 但是`int(s)`返回`0`, 在计算`10 / 0`时出错, 至此, 找到错误源头.
出错的时候, 一定要分析错误的调用栈信息, 才能定位错误的位置.

#### 记录错误

如果不捕获错误, 自然可以让Python解释器来打印出错误堆栈, 但程序也被结束了. 既然我们能捕获错误, 就可以把错误堆栈打印出来, 然后分析错误原因, 同时, 让程序继续执行下去.

Python内置的`logging`模块可以非常容易地记录错误信息:

```python
# err_logging.py

import logging

def foo(s):
    return 10 / int(s)

def bar(s):
    return foo(s) * 2

def main():
    try:
        bar('0')
    except Exception as e:
        logging.exception(e)

main()
print('END')
```

同样是出错, 但程序打印完错误信息后会继续执行, 并正常退出:

```python
$ python3 err_logging.py
ERROR:root:division by zero
Traceback (most recent call last):
  File "err_logging.py", line 13, in main
    bar('0')
  File "err_logging.py", line 9, in bar
    return foo(s) * 2
  File "err_logging.py", line 6, in foo
    return 10 / int(s)
ZeroDivisionError: division by zero
END
```

通过配置, `logging`还可以把错误记录到日志文件里, 方便事后排查.

#### 抛出错误

因为错误是class, 捕获一个错误就是捕获到该class的一个实例. 因此, 错误并不是凭空产生的, 而是有意创建并抛出的. Python的内置函数会抛出很多类型的错误, 我们自己编写的函数也可以抛出错误.

如果要抛出错误, 首先根据需要, 可以定义一个错误的class, 选择好继承关系, 然后, 用`raise`语句抛出一个错误的实例:

```python
# err_raise.py
class FooError(ValueError):
    pass

def foo(s):
    n = int(s)
    if n==0:
        raise FooError('invalid value: %s' % s)
    return 10 / n

foo('0')
```

执行, 可以最后跟踪到我们自己定义的错误:

```python
$ python3 err_raise.py
Traceback (most recent call last):
  File "err_throw.py", line 11, in <module>
    foo('0')
  File "err_throw.py", line 8, in foo
    raise FooError('invalid value: %s' % s)
__main__.FooError: invalid value: 0
```

只有在必要的时候才定义我们自己的错误类型. 如果可以选择Python已有的内置的错误类型(比如ValueError, TypeError), 尽量使用Python内置的错误类型.

最后, 我们来看另一种错误处理的方式:

```python
# err_reraise.py

def foo(s):
    n = int(s)
    if n==0:
        raise ValueError('invalid value: %s' % s)
    return 10 / n

def bar():
    try:
        foo('0')
    except ValueError as e:
        print('ValueError!')
        raise

bar()
```

在`bar()`函数中, 我们明明已经捕获了错误, 但是, 打印一个ValueError!后, 又把错误通过`raise`语句抛出去了, 这不有病么?

其实这种错误处理方式不但没病, 而且相当常见. 捕获错误目的只是记录一下, 便于后续追踪. 但是, 由于当前函数不知道应该怎么处理该错误, 所以, 最恰当的方式是继续往上抛, 让顶层调用者去处理. 好比一个员工处理不了一个问题时, 就把问题抛给他的老板, 如果他的老板也处理不了, 就一直往上抛, 最终会抛给CEO去处理.

`raise`语句如果不带参数, 就会把当前错误原样抛出. 此外, 在except中`raise`一个Error, 还可以把一种类型的错误转化成另一种类型:

```python
try:
    10 / 0
except ZeroDivisionError:
    raise ValueError('input error!')

只要是合理的转换逻辑就可以, 但是, 决不应该把一个IOError转换成毫不相干的ValueError.
```

#### 练习-错误处理

运行下面的代码, 根据异常信息进行分析, 定位出错误源头, 并修复:

```python
# -*- coding: utf-8 -*-
from functools import reduce

def str2num(s):
    return int(s)

def calc(exp):
    ss = exp.split('+')
    ns = map(str2num, ss)
    return reduce(lambda acc, x: acc + x, ns)

def main():
    r = calc('100 + 200 + 345')
    print('100 + 200 + 345 =', r)
    r = calc('99 + 88 + 7.6')
    print('99 + 88 + 7.6 =', r)

main()
```

```python
def str2num(s):
    return float(s)
```

### 调试

程序能一次写完并正常运行的概率很小, 基本不超过1%. 总会有各种各样的bug需要修正. 有的bug很简单, 看看错误信息就知道, 有的bug很复杂, 我们需要知道出错时, 哪些变量的值是正确的, 哪些变量的值是错误的, 因此, 需要一整套调试程序的手段来修复bug.

第一种方法简单直接粗暴有效, 就是用`print()`把可能有问题的变量打印出来看看:

```python
def foo(s):
    n = int(s)
    print('>>> n = %d' % n)
    return 10 / n

def main():
    foo('0')

main()
```

执行后在输出中查找打印的变量值:

```python
$ python err.py
>>> n = 0
Traceback (most recent call last):
  ...
ZeroDivisionError: integer division or modulo by zero
```

用`print()`最大的坏处是将来还得删掉它, 想想程序里到处都是`print()`, 运行结果也会包含很多垃圾信息. 所以, 我们又有第二种方法.

#### 断言

凡是用`print()`来辅助查看的地方, 都可以用断言(assert)来替代:

```python
def foo(s):
    n = int(s)
    assert n != 0, 'n is zero!'
    return 10 / n

def main():
    foo('0')
```

`assert`的意思是, 表达式`n != 0`应该是`True`, 否则, 根据程序运行的逻辑, 后面的代码肯定会出错.

如果断言失败, `assert`语句本身就会抛出`AssertionError: `

```python
$ python err.py
Traceback (most recent call last):
  ...
AssertionError: n is zero!
```

程序中如果到处充斥着`assert`, 和`print()`相比也好不到哪去. 不过, 启动Python解释器时可以用`-O`参数来关闭`assert`:

```python
$ python -O err.py
Traceback (most recent call last):
  ...
ZeroDivisionError: division by zero
```

注意: 断言的开关"`-O`"是英文大写字母`O`, 不是数字`0`.

关闭后, 你可以把所有的`assert`语句当成pass来看.

#### logging

把`print()`替换为`logging`是第3种方式, 和assert比, `logging`不会抛出错误, 而且可以输出到文件:

```python
import logging

s = '0'
n = int(s)
logging.info('n = %d' % n)
print(10 / n)
```

`logging.info()`就可以输出一段文本.
运行, 发现除了`ZeroDivisionError`, 没有任何信息. 怎么回事?

别急, 在`import logging`之后添加一行配置再试试:

```python
import logging
logging.basicConfig(level=logging.INFO)
```

看到输出了:

```python
$ python err.py
INFO:root:n = 0
Traceback (most recent call last):
  File "err.py", line 8, in <module>
    print(10 / n)
ZeroDivisionError: division by zero
```

这就是logging的好处, 它允许你指定记录信息的级别,
有`debug`, `info`, `warning`, `error`等几个级别, 当我们指定`level=INFO`时, `logging.debug`就不起作用了.
同理, 指定`level=WARNING`后, `debug`和`info`就不起作用了.
这样一来, 你可以放心地输出不同级别的信息, 也不用删除,
最后统一控制输出哪个级别的信息.

`logging`的另一个好处是通过简单的配置, 一条语句可以同时输出到不同的地方, 比如`console`和文件.

#### pdb

第4种方式是启动Python的调试器`pdb`, 让程序以单步方式运行, 可以随时查看运行状态. 我们先准备好程序:

```python
# err.py
s = '0'
n = int(s)
print(10 / n)
```

然后启动:

```python
$ python -m pdb err.py
> /Users/michael/Github/learn-python3/samples/debug/err.py(2)<module>()
-> s = '0'
```

以参数`-m pdb`启动后, `pdb`定位到下一步要执行的代码`-> s = '0'`. 输入命令`l`(letter)来查看代码:

```python
(Pdb) l
  1     # err.py
  2  -> s = '0'
  3     n = int(s)
  4     print(10 / n)
```

输入命令`n`可以单步执行代码:

```python
(Pdb) n
> /Users/michael/Github/learn-python3/samples/debug/err.py(3)<module>()
-> n = int(s)
(Pdb) n
> /Users/michael/Github/learn-python3/samples/debug/err.py(4)<module>()
-> print(10 / n)
```

任何时候都可以输入命令`p 变量名`来查看变量:

```python
(Pdb) p s
'0'
(Pdb) p n
0
```

输入命令`q`结束调试, 退出程序:

```python
(Pdb) q
```

这种通过`pdb`在命令行调试的方法理论上是万能的, 但实在是太麻烦了, 如果有一千行代码, 要运行到第`999`行得敲多少命令啊.
还好, 我们还有另一种调试方法.

#### pdb.set_trace()

这个方法也是用pdb, 但是不需要单步执行, 我们只需要`import pdb`, 然后, 在可能出错的地方放一个`pdb.set_trace()`, 就可以设置一个断点:

```python
# err.py
import pdb

s = '0'
n = int(s)
pdb.set_trace() # 运行到这里会自动暂停
print(10 / n)
```

运行代码, 程序会自动在`pdb.set_trace()`暂停并进入`pdb`调试环境, 可以用命令`p`查看变量, 或者用命令`c`继续运行:

```python
$ python err.py
> /Users/michael/Github/learn-python3/samples/debug/err.py(7)<module>()
-> print(10 / n)
(Pdb) p n
0
(Pdb) c
Traceback (most recent call last):
  File "err.py", line 7, in <module>
    print(10 / n)
ZeroDivisionError: division by zero
```

这个方式比直接启动`pdb`单步调试效率要高很多, 但也高不到哪去.

#### IDE

如果要比较爽地设置断点, 单步执行, 就需要一个支持调试功能的`IDE`. 目前比较好的Python IDE有:

[Visual Studio Code][] , 需要安装Python插件.

[Visual Studio Code]: https://code.visualstudio.com/

[PyCharm][] , 需要安装Python插件.

[PyCharm]: http://www.jetbrains.com/pycharm/

另外, `Eclipse`加上`pydev`插件也可以调试Python程序.

#### 小结

写程序最痛苦的事情莫过于调试, 程序往往会以你意想不到的流程来运行, 你期待执行的语句其实根本没有执行, 这时候, 就需要调试了.

虽然用IDE调试起来比较方便, 但是最后你会发现, logging才是终极武器.

### 单元测试

如果你听说过"测试驱动开发"(TDD: Test-Driven Development), 单元测试就不陌生.

单元测试是用来对一个模块, 一个函数或者一个类来进行正确性检验的测试工作.

比如对函数abs(), 我们可以编写出以下几个测试用例:

+ 输入正数, 比如`1`, `1.2`, `0.99`, 期待返回值与输入相同;
+ 输入负数, 比如`-1`, `-1.2`, `-0.99`, 期待返回值与输入相反;
+ 输入`0`, 期待返回`0`;
+ 输入非数值类型, 比如`None`, `[]`, `{}`, 期待抛出`TypeError`.

把上面的测试用例放到一个测试模块里, 就是一个完整的单元测试.

如果单元测试通过, 说明我们测试的这个函数能够正常工作.
如果单元测试不通过, 要么函数有bug, 要么测试条件输入不正确,
总之, 需要修复使单元测试能够通过.

单元测试通过后有什么意义呢? 如果我们对`abs()`函数代码做了修改, 只需要再跑一遍单元测试, 如果通过, 说明我们的修改不会对`abs()`函数原有的行为造成影响, 如果测试不通过, 说明我们的修改与原有行为不一致, 要么修改代码, 要么修改测试.

这种以测试为驱动的开发模式最大的好处就是确保一个程序模块的行为符合我们设计的测试用例. 在将来修改的时候, 可以极大程度地保证该模块行为仍然是正确的.

我们来编写一个`Dict`类, 这个类的行为和`dict`一致, 但是可以通过属性来访问, 用起来就像下面这样:

```python
>>> d = Dict(a=1, b=2)
>>> d['a']
1
>>> d.a
1
```

`mydict.py`代码如下:

```python
class Dict(dict):

    def __init__(self, **kw):
        super().__init__(**kw)

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            raise AttributeError(r"'Dict' object has no attribute '%s'" % key)

    def __setattr__(self, key, value):
        self[key] = value
```

为了编写单元测试, 我们需要引入Python自带的`unittest`模块, 编写`mydict_test.py`如下:

```python
import unittest

from mydict import Dict

class TestDict(unittest.TestCase):

    def test_init(self):
        d = Dict(a=1, b='test')
        self.assertEqual(d.a, 1)
        self.assertEqual(d.b, 'test')
        self.assertTrue(isinstance(d, dict))

    def test_key(self):
        d = Dict()
        d['key'] = 'value'
        self.assertEqual(d.key, 'value')

    def test_attr(self):
        d = Dict()
        d.key = 'value'
        self.assertTrue('key' in d)
        self.assertEqual(d['key'], 'value')

    def test_keyerror(self):
        d = Dict()
        with self.assertRaises(KeyError):
            value = d['empty']

    def test_attrerror(self):
        d = Dict()
        with self.assertRaises(AttributeError):
            value = d.empty
```

编写单元测试时, 我们需要编写一个测试类, 从`unittest.TestCase`继承.

以`test`开头的方法就是测试方法, 不以`test`开头的方法不被认为是测试方法, 测试的时候不会被执行.

对每一类测试都需要编写一个`test_xxx()`方法. 由于`unittest.TestCase`提供了很多内置的条件判断, 我们只需要调用这些方法就可以断言输出是否是我们所期望的.
最常用的断言就是`assertEqual()`:

```python
self.assertEqual(abs(-1), 1)
# 断言函数返回的结果与1相等
```

另一种重要的断言就是期待抛出指定类型的`Error`, 比如通过`d['empty']`访问不存在的`key`时, 断言会抛出`KeyError`:

```python
with self.assertRaises(KeyError):
    value = d['empty']
```

而通过`d.empty`访问不存在的`key`时, 我们期待抛出`AttributeError`:

```python
with self.assertRaises(AttributeError):
    value = d.empty
```

运行单元测试

一旦编写好单元测试, 我们就可以运行单元测试. 最简单的运行方式是在`mydict_test.py`的最后加上两行代码:

```python
if __name__ == '__main__':
    unittest.main()
```

这样就可以把`mydict_test.py`当做正常的python脚本运行:

```shell
$ python mydict_test.py
None
```

另一种方法是在命令行通过参数`-m unittest`直接运行单元测试:

```shell
$ python -m unittest mydict_test
.....
----------------------------------------------------------------------
Ran 5 tests in 0.000s

OK
```

这是推荐的做法, 因为这样可以一次批量运行很多单元测试, 并且, 有很多工具可以自动来运行这些单元测试.

#### setUp与tearDown

可以在单元测试中编写两个特殊的`setUp()`和`tearDown()`方法. 这两个方法会分别在每调用一个测试方法的前后分别被执行.

`setUp()`和`tearDown()`方法有什么用呢? 设想你的测试需要启动一个数据库, 这时, 就可以在`setUp()`方法中连接数据库, 在`tearDown()`方法中关闭数据库, 这样, 不必在每个测试方法中重复相同的代码:

```python
class TestDict(unittest.TestCase):

    def setUp(self):
        print('setUp...')

    def tearDown(self):
        print('tearDown...')
```

可以再次运行测试看看每个测试方法调用前后是否会打印出`setUp...`和`tearDown...`.

#### 练习-调试

对Student类编写单元测试, 结果发现测试不通过, 请修改Student类, 让测试通过:

```python
# -*- coding: utf-8 -*-
import unittest

class Student(object):

    def __init__(self, name, score):
        self.name = name
        self.score = score

    def get_grade(self):
        if 80 > self.score >= 60:
            return 'B'
        elif 100>=self.score >= 80:
            return 'A'
        elif 60>self.score>=0:
            return 'C'
        else:
            raise ValueError('%s is a cuode score' % self.score)

class TestStudent(unittest.TestCase):

    def test_80_to_100(self):
        s1 = Student('Bart', 80)
        s2 = Student('Lisa', 100)
        self.assertEqual(s1.get_grade(), 'A')
        self.assertEqual(s2.get_grade(), 'A')

    def test_60_to_80(self):
        s1 = Student('Bart', 60)
        s2 = Student('Lisa', 79)
        self.assertEqual(s1.get_grade(), 'B')
        self.assertEqual(s2.get_grade(), 'B')

    def test_0_to_60(self):
        s1 = Student('Bart', 0)
        s2 = Student('Lisa', 59)
        self.assertEqual(s1.get_grade(), 'C')
        self.assertEqual(s2.get_grade(), 'C')

    def test_invalid(self):
        s1 = Student('Bart', -1)
        s2 = Student('Lisa', 101)
        with self.assertRaises(ValueError):
            s1.get_grade()
        with self.assertRaises(ValueError):
            s2.get_grade()

if __name__ == '__main__':
    unittest.main()
```

#### 小结-单元测试

+ 单元测试可以有效地测试某个程序模块的行为, 是未来重构代码的信心保证.
+ 单元测试的测试用例要覆盖常用的输入组合, 边界条件和异常.
+ 单元测试代码要非常简单, 如果测试代码太复杂, 那么测试代码本身就可能有bug.
+ 单元测试通过了并不意味着程序就没有bug了, 但是不通过程序肯定有bug.

### 文档测试

如果你经常阅读Python的官方文档, 可以看到很多文档都有示例代码. 比如`re`模块就带了很多示例代码:

```python
>>> import re
>>> m = re.search('(?<=abc)def', 'abcdef')
>>> m.group(0)
'def'
```

可以把这些示例代码在Python的交互式环境下输入并执行, 结果与文档中的示例代码显示的一致.

这些代码与其他说明可以写在注释中, 然后, 由一些工具来自动生成文档. 既然这些代码本身就可以粘贴出来直接运行, 那么, 可不可以自动执行写在注释中的这些代码呢?

答案是肯定的.

当我们编写注释时, 如果写上这样的注释:

```python
def abs(n):
    '''
    Function to get absolute value of number.

    Example:

    >>> abs(1)
    1
    >>> abs(-1)
    1
    >>> abs(0)
    0
    '''
    return n if n >= 0 else (-n)
```

无疑更明确地告诉函数的调用者该函数的期望输入和输出.

并且, Python内置的"文档测试"(`doctest`)模块可以直接提取注释中的代码并执行测试.

`doctest`严格按照Python交互式命令行的输入和输出来判断测试结果是否正确. 只有测试异常的时候, 可以用...表示中间一大段烦人的输出.

让我们用`doctest`来测试上次编写的Dict类:

```python
# mydict2.py
class Dict(dict):
    '''
    Simple dict but also support access as x.y style.

    >>> d1 = Dict()
    >>> d1['x'] = 100
    >>> d1.x
    100
    >>> d1.y = 200
    >>> d1['y']
    200
    >>> d2 = Dict(a=1, b=2, c='3')
    >>> d2.c
    '3'
    >>> d2['empty']
    Traceback (most recent call last):
        ...
    KeyError: 'empty'
    >>> d2.empty
    Traceback (most recent call last):
        ...
    AttributeError: 'Dict' object has no attribute 'empty'
    '''
    def __init__(self, **kw):
        super(Dict, self).__init__(**kw)

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            raise AttributeError(r"'Dict' object has no attribute '%s'" % key)

    def __setattr__(self, key, value):
        self[key] = value

if __name__=='__main__':
    import doctest
    doctest.testmod()
```

运行`python mydict2.py`:

```python
$ python mydict2.py
None
```

什么输出也没有. 这说明我们编写的`doctest`运行都是正确的. 如果程序有问题, 比如把`__getattr__()`方法注释掉, 再运行就会报错:

```python
$ python mydict2.py
**********************************************************************
File "/Users/michael/Github/learn-python3/samples/debug/mydict2.py", line 10, in __main__.Dict
Failed example:
    d1.x
Exception raised:
    Traceback (most recent call last):
      ...
    AttributeError: 'Dict' object has no attribute 'x'
**********************************************************************
File "/Users/michael/Github/learn-python3/samples/debug/mydict2.py", line 16, in __main__.Dict
Failed example:
    d2.c
Exception raised:
    Traceback (most recent call last):
      ...
    AttributeError: 'Dict' object has no attribute 'c'
**********************************************************************
1 items had failures:
   2 of   9 in __main__.Dict
***Test Failed*** 2 failures.
```

注意到最后3行代码. 当模块正常导入时, `doctest`不会被执行. 只有在命令行直接运行时, 才执行`doctest`. 所以, 不必担心`doctest`会在非测试环境下执行.

```python
if __name__=='__main__':
    import doctest
    doctest.testmod()
```

当我们在命令行运行 `hello` 模块文件时,
Python解释器把特殊变量 `__name__` 设置为 `__main__`,
而如果在其他地方导入该 `hello` 模块时, `if`判断将失败.
因此, 这种 `if` 测试的作用是: 
当模块通过命令行运行时, 令它执行一些额外代码, 最常见的就是运行测试.

#### 练习-文档测试

对函数`fact(n)`编写`doctest`并执行:

```python
# -*- coding: utf-8 -*-
def fact(n):
    '''
    Calculate 1*2*...*n

    >>> fact(1)
    1
    >>> fact(10)
    3628800
    >>> fact(-1)
    Traceback (most recent call last):
        ...
    ValueError
    '''
    if n < 1:
        raise ValueError()
    if n == 1:
        return 1
    return n * fact(n - 1)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
```

#### 小结-文档测试

`doctest`非常有用, 不但可以用来测试, 还可以直接作为示例代码. 通过某些文档生成工具, 就可以自动把包含`doctest`的注释提取出来. 用户看文档的时候, 同时也看到了`doctest`.

## IO编程

IO在计算机中指Input/Output, 也就是输入和输出. 由于程序和运行时数据是在内存中驻留, 由CPU这个超快的计算核心来执行, 涉及到数据交换的地方, 通常是磁盘, 网络等, 就需要IO接口.

比如你打开浏览器, 访问新浪首页, 浏览器这个程序就需要通过网络IO获取新浪的网页. 浏览器首先会发送数据给新浪服务器, 告诉它我想要首页的HTML, 这个动作是往外发数据, 叫Output, 随后新浪服务器把网页发过来, 这个动作是从外面接收数据, 叫Input. 所以, 通常, 程序完成IO操作会有Input和Output两个数据流. 当然也有只用一个的情况, 比如, 从磁盘读取文件到内存, 就只有Input操作, 反过来, 把数据写到磁盘文件里, 就只是一个Output操作.

IO编程中, Stream(流)是一个很重要的概念, 可以把流想象成一个水管, 数据就是水管里的水, 但是只能单向流动. Input Stream就是数据从外面(磁盘, 网络)流进内存, Output Stream就是数据从内存流到外面去. 对于浏览网页来说, 浏览器和新浪服务器之间至少需要建立两根水管, 才可以既能发数据, 又能收数据.

由于CPU和内存的速度远远高于外设的速度, 所以, 在IO编程中, 就存在速度严重不匹配的问题. 举个例子来说, 比如要把100M的数据写入磁盘, CPU输出100M的数据只需要0.01秒, 可是磁盘要接收这100M数据可能需要10秒, 怎么办呢? 有两种办法:

第一种是CPU等着, 也就是程序暂停执行后续代码, 等100M的数据在10秒后写入磁盘, 再接着往下执行, 这种模式称为同步IO;

另一种方法是CPU不等待, 只是告诉磁盘, "您老慢慢写, 不着急, 我接着干别的事去了", 于是, 后续代码可以立刻接着执行, 这种模式称为异步IO.

同步和异步的区别就在于是否等待IO执行的结果. 好比你去麦当劳点餐, 你说"来个汉堡", 服务员告诉你, 对不起, 汉堡要现做, 需要等5分钟, 于是你站在收银台前面等了5分钟, 拿到汉堡再去逛商场, 这是同步IO.

你说"来个汉堡", 服务员告诉你, 汉堡需要等5分钟, 你可以先去逛商场, 等做好了, 我们再通知你, 这样你可以立刻去干别的事情(逛商场), 这是异步IO.

很明显, 使用异步IO来编写程序性能会远远高于同步IO, 但是异步IO的缺点是编程模型复杂. 想想看, 你得知道什么时候通知你"汉堡做好了", 而通知你的方法也各不相同. 如果是服务员跑过来找到你, 这是回调模式, 如果服务员发短信通知你, 你就得不停地检查手机, 这是轮询模式. 总之, 异步IO的复杂度远远高于同步IO.

操作IO的能力都是由操作系统提供的, 每一种编程语言都会把操作系统提供的低级C接口封装起来方便使用, Python也不例外. 我们后面会详细讨论Python的IO编程接口.

注意, 本章的IO编程都是同步模式, 异步IO由于复杂度太高, 后续涉及到服务器端程序开发时我们再讨论

小结-io编程

+ 同步IO:  顺序执行, 等结果, 不复杂
+ 异步IO:  异步执行, 不等结果, 复杂

### 文件读写

读写文件是最常见的IO操作. Python内置了读写文件的函数, 用法和C是兼容的.

读写文件前, 我们先必须了解一下, 在磁盘上读写文件的功能都是由操作系统提供的, 现代操作系统不允许普通的程序直接操作磁盘, 所以, 读写文件就是请求操作系统打开一个文件对象(通常称为文件描述符), 然后, 通过操作系统提供的接口从这个文件对象中读取数据(读文件), 或者把数据写入这个文件对象(写文件).

#### 读文件

要以读文件的模式打开一个文件对象, 使用Python内置的`open()`函数, 传入文件名和标示符:

```python
>>> f = open('/Users/thomas/desktop/test.py', 'r')
```

标示符`'r'`表示读, 这样, 我们就成功地打开了一个文件.

如果文件不存在, `open()`函数就会抛出一个`IOError`的错误, 并且给出错误码和详细的信息告诉你文件不存在:

```python
>>> f=open('/Users/michael/notfound.txt', 'r')
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
FileNotFoundError: [Errno 2] No such file or directory: '/Users/michael/notfound.txt'
```

如果文件打开成功, 接下来, 调用`read()`方法可以一次读取文件的全部内容, Python把内容读到内存, 用一个`str`对象表示:

```python
>>> f.read()
'Hello, world!'
```

最后一步是调用`close()`方法关闭文件. 文件使用完毕后必须关闭, 因为文件对象会占用操作系统的资源, 并且操作系统同一时间能打开的文件数量也是有限的:

```python
>>> f.close()
```

由于文件读写时都有可能产生`IOError`, 一旦出错, 后面的`f.close()`就不会调用. 所以, 为了保证无论是否出错都能正确地关闭文件, 我们可以使用`try ... finally`来实现:

```python
try:
    f = open('/path/to/file', 'r')
    print(f.read())
finally:
    if f:
        f.close()
```

但是每次都这么写实在太繁琐, 所以, Python引入了`with`语句来自动帮我们调用`close()`方法:

```python
with open('/path/to/file', 'r') as f:
    print(f.read())
```

这和前面的`try ... finally`是一样的, 但是代码更佳简洁, 并且不必调用`f.close()`方法.

调用`read()`会一次性读取文件的全部内容, 如果文件有10G, 内存就爆了, 所以, 要保险起见, 可以反复调用`read(size)`方法, 每次最多读取`size`个字节的内容. 另外, 调用`readline()`可以每次读取一行内容, 调用`readlines()`一次读取所有内容并按行返回list. 因此, 要根据需要决定怎么调用.

如果文件很小, `read()`一次性读取最方便; 如果不能确定文件大小, 反复调用`read(size)`比较保险; 如果是配置文件, 调用`readlines()`最方便:

```python
for line in f.readlines():
    print(line.strip()) # 把末尾的'\n'删掉
```

#### file-like Object

像`open()`函数返回的这种有个`read()`方法的对象, 在Python中统称为`file-like Object`. 除了file外, 还可以是内存的字节流, 网络流, 自定义流等等. `file-like Object`不要求从特定类继承, 只要写个`read()`方法就行.

StringIO就是在内存中创建的`file-like Object`, 常用作临时缓冲.

#### 二进制文件

前面讲的默认都是读取文本文件, 并且是`UTF-8`编码的文本文件. 要读取二进制文件, 比如图片, 视频等等, 用'`rb`'模式打开文件即可:

```python
>>> f = open('/Users/michael/test.jpg', 'rb')
>>> f.read()
b'\xff\xd8\xff\xe1\x00\x18Exif\x00\x00...' # 十六进制表示的字节
```

#### 字符编码

要读取非`UTF-8`编码的文本文件, 需要给`open()`函数传入`encoding`参数, 例如, 读取GBK编码的文件:

```python
>>> f = open('/Users/michael/gbk.txt', 'r', encoding='gbk')
>>> f.read()
'测试'
```

遇到有些编码不规范的文件, 你可能会遇到`UnicodeDecodeError`, 因为在文本文件中可能夹杂了一些非法编码的字符. 遇到这种情况, `open()`函数还接收一个`errors`参数, 表示如果遇到编码错误后如何处理. 最简单的方式是直接忽略:

```python
>>> f = open('/Users/michael/gbk.txt', 'r', encoding='gbk', errors='ignore')
```

#### 写文件

写文件和读文件是一样的, 唯一区别是调用`open()`函数时, 传入标识符`'w'`或者`'wb'`表示写文本文件或写二进制文件:

```python
>>> f = open('/Users/michael/test.txt', 'w')
>>> f.write('Hello, world!')
>>> f.close()
```

你可以反复调用`write()`来写入文件, 但是务必要调用`f.close()`来关闭文件. 当我们写文件时, 操作系统往往不会立刻把数据写入磁盘, 而是放到内存缓存起来, 空闲的时候再慢慢写入. 只有调用`close()`方法时, 操作系统才保证把没有写入的数据全部写入磁盘. 忘记调用`close()`的后果是数据可能只写了一部分到磁盘, 剩下的丢失了. 所以, 还是用`with`语句来得保险:

```python
with open('/Users/michael/test.txt', 'w') as f:
    f.write('Hello, world!')
```

要写入特定编码的文本文件, 请给`open()`函数传入`encoding`参数, 将字符串自动转换成指定编码.

细心的童鞋会发现, 以'`w`'模式写入文件时, 如果文件已存在, 会直接覆盖(相当于删掉后新写入一个文件). 如果我们希望追加到文件末尾怎么办? 可以传入`'a'`以追加(`append`)模式写入.

所有模式的定义及含义可以参考 [Python的官方文档][].

[Python的官方文档]: https://docs.python.org/3/library/functions.html#open

#### 练习

请将本地一个文本文件读为一个str并打印出来:

```python
# -*- coding: utf-8 -*-
fpath = r'C:\Windows\system.ini'

with open(fpath, 'r') as f:
    s = f.read()
    print(s)

# 运行代码观察结果
```

#### 小结-文件读写

+ 在Python中, 文件读写是通过`open()`函数打开的文件对象完成的.
+ 使用`with`语句操作文件IO是个好习惯.

### StringIO和BytesIO

#### StringIO

很多时候, 数据读写不一定是文件, 也可以在内存中读写.
`StringIO`顾名思义就是在内存中读写str.
要把str写入`StringIO`, 我们需要先创建一个`StringIO`, 然后, 像文件一样写入即可:

```python
>>> from io import StringIO
>>> f = StringIO()
>>> f.write('hello')
5
>>> f.write(' ')
1
>>> f.write('world!')
6
>>> print(f.getvalue())
hello world!
```

`getvalue()`方法用于获得写入后的`str`.

要读取`StringIO`, 可以用一个`str`初始化`StringIO`, 然后, 像读文件一样读取:

```python
>>> from io import StringIO
>>> f = StringIO('Hello!\nHi!\nGoodbye!')
>>> while True:
...     s = f.readline()
...     if s == '':
...         break
...     print(s.strip())
...
Hello!
Hi!
Goodbye!
```

#### BytesIO

`StringIO`操作的只能是`str`, 如果要操作二进制数据, 就需要使用`BytesIO`.

`BytesIO`实现了在内存中读写bytes, 我们创建一个`BytesIO`, 然后写入一些`bytes`:

```python
>>> from io import BytesIO
>>> f = BytesIO()
>>> f.write('中文'.encode('utf-8'))
6
>>> print(f.getvalue())
b'\xe4\xb8\xad\xe6\x96\x87'
```

请注意, 写入的不是`str`, 而是经过`UTF-8`编码的`bytes`.

和`StringIO`类似, 可以用一个`bytes`初始化`BytesIO`, 然后, 像读文件一样读取:

```python
>>> from io import BytesIO
>>> f = BytesIO(b'\xe4\xb8\xad\xe6\x96\x87')
>>> f.read()
b'\xe4\xb8\xad\xe6\x96\x87'
```

#### 小结-stringio-and-bytesio

`StringIO`和`BytesIO`是在内存中操作`str`和`bytes`的方法,
使得和读写文件具有一致的接口.

### 操作文件和目录

如果我们要操作文件, 目录, 可以在命令行下面输入操作系统提供的各种命令来完成. 比如`dir`, `cp`等命令.

如果要在Python程序中执行这些目录和文件的操作怎么办?
其实操作系统提供的命令只是简单地调用了操作系统提供的接口函数,
Python内置的`os`模块也可以直接调用操作系统提供的接口函数.

打开Python交互式命令行, 我们来看看如何使用`os`模块的基本功能:

```python
>>> import os
>>> os.name # 操作系统类型
'posix'
```

如果是`posix`, 说明系统是`Linux`, `Unix`或`Mac OS X`, 如果是`nt`, 就是`Windows`系统.

要获取详细的系统信息, 可以调用`uname()`函数:

```python
>>> os.uname()
('Linux', 'OP7050', '4.15.0-112-generic', '#113-Ubuntu SMP Thu Jul 9 23:41:39 UTC 2020', 'x86_64')
```

注意`uname()`函数在Windows上不提供, 也就是说, `os`模块的某些函数是跟操作系统相关的.

#### 环境变量

在操作系统中定义的环境变量, 全部保存在`os.environ`这个变量中, 可以直接查看:

```python
>>> os.environ
environ({'VERSIONER_PYTHON_PREFER_32_BIT': 'no', 'TERM_PROGRAM_VERSION': '326', 'LOGNAME': 'michael', 'USER': 'michael', 'PATH': '/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/mysql/bin', ...})
```

要获取某个环境变量的值, 可以调用`os.environ.get('key')`:

```python
>>> os.environ.get('PATH')
'/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/mysql/bin'
>>> os.environ.get('x', 'default')
'default'
```

#### 操作文件和目录-2

操作文件和目录的函数一部分放在`os`模块中, 一部分放在`os.path`模块中, 这一点要注意一下.
查看, 创建和删除目录可以这么调用:

```python
# 查看当前目录的绝对路径:
>>> os.path.abspath('.')
'/Users/michael'
# 在某个目录下创建一个新目录, 首先把新目录的完整路径表示出来:
>>> os.path.join('/Users/michael', 'testdir')
'/Users/michael/testdir'
# 然后创建一个目录:
>>> os.mkdir('/Users/michael/testdir')
# 删掉一个目录:
>>> os.rmdir('/Users/michael/testdir')
```

***
把两个路径合成一个时, 不要直接拼字符串, 而要通过`os.path.join()`函数, 这样可以正确处理不同操作系统的路径分隔符.
在`Linux/Unix/Mac`下, `os.path.join()`返回这样的字符串:
`part-1/part-2`
而`Windows`下会返回这样的字符串:
`part-1\part-2`

同样的道理, 要拆分路径时, 也不要直接去拆字符串, 而要通过`os.path.split()`函数, 这样可以把一个路径拆分为两部分, 后一部分总是最后级别的目录或文件名:

```python
>>> os.path.split('/Users/michael/testdir/file.txt')
('/Users/michael/testdir', 'file.txt')
```

`os.path.splitext()`可以直接让你得到文件扩展名, 很多时候非常方便:

```python
>>> os.path.splitext('/path/to/file.txt')
('/path/to/file', '.txt')
```

这些合并, 拆分路径的函数**并不要求目录和文件要真实存在**, 它们只对字符串进行操作.

文件操作使用下面的函数. 假定当前目录下有一个`test.txt`文件:

```python
# 对文件重命名:
>>> os.rename('test.txt', 'test.py')
# 删掉文件:
>>> os.remove('test.py')
```

但是复制文件的函数居然在`os`模块中不存在! 原因是复制文件并非由操作系统提供的系统调用.
理论上讲, 我们通过上一节的读写文件可以完成文件复制, 只不过要多写很多代码.

幸运的是`shutil`模块提供了`copyfile()`的函数, 你还可以在`shutil`模块中找到很多实用函数, 它们可以看做是`os`模块的补充.

最后看看如何利用Python的特性来过滤文件.
比如我们要列出当前目录下的所有**目录**, 只需要一行代码:

```python
>>> [x for x in os.listdir('.') if os.path.isdir(x)]
['.lein', '.local', '.m2', '.npm', '.ssh', '.Trash', '.vim', 'Applications', 'Desktop', ...]
```

要列出所有的`.py`文件, 也只需一行代码:

```python
>>> [x for x in os.listdir('.') if os.path.isfile(x) and os.path.splitext(x)[1]=='.py']
['apis.py', 'config.py', 'models.py', 'pymonitor.py', 'test_db.py', 'urls.py', 'wsgiapp.py']
```

是不是非常简洁?

#### 小结-操作文件和目录

Python的`os`模块封装了操作系统的目录和文件操作, 要注意这些函数有的在`os`模块中, 有的在`os.path`模块中.

#### 练习-操作文件和目录

1. 利用os模块编写一个能实现`dir -l`输出的程序.
1. 编写一个程序, 能在当前目录以及当前目录的所有子目录下查找文件名包含指定字符串的文件, 并打印出相对路径.

### 序列化

在程序运行的过程中, 所有的变量都是在内存中, 比如, 定义一个dict:

```python
d = dict(name='Bob', age=20, score=88)
```

可以随时修改变量, 比如把`name`改成`'Bill'`, 但是一旦程序结束, 变量所占用的内存就被操作系统全部回收. 如果没有把修改后的`'Bill'`存储到磁盘上, 下次重新运行程序, 变量又被初始化为`'Bob'`.

我们把变量从内存中变成可存储或传输的过程称之为序列化, 在Python中叫`pickling`, 在其他语言中也被称之为`serialization`, `marshalling`, `flattening`等等, 都是一个意思.

序列化之后, 就可以把序列化后的内容写入磁盘, 或者通过网络传输到别的机器上.

反过来, 把变量内容从序列化的对象重新读到内存里称之为反序列化, 即`unpickling`.

Python提供了`pickle`模块来实现序列化.

首先, 我们尝试把一个对象序列化并写入文件:

```python
>>> import pickle
>>> d = dict(name='Bob', age=20, score=88)
>>> pickle.dumps(d)
b'\x80\x03}q\x00(X\x03\x00\x00\x00ageq\x01K\x14X\x05\x00\x00\x00scoreq\x02KXX\x04\x00\x00\x00nameq\x03X\x03\x00\x00\x00Bobq\x04u.'
```

`pickle.dumps()`方法把任意对象序列化成一个`bytes`, 然后, 就可以把这个`bytes`写入文件.
或者用另一个方法`pickle.dump()`直接把对象序列化后写入一个`file-like Object`:

```python
>>> f = open('dump.txt', 'wb')
>>> pickle.dump(d, f)
>>> f.close()
```

看看写入的`dump.txt`文件, 一堆乱七八糟的内容, 这些都是Python保存的对象内部信息.

当我们要把对象从磁盘读到内存时, 可以先把内容读到一个`bytes`, 然后用`pickle.loads()`方法反序列化出对象, 也可以直接用`pickle.load()`方法从一个`file-like Object`中直接反序列化出对象. 我们打开另一个Python命令行来反序列化刚才保存的对象:

```python
>>> f = open('dump.txt', 'rb')
>>> d = pickle.load(f)
>>> f.close()
>>> d
{'age': 20, 'score': 88, 'name': 'Bob'}
```

变量的内容又回来了!

当然, 这个变量和原来的变量是完全不相干的对象, 它们只是内容相同而已.

Pickle的问题和所有其他编程语言特有的序列化问题一样, 就是它只能用于Python, 并且可能不同版本的Python彼此都不兼容, 因此, 只能用Pickle保存那些不重要的数据, 不能成功地反序列化也没关系.

#### JSON

如果我们要在不同的编程语言之间传递对象, 就必须把对象序列化为标准格式, 比如`XML`, 但更好的方法是序列化为`JSON`, 因为`JSON`表示出来就是一个字符串, 可以被所有语言读取, 也可以方便地存储到磁盘或者通过网络传输. `JSON`不仅是标准格式, 并且比XML更快, 而且可以直接在Web页面中读取, 非常方便.

`JSON`表示的对象就是标准的JavaScript语言的对象, `JSON`和Python内置的数据类型对应如下:

|JSON类型|Python类型|
| ----- | ----- |
|  {} | dict |
| [] | list |
| "string" | str |
| 1234.56 | int或float |
| true/false | True/False |
| null | None |

Python内置的`json`模块提供了非常完善的Python对象到`JSON`格式的转换. 我们先看看如何把Python对象变成一个`JSON`:

```python
>>> import json
>>> d = dict(name='Bob', age=20, score=88)
>>> json.dumps(d)
'{"age": 20, "score": 88, "name": "Bob"}'
```

`dumps()`方法返回一个`str`, 内容就是标准的`JSON`. 类似的, `dump()`方法可以直接把`JSON`写入一个file-like Object.

要把`JSON`反序列化为Python对象, 用`loads()`或者对应的`load()`方法, 前者把`JSON`的字符串反序列化, 后者从`file-like Object`中读取字符串并反序列化:

```python
>>> json_str = '{"age": 20, "score": 88, "name": "Bob"}'
>>> json.loads(json_str)
{'age': 20, 'score': 88, 'name': 'Bob'}
```

由于JSON标准规定JSON编码是`UTF-8`, 所以我们总是能正确地在Python的`str`与`JSON`的字符串之间转换.

#### JSON进阶

Python的dict对象可以直接序列化为JSON的`{}`, 不过, 很多时候, 我们更喜欢用`class`表示对象, 比如定义Student类, 然后序列化:

```python
import json

class Student(object):
    def __init__(self, name, age, score):
        self.name = name
        self.age = age
        self.score = score

s = Student('Bob', 20, 88)
print(json.dumps(s))
```

运行代码, 毫不留情地得到一个`TypeError`:

```python
Traceback (most recent call last):
  ...
TypeError: <__main__.Student object at 0x10603cc50> is not JSON serializable
```

错误的原因是Student对象不是一个可序列化为`JSON`的对象.

如果连class的实例对象都无法序列化为JSON, 这肯定不合理!

别急, 我们仔细看看`dumps()`方法的参数列表, 可以发现, 除了第一个必须的obj参数外, `dumps()`方法还提供了一大堆的可选参数:

[dumps()方法的参数列表][]

[dumps()方法的参数列表]: https://docs.python.org/3/library/json.html#json.dumps

这些可选参数就是让我们来定制JSON序列化. 前面的代码之所以无法把Student类实例序列化为JSON, 是因为默认情况下, `dumps()`方法不知道如何将Student实例变为一个JSON的`{}`对象.

可选参数`default`就是把任意一个对象变成一个可序列为JSON的对象,
我们只需要为Student专门写一个转换函数, 再把函数传进去即可:

```python
def student2dict(std):
    return {
        'name': std.name,
        'age': std.age,
        'score': std.score
    }
```

这样, Student实例首先被`student2dict()`函数转换成dict, 然后再被顺利序列化为JSON:

```python
>>> print(json.dumps(s, default=student2dict))
{"age": 20, "name": "Bob", "score": 88}
```

不过, 下次如果遇到一个`Teacher`类的实例, 照样无法序列化为JSON. 我们可以偷个懒, 把任意class的实例变为dict:

```python
print(json.dumps(s, default=lambda obj: obj.__dict__))
```

因为通常class的实例都有一个`__dict__`属性, 它就是一个`dict`, 用来存储实例变量. 也有少数例外, 比如定义了`__slots__`的`class`.

同样的道理, 如果我们要把JSON反序列化为一个Student对象实例, `loads()`方法首先转换出一个dict对象, 然后, 我们传入的`object_hook`函数负责把dict转换为Student实例:

```python
def dict2student(d):
    return Student(d['name'], d['age'], d['score'])
```

运行结果如下:

```python
>>> json_str = '{"age": 20, "score": 88, "name": "Bob"}'
>>> print(json.loads(json_str, object_hook=dict2student))
<__main__.Student object at 0x10cd3c190>
```

打印出的是反序列化的Student实例对象.

#### 练习-序列化

对中文进行JSON序列化时, `json.dumps()`提供了一个`ensure_ascii`参数, 观察该参数对结果的影响:

```python
# -*- coding: utf-8 -*-

import json
obj = dict(name='小明', age=20)
s = json.dumps(obj, ensure_ascii=True)

print(s)
```

`ensure_ascii`默认情况是`True`, 这时会把中文转成`Unicode码`, 设置成`False`的话就是打印中文

#### 小结-序列化

Python语言特定的序列化模块是`pickle`, 但如果要把序列化搞得更通用, 更符合Web标准, 就可以使用`json`模块.

`json`模块的`dumps()`和`loads()`函数是定义得非常好的接口的典范. 当我们使用时, 只需要传入一个必须的参数. 但是, 当默认的序列化或反序列机制不满足我们的要求时, 我们又可以传入更多的参数来定制序列化或反序列化的规则, 既做到了接口简单易用, 又做到了充分的扩展性和灵活性.

## 进程和线程

很多同学都听说过, 现代操作系统比如Mac OS X, UNIX, Linux, Windows等, 都是支持"多任务"的操作系统.

什么叫"多任务"呢? 简单地说, 就是操作系统可以同时运行多个任务. 打个比方, 你一边在用浏览器上网, 一边在听MP3, 一边在用Word赶作业, 这就是多任务, 至少同时有3个任务正在运行. 还有很多任务悄悄地在后台同时运行着, 只是桌面上没有显示而已.

现在, 多核CPU已经非常普及了, 但是, 即使过去的单核CPU, 也可以执行多任务. 由于CPU执行代码都是顺序执行的, 那么, 单核CPU是怎么执行多任务的呢?

答案就是操作系统轮流让各个任务交替执行, 任务1执行0.01秒, 切换到任务2, 任务2执行0.01秒, 再切换到任务3, 执行0.01秒... ... 这样反复执行下去. 表面上看, 每个任务都是交替执行的, 但是, 由于CPU的执行速度实在是太快了, 我们感觉就像所有任务都在同时执行一样.

真正的并行执行多任务只能在多核CPU上实现, 但是, 由于任务数量远远多于CPU的核心数量, 所以, 操作系统也会自动把很多任务轮流调度到每个核心上执行.

对于操作系统来说, 一个任务就是一个进程(`Process`), 比如打开一个浏览器就是启动一个浏览器进程, 打开一个记事本就启动了一个记事本进程, 打开两个记事本就启动了两个记事本进程, 打开一个Word就启动了一个Word进程.

有些进程还不止同时干一件事, 比如Word, 它可以同时进行打字, 拼写检查, 打印等事情.
在一个进程内部, 要同时干多件事, 就需要同时运行多个"子任务", 我们把进程内的这些"子任务"称为线程(`Thread`).

由于每个进程至少要干一件事, 所以, 一个进程至少有一个线程. 当然, 像Word这种复杂的进程可以有多个线程, 多个线程可以同时执行, 多线程的执行方式和多进程是一样的, 也是由操作系统在多个线程之间快速切换, 让每个线程都短暂地交替运行, 看起来就像同时执行一样. 当然, 真正地同时执行多线程需要多核CPU才可能实现.

我们前面编写的所有的Python程序, 都是执行单任务的进程, 也就是只有一个线程. 如果我们要同时执行多个任务怎么办?

有两种解决方案:

一种是启动多个进程, 每个进程虽然只有一个线程, 但多个进程可以一块执行多个任务.

还有一种方法是启动一个进程, 在一个进程内启动多个线程, 这样, 多个线程也可以一块执行多个任务.

当然还有第三种方法, 就是启动多个进程, 每个进程再启动多个线程, 这样同时执行的任务就更多了, 当然这种模型更复杂, 实际很少采用.

总结一下就是, 多任务的实现有3种方式:

+ 多进程模式;
+ 多线程模式;
+ 多进程+多线程模式.

同时执行多个任务通常各个任务之间并不是没有关联的, 而是需要相互通信和协调, 有时, `任务1`必须暂停等待`任务2`完成后才能继续执行, 有时, `任务3`和`任务4`又不能同时执行, 所以, 多进程和多线程的程序的复杂度要远远高于我们前面写的单进程单线程的程序.

因为复杂度高, 调试困难, 所以, 不是迫不得已, 我们也不想编写多任务. 但是, 有很多时候, 没有多任务还真不行.
想想在电脑上看电影, 就必须由一个线程播放视频, 另一个线程播放音频, 否则, 单线程实现的话就只能先把视频播放完再播放音频, 或者先把音频播放完再播放视频, 这显然是不行的.

Python既支持多进程, 又支持多线程, 我们会讨论如何编写这两种多任务程序.

小结

线程是最小的执行单元, 而进程由至少一个线程组成.
如何调度进程和线程, 完全由操作系统决定, 程序自己不能决定什么时候执行, 执行多长时间.

多进程和多线程的程序涉及到同步, 数据共享的问题, 编写起来更复杂.

### multiprocessing

要让Python程序实现多process(`multiprocessing`), 我们先了解操作系统的相关知识.

`Unix/Linux`操作系统提供了一个`fork()`系统调用, 它非常特殊. 普通的函数调用, 调用一次, 返回一次, 但是`fork()`调用一次, 返回两次, 因为操作系统自动把当前process(称为父process)复制了一份(称为子process), 然后, 分别在父process和子process内返回.

子process永远返回`0`, 而父process返回子process的`ID`. 这样做的理由是, 一个父process可以`fork`出很多子process, 所以, 父process要记下每个子process的`ID`, 而子process只需要调用`getppid()`就可以拿到父process的`ID`.

Python的`os`模块封装了常见的系统调用, 其中就包括`fork`, 可以在Python程序中轻松创建子process:

```python
import os

print('Process (%s) start...' % os.getpid())
# Only works on Unix/Linux/Mac:
pid = os.fork()
if pid == 0:
    print('I am child process (%s) and my parent is %s.' % (os.getpid(), os.getppid()))
else:
    print('I (%s) just created a child process (%s).' % (os.getpid(), pid))
```

运行结果如下:

```python
Process (876) start...
I (876) just created a child process (877).
I am child process (877) and my parent is 876.
```

由于Windows没有`fork`调用, 上面的代码在Windows上无法运行.
而Mac系统是基于BSD(Unix的一种)内核, 所以, 在Mac下运行是没有问题的.

有了`fork`调用, 一个process在接到新任务时就可以复制出一个子process来处理新任务, 常见的`Apache`服务器就是由父process监听端口, 每当有新的`http`请求时, 就`fork`出子process来处理新的http请求.

#### multiprocessing

如果你打算编写多process的服务程序, `Unix/Linux`无疑是正确的选择.
由于Windows没有`fork`调用, 难道在Windows上无法用Python编写多process的程序?

由于Python是跨平台的, 自然也应该提供一个跨平台的多process支持. `multiprocessing`模块就是跨平台版本的多process模块.

`multiprocessing`模块提供了一个Process类来代表一个process对象, 下面的例子演示了启动一个子process并等待其结束:

```python
from multiprocessing import Process
import os

# 子process要执行的代码
def run_proc(name):
    print('Run child process %s (%s)...' % (name, os.getpid()))

if __name__=='__main__':
    print('Parent process %s.' % os.getpid())
    p = Process(target=run_proc, args=('test',))
    print('Child process will start.')
    p.start()
    p.join()
    print('Child process end.')
```

执行结果如下:

```python
Parent process 928.
Child process will start.
Run child process test (929)...
Child process end.
```

创建子process时, 只需要传入一个执行函数和函数的参数,
创建一个Process实例, 用`start()`方法启动, 这样创建process比`fork()`还要简单.

`join()`方法可以等待子process结束后再继续往下运行, 通常用于process间的同步.

#### Pool

如果要启动大量的子process, 可以用`process池`的方式批量创建子process:

```python
import multiprocessing
import os, time, random

def long_time_task(name):
    print('Run task %s (%s)...' % (name, os.getpid())) #打印process名字和号码
    start = time.time() # 打印开始时间
    time.sleep(random.random() * 3)
    end = time.time() # 打印结束时间
    print('Task %s runs %0.2f seconds.' % (name, (end - start)))

if __name__=='__main__':
    print('Parent process %s.' % os.getpid())
    p = multiprocessing.Pool(4)
    for i in range(5):
        p.apply_async(long_time_task, args=(i,))
    print('Waiting for all subprocesses done...')
    p.close()
    p.join()
    print('All subprocesses done.')
```

执行结果如下:

```python
Parent process 669.
...
Task 4 runs 1.91 seconds.
All subprocesses done.
```

代码解读:

对`Pool`对象调用`join()`方法会等待所有子process执行完毕, 调用`join()`之前必须先调用`close()`, 调用`close()`之后就不能继续添加新的Process了.

请注意输出的结果, `task 0`, `1`, `2`, `3`是立刻执行的, 而`task 4`要等待前面某个`task`完成后才执行, 这是因为`Pool`的默认大小在我的电脑上是`4`, 因此, 最多同时执行`4`个process. 这是`Pool`有意设计的限制, 并不是操作系统的限制. 如果改成:

```python
p = Pool(5)
```

就可以同时跑`5`个process.

由于Pool的默认大小是`CPU`的核数, 如果你不幸拥有8核`CPU`, 你要提交至少9个子process才能看到上面的等待效果.

#### 子process

很多时候, 子process并不是自身, 而是一个外部process.
我们创建了子process后, 还需要控制子process的输入和输出.

`subprocess`模块可以让我们非常方便地启动一个子process, 然后控制其输入和输出.

下面的例子演示了如何在Python代码中运行命令`nslookup www.python.org`,
这和命令行直接运行的效果是一样的:

```python
import subprocess

print('$ nslookup www.python.org')
r = subprocess.call(['nslookup', 'www.python.org'])
print('Exit code:', r)
```

运行结果:

```python
$ nslookup www.python.org
Server: 192.168.19.4
...

Exit code: 0
```

如果子process还需要输入, 则可以通过`communicate()`方法输入:

```python
import subprocess

print('$ nslookup')
p = subprocess.Popen(
    ['nslookup'],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE
    )
output, err = p.communicate(b'set q=mx\npython.org\nexit\n')
print(output.decode('utf-8'))
print('Exit code:', p.returncode)
```

上面的代码相当于在命令行执行命令`nslookup`, 然后手动输入:

```python
set q=mx
python.org
exit
```

运行结果如下:

```python
$ nslookup
Server: 192.168.19.4
Address:    192.168.19.4#53

Non-authoritative answer:
python.org  mail exchanger = 50 mail.python.org.

Authoritative answers can be found from:
mail.python.org internet address = 82.94.164.166
mail.python.org has AAAA address 2001:888:2000:d::a6

Exit code: 0
```

#### process间通信

Process之间肯定是需要通信的, 操作系统提供了很多机制来实现process间的通信. Python的`multiprocessing`模块包装了底层的机制, 提供了`Queue`, `Pipes`等多种方式来交换数据.

我们以`Queue`为例, 在父process中创建两个子process, 一个往`Queue`里写数据, 一个从`Queue`里读数据:

```python
from multiprocessing import Process, Queue
import os, time, random

# 写数据process执行的代码:
def write(q):
    print('Process to write: %s' % os.getpid())
    for value in ['A', 'B', 'C']:
        print('Put %s to queue...' % value)
        q.put(value)
        time.sleep(random.random())

# 读数据process执行的代码:
def read(q):
    print('Process to read: %s' % os.getpid())
    while True:
        value = q.get(True)
        print('Get %s from queue.' % value)

if __name__=='__main__':
    # 父process创建Queue, 并传给各个子process:
    q = Queue()
    pw = Process(target=write, args=(q,))
    pr = Process(target=read, args=(q,))
    # 启动子processpw, 写入:
    pw.start()
    # 启动子processpr, 读取:
    pr.start()
    # 等待pw结束:
    pw.join()
    # prprocess里是死循环, 无法等待其结束, 只能强行终止:
    pr.terminate()
```

运行结果如下:

```python
Process to write: 50563
Put A to queue...
Process to read: 50564
Get A from queue.
Put B to queue...
Get B from queue.
Put C to queue...
Get C from queue.
```

在Unix/Linux下, multiprocessing模块封装了`fork()`调用, 使我们不需要关注`fork()`的细节. 由于Windows没有fork调用, 因此, multiprocessing需要"模拟"出fork的效果, 父process所有Python对象都必须通过`pickle`序列化再传到子process去, 所以, 如果multiprocessing在Windows下调用失败了, 要先考虑是不是`pickle`失败了.

#### 小结-多process

+ 在Unix/Linux下, 可以使用`fork()`调用实现多process.
+ 要实现跨平台的多process, 可以使用`multiprocessing`模块.
+ process间通信是通过`Queue`, `Pipes`等实现的.

#### 理解-2

[为什么猫不吃鱼的理解][]

[为什么猫不吃鱼的理解]: https://www.liaoxuefeng.com/wiki/1016959663602400/1017628290184064#0

最开始看这一节的时候真的是一脸蒙蔽. . . 估计很多人开始也是这样. .

但现在突然觉得挺简单的, 回头来写点自己的理解:

**保证在座的各位都能看懂的版本! **

先说两句:
什么是process? 有什么用?
 -- ok, 那我问你, 你能一手画圆一手画方吗?
 -- 我猜不能.

但计算机就不一样了, 一边绘制正方体一边绘制球体都是小case(屏幕上自动绘制图形),
这是因为计算机启动了另一个"大脑"来处理另一个任务, 即两个"大脑"分别同时画两个图形 效率X2!
我们之前的写程序都是计算机一个"大脑"在工作! ok, 那怎么启动计算机其他的大脑呢?
 -- 启动另一个process就可以了!

##### 创建process

```python
import multiprocessing
import time

def action(a, b):  # 待会两个process要执行的任务↓
    for i in range(30):  # 循环30次
        print(a, ' ', b)
        time.sleep(0.1)  # 等待0.1s

if __name__ == '__main__':
# 这行代码很重要, 新建process的时候都加上它! ! 原因不用管(我也不知道233)
# 在命令行执行.py文件的时候, 才会执行下面的语句

    jc1 = multiprocessing.Process(target=action, args=('process一', 0))  # 准备建立一个process: multiprocessing.Process()
    jc2 = multiprocessing.Process(target=action, args=('process二', 1))

    '''
    再准备建立一个新process, 这是基本格式 记住←
    必要参数 target : 指定process要执行的任务(这里是执行函数 action),
    必要参数args : 直译成中文就是'参数', 顾名思义就是前面target的参数,
    即action的参数, 注意args是个元组, 所以args后的参数写成tuple元组格式.
    直接写target('process一',0) 一定报错的
    '''

    jc1.start()  # 将蓄势待发的jc1process正式启动! !
    jc2.start()  # 同上...

    jc1.join()  # 等待processjc1将任务执行完...
    jc2.join()  # ...
    print('jc1,jc2任务都已执行完毕')

    jc1.close()  # 彻底关闭processjc1
    jc2.close()  # ...

     #输出结果是两个process同时且连续打印0, 1
```

##### Pool-2

```python
from multiprocessing import Pool
import time
import os

def action1(a, b=50):
    for i in range(b):
        print(a, os.getpid(), ' ', i)
        # os.getpid(): pid简单来说就是每个process的"身份证"
        time.sleep(0.1)

if __name__ == '__main__':  # 还要添加这行, 否则可能出现异常

    ci = Pool(3)  # 创建一个process池, 容量为3个process
    ci.apply_async(action1, args=('process一',))  # 启动第一个子process...
    ci.apply_async(action1, args=('process二', 50))
    # 和普通process的启动方式有很大不同仔细看
    ci.apply_async(action1, args=('process三', 60))
    # Pool的最基本格式记住←

    '''
    注意: 程序现在有4个process在运行:
    上面的三个子process 和一个最为核心的: 主process
    '''
    ci.close()  # 关闭process池(但池子内已启动的子process还会继续进行)
    ci.join()  # 等待process池内的所有子process完毕
    print('比如说这最后的一行输出就是主process执行任务打印出来的')

    '''
    主process(父process)全程干了什么?
    创建process池, 启动子process, 关闭process池, 等待子process完毕, 打印最后一行
    '''
```

##### process间的通信

```python
import multiprocessing,time

def foo(tx):
    time.sleep(0.5)
    ss = tx.get()  # 管子的另一端放在子process这里, 子process接收到了数据
    print('子process已收到数据...')
    print(ss)  # 子process打印出了数据内容

if __name__ == '__main__':  # 要加这行

    tx = multiprocessing.Queue()
    # 创建process通信的Queue, 你可以理解为我拿了个管子来...
    jc = multiprocessing.Process(target=foo, args=(tx,))
    # 创建子process
    jc.start()  # 启子子process

    print('主process准备发送数据...')
    tx.put('有内鬼, 终止交易! ')
    # 将管子的一端放在主process这里, 主process往管子里丢入数据
    jc.join()

    #这种方法可以实现任意process间的通信, 这里写的是主, 子process间的通信
```

### multithread

多任务可以由多process完成, 也可以由一个process内的多thread完成.

我们前面提到了process是由若干thread组成的, 一个process至少有一个thread.

由于thread是操作系统直接支持的执行单元, 因此, 高级语言通常都内置多thread的支持, Python也不例外, 并且, Python的thread是真正的`Posix Thread`, 而不是模拟出来的thread.

Python的标准库提供了两个模块: `_thread`和`threading`, `_thread`是低级模块, `threading`是高级模块, 对`_thread`进行了封装. 绝大多数情况下, 我们只需要使用`threading`这个高级模块.

启动一个thread就是把一个函数传入并创建Thread实例, 然后调用`start()`开始执行:

```python
import time, threading

# 新thread执行的代码:
def loop():
    print('thread %s is running...' % threading.current_thread().name)
    n = 0
    while n < 5:
        n = n + 1
        print('thread %s >>> %s' % (threading.current_thread().name, n))
        time.sleep(1)
    print('thread %s ended.' % threading.current_thread().name)

print('thread %s is running...' % threading.current_thread().name)
t = threading.Thread(target=loop, name='LoopThread')
t.start()
t.join()
print('thread %s ended.' % threading.current_thread().name)
```

执行结果如下:

```python
thread MainThread is running...
thread LoopThread is running...
thread LoopThread >>> 1
thread LoopThread >>> 2
thread LoopThread >>> 3
thread LoopThread >>> 4
thread LoopThread >>> 5
thread LoopThread ended.
thread MainThread ended.
```

由于任何**process**默认就会启动一个**thread**, 我们把该thread称为主thread, 主thread又可以启动新的thread, Python的`threading`模块有个`current_thread()`函数, 它永远返回当前thread的实例.
主thread实例的名字叫`MainThread`, 子thread的名字在创建时指定, 我们用`LoopThread`命名子thread. 名字仅仅在打印时用来显示, 完全没有其他意义, 如果不起名字Python就自动给thread命名为Thread-1, Thread-2... ...

#### Lock

多thread和多process最大的不同在于, 多process中, 同一个变量, 各自有一份拷贝存在于每个process中, 互不影响, 而多thread中, 所有变量都由所有thread共享, 所以, 任何一个变量都可以被任何一个thread修改, 因此, thread之间共享数据最大的危险在于多个thread同时改一个变量, 把内容给改乱了.

来看看多个thread同时操作一个变量怎么把内容给改乱了:

```python
import time, threading

# 假定这是你的银行存款:
balance = 0

def change_it(n):
    # 先存后取, 结果应该为0:
    global balance
    balance = balance + n
    balance = balance - n

def run_thread(n):
    for i in range(100000):
        change_it(n)

t1 = threading.Thread(target=run_thread, args=(5,))
t2 = threading.Thread(target=run_thread, args=(8,))
t1.start()
t2.start()
t1.join()
t2.join()
print(balance)
```

我们定义了一个共享变量`balance`, 初始值为`0`, 并且启动两个thread, 先存后取, 理论上结果应该为`0`, 但是, 由于thread的调度是由操作系统决定的, 当t1, t2交替执行时, 只要循环次数足够多, `balance`的结果就不一定是`0`了.

原因是因为高级语言的一条语句在CPU执行时是若干条语句, 即使一个简单的计算:

```python
balance = balance + n
```

也分两步:

+ 计算`balance + n`, 存入临时变量中;
+ 将临时变量的值赋给`balance`.

也就是可以看成:

```python
x = balance + n
balance = x
```

由于`x`是局部变量, 两个thread各自都有自己的`x`, 当代码正常执行时:

初始值 balance = 0

```python
t1: x1 = balance + 5 # x1 = 0 + 5 = 5
t1: balance = x1     # balance = 5
t1: x1 = balance - 5 # x1 = 5 - 5 = 0
t1: balance = x1     # balance = 0

t2: x2 = balance + 8 # x2 = 0 + 8 = 8
t2: balance = x2     # balance = 8
t2: x2 = balance - 8 # x2 = 8 - 8 = 0
t2: balance = x2     # balance = 0
```

结果 `balance = 0`

但是t1和t2是交替运行的, 如果操作系统以下面的顺序执行t1, t2:

初始值 `balance = 0`

```python
t1: x1 = balance + 5  # x1 = 0 + 5 = 5

t2: x2 = balance + 8  # x2 = 0 + 8 = 8
t2: balance = x2      # balance = 8

t1: balance = x1      # balance = 5
t1: x1 = balance - 5  # x1 = 5 - 5 = 0
t1: balance = x1      # balance = 0

t2: x2 = balance - 8  # x2 = 0 - 8 = -8
t2: balance = x2   # balance = -8
```

结果 `balance = -8`

究其原因, 是因为修改`balance`需要多条语句, 而执行这几条语句时, thread可能中断, 从而导致多个thread把同一个对象的内容改乱了.

两个thread同时一存一取, 就可能导致余额不对, 你肯定不希望你的银行存款莫名其妙地变成了负数, 所以, 我们必须确保一个thread在修改`balance`的时候, 别的thread一定不能改.

如果我们要确保`balance`计算正确, 就要给`change_it()`上一把锁, 当某个thread开始执行`change_it()`时, 我们说, 该thread因为获得了锁, 因此其他thread不能同时执行`change_it()`, 只能等待, 直到锁被释放后, 获得该锁以后才能改.
由于锁只有一个, 无论多少thread, 同一时刻最多只有一个thread持有该锁, 所以, 不会造成修改的冲突. 创建一个锁就是通过`threading.Lock()`来实现:

```python
balance = 0
lock = threading.Lock()

def run_thread(n):
    for i in range(100000):
        # 先要获取锁:
        lock.acquire()
        try:
            # 放心地改吧:
            change_it(n)
        finally:
            # 改完了一定要释放锁:
            lock.release()
```

当多个thread同时执行`lock.acquire()`时, 只有一个thread能成功地获取锁, 然后继续执行代码, 其他thread就继续等待直到获得锁为止.

获得锁的thread用完后一定要释放锁, 否则那些苦苦等待锁的thread将永远等待下去, 成为死thread. 所以我们用`try...finally`来确保锁一定会被释放.

锁的好处就是确保了某段关键代码只能由一个thread从头到尾完整地执行, 坏处当然也很多, 首先是阻止了多thread并发执行, 包含锁的某段代码实际上只能以单thread模式执行, 效率就大大地下降了. 其次, 由于可以存在多个锁, 不同的thread持有不同的锁, 并试图获取对方持有的锁时, 可能会造成死锁, 导致多个thread全部挂起, 既不能执行, 也无法结束, 只能靠操作系统强制终止.

#### 多核CPU

如果你不幸拥有一个多核CPU, 你肯定在想, 多核应该可以同时执行多个线程.

如果写一个死循环的话, 会出现什么情况呢?

打开Mac OS X的Activity Monitor, 或者Windows的Task Manager, 都可以监控某个process的CPU使用率.

我们可以监控到一个死循环线程会100%占用一个CPU.

如果有两个死循环线程, 在多核CPU中, 可以监控到会占用200%的CPU, 也就是占用两个CPU核心.

要想把N核CPU的核心全部跑满, 就必须启动N个死循环线程.

试试用Python写个死循环:

```python
import threading, multiprocessing

def loop():
    x = 0
    while True:
        x = x ^ 1

for i in range(multiprocessing.cpu_count()):
    t = threading.Thread(target=loop)
    t.start()
```

启动与CPU核心数量相同的N个线程, 在4核CPU上可以监控到CPU占用率仅有102%, 也就是仅使用了一核.

但是用C, C++或Java来改写相同的死循环, 直接可以把全部核心跑满, 4核就跑到400%, 8核就跑到800%, 为什么Python不行呢?

因为Python的线程虽然是真正的线程, 但解释器执行代码时, 有一个`GIL`锁: Global Interpreter Lock, 任何Python线程执行前, 必须先获得`GIL`锁, 然后, 每执行100条字节码, 解释器就自动释放`GIL`锁, 让别的线程有机会执行. 这个`GIL`全局锁实际上把所有线程的执行代码都给上了锁, 所以, 多线程在Python中只能交替执行, 即使100个线程跑在100核CPU上, 也只能用到1个核.

`GIL`是Python解释器设计的历史遗留问题, 通常我们用的解释器是官方实现的CPython, 要真正利用多核, 除非重写一个不带`GIL`的解释器.

所以, 在Python中, 可以使用多线程, 但不要指望能有效利用多核. 如果一定要通过多线程利用多核, 那只能通过C扩展来实现, 不过这样就失去了Python简单易用的特点.

不过, 也不用过于担心, Python虽然不能利用多线程实现多核任务, 但可以通过多process实现多核任务. 多个Pythonprocess有各自独立的`GIL`锁, 互不影响.

#### 小结-多线程

+ 多线程编程, 模型复杂, 容易发生冲突, 必须用锁加以隔离, 同时, 又要小心死锁的发生.
+ Python解释器由于设计时有GIL全局锁, 导致了多线程无法利用多核. 多线程的并发在Python中就是一个美丽的梦.

### ThreadLocal

在多线程环境下, 每个线程都有自己的数据. 一个线程使用自己的局部变量比使用全局变量好, 因为局部变量只有线程自己能看见, 不会影响其他线程, 而全局变量的修改必须加锁.

但是局部变量也有问题, 就是在函数调用的时候, 传递起来很麻烦:

```python
def process_student(name):
    std = Student(name)
    # std是局部变量, 但是每个函数都要用它, 因此必须传进去:
    do_task_1(std)
    do_task_2(std)

def do_task_1(std):
    do_subtask_1(std)
    do_subtask_2(std)

def do_task_2(std):
    do_subtask_2(std)
    do_subtask_2(std)
```

每个函数一层一层调用都这么传参数那还得了? 用全局变量? 也不行, 因为每个线程处理不同的`Student`对象, 不能共享.

如果用一个全局`dict`存放所有的`Student`对象, 然后以`thread`自身作为`key`获得线程对应的`Student`对象如何?

```python
global_dict = {}

def std_thread(name):
    std = Student(name)
    # 把std放到全局变量global_dict中:
    global_dict[threading.current_thread()] = std
    do_task_1()
    do_task_2()

def do_task_1():
    # 不传入std, 而是根据当前线程查找:
    std = global_dict[threading.current_thread()]
    ...

def do_task_2():
    # 任何函数都可以查找出当前线程的std变量:
    std = global_dict[threading.current_thread()]
    ...
```

这种方式理论上是可行的, 它最大的优点是消除了`std`对象在每层函数中的传递问题, 但是, 每个函数获取`std`的代码有点丑.

有没有更简单的方式?

`ThreadLocal`应运而生, 不用查找dict, `ThreadLocal`帮你自动做这件事:

```python
import threading

# 创建全局ThreadLocal对象:
local_school = threading.local()

def process_student():
    # 获取当前线程关联的student:
    std = local_school.student
    print('Hello, %s (in %s)' % (std, threading.current_thread().name))

def process_thread(name):
    # 绑定ThreadLocal的student:
    local_school.student = name
    process_student()

t1 = threading.Thread(target= process_thread, args=('Alice',), name='Thread-A')
t2 = threading.Thread(target= process_thread, args=('Bob',), name='Thread-B')
t1.start()
t2.start()
t1.join()
t2.join()
```

执行结果:

```python
Hello, Alice (in Thread-A)
Hello, Bob (in Thread-B)
```

全局变量`local_school`就是一个`ThreadLocal`对象, 每个Thread对它都可以读写student属性, 但互不影响. 你可以把`local_school`看成全局变量, 但每个属性如`local_school.student`都是线程的局部变量, 可以任意读写而互不干扰, 也不用管理锁的问题, `ThreadLocal`内部会处理.

可以理解为全局变量`local_school`是一个dict, 不但可以用`local_school.student`, 还可以绑定其他变量, 如`local_school.teacher`等等.

`ThreadLocal`最常用的地方就是为每个线程绑定一个数据库连接, HTTP请求, 用户身份信息等, 这样一个线程的所有调用到的处理函数都可以非常方便地访问这些资源.

#### 小结-ThreadLocal

一个`ThreadLocal`变量虽然是全局变量, 但每个线程都只能读写自己线程的独立副本, 互不干扰. `ThreadLocal`解决了参数在一个线程中各个函数之间互相传递的问题.

### process vs. 线程

我们介绍了多process和多线程, 这是实现多任务最常用的两种方式. 现在, 我们来讨论一下这两种方式的优缺点.

首先, 要实现多任务, 通常我们会设计Master-Worker模式, Master负责分配任务, Worker负责执行任务, 因此, 多任务环境下, 通常是一个Master, 多个Worker.

如果用多process实现Master-Worker, 主process就是Master, 其他process就是Worker.

如果用多线程实现Master-Worker, 主线程就是Master, 其他线程就是Worker.

多process模式最大的优点就是稳定性高, 因为一个子process崩溃了, 不会影响主process和其他子process. (当然主process挂了所有process就全挂了, 但是Masterprocess只负责分配任务, 挂掉的概率低)著名的Apache最早就是采用多process模式.

多process模式的缺点是创建process的代价大, 在Unix/Linux系统下, 用fork调用还行, 在Windows下创建process开销巨大. 另外, 操作系统能同时运行的process数也是有限的, 在内存和CPU的限制下, 如果有几千个process同时运行, 操作系统连调度都会成问题.

多线程模式通常比多process快一点, 但是也快不到哪去, 而且, 多线程模式致命的缺点就是任何一个线程挂掉都可能直接造成整个process崩溃, 因为所有线程共享process的内存. 在Windows上, 如果一个线程执行的代码出了问题, 你经常可以看到这样的提示: "该程序执行了非法操作, 即将关闭", 其实往往是某个线程出了问题, 但是操作系统会强制结束整个process.

在Windows下, 多线程的效率比多process要高, 所以微软的IIS服务器默认采用多线程模式. 由于多线程存在稳定性的问题, IIS的稳定性就不如Apache. 为了缓解这个问题, IIS和Apache现在又有多process+多线程的混合模式, 真是把问题越搞越复杂.

#### 线程切换

无论是多process还是多线程, 只要数量一多, 效率肯定上不去, 为什么呢?

我们打个比方, 假设你不幸正在准备中考, 每天晚上需要做语文, 数学, 英语, 物理, 化学这5科的作业, 每项作业耗时1小时.

如果你先花1小时做语文作业, 做完了, 再花1小时做数学作业, 这样, 依次全部做完, 一共花5小时, 这种方式称为单任务模型, 或者批处理任务模型.

假设你打算切换到多任务模型, 可以先做1分钟语文, 再切换到数学作业, 做1分钟, 再切换到英语, 以此类推, 只要切换速度足够快, 这种方式就和单核CPU执行多任务是一样的了, 以幼儿园小朋友的眼光来看, 你就正在同时写5科作业.

但是, 切换作业是有代价的, 比如从语文切到数学, 要先收拾桌子上的语文书本, 钢笔(这叫保存现场), 然后, 打开数学课本, 找出圆规直尺(这叫准备新环境), 才能开始做数学作业. 操作系统在切换process或者线程时也是一样的, 它需要先保存当前执行的现场环境(CPU寄存器状态, 内存页等), 然后, 把新任务的执行环境准备好(恢复上次的寄存器状态, 切换内存页等), 才能开始执行. 这个切换过程虽然很快, 但是也需要耗费时间. 如果有几千个任务同时进行, 操作系统可能就主要忙着切换任务, 根本没有多少时间去执行任务了, 这种情况最常见的就是硬盘狂响, 点窗口无反应, 系统处于假死状态.

所以, 多任务一旦多到一个限度, 就会消耗掉系统所有的资源, 结果效率急剧下降, 所有任务都做不好.

#### 计算密集型 vs. IO密集型

是否采用多任务的第二个考虑是任务的类型. 我们可以把任务分为计算密集型和IO密集型.

计算密集型任务的特点是要进行大量的计算, 消耗CPU资源, 比如计算圆周率, 对视频进行高清解码等等, 全靠CPU的运算能力. 这种计算密集型任务虽然也可以用多任务完成, 但是任务越多, 花在任务切换的时间就越多, CPU执行任务的效率就越低, 所以, 要最高效地利用CPU, 计算密集型任务同时进行的数量应当等于CPU的核心数.

计算密集型任务由于主要消耗CPU资源, 因此, 代码运行效率至关重要. Python这样的脚本语言运行效率很低, 完全不适合计算密集型任务. 对于计算密集型任务, 最好用C语言编写.

第二种任务的类型是IO密集型, 涉及到网络, 磁盘IO的任务都是IO密集型任务, 这类任务的特点是CPU消耗很少, 任务的大部分时间都在等待IO操作完成(因为IO的速度远远低于CPU和内存的速度). 对于IO密集型任务, 任务越多, CPU效率越高, 但也有一个限度. 常见的大部分任务都是IO密集型任务, 比如Web应用.

IO密集型任务执行期间, 99%的时间都花在IO上, 花在CPU上的时间很少, 因此, 用运行速度极快的C语言替换用Python这样运行速度极低的脚本语言, 完全无法提升运行效率. 对于IO密集型任务, 最合适的语言就是开发效率最高(代码量最少)的语言, 脚本语言是首选, C语言最差.

#### 异步IO

考虑到CPU和IO之间巨大的速度差异, 一个任务在执行的过程中大部分时间都在等待IO操作, 单process单线程模型会导致别的任务无法并行执行, 因此, 我们才需要多process模型或者多线程模型来支持多任务并发执行.

现代操作系统对IO操作已经做了巨大的改进, 最大的特点就是支持异步IO. 如果充分利用操作系统提供的异步IO支持, 就可以用单process单线程模型来执行多任务, 这种全新的模型称为事件驱动模型, Nginx就是支持异步IO的Web服务器, 它在单核CPU上采用单process模型就可以高效地支持多任务. 在多核CPU上, 可以运行多个process(数量与CPU核心数相同), 充分利用多核CPU. 由于系统总的process数量十分有限, 因此操作系统调度非常高效. 用异步IO编程模型来实现多任务是一个主要的趋势.

对应到Python语言, **单线程的异步编程模型称为协程**, 有了协程的支持, 就可以基于事件驱动编写高效的多任务程序. 我们会在后面讨论如何编写协程.

+ 同步IO:  顺序执行, 等结果, 不复杂
+ 异步IO:  异步执行, 不等结果, 复杂

### 分布式进程

在Thread和Process中, 应当优选Process, 因为Process更稳定, 而且, Process可以分布到多台机器上, 而Thread最多只能分布到同一台机器的多个CPU上.

Python的`multiprocessing`模块不但支持多进程, 其中`managers`子模块还支持把多进程分布到多台机器上. 一个服务进程可以作为调度者, 将任务分布到其他多个进程中, 依靠网络通信. 由于`managers`模块封装很好, 不必了解网络通信的细节, 就可以很容易地编写分布式多进程程序.

举个例子: 如果我们已经有一个通过`Queue`通信的多进程程序在同一台机器上运行, 现在, 由于处理任务的进程任务繁重, 希望把发送任务的进程和处理任务的进程分布到两台机器上. 怎么用分布式进程实现?

原有的`Queue`可以继续使用, 但是, 通过`managers`模块把`Queue`通过网络暴露出去, 就可以让其他机器的进程访问`Queue`了.

我们先看服务进程, 服务进程负责启动`Queue`, 把`Queue`注册到网络上, 然后往`Queue`里面写入任务:

```python
# task_master.py

import random, time, queue
from multiprocessing.managers import BaseManager

# 发送任务的队列:
task_queue = queue.Queue()
# 接收结果的队列:
result_queue = queue.Queue()

# 从BaseManager继承的QueueManager:
class QueueManager(BaseManager):
    pass

# 把两个Queue都注册到网络上, callable参数关联了Queue对象:
QueueManager.register('get_task_queue', callable=lambda: task_queue)
QueueManager.register('get_result_queue', callable=lambda: result_queue)

# 绑定端口5000, 设置验证码'abc':
manager = QueueManager(address=('', 5000), authkey=b'abc')

# 启动Queue:
manager.start()

# 获得通过网络访问的Queue对象:
task = manager.get_task_queue()
result = manager.get_result_queue()

# 放几个任务进去:
for i in range(10):
    n = random.randint(0, 10000)
    print('Put task %d...' % n)
    task.put(n)

# 从result队列读取结果:
print('Try get results...')
for i in range(10):
    r = result.get(timeout=10)
    print('Result: %s' % r)

# 关闭:
manager.shutdown()
print('master exit.')
```

请注意, 当我们在一台机器上写多进程程序时, 创建的`Queue`可以直接拿来用,
但是, 在分布式多进程环境下, 添加任务到`Queue`不可以直接对原始的`task_queue`进行操作, 那样就绕过了`QueueManager`的封装, 必须通过`manager.get_task_queue()`获得的`Queue`接口添加.

然后, 在另一台机器上启动任务进程(本机上启动也可以):

```python
# task_worker.py

import time, sys, queue
from multiprocessing.managers import BaseManager

# 创建类似的QueueManager:
class QueueManager(BaseManager):
    pass

# 由于这个QueueManager只从网络上获取Queue, 所以注册时只提供名字:
QueueManager.register('get_task_queue')
QueueManager.register('get_result_queue')

# 连接到服务器, 也就是运行task_master.py的机器:
server_addr = '127.0.0.1'
print('Connect to server %s...' % server_addr)
# 端口和验证码注意保持与task_master.py设置的完全一致:
m = QueueManager(address=(server_addr, 5000), authkey=b'abc')

# 从网络连接:
m.connect()
# 获取Queue的对象:
task = m.get_task_queue()
result = m.get_result_queue()

# 从task队列取任务,并把结果写入result队列:
for i in range(10):
    try:
        n = task.get(timeout=1)
        print('run task %d * %d...' % (n, n))
        r = '%d * %d = %d' % (n, n, n*n)
        time.sleep(1)
        result.put(r)
    except Queue.Empty:
        print('task queue is empty.')
# 处理结束:
print('worker exit.')
```

任务进程要通过网络连接到服务进程, 所以要指定服务进程的IP.

现在, 可以试试分布式进程的工作效果了. 先启动`task_master.py`服务进程:

```python
$ python3 task_master.py
Put task 3411...
Put task 1605...
Put task 1398...
Put task 4729...
Put task 5300...
Put task 7471...
Put task 68...
Put task 4219...
Put task 339...
Put task 7866...
Try get results...
```

task_master.py进程发送完任务后, 开始等待result队列的结果. 现在启动`task_worker.py`进程:

```python
$ python3 task_worker.py
Connect to server 127.0.0.1...
run task 3411 * 3411...
run task 1605 * 1605...
run task 1398 * 1398...
run task 4729 * 4729...
run task 5300 * 5300...
run task 7471 * 7471...
run task 68 * 68...
run task 4219 * 4219...
run task 339 * 339...
run task 7866 * 7866...
worker exit.
```

`task_worker.py`进程结束, 在`task_master.py`进程中会继续打印出结果:

```python
Result: 3411 * 3411 = 11634921
Result: 1605 * 1605 = 2576025
Result: 1398 * 1398 = 1954404
Result: 4729 * 4729 = 22363441
Result: 5300 * 5300 = 28090000
Result: 7471 * 7471 = 55815841
Result: 68 * 68 = 4624
Result: 4219 * 4219 = 17799961
Result: 339 * 339 = 114921
Result: 7866 * 7866 = 61873956
```

这个简单的Master/Worker模型有什么用? 其实这就是一个简单但真正的分布式计算, 把代码稍加改造, 启动多个worker, 就可以把任务分布到几台甚至几十台机器上, 比如把计算`n*n`的代码换成发送邮件, 就实现了邮件队列的异步发送.

`Queue`对象存储在哪? 注意到`task_worker.py`中根本没有创建`Queue`的代码, 所以, `Queue`对象存储在`task_master.py`进程中:

????????????????????????????????????????????????

而`Queue`之所以能通过网络访问, 就是通过`QueueManager`实现的. 由于`QueueManager`管理的不止一个`Queue`, 所以, 要给每个`Queue`的网络调用接口起个名字, 比如`get_task_queue`.

`authkey`有什么用? 这是为了保证两台机器正常通信, 不被其他机器恶意干扰. 如果`task_worker.py`的`authkey`和`task_master.py`的`authkey`不一致, 肯定连接不上.

注意两点:

1. Windows下在注册任务队列和结果队列时不支持lambda表达式, 需要使用函数代替
2. 在绑定网络地址时不能为空, 可以使用`'127.0.0.1'`代替空置

[Windows下运行分布式应用的代码][]

[Windows下运行分布式应用的代码]: https://www.liaoxuefeng.com/discuss/969955749132672/1331182822228002

#### 小结-分布式进程

Python的分布式进程接口简单, 封装良好, 适合需要把繁重任务分布到多台机器的环境下.

注意`Queue`的作用是用来传递任务和接收结果, 每个任务的描述数据量要尽量小.
比如发送一个处理日志文件的任务, 就不要发送几百兆的日志文件本身, 而是发送日志文件存放的完整路径, 由`Worke`r进程再去共享的磁盘上读取文件.
