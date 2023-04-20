# 使用list和tuple

## list 列表

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

## tuple 元组

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

## 条件判断,if

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

## 再议-input

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

## 循环

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

## break

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

## continue

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

## 小结

循环是让计算机做重复任务的有效的方法.

`break`语句可以在循环过程中直接退出循环, 而`continue`语句可以提前结束本轮循环, 并直接开始下一轮循环.
这两个语句通常都必须配合`if`语句使用.

要特别注意, 不要滥用`break`和`continue`语句. `break`和`continue`会造成代码执行逻辑分叉过多, 容易出错.
大多数循环并不需要用到`break`和`continue`语句, 上面的两个例子, 都可以通过改写循环条件或者修改循环逻辑, 去掉`break`和`continue`语句.

有些时候, 如果代码写得有问题, 会让程序陷入"死循环", 也就是永远循环下去.
这时可以用`Ctrl+C`退出程序, 或者强制结束 `Python` 进程.

## 切片

[Python 切片完全指南(语法篇)](https://zhuanlan.zhihu.com/p/79541418)

## set 集合类型

[使用in判断元素是否在列表(list)中, 如何提升搜索效率](https://www.moxiaoye.fun/archives/python%E6%8F%90%E5%8D%87%E5%88%97%E8%A1%A8%E5%85%83%E7%B4%A0%E6%90%9C%E7%B4%A2%E9%80%9F%E7%8E%87)

经常会做的一个操作是使用 `in` 来判断元素是否在列表中,
这种操作非常便捷, 省去了自行遍历的工作,
而且因为大多数时候列表数据量比较小, 搜索的速度也能满足需求.

```python
key_list = [1, 2, 3, 4, 5, 6, 7, 8]
key = 10

if key in key_list:
    print("Hello!")
```

但是, 凡是就怕个但是, 当列表数据量非常大的时候,
比如你要在一个长度为一百万(1000000)的列表中搜索某个元素是否存在,
这种搜索变得非常低效, 很可能需要花费几个小时甚至几天时间来处理你的数据, 这种效率是无法接受的.

如何能够快速判断某个元素在一个数据集中? 答案是使用 `集合`(set).
相比于使用列表来存储我们的数据集, 我们使用集合来存储数据集只需要多加一个步骤,
即将列表集合化(以 `列表` 为参数, 使用 `set` 函数进行初始化), 而判断是否包含仍用 `in`:

```python
key_list = [1, 2, 3, 4, 5, 6, 7, 8]
key = 10

key_set = set(key_list)

if key in key_list:
    print("Hello!")
```

使用集合来存储数据集并做包含关系的判断, 可以大大提升搜索速度!
