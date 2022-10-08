# Python super() 函数

[Python super() 函数](https://zhuanlan.zhihu.com/p/28340924)

## super函数简介

通常情况下, 我们在 `子类` 中定义了和 `父类` 同名的方法, 那么 子类 的方法就会覆盖 `父类` 的方法.
而 `super` 关键字实现了对父类方法的 `改写`(增加了功能, 增加的功能写在子类中, 父类方法中原来的功能得以保留).
也可以说, `super` 关键字帮助我们实现了在子类中调用父类的方法

## python3和python2中super的用法稍有差别

```python
# /usr/bin/env python3
# -*-coding:utf-8 -*-
class Animals(object):
    def __int__(self, name):
        self.name = name

    def greet(self):
        print(f'Hello, I am {self.name}')

class Dog(Animals):
    def greet(self):
        super(Dog, self).greet()  # Python3中可以用 super().greet()
        print('wang wang ...')

dog = Dog('Mike')
dog.greet()
```

输出如下:

```bash
python3 use_super.py
Hello, I am Mike
wang wang ...
```

## super函数深入

`MRO`(Method Resolution Order): python 对于每一个类都有一个 `MRO` 列表.
此表的生成有以下原则:
子类永远在父类之前, 如果有多个父类, 那么按照它们在列表中的顺序被检查,
如果下一个类有两个合法的选择, 那么就只选择第一个

```python
super(cls, inst):
    mro = inst.__class__mro()
    return mro(mro.index(cls) + 1)
```

`super` 做了两件事情, 获取 `mro` 列表, 然后返回列表中下一个类.

实例如下:

```python
# /usr/bin/env python3
# -*-coding:utf-8 -*-
class BaseClass(object):
    def __init__(self):
        print('enter BaseClass')
        print('leave BaseClass')

class A(BaseClass):
    def __init__(self):
        print('enter A')
        super(A, self).__init__() # 调用 BaseClass 的构造函数
        print('leave A')

class B(BaseClass):
    def __init__(self):
        print('enter B')
        super(B, self).__init__() # 调用 A 的构造函数, 此时的 self 实际上是 C 的实例.
        print('leave B')

class C(B, A):
    def __init__(self):
        print('enter C')
        super(C, self).__init__() # 调用 B 的构造函数
        print('leave C')

c = C()
```

以其中的 `super(C, self).__init__()` 为例,
`self` 是 `C` 的实例, 这个就返回了 `C` 的 `MRO` 列表

```python
__main__.C,
__main__.B, __main__.A,
__main__.BaseClass,
object
```

`self` 都是 `C` 的实例, 所以才会在 `C` 的 `MRO` 列表中顺序搜索. 输出如下:

```bash
python3 use_superV2.py
enter C
enter B
enter A
enter BaseClass
leave BaseClass
leave A
leave B
leave C
```
