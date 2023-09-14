# python 函数, unbound 方法和 bound 方法

[What is the difference between a function, an unbound method and a bound method?](https://stackoverflow.com/questions/11949808/what-is-the-difference-between-a-function-an-unbound-method-and-a-bound-method)

函数由 `def` 语句或 `lambda` 创建.
在 Python 2 中, 当一个函数出现在 `class statement` 的主体中时(或传递给 type class construction 调用时),
它会被转换成 unbound 方法(Python 3 没有 unbound方法, 见下文).
当在 class instance 上访问一个函数时,
它会被转换成 bound 方法, 自动将 instance 作为 self参数(首参数)提供给方法.

```python
def f1(self):
    pass
```

Here f1 is a function.

```python
class C(object):
    f1 = f1
```

Now `C.f1` is an unbound method.

```python
>>> C.f1
<unbound method C.f1>
>>> C.f1.im_func is f1
True
```

We can also use the `type` class constructor:

```python
>>> C2 = type('C2', (object,), {'f1': f1})
>>> C2.f1
<unbound method C2.f1>
```

We can convert `f1` to an unbound method manually:

```python
>>> import types
>>> types.MethodType(f1, None, C)
<unbound method C.f1>
```

Unbound methods are bound by access on a class instance:

```python
>>> C().f1
<bound method C.f1 of <__main__.C object at 0x2abeecf87250>>
```

Access is translated into calling through the descriptor protocol:

```python
>>> C.f1.__get__(C(), C)
<bound method C.f1 of <__main__.C object at 0x2abeecf871d0>>
```

合并起来就是:

```python
>>> types.MethodType(f1, None, C).__get__(C(), C)
<bound method C.f1 of <__main__.C object at 0x2abeecf87310>>
```

或者直接使用:

```python
>>> types.MethodType(f1, C(), C)
<bound method C.f1 of <__main__.C object at 0x2abeecf871d0>>
```

The main difference between a `function` and an `unbound method` is that
the latter knows which class it is bound to;
calling or binding an unbound method requires an instance of its class type:

```python
>>> f1(None)
>>> C.f1(None)
TypeError: unbound method f1() must be called with C instance as first argument (got NoneType instance instead)
>>> class D(object): pass
>>> f1.__get__(D(), D)
<bound method D.f1 of <__main__.D object at 0x7f6c98cfe290>>
>>> C.f1.__get__(D(), D)
<unbound method C.f1>
```

Since the difference between a `function` and an `unbound method` is pretty minimal,
Python 3 gets rid of the distinction;
under Python 3 accessing a function on a class instance just gives you the function itself:

```python
>>> C.f1
<function f1 at 0x7fdd06c4cd40>
>>> C.f1 is f1
True
```

In both Python 2 and Python 3, then, these three are equivalent:

```python
f1(C())
C.f1(C())
C().f1()
```

将函数绑定到 `instance` 的效果是将函数的第一个参数(通常称为 `self`)固定到 `instance` 上.
因此, bound method `C().f1` 等同于以下两种方法中的一种:

```python
(lamdba *args, **kwargs: f1(C(), *args, **kwargs))
functools.partial(f1, C())
```
