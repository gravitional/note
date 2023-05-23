# python 注释

[Python代码注释规范代码实例解析](https://zhuanlan.zhihu.com/p/344543685)

## python代码注释基础

Python中使用 `#` 表示单行注释.
单行注释可以作为单独的一行放在被注释代码行之上, 也可以放在语句或表达式之后.
如下例子:

```python
name = 'xiaohong' # 单行注释
# 单行注释
name = 'xiaohong'
```

Python中使用三个单引号或三个双引号表示多行注释.
用在注释多写不下的情况, 如下例子:

```python
'''
这是使用三个单引号的多行注释
'''
"""
这是使用三个双引号的多行注释
"""
```

## DocStrings常用编写风格

### reST风格

这是现在流行的一种风格, reST风格, Sphinx的御用格式, 比较紧凑.

```python
"""
This is a reST style.

:param param1: this is a first param
:param param2: this is a second param
:returns: this is a description of what is returned
:raises keyError: raises an exception
"""
```

### Google风格

```python
"""
This is a groups style docs.

Parameters:
 param1 - this is the first param
 param2 - this is a second param

Returns:
 This is a description of what is returned

Raises:
 KeyError - raises an exception
"""
```

### Numpydoc (Numpy风格)

```python
"""
My numpydoc description of a kind
of very exhautive numpydoc format docstring.

Parameters
----------
first : array_like
 the 1st param name `first`
second :
 the 2nd param
third : {'value', 'other'}, optional
 the 3rd param, by default 'value'

Returns
-------
string
 a value in a string

Raises
------
KeyError
 when a key error
OtherError
 when an other error
"""
```
