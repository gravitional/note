# python None, 判断变量是否存在

[深入理解Python中的None](https://zhuanlan.zhihu.com/p/65193194)
[代码中经常会有变量是否为None的判断, 有三种主要的写法](https://zhuanlan.zhihu.com/p/105177340)
[内置常量](https://docs.python.org/zh-cn/3/library/constants.html?highlight=none)
[在 Python 中检查一个变量是否存在](https://www.delftstack.com/zh/howto/python/python-check-if-variable-exists/)

本方法将使用 `locals()` 函数检查本地变量的存在.
locals() 返回一个字典, 它的键是存在于本地命名空间的变量名称的字符串.

```python
def local_func():
    var = "Test"
    if 'var' in locals():
        print ('var variable exists')
    else:
        print ('var variable does not exist in the local namespace')

local_func()
```

该函数将使用 `globals()` 方法检查全局命名空间中是否存在一个变量.
globals() 返回一个字典, 它的键是存在于全局命名空间的变量名称的字符串.

```python
var2 = "Python"

if 'var2' in globals():
    print ("var2: variable exist")
else:
    print ("var2: variable does not exist")
```
