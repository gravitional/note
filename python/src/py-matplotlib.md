# matplotlib

[绘图: matplotlib 核心剖析](https://www.cnblogs.com/vamei/archive/2013/01/30/2879700.html)

## pyplot.show

[matplotlib.pyplot.show](https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.show.html)

将数据保存到 `文件`, 并同时显示窗口

如果既想要图像文件又想要用户界面窗口, 请在 `pyplot.show` 之前使用 `pyplot.savefig`.
在(阻塞)`show()`结束时, 图形会关闭, 从而从 `pyplot` 反注册(unregistered).
之后再调用 `pyplot.savefig` 会保存一个新的空图形.
如果 `show` 是非阻塞的,
或者你保留了对图形的引用并使用 `Figure.savefig,` 命令顺序的限制就不适用了.

## matplotlib 介绍

[matplotlib](https://matplotlib.org/stable/api/matplotlib_configuration_api.html)

一个 object-oriented 的绘图库.
库中附带的 pyplot 模块, 提供了 `procedural interface`(过程式接口), 可以直接导入, 例如

```python
import matplotlib.pyplot as plt
```

或使用 `ipython`, 在终端输入

```bash
ipython
```

然后在 ipython shell 提示符后面输入

```bash
In [1]: %matplotlib
In [2]: import matplotlib.pyplot as plt
```

在大多数情况下, 我们鼓励在编程时直接使用**显式面向对象**库;
**隐式pyplot接口**主要用于交互式工作.
但 pyplot 函数
[pyplot.figure](https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.figure.html#matplotlib.pyplot.figure),
[pyplot.subplot](https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.subplot.html#matplotlib.pyplot.subplot),
[pyplot.subplots](https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.subplots.html#matplotlib.pyplot.subplots) 和
[pyplot.savefig](https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.savefig.html#matplotlib.pyplot.savefig)
除外, 它们可以大大简化脚本编写.
请参阅 [Matplotlib 应用程序接口 (API)](https://matplotlib.org/stable/users/explain/figure/api_interfaces.html#api-interfaces), 了解隐式接口和显式接口之间的权衡.

## matplotlib 库的结构

[matplotlib_configuration_api.html](https://matplotlib.org/stable/api/matplotlib_configuration_api.html)

matplotlib 采用面向对象的设计思想,
上面的链接给出了 matplotlib 的所有模块, 在侧边栏.
顶层模块即 [matplotlib](https://matplotlib.org/stable/api/matplotlib_configuration_api.html) 库,
往下是子模块, 例如 [matplotlib.axes](https://matplotlib.org/stable/api/axes_api.html), 即用小写字母 `axes` 表示.

`matplotlib.axes` 中有同名的 [`Axes` 类](https://matplotlib.org/stable/api/_as_gen/matplotlib.axes.Axes.html#matplotlib.axes.Axes),
再往下是 `Axes` 类的子类和成员函数 等等, 例如[matplotlib.axes.Axes.plot](https://matplotlib.org/stable/api/_as_gen/matplotlib.axes.Axes.plot.html#matplotlib.axes.Axes.plot)
总结如下,

```python
matplotlib # library
    matplotlib.axes # module
        matplotlib.axes.Axes # class
            matplotlib.axes.Axes.plot #function
```
