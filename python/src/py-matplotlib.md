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
