
# Python接口: PyROOT

通过 `PyROOT`, 即ROOT的Python-C++绑定, 你可以从Python中使用ROOT.
`PyROOT` 是HEP从 `Python` 进入所有 `C++` 功能的入口,例如, 用于`框架`和他们的引导代码(steering code).
PyROOT的绑定是自动和动态的: 不需要预先生成 `Python 包装器`(wrappers).

通过 `PyROOT`, 你可以从 `Python` 中访问完整的ROOT功能, 同时受益于 `ROOT C++` 库的性能.

PyROOT与Python2(>=2.7)和Python3都兼容.

>使用PyROOT需要有Python的工作知识.
>关于Python的详细信息, 请参考Python语言参考.

与 `ROOT 6.22` 一起, PyROOT的一个主要修订版已经发布.
新的PyROOT对现代C++有广泛的支持(它在cppyy的基础上运行), 更加pythonic,
能够与广泛使用的Python数据科学库(例如, NumPy, pandas, Numba)进行互操作.

因此, 我们强烈建议使用新的PyROOT.

[PyRoot 教程](https://root.cern/doc/master/group__tutorial__pyroot.html)

