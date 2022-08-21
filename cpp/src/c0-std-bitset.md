# Std bitset

[C++ std::Bitset](https://blog.csdn.net/zy2317878/article/details/80082863)
[cpp reference std::bitset](https://en.cppreference.com/w/cpp/utility/bitset)

## 写在前面

这一篇文章系统的学习一下std标准库中的容器bitset, `bitset` 用来储存诸多bit,
这些元素可以用来表示两种状态: 0或1, true或false….
所以有一些时候可以很方便的用该容器快速实现状态储存.

该容器通过对空间的特殊优化, 使得该容器对状态的储存空间非常小, 相当于将若干状态储存在一个个bit上.
该容器的元素访问也可以通过[n]访问容器中第n个元素,
但是由于一般语言没有一个bit大小的数据类型,
所以这里使用了一种特殊的引用类型访问, 如: bitset::reference.

## bitset类与头文件包含

`bitset` 头文件引用如下:

```cpp
#include <bitset>
```
