# 迭代器库

[迭代器库](https://zh.cppreference.com/w/cpp/iterator)

`迭代器库` 提供了五种迭代器的定义,
同时还提供了 `迭代器特征`, `适配器` 及相关的工具函数.

### 迭代器分类

迭代器共有五 (C++17 前), 六 (C++17 起)种:

+ 老式输入迭代器 (LegacyInputIterator) ,
+ 老式输出迭代器 (LegacyOutputIterator) ,
+ 老式向前迭代器 (LegacyForwardIterator) ,
+ 老式双向迭代器 (LegacyBidirectionalIterator) ,
+ 老式随机访问迭代器 (LegacyRandomAccessIterator) ,
+ 及老式连续迭代器 (LegacyContiguousIterator) (C++17 起).

迭代器的分类的依据并不是迭代器的类型, 而是迭代器所支持的操作.
换句话说, 某个类型只要支持相应的操作, 就可以作为迭代器使用.
例如, `完整对象类型指针` 支持所有 `老式随机访问迭代器` (LegacyRandomAccessIterator) 要求的操作,
于是任何需要 `老式随机访问迭代器` (LegacyRandomAccessIterator) 的地方都可以使用指针.

### C++20 迭代器概念

C++20 引入基于概念的新迭代器系统, 它异于 C++17 迭代器.
虽然基础分配法保持类似, 但单独的迭代器类别的要求有些区别.
