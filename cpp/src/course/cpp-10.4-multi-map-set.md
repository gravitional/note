# 多重集合和多重映射,multiset,multimap

`多重集合`(multiset) 和 `多重映射` (multimap) 去掉了键必须唯一的限制.

对于多重容器, 一般较少使用 `find` 成员函数, 而较多使用 `equal_range` 和 `count` 成员函数.
如果需要得到键对应的元素数目, 可以使用 `count` 成员函数.

映射支持的 `[]` 运算符不被多重映射支持, 原因是键不唯一.

## 无序容器

C++11 标准定义了 `4` 个无序容器, 对应之前的 `4` 种关联容器, 分别为

+ `unordered_set`
+ `unordered_map`
+ `unordered_mulitset`
+ `unordered_mulitmap`

这些容器并不使用 `比较运算符` 来组织元素, 而是通过 `哈希函数` 和 键类型的 `==` 运算符.

`哈希函数` 是一类将给定类型的值映射到整数值的一类函数.
相等的值映射到相同的整数, 不相等的值尽量映射到不同的整数.

若键类型没有明确的序关系, 无序函数是非常有用的.
另外理论上哈希函数能获得更好的平均性能, 因此无序容器通常会有更好的性能.

无序容器提供了与有序容器相同的操作, 如 find, insert 等.
由于元素未按照顺序排列, 无序容器的输出通常和对应的有序容器不同.

默认情况下, 无序容器使用键类型的 `==` 运算符来比较元素,
它们还是用一个 `hash<key_type>` 类型的对象来生成每个元素的 哈希值.

C++ 标准库为内置类型提供了 `hash` 模板,
还为一些标准库类型, 如 `string` 等定义了 `hash`.
因此我们可以直接定义关键字是内置类型的无序容器,
对于自定义类型, 必须提供自己的 `hash` 模板版本.
