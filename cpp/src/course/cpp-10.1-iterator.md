# 泛型程序设计与C++语言标准库

> `指针` 就是一种随机访问 `迭代器`.

+ STL 中各容器头文件和所属概念

|容器名 | 中文名 | 头文件 | 所属概念|
|---|---|---|---|
|vector| 向量|    `<vector>`    |随机访问容器, 顺序容器|
|deque| 双端队列 |   `<deque>`   |随机访问容器, 顺序容器|
|list|  列表|    `<list>`    |可逆容器, 顺序容器|
|forward_list|  单向链表 |   `<forward_list>`    |单向访问容器, 顺序容器|
|array| 数组|    `<array>`   |随机访问容器, 顺序容器|
|||||
|set|   集合 |   `<set>` |可逆容器, 关联容器|
|multiset|  多重集合|    `<set>` |可逆容器, 关联容器|
|map|   映射 |   `<map>` |可逆容器, 关联容器|
|multimap|  多重映射 |   `<map>` |可逆容器, 关联容器|
|||||
|unordered_set| 无序集合 |   `<unordered_set>`   |可逆容器, 关联容器|
|unordered_multiset|    无序多重集合 |  `<unordered_set>`  |可逆容器, 关联容器|
|unordered_map| 无序映射 |  `<unordered_map>`   |可逆容器, 关联容器|
|unordered_multimap|    无序多重映射|    `<unordered_map>`   |可逆容器, 关联容器|

概念: 泛型程序设计中, 作为参数的数据类型 所需具备的功能,
他的内涵是这些功能, 他的外延是具备这些功能的所有数据类型.

具备一个概念所需功能的数据类型称为这一概念的一个模型(model)
例如:

`Sortable`: 可以比大小, 具有公有的复制构造函数, 并可以用 `=` 赋值的所有数据类型
`Comparable`: 可以比大小的所有数据类型.
`Assignable`: 具有公有的复制构造函数, 并可以用 `=` 赋值的所有数据类型.

为了区分不同的具体模型, 模板参数中可以用 `Sortable1`, `Sortable` 表示.

+ `容器` 是容纳, 包含一组元素的对象.

## 迭代器的分类

+ `输入迭代器`,`输出迭代器` (IO Iterator)
+ `前向迭代器`(Forward Iterator)
+ `双向迭代器` (Bidirectional Iterator)
+ `随机访问迭代器` (Random Access Iterator)

+ `迭代器` 都是 `Assignable` 的子概念, 即具有 `公有的复制构造函数`, 可以用 `=` 赋值.

`P` 表示某种迭代器数据类型, `p1,p2` 表示 `P` 类型迭代器的对象
`T` 表示 `P` 类型迭代器元素指向的数据类型, `t` 表示T类型的对象
`m` 表示 `T` 类型中的成员, `n` 表示整数.

### 所有迭代器都具备的功能

+ `++p1`
+ `p1++`

`返回类型不确定` 是指对于迭代器这一概念而言,
`p1++` 的返回类型没有一个一致的定义,
而不是说, 对于每个迭代器实例 `p1++` 的返回类型都是不确定的.

### 输入迭代器

对序列进行 `不可重复` 的单向遍历.

+ `p1==p2`
+ `p1!=p2`
+ `*p1`; 使用 `*` 获取输入迭代器所指向元素的值,
该表达式返回值可以转换到 `T` 类型(可以是 `T`, `T&`, `const T&` 等类型. )

+ `p1->m`;
+ `*p1++`; `p1++` 的返回类型不确定(可以来自不同的输入设备?),
但 `*(p1++)` 的类型是确定的.

    ```cpp
    {T t=*p1; ++p1; return t;}
    ```

### 输出迭代器

也支持对序列进行 `单向遍历`.

+ `*p1=t`
+ `*p1++=t`; 等价于 `{*p1=t;++p1;}`, 返回类型不确定(多种输出设备?).

`写入` 元素的操作和使用 `++自增` 的操作必须交替进行.

### 前向迭代器

`前向迭代器` 支持对序列进行 `可重复的` 单向遍历.

`*p1`; 对前向迭代器使用 `*` 运算符的结果保证具有 `T&` 类型.
`p1++`; 对迭代器实例可以使用 `后置++` 使迭代器指向下一个元素

### 双向迭代器

增加支持迭代器的 `反向移动`.

`--p1`; 可以使用前置 `--` 使迭代器指向上一个元素, 返回值为 `p1` 自身的引用.
`p1--`; 可以使用后置 `--` 使迭代器指向上一个元素.

### 随机访问迭代器

新增向前或向后移动 `n` 个元素, 随机访问迭代器的能力几乎和 指针 相同.

+ `p1+=n`; 将迭代器 p1 向前移动 n 个元素
+ `p1-=n`; 将迭代器 p1 向后移动 n 个元素
+ `p1+n`; 或 n+p1;获得指向迭代器 p1 前第 n 个元素的迭代器
+ `p1-n`;获得指向迭代器 p1 后第 n 个元素的迭代器
+ `p1-p2`; 返回一个满足 `p1==p2+n` 的整数 n
+ `p1 op p2`; 这里 op 可以是 `<`,`<=`,`>`,`>=`,
用于比较 p1 和 p2 所指向位置的前后关系, 等价于 `p1-p2 op 0`.
+ `p1[n]`; 等价于 `*(p1+n)`

+ 通过向量容器 `vector` 的 `begin` 和 `end` 函数得到的 `迭代器` 就是随机访问迭代器,
+ `指针` 也是 `随机访问迭代器`
