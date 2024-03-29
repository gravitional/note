# 泛型与程序设计--关联容器

## 关联容器

`顺序容器` 的元素顺序由程序员决定, 可以随意制定新元素插入的位置.
而 `关联容器` 的每个元素都有个 `键`(key), 容器中的元素的顺序不能由程序员随意决定,
而是按照 `键` 的取值升序排列的.

对于 `关联容器`, 使用迭代器在 `[s.begin(), s.end())` 区间内遍历,
访问到的序列总是升序的, s.begin() 最小,  s.end()-1 所指向的元素总是最大的,
位于中间的迭代器, 总满足 `*iter <= *(iter+1)`.

`关联容器` 的最大优势在于, 可以 `高效` 地根据 `键` 来查找容器中的一个元素.

>最坏情况下, 只需要大概 log_2^n 次比较就可根据键来查找一个元素.

按照 容器中是否允许出现重复键,
关联容器又可以分为 单重关联容器 和 多重关联容器,
多重关联容器允许 键 重合.

+ 按照键和元素的关系可以分为 `简单关联容器` 和 `二元关联容器`.
简单关联容器以 `元素本身` 作为键,
二元关联容器的元素是由 `键` 和某种类型的 `附加数据` 共同构成的.

简单关联容器例如 `set<int>`, `multiset<string>`.
二元关联容器例如 `map<int,double>`, `multimap<string, int>`.

+ 二元关联容器 的元素类型可以用 二元组(pair)来表示, pair 是 `<utility>` 头文件中定义的结构体模板:

```cpp
template<class T1, class T2>
struct pair{
// pair 类型用 struct 定义, 以下成员均为 public, 可直接通过对象访问.
T1 first; //二元组的第一元
T2 second; //二元组的第二元
pair(); //默认构造函数
pair(const T1&x, const T2 &y); //构造 first=x, second=y 的二元组
template<class U, class V>pair(const pair<U,V> &p); //赋值构造函数.
}
```

+ 例如 `map<int, double>` 的元素类型是 `pair<int, double>`,
`multimap<string, int>` 的元素类型是 `pair<string ,int>`.

+ `pair` 类型的对象构造支持 列表初始化 方式:

```cpp
pair<string, int>string_int_pair={"first", 2};
```

关联容器的键之间必须能够使用 `<` 比大小.
如果是基本数据类型, 它们具有内置的比较运算符.
如果键的类型是 class type, 则需要重载 `<` 运算符.
且 `<` 需要满足 `严格弱序关系`,

非自反性; `<` 传递性;`==` 传递性.

## 容器的操作

用 S 表示容器类型名, 用 s 表示 S 类型的实例,
用 T 表示 S 容器的元素类型,  用 t 表示 T 类型的实例,
用 K 表示 S 容器 的键类型, 用 k 表示 K 的实例,
用 n 表示整型数据, p1 和 p2 表示指向 s 元素的迭代器,
用 q1 和 q2 表示不指向 s 的元素的输入迭代器.

下面给出关联容器的基本功能,
`单重` 和 `多重` 关联容器类型的操作大体相同, 差异将处予以说明

### 构造函数

+ `S s()`; 默认构造函数.
+ `S s(q1, q2)`; 将 `[q1, q2)` 区间内的数据作为 `s` 的元素, 构造 `s`.
    + 单重关联容器;当 `[q1, q2)` 区间 出现相同键的元素时, 只有第一个元素会被加入 `s`
    + 多重关联容器;`[q1, q2)` 区间内的元素均被无条件加入 `s` 中.

### 元素的插入

可以通过 `insert` 成员函数插入一个或多个元素,
相比于顺序容器, 无需通过迭代器指定插入位置:

+ `s.insert(t)`; 将元素 `t` 插入 `s` 容器中.
    + 单重; 只有不存在相同键的元素时才能成功插入, 返回类型为 `pair<S::iterator,bool>`.
    成功时返回被插元素的迭代器和 true;否则返回与 t 相同元素的迭代器  和  false.
    + 多重; 插入总会成功, 返回被插元素的迭代器.

+ `s.insert(p1,t)`; 将元素 t 插入 s 容器中,  p1 是提示的插入位置.
如果准确, 可以提高效率. 如果不准确也可以正确插入, 该函数总是返回迭代器.
单重和多重的区别同上.

+ `s.insert(q1, q2)`; 相对于按顺序对 `[q1, q2)` 区间内的每个元素 `x ` 分别执行 `s.insert(x)`.

### 元素的删除

+ `s.erase(p1)`; 删除 p1 所指向的元素.
+ `s.erase(p1,p2)`; 删除 `[p1, p2)` 区间内的元素.
+ `s.erase(k)`; 删除所有键为 k 的元素, 返回被删除元素的个数, k 是键类型的实例

### 基于键的查找和计数

+ `s.find(k)`; 找到任意一个键为 `k` 的元素, 返回该元素的迭代器, 如果没有, 则返回 `s.end()`;
+ `s.lower_bound(k)`; 得到 `s` 中的第一个 `键值 >= k` 的元素的迭代器.
+ `s.upper_bound(k)`; 得到 `s` 中的第一个 `键值 >k` 的元素的迭代器.
+ `s.equal_range(k)`; 得到一个用 `pair<S::iterator, S::iterator>` 表示的区间, 记为 `[p1, p2)`;
该区间刚好包含所有键为 `k` 的元素, `p1==s.lower_bound(k)`, `p2==s.upper_bound(k)`.
+ `s.count(k)`; 得到 `s` 容器中 键为 `k` 的元素个数.

### 关联容器的列表初始化

一元容器直接提供元素列表,
二元容器提供 `{key, value}` 键值对:

```cpp
set<int>integer_set={3,5,7} //以列表中 int 类型的元素为键创建 一元集合对象
map<string, int>id_map={{"小明",1}, {"李华",2}} //创建姓名到 id 的二元映射对象.
```

## 集合set

`STL` 中的 `set` 由不重复的元素组成, 元素是有限多的,
且集合的元素本身是有序的, 可以高效地查找指定元素,
也可以方便地得到指定大小范围内元素在容器中所处的区间.

## 映射map

`映射` 很像一个 `字典`;

```cpp
map<string, int> courses;
courses.insert(make_pair("CSAPP",3));
courses.insert(make_pair("C++",2));
courses.insert(make_pair("CSARCH",4));
courses.insert(make_pair("COMPILER",4));
courses.insert(make_pair("OS",5));
```

### 插入元素

```cpp
s.insert(pair<string, int>("CSAPP",3));
```

但为了简便, 一般使用 `make_pair` 来构造二元组,
它是 `<utility>` 中定义的专用于辅助构造二元组的函数模板:

```cpp
template<class T1, class T2>
pair<T1, T2> make_pair(T1 v1, T2 v2){return pair<T1, T2>(v1, v2);}
```

使用起来像这样:

```cpp
map<string,string> courses;
courses.insert(make_pair("CSAPP","3"));
courses.insert(make_pair("C++","2"));
...
```

### 获取元素

`iter->first` 和 `iter->second` 可以分别获取 `pair` 的元素.

### 删除元素

```cpp
map<string,string> courses;
courses.insert(make_pair("CSAPP","3"));
courses.insert(make_pair("C++","2"));
name="C++";
auto iter=courses.find(name); //查找课程
courses.erase(iter);
//courses.erase(name);
```

通过迭代器删除元素的效率更高, (不必先根据键找到迭代器):

### `[]`运算符

除了 `insert` 和 `find`, 映射还提供了 `[]` 运算符用于插入和查找元素.

+ `s[k]` 获取键为 `k` 的元素, 如果存在, 返回它的附加数据的引用;
如果不存在, 插入 `新元素` 并返回引用, 初值为 `V()`, `V` 是附加数据的类型
+ `s[k]=v`; 查找键为 k 的元素, 并赋值为 v.

>`V()` 的值与执行 `new V()` 时为堆对象产生的初值是一样的.
>如果 V 是 `数字` 或 `指针` 类型(内置类型), `V()` 的值就是 `0`, 参考 Sec 6.3, p221.

+ `[]` 不能用来检测 容器中是否具有指定键的元素.
