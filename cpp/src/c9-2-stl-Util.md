# C++ STL utility和iterator

[C++ STL utility和iterator](https://blog.csdn.net/lao_tan/article/details/81155807)

## utility

`utility` 是一个很小的头文件, 它包括了贯穿使用在STL中的几个模版的声明.
现在 `utility` 中只有模板类 `pair`,
一些与之相关的模版函数和操作符, 以及其他四个模版操作符了.
`pair` 模板类用于将2个对象表示成一个对象.
四个模版操作符赋予了"=="和"<"新的内涵.

+ `pair<T,U>`; 构造模板类 `pair`, `x` 为 `pair` 的一个对象,
x的第一个成员对象可以写为 `x.first`, 其类型是 `T`.

+ `make_pair`; 使用该工具可以实时产生一个pair对象.
但不能依赖make_pair产生一个含有一个或多个常量成员的pair对象,
因为模版函数在检测模版时将忽略掉所有的const属性.
+ `operator==`; 比较2个对象是否相等,
若x和y对应的成员都相等的话, 则可以说x和y是相等的.

+ `operator<`;
假设在pair的2个成员对象中, 第一个成员对象的权重总是大于第二个的,
若在此情况下可以确定 `x.first<y.first` 的话, 那 `x` 就小于 `y`.

```cpp
template<class T,class U> inline
    bool operator<(const pair<T,U>& x, const pair<T,U>& y)
    {return (x.first<y.first||!(y.first<x.first)&&x.second<y.second);}
```

+ operator!=
+ operator>
+ operator<=
+ operator>=
