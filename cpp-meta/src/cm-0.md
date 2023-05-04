# c++ 模板, 元编程

[https://www.zhihu.com/column/c_1306966457508118528](https://www.zhihu.com/column/c_1306966457508118528)
[Simple C++11 metaprogramming](https://www.boost.org/doc/libs/master/libs/mp11/doc/html/simple_cxx11_metaprogramming.html)
[把玩C++模板:TypeList和Map](https://zhuanlan.zhihu.com/p/52539483)
[C++11 TypeList 妙用](https://www.cnblogs.com/tangzhenqiang/p/4209100.html)

## 引入

在写模板的时候, 经常会处理可变参的情况,
为了对这些参数进行操作,我们需要把它用容器包装起来,并在容器上定义我们的操作.

## 古典的TypeList

在C++98/11时代,还不存在可变参模板,
在那时要实现可变参运用的是类似于 `Lisp` 的嵌套结构:

```cpp
typelist<A,typelist<B,empty>>
```

其中 `typelist` 和 `empty` 定义如下:

```cpp
template<class H, class T> struct typelist
{
    typedef H Head;
    typedef T Tail;
};
struct empty {};
```

## 现代的TypeList

从C++14开始,引入了可变参模板和更好的偏特化,这使得我们可以以更直观的方式来定义 `TypeList`:

注意这里用到了模板偏特化引入的一个有趣的现象: `模板既是函数,又是容器`

```cpp
//在使用的时候通过偏特化来萃取参数包
template<class...> struct typelist {};

template<class T> struct QAQ;
template<class ...Ts> struct QAQ<typelist<Ts...>> { ... };
```

## 宽泛的TypeList

再进一步,可以使用模板模板参数直接去掉所谓的 `typelist` - 因为任何模板都是容器:

```cpp
// 定义宏, typelist 为 type 列表
#define typelist template<class...> class

template<class T> struct QAQ;
template<typelist L, class ...Ts> struct QAQ<L<Ts...>> { ... };
```

## 一些基础的操作

有了容器,就可以定义操作了,首先定义几个重要的操作:

+ `map` - 对所有元素应用元函数 `F`
+ `length` - 容器长度
+ `cast` - 把元素转移到其他容器
+ `contain` - 检查元素是否存在
+ `contact` - 连接n个list

代码如下:

```cpp
template<class> struct list_helper;
template<typelist L, class ...Ts> 
struct list_helper
{
    template<template<class> class F> using map = L<F<Ts>...>;
    
    constvalue length = sizeof...(Ts);

    template<typelist V> using cast = V<Ts...>;
    
    template<typename T> constvalue contain = std::disjunction_v<std::is_same<T, Ts>...>;
}

template<class L, template<class> class F>
using map_t = typename list_helper<L>::template map<F>;

template<class T> 
constexpr auto length_v = list_helper<T>::length;

template<class T, typelist F> 
using cast_t = typename list_helper<T>::template cast<F>;

template <class L, class T> 
constexpr auto contain_v = list_helper<L>::template contain<T>;
```
