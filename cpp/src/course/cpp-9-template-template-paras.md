# 模板的模板参数

[c++11-17 模板核心知识(十二)—— 模板的模板参数](https://zhuanlan.zhihu.com/p/338915581)

## 概念

一个模板的参数是模板类型. (Template Template Parameters)

## 举例

在 c++11-17 模板核心知识(二)—— 类模板 中(见参考资料),
如果我们想要允许指定存储 `Stack` 元素的容器, 是这么做的:

```cpp
// Cont 默认类型是 std::vector<T>
template <typename T, typename Cont = std::vector<T>>
class Stack {
private:
  Cont elems; // elements
  ......
};
```

使用:

```cpp
Stack<double,std::deque<double>> dblStack;
```

但是这样的缺点是需要指定元素类型(`double`)两次, 然而这两个类型是一样的.
使用 `模板的模板参数`(Template Template Parameters),
允许我们在声明 `Stack` 类模板的时候只指定 `容器的类型` 而不去指定容器中 `元素的类型`. 例如:

```cpp
//Cont 默认类型为 std::deque<Elem>
template <typename T, template <typename Elem> class Cont = std::deque>
class Stack {
private:
  Cont<T> elems; // 指定 elem 的类型是 Cont<T>, 即令 Elem is T
public:
  void push(T const &); // push element
  void pop();           // pop element
  T const &top() const; // return top element
  bool empty() const {  // return whether the stack is empty
    return elems.empty();
  }
  ...
};
```

使用:

```cpp
// 基于 vector 的 int stack, 元素类型自动推断为 int
Stack<int, std::vector> vStack;
```

与第一种方式的区别是: 第二个 `模板参数` 是一个类模板:

```cpp
template<typename Elem> class Cont
```

默认值从 `std::deque<T>` 改为了 `std::deque`.

在 C++17 之后, 模板的模板参数中的 `class` 也可以使用 `typename`,
但是不可以使用 `struct` 和 `union`:

```cpp
template <typename T,
          template <typename Elem> typename Cont = std::deque>
class Stack {       // ERROR before C++17
  ...
};

......

template<template<typename X> class C> // OK
void f(C<int>* p);

template<template<typename X> struct C> // ERROR: struct not valid here
void f(C<int>* p);

template<template<typename X> union C> // ERROR: union not valid here
void f(C<int>* p);
```

当然, 由于模板的模板参数中的 `Elem` 没有用到, 可以省略:

```cpp
template <typename T, template <typename> class Cont = std::deque>
class Stack {
  ...
};
```

另外注意一点, 模板的 `模板参数` 中的 `模板参数`, 只能和模板的 `模板参数` 配合用.
有点绕, 举个例子:

```cpp
template< template<typename T, T*> class Buf >  // OK
class Lexer {
    static T* storage;        // ERROR: 内层嵌套参数不能在这里使用
    ...                       // ERROR: template template parameter cannot be used here
};
```

## 模板的模板参数的参数匹配 Template Template Argument Matching

大家可以尝试自己编译一下上面的代码, 可能会出现下列问题:

```bash
error: template template argument has different template parameters than its corresponding template template parameter
template <typename T, template <typename Elem> class Cont = std::deque>

...

/Library/Developer/CommandLineTools/usr/bin/../include/c++/v1/deque:1197:1: note: too many template parameters in template template argument
template <class _Tp, class _Allocator /*= allocator<_Tp>*/>
```

意思是 `std::deque` 和 `Cont` 不匹配.
标准库的 `std::deque` 有两个参数, 还有一个默认参数 `Allocator` :

```cpp
template <class _Tp, class _Allocator = allocator<_Tp> > class _LIBCPP_TEMPLATE_VIS deque;
```

### 解决办法一

将 `Cont` 和 `std::deque` 的参数匹配即可:

```cpp
template < typename T,
           template <typename Elem, typename Alloc = std::allocator<Elem>>
           class Cont = std::deque >
class Stack {
......
};
```

这里的 `Alloc` 没有用到, 同样可以省略.

成员函数定义举例:

```cpp
template<typename T, template< typename,typename > class Cont>
void Stack<T,Cont>::push (T const& elem) {
    elems.push_back(elem);       // append copy of passed elem
}
```

### 解决办法二

利用c++11-17 模板核心知识(四)—— 可变参数模板 Variadic Template

```cpp
template < typename T,
           template <typename......>
           class Cont = std::deque >
class Stack {
......
};
```

但是, 这点对于 `std::array` 无效,
因为 `std::array` 的第二个参数是 `非类型模板参数` Nontype Template Parameters[3]:

```cpp
template<typename T, size_t N>
class array;
```

假如使用

```cpp
Stack<int,std::array> s;
```

那么编译器会报错:

```bash
/Library/Developer/CommandLineTools/usr/bin/../include/c++/v1/array:126:29:
    note: template parameter has a different kind in template argument
template <class _Tp, size_t _Size>
                            ^
main.cc:22:33: note: previous template template parameter is here
          template <typename... Elem>
                                ^
```

## 参考资料

[c++11-17 模板核心知识(二)—— 类模板](https://github.com/zhangyachen/zhangyachen.github.io/issues/152)
[c++11-17 模板核心知识(四)—— 可变参数模板 Variadic Template](https://github.com/zhangyachen/zhangyachen.github.io/issues/154)
[非类型模板参数 Nontype Template Parameters](https://github.com/zhangyachen/)

## 例子

函数模板, 其中使用了嵌套模板参数

```cpp
template <typename T, template <typename T> class Vec>
void PromoteDim(Vec<T> &v_new, const Vec<T> &v_old, int dim3 = 3, int dim2 = 2)
{
    assert(v_new.Rows == dim3);
    v_new.SetZero();
    Util::Copy(v_new.Data(), v_old.Data(), dim2);
}
```
