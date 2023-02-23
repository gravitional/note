# CRTP介绍, 使用和原理

[CRTP介绍, 使用和原理](https://zhuanlan.zhihu.com/p/476001202)

## CRTP介绍

CRTP全称是curious recurring template pattern, 是一种c++的设计模式,
精巧地结合了继承和模板编程的技术. 可以用来给c++的class提供额外功能, 实现 `静态多态` 等.

## CRTP使用

下边是cppreference上的示例:

```cpp
#include <iostream>
template <class Derived>
struct Base {
    // static 转型到 Derived 类
    void name() {(void static_cast<Derived *>(this))->impl();}};
struct D1 : public Base<D1>{
    void impl(){std::cout << "D1::impl()"<<std::endl; }};
struct D2 : public Base<D2>{
    void impl(){std::cout << "D2::impl()"<<std::endl; }};

int main(){
    Base<D1> b1;    b1.name();
    Base<D2> b2;    b2.name();
    // 作为对比
    D1 d1;    d1.name();
    D2 d2;    d2.name();
}
```

可以看出, `CRTP` 模式给 `D1` 和 `D2` 这两个类提供了统一的 `name` 函数,
但是各自的实现又不相同. 可以称之为 `静态多态`.
不过说实话我觉得这个例子非常牵强, 直接调用 `imp` 不是一样?
`D1` 和 `D2` 本来就有统一的的 `imp` 接口.
不过, 这个例子过于简单了, 在更复杂的场景中, 尤其是模板编程中,
`CRTP` 提供的明确语义会让代码明确清晰.

`CRTP` 十分晦涩, 相信大家和我一样,
第一次看见 `CRTP` 连它为啥是合法的语法都搞不懂, 更别提它有什么功能了.
接下来就来讲这个语法的合法性和原理.

### CRTP 原理

>在 c++ 中, 模板类的 `成员函数` 只有在用到的时候才会实例化,

对象中的 `成员函数` 存储的应该是 `指针`, 所以 `Base<Derived1>` 的大小是可以确定的.
待到实例化 `PrintType` 时, `Derived1` 已经是有确定大小的 `class` 类型了,
所以调用不会报错.

看到这里, 大家应该对 `static_cast` 的工作原理就很清楚了,

+ 首先从语法上, `static_cast` 就可以把 `Base类型` 变量 转成 `Derive类型`,
由用户去保证使用上不出问题, 比如不能使用 `Derived` 有, 而Base没有的变量.
+ 其次, 在CRTP中, `this指针` 指向的内存其实就是 `Derived类` 的完整内存, 所以不会有任何问题.

## 静态获取类型名

下边看 `CRTP` 的另一个例子

```cpp
#include <iostream>
#include <string_view>

template <typename Derived>
class Base2{
public:
    void PrintType() const  {
        std::string_view name = typeid(Derived).name();
        std::cout << name.substr(1, name.size() - 1) << std::endl;  }};

class Derived1 : public Base2<Derived1>{};
class Derived2 : public Base2<Derived2>{};

int main()
{
    //---------- example 2
    Derived1 derived1;      Derived2 derived2;
    derived1.PrintType(); derived2.PrintType();
    return 0;
}
```

这个例子展示了 `CRTP` 给派生类提供功能的作用,
这里 `Derived1` 和 `Derived2` 不用写任何代码,
只需用 `CRTP` 继承 `Base`, 就可以获得 `PrintType` 函数了.

另外, 像 `eigen` 这类数值计算库都会结合 Expression Template 和 CRTP 来实现, 等我有本事了再来展开.

### 补充

这种技巧对于 `成员变量` 不一定适用, 修改上面代码中的 `Base2`:

```cpp
template <typename Derived>
class Base2{
public:
    void PrintType() const  {
        std::string_view name = typeid(Derived).name();
        std::cout << name.substr(1, name.size() - 1) << std::endl; }
Derived a{};
};
```

其他部分不变再次编译, 报错信息如下:

```bash
error C2079: "Base2<Derived1>::a" 使用未定义的 class "Derived1"
```

这个例子跟原版相比, 只多了个 `成员变量`, 就产生了编译错误.
在 `c++` 的内存布局中, 派生类中 `父类` 的成员变量会排到前边,
初始化基类的时候, `Derived1` or `Derived1` 还不存在,
编译器不知道如何分配空间. 所以拒绝工作.
如果将新增成员修改成

```cpp
Derived* a{};
```

是可以通过编译的.
