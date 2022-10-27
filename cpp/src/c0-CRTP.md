# CRTP介绍, 使用和原理

## CRTP介绍

CRTP全称是curious recurring template pattern, 是一种c++的设计模式,
精巧地结合了继承和模板编程的技术. 可以用来给c++的class提供额外功能, 实现 `静态多态` 等.

## CRTP使用

下边是cppreference上的示例:

```cpp
#include <iostream>
template <class Derived>
struct Base {
    void name() {(void static_cast<Derived *>(this))->impl();}};
struct D1 : public Base<D1>{
    void impl(){std::cout << "D1::impl()\n";}};
struct D2 : public Base<D2>{
    void impl(){std::cout << "D2::impl()\n";}};

int main(){
    Base<D1> b1;    b1.name();
    Base<D2> b2;    b2.name();

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

下边看 `CRTP` 的另一个例子

![exa2](https://pic3.zhimg.com/80/v2-4918ab10e73fa4e12b9a09ef8ee06566_720w.webp)

这个例子展示了CRTP给派生类提供功能的作用, 
这里Derived1和Derived2不用写任何代码, 只需用CRTP继承Base, 就可以获得PrintType函数了.

另外, 像eigen这类数值计算库都会结合Expression Template和CRTP来实现, 等我有本事了再来展开.

## CRTP原理

CRTP十分晦涩, 相信大家和我一样, 
第一次看见CRTP连它为啥是合法的语法都搞不懂?更别提它有什么功能了. 
接下来就来讲这个语法的合法性和原理.

以PrintType实现为例, 第13行Derived1作为 `Base<Derived1>` 是还是一个不完整的类型, 
但是这里编译器不报错误, 因为在c++中, 模板类的成员函数只有在用到的时候才会实例化, 
比如在20行, 而这时Derived1已经是一个完整的class类型了.

这种惰性初始化对于成员变量是不适用的:

![imag](https://pic3.zhimg.com/80/v2-a0785915b12c8ce1e69478a266987092_720w.webp)

![out](https://pic3.zhimg.com/80/v2-a5a2b248f48ac964f138c0fafa0dcf9e_720w.webp)

这个例子跟原版相比, 只多了一个成员变量, 就产生了编译错误. 
这是因为在c++的内存布局中, 派生类中父类的成员变量会排到前边, 如果父类是一个不完整类, 不知道父类内存占多大, 编译器就拒绝工作了.

看到这里, 大家应该对static_cast的工作原理就很清楚了, 
首先从语法上, static_cast就可以把Base类型变量转成Derive类型, 
由用户去保证使用上不出问题, 比如不能使用Derived有而Base没有的变量. 
其次, 在CRTP中, this指针指向的内存其实就是Derived类的完整内存, 所以不会有任何问题. 
