# c++ lambda 函数

[C++中的Lambda函数](https://blog.csdn.net/weixin_36035585/article/details/112122959)
[All About Lambda Functions in C++: from C++11 to C++17](https://hackernoon.com/all-about-lambda-functions-in-cfrom-c11-to-c17-2t1j32qw)

## 什么是lambda函数?

本文标题有点误导. 因为 lambda并不总是函数指针, 它一个表达式.
但是为了简单起见, 我一直都称它为函数.
那么在本文中, 我将会交替使用lambda函数和表达式来称呼.

Lambda函数是一段简短的代码片段, 它具有以下特点:

(1)不值得命名(无名, 匿名, 可处理等, 无论您如何称呼)

(2)不会重复使用

换句话说, 它只是语法糖. lambda函数的语法定义为:

```cpp
[ capture list ] (parameters) -> return-type
{
    method definition
}
```

通常,  编译器会计算lambda函数本身的返回类型.
因此, 我们不需要显式指定返回类型, 即 `-> return-type`,
但是在某些复杂的情况下, 编译器无法推断出返回类型, 此时我们需要指定返回类型.

为什么要使用lambda函数?

C++包括许多有用的通用函数, 例如 `std::for_each`,
通常情况下方便了我们的使用.
但是, 当需要考虑特殊需求的时候, 就比较头疼了. 如下代码所示:

```cpp
struct print
{
    void operator()(int element)
    {
        cout << element << endl;
    }
};

int main(void)
{
    std::vector<int> v = {1, 2, 3, 4, 5};
    std::for_each(v.begin(), v.end(), print());
    return 0;
}
```

如果您只在某个特定位置使用一次 `print`, 却写了一个类. 这样的操作代价似乎太大了.
但是, 对于这种情况, 内联代码将更合适, 并且可以通过如下的lambda函数来实现:

```cpp
std::for_each(v.begin(), v.end(),
[](int element) { cout << element << endl; });
```

## lambda函数工作原理

```cpp
[&i] ( ) { std::cout << i; }

// is equivalent to

struct anonymous
{
    int& m_i;
    anonymous(int &i) : m_i(i) {}
    inline auto operator()() const
    {
        std::cout << i;
    }
};
```

如代码所示, 编译器为每一个 `lambda` 函数生成独特的闭合.
捕获列表将成为闭包中的构造函数参数,
如果按值捕获, 则会在闭包中创建相应类型的数据成员.

此外, 您可以在 `lambda` 函数参数中声明变量/对象,
它将成为调用运算符的参数, 即 `operator()`.

## 使用Lambda函数的好处

(1)零成本抽象. 是的!  你没看错. lambda 不会牺牲性能, 运行速度和普通函数一样快.
(2)此外, 代码会变得更加紧凑, 结构层次更加明显和代码可读性更佳.

## 学习lambda表达式

1.通过引用/值捕获

```cpp
int main()
{
    int x = 100, y = 200;
    auto print = [&] { // Capturing object by reference
        std::cout << __PRETTY_FUNCTION__ << " : " << x << " , " << y << std::endl;
    };
    print();
    return 0;
}
```

输出:

```cpp
main()::<lambda()> : 100 , 200
```

在上面的示例中, 我在捕获列表中用到了 `&`. 通过引用的方式捕获变量 `x` 和 `y`.
同样, `=` 表示按值捕获, 这将在闭包内创建相同类型的数据成员, 同时赋上相同的值.
需要注意的是, 参数列表是可选的,
如果不将参数传递给 `lambda` 表达式, 则可以省略空括号.

The following table shows different use cases for the same:

2.Lambda捕获列表

2.1 将lambda作为参数传递

```cpp
template <typename Functor>
void f(Functor functor)
{
    std::cout << __PRETTY_FUNCTION__ << std::endl;
}

/* Or alternatively you can use this*/
void f(std::function<int(int)> functor)
{
    std::cout << __PRETTY_FUNCTION__ << std::endl;
}

int g() { static int i = 0; return i++; }
int main()
{
    auto lambda_func = [i = 0]() mutable { return i++; };
    f(lambda_func); // Pass lambda
    f(g);           // Pass function
}
```

输出:

```cpp
Function Type : void f(Functor) [with Functor = main()::<lambda(int)>]
Function Type : void f(Functor) [with Functor = int (*)(int)]
```

您也可以将lambda函数作为参数传递给其他函数, 就像我上面编写的普通函数一样.
相信您注意到了, 我在捕获列表中声明了变量i, 它将成为数据成员.
所以, 每次调用 `lambda_func` 时, 它将被返回并递增.

2.2lambda捕获 `this` 指针或成员变量

```cpp
class Example
{
public:
    Example() : m_var(10) {}
    void func()
    {
        [=]() { std::cout << m_var << std::endl; }(); // IIFE
    }
private:
    int m_var;
};
int main()
{
    Example e;
    e.func();
}
```

捕获this指针也可以使用 `[this]`,  `[=]` 或者 `[&]`.
在上述任何情况下, 类内数据成员(包括 private)的访问方式与常规方法一样.

可以看到lambda表达式的末尾, 我多写了一个 `()`,

该函数通常在声明之后立刻对其进行调用.
它称为 `IIFE` (立即调用函数表达式, Immediately Invoked Function Expression.)

## C++ lambda函数类型

1.通用lambda

```cpp
const auto l = [](auto a, auto b, auto c) {};

// is equivalent to
struct anonymous
{
    template <class T0, class T1, class T2>
    auto operator()(T0 a, T1 b, T2 c) const
    {
    }
};
```

C++ 14中引入的通用lambda可以使用 `auto` 说明符.

2.可变参数通用 lambda

```cpp
void print() {}
template <typename First, typename... Rest>
void print(const First &first, Rest &&... args)
{
    std::cout << first << std::endl;
    print(args...);
}

int main()
{
    auto variadic_generic_lambda = [](auto... param) {
        print(param...);
    };
    variadic_generic_lambda(1, "lol", 1.1);
}
```

具有可变参数的Lambda在许多情况下非常有用,
例如调试, 使用不同的数据输入重复操作等.

3.mutable lambda函数

通常, lambda的函数调用运算符是const-by-value,
这意味着lambda需要捕获可变值的 关键字时, 需要使用mutable关键字.

```cpp
[]() mutable {}

// is equivalent to

struct anonymous
{
    auto operator()()  // call operator
    {
    }
};
```

4.Lambda作为函数指针

```cpp
#include <iostream>
#include <type_traits>

int main()
{
    auto funcPtr = +[] {};
    static_assert(std::is_same<decltype(funcPtr), void (*)()>::value);
}
```

您可以通过添加命令"+"来强制编译器将lambda生成为函数指针而不是闭包.

5.lambda函数作为返回值

```cpp
const auto less_than = [](auto x) {
    return [x](auto y) {
        return y < x;
    };
};

int main(void)
{
    auto less_than_five = less_than(5);
    std::cout << less_than_five(3) << std::endl;
    std::cout << less_than_five(10) << std::endl;
    return 0;
}
```

再进一步, lambda函数还可以返回另一个lambda函数.
这也为代码的自定义性, 代码可读性和紧凑性带来无限可能.

6.constexpr lambda表达式

从C ++ 17开始, 可以将lambda表达式声明为 constexpr.

```cpp
constexpr auto sum = [](const auto &a, const auto &b)
{ return a + b; };

/*is equivalent to*/
constexpr struct anonymous
{
    template <class T1, class T2>
    constexpr auto operator()(T1 a, T2 b) const
    {
        return a + b;
    }
};

constexpr int answer = sum(10, 10);
```

即使你没有指定 constexpr,
如果它恰好满足所有 constexpr函数的要求, 那么它也会被声明为constexpr.

结束语

希望您喜欢这篇文章. 文中我使用几个简单的小例子来介绍关于lambda的大量复杂问题.
考虑到代码的可表达性和易维护性, 无论您想到什么, 都应该使用lambda,
就像可以在自定义删除器中将其用于智能指针和大多数STL算法一样
