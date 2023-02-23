# C++ auto 类型推导

[C++ auto(类型推导)精讲](http://c.biancheng.net/view/3718.html)

C++11 引入了 auto 和 decltype 关键字实现类型推导,
通过这两个关键字不仅能方便地获取复杂的类型, 而且还能简化书写, 提高编码效率.
本节我们先讲解 auto 关键字, 下节再讲解 decltype 关键字.

## auto 关键字的新意义

用过 `C#` 的读者可能知道, 从 Visual C#3.0 开始, 在方法范围中声明的变量可以具有隐式类型 `var`.
例如, 下面这样的写法(C#代码):

```c#
var i = 10;  // 隐式(implicitly)类型定义
int i = 10;  // 显式(explicitly)类型定义
```

其中, 隐式的类型定义也是强类型定义, 前一行的隐式类型定义写法和后一行的显式写法是等价的.

不同于 Python 等动态类型语言的运行时变量类型推导, 隐式类型定义的类型推导发生在编译期.
它的作用是让编译器自动推断出这个变量的类型, 而不需要显式指定类型.

现在, C++11中 也拥有了类似的功能: `auto` 类型推导. 其写法与上述 `C#` 代码等价:

```cpp
auto i = 10;
```

是不是和 C# 的隐式类型定义很像呢?

下面看下 auto 的一些基本用法:

```cpp
auto x = 5;                 // OK: x是int类型
auto pi = new auto(1);      // OK: pi被推导为int*
const auto *v = &x, u = 6;  // OK: v是const int*类型, u是const int类型
static auto y = 0.0;        // OK: y是double类型
auto int r;                 // error: auto不再表示存储类型指示符
auto s;                     // error: auto无法推导出s的类型
```

在上面的代码示例中:
字面量 5 是一个 `const int` 类型,
变量 x 将被推导为 int 类型(const被丢弃, 后面说明), 并被初始化为 5.
pi 的推导说明 auto 还可以用于 new 操作符.
在例子中, new 操作符后面的 auto(1) 被推导为 int(1), 因此 pi 的类型是 `int*`.
接着, 由 `&x` 的类型为 `int*`, 推导出 `const auto*` 中的 auto 应该是 int,
于是 v 被推导为 `const int*`, 而 u 则被推导为 `const int`.
最后 y, r, s 的推导过程比较简单, 就不展开讲解了.
读者可自行在支持 C++11 的编译器上实验.

v 和 u 的推导需要注意两点:
虽然经过前面 `const auto*v=&x` 的推导,
`auto` 的类型可以确定为 int 了, 但是 u 仍然必须要写后面的 `=6`, 否则编译器不予通过.
u 的初始化不能使编译器推导产生二义性.
例如, 把 u 的初始化改成 `u=6.0`, 编译器将会报错:

```cpp
const auto *v = &x, u = 6.0;
error: inconsistent deduction for 'const auto': 'int' and then 'double'
```

由上面的例子可以看出来, `auto` 并不能代表一个实际的类型声明(如 s 的编译错误),
只是一个类型声明的 "占位符". 使用 auto 声明的变量必须马上初始化,
以让编译器推断出它的实际类型, 并在编译时将 `auto` 占位符替换为真正的类型.

细心的读者可能会发现, `auto` 关键字其实并不是一个全新的关键字.
在旧标准中, 它代表"具有自动存储期的局部变量", 不过其实它在这方面的作用不大, 比如:

```cpp
auto int i = 0;  // C++98/03, 可以默认写成 int i = 0;
static int j = 0;
```

上述代码中的 auto int 是旧标准中 auto 的使用方法.
与之相对的是下面的 static int, 它代表了静态类型的定义方法.

实际上, 我们很少有机会这样直接使用 auto, 因为非 static 的局部变量默认就是"具有自动存储期的".

考虑到 auto 在 C++ 中使用的较少, 在 C++11 标准中,
auto 关键字不再表示 `存储类型指示符`
(storage-class-specifiers, 例如 static, register, mutable 等),
而是改成了一个类型指示符(type-specifier), 用来提示编译器对此类型的变量做类型的自动推导.

## auto 的推导规则

从上面的示例中可以看到 auto 的一些使用方法.
它可以同指针, 引用结合起来使用,
还可以带上 cv 限定符(cv-qualifier, const 和 volatile 限定符的统称).

再来看一组例子:

```cpp
int x = 0;
auto *a = &x;      // a -> int*, auto被推导为int
auto  b = &x;      // b -> int*, auto被推导为int*
auto &c = x;       // c -> int&, auto被推导为int
auto  d = c;       // d -> int , auto被推导为int
const auto e = x;   // e -> const int
auto f = e;         // f -> int
const auto& g = x;  // g -> const int&
auto& h = g;        // h -> const int&
```

由上面的例子可以看出:

+ a 和 c 的推导结果是很显然的, `auto` 在编译时被替换为 `int`, 因此 a 和 c 分别被推导为 `int*` 和 `int&`.
+ b 的推导结果说明, 其实 auto 不声明为指针, 也可以推导出指针类型.
+ d 的推导结果说明当表达式是一个引用类型时, auto 会把引用类型抛弃, 直接推导成原始类型 int.
+ e 的推导结果说明, const auto 会在编译时被替换为 const int.

+ f 的推导结果说明, 当表达式带有 const(实际上 volatile 也同样)属性时,
auto 会把 const 属性抛弃掉, 推导成 non-const 类型 int.
+ g, h 的推导说明, 当 auto 和 `引用`(换成`指针`也同样)结合时,
auto 的推导将保留表达式的 const 属性.

通过上面的一系列示例, 可以得到下面这两条规则:

+ 当不声明为指针或引用时, auto 的推导结果, 和初始化表达式抛弃 `引用`和 `cv` 限定符后类型一致.
+ 当声明为指针或引用时, auto 的推导结果, 将保持初始化表达式的 cv 属性.

看到这里, 对函数模板自动推导规则比较熟悉的读者可能会发现,
auto 的推导和函数模板参数的自动推导有相似之处.
比如上面例子中的 auto, 和下面的模板参数自动推导出来的类型是一致的:

```cpp
template <typename T> void func(T   x) {}        // T   -> auto
template <typename T> void func(T * x) {}        // T * -> auto *
template <typename T> void func(T & x) {}        // T & -> auto &
template <typename T> void func(const T   x) {}  // const T   -> const auto
template <typename T> void func(const T * x) {}  // const T * -> const auto *
template <typename T> void func(const T & x) {}  // const T & -> const auto &
```

注意: auto 是不能用于函数参数的.
上面的示例代码只是单纯比较函数模板参数推导和 auto 推导规则的相似处.

因此, 在熟悉 auto 推导规则时, 可以借助函数模板的参数自动推导规则来帮助和加强理解.

## auto 的限制

上面提到了 auto 是不能用于函数参数的. 那么除了这个之外, 还有哪些限制呢?

我们通过下面的代码来演示一下 auto 的限制:

```cpp
void func(auto a = 1) {}          // error: auto不能用于函数参数
struct Foo
{
    auto var1_ = 0;               // error: auto不能用于非静态成员变量
    static const auto var2_ = 0;  // OK: var2_ -> static const int
};
template <typename T>
struct Bar {};
int main(void)
{
    int arr[10] = {0};
    auto aa     = arr;   // OK: aa -> int *
    auto rr[10] = arr;   // error: auto无法定义数组
    Bar<int> bar;
    Bar<auto> bb = bar;  // error: auto无法推导出模板参数
    return 0;
}
```

在 Foo 中, auto 仅能用于推导 static const 的整型或者枚举成员
(因为其他静态类型在 C++ 标准中无法就地初始化),
虽然 C++11 中可以接受非静态成员变量的就地初始化,
但却不支持 auto 类型非静态成员变量的初始化.

在 main 函数中, auto 定义的数组 rr 和 `Bar<auto>bb` 都是无法通过编译的.

注意　main 函数中的 aa 不会被推导为 int[10], 而是被推导为 `int*`.
这个结果可以通过 `auto` 与函数模板参数自动推导的对比来理解.

## 什么时候用 auto

前面说了这么多, 最重要的是, 应该在什么时候使用 auto 呢?
在 C++11 之前, 定义了一个 stl 容器以后, 在遍历的时候常常会写这样的代码:

```cpp
#include <map>
int main(void)
{
    std::map<double, double> resultMap;
    // ...
    std::map<double,double>::iterator it = resultMap.begin();
    for(; it != resultMap.end(); ++it)
    {
        // do something
    }
    return 0;
}
```

观察上面的迭代器(`iterator`)变量 `it` 的定义过程, 总感觉有点憋屈.
其实通过 `resultMap.begin()`, 已经能够知道 `it` 的具体类型了,
却非要书写上长长的类型定义才能通过编译.
来看看使用了 auto 以后的写法:

```cpp
#include <map>
int main(void)
{
    std::map<double, double> resultMap;
    // ...
    for(auto it = resultMap.begin(); it != resultMap.end(); ++it)
    {
        // do something
    }
    return 0;
}
```

再次观察 it 的定义过程, 是不是感到清爽了很多?

再看一个例子, 在一个 `unordered_multimap` 中查找一个范围, 代码如下:

```cpp
#include <map>
int main(void)
{
    std::unordered_multimap<int, int> resultMap;
    // ...
    std::pair<
        std::unordered_multimap<int,int>::iterator,
        std::unordered_multimap<int, int>::iterator >
        range = resultMap.equal_range(key);
    return 0;
}
```

这个 `equal_range` 返回的类型声明显得烦琐而冗长, 而
且实际上并不关心这里的具体类型(大概知道是一个 std::pair 就够了).
这时, 通过 auto 就能极大的简化书写, 省去推导具体类型的过程:

```cpp
#include <map>
int main(void)
{
    std::unordered_multimap<int, int> map;
    // ...
    auto range = map.equal_range(key);
    return 0;
}
```

另外, 在很多情况下我们是无法知道变量应该被定义成什么类型的, 比如下面的例子.

### 实例: auto 简化函数定义的示例

```cpp
class Foo
{
    public:
    static int get(void)
    {
        return 0;
    }
};
class Bar
{
    public:
    static const char* get(void)
    {
        return "0";
    }
};
template <class A>
void func(void)
{
    auto val = A::get();
    // ...
}
int main(void)
{
    func<Foo>();
    func<Bar>();
    return 0;
}
```

在这个例子里, 我们希望定义一个泛型函数 `func`,
对所有具有静态 `get` 方法的类型 `A`, 在得到 `get` 的结果后做统一的后续处理.
若不使用 `auto`, 就不得不对 `func` 再增加一个模板参数,
并在外部调用时手动指定 `get` 的返回值类型.

上面给出的各种示例仅仅只是实际应用中很少的一部分,
但也足以说明 auto 关键字的各种常规使用方法.
更多的适用场景, 希望读者能够在实际的编程中亲身体验.

注意　auto 是一个很强大的工具, 但任何工具都有它的两面性. 不
加选择地随意使用 auto, 会带来代码可读性和维护性的严重下降.
因此, 在使用 auto 的时候, 一定要权衡好它带来的"价值"和相应的"损失".
