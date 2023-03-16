# cpp error

## Run-Time Check Failure #3

[Run-Time Check Failure #3](https://blog.csdn.net/qq_29894329/article/details/51184920)

类似下面这种报错:

    Run-Time Check Failure #3 - The variable 'p' is being used without being initialized.

代码示例:

```cpp
calc::CoordSysUtil *cs;
cs->SetCartesianCS(axisX,axisY,axisZ)
```

通过分析发现这与RTC(Run-Time Check, 运行时检查)机制有关(以下都是以VS2012为标准).
首先普及一下RTC(Run-Time Check)机制, 包括:
`堆栈帧`(RTCS), `未初始化变量`(RTCu), `两者都有`, 以及 `默认值` 四种.
在 VS2022 编译器中, `项目`->`属性`->`配置属性`->`C/C++` ->`代码生成`->`基本运行时检查`

`Type *var` 声明的变量, 必须用指针类型初始化, 也就是需要用 `new` 或其他方式返回的指针.

## inline, 无法解析的外部符号

[inline和无法解析的外部符号](https://blog.csdn.net/Sleeping_Zky/article/details/81383057)

## 问题原因

因为想尽可能的节省开销,
所以想把一些 构造函数 析构函数 简单函数 等都命名为 `inline函数`,
但同时遵循定义和声明分开的原则,
在另一个文件中将这些类的函数定义为了 `inline函数`, 然后就发生了上述悲剧.

所谓 `inline`, 就是编译器实际 `不产生函数` 和函数调用, 而是将函数实现直接内嵌在调用的代码处.
既然如此, 在你的静态库中定义一个 `inline函数`, 又要调用者看见它怎么可能呢?
C++编译器只能将源代码嵌入, 而没办法从外部库中将机器代码嵌入.

解决办法

1. 在类的 `声明` 中就直接完成内联函数的定义, 无须 `inline` 关键字
1. 在类的 声明文件(.h) 文件的后面, 用 `inline` 关键字完成定义, 因为和声明在同一个文件中, 所以编译器可以找到他
1. 在别的文件中定义时候, 就不能再加 `inline` 关键字了

## vector 容器, unique_ptr 尝试引用已删除的函数

[C++ Error C2280 尝试引用已删除的函数](https://blog.csdn.net/qq_26735913/article/details/109688203)

如果把 `new` 对象的 `unique_ptr` 放在 `std::vector` 等容器 `Con` 中,
然后又定义 `Con` 对象的访问接口, 例如 `GetXXX` 之类,
用于获取 `vector` 内部元素的函数, 则会引发编译错误:

```bash
xxx error C2280: "unique_ptr(const unique_ptr &)": 尝试引用已删除的函数
```

因为使用 `Getxx` 获取容器内的元素时, 需要对元素进行 `拷贝构造`,
而 `unique_ptr` 的拷贝构造函数被删除, 它不允许拷贝(所以才叫做 unique pointer);
而如果 `Con` 的访问接口 改成 `移动构造`, 那么元素将被移出 vector 容器, 不再被 vector 管理,
也就是只能访问一次, 那一般这样使用 vector 就没啥意义了.

最简单的解决方式是, 使用 `裸指针`,
并且在 `Con` 的封闭类的析构函数中, `delete` 这些 `new` 出来的对象.

## 没有为显式模板实例化请求提供适当的定义

[编译器警告(等级1)C4661](https://learn.microsoft.com/zh-cn/cpp/error-messages/compiler-warnings/compiler-warning-level-1-c4661?view=msvc-170)
[警告C4661: 没有为显式模板实例化请求提供适当的定义](https://www.codenong.com/44160467/)

原因: 未定义模板类的成员.
应该将 显式实例化的语句 `template xxx` 移动到 源文件(`.cpp`)中,
这样 编译器才能找到 `构造函数` 等成员函数的定义

```cpp
// C4661.cpp
// compile with: /W1 /LD
template<class T> class MyClass {
public:
   void i();   // 声明, 但没有定义
};
template MyClass< int >;  // C4661
```
