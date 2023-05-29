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

通过分析发现这与 `RTC`(Run-Time Check, 运行时检查)机制有关(以下都是以VS2012为标准).
首先普及一下RTC(Run-Time Check)机制, 包括:
`堆栈帧`(RTCS), `未初始化变量`(RTCu), `两者都有`, 以及 `默认值` 四种.
在 VS2022 编译器中, `项目`->`属性`->`配置属性`->`C/C++` ->`代码生成`->`基本运行时检查`

`Type *var` 声明的变量, 必须用指针类型初始化, 也就是需要用 `new` 或其他方式返回的指针.

## inline, 无法解析的外部符号

[inline和无法解析的外部符号](https://blog.csdn.net/Sleeping_Zky/article/details/81383057)

### 问题原因

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

## 引发了异常: 读取访问权限冲突

[C++读取访问权限冲突引发异常问题](https://blog.csdn.net/gabriel1217/article/details/110083837)

报错信息形如:

```bash
std::List_alloc<std::List_base_types<std::pair<int const, ETFilmBaseResistBase* __ptr64>,
std::allocator<std::List_base_types<std::pair<int const, ETFilmBaseResistBase* __ptr64>>>::Mysize(...) 返回 0x120
```

经检查是由于:
边界单元可能没有赋予材料, 而`mat` 指针没有检测是否为 `nullptr`,

```cpp
const auto* const mat=GetThreadMaterial();
```

### 访问数组越界

当采用线性表的顺序结构, 例如顺序表, 队列, 栈等, 用数组存储数据时,
若将要读取数据的位置超出了当前数组的长度, 就会发生数组访问越界的状况.

### 空指针异常

这主要发生在通过指针读取数据时, 比如在使用链表的过程中.

当然, 不止链表, 空指针异常还会出现在很多其他情况下,
比如在数据库查询, 指针未初始化时也会产生空指针异常.

## 编译错误: 表达式必须包含类类型

可能是 VS 的自动补全没有识别出类型信息,
一般指针后面的 `.` 访问符会自动转换成 `->`, 有的时候 `VS` 识别不出来, 需要手动添加,
同理, 有的时候会识别错误, 把 非指针后面的 `.` 改成 `->`,
这时候按下 `backspace` 回退一格即可.

## 程序运行时突然卡死, 调用栈崩溃在 ~Vector, ~Matrix ~Dense 等等.

可以是 Vector 互相复制数据时, 输入了错误的 长度(size_t), 会引发奇怪的随机错误/异常,
因为程序的运行栈可能会被破坏.
例如 `Util::Copy` 的长度参数 `size_t n`, 指的是数组的长度, 而不是 `byte` 的长度.
`memcpy` 的长度参数才是 `byte` 长度,  如果混淆就会发生上述错误.

```cpp
Util::Copy(T* dst, T* scr, size_t n);
memcpy(T* dst, const T* scr, size_t Size)
```

## 超出修饰名的长度, 名称被截断

[编译器警告(等级 1)C4503](https://learn.microsoft.com/zh-cn/cpp/error-messages/compiler-warnings/compiler-warning-level-1-c4503?view=msvc-170)

此编译器警告已过时, 不会在 Visual Studio 2017 及更高版本的编译器中生成.

修饰名超过编译器限制 (4096), 已截断.
为避免此警告和截断, 请减少使用的自变量数目或标识符的名称长度.
超过编译器限制的修饰名应用了哈希, 不会有名称冲突的危险.

使用旧版 Visual Studio 时, 如果代码重复包含模板中所述的模板, 可能会发出此警告.
例如映射的映射(来自 C++ 标准库).
在这种情况下, 可以将 typedef 设置为包含映射的类型(例如 struct).

但你可以决定不重构代码.  可以推出生成 C4503 的应用程序,
但如果截断符号出现链接时间错误, 则可能更难以确定错误中符号的类型.
调试也可能更加困难;调试器可能很难将符号名称映射到类型名称.
但程序的正确性不受截断名称的影响.

示例
以下示例在低于 Visual Studio 2017 的编译器中生成 C4503:

```cpp
// C4503.cpp
// compile with: /W1 /EHsc /c
// C4503 expected
#include <string>
#include <map>

class Field{};

typedef std::map<std::string, Field> Screen;
typedef std::map<std::string, Screen> WebApp;
typedef std::map<std::string, WebApp> WebAppTest;
typedef std::map<std::string, WebAppTest> Hello;
Hello MyWAT;
```

## 多线程, 程序结束时, Material 类析构时引发异常

可能是手动管理的资源, 在多线程下析构了多次.
使用 `MPIUtil::IsHost()`, 只在主线程进行析构.

## 初始化操作由 case 标签跳过

[初始化操作由 case 标签跳过](https://blog.csdn.net/hhhenjoy/article/details/117856700)

如果要在case里面定义变量, 需要用括号括起来{}

```cpp
// BUG!
char c;
cin >> c;
switch (c)
{
case '1':
    cout << "input the complex:";
    double a, b;
    cin >> a >> b;
    cout << sqrt(a * a + b * b);
    break;
case '2':
    // …………
}

// RIGHT!
else if (input == "4")
{
    char c;
    cin >> c;
    switch (c)
    {
    case '1':
    {
        cout << "input the complex:";
        double a, b;
        cin >> a >> b;
        cout << sqrt(a * a + b * b);
        break;
    }
    case '2':
        // …………
    }
}
```

## C++编译报错: error: expected constructor, destructor, or type conversion before `(` token

[error: expected constructor, destructor](https://blog.csdn.net/Dontla/article/details/129241996)

错误代码, 在某个文件中

```cpp
int ticket_sum = 10;

// 创建互斥锁
pthread_mutex_t myMutex;
pthread_mutex_init(&myMutex, nullptr);

// 另一些函数
void *sell_ticket(void *arg)
{...}
```

报错原因:
C++中, 全局域只能声明, 初始化变量; 不能用于赋值, 运算, 调用函数

解决方案
改成这样就好了:

```cpp
int ticket_sum = 10;

void initMutex()
{
// 创建互斥锁
pthread_mutex_t myMutex;
pthread_mutex_init(&myMutex, nullptr);
...
}

// 另一些函数
void *sell_ticket(void *arg)
{...}
```

把调用语句放到函数里, 然后再调用函数

## undefined reference to std::cout

[gcc编译报错: undefined reference to std::cout](https://blog.csdn.net/weixin_41010198/article/details/117523288)

编译文件: `test.cpp` 内容如下:

```cpp
#include <iostream>
using namespace std;
int main()
{
    cout << "Hello, world!" << endl;
    return 0;
}
```

上面使用gcc编译test.cpp会报错(参考):

我们常见的编译器有两个:
gcc 编译器
g++ 编译器
gcc和g++都是GNU(组织)的编译器.

gcc和g++编译器的区别

+ g++:  会把 `.c` 和 `.cpp` 的文件都当作是C++的源程序进行编译.
+ gcc: 会把 `.c` 的程序当作是C的源程序进行编译, `.cpp` 的程序当作是 `C++` 的源程序进行编译

解决上面的错误, 就是把gcc编译器换成g++编译器, 即使是把.cpp的后缀改成.c的后缀也可以正常编译!

## 控制台乱码, 控制台中文

在 main.cpp 中添加

```cpp
//... 在这个位置添加
#ifdef _WIN32
// ...

system("chcp 65001")
#endif
```

## 大型整数

[c++大型整数乘积溢出, 注意给值后面添加 ui64 i64等后缀](https://blog.csdn.net/yezishuang/article/details/108619191)

```cpp
uint64_t n1 = 1000 * 1000 * 1000 * 1000; //(错误)
uint64_t n2 = (uint64_t)1000 * (uint64_t)1000 * (uint64_t)1000 * (uint64_t)10;
uint64_t n3 = 1000ui64 * 1000ui64 * 1000ui64 * 10ui64;
```

## LNK2001 连接错误

[Error LNK2001 无法解析的外部符号 的几种情况及解决办法](https://blog.csdn.net/shenyulv/article/details/6699836)

## 编译器错误 C2766, 编译器错误 C2766

不允许重复显式专用化.  有关详细信息, 请参阅 [函数模板的显式专用化](https://learn.microsoft.com/zh-cn/cpp/cpp/explicit-specialization-of-function-templates?view=msvc-170).

以下示例生成 C2766:

```C++
// C2766.cpp
// compile with: /c
template<class T>
struct A {};

template<>
struct A<int> {};

template<>
struct A<int> {};   // C2766
// try the following line instead
// struct A<char> {};
```
