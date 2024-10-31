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

## C2280, vector 容器, unique_ptr 尝试引用已删除的函数

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

## C4661, 没有为显式模板实例化请求提供适当的定义

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

## C4503 超出修饰名的长度, 名称被截断

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

## C2766, 编译器错误

不允许重复显式专用化.
有关详细信息, 请参阅 [函数模板的显式专用化](https://learn.microsoft.com/zh-cn/cpp/cpp/explicit-specialization-of-function-templates?view=msvc-170).

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

## C4834, Compiler warning (level 1)

[Compiler warning (level 1) C4834](https://learn.microsoft.com/en-us/cpp/error-messages/compiler-warnings/c4834?view=msvc-170)

丢弃 带有 `nodiscard` 属性的 函数返回值

### 备注

从 C++17 标准开始, `[[nodiscard]]`属性指定 函数的返回值 不会被丢弃.
如果调用者丢弃了返回值, 编译器会生成 C4834 警告.

要解决此警告, 请考虑代码不使用返回值的原因.
您对函数的使用可能与其意图不符.
您可以将返回值赋值给 `std::ignore`,
或者在有意丢弃返回值的情况下将返回值赋值给 void, 从而规避警告.

在 C++ 11 及更高版本中, 赋值到 `std::ignore` 比将其赋值到 `void` 更受青睐,
因为它使您的意图更加明确, 而且如果在代码分析设置中启用, 也不会触发 C26457 警告.

该警告在 Visual Studio 2017 版本 15.3 中作为第 3 级警告引入.
在 Visual Studio 2017 版本 15.7 中, 它被改为 1 级警告.
在 Visual Studio 2017 版本 15.3 之前的编译器版本中编译时没有警告的代码现在可以生成 C4834.
有关如何禁用特定编译器版本或更高版本中引入的警告的信息,
请参阅[按编译器版本划分的编译器警告](https://learn.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warnings-by-compiler-version?view=msvc-170).

在不修改代码的情况下关闭警告
您可以使用警告实用程序 `#pragma warning(suppress : 4834)` 关闭特定代码行的警告.
您也可以使用警告语句 `#pragma warning(disable : 4834)` 在文件中关闭警告.
使用 `/wd4834` 命令行选项, 可以在命令行编译中全局关闭警告.

在 Visual Studio IDE 中关闭整个项目的警告:

打开项目的 `属性页`(Property Pages) 对话框.
有关如何使用属性页对话框的信息, 请参阅 [属性页](https://learn.microsoft.com/en-us/cpp/build/reference/property-pages-visual-cpp?view=msvc-170).

选择 `配置属性`>`C/C++`>`高级` 页面.

编辑禁用特定警告属性(Disable Specific Warnings), 添加 `4834`.
选择 `确定` 应用更改.

## c++ 找不到用户定义的文本运算符

c++ 编译错误, 对于代码中的 `std::string` 字面字符串 `"xxx"s`.
原因是没有引入 std namespace, 使用 `using namespace std` 即可.

## c++ 重复free内存

[VS报错: 0xC0000005: 读取位置 0xFFFFFFFFFFFFFFFF 时发生访问冲突/无可用源](https://www.cnblogs.com/zzzsj/p/14985746.html)

c++ 异常为:

```bash
0x00007FFC16EA605D (electrics.dll)处(位于 solver.exe 中)引发的异常:
0xC0000005: 读取位置 0xFFFFFFFF FFFFFFFF 时发生访问冲突.
```

原因是重复释放 new 出来的内存, 例如

```cpp
delete _mediator;
delete _adapter0;
```

`_adapter0` 已经被 `_mediator` 管理,
例如 `_mediator` 中使用 `unique_ptr` 自动释放了 `_adapter0` 指向的堆内存,
而后面的 `delete _adapter0;` 重复释放, 就会报这个异常.

## c2504 未定义基类

[error C2504: 未定义基类](https://blog.csdn.net/qncj666/article/details/8562338)

此错误是编译错误, 和 `inclued头文件` 有关

问题描述:
有三个头文件 `SDK.h`, `AA.h`, `BB.h`, 其中 `BB` 类继承自 `AA`.

头文件包含顺序如下,

1. `AA.h` 包含 `SDK.h`
2. `SDK.h` 包含 `BB.h`
3. `BB.h` 包含 `AA.h`

原因分析: 编译器首先编译 `AA.h`, 因其包含 `SDK.h`, 引入 `SDK.h` 继续编译.
因为 `SDK.h` 包含 `BB.h`, 载入 `BB.h` 内容准备编译.
`BB` 继承自 `AA`, `AA` 尚未编译成功, 此时VS错误列表中会出现
>error C2504: "CAA": 未定义基类.
此编译错误就是在编译 `AA.h` 头文件时出的错.

结论: 头文件在包含顺序上不要成闭合的环状, 结构顺序最好应该是树.

## 禁止编译器警告

```cpp
#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 26454)    # 禁止compiler警告
#endif

//  your code  here

#if defined(_MSC_VER)
#pragma warning(pop)  # 恢复compiler警告
#endif
```


## C26495

[警告 C26495](https://learn.microsoft.com/zh-cn/cpp/code-quality/c26495?view=msvc-170)

变量 "variable" 未初始化. 始终初始化成员变量 (type.6).
Variable 'variable' is uninitialized. Always initialize a member variable (type.6).

## Remarks

成员变量 没有被构造函数或 初始化器(initializer) 初始化.
请确保在构造结束时初始化了所有变量.
更多信息, 请参见 C++ 核心指南 [Type.6](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#SS-type) 和 [C.48](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#c48-prefer-in-class-initializers-to-member-initializers-in-constructors-for-constant-initializers).

该检查属于过程内检查(intra-procedural).
只要有函数调用到 非Const成员函数, 检查就会假定该成员函数初始化了所有成员.
这种启发式方法可能会导致错误遗漏, 因此采用这种方法是为了避免出现错误结果.
此外, 当一个成员通过 非Const引用 传递给函数时,
检查会假定该函数初始化了该成员.

代码分析名称: `MEMBER_UNINIT`

示例
下面的示例产生了 C26495 警告, 因为在创建 MyStruct 对象时没有初始化成员变量值.

```C++
struct MyStruct
{
    int value;
    MyStruct() {} // C26495, MyStruct::value is uninitialized
};
```

To resolve the issue, you can add in-class initialization to all of the member variables.

```C++
struct MyStruct
{
    int value{};  // empty brace initializer sets value to 0
    MyStruct() {} // no warning, MyStruct::value is set via default member initialization
};
```

## C6011

[Warning C6011](https://learn.microsoft.com/en-us/cpp/code-quality/c6011?view=msvc-170)

解引用 `NULL` 指针 `pointer-name`.
以下代码会产生此警告, 因为调用 `malloc`可能会在内存不足时返回 null:

```cpp
#include <malloc.h>

void f( )
{
  char *p = ( char * ) malloc( 10 );
  *p = '\0';

  // code ...
 free( p );
}
```

要纠正这一警告, 请检查指针是否为空值, 如以下代码所示:

```cpp
#include <malloc.h>
void f( )
{
  char *p = ( char * )malloc ( 10 );
  if ( p )
  {
    *p = '\0';
    // code ...

    free( p );
  }
}
```

函数可以通过在 预条件(Pre condition) 中使用 `Null属性` 来注释参数.
在 解引用参数 之前, 请在这些函数内部分配内存.
下面的代码生成了 C6011 警告,
因为尝试在函数内部 解引用空指针 (pc), 而没有首先分配内存:

```cpp
#include <sal.h>
using namespace vc_attributes;
void f([Pre(Null=Yes)] char* pc)
{
  *pc='\0'; // warning C6011 - pc is null
  // code ...
}
```

不小心使用 `malloc` 和 `free` 会导致内存泄漏和异常.
要想彻底减少此类泄漏和异常问题, 应避免自己分配原始内存.

相反, 应使用 C++ 标准库 (STL) 提供的机制.
这些机制包括 shared_ptr, unique_ptr 和 `std::vector`.
更多信息, 请参阅智能指针和 C++ 标准库.

## C2910 "function": 不能显式专用化

编译器检测到尝试将函数 显式专用化 两次. explicit specializition

下面的示例生成 C2910:

```cpp
// C2910.cpp
// compile with: /c
template <class T>
struct S;
/// 特化, 且已经 定义 f()
template <>
struct S<int> { void f() {} };

template <>
void S<int>::f() {}   // C2910 delete this specialization
```

如果你尝试将 非模板成员 显式专用化, 也可能出现 `C2910`.
也就是说, 只能将函数模板显式专用化.

下面的示例生成 C2910:

```cpp
// C2910b.cpp
// compile with: /c
template <class T> 
struct A {
   A(T* p);
};
template <class T>
inline A<T>::A(T* p) {}

/// 成员函数 f 声明
template <>
struct A<void> {
   A(void* p);
};

//---------- C2910; 尝试特化成员函数; A<void> 此时已经是普通类; gcc 可以编过,虽然
template <>
A<void>::A(void* p){}   
//-------- 应该使用下面的写法
// A<void>::A(void* p){}
```

此错误还可能来自于 Visual Studio .NET 2003 中执行的编译器一致性工作.
要使代码在 Visual Studio .NET 2003 和 Visual Studio .NET 版本的 Visual C++ 中有效, 
请删除 `template <>`.

```cpp
// C2910c.cpp
// compile with: /c
template <class T> class A {
   void f();
};

// 类模板特化, 成员函数 f 声明
template <> class A<int> {
   void f();
};

template <> void A<int>::f() {}   // C2910
//------ 使用下面的写法
// void A<int>::f(){}   // OK
```
