# c++ warning

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
