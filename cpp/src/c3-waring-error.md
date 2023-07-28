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
只要有函数调用到非 Const 成员函数, 检查就会假定该成员函数初始化了所有成员.
这种启发式方法可能会导致错误遗漏, 因此采用这种方法是为了避免出现错误结果.
此外, 当一个成员通过非 Const 引用传递给一个函数时, 检查会假定该函数初始化了该成员.

代码分析名称: MEMBER_UNINIT

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

Dereferencing NULL pointer 'pointer-name'.
The following code generates this warning because a call to `malloc`
might return null if insufficient memory is available:

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

To correct this warning, examine the pointer for a null value as shown in the following code:

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

Functions may have parameters annotated by using the Null property in a Pre condition.
Allocate memory inside these functions before you dereference the parameter.
The following code generates warning C6011 because an attempt
is made to dereference a null pointer (pc) inside the function without first allocating memory:

```cpp
#include <sal.h>
using namespace vc_attributes;
void f([Pre(Null=Yes)] char* pc)
{
  *pc='\0'; // warning C6011 - pc is null
  // code ...
}
```

The careless use of malloc and free leads to memory leaks and exceptions.
To minimize these kinds of leaks and exception problems altogether, avoid allocating raw memory yourself.
Instead, use the mechanisms provided by the C++ Standard Library (STL).
These include shared_ptr, unique_ptr, and vector.
For more information, see Smart Pointers and C++ Standard Library.