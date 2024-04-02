# DLL 导入和导出

[导入和导出](https://docs.microsoft.com/zh-cn/cpp/build/importing-and-exporting?view=msvc-170)

可以使用两种方法将公共符号导入应用程序或从 DLL 导出函数:

+ 生成 DLL 时使用模块定义 (.def) 文件
+ 在主应用程序的函数定义中使用关键字 `__declspec(dllimport)` 或 `__declspec(dllexport)`

导出导入的头文件模板如下:

```cpp
// MATH_API
#pragma once
#ifdef _LINUX
#   define MATH_API
#else
#   ifdef Math_EXPORTS
#       define MATH_API __declspec(dllexport)
#   else
#       define MATH_API __declspec(dllimport)
#   endif
#endif // _LINUX
```

### target_EXPORTS

其中 `Math_EXPORTS` 是 CMake 在编译时自动添加的 `编译器宏定义`,
参考 [DEFINE_SYMBOL](https://cmake.org/cmake/help/latest/prop_tgt/DEFINE_SYMBOL.html#prop_tgt:DEFINE_SYMBOL)

在编译`当前目标源代码`时定义一个符号.

`DEFINE_SYMBOL` 用于设置在编译 `共享库源代码` 时定义的 `预处理器符号` 名称.
如果未在此处设置, 则默认设置为 `target_EXPORTS`(如果目标不是有效的 C 标识符, 则会进行一些替换).
例如 `Math` lib 的名称就是 `Math_EXPORTS`.
这个 `Math` 与添加库语句 `add_library(Math SHARED ...)` 中的 Math 一致.

这对头文件很有用, 可以知道它们是被包含在 lib自身内, 还是被用户使用.
以便在 Windows 上正确设置 `dllexport`/`dllimport` 装饰.

在 POSIX 平台上, 可选择使用此功能来控制符号的可见性.

CMake 通过 GenerateExportHeader 模块为此类装饰提供支持.

>stackoverflow

或参考 [stackoverflow:CMake adds -Dlibname_EXPORTS compile definition](https://stackoverflow.com/questions/27429732/cmake-adds-dlibname-exports-compile-definition)

cmake 仅为共享库添加 `<libname>_EXPORTS` 宏. 当在 Windows DLL 中导出 API 时, 它非常有用.

```cmake
#if defined(_WINDOWS) && defined(testlib_EXPORTS)
#   define API_DLL extern "C" __declspec(dllexport)
#else
#   define API_DLL
#endif
```

在 c++ 文件中如下使用 `API_DLL`

```cpp
class API_DLL Foo {...}; // class
API_DLL void foo(); // function
```

将 `target` 的 `DEFINE_SYMBOL` 属性设置为空, 就可以禁用它.

```cmake
# 禁用 <libname>_EXPORTS
set_target_properties(sharedlib
  PROPERTIES
  DEFINE_SYMBOL "")
```

## 导出整个类

另外参考
[dllexport, dllimport](https://learn.microsoft.com/zh-cn/cpp/cpp/dllexport-dllimport?view=msvc-170)
[在 C++ 类中使用 dllimport 和 dllexport](https://learn.microsoft.com/zh-cn/cpp/cpp/using-dllimport-and-dllexport-in-cpp-classes?view=msvc-170)

>Microsoft 专用

可以使用 `dllimport` 或 `dllexport` 特性来声明 C++ 类.
这些形式将 整个类的内容导出或导入.
以这种方式导出的类称为 `exportable classes`(可导出类).

以下示例定义了 `可导出类`.
将导出其所有成员函数和静态数据:

```C++
#define DllExport   __declspec( dllexport )

class DllExport C {
   int i;
   virtual int func( void ) { return 1; }
};
```

**请注意, 禁止对可导出类的成员显式使用 `dllimport` 和 `dllexport` 特性.**

### dllexport 类

当你声明 dllexport 类时, 它的所有成员函数和静态数据成员都会导出.
您必须在同一程序中提供所有此类成员的定义.
否则, 将生成链接器错误.

此规则有一个例外情况, 即对于纯虚函数, 您无需为其提供显式定义.
但是, 由于基类的析构函数始终在调用抽象类的析构函数, 因此纯虚拟析构函数必须始终提供定义.
请注意, 这些规则对不可导出的类是相同的.

如果导出类类型的数据或返回类的函数, 请务必 导出类.

### dllimport 类

当你声明 `dllimport` 类时, 它的所有 `成员函数` 和 `静态数据成员` 都会导入.

不同于 在`非类类型` 上的 `dllimport` 和 `dllexport` 的行为,
`静态数据成员` 无法在定义 dllimport 类的同一程序中指定定义(重定义).

### 继承和可导出类

`可导出类` 的所有基类都必须是 `exportable`.
否则, 会生成编译器警告.
此外, 所有可访问成员如果是 `class`, 也必须是可导出的.

此规则只允许 `dllexport` 类从 `dllimport` 类继承,
`dllimport` 类从 `dllexport` 类继承(但不建议后者).

通常来说, 对 DLL 客户端可访问的所有内容
(根据 C++ 访问规则)都应该是 `exportable interface` 的一部分.

这包括在 `内联函数` 中引用的 `私有数据成员`.
