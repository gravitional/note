# c++ 预编译指令, 预处理器

## 预定义宏

[预定义宏](https://learn.microsoft.com/zh-cn/cpp/preprocessor/predefined-macros?view=msvc-170)

### 标准预定义宏

编译器支持 ISO C99, C11, C17 和 ISO C++17 标准指定的以下预定义宏:

`__cplusplus`: 当翻译单元编译为 `C++` 时, 定义为整数文本值.  其他情况下则不定义.

`__DATE__`: 当前源文件的编译日期.  日期是 `Mmm dd yyyy` 格式的恒定长度字符串文本 .
月份名 Mmm 与 C 运行时库 (CRT) asctime 函数生成的缩写月份名相同 .
如果值小于 10, 则日期 dd 的第一个字符为空格 .  任何情况下都会定义此宏.

`__FILE__`: 当前源文件的名称.  `__FILE__` 展开为字符型字符串文本.
要确保显示文件的完整路径, 请使用 /FC(诊断中源代码文件的完整路径).
任何情况下都会定义此宏.

`__LINE__`: 定义为当前源文件中的整数行号.
可使用 `#line` 指令来更改 `__LINE__` 宏的值.  `__LINE__` 值的整型类型因上下文而异.
任何情况下都会定义此宏.

`__STDC__`: 仅在编译为 `C`, 并且指定了 `/Za` 编译器选项时, 定义为 1.
从 Visual Studio 2022 17.2 版本开始,
当编译为 C 并指定 `/std:c11` 或 `/std:c17` 编译器选项时, 它也定义为 1.  其他情况下则不定义.

`__STDC_HOSTED__`: 如果实现是托管实现(hosted implementation)并且支持整个必需的标准库, 则定义为 `1`.
其他情况下则定义为 0.

`__STDC_NO_ATOMICS__` 如果实现不支持可选的标准原子, 则定义为 1.
当编译为 C 且指定 /std C11 或 C17 选项之一时, MSVC 实现会将其定义为 1.

`__STDC_NO_COMPLEX__` 如果实现不支持可选的标准复数, 则定义为 1.
当编译为 C 且指定 /std C11 或 C17 选项之一时, MSVC 实现会将其定义为 1.

`__STDC_NO_THREADS__` 如果实现不支持可选的标准线程, 则定义为 1.
当编译为 C 且指定 /std C11 或 C17 选项之一时, MSVC 实现会将其定义为 1.

`__STDC_NO_VLA__` 如果实现不支持可选的可变长度数组, 则定义为 1.
当编译为 C 且指定 /std C11 或 C17 选项之一时, MSVC 实现会将其定义为 1.

`__STDC_VERSION__` 当编译为 C 且指定 /std C11 或 C17 选项之一时定义.
对于 /std:c11, 它扩展到 201112L;对于 /std:c17, 则扩展到 201710L.

`__STDCPP_DEFAULT_NEW_ALIGNMENT__` 当指定 /std:c17 或更高版本时,
此宏会扩展为 `size_t` 字面量, 该字面量的对齐值由对非对齐感知的 operator new 的调用所保证.
较大的对齐值传递到对齐感知重载, 例如 operator new(std::size_t, std::align_val_t).
有关详细信息, 请参阅 /Zc:alignedNew(C++17 过度对齐的分配).

`__STDCPP_THREADS__`: 当且仅当程序可以有多个执行线程并编译为 C++ 时, 定义为 1.
其他情况下则不定义.

`__TIME__`: 预处理翻译单元的翻译时间.  时间是 hh:mm:ss 格式的字符型字符串文本,
与 CRT asctime 函数返回的时间相同 .  任何情况下都会定义此宏.

### Microsoft 专用预定义宏

+ `_DEBUG` Defined as 1 when the /LDd, /MDd, or /MTd compiler option is set. Otherwise, undefined.

+ `_DLL` Defined as 1 when the /MD or /MDd (Multithreaded DLL) compiler option is set. Otherwise, undefined.

+ `__FUNCDNAME__` 定义为字符串字面, 包含了 外套函数 的装饰名称(decorated name).
这个宏只能在函数中定义.
如果使用 `/EP` 或 `/P` 编译器选项, `__FUNCDNAME__`宏不会被展开.

这个例子使用`__FUNCDNAME__`, `__FUNCSIG__` 和 `__FUNCTION__` 宏来显示函数信息.

```C++
// Demonstrates functionality of __FUNCTION__, __FUNCDNAME__, and __FUNCSIG__ macros
void exampleFunction()
{
    printf("Function name: %s\n", __FUNCTION__);
    printf("Decorated function name: %s\n", __FUNCDNAME__);
    printf("Function signature: %s\n", __FUNCSIG__);

    // Sample Output
    // -------------------------------------------------
    // Function name: exampleFunction
    // Decorated function name: ?exampleFunction@@YAXXZ
    // Function signature: void __cdecl exampleFunction(void)
}
```

+ `FUNCSIG__` 定义为一个字符串字面(string literal),
包含 外套函数(enclosing function) 的签名.
这个宏只能在函数中定义.
如果使用 `/EP` 或 `/P` 编译器选项, `__FUNCSIG__` 宏不会被展开.
当为 `64位` 目标编译时, 调用约定默认为 `__cdecl`.
关于使用的例子, 请看 `__FUNCDNAME__` 宏.

+ `__FUNCTION__` 定义为一个字符串字面, 包含了 外套函数 的 未装饰名称.
这个宏只能在函数中定义. 如果使用 `/EP` 或 `/P` 编译器选项, `__FUNCTION__`宏不会被展开.
关于使用的例子, 请看 `__FUNCDNAME__` 宏.

+ `_INTEGRAL_MAX_BITS` 定义为整数字面值 `64`, 即 非向量 整数类型 的最大尺寸(in bits). 这个宏总是被定义.

```C++
// integral_max_bits.cpp
#include <stdio.h>
int main() {
    printf("%d\n", _INTEGRAL_MAX_BITS);
}
```

+ `_MSVC_LANG` 定义为一个整数字, 用于指定编译器所针对的C++语言标准.
它只在编译为C++的代码中被设置. 默认情况下, 或当指定 `/std:c++14` 编译器选项时, 该宏是整数字值 `201402L`.
如果指定了 `/std:c++17` 编译器选项, 该宏将被设置为 `201703L`.
如果指定了 `/std:c++20` 编译器选项, 该宏被设置为 `202002L`.
如果指定了 `/std:c++latest` 选项, 它将被设置为一个更高的, 未指定的值.
否则, 该宏是未定义的.
`_MSVC_LANG` 宏和/std(指定语言标准版本)编译器选项从Visual Studio 2015 Update 3开始可用.

+ `__MSVC_RUNTIME_CHECKS` Defined as 1 when one of the /RTC compiler options is set. Otherwise, undefined.

`_WIN32` Defined as 1 when the compilation target is 32-bit ARM, 64-bit ARM, x86, or x64. Otherwise, undefined.

`_WIN64` Defined as 1 when the compilation target is 64-bit ARM or x64. Otherwise, undefined.

`_WINRT_DLL` 当编译为C++并且 `/ZW` (Windows Runtime Compilation)和 (`/LD` 或 `/LDd`) 编译器选项被设置时, 定义为1. 否则, 未定义.

## pragma once

[gcc 7 Pragmas](https://gcc.gnu.org/onlinedocs/cpp/Pragmas.html)
[MSVC: once pragma](https://learn.microsoft.com/en-us/cpp/preprocessor/once?view=msvc-170)

If `#pragma once` is seen when scanning a header file,
that file will never be read again, no matter what.
It is a less-portable alternative to using '#ifndef' to
guard the contents of header files against multiple inclusions.

下面两种都可以, pragma once 简单一些.

```cpp
// header.h
#pragma once
// Code placed here is included only once per translation unit
```

```cpp
// header.h
// Demonstration of the #include guard idiom.
// Note that the defined symbol can be arbitrary.
#ifndef HEADER_H_     // equivalently, #if !defined HEADER_H_
#define HEADER_H_
// Code placed here is included only once per translation unit
#endif // HEADER_H_
```
