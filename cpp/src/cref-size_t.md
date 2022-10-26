# size_t 数据类型

[size_t 数据类型](https://blog.csdn.net/fuxiaoxiaoyue/article/details/82747332)
[size_t,ssize_t,int和long的区别](https://blog.csdn.net/qq_30866297/article/details/51465473)
[size_t 数据类型](https://blog.csdn.net/bzhxuexi/article/details/19899803)
[C/C++ size_t详解](https://blog.csdn.net/qq_34018840/article/details/100884317)

## std::size_t

+ Header

`<cstddef>`
`<cstdio>`
`<cstdlib>`
`<cstring>`
`<ctime>`
`<cuchar>` (C++17 起)
`<cwchar>`

```cpp
typedef /* 由实现定义 */ size_t;
```

+ `std::size_t` 是 `sizeof` 运算符, 还有 `sizeof...` 运算符
及 `alignof` 运算符 (C++11 起)的`结果`, 所拥有的 `无符号整数` 类型.
+ `std::size_t` 的 `宽度`(bit width)不小于 16 位. (C++11 起)

### Notes

`std::size_t` 可以存放下理论上可能存在的对象的最大大小, 该对象可以是任何类型(包括数组).
大小无法以 `std::size_t` 表示的类型是非良构的.
在许多平台上(使用分段寻址的系统除外), `std::size_t` 可以存放任何 `非成员指针` 的值,
此时它与 `std::uintptr_t` 同义.

`std::size_t` 通常用于数组索引和循环计数.
使用其他类型来进行数组索引操作的程序可能会在某些情况下出错,
例如在 64 位系统中使用 `unsigned int` 进行索引时,
如果索引号超过 [UINT_MAX](https://zh.cppreference.com/w/cpp/types/climits)
或者依赖于 32 位取模运算的话, 程序就会出错.

在对诸如 `std::string`, `std::vector` 等 C++ 容器进行索引操作时,
正确的类型是该容器的成员 `typedef size_type`,
而该类型通常被定义为与 `std::size_t` 相同.

`std::size_t` 的整数字面量后缀是 `z` 或 `z` 与 `u` 或 `U` 的任何组合
(即 zu, zU, Zu, ZU, uz, uZ, Uz 或 UZ).(C++23 起)

### 示例

```cpp
#include <cstddef>
#include <iostream>
#include <array>

int main()
{
    std::array<std::size_t, 10> a;

    // 使用 C++23 size_t 字面量的例子, for循环改成
    //for (auto i = 0uz; i != a.size(); ++i)
    // 使用 C++11 size_t 字面量的例子
    for (decltype(a)::size_type i = 0; i != a.size(); ++i)
        std::cout << (a[i] = i) << ' ';
    std::cout << '\n';

    // 自减循环的例子
    for (std::size_t i = a.size(); i--;)
        std::cout << a[i] << ' ';

    // 注意以下自减循环的简单实现：
    //  for (std::size_t i = a.size() - 1; i >= 0; --i) ...
    // 是无限循环，因为无符号数不会是负数
}
```

## `size_t`

`size_t` 是一些 C/C++ 标准在 `stddef.h` 中定义的,
`size_t` 类型表示 C 中任何 `对象` 所能达到的 `最大长度`, 它是 `无符号整数`.

它是为了方便系统之间的移植而定义的, 不同的系统上, 定义 `size_t` 可能不一样.
`size_t` 在 `32位` 系统上定义为 `unsigned int`, 也就是 `32位无符号整型`.
在 `64位` 系统上定义为 `unsigned long`, 也就是 `64位无符号整型`.
`size_t` 的目的是提供一种 `可移植` 的方法, 来声明与系统中 `可寻址的内存区域` 一致的长度.

`size_t` 在 `数组下标` 和 `内存管理` 函数之类的地方广泛使用.
例如, `size_t` 用做 `sizeof` 操作符的 `返回值类型`,
同时也是很多函数的参数类型, 包括 `malloc` 和 `strlen`.

在声明诸如 `字符数` 或者 `数组索引` 这样的长度变量时,  使用 `size_t` 是好的做法.
它经常用于 `循环计数器`, `数组索引`, 有时候还用在 `指针算术运算` 上.
`size_t` 的声明是实现相关的. 它出现在一个或多个标准头文件中,
比如 `stdio.h` 和 `stblib.h`, 典型的定义如下:

```cpp
#ifndef __SIZE_T
#define __SIZE_T
typedef unsigned int size_t;
#endif
```

`define` 指令确保它只被定义一次. 实际的长度取决于实现.
通常在32 位系统上它的长度是 32 位, 而在64 位系统上则是64 位.
一般来说, `size_t` 可能的最大值是 `SIZE_MAX`.

打印 `size_t` 类型的值时要小心. 这是无符号值, 如果选错格式说明符,
可能会得到不可靠的结果. 推荐的格式说明符是 `%zu`.
不过, 某些情况下不能用这个说明符,  作为替代, 可以考虑%u 或%lu.
下面这个例子将一个变量定义为size_t, 然后用两种不同的格式说明符来打印:

```cpp
size_t sizet = -5;
printf("%d\n",sizet);
printf("%zu\n",sizet);
```

因为 `size_t` 本来是用于表示正整数的, 如果用来表示负数就会出问题.
如果为其赋一个负数, 然后用 `%d` 和 `%zu` 格式说明符打印, 就得到如下结果:

```cpp
-5
4294967291
```

`%d` 把 `size_t` 当做有符号整数, 它打印出 `-5`, 因为变量中存放的就是 `-5`.
`%zu` 把 `size_t` 当做无符号整数, 当 `-5` 被解析为有符号数时, 高位置为 `1`, 表示这个数是负数.
当它被解析为无符号数时, 高位的 `1` 被当做 `2` 的乘幂.
所以在用 `%zu` 格式说明符时才会看到那个大整数.

```cpp
sizet = 5;
printf("%d\n",sizet); // 显示5
printf("%zu\n",sizet); // 显示5
```

因为 `size_t` 是无符号的, 一定要给这种类型的变量赋正数 .

## `ssize_t`

`ssize_t` 和 `size_t类似`,但必需是 `signed`(表示 `signed size_t` 类型),
用来表示可以被执行读写操作的 `数据块` 的大小.

## `size_t` 和 int 比较

+ `size_t` 在32位架构中定义为: `typedef unsigned int size_t`;
+ `size_t` 在64位架构中被定义为: `typedef unsigned long size_t`;
+ `size_t` 是无符号的, 并且是平台无关的, 表示 `0-MAXINT` 的范围; `int` 是有符号的;
+ `int` 在不同架构上都是 `4字节`,
`size_t` 在 `32位` 和 `64位` 架构上分别是4字节和8字节, 在不同架构上进行编译时需要注意这个问题.

+ `ssize_t` 是有符号整型, 在 `32位`机器上等同于 `int`, 在64位机器上等同于 `long int`.

## C语言编程需要注意的64位和32机器的区别

        char;   short    int    long    long long    指针
16位平台; `1Byte 8位`; `2Byte 16位`; `2Byte 16位`; `4Byte 32位`; `  `; `2 Byte`
`32位平台`; `1Byte 8位`; `2Byte 16位`; `4Byte 32位`;  `4Byte 32位`;  `8 Byte 64位`; `4 Byte`
`64位平台`; `1Byte`; `2Byte`; `4Byte`;  `8Byte`;  `8Byte`; `8Byte`

## 编程注意事项

为了保证平台的通用性, 程序中尽量不要使用 `long` 数据库型.
可以使用固定大小的 数据类型  宏定义, 这些宏定义需要引用 `stdint.h` 头文件:

```cpp
typedef signed char int8_t
typedef short int int16_t;
typedef int int32_t;
# if __WORDSIZE == 64
    typedef long int int64_t;
# else
    __extension__
    typedef long long int int64_t;
#endif
```

## `intptr_t`

使用 `int` 时也可以使用 `intptr_t` 来保证平台的通用性,
它在不同的平台上编译时长度不同, 但都是标准的平台字长, 比如 `64位` 机器它的长度就是 `8字节`,
`32位` 机器它的长度是 `4字节`, 使用它可以安全地进行 `整数` 与 `指针` 的转换运算,
也就是说当需要将指针作为整数运算时, 将它转换成 `intptr_t` 进行运算才是安全的.
`intptr_t` 需要引用 `stddef.h` 头文件, 它的定义如下:

```cpp
#if __WORDSIZE == 64
    typedef long int intptr_t;
#else
    typedef int intptr_t;
#endif
```

编程中要尽量使用 `sizeof` 来计算数据类型的大小.
以上类型定义都有相应的 `无符号类型`.

## 使用`ssize_t`和`size_t`

它们分别是 `unsigned` 和 `signed size of computer word size`.
它们也是表示计算机的字长, 在32位机器上是int型, 在64位机器上long型.
使用它们对于增加平台的通用性有很大好处, 从某种意义上来说它们等同于 `intptr_t` 和 `uintptr_t`.
使用它们也需要引用 `stddef.h` 头文件.

`socket` 的 `accept` 函数在有些操作系统上使用 `size_t` 是不正确的,
因为 `accept` 接收的 `int*` 类型, 而 `size_t` 的长度可能会超过 `int*` 的长度限制, 导致错误.
后来 BSD 使用 `sock_t` 来替代它.
