# size_t 数据类型

[size_t 数据类型](https://blog.csdn.net/fuxiaoxiaoyue/article/details/82747332)
[size_t,ssize_t,int和long的区别](https://blog.csdn.net/qq_30866297/article/details/51465473)
[size_t 数据类型](https://blog.csdn.net/bzhxuexi/article/details/19899803)

## size_t

size_t 是一些C/C++标准在stddef.h中定义的, size_t 类型表示C中任何对象所能达到的最大长度, 它是无符号整数.

它是为了方便系统之间的移植而定义的, 不同的系统上, 定义size_t 可能不一样.
size_t在32位系统上定义为 unsigned int, 也就是32位无符号整型. 在64位系统上定义为 unsigned long ,
也就是64位无符号整形. size_t 的目的是提供一种可移植的方法来声明与系统中可寻址的内存区域一致的长度.

size_t 在数组下标和内存管理函数之类的地方广泛使用.
例如, size_t 用做sizeof 操作符的返回值类型, 同时也是很多函数的参数类型, 包括malloc 和strlen.

在声明诸如字符数或者数组索引这样的长度变量时用size_t 是好的做法.
它经常用于循环计数器, 数组索引, 有时候还用在指针算术运算上. size_t 的声明是实现相关的.
它出现在一个或多个标准头文件中, 比如stdio.h 和stblib.h, 典型的定义如下:

    #ifndef __SIZE_T
    #define __SIZE_T
    typedef unsigned int size_t;
    #endif

define 指令确保它只被定义一次. 实际的长度取决于实现.
通常在32 位系统上它的长度是32 位, 而在64 位系统上则是64 位. 一般来说, size_t 可能的最大值是SIZE_MAX.

打印size_t 类型的值时要小心. 这是无符号值, 如果选错格式说明符,
可能会得到不可靠的结果. 推荐的格式说明符是%zu.
不过, 某些情况下不能用这个说明符,  作为替代, 可以考虑%u 或%lu.
下面这个例子将一个变量定义为size_t, 然后用两种不同的格式说明符来打印:

    size_t sizet = -5;
    printf("%d\n",sizet);
    printf("%zu\n",sizet);

因为size_t 本来是用于表示正整数的, 如果用来表示负数就会出问题.
如果为其赋一个负数, 然后用%d 和%zu 格式说明符打印, 就得到如下结果:

    -5
    4294967291

%d 把size_t 当做有符号整数, 它打印出-5 因为变量中存放的就是-5. %zu 把size_t 当做无符号整数.
当-5 被解析为有符号数时, 高位置为1, 表示这个数是负数.
当它被解析为无符号数时, 高位的1 被当做2 的乘幂. 所以在用%zu 格式说明符时才会看到那个大整数.

    sizet = 5;
    printf("%d\n",sizet); // 显示5
    printf("%zu\n",sizet); // 显示5

因为size_t 是无符号的, 一定要给这种类型的变量赋正数 .

## ssize_t

ssize_t 和size_t类似,但必需是signed(表示 signed size_t类型),
用来表示可以被执行读写操作的数据块的大小.

## size_t 和 int 比较

    size_t在32位架构中定义为: typedef   unsigned int size_t;
    size_t在64位架构中被定义为: typedef  unsigned long size_t;
    size_t是无符号的, 并且是平台无关的, 表示0-MAXINT的范围;int为是有符号的;
    int在不同架构上都是4字节, size_t在32位和64位架构上分别是4字节和8字节, 在不同架构上进行编译时需要注意这个问题.
    ssize_t是有符号整型, 在32位机器上等同与int, 在64位机器上等同与 long int.

## C语言编程需要注意的64位和32机器的区别

     char    short    int    long    long long    指针
16 位 平台    1Byte 8位    2Byte 16位    2Byte 16位    4Byte 32位         2 Byte
32 位 平台

1Byte 8位
    2Byte 16位    4Byte 32位    4Byte 32位    8 Byte 64位    4 Byte
64 位 平台    1Byte    2Byte    4Byte    8Byte    8Byte    8Byte

## 编程注意事项

为了保证平台的通用性, 程序中尽量不要使用long数据库型.
可以使用固定大小的数据类型宏定义, 这些宏定义需要引用stdint.h头文件:

    typedef signed char int8_t
    typedef short int int16_t;
    typedef int int32_t;
    # if __WORDSIZE == 64
        typedef long int int64_t;
    # else
        __extension__
        typedef long long int int64_t;
    #endif

## intptr_t

使用int时也可以使用intptr_t来保证平台的通用性,
它在不同的平台上编译时长度不同, 但都是标准的平台字长, 比如64位机器它的长度就是8字节,
32位机器它的长度是4字节, 使用它可以安全地进行整数与指针的转换运算,
也就是说当需要将指针作为整数运算时, 将它转换成intptr_t进行运算才是安全的. intptr_t需要引用stddef.h头文件, 它的定义如下:

    #if __WORDSIZE == 64
        typedef long int intptr_t;
    #else
        typedef int intptr_t;
    #endif

编程中要尽量使用sizeof来计算数据类型的大小.
以上类型定义都有相应的无符号类型.

## 使用ssize_t和size_t

它们分别是 `unsigned` 和 `signed size of computer word size`.
它们也是表示计算机的字长, 在32位机器上是int型, 在64位机器上long型.
使用它们对于增加平台的通用性有很大好处, 从某种意义上来说它们等同于 `intptr_t` 和 `uintptr_t`.
使用它们也需要引用 `stddef.h` 头文件.

`socket` 的 `accept` 函数在有些操作系统上使用 `size_t` 是不正确的,
因为 `accept` 接收的 `int*` 类型, 而 `size_t` 的长度可能会超过 `int*` 的长度限制, 导致错误.
后来 BSD 使用 `sock_t` 来替代它.
