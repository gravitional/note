# c++ 基本类型

[C++学习笔记: [4]类型type--基础类型](https://zhuanlan.zhihu.com/p/85940349)
[Type - cpp reference](https://en.cppreference.com/w/cpp/language/type)

首先是定义:

Objects, references, functions including function template specializations,
and expressions have a property called type,
which both restricts the operations that are permitted for those entities
and provides semantic meaning to the otherwise generic sequences of bits.

这里重点是几个, 类型具有:

特定的操作
提供对应的语义
(还有占内存的不同)
在C++中, 类型分为几类:

## fundamental types 基础类型

可以用 `std::is_fundamental<TYPE>::value` 来判定是否是基础类型,
如果是的话结果为1, 否则为 `0`.

[例1](../cpp-types/types-exa1.cpp)

输出:

```bash
int is:  true.
int* is:  false.
int& is:  false.
float is:  true.
float* is:  false.
float& is:  false.
A is:  false.
```

基础类型包括:

Void type: 以void 为关键字, void作为函数的返回值类型, 表示函数没有返回值,
void作为函数的参数,  则表示没有参数, void不能直接作为变量的类型,
但是void *可以作为指针的类型, 一般称为"万能指针";

std::nullptr_t: (since C++11), 表示空指针类型;

Boolean type: 以bool为关键字, 就两个值: true, false,
和 C 不一样的, 不需要额外再包含其他的头文件就可以使用;
注意, C++中并不鼓励你把bool值和其他类型的变量和常量混用,
但是, 如果需要, 会把其值转化为整数, 标准中只是规定了false转换为0,
对于true, 一般情况下, 是转换为1(至少在g++中是这样).

在内存中如何保存bool类型的变量是各个编译器厂家自定的,
一般会用char类型存储, 也就是 sizeof(bool)为1, 表示以一个字节来保存, 但是也不一定.

[例2](../cpp-types/types-exa2.cpp)

输出:

```bash
true is :1
false is :0
sizeof(bool) is :1
```

## 整型integer type

首先, 在实际的实现中, 一般有几种数据模型data model:

+ 一种是所谓的LP32或称2/4/4模型, 这用于win16 API,
也就是int占2字节, long占4字节, pointer占4字节,

+ 一种是ILP32或称4/4/4模型, 这用于win32 API和32bit UNIX/Linux的系统,
+ 对于64bit的系统, 则有LLP64或称4/4/8模型, 这用于win64 API,
+ 以及LP64或称4/8/8模型, 这用于64bit Unix/Linux系统.

具体的占多少字节, 还是需要用编译器自带的标准库头文件中的常量来定, 见后面的例子.

其次, 对于整数类型, C++支持以下的类型,
下例显示了不同类型变量占内存的字节数和表示范围,
这都是在64bit Linux的环境下, 由g++-9定义的:

看下面的例3, 在 g++-9 64bit 中编译运行, 显示
short, unsigned short, int, unsigned int,
long, unsigned long, long long(since C++11), u
nsigned long long(since C++11)
这些类型的变量在内存中占的字节数,  最小值和最大值.

[整数类型](../cpp-types/types-intger.cpp)

输出:

```bash
sizeof(short) = 2
sizeof(int) = 4
sizeof(long) = 8
sizeof(int*) = 8
sizeof(long long) = 8
lowest of short int = -32768
lowest of int = -2147483648
lowest of long int = -9223372036854775808
lowest of long long int = -9223372036854775808
lowest of unsigned short int = 0
lowest of unsigned int = 0
lowest of unsigned long int = 0
lowest of unsigned long long int = 0
max of short int = 32767
max of int = 2147483647
max of long int = 9223372036854775807
max of long long int = 9223372036854775807
max of unsigned short int = 65535
max of unsigned int = 4294967295
max of unsigned long int = 18446744073709551615
max of unsigned long long int = 18446744073709551615
```

由此可见, 不同的系统整数类型变量所占内存, 最小值和最大值都不一样, 这为代码移植带来很多麻烦,
因此, 在C++11中, 引入了C99标准里的所谓定宽整数类型: Fixed width integer types,
可以直接指定类型变量是多少字节(或位), 带来很多方便, 仅需要包含 `cstdint` 头文件即可.

除此之外, c++11还引入了所谓fast整数类型,
也就是说, 要满足至少有这么多位的最快的整数类型,
比如, `int_fast16_t`, 就是至少有16位的最快整数,
这在Linux g++ -9 64bit的环境下, 就等效于int64_t.

C++11还引入了 least整数类型, 也就是为了满足这么多位所需的最小整数类型,
比如, `int_least16_t`, 在Linux g++ -9 64bit的环境下, 等效于int16_t.

参看 [例4: 定宽整数类型](../cpp-types/types-fixed-intger.cpp)

输出为:

```bash
sizeof(int8_t) = 1
sizeof(int16_t) = 2
sizeof(int32_t) = 4
sizeof(int64_t) = 8
sizeof(uint8_t) = 1
sizeof(uint16_t) = 2
sizeof(uint32_t) = 4
sizeof(uint64_t) = 8
... 以下省略
```

注意, 在C++20之前, 带符号的整数的表示范围支持两种表示:

+ $-(2^{N-1}-1) \to 2^{N-1}-1$ 比如, `int8_t` 的范围是 `-127 -> 127`;
+ 以及: $-2^{N-1} \to 2^{N-1}-1$
也就是 `int8_t` 的范围是 `-128 -> 127`, 但是在 C++20 及以后, 只支持后一种表示范围.

整数在C++中, 可以以 `std::size_t` 来表示 `unsigned integer` type.

整数在C++中, 可以有几种表示方式:

+ 以十进制方式表示, 比如: 10, 128, 454222等,
在C++14 中可以用'单引号来分隔数字, 以更好的阅读, 这个在编译的时候会自动去除,
比如: 94'500和9'4500其实都是94500,
前者是英语国家的方便表示,
而中间则是我国以万为基准的方便表示, 本质上, C++编译器都是把它们当作一样的数;

+ 可以用八进制来表示, 以数字0开头,
注意这时候, 不能有0,1,2,3,4,5,6,7以外的数字, 比如0123是正确的, 表示十进制的83,
但是018就是错误的, 同样, 在C++14中, 也引入了'来分隔数字;

+ 可以用十六进制来表示, 以0x来开头,
这时候可以使用的字符包括0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f,
字母可以是大小写都行, 比如0x1f3a表示 十进制的7994,
而0x1FFF'536B则对应十进制的536826731;

+ 可以用二进制来表示 , 这是从C++14开始的, 以0b来开头,
这时候只能用0和1两个数字, 比如0b1100'1101'0001'1011 相当于十进制的52507.

例5: [整数类型的前缀](../cpp-types/types-intger-prefix.cpp)
输出:

```bash
94500 = 94500
94'500 = 94500
9'4500 = 94500
0123 = 83
0x1f3a = 7994
0x1FFF'536B = 536826731
0b1100'1101'0001'1011 = 52507
```

这些是用前缀来表示, 还可以在整数字面量(literal value)后面加后缀, 包括几种:

+ 无符号后缀: `u`或`U`, 比如1表示int, 1U则是unsigned int;
+ 长整型后缀: `l`或`L`, 比如1L表示long int;
+ 长长整型后缀: `ll`或`LL`, 比如1LL表示long long int, 而1ull则表示unsigned long long.

## 浮点数据类型Floating point types

一般情况下(虽然标准没有强制),
都是按照IEEE-754 32bit 定义单精度类型float,
按照IEEE-754 64bit定义双精度类型double,  C++11引入long double,
有些系统采用IEEE-754 128bit标准, 有的使用x87的80-bit浮点类型.

浮点类型有几个关注点:
浮点类型变量和常量占内存字节数和表示范围:

参见下例6:  [浮点类型占内存字节数和表示范围](../cpp-types/types-float.cpp),
编译环境: Linux x86 g++-9 64bit:
输出:

```bash
sizeof(float) = 4
sizeof(double) = 8
sizeof(long double) = 16
------------------
float min = 1.17549e-38
double min = 2.22507e-308
long double min = 3.3621e-4932
------------------
float max = 3.40282e+38
double max = 1.79769e+308
long double max = 1.18973e+4932
```

[2]浮点类型有几个特殊的值:

+ `infinity` 和 `-infinity`: 正/负无穷;
+ `0.0` 和 `-0.0`: 从数学上是等价的, 但是在实际使用时某些情况下会有不同效果,
比如: `1.0/0.0 == infinity`, `1.0/-0.0 == -infinity`;

+ `NAN` 和 `std::nan`: (not a number)非数值.

## 字符类型: Character types

字符类型包括几种:

+ signed char: 带符号的字符类型, sizeof(signed char)==1, 表示范围是-128-127;
+ unsigned char: 不带符号的字符类型, sizeof(unsigned char)==1, 表示范围: 0-255;
+ char: 字符类型, 标准没有限定是带符号还是不带, 一般, ARM/PowerPC平台的默认设置是unsigned char, 而x86/x86-64则默认是signed char.
+ wchar_t:宽字符类型, 在Linux/Unix系统下, 一般sizeof(wchar_t)==4,
这是为了装下足够的Unicode字符, 而在Windows系统下, sizeof(wchar_t)==2, 这是因为Windows采用的是utf-16 ;

+ char16-t: (since C++11), 为了装下utf-16的字符, 其占内存字节数,
对齐, 符号和 `std::uint_least16_t` 类型是一致的,  但类型不同;
+ char16-t: (since C++11), 为了装下utf-32的字符, 其占内存字节数,
对齐, 符号和std::uint_least32_t类型是一致的,  但类型不同;
+ char8-t: (since C++20), 为了装下utf-8的字符, 其占内存字节数,
对齐, 符号和unsigned char类型是一致的,  但类型不同;

例7: [显示字符类型的字节数和范围](../cpp-types/types-char.cpp)

Linux x86-64 g++-9 64bit:
输出:

```bash
CHAR_BIT = 8
CHAR_MIN = -128
CHAR_MAX = 127
------------------
sizeof(wchar_t) = 4
sizeof(char8_t) = 1
sizeof(char16_t) = 2
sizeof(char32_t) = 4
```

## 转义字符Escape sequences

常见的转义字符有:

| Escape sequence | Description                                                                    |
| --------------- | ------------------------------------------------------------------------------ |
| `\'`            | single quote                                                                   |
| `\"`            | double quote                                                                   |
| `\?`            | question mark                                                                  |
| `\\`            | backslash                                                                      |
| `\a`            | audible bell                                                                   |
| `\b`            | backspace                                                                      |
| `\f`            | new page                                                                       |
| `\n`            | new line                                                                       |
| `\r`            | carriage return                                                                |
| `\t`            | horizontal tab                                                                 |
| `\v`            | vertical tab                                                                   |
| `\0`            | terminating null character                                                     |
| `\nnn`          | `nnn` for octal value code (最多3个八进制数字, 或遇到第一个非八进制数字的字符) |
| `\xnn`          | `nn` for hexadecimal code (任意多个十六进制数字, 一直到第一个非十六进制数字)   |
| `\unnnn`        | `nnnn` for universal code point                                                |
| `\Unnnnnnnn`    | `nnn nnn nn` for universal code point                                          |
