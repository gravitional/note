# c++ char char8_t char16_t 等类型

[C++:char, wchar_t, char8_t, char16_t char32_t](https://blog.csdn.net/it_cplusplus/article/details/118191097)

## 简介

类型 `char`, `wchar_t`, `char8_t`, `char16_t` 和 `char32_t` 是内置类型,
这些类型表示 `字母数字` 字符, 非字母数字的标志符号和非打印字符.

## 语法

```cpp
char     ch1{ 'a' };  // or { u8'a' }
wchar_t  ch2{ L'a' };
char16_t ch3{ u'a' };
char32_t ch4{ U'a' };
```

## 备注

char 类型为 C 和 c + + 中的原始字符类型.
char 类型可用于存储 ASCII 字符集中的字符或任意 ISO-8859 字符集,
以及单个字节的多字节字符(例如, shift-jis 或 Unicode 字符集的 utf-8 编码).  在 Microsoft 编译器中,
char 是一个8位类型.  它是和 signed char 的不同类型 unsigned char.
默认情况下, 类型的变量 char 将提升为 int(signed char),  除非  使用编译器选项 /J.
在 /J 下  , 它们被视为类型 `unsigned char` 并提升为 `int` 无需签名扩展名.

类型 unsigned char 通常用于表示一个字节, 该 字节 不是 c + + 中的内置类型.

wchar_t 类型是实现定义的宽字符类型.  在 Microsoft 编译器中, 它表示用于将 Unicode 编码为 UTF-16LE 的16位宽字符, Windows 操作系统上的本机字符类型.  通用 C 运行时 (UCRT) 库函数使用的宽字符版本 wchar_t 及其指针和数组类型作为参数和返回值, 这与本机 WINDOWS API 的宽字符版本相同.

char8_t,  char16_t 和 char32_t 类型分别表示8位, 16位和32位宽字符.
(char8_t 是 c + + 20 中的新项, 需要 /std:c++latest 编译器选项. 以 utf-8 编码的 ) Unicode 可以存储在 char8_t 类型中.
char8_t 和类型的字符串 char 称为 窄 字符串, 即使用于编码 Unicode 或多字节字符.
以 UTF-16 编码的 unicode 可以存储在 char16_t 类型中,
而编码为 utf-32 的 unicode 可以存储在 char32_t 类型中.
这些类型的字符串 wchar_t 都被称为 宽 字符串, 但术语通常是专门引用类型为的字符串 wchar_t .

在 c + + 标准库中,  basic_string 类型专用于窄字符串和宽字符串.
当字符为类型时, 请使用, 当字符为类型时, 当字符为类型时, 则使用;
当字符类型为时使用 std::string char std::u8string char8_t std::u16string char16_t std::u32string char32_t std::wstring wchar_t .
表示文本的其他类型, 包括 std::stringstream 和 std::cout 的专用化的窄字符串和宽字符串.

## another

[C++ 中char wchar_t char16_t char32_t的简单区别](https://blog.csdn.net/dongxianfei/article/details/107489599)

首先这几种字符类型的首要区别就是其占用 `存储空间` 的大小不同.
`char` 由一个字节表示, `wchar_t` 宽体字符, 由两个字符表示.
char16_t, char32_t C++ 11 新增的字符类型, `char16_t` 占两个字节, `char32_t` 占四个字节.

初始化的方法不同:

```cpp
#include <iostream>
#include <string>

using namespace std;

int main()
{
    char nameChar[] = "This is a char array";
    wchar_t nameWchar[] = L"This is a wchar array";
    char16_t nameChar16[] = u"This is a char16 array";
    char32_t nameChar32[] = U"This is a char32 array";
    cin.get();
}

char     ch1{ 'a' };  // or { u8'a' }
wchar_t  ch2{ L'a' };
char16_t ch3{ u'a' };
char32_t ch4{ U'a' };
```

Char类型是 C 和C++中的原始字符类型. 类型无符号字符通常用于表示字节;
Wchar_t类型是实现定义的宽字符类型;
Char16_t和char32_t类型分别表示16位和32位宽字符;
在C++标准库中, 每个 basic_string 类型都专用于窄字符串和宽字符串.

当字符的类型为char时, 使用 std::string;

+ `std::u16string` 字符类型 `char16_t`;
+ `std::u32string` 字符类型 `char32_t`;
+ `std::wstring` 字符类型 `wchar_t`;

表示文本的其他类型, 包括 std::stringstream 和 std::cout 专用于窄字符串和宽字符串.

```cpp
typedef basic_string<char> string;
typedef basic_string<wchar_t> wstring;
typedef basic_string<char16_t> u16string; //C++11
typedef basic_string<char32_t> u32string; //C++11
```
