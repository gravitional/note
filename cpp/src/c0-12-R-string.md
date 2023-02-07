# R字符串原始字面量

[C++11 R字符串原始字面量](https://blog.csdn.net/sandrew0916/article/details/109525562)

`原始字符串字面量` 的定义为:

```cpp
R"xxx(raw string)xxx"
```

其中, 原始字符串必须用括号 `()` 括起来,
括号的前后可以加其他字符串 `xxx`, 所加的字符串会被忽略,
并且 `xxx` 必须在括号两边同时出现.

```cpp
#include <iostream>
#include <string>

int main()
{
    // 一个普通的字符串, '\n'被当作是转义字符, 表示一个换行符.
    std::string normal_str = "First line.\nSecond line.\nEnd of message.\n";
    //---输出:
    First line.
    Second line.
    End of message.

    // 一个raw string, '\'不会被转义处理. 因此, "\n"表示两个字符: 字符反斜杠 和 字母n.
    std::string raw_str = R"(First line.\nSecond line.\nEnd of message.\n)";
    //---输出:
    //First line.\nSecond line.\nEnd of message.\n

    std::cout << normal_str << std::endl;
    std::cout << raw_str << std::endl;
    std::cout << R"foo(Hello, world!)foo" << std::endl;
    //---输出:
    //Hello, world!

    // raw string可以跨越多行, 其中的空白和换行符都属于字符串的一部分.
    std::cout <<R"(
                   Hello,
                   world!
                   )" << std::endl;
    //---输出:

    //              Hello,
    //              world!

    return 0;
}
```
