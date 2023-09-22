# c++ utf-8

[C++如何方便的处理中文?](https://www.zhihu.com/question/522195344)
[C++17 怎样优雅的进行gbk, utf-8字符编码互转?](https://www.zhihu.com/question/400605049)
[UTF-8 wiki](https://zh.wikipedia.org/wiki/UTF-8)
[/utf-8 将源字符集和执行字符集设置为 UTF-8](https://learn.microsoft.com/zh-cn/cpp/build/reference/utf-8-set-source-and-executable-character-sets-to-utf-8)

使用 `UTF-8` 存储字符串.
使用 `find` 时, 通过 `"xxx"` 查找, 而不是 `'xxx'`, 中文字符无法放入 `char` 类型中.
计算字符数时, 对有效的 UTF-8 string, 直接计算其中非 `0b10xxxxxx` 的 `byte` 即可.

```cpp
    // utf8 编码的字符首字节样式为 0xxx, 110xxx, 1110xx, 11110xxx ....
    //E68891, E4BBAC
    std::string test = "我们的世界由原子组成, 原子由强子组成";
    int nchar = 0;
    for (const auto& ch : test)
    {  //每次右移6个bit, 判断 byte 开头两bit 是否为 0b11;
        const auto ret = (((ch >> 6) & 0b11) != 0b10);
        nchar += ret;
        fmt::print(fg(color::aqua), "\ntest utf-8 decode {:d}", ret);
    }

    fmt::print(fg(color::aqua), "\nutf-8 char number: {:d}", nchar);
```

这里有详细说明:
[UTF8-的编码方式](https://link.zhihu.com/?target=https%3A//zh.wikipedia.org/wiki/UTF-8),
想简单处理可以临时转成 UTF-32, 能达到你想要的效果.
