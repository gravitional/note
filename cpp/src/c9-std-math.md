# c++ math

[Use of min and max functions in C++](https://stackoverflow.com/questions/1632145/use-of-min-and-max-functions-in-c)

`fmin` 和 `fmax` 专门用于浮点数(因此用了 `f`).
如果将其用于 `int`, 可能会因转换, 函数调用开销等原因导致性能或精度下降,
具体取决于编译器/平台.

`std::min` 和 `std::max` 是模板函数,
定义在 header [<algorithm>](http://en.cppreference.com/w/cpp/header/algorithm)中,
可用于任何带有 `小于`(`<`)操作符的类型,
因此它们可以用于任何允许进行比较的数据类型.
如果不想使用 `<`, 也可以提供自己的比较函数.

这样做更安全, 因为当参数的类型不同时, 你必须明确地转换参数才能匹配.
例如, 编译器不会让您意外地将 64 位 int 转换为 64 位 float.
仅凭这一点, 模板就应该成为你的默认选择.
(归功于 Matthieu M 和 bk1e)

即使使用浮点数, 模板也**可能**在性能上胜出.
由于 源代码 是编译单元的一部分, 因此编译器总是可以选择 内联调用模板函数.
另一方面, 有时*不可能*内联对库函数的调用(共享库, 没有链接时优化等).
