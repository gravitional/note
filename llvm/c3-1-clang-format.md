# clang format

[使用 clang-format 进行 C++ 代码风格管理](https://zhuanlan.zhihu.com/p/514541589)
[Clang-Format Style Options](https://clang.llvm.org/docs/ClangFormatStyleOptions.html)

## AlignAfterOpenBracket (BracketAlignmentStyle) clang-format 3.8 ¶.

如果为 `true`, 则水平对齐开括号后的参数.

这适用于 圆括号(parentheses), 角括号(angle brackets) 和方括号(square brackets).

可能的值:

+ `BAS_Align` (在配置中: `Align`) 将参数与 开括弧对齐, 例如

```cpp
someLongFunction(argument1,
                 argument2);
```

+ `BAS_DontAlign` (配置中: `DontAlign`)不对齐, 而是使用 `ContinuationIndentWidth`, 例如

```cpp
someLongFunction(argument1,
    argument2);
```

+ `BAS_AlwaysBreak` (在配置中: `AlwaysBreak`) 如果参数不能在一行中显示, 则总是在开放括号后断开, 例如

```cpp
someLongFunction(
    argument1, argument2);
```

+ `BAS_BlockIndent` (配置中: `BlockIndent`)如果参数不能在一行中显示,
则总是在打开的括号后断开. 闭合括号将另起一行. 例如

```cpp
someLongFunction(
    argument1, argument2
)
```

>注意
> 目前仅适用于带括号的初始化列表(当 Cpp11BracedListStyle 为 true 时)和括号.
