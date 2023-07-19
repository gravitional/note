# python io

[Input and Output](https://docs.python.org/3/tutorial/inputoutput.html)
[Format String Syntax](https://docs.python.org/3/library/string.html#format-string-syntax)
[Format Specification Mini-Language](https://docs.python.org/3/library/string.html#format-string-syntax)

`#` 选项会导致在转换时使用 `替代形式`.
不同类型的 `替代形式` 定义不同.
该选项仅适用于 `整数`, `浮点数` 和 `复数类型`.
对于整数, 当使用二进制, 八进制或十六进制输出时, 
该选项会在输出值上添加相应的前缀 `0b`, `0o`, `0x` 或 `0X`.

对于 `浮点数` 和 `复数`, 
替代形式 会使转换结果始终包含一个小数点字符, 即使后面没有数字.
通常情况下, 小数点字符只有在后面有数字时才会出现在这些转换的结果中.

此外, 对于 `g` 和 `G` 类型的转换, 结果中不会去掉尾数零.
