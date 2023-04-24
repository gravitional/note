# python 字符串

## 如何将数字转换为字符串

比如要把数字 `144` 转换为字符串 `'144'`, 可使用内置类型构造器 `str()`.
如果要表示为 `十六进制` 或 `八进制` 数格式, 可使用内置函数 `hex()` 或 `oct()`.

更复杂的格式化方法请参阅 [格式字符串字面值][] 和 [格式字符串语法] 等章节,
比如 `"{:04d}".format(144)` 会生成 `'0144'` ,  `"{:.3f}".format(1.0/3.0)` 则会生成 `'0.333'`.

[格式字符串字面值](https://docs.python.org/zh-cn/3/reference/lexical_analysis.html#formatted-string-literals)
[格式字符串字面值]: https://docs.python.org/zh-cn/3/reference/lexical_analysis.html#f-strings
[格式字符串语法]: https://docs.python.org/zh-cn/3/library/string.html#formatstrings
