# python 字符串

## 如何将数字转换为字符串

[格式字符串字面值](https://docs.python.org/zh-cn/3/reference/lexical_analysis.html#formatted-string-literals)

比如要把数字 `144` 转换为字符串 `'144'`, 可使用内置类型构造器 `str()`.
如果要表示为 `十六进制` 或 `八进制` 数格式, 可使用内置函数 `hex()` 或 `oct()`.

更复杂的格式化方法请参阅 [格式字符串][] 和 [格式字符串语法] 等章节,
比如 `"{:04d}".format(144)` 会生成 `'0144'` ,  `"{:.3f}".format(1.0/3.0)` 则会生成 `'0.333'`.

[格式字符串]: https://docs.python.org/zh-cn/3/reference/lexical_analysis.html#f-strings
[格式字符串语法]: https://docs.python.org/zh-cn/3/library/string.html#formatstrings

格式化 浮点数 实例

```py
x = 153.5102
y = -153.5102

# 默认空格填充 fill align sign
# [[fill]align][sign]["z"]["#"]["0"][width][grouping_option]["." precision][type]

# 使用字母 X 填充, 对齐方式可以为 = < >;
# 井号#开启第二格式. G 表示 pretty 科学计数法

print(f'{x:<#20.16g}')

print(f'ret 1: {x:Q=#.11G}')
print(f'ret 2: {x:Q<+#.11G}')

print(f'ret 3: {x:Q>-#.11G}')
print(f'ret 3: {y:Q>-#.11G}')
print(f'ret 4: {y:Q> #.11G}')

print(f'ret 5: {y:0<#.11G}')
print(f'ret 6: {y:0>#.11G}')
print(f'ret 7: {y:0=#.11G}')
```

cpp `fmt` 库使用相同的语法:

```cpp
fmt::print(fmt::fg(fmt::color::aqua), "test ret1: {:#15.12g}", c);
```
