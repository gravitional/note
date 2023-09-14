# python 运算符内置名称

[operator --- 标准运算符替代函数](https://docs.python.org/zh-cn/3/library/operator.html)
[6.17. 运算符优先级](https://docs.python.org/zh-cn/3/reference/expressions.html)

## python 运算符优先级

`(expressions...)`,
`[expressions...], {key: value...}, {expressions...}`
绑定或加圆括号的表达式, 列表显示, 字典显示, 集合显示

`x[index], x[index:index]`,
`x(arguments...), x.attribute`; 抽取, 切片, 调用, 属性引用

`await x`; await 表达式

`**`; 乘方
幂运算符 `**` 绑定的紧密程度, 低于在其右侧的 算术 或 按位 一元运算符, 也就是说 `2**-1` 为 `0.5`

`+x, -x, ~x`; 正, 负, 按位非 NOT

`*, @, /, //, %`; 乘, 矩阵乘, 除, 整除, 取余
`%` 运算符也被用于字符串格式化;在此场合下会使用同样的优先级.
