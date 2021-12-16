# mma 的数据类型 值类型

绑定到 `变量` 上的 `值`, 具有许多分类, 见: ref/Set

可以使用 `Information["*Values"]` 查看各种值的说明;

OwnValues
DownValues
SubValues;
UpValues
DefaultValues
NValues
FormatValues
Definition

`FullName`;  完整的符号名称, 包括 `上下文`(context)
`Usage`; 符号的使用信息
`Options`; 默认选项的列表
`Attributes`; 属性的列表
`Documentation`; 本地和网络文档的链接
`Definitions`; 函数定义
`OwnValues`; ownvalue 的定义, 相当于其他程序语言中的 `值绑定`
`DownValues`; `下值` 的定义
`UpValues`; `上值` 的定义, 定义外层映射
`SubValues`; `子值`定义, Curry 化函数的定义
`DefaultValues`; `默认值` 的定义
`NValues`; `数值值` 的定义

`FormatValues[f]` 给出为符号 `f` 定义的, 与打印格式(`Format[f[x,...],...]` 的值 等)有关的`转换规则列表`.

## `上值`:`UpValue`

`^:=` 定义上值(`upvalue`), 使用它, 等价于用 `TagSetDelayed(/::=)` 设置单个标签:

```mathematica
g /: f[g[x_]] := f1[x]
f[h[x_]] ^:= f2[x]
{UpValues[g], UpValues[h]}

Out[3]= {{HoldPattern[f[g[x_]]] :> f1[x]}, {HoldPattern[f[h[x_]]] :> f2[x]}}
```

`TagSetDelayed` 中的 `标签` 仅定义单个符号的 `上值`(upvalue),
`^:=` 定义左边所有符号的上值:

```mathematica
g /: f1[g[x_], h[y_]] := gh[x y]
f2[g[x_], h[y_]] ^:= gh[x y]
{UpValues[g], UpValues[h]}

Out[3]= {{HoldPattern[f1[g[x_], h[y_]]] :> gh[x y],
    HoldPattern[f2[g[x_], h[y_]]] :> gh[x y]},
    {HoldPattern[f2[g[x_], h[y_]]] :>  gh[x y]}}
```

进行定义时, 计算立即赋值的右边:

```mathematica
rand[int] ^= Random[Integer];
{rand[int], rand[int]}

Out[2]= {0, 0}
```

每次使用定义时, 每次计算延迟定义的右边:

```mathematica
rand[real] ^:= Random[Real]
{rand[real], rand[real]}

Out[4]= {0.409393, 0.730688}
```

## SubValue

`SubValues[f]`; 给出符号`f` 中定义的所有和 `subvalues` 相关的变换规则,
其中 `subvalues` 是形如 `f[x, ...][...]` 的值.
