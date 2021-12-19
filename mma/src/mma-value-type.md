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

## Null

`Null` 是用来表示 `没有表达式或结果` 的符号. 它在普通输出中不显示.
当 `Null` 是 `输出表达式`的全部时, 不打印输出.

### 细节

+ `e1; e2; ...; ek;` 返回 `Null`, 并且不打印输出.
+ 像 `f[e1, , e2]` 这样的表达式会被解释为, 在每对相邻逗号之间有 `Null`.
+ 在 `InputField` 或 `Grid` 这样的结构中, `Null` 表示没有 `内容` 或 `元素`.

### 基本例子

+ 当 `命令序列` 以 `分号` 结束时, 不显示输出:

    ```mathematica
    null = (a = 1 + 2; b = a + 3;)
    ```

    输出的完全形式是 `Null`:

    ```mathematica
    FullForm[null]

    Out[2]= Null
    ```

+ 如果你在程序中忘记了 `分号`, 你可能会看到输出中出现 `Null`:

    ```mathematica
    f[x_] := Module[{y = x},
    While[y > 2, y = Sqrt[y]]
    y ]

    f[100.]
    Out[2]= 1.77828 Null
    ```

    这是因为`While[...]` 后面的 `空白` 被解释为 `乘法`, 用 `分号` 重新定义:

    ```mathematica
    f[x_] := Module[{y = x},
      While[y > 2, y = Sqrt[y]];
      y ]

    f[100.]
    Out[4]= 1.77828
    ```

## Nothing

```mathematica
Nothing 如果处于列表中, 会被自动移除.
Nothing[...]; gives Nothing.
```

### 细节

在 `标准计算过程` 中, `Nothing` 被删除.
在被 `Hold` 或 `Inactive` 的表达式中, `Nothing` 不会被删除.

### 基本例子

列表中出现的任何 `Nothing`  都会被自动删除:

```mathematica
{a, b, Nothing, c, d, Nothing}

Out[1]= {a, b, c, d}
```

将列表中的元素替换成 `Nothing` 可以删除它:

```mathematica
{a, b, c, d} /. c -> Nothing
Out[1]= {a, b, d}
```

### 范围

在列表中的任何地方出现的 `Nothing` 都会被删除:

```mathematica
I{{a, b, Nothing}, {c, d}, Nothing}

Out[1]= {{a, b}, {c, d}}
```

如果头部不是 `List`, `Nothing` 不会被删除:

```mathematica
f[a, Nothing, g[Nothing], c]

Out[1]= f[a, Nothing, g[Nothing], c]
```

### 推广

当 `Nothing` 作为头部出现, 它将任何表达式约化成 `Nothing`:

```mathematica
Nothing[a]
Out[1]= Nothing

Nothing[a, b]
Out[2]= Nothing
```

### 应用

+ 从 `plot` 中删除 `线条`, 只留下孤立的 `点`:

    ```mathematica
    plot=DiscretePlot[{Sin[t], Cos[t], Sin[Pi + t]}, {t, 0, 2 Pi, Pi/12}]

    plot /. Line -> Nothing
    ```

+ `Nothing` 在 `标准计算过程` 中被去掉:

    ```mathematica
    Hold[{a, Nothing[b], Nothing}]
    Out[1]= Hold[{a, Nothing[b], Nothing}]

    % // ReleaseHold
    Out[2]= {a}
    ```

+ 从列表中删除 `素数`, 用 `Nothing` 替换它们:

    ```mathematica
    Range[20] /. p_?PrimeQ -> Nothing

    Out[1]= {1, 4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20}
    ```

    使用 `模式` 删除素数:

    ```mathematica
    DeleteCases[Range[20], p_?PrimeQ]

    Out[2]= {1, 4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20}
    ```

    选择非素数:

    ```mathematica
    Select[Range[20], ! PrimeQ[#] &]

    Out[3]= {1, 4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20}
    ```

+ 只有 `lists` 内部的 `Nothing` 会被去掉:

```mathematica
{Nothing, a}
Out[1]= {a}

f[Nothing, a]
Out[2]= f[Nothing, a]
```

+ 空序列: `Sequence[]` 将在任何头中 `蒸发`(evaporate), 除了那些有特殊属性的:

```mathematica
{Sequence[], a}
Out[3]= {a}

f[Sequence[], a]
Out[4]= f[a]
```

### 可能的问题

`Nothing` 作为 `标准计算过程` 一部分时被移除, 所以它被保留在 `held` 表达式中.

```mathematica
Hold[{a, b, Nothing, c}]
Out[1]= Hold[{a, b, Nothing, c}]
```

当 `Hold` 被释放时,  `Nothing` 被移除.

```mathematica
ReleaseHold[%]
Out[2]= {a, b, c}.
```
