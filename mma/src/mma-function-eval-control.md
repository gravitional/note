# 计算控制,Evaluation

Wolfram Language 所执行的基本操作是`计算`.
每当您输入一个表达式时, Wolfram 语言就会对该表达式进行`计算`, 然后返回结果.
Wolfram 语言的`计算`是通过应用一连串的定义来进行的.
这些定义可以是你明确输入的定义, 也可以是Wolfram语言中内置的定义.

因此, 举例来说, Wolfram语言使用内置的整数加法程序来`计算`表达式`6+7`.
同样地, Wolfram语言使用内置的`化简`程序来`计算`代数表达式`x-3x+1`. 如果你做了`x=5` 的定义, 那么`Wolfram`语言会使用这个定义将`x-3x+1`简化为`-9`.

Wolfram语言中最核心的两个概念可能是`表达式`和`计算`. `表达式`讨论了Wolfram语言所处理的所有不同种类的对象是如何用表达式来统一表示的.
本教程介绍了很多例子, 描述了Wolfram语言如何将众多操作, 以统一的方式看成是`Evaluation`.

Wolfram语言是一个`infinite evaluation`系统. 当你输入一个表达式时, Wolfram语言会不断地使用它所知道的定义, 一直到没有定义可以使用.
下面是一个递归定义, 其中阶乘函数是以自身为单位定义的.

```mathematica
fac[1] = 1; fac[n_] := n fac[n - 1]
```

如果你要求`fac[10]`, Wolfram Language将持续应用你给的定义, 直到它得到的结果不再改变.

```mathematica
fac[10]
3628800
```

当 Wolfram Language 使用了它所知道的所有定义后, 它就会给出它所得到的任何表达式作为结果.
有时, 结果可能是一个对象, 如一个数字. 但通常情况下, 结果是一个表达式, 其中一些对象是以符号形式表示的.
Wolfram语言使用其内置的定义来简化求和, 但不知道`f[3]`的定义, 所以将其留在符号形式中.

```mathematica
f[3] + 4 f[3] + 1
1 + 5 f[3]
```

Wolfram语言遵循的原则是: 持续应用定义, 直到它得到的结果不再改变. 这意味着, 如果你把 Wolfram 语言的输出作为输入, 你将再次得到相同的结果.
(在`Controlling Infinite Evaluation`中讨论了一些微妙的情况, 稍有不同).

在任何时候, Wolfram Language 只能使用它当时知道的那些定义.
然而, 如果你以后添加了更多的定义, Wolfram Language 将能够使用这些定义. 在这种情况下, 你从 Wolfram Language 得到的结果可能会改变.

下面是函数 `f` 的新定义.

```mathematica
f[x_] = x^2
x^2
```

有了新的定义, 你得到的结果可能会改变.

```mathematica
1 + 5 f[3]
46
```

`Evaluation`最简单的例子是使用定义, 如`f[x_]=x^2`, 它将一个表达式直接转化为另一个表达式.

但`Evaluation`也用于执行 Wolfram 程序. 因此, 例如, 如果你有一个由Wolfram语言表达式序列组成的程序, 其中一些可能表示`条件`和`循环`, 这个程序的执行相当于对这些表达式进行`计算`.
计算过程可能涉及多次`Evaluation`一个特定的表达式, 例如在一个循环中.

```mathematica
Do[Print[zzz], {3}]
zzzz
zzzz
zzzz
```

## Hold 属性类

### HoldFirst,HoldRest,HoldAll

+ `HoldFirst`: 保持第一个参数不计算
+ `HoldRest`: 不计算第一个参数之外的参数. `RuleDelayed`具有`HoldRest` 以及`SequenceHold`属性.

+ `HoldAll`: 通过设置函数的临时属性, 保持所有参数不计算.
    + 但是展平 `Sequence`, 使用`UpValue`
    + 参数被临时的 `Unevaluated` 包裹, 但是在传递给 `结构体` 的时候, 这个临时的 `wrapper` 会被去掉.
    + `Hold` 是带有 `HoldAll` 属性的 `容器` (container).

`Evaluate` 可以覆盖上面的属性, 强制运算函数的参数.

### HoldAllComplete,SequenceHold

+ `HoldAllComplete`: 不得以任何方式修改或查看函数的所有参数.
    + 不展开`Sequence`, 不移除`Unevaluated` wrappers, 不使用`UpValue`, 内部`Evaluate`无效
    + `HoldComplete` 是带有 `HoldAllComplete` 属性的 `容器`.

任何形式的 `计算控制` 都不会影响到具有 `HoldAllComplete` 属性的表达式.
在 `HoldAllComplete` 中的 `Evaluate` 无效.

```mathematica
HoldComplete[1 + 2]
HoldComplete[Evaluate[1 + 2]]
HoldComplete[Sequence[a, b]]

g /: HoldComplete[g[x_]] := x
HoldComplete[g[1]]
```

`HoldAllComplete` 只影响计算; 输入转换仍被应用.

```mathematica
FullForm[HoldComplete[a - b, a/b]]
HoldComplete[Plus[a,Times[-1,b]],Times[a,Power[b,-1]]]
```

`HoldAllComplete` 不会阻止格式化.

```mathematica
HoldComplete[Grid[{{1, 2}, {3, 4}}]]
```

添加 `DisableFormatting` 以防止格式化:

```mathematica
HoldComplete[DisableFormatting[Grid[{{1, 2}, {3, 4}}]] ]
```

+ `SequenceHold`; 指定出现在函数参数中的`Sequence`对象不应自动被压平.
    + `Rule`就具有此项属性.

## Unevaluated

### Detail

+ `Unevaluated[expr]`; 表示当`expr`作为 `函数` 的 `参数` 出现时, 它的 `未计算` 的 `原始形式`.
`f[Unevaluated[expr]]` 通过`临时设置属性`, 例如`HoldFirst`, 使 `f` 保持其参数不运算,
然后再计算 `f[expr]`, 其中`expr`保持接受时的形式. 例如:

+ 当 `Unevaluated[expr]` 独自出现时, 保持不计算的形式,  被放进嵌套结构时, 将发挥作用.
+ 根据这个描述, `Unevaluated` 不给表达式增加额外深度, `Depth` 和 `Level` 均跳过 `Unevaluated` 这层结构.

```mathematica
Depth[Hold[Plus[5,6,7,8]]] (*Depth 返回最大索引数目+1*)
Out[4]= 3

Depth[Unevaluated[5 + 6 + 7 + 8]]
Out[3]= 2

Level[Unevaluated[5 + 6 + 7 + 8], 1]
Out[2]= {5, 6, 7, 8}
```

将`未运算`的表达式送入`Length`:

```mathematica
Length[Unevaluated[5 + 6 + 7 + 8]]
```

+ 使用`HoldAll`和`Unevaluated` 来抑制符号的运算, 不管它们在哪里出现.

```mathematica
SetAttributes[symbolLength, HoldAll];
symbolLength[s_Symbol] := StringLength[SymbolName[Unevaluated[s]]]
(*查找符号名称的长度, 即使它已经定义了值 *)
xyzzy = 42;
symbolLength[xyzzy]
```

+ 使用 `Unevaluated` 来临时处理一个函数, 就像它有 `HoldAll` 属性一样.

```mathematica
Length[Unevaluated[1 + 2 + 3]]
Out[1]= 3
Length[1 + 2 + 3] .
Out[2]=0
```

+ `Unevaluated`只在它出现的地方起作用;它不会被传播.

```mathematica
f[x_] := g[x]
f[Unevaluated[1 + 1]]
Out: g[2]
```

+ `Unevaluated` 阻止 `Evaluate`.

```mathematica
Hold[Evaluate[Unevaluated[1 + 2]]]
```

+ 在 `hold` 类的函数中, `Unevaluated` 被保留:

```mathematica
SetAttributes[f, HoldAll]
f[Unevaluated[1 + 2]]
```

### 应用

对于函数`定义`/`调用`

    f:=[expr1,expr2]

尽管`Set`具有`HoldFirst`属性, `SetDelayed, UpSetDelayed` 具有 `HoldAll`属性.
`arguments` 在传递给函数`f` 的时候, 的确保持未被计算的形式,
但是在 `f` 的函数体中简单地 `调用` 参数, 或者将 `算符` 作用于 `参数`, 都会造成`参数`被递归计算.
所以最终参数无法维持 `raw` 形式.

例如在下面的定义中, 还是会先计算`g[x]`的值:

```mathematica
g[x]=2; f[g[x]]:=0
??f
Out: f[2]:=0
```

最后 `f` 的定义是 `f[2]=0`, 而不是 `f[g[x]]=0`.
这样的好处是, 可以放心将 `g[x]` 用作复杂表达式的接口,
定义 `f` 的时候, 内部的表达式会自动替换成其定义. 可以方便输入.

如果想要保持 `参数` 始终不被计算, 需要同时使用 `HoldAll` 属性和 `Unevaluated`,
这样在 `参数传递` 的整条链路上, 始终不进行计算.
比如想实现 `f[g[x]]=0` 这种定义, 需要使用

```mathematica
ClearAll[f];(* 清除之前的定义 *)
g[x]=2; f[Unevaluated[g[x]]]:=0
??f
```

## Hold 容器,函数

非标准计算流程,Hold

控制表达式计算流程: 各种 `Hold`

`Hold` 相关的分成两类, 一类是函数 `Hold`, 另一类是 属性 `Attribute`中的 `Hold`

### Hold

属性:  `HoldAll`
解封:  `ReleaseHold`

使用 `UpValue`
展开`Sequence`
内部`Evaluate`有效
不移除`Unevaluated`
内部`Replace`有效

### HoldComplete

`HoldComplete`
属性:  `HoldAllComplete`
解封:  `ReleaseHold`

不使用 `UpValue`
不展开`Sequence`
内部`Evaluate`无效
不移除`Unevaluated`
内部`Replace`有效

### HoldPattern

`HoldPattern[expr]` ; 用于模式匹配的时候, 等价于 `expr`, 但是保持 `expr` 不计算.
属性:  `HoldAll`

`HoldPattern` 的一个应用是, 指定适用于`未计算`的表达式的`模式`, 或以未计算形式被 `Hold`的表达式, 例如:

`HoldPattern` 使 `1+1` 不被计算, 并允许它与 `/.` 运算符左侧的 `1+1` 相匹配.

```mathematica
Hold[u[1 + 1]] /. HoldPattern[1 + 1] -> x
Out[3]= Hold[u[x]]
```

请注意, 虽然像 `Hold` 这样的函数阻止了对表达式的计算,
但它们并不影响用 `/.` 和其他 `运算符` 对这些表达式的部分进行操作.

下面定义了 `r` 的值, 只要`r` 的参数不是原子对象(原子表达式):

```mathematica
r[x_] := x^2 /; ! AtomQ[x]
```

根据这个定义, 像 `r[3]` 这样的表达式就不会被改变:

```mathematica
r[3]
Out[5]=r[3]
```

然而, 根据 `r` 的定义, 模式 `r[x_]` 将被转化:

```mathematica
r[x_]
Out[6]= x_^2
```

你需要把 `HoldPattern` 包在 `r[x_]` 周围, 以防止 `r[x_]`被计算后,  从而无法用于替换规则:

```mathematica
{r[3], r[5]} /. HoldPattern[r[x_]] -> x
Out[7]= {3, 5}
```

如上图所示, 像 `lhs->rhs` 这样的`转换规则`的左手边通常被立即计算,
因为这些规则通常被应用于`已经被计算过的`表达式. `lhs->rhs` 的右边也被立即计算.
然而, 在延迟规则 `lhs:>rhs` 中, 表达式 `rhs` 不被计算.

在`->`规则中, 右手边会立即被计算, 但在`:>`规则中不会.

```mathematica
{{x -> 1 + 1}, {x :> 1 + 1}}
Out[8]= {{x -> 2}, {x :> 1 + 1}}
```

下面是应用这些规则的结果. `:>` 规则的右侧被插入 `Hold` 内, 保持不计算的形式.

```mathematica
{x^2, Hold[x]} /. {{x -> 1 + 1}, {x :> 1 + 1}}
Out[9]= {{4, Hold[2]}, {4, Hold[1 + 1]}}
```

### Verbatim

`Verbatim` 相比于 `HoldPattern`, 具有更强的控制

`Verbatim`, 不翻译模式比如`_`, 只匹配字面值

`HoldForm`
输出形式的函数
属性:  `HoldAll`
解封:  `ReleaseHold`

### Evaluate

`Evaluate[expr]` ; 导致 `expr` 被计算, 即使它作为一个函数的参数, 而这个函数的`属性`指定它应该被保持不被计算.

+ 你可以使用 `Evaluate` 来覆盖(override)内置函数的 `HoldFirst` 等属性.
+ 只有当 `Evaluate` 直接作为函数参数的头部出现时, 它才会覆盖外层的 `HoldFirst` 等属性, 否则会被保留.
+ `Evaluate`可以强行计算带有`HoldAll`, `HoldFirst`,`HoldRest`属性的参数,

+ `Evaluate` 在带有 `HoldAllComplete` 属性的函数中不起作用.
+ `Evaluate` 在 `Unevaluated` 中不起作用.

```mathematica
HoldComplete[Evaluate[1 + 2]]
Out[1]=Unevaluated[Evaluate[1+1]]
Unevaluated[Evaluate[1 + 1]]
Out[1]= Unevaluated[Evaluate[1 + 1]]
```

在 `Held` 类的函数里, `Evaluate` 只在第一层起作用

```mathematica
Hold[f[Evaluate[1+2]]]
Out[1]= Hold[f[Evaluate[1+2]]]

Hold[Evaluate[f[1 + 2]]]
Hold[f[3]]
```

### ReleaseHold

+ `ReleaseHold[expr]` ; 删除 `expr` 中的 `Hold`, `HoldForm`, `HoldPattern` 和 `HoldComplete`.
+ `ReleaseHold` 只删除一层 `Hold` 等函数; 它不会删除`Hold` 等函数中出现的 嵌套的 `Hold`.

+ 基本例子

```mathematica
Hold[1 + 1]
Out[1]:= Hold[1 + 1]
ReleaseHold[%]
输出[2]=2
```

+ `ReleaseHold` 删除所有标准的`不计算`容器(containers).

```mathematica
ReleaseHold /@ {Hold[1 + 2], HoldForm[2 + 3], HoldComplete[3 + 4],
  HoldPattern[_*_]}
Out[1]= {3, 5, 7, _^2}.
```

+ `ReleaseHold` 只删除最外层的 `Hold`:

```mathematica
ReleaseHold[f[Hold[1 + 2]]]
f[3]

ReleaseHold[f[Hold[1 + g[Hold[2 + 3]]]]]
Out[2]=f[1+g[Hold[2+3]]]
```
