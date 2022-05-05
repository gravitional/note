# 函数

    定义具有可选变量的函数: tutorial/SettingUpFunctionsWithOptionalArguments
    可选变量与默认变量: tutorial/OptionalAndDefaultArguments, tutorial/Patterns#17673

指定函数参数的类型, 可以用以下语法:

+ 指定类型用`x_ h`语法, or `Blank[h]`
+ `x_:v`;  如果没有提供, 默认值是`v`
+ `x_ h:v`; 头部是`h`, 默认值是`v`
+ `x_.`;  一个表达式, 带有内置的默认值, 内置默认值用`Default`设置, `Default[f,i]` 设定第`i`个默认值.
+ `p|PatternSequence[]`  可选模式`p`, 不带默认值,`PatternSequence[]` 表示长度为零的模式.

`位置参数` 的比较完整的形式是: `name:_head`

```mathematica
x : _Integer
```

`默认参数` 的比较完整的形式是: `name:_head:default`

```mathematica
x:_h:v
```

## 可选参数与默认参数

可选参数是用 `Optional` 定义的. 函数的默认值是用 `Default` 定义, 或者查看的.
它们的关系和区别是:

+ `Optional` 是模式对象, 可以在定义函数原型时使用, 模式匹配不到时会返回默认值.
但是除非知道声明原型, 否则不知道默认值. 它一般是对某单个位置上的模式指定的.

+ `Default` 给出了设定函数 `f` 的参数的全局默认值, 可以通过 `_.` 取回.
可以用在 `f` 出现的各种地方, `f` 出现的替换也会考虑默认值.
`Default` 可以设定无穷个参数的默认值, 如具有 `Flat` 属性的 `Plus` 和 `Times`,
且此值无需知道 `f` 的 声明原型 即可使用.

```mathematica
patt:def 或者 Optional[patt,def];  是模式对象, 表示 `patt` 形式, 如果被忽略, 将被默认值`def`代替.
```

+ `Optional[s_h]`  表示一个可以忽略的函数, 但如果存在, 必须有头部 `h`. 这种情况下没有任何更简单的句法形式.
+ `f[x_,k_:kdef]:=value`: 第二个位置为可选变量, 默认值为 `kdef` 的函数

+ `默认值` 的模式, 加上头部指定, 以及函数的默认取值`_.`, 一共有三种写法, 以及简略写法:

+ `s:_`: 模式记号为`s`.
+ `s:_h`:模式记号为`s`, 模式的头部为`h`.
+ `s : _ : v`: 模式记号为`s`, 默认值为`v`.
+ `s : _.` : 模式记号为`s`,模式的默认值取函数此位置的全局默认值.
+ `s : _h : v`:模式记号为`s`, 模式的头部为`h`,默认值为`v`.

下面是对应的缩写

+ `s_`
+ `s_h`
+ `s_: v`
+ `s_.`
+ `s_h: v`

### Default

```mathematica
Default[f]; 给出函数`f` 的默认值, 此值可以通过模式对象 `_.` 使用.
Default[f,i]; 给出默认值, 此值仅当 `_.` 作为 `f` 的第 i 个参数时使用.
Default[f,i,n]; 给出默认值, 此值仅当 `_.` 作为 `f` 的第 i 个参数时使用, 并要求总共有 `n` 个参数.

Default[f, ...]=val; 定义 `f` 的参数的默认值.
```

+ `_.` 代表函数的 `可选参数`, 其 `默认值` 由 `Default` 指定.
+ 在 `_.` 被用作 `f` 的参数之前, 必须先定义 `Default[f]` 的必要值.
+ 为 `Default[f]` 定义的值被保存在 `DefaultValues[f]` 中.

### 例子

+ 定义 `f` 的默认值为 `0`:

```mathematica
Default[f] = 0
Out[1]= 0
```

+ `_.` 表示一个可以忽略的参数:

```mathematica
f[x_., y_.] = {x, y}
Out[2]= {x, y}
```

如果省略第二个参数, 则使用默认值 `0` 来代替:

```mathematica
f[a]
Out[3]= {a, 0}
```

如果两个参数都被省略, 则两个参数都被认为是默认值0:

```mathematica
f[]
Out[4]= {0, 0}
```

## 函数的选项

+ `Options[f]`
+ `OptionsPattern`(选项模式)
+ `OptionValue`(选项值)

`key-value` 键值对类型的参数, 在 `mma` 中, 通过`选项`--`Option`实现.
在定义函数的时候, 通过 `OptionsPattern[]` 来匹配或表示. 它表示`0`个或任意个具有名字的可选参数.

+ `OptionsPattern`匹配由`->`或`:>`指定的的任何替换规则序列, 或规则的嵌套列表.
+ 在`OptionsPattern [{spec1,spec2, ...}]`中, `speci`可以是函数头部`fi`, 或显式的规则`opti->vali`.  Head `fi`也将当成规则列表处理, 使用`Options[fi]`获得规则列表.
+ `OptionsPattern[]`使用`nearest enclosing function`的默认选项.
+ 使用`OptionsPattern[{}]`表示不包含默认选项.

使用`OptionValue[f, {Frame, PlotPoints}]`获取选项的`value`

使用`FilterRules[rules,patt] `挑选规则列表, 例如:

```mathematica
FilterRules[{a->1,b->2,c->3},{b,a}]
{a -> 1, b -> 2}
```

## 函数的属性

ref: tutorial/Attributes

+ `Attributes[f]` ; 给出 f 的属性
+ `Attributes[f]={attr1,attr2,...}` ; 设置 f 的属性
+ `Attributes[f]={}` ;  令 f 没有属性
+ `SetAttributes[f,attr]` ;  为 `f` 添加属性 `attr`
+ `ClearAttributes[f,attr]` ;   从 f 中清除 attr 属性

## 带有默认参数的匹配

ref: tutorial/OptionalAndDefaultArguments

有些函数, 比如 `Plus`, 具有 `Flat` 性质, 在模式匹配中可以匹配任意多的数目的参数, 因为

```mathematica
Plus[1,2,3]=Plus[1,Plus[2,3]]
```

但是它不能匹配单个`a`. 这时候可以使用`x_+y_.`这样的写法,
对应的函数是`x+Optional[y_]`, 就可以匹配到 `a+0` 了, 由于`Plus`具有全局默认参数, `0`.

使用 `x_.` 可以匹配那些在数学上相等, 但是在结构上不相等的式子.
`x_.` 会自动选取外层函数的 `全局默认值`.

```mathematica
{g[a^2], g[a + b]} /. g[x_^n_] -> p[x, n]
{p[a, 2], g[a + b]}
```

有时候需要分配 `没有默认值` 的可选参数, 可以使用`2 | PatternSequence[]`, 如

```mathematica
{g[1], g[1, 1], g[1, 2]} /. g[x_, 2 | PatternSequence[]] :> p[x]
{p[1], g[1, 1], p[1]}
```

## 函数定义的顺序

tutorial/TransformationRulesAndDefinitions#16933

当您在 Wolfram  中给定一连串的定义时, 有些定义可能比其他定义更普适.
Wolfram  遵循的原则是, 尽量把更一般的定义放在更具体的定义之后.
这意味着先应用特殊规则, 再应用一般规则.

这种行为对 `Making Definitions for Functions` 中给出的阶乘函数的例子至关重要.
无论您输入的顺序如何, Wolfram 系统总是将特殊情况 `f[1]` 的规则放在一般情况 `f[n_]` 的规则之前.
这意味着, 当 Wolfram  寻找 `f[n]`形式的表达式的值时, 它会首先尝试特殊情况 `f[1]`, 只有当它不适用时, 才会尝试一般情况 `f[n_]` .
因此, 当你要求`f[5]`时, `Wolfram` 系统将继续使用一般规则, 直到结束条件`f[1]`适用.

>Wolfram系统试图把具体的定义放在一般的定义之前.

在上面使用的阶乘函数的例子中, 很明显能看出哪个规则更通用. 然而, 通常情况下, 无法判断你给出的规则在普适性方面的顺序.
在这种情况下, Wolfram 系统会按照您提供的顺序来使用这些规则. 例如:

```mathematica
log[x_ y_] := log[x] + log[y].
log[x_^n_] := n log[x] .
log[2 x_] := log[x] + log2
?log
定义存储的顺序是:
log[2 x_] := log[x] + log2,
log[x_ y_] := log[x] + log[y],
log[x_^n_] := n log[x]
```

您应该意识到, 没有一般的方法可以判断规则的顺序, 例如当两条规则都包含复杂的`/;`条件.
而且有可能规则本质上就没有确定的顺序. 这时候 Wolfram 将按照您提供的顺序存储规则.

## 函数缓存

tutorial/TransformationRulesAndDefinitions#16933

当你使用 `:=` 来定义一个函数时, 每次你要求该函数的值时都会重新计算. 在某些类型的计算中, 您可能会多次要求获得相同的函数值.
在这种情况下, 你可以通过让` Wolfram Language` 记忆它找到的所有函数值来节省时间. 这里有一个定义函数的 `习语`, 可以做到这一点.

`f[x_]:=f[x]=rhs` ; 定义一个函数, 记住已经求解过的值. 例如:

```mathematica
In[1]:= f[x_] := f[x] = f[x - 1] + f[x - 2]
Here are the end conditions for the recursive function f:
In[2]:= f[0] = f[1] = 1
f[8]
```

你可以看到`f[x_]:=f[x]=f[x-1]+f[x-2]`的定义是如何工作的. 函数`f[x_]`被定义为 "程序" `f[x]=f[x-1]+f[x-2]`.
当你要求得到函数f的值时, `程序` 就被执行. 程序首先计算`f[x-1]+f[x-2]`的值, 然后将结果保存为`f[x]`.

当你在Wolfram语言中实现数学递归关系时, 使用这种方法通常效果不错. 当你需要反复应用递归关系来计算`f(10)`, 你会多次重复计算`f(5)`等.
因此在这种情况下, 最好记住`f(5)`的值, 当你需要它的时候直接调用.

当然, 在记忆数值方面也会有代价. 找到一个特定的值会更快, 但要存储所有的值需要更多的内存空间.
通常只有在将要产生的不同数值的总数相对较少, 或者重新计算它们的费用非常大的情况下, 你才应该定义函数来记忆数值.

## 中断和放弃

tutorial/EvaluationOfExpressions#13759; Interrupts and Aborts

`Interrupt[]` ; 中断一个计算
`Abort[]`;  放弃一个计算
`CheckAbort[expr,failexpr]` ; 计算`expr`并返回结果, 如果发生中止, 则返回`failexpr`.
`AbortProtect[expr]` ; 计算`expr`, 掩盖`Abor`的影响, 直到运算完成.

函数`Abort[]`的效果与先`Interrupt`, 再在中断菜单中选择`放弃`选项的效果相同.
你可以使用`Abort[]`来实现程序中的 "紧急停止".
然而大部分情况, 你应该尽量使用像`Return`和`Throw`这样的函数, 它们的行为更加容易控制.

例子: `Abort`终止计算, 所以只有第一个`Print`被执行.

```mathematica
Print[a]; Abort[]; Print[b]
a
```

如果你在运算 Wolfram  表达式的任何时候中止, Wolfram Language 通常会放弃对整个表达式的运算, 并返回值 `$Aborted`.

然而, 你可以使用函数`CheckAbort`来 "捕获 "中止.
如果在`CheckAbort[expr,failexpr]`中运算`expr`时发生中止, 那么`CheckAbort`返回`failexpr`, 但`Abort`不会进一步向外层传播.
像`Dialog`这样的函数以这种方式使用`CheckAbort`来控制中止的效果.

当你在 Wolfram  中构建复杂的程序时, 有时你可能想保证程序中的某个特定部分的代码不能被中止,
无论是交互式的还是通过调用 `Abort`. 函数 `AbortProtect` 允许你运算一个表达式, 将任何`Abort`暂存到表达式运算完成之后.  例如:

```mathematica
AbortProtect[Abort[]; Print[a]]; Print[b]
a
$Aborted
```

`CheckAbort` 可以看到 `Abort`, 但不会进一步传播:

```mathematica
AbortProtect[Abort[]; CheckAbort[Print[a], x]]; Print[b]
```

即使在 `AbortProtect` 内部, `CheckAbort` 也能看到产生的 `aborts`, 并返回合适的`failexpr`.
除非`failexpr`也包含`Abort[]`, 否则`aborts`将被`CheckAbort`吸收.

## Catch,Throw

`Catch[expr]` ; 运算`expr`直到遇到`Throw[value]`, 然后返回值.
`Catch[expr,form]` 运算`expr`直到遇到`Throw[value,tag]`, 其中`form`与`tag`匹配.
`Catch[expr,form,f]` 返回`f[value,tag]`而不是`value`.

例子: 当遇到`Throw`时, 运算停止, `i`的当前值被返回作为最靠近的 `Catch`的值.

```mathematica
Catch[Do[Print[i]; If[i > 3, Throw[i]], {i, 10}]] .
```

`Throw` 和 `Catch` 提供了一种灵活的方式来控制 `Wolfram`  的运算过程.
其基本思想是, 每当遇到 `Throw` 时, 正在进行的运算就会停止, `Wolfram`  会立即返回到最近的 `Catch`包裹.

对于`Map`也会得到相同的结果, 尽管`Map`本来会返回一个列表, 但是`Throw`会中断列表的形成.

```mathematica
Catch[Map[(Print[#]; If[# < 6, Throw[#]]) &, {7, 6, 5, 4}]]
```

你可以使用`Throw`和`Catch`来控制函数式编程结构的流向, 例如, 允许这些结构的运算持续进行, 直到满足某些条件后停止.
请注意, 如果你使用`Throw`来停止运算, 那么你得到的结果, 其结构可能与完整运算得到的结构不同.

`Throw`和`Catch`的操作是 Global 的: 不管`Throw`是如何产生的, 也不管在哪里产生的, 它总是会停止求值, 并返回到包裹一的`Catch`.
例子: `Throw`停止对`f`的计算, 导致`Catch`只返回`a`, 而不含有`f`

```mathematica
Catch[f[Throw[a]]]
```

在小型程序中, 使用`Throw[value]`和`Catch[expr]`的最简单形式通常就足够了.
但如果你写的程序比较大, 包含很多独立的部分, 通常使用`Throw[value,tag]`和`Catch[expr,form]`会好很多.

通过让表达式`tag`和`form` local 在程序的某特定部分, 你可以保证`Throw`和`Catch`也只在那个部分生效.

```
Catch[f[Catch[Throw[x, a], a]], b] . (*被内层捕捉*)
Catch[f[Catch[Throw[x, b], a]], b] . (*被外层捕捉*)
Catch[Throw[x, a], a | b] (* 可以使用模式 *)
```

你出现在`Throw`中的标签不必是常量, 它可以是任何表达式.

```mathematica
Catch[Do[Catch[Throw[i^2, i], n_ /; n < 4], {i, 10}], _]
```

当使用`Catch[expr,form]`和`Throw[value,tag]`时, `Catch`返回的值只是`Throw`中表达式的值.
而使用`Catch[expr,form,f]`,返回的值是`f[value,tag]`.

```mathematica
Catch[Throw[x, a], a, f] (* 这里f被应用于Throw中的值和tag.*)
Catch[x, a, f] (*如果没有Throw, 就不会被使用f  *)
```
