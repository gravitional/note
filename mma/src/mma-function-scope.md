# 变量局部化

`Block` 居域化变量, 但不创建新变量; `Module` 创建新变量.

```mathematica
x = 7;
Block[{x}, Print[x]]
Module[{x}, Print[x]]
```

`Block`和`Moudle`的区别可以查看`tutorial/BlocksComparedWithModules`,

`Module` 是 `lexical scoping`, 而`Block`是`dynamic scoping`,
`Module` 把 `程序文本` 中出现的变量替换成局部变量,

而`Block` 把 `计算过程`中出现的变量替换成局部变量, 也就是运行时修改, 或者说 `Block` 在执行堆栈中进行替换.
`Block`在交互计算的时候, 更常用, 因为这时候更关注计算历史.

## Block

+ `Block[{x,y,...},expr] ` ;  指定 `expr` 用符号 `x, y, ...` 的局部值进行计算.
+ `Block[{x=x0,...},expr]`  ; 规定了 `x, ...` 的初始局部值.

### Background

`Block`是一个`scoping`结构, 它将符号本地化, 通过隔离符号在`Block` 中拥有的值, 与在`Block` 外可能拥有的值.

`Block`实现了变量的动态范围, 这意味着本地符号的名称保持不变.
当 `Block` 内的 `局部符号` 具有`值`时, 就使用这个值;
否则, 就使用外层闭包函数中的值(以此类推).
`Block`只对值进行 局域化, 不对其进行替换.

相比之下, `Module`  实现 `词法作用域`(lexical scoping),
意味着在每次调用中都会创建一个`新的`, `唯一`的变量副本,
在任何`外层包围函数`, 或随后对 `Module` 的调用中都不会再次用到.

`Block` 通常比 `Module` 快, 所以当带有 `scoped 变量` 的函数被多次调用时, 使用 `Block` 可能导致更快的计算.

`Block` 被自动用来局域化 `constructs` 中的`迭代器`的值, 如 `Do`, `Sum`, `Product` 和 `Table`. `Block`可以被嵌套.
由于其`Dynamic scoping` 的特性, `Table` 中的迭代指标相继引用. 例如:

```mathematica
Table[1, {i, 5}, {j, 1, i}] // TableForm
```

`With` 是另一个允许定义局部常量的`scoping construct`.

它实现了`只读词法变量`(read-only lexical variables),
并且只有当`符号` 不作为`局部变量`, 出现在更深层的嵌套`作用域`中时才替换它们.

和 `Block` 一样, `With` 也比 `Module` 快.
`普通的替换`不能保持`作用域`, 但 `With`允许在`未计算`的表达式中进行`替换`, 从而保持嵌套的作用域.

在 `Block` 构造内部(或外部), 可以使用 `CompoundExpression` 将多个表达式组合在一起.

### Detail

+ `Block` 允许你设置一个环境, 在这个环境中, `变量` 的值可以暂时被改变.
+ 当你执行一个 `Block` 时, 分配给局域变量 `x, y, ...` 的值将被清除.
当`Block`的执行结束后, 这些符号的`原始值`会被恢复.
+ `Block`只影响符号的值, 不影响它们的名称.
+ 为 `x, y, ...` 指定的初始值`x0,y0,...`, 在 `x, y, ...` 被清空之前被计算. 这意味着初始值的表达式可以引用它们原来的值.

    x = 1;Block[{x = x + 1}, x]

+ 你可以使用 `Block[{vars},body/;cond]` 作为`替换规则` 的右侧, 并可以带有`condition`.
+ `Block` 具有 `HoldAll` 属性.
+ `Block` 实现了变量的`动态作用域`(dynamic scoping), 也就是运行时作用域.
+ 在迭代结构(如 `Do`, `Sum` 和 `Table`)中, `Block` 被自动用来局域化`迭代器`的值.

### 例子

```mathematica
Block[{$RecursionLimit = 20}, x = x + 1]

During evaluation of In[1]:= $RecursionLimit::reclim2: Recursion depth of 20 exceeded during evaluation of 1+x.
Hold[1 + x]
```

计算函数体时, `x=x+1`  返回 `x+1`, 其中 `x` 又定义为 `x+1`, 于是发生了递归.
这个特点是由 `Block` 的动态特性决定的.

### Scope

暂时改变 `全局系统参数` 的设置:

```mathematica
Block[{$MaxPrecision = 24, $MinPrecision = 24}, Sin[1`20]]
Out[1]= 0.841470984807896506652502

Block[{$MaxExtraPrecision = 100}, Sin[Exp[200]] > 0]
Out[2]= False
```

计算`表达式`, 并将`变量`暂时设置为给定值.

```mathematica
Expand[(1 + x)^5]
Out[1]= 1 + 5 x + 10 x^2 + 10 x^3 + 5 x^4 + x^5

Block[{x = 5}, %]
Out[2]= 7776

x
Out[3]= x
```

## 应用

进行深度递归计算:

```mathematica
cl[1] = 1;
cl[n_Integer?EvenQ] := cl[n/2] + 1
cl[n_Integer?OddQ] := cl[3 n + 1] + 1

Block[{$RecursionLimit = Infinity}, cl[9780657630]]

Out[2]= 1133
```

`$RecursionLimit` 的默认设置对这个计算来说太低了.

```mathematica
cl[9780657630]

在计算In[3]:= $RecursionLimit:: reclim2时. 在计算EvenQ[18215]的过程中, 超过了1024的递归深度...
Out[3]=1022+cl[18215]
```

使用`局部表达式`来可视化一个复杂的图.

```mathematica
Block[{f = Sin[x + I y]}, 
 ParametricPlot[Evaluate[{Re[f], Im[f]}], {x, -Pi, Pi}, {y, -2, 2}, Mesh -> 10]]
```

### 属性与关系

`Block` 只对`values`进行`局域化`, 它不创建新的`符号`:

```mathematica
x = 7;
Block[{x}, Print[x]]
```

`Module`创建新的符号.

```mathematica
Module[{x}, Print[x]]
x$358
```

`Iterators` 以类似于 `Block` 的方式将他们的迭代指标局域化:

```mathematica
i = 5;
Table[i, {i, 1, i}]
Out[1]= {1, 2, 3, 4, 5}
```
