# mma 控制结构 flow

## If

### 背景

`If`是一种过程性(procedural)编程结构, 其实际计算的`分支` 取决于哪个`条件`为`真`.
`If` 通常需要一个条件和两个附加参数:

    If[cond,t,f]

这里, 分别对应条件为`真`或`假`, `t` 和 `f` 将分别被计算.
`If` 也可以带三个附加参数:

    If[cond,t,f,u]

这里, `u` 给出了在指定的条件既不是明确的 `True` 也不是明确的 `False` 时要进行的计算.
最后, `If` 可以只接受一个额外参数.

    If[cond,t]

在这种情况下, `f`(false) 的值被认为是 `Null`.

为了提高效率, `If` 只对相关的参数进行计算.
例如, 如果 `If[cond,t,f,u]` 中的条件是 `True`, 只有`t`(而不是`f`或`u`)会被计算.

`ConditionalExpression` 是一个相关的符号结构, 它只在给定条件为 `True` 时表示一个表达式.
其他更灵活的编程结构概括了`If`, 包括 `Which` 和 `Switch`.
根据参数值计算的数学函数包括 `Boole` 和 `Piecewise`.

+ `Condition` 是一种模式, 只有在测试结果为 `True` 时才会匹配.
+ `TrueQ`是 `If` 的一种特殊情况, 如果表达式明确为 `True`, 则产生 `True`, 否则为 `False`.

### 性质和关系

通过 `cases`定义一个函数:

```mathematica
sign[x_] := If[x < 0, -1, 1]
sign /@ {-1, 0, 1, I}

Less::nord: Invalid comparison with I attempted.
Out[2]= {-1, 1, 1, If[I < 0, -1, 1]}
```

或者, 使用几个`conditional`定义.

```mathematica
sign2[x_ /; x < 0] := -1
sign2[x_ /; x >= 0] := x
sign2 /@ {-1, 0, 1, I}

Less::nord: Invalid comparison with I attempted.
GreaterEqual::nord: Invalid comparison with I attempted.
Out[4]= {-1, 0, 1, sign2[I]}
```

使用 `Which` 而不是嵌套的 `if-then-elseif` 链:

```mathematica
cut[x_] := If[x < -1, -1, If[x < 1, x, 1]]
cut /@ {-2, -1, 0, 1, 2}
Out[2]= {-1, -1, 0, 1, 1}

cut2[x_] := Which[
  x < -1, -1,
  x < 1, x,
  True, 1]
cut2 /@ {-2, -1, 0, 1, 2}
Out[4]= {-1, -1, 0, 1, 1}
```

使用 `PiecewiseExpand` 将 `If` 转换为 `Piecewise`:

```mathematica
PiecewiseExpand[If[c, a, b]]
PiecewiseExpand[If[If[c1, a1, b1], a, b]]
PiecewiseExpand[If[c, If[c2, a2, b2], b]]
```

## Switch

    Switch[expr, form1, value1, form2, value2, ... ]

计算 `expr`, 然后依次与每个 `form_i`进行比较,
对于第一个匹配成功的 `form`, 计算并`返回`对应的 `value`

### Details

+ 只有与`第一个`匹配 `expr` 的 `form_i`, 对应的 `value_i`被计算.
每个`form_i`, 只有在轮到它尝试匹配`expr`时, 才会被计算.
+ 若最后一个 `form_i` 是模式 `_`, 那么如果达到这个`case`,
相应的 `value_i`总是被返回, 也就是默认结果.
+ 如果没有`form_i` 与 `expr` 匹配, 那么 `Switch` 将`不被计算`(unevaluated), 返回原式.
+ `Switch` 有属性 `HoldRest`(即 `form,value` 对在传入时保持不计算).
+ 你可以在 `Switch` 中使用 `Break`, `Return` 和 `Throw`.

### 实例

### 基本例子

将布尔值转换为 `1` 或 `0`, 给出一个 `message`, 当不是 `布尔值` 时默认为`0`.

```mathematica
f::boole = "The value `1` is not True or False.";
f[b_] := Switch[b, True, 1, False, 0, _, Message[f::boole, b]; 0]
{f[True], f[False], f[x]}
f::boole: The value x is not True or False.
Out[3]= {1, 0, 0}
```

给出一个表达式的 `符号变换` 建议

```mathematica
t[e_] := Switch[e, _Plus, Together, _Times, Apart, _, Identity]
e = (1 + x)/(1 - x) + x/(1 + x);
t[e]
Out[2]= Together

(*试着进行转换:*)
e1 = t[e]
Out[3]= (-1 - 3 x)/((-1 + x) (1 + x))
```

## Set

```mathematica
lhs=rhs 对 `rhs` 进行计算, 并将结果指定为 `lhs` 的值. 从此以后, 只要 `lhs` 出现就会被 `rhs` 取代.
{l1, l2, ...}={r1, r2, ...} 计算 `r_i`, 并将结果指定为相应的 `l_i`的值.
```

+ `lhs` 可以是任何表达式, 包括 `模式`.
+ `f[x_]=x^2` 是典型的模式的赋值. 请注意左边的 `_`, 而不是位于右边.
+ `f[args]=rhs` 这种形式的赋值, 设置了与符号 `f` 相关的转换规则.
+ 与 `特定符号` 相关的不同规则, 通常按照你给出的顺序放置.
如果你给出的 `新规则` 相比于 `现有规则` 更 `具体`, 那么它会被优先放置(即放在前面).
当这些 `规则` 被使用时, 它们会按顺序被测试.

+ 具有相同 `lhs` 的新赋值, 会覆盖旧的赋值.
+ 你可以用 `?f` 或 `Definition[f]` 查看与符号 `f` 相关的所有赋值.
+ 如果你为具有 `Flat` 和 `Orderless` 等属性的函数做赋值,
你必须确保在 `赋值` 之前, 为函数设置这些属性.
+ `Set` 具有 `HoldFirst` 属性.
+ 如果 `lhs` 是 `f[args]` 的形式, 则 `args` 会被计算.

+ 有一些特殊的函数, 对 `s[f[args]]` 的赋值, 会自动与 `f` 而不是 `s` 相关联,
这些函数包括: `Attributes`, `Default`, `Format`, `MessageName`, `Messages`, `N` 和 `Options`.

+ 当 `Set` 以不计算的 `符号形式` 出现时, 它被视为 `scoping` (作用域)结构,
因此如果需要, 嵌套出现的 `变量` 会被 `重命名`.
+ `lhs=rhs` 返回 `rhs`, 即使由于某种原因不能执行指定的 `赋值`.
+ 一些 `全局变量`, 如 `$RecursionLimit`, 只能被分配到特定的范围或类别的值.

### 背景和上下文

`Set` 是函数, 它计算并将 `表达式` 的 `值` 赋给 `变量`.
表达式 `Set[lhs,rhs]` 通常用速记语法 `lhs=rhs` 表示.

在 `Set` 被计算后, 只要出现 `lhs`, 就会用 `rhs` 取代.
根据 `lhs` 的形式, 结果被存储在相关的 `OwnValues`, `DownValues` 或专门的 `数据结构` 中.

`Set` 的 `lhs` 通常包含模式, 这些模式将按照 `rhs` 的规则得到 `转换值` , 例如 `f[x_] = x^2`.
`lhs=rhs` 会立即计算 `rhs`, 所以如果需要先赋值, 再计算 `rhs`, 应该使用 `SetDelayed`(简写为 `:=` )来代替.
使用 `Definition[f]` 可以看到与给定符号相关的赋值.
单个赋值可以用 `Unset` 从符号中删除;
`Clear` 和 `ClearAll` 可以一次性删除所有定义.

### 例子

为 `x ` 设置 `值`:

```mathematica
x = a + b
Out[1]= a + b

1 + x^2
Out[2]= 1 + (a + b)^2
```

Unset `x`:

```mathematica
x =.
```

设置 `多个值`:

```mathematica
{x, y, z} = Range[3]
Out[1]= {1, 2, 3}

x + y^2 + z^3
Out[2]= 32
```

### 范围

#### 左边

普通程序变量:

```mathematica
i = 1;
While[Prime[i] < 100, i = i + 1]; i
26
```

为 `索引变量` 设置 `值`:

```mathematica
a[1] = x; a[2] = y;

{a[1], a[2], a[3]}
{x, y, a[3]}
```

从 `表达式` 定义 `函数`:

```mathematica
Expand[(1 + x)^3]
Out[1]= 1 + 3 x + 3 x^2 + x^3

f[x_] = %
Out[2]= 1 + 3 x + 3 x^2 + x^3

f[a + b]
Out[3]= 1 + 3 (a + b) + 3 (a + b)^2 + (a + b)^3
```

使用 `Block` 来临时设置变量:

```mathematica
Block[{$RecursionLimit = 20}, x = x + 1]
18 + Hold[1 + x]
```

设置列表的部分:

```mathematica
v = {a, b, c, d}
Out[1]= {a, b, c, d}

v[[2]] = x
Out[2]= x

v
Out[3]= {a, x, c, d}
```

设置 `表达式` 的部分:

```mathematica
v = 1 + x^5
Out[1]= 1 + x^5

v[[2, 2]] = 77777
Out[2]= 77777

v
Out[3]= 1 + x^77777
```

+ 替换矩阵的某一行:

```mathematica
mat = ( {
    {1, 2, 3},
    {4, 5, 6},
    {7, 8, 9}
   } );

mat[[2]] = mat[[2]] + 10;
MatrixForm[mat]
```

+ 替换矩阵的某一列:

```mathematica
mat[[All, 3]] = {100, 101, 102};
MatrixForm[mat]
```

#### 不同类型的 `值`

`Ownvalues`:

```mathematica
x = 7;
OwnValues[x]
Out[2]= {HoldPattern[x] :> 7}
```

+ `Downvalues`:

```mathematica
a[1] = 17;
DownValues[a]
Out[2]= {HoldPattern[a[1]] :> 17}
```

+ `Subvalues`:

```mathematica
derivative[1][f] = fg;
SubValues[derivative]
Out[2]= {HoldPattern[derivative[1][f]] :> fg}
```

+ `Upvalues`:

```mathematica
sq /: area[sq] = s^2;
UpValues[sq]
Out[2]= {HoldPattern[area[sq]] :> s^2}
```

+ 默认值和选项:

```mathematica
Default[f] = 0;
Options[f] = {opt1 -> def1, opt2 -> def2};
DefaultValues[f]
Out[3]= {HoldPattern[Default[f]] :> 0, HoldPattern[Options[f]] :> {opt1 -> def1, opt2 -> def2}}
```

+ `数值值`:

```mathematica
N[const] = Product[1 - 2^-i, {i, 2, 10}];
NValues[const]
Out[2]= {HoldPattern[N[const, {MachinePrecision, MachinePrecision}]] :>  10414855105976475/18014398509481984}
N[const]
Out[3]= 0.578141
```

+ `Format values`:

```mathematica
Format[a] = \[Alpha];
FormatValues[a]
Out[2]={HoldPattern[\[Alpha]]:>\[Alpha],HoldPattern[MakeBoxes[\[Alpha],FormatType_]]:>Format[\[Alpha],FormatType]}

a
Out[3]= \[Alpha]
```

`Attributes` 的定义与 `f` 相关联, 而不是与 `Attributes` 关联:

```mathematica
Attributes[f] = HoldAll
Out[1]= HoldAll

Definition[f]
Out[2]= Definition[f]
```

### 推广和延伸

将 `x` 和 `y` 设为相同的值:

```mathematica
x = y = 77
{x, y}
```

设置 `x` 和 `y` 为不同的值:

```mathematica
{x, y} = {a, b}
{a, b}
```

交换值:

```mathematica
{x, y} = {y, x}
{b, a}
```

设置 `稀疏数组` 的部分:

```mathematica
v = SparseArray[{1 -> 1, 4 -> 4}]

v[[2]] = 3
```

`v` 仍然是 `稀疏数组`, 只是其第二部分被改变了:

```mathematica
{v, Normal[v]}
```

将 `函数` 的多个 `返回值` 分配给各个变量:

```mathematica
{time, res} = Timing[N[Catalan, 100000]];

time
Out[2]= 5.703

N[res]
Out[3]= 0.915966
```

若对特定头部的 `模式` 作定义, 则定义与该 `头部` 相关:

```mathematica
_a = \[Alpha]
Out[1]= \[Alpha]

Definition[a]

Out[2]= Definition[a]

{a, a[1]}

Out[3]= {a, \[Alpha]}
```

### 应用

计算两个数字的 `GCD`:

```mathematica
{a, b} = {27, 6};
While[b != 0, {a, b} = {b, Mod[a, b]}];
a

Out[1]= 3
```

寻找 `固定点`:

```mathematica
x = 1.0;
While[Cos[x] != x, x = Cos[x]];
x

Out[1]= 0.739085
```

用 `牛顿法` 计算 `Sqrt[2]`:

```mathematica
x = 1.0;
Do[x = (x + 2/x)/2, {5}];
x

Out[1]= 1.41421
```

将 `变量` 作为 `复杂表达式` 的缩写, 使用一次以上:

```mathematica
upperTriangularLinearSolve[U_, v_] := Module[{x, m, n},
    {m, n} = Dimensions[U]; x = Range[n];
    Do[x[[i]] = (v[[i]] - Sum[U[[i,j]]*x[[j]], {j, i + 1, n}])/U[[i,i]], {i, n, 1, -1}]; x]

upperTriangularLinearSolve[{{1, 2}, {0, 3}}, {1, 2} ]
Out[2]= {-(1/3), 2/3}
```

`线性方程组`中 `反置换`(backsubstitution) 的 向量实现:

```mathematica
upperTriangularLinearSolve[ U_, v_ ] := Module[ {x, m, n},
    {m, n} = Dimensions[U]; x = Range[n];
    Do[x[[i]] = (v[[i]] - U[[i, i + 1 ;; n]] . x[[i + 1 ;; n]])/U[[i, i]], {i, n, 1, -1}];
    x];

upperTriangularLinearSolve[{{1, 2}, {0, 3}}, {1, 2} ]
Out[2]= {-(1/3), 2/3}
```

### 性质和关系

`即时定义` 的 `右侧` 在定义时被计算:

```mathematica
x = Random[];
{x, x, x}

Out[2]= {0.446837, 0.446837, 0.446837}
```

`延迟定义` 的右侧在每次使用定义时都会被计算:

```mathematica
y := Random[];
{y, y, y}

Out[4]= {0.72466, 0.646934, 0.872933}
```

定义的 `左参数` 在定义之前就被计算了:

```mathematica
x = 5;
f[x] = 17
Out[2]= 17

Definition[f]
Out[3]= Definition[f]
```

`相同左值` 的定义会覆盖先前的 `定义`:

```mathematica
a = 5;
a = 6;
a

Out[3]= 6
```

使用 `即时` 和 `延迟赋值` 为特殊和一般情况下做定义:

```mathematica
fact[1] = 1;
fact[n_] := n fact[n - 1]

fact[10]
Out[2]= 3628800
```

`更具体` 的定义被放在 `更一般` 的定义前面:

```mathematica
fact[n_] := n fact[n - 1];
fact[1] = 1;

Definition[fact]
Out[2]= Definition[fact]

fact[10]
Out[3]= 3628800
```

如果有必要, `模式变量` 在 嵌套的 作用域内 会被重命名:

```mathematica
makedef[z_] := (f[x_] = z^2;)
makedef[x]

Definition[f]
Out[3]= f[x$_] = x^2 (*变量x 被重命名为 x$*)

f[5]
Out[4]= x^2
```

`Module` 引入了 `新符号`, 区别于 `全局符号`:

```mathematica
x = 5;
Module[{x}, x = 17; x]
Out[2]= 17

x
Out[3]= 5
```

+ `Definition` 打印与符号相关的定义:

```mathematica
a = Sqrt[2]
Sqrt[2]

Definition[a]
Out[2]= a = Sqrt[2]
```

`Information` 打印关于 `符号` 的各种信息, 包括任何定义:

```mathematica
?a
Cell$$2274`a
a=Sqrt[2]
```

+ `OwnValues` 返回与定义的 `downvalues` 对应的 `规则列表`:

```mathematica
OwnValues[a]

Out[4]= {HoldPattern[a] :> Sqrt[2]}
```

+ 使用 `Unset` (`=.`)清除具有特定左手边的定义:

```mathematica
fact[1] = 1;
fact[n_] := n fact[n - 1]

fact[1] =.
Definition[fact]

Out[3]= Definition[fact]
```

清除所有定义:

```mathematica
Clear[f]
Definition[fact]

Null
```

### 可能的问题

若存在 `全局变量` , `模式变量` 可能会出现意外的行为:

```mathematica
x = 5;
f[x_] = x^2;
f[2]

Out[3]= 25
```

`延迟赋值` 的行为符合预期:

```mathematica
g[x_] := x^2
g[2]

Out[5]= 4
```

Runaway 定义:

```mathematica
x = x + 1
$RecursionLimit::reclim: Recursion depth of 256 exceeded.
Out[1]= 255 + Hold[1 + x]

y = 5;
y = y + 1
Out[2]= 6
```

+ `模式变量` 不是 `symbols`; 你通常不能向它们赋值:

```mathematica
wrong[x_] := (x = x^2)
wrong[5]

During evaluation of In[2]:= Set::setraw: Cannot assign to raw object 5.

Out[2]= 25
```

为此要使用 `局部变量`:

```mathematica
right[x0_] := Module[{x}, x = x0^2]
right[5]

Out[4]= 25
```

使用 `upvalues` 来分配给下标的变量:

```mathematica
a /: Subscript[a, 1] = 5;
FullForm[UpValues[a]]

List[RuleDelayed[HoldPattern[Subscript[a,1]],5]]
```

`被下标的变量` 与 `带索引的变量` 不同:

```mathematica
a[1] = 6;
FullForm[DownValues[a]]

List[RuleDelayed[HoldPattern[a[1]],6]]
```

### 巧妙范例

计算两个数字的 `算术几何平均数` [更多信息]:

```mathematica
{x, y} = N[{1, 2}, 20];
While[x != y, {x, y} = {(x + y)/2, Sqrt[x y]}]; x

Out[1]= 1.4567910310469068692
```
