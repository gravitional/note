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

## 性质

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
