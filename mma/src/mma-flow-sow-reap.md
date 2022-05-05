# Sow,Reap

tutorial/CollectingExpressionsDuringEvaluation

在 `计算过程中` 收集表达式, 使用 `Sow Reap` 函数. `Reap`收割, 收获

+ 获得非末尾表达式的值:

    ```mathematica
    Extract[{2, 1, 1}]@Reap[Sow@expr; expr2]
    ```

## Reap 语法

```mathematica
Reap[expr] ; 给出 expr 的值, 以及所有在其计算过程中被 Sow 封装的表达式.
使用 Sow[e] 或 Sow[e, tag_i] 播种的具有不同标记的表达式, 将收集到不同的列表中.

Reap[expr,patt]; 只收集 ` sown tag` 与 patt 匹配的表达式.

Reap[expr,{ patt1, patt2, ...}]; 将与 patt_i 相关的表达式放在单独的列表中.

Reap[expr,patt,f]; 返回
    {expr, {
            f[tag1, {e_11, e_12, ...}], ...
            }} .
```

最后一种 `重载` 的结果包含两个元素.
`首元素` 是原表达式的值, `次元素` 是 `Reap` 的收获, 为一列表.
`Reap` 列表的每个元素是 `f[tag, {...}]` 的形式.
对于多次出现的 `tag`, 就多次捕获.

`Reap[...Sow...]` 的执行方式是:

+ 先根据 `Reap` 的 `patt` 参数设置收集槽, 对所有 `patt` 迭代:
    + 对表达式中出现的所有 `Sow` 迭代:
    + 对每个 `Sow` 中所有的 `tag` 迭代, `tag` 匹配 `patt` 就收集到槽中,
同一表达式具有多个 `tag`, 匹配成功`x`次, 就重复收集`x`次.
+ 应用 `Reap` 的自定义 `f` 参数, 对于每个独立的 `tag`, 结果是 `f{tag, {tag 对应收获}}`

### 细节

+ `Sow` 和 `Reap` 提供了方便的方法, 来累积计算中的 `中间结果`列表.
+ `Reap` 积累表达式时, 按照 `Sow` 作用于表达式的顺序.
+ 使用特定 `tag` 播种的表达式, 被最内层匹配的 Reap 收集, 即要求 `Reap` 中的模式与 `tag` 可以匹配成功.
+ `Reap[expr]` 等同于 `Reap[expr,_]`.
+ `Reap` 具有 `HoldFirst` 属性.

### 例子

+ 计算一连串的表达式, `收割` 那些 `被播种` 的表达式:

    ```mathematica
    Reap[Sow[a]; b; Sow[c]; Sow[d]; e]
    Out[1]={e, {{a, c, d}}}
    ```

+ 计算求和, "收集" 每一步 "播种" 的 `i^2` :

```mathematica
Reap[Sum[Sow[i^2] + 1, {i, 10}]]
Out[1]= {395, {{1, 4, 9, 16, 25, 36, 49, 64, 81, 100}}}
```

### 范围

为每个将被 `收割` 的 `tag` 做一个单独的子列表:

```mathematica
Reap[Sow[1, {x, x}]; Sow[2, y]; Sow[3, x], {x, x, y}]
Out[1]= {3, {{{1, 1, 3}}, {{1, 1, 3}}, {{2}}}}
```

计算 `整数` 出现的次数, 分离出 `负数` 部分:

```mathematica
Reap[Sow[1,
  (*随即 生成100 个标签, 每个匹配成功的标签, 都会导致 1 被收集一次*)
  RandomInteger[{-9, 9}, 100]],
 (*将标签按照是否为 负数, 分别收集到两个列表中*)
 {_?Negative, _?NonNegative},
 (*#1 是 tag, #2是 Reap 的每个 patt 收集到的列表*)
 #1 -> Total[#2] &
 ]

Out[1]={1, {{-1 -> 4,...}}}
```

### 推广和延伸

将 `f` 应用到每个独立的 `tag` 和对应的值列表上:

```mathematica
Reap[Sow[1, {x, x}]; Sow[2, y]; Sow[3, x], _, f]
Out[1]= {3, {f[x, {1, 1, 3}], f[y, {2}]}}

Reap[Sow[1, {x, x}]; Sow[2, y]; Sow[3, x], _, Rule]
Out[2]= {3, {x -> {1, 1, 3}, y -> {2}}}
```

### 应用

+ 找到 `Plot` 采样的值的列表, 即 `x` 坐标点的列表

```mathematica
Reap[Plot[Sin[x], {x, 0, 10}, EvaluationMonitor :> Sow[x]];] // Short
```

+ 从列表中删除重复的内容, 保持原来的顺序:

```mathematica
unsortedUnion[x_] := Reap[
   (*把含有重复元素的列表当 tag*)
   Sow[1, x], _,
   (*#1 为独立的tag, 即收集tag*)
   #1 &
   ][[2]](*1元素是尾端返回值,2元素是自定义返回列表*)

unsortedUnion[{b, b, c, a, c, a, b, d}]
{b, c, a, d}
```

+ 这就像不带排序的 `Union`:

```mathematica
Union[{b, b, c, a, c, a, b, d}]
Out[3]= {a, b, c, d}
```

### 性质和关系

+ 如果没有 `Sow` 任何表达式, `Reap` 就返回 `空列表`, 作为它所收获的表达式.

```mathematica
Reap[x]

Out[6]= {x, {}}
```

+ `Reap` 准确地按照 `播种` 的顺序, 收割表达式:

```mathematica
Reap[MapAll[Sow, (a + b) (c + x^2)];]

Out[1]= {Null, {{a, b, a + b, c, x, 2, x^2, c + x^2, (a + b) (c + x^2)}}}
```

+ `Reap` 结果列表的顺序为, 优先给出第一个遇到的 `标签` 对应的列表:

```mathematica
Reap[Sow[1, y]; Sow[2, x]; Sow[3, y]]
(* 这里先给出 tag y 对应的 结果*)
Out[1]= {3, {{1, 3}, {2}}}

Reap[Sow[1, y]; Sow[2, x]; Sow[3, y], _, Rule]
(*使用 Rule 可以更清楚的看出来*)
Out[2]= {3, {y -> {1, 3}, x -> {2}}}
```

## Sow

```mathematica
Sow[e]; 指定 `e` 应该由 `最近层` 的 `Reap` 收集(nearest enclosing).

Sow[e,tag]; 指定 `e` 应该被 `模式` 与 `tag` 相匹配的 `最近层` 的 `Reap` 所收集.

Sow[e,{tag1, tag2, ...}];  对于每个 `Reap` 模式, `tag_i` 每次和模式匹配成功, `e` 都应该被收集一次.
```

### 细节

+ `Sow[e, ...]` 返回 `e`.
+ 重用同一 `tag_i`, 可以使 `表达式` 被多次收集入 `Reap` 返回的列表中.
+ `Sow[e]` 相当于 `Sow[e,None]`.
+ `Sow[e,{{tag}}]` 播种带有 `{tag}` 标签的表达式. 相当于 `转义` 操作(escape)

### 例子

计算表达式序列, "播种" 一些表达式, 然后用 `Reap` 收集:

```mathematica
In[1]:= Reap[Sow[a]; b; Sow[c]; Sow[d]; e]

Out[1]= {e, {{a, c, d}}}
```

+ 计算求和, 在每一步 收割 `i^2`:

```mathematica
Reap[Sum[Sow[i^2] + 1, {i, 10}]]
```

+ 用不同的标签 "播种 "表达式, 然后收集到单独的列表中:

```mathematica
Reap[Sow[1, x]; Sow[2, y]; Sow[3, x]; Sow[4, y]]
Out[1]= {4, {{1, 3}, {2, 4}}}
```

+ 只 `Reap` 带有播种标签 `x` 的表达式:

```mathematica
Reap[Sow[1, x]; Sow[2, y]; Sow[3, x]; Sow[4, y], x]

Out[1]= {4, {{1, 3}}}
```

### 范围

+ `Sow` 可以在计算的任何地方使用:

```mathematica
Reap[MapAll[If[PrimeQ[#], Sow[#]] &, Integrate[1/(x^5 - 1), x]];]

Out[1]= {Null, {{-2, 2, 5, 5, 2, 5, 5, 5, -2, -2, 5, -2, 5, 5, 5, 5, 2, 5, 5, 2}}}
```

+ 它在函数内也仍工作:

```mathematica
f[e_] := If[PrimeQ[e], Sow[e]]
Reap[MapAll[f, Integrate[1/(x^5 - 1), x]];]

Out[3]= {Null, {{-2, 2, 5, 5, 2, 5, 5, 5, -2, -2, 5, -2, 5, 5, 5, 5, 2, 5, 5, 2}}}
```

+ 表达式可以带多个 "播种"标签:

```mathematica
Reap[Sow[1, x]; Sow[2, {x, y}]; Sow[3, y]]

Out[1]= {3, {{1, 2}, {2, 3}}}
```

+ 如果 `标签` 被重复, 则表达式被重复播种, 相当于会被重复收割:

```mathematica
Reap[Sow[1, {x, x, x}]; Sow[2, {x, y}]; Sow[3, y]]

Out[1]= {3, {{1, 1, 1, 2}, {2, 3}}}
```

+ `Reap` 中的 `模式` 可以用来指定要收集哪些 `标签`:

```mathematica
Reap[
    Sow[1, x];
    Sow[2, {x, y}];
    Sow[3, {y, z}];
    Sow[4, {x, y, z}], x | y]

Out[1]= {4, {{1, 2, 4}, {2, 3, 4}}}
```

+ `默认` 是模式 `_`, 这样所有的东西都被收集起来:

```mathematica
Reap[Sow[1, x]; Sow[2, {x, y}]; Sow[3, {y, z}];Sow[4, {x, y, z}]]

Out[2]= {4, {{1, 2, 4}, {2, 3, 4}, {3, 4}}}
```

+ `tags` 可以有任何形式; 在这里它们是 `True` 和 `False`:

```mathematica
Reap[Sow[#, # > 0] & /@ {1, -1, 2, -3, 1, 4, 5};]

Out[1]= {Null, {{1, 2, 1, 4, 5}, {-1, -3}}}

Reap[Sow[#, # > 0] & /@ {1, -1, 2, -3, 1, 4, 5};, True]

Out[2]= {Null, {{1, 2, 1, 4, 5}}}
```

### 应用

将计算 `FindRoot` 的所有步骤列成清单:

```mathematica
Reap[FindRoot[Cos[x] == x, {x, 1}, StepMonitor :> Sow[x]]]

Out[1]= {{x -> 0.739085}, {{0.750364, 0.739113, 0.739085, 0.739085}}}
```

计算迭代的 `映射`, 保留低于 `1/100` 的迭代:

```mathematica
Reap[Nest[(If[# < 1/100, Sow[#]]; 4 # (1 - #)) &, 0.2, 100]]

Out[1]= {0.875591, {{0.000246305, 0.000984976, 0.00393603, 0.00631451,0.00264312}}}
```

寻找 `i^2 mod 10` 的 `等价类`:

```mathematica
Reap[Do[Sow[i, Mod[i^2, 10]], {i, 20}]]

Out[1]= {Null, {{1, 9, 11, 19}, {2, 8, 12, 18}, {3, 7, 13, 17}, {4, 6,14, 16}, {5, 15}, {10, 20}}}
```

### 性质和关系

`播种的表达` 由最内层的包裹收割(innermost enclosing Reap):

```mathematica
Reap[Reap[Sow[2^20]; Sow[2^30]]]

Out[1]= {{1073741824, {{1048576, 1073741824}}}, {}}
```
