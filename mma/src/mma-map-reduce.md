# Map,Reduce,Thread

## Thread

```mathematica
Thread[f[args]] ; 将 f "threads" 到 args 中出现的任意列表上.
Thread[f[args],h]; 将 f thread 到 args 中出现的任何头部为 h 的对象.
Thread[f[args],h,n]; 将 f thread 到前 n 个 args 中头部为 h 的对象上.
```

`Thread`可以理解成, `Listable` 的函数, 即向量化算符, `+`, `-` , `*` , `/`, 在一般情形下的推广.
`Thread@f[args]` 让 `f` 也对 `args` 进行向量化运算;

+ 如果是`一元算符`, 则直接轮流作用一遍.
+ 如果是`多元算符`, 首先把`参数列表`对齐:
例如`Thread@f[list1,list2, x]`,  首先把单个元素`x`扩充到和其他两个列表一样长.
+ 还有神奇的自动过滤选项, `Thread[f[args],h,n] ` ;
将`f`逐项作用到, 前`n`项头部为`h`的参数. 也就是只对前`n`个`h[...]` thread, 其他的保持不变.

```mathematica
In[1]:= Thread[f[{a, b}, {r, s}, {u, v}, {x, y}], List, 2]
Out[1]= {f[a, r, {u, v}, {x, y}], f[b, s, {u, v}, {x, y}]}
```

### 可能的问题

+ `Thread` 会在 threading 之前计算整个表达式:

    ```mathematica
    Thread[D[{x, x y, x z}, {x, y, z}]]
    报错...
    ```

    `MapThread` 将函数和它的参数分开:

    ```mathematica
    MapThread[D, {{x, x y, x z}, {x, y, z}]
    Out[2]= {1, x, x}.
    ```

+ 抑制计算也有类似的效果:

    ```mathematica
    Thread[Unevaluated[D[{x, x y, x z}, {x, y, z}]].
    Out[3]= {1, x, x}.
    ```

### 应用

+ 建立一个规则列表:

    ```mathematica
    Thread[{a, b, c}-> {1, 2, 3}]
    Out[1]= {a -> 1, b -> 2, c -> 3}
    ```

+ 联系多项式方程中的系数:

    ```mathematica
    CoefficientList[#, x] & /@ (1 + 2 x + 3 x^2 == a + b x + c x^2)
    Out[1]= {1, 2, 3} == {a, b, c}
    Thread[%]
    Out[2]= {1 == a, 2 == b, 3 == c}
    ```

    或者, 使用 `SolveAlways`:

    ```mathematica
    SolveAlways[1 + 2 x + 3 x^2 == a + b x + c x^2, x]
    Out[3]= {{a -> 1, b -> 2, c -> 3}}.
    ```

+ 组成具有恒定第二元素的对儿:

    ```mathematica
    Thread[{{a, b, c}, 0}]
    Out[1]= {{a, 0}, {b, 0}, {c, 0}}
    ```

### 性质和关系

+ 具有 `Listable` 属性的函数会在列表上自动被 thread:

    ```mathematica
    Sqrt[{1, 2, 3, 4}]
    Out[1]= {1, Sqrt[2], Sqrt[3], 2}

    {1, 2, 3, 4} + 1
    Out[2]= {2, 3, 4, 5}

    {1, 2, 3, 4} + {a, b, c, d}
    Out[3]= {1 + a, 2 + b, 3 + c, 4 + d}.
    ```

+ `MapThread` 的工作原理与 `Thread` 类似, 但分别接受 `函数` 和 `参数` :

    ```mathematica
    MapThread[f, {{a, b, c}, {x, y, z}}]
    Out[1]= {f[a, x], f[b, y], f[c, z]}

    Thread[f[{a, b, c}, {x, y, z}]]
    输出[2]= {f[a, x], f[b, y], f[c, z]}
    ```

+ 用来 `thread` 的函数也可以是 `List`:

    ```mathematica
    Thread[{{a, b, c}, {x, y, z}}]
    Out[1]= {{a, x}, {b, y}, {c, z}}
    ```

    在这种情况下, 其结果与转置相同:

    ```mathematica
    Transpose[{{a, b, c}, {x, y, z}}]
    Out[2]= {{a, x}, {b, y}, {c, z}}}
    ```

### Thread 和 MapThread 的区别

`MapThread` 和 `Thread` 类似, 但是将 `函数f` 和 `参数` 分开处理:

```mathematica
In[1]:= MapThread[f,{{a,b,c},{x,y,z}}]
Out[1]= {f[a,x],f[b,y],f[c,z]}
```

## Map

guide/ApplyingFunctionsToLists

+ `Map[f,expr]` or `f/@expr`;  将`f` 应用到`expr`第一层的每个元素上.
+ `MapAll[f,expr]` or `f//@expr` ; 将`f` 应用到`expr` 的每个子表达式上.
+ `MapAt[f,expr,n]` ; 将`f`应用到`expr` 位置`n` 的元素上. 如果`n`是负数,则从尾巴开始数.

```mathematica
MapAt[f, {a, b, c, d}, 2]
{a, f[b], c, d}
```

+ `MapIndexed[f,expr]`; 将`f`应用到`expr`, 将元素的索引(index)当作`f`的第二个参数.

```mathematica
MapIndexed[f, {a, b, c, d}]
{f[a, {1}], f[b, {2}], f[c, {3}], f[d, {4}]}
```

+ `BlockMap[f,list,n] ` ; 将`f` 应用到`expr`的不重叠划分上,每个划分长度为`n`

```mathematica
BlockMap[f, Range[10], 2]
{f[{1, 2}], f[{3, 4}], f[{5, 6}], f[{7, 8}], f[{9, 10}]}
```

+ `SubsetMap[f,{e1,e2,...},{i,j,...}]` ;  替换列表`{e1,e2,...}` 中的指定元素 `ei, ej`, 使用`f[{ei,ej}]`的结果替换.

```mathematica
SubsetMap[Reverse, {x1, x2, x3, x4, x5, x6}, {2, 4}]
{x1, x4, x3, x2, x5, x6}
```

## Array

`ArrayFilter[f,array,r]` ; 将`f` 应用到 `arry` 中 所有范围是`r`的 block 上.

```mathematica
ArrayFilter[f, Range[10], 1]
{f[{1, 1, 2}], f[{1, 2, 3}], f[{2, 3, 4}], f[{3, 4, 5}], f[{4, 5, 6}],  ...  f[{9, 10, 10}]}
```

`ArrayReduce[f,array,n]`; 使用`f` 化简 `array` 的维度`n`.

## Nest

+ `Nest[f,expr,n]` ; 给出`f`应用到`expr`达`n`次的结果.

```mathematica
In[1]:= Nest[f, x, 3]
Out[1]= f[f[f[x]]]
```

+ `NestList[f,expr,n]`; 给出列表,从`0`次作用到`n`次.

```mathematica
In[1]:= NestList[f, x, 4]
Out[1]= {x, f[x], f[f[x]], f[f[f[x]]], f[f[f[f[x]]]]}
```

+ `NestWhileList[f,expr,test]` ;  给出重复作用`f` 得到的列表,知道`test`不再满足.
例如;重复除以`2`,直到结果不再是偶数.

```mathematica
In[1]:= NestWhileList[#/2 &, 123456, EvenQ]
Out[1]= {123456, 61728, 30864, 15432, 7716, 3858, 1929}
```

## Fold

guide/FunctionalProgramming

`FixedPointList[f,expr]` ; 不停作用`f`到`expr`上,直到结果不变, 给出结果的列表.

```mathematica
In[1]:= FixedPointList[1 + Floor[#/2] &, 1000]
Out[1]= {1000, 501, 251, 126, 64, 33, 17, 9, 5, 3, 2, 2}
```

+ `FoldList[f,x,{a,b,...}]` ; 给出 `{x,f[x,a],f[f[x,a],b],...}`, 也就是函数`f`, 带参数list的迭代列表.

```mathematica
FoldList[f, x, {a, b, c, d}]
{x, f[x, a], f[f[x, a], b], f[f[f[x, a], b], c],  f[f[f[f[x, a], b], c], d]}
```

`Fold[f,x,list]` ; 给出 `FoldList[f,x,list]` 的最后一个元素.

```mathematica
In[1]:= Fold[f, x, {a, b, c, d}]
Out[1]= f[f[f[f[x, a], b], c], d]
```

+ `FoldPairList[f, y0 ,{a1, a2,...}] ` ;  给出一系列`xi`, 通过将函数`f`不断应用到`{y[i-1], ai}`上,而函数`f`每一步返回`{xi, yi}`.
函数`f`要接受两个参数,并且要返回一对值`{x,y}`

大概过程如下,

```mathematica
{y0, a1} ->{x1,y1}
{y1, a2} ->{x2,y2}
{y2, a3} ->{x3,y3}
```

最后提取出所有的`xi`. 例如:

```mathematica
In[1]:= FoldPairList[{p[#1, #2], q[#1, #2]} &, u, {1, 2, 3, 4}]
Out[1]= {p[u, 1], p[q[u, 1], 2], p[q[q[u, 1], 2], 3],   p[q[q[q[u, 1], 2], 3], 4]}
(*  sdf *)
FoldPairList[List, u, {1, 2, 3, 4}, g]
{g[{u, 1}], g[{1, 2}], g[{2, 3}], g[{3, 4}]}
```

+ `FoldPair[f,y0 ,list]` ; 给出`FoldPairList[f,y0 ,list]`的最后一个元素.

```mathematica
In[1]:= FoldPair[{p[#1, #2], q[#1, #2]} &, u, {1, 2, 3}]
Out[1]= p[q[q[u, 1], 2], 3]
```

+ `SequenceFoldList[f,{x1, ..., xn},{ a1,a2, ...}]`;
给出`{x1, ..., xn,f[x1, ..., xn, a1],f[x2, ..., xn,f[x1, ..., xn, a1],a2], ...}`.

```mathematica
In[1]:= SequenceFoldList[f, {x, y}, {a, b, c}]
Out[1]= {
                                  x, y,
                              f[x, y, a],
                      f[y, f[x, y, a], b],
f[f[x, y, a], f[y, f[x, y, a], b], c]
}
```

+ `FoldWhile[f,x,{ a1,a2, ...},test] `; 返回 `f[ ... f[f[x, a1],a2] ...,ak]` 列表中,第一个`test`不为`True`的表达式
例子; 将 `5!` 除以连续的正整数,直到结果是一个非整数.

```mathematica
In[1]:= FoldWhile[Divide, 5!, Range[10], IntegerQ]
Out[1]= 1/6
```

## Construct

+ `Construct[f,x] `; 给出 `f[x]`.

```mathematica
In[1]:= Construct[f, x]
Out[1]= f[x]
```

+ `ComposeList[{f1,f2,...},x]` ; 给出 `{x,f1[x],f2[f1[x]],...}` 形式的列表. 也就是算符的复合列表

```mathematica
ComposeList[{a, b, c, d}, x]
{x, a[x], b[a[x]], c[b[a[x]]], d[c[b[a[x]]]]}
```

+ `OperatorApplied[f,n]`; 代表具有`n`个参数的算符`f`, `OperatorApplied[f,n][x1] ...[ xn]` 等价于 `f[x1, ..., xn]`.

```mathematica
In[1]:= OperatorApplied[f][x][y]
Out[1]= f[y, x]
In[1]:= OperatorApplied[f, 3][x][y][z]
```

+ `CurryApplied`

`CurryApplied[arity][f]` 等价于 `CurryApplied[f,arity]`.
`Curry` 一个两参数的函数:

```mathematica
In[1]:= CurryApplied[f, 2][x][y]
Out[1]= f[x, y]
```

使用算符形式的 `CurryApplied` 达到相同效果:

```mathematica
In[2]:= CurryApplied[2][f][x][y]
Out[2]= f[x, y]
```

## Take

+ `TakeList[list,{n1,n2, ...}] `; 给出相继去除元素`ni`得到的列表.

```
In[1]:= TakeList[{a, b, c, d, e, f, g, h}, {2, 3, 1}]
Out[1]= {{a, b}, {c, d, e}, {f}}
```

+ `TakeWhile[list,crit] `; 从列表开始相继取出元素,只要`crit[ei]`仍然为`True`.
`TakeWhile` 得到的元素是`Select`的子集:

```mathematica
In[1]:= data = {E, 8, a, b, 20, 1.4}
Out[1]= {E, 8, a, b, 20, 1.4}
In[2]:= TakeWhile[data, NumericQ]
Out[2]= {E, 8}
In[3]:= Select[data, NumericQ]
Out[3]= {E, 8, 20, 1.4}
```

+ `LengthWhile[list,crit] `;  给出从开头到`crit[ei]`保持为`True`的连续长度.

```mathematica
In[1]:= LengthWhile[{1, 1, 2, 11, 5, 8, 13, 21}, # < 10 &]
Out[1]= 3
```

+ `Position[expr,pattern]` ; 给出`expr`中匹配`pattern`的元素的位置.

```mathematica
In[1]:= Position[{a, b, a, a, b, c, b}, b]
Out[1]= {{2}, {5}, {7}}
```

+ `PositionIndex[list] `; 给出一个关联,指示`unique`元素的位置.

```mathematica
In[1]:= PositionIndex[{a, b, c, a, c, a}]
Out[1]= <|a -> {1, 4, 6}, b -> {2}, c -> {3, 5}|>
```

## 列表 List

tutorial/ListsOverview
tutorial/CollectingObjectsTogether

`函数作用于表达式的部分项` 和 `结构的操作` 将介绍如何用 `Map` 和 `Thread` 使一个函数分别地作用于列表的每个元素.

## 表达式的层次

`Level` `Map` `Scan` 等的区别:

+ 它们都可以使用标准的层次指定, `Scan` 和 `Map`效果一样, 但是`Scan`不会返回结果(不会建立一个新的表达式).
+ `Scan` 可以用过程化的控制: `Return`, `Throw`, `Catch`
+ 可以使用 `Shallow`函数来浏览表达式的结构,

  ```mathematica
  Shallow[First[fig`origin[["merge", "normal", "gm_charge"]]] //
    InputForm,
   {8, 10}
   ]
  ```

+ `Level` 也可以对子表达式应用函数`f`, 但将整个序列作为`f`的参数, 例如:

  ```mathematica
  Level[a + f[x, y^n], 3, ft]
  Level[a + f[x, y^n], {3}, ft]
  out:ft[a, x, y, n, y^n, f[x, y^n]]
  out: ft[y, n]
  ```

+ 过程控制函数 ; `Sow`, `Reap`, `Catch`,  `Throw`
+ 插入元素: `Insert[expr,elem,{i,j,...}]`: 把元素插入到 `expr` 的 `{i,j,...}` 位置.
+ 求数列公式有个函数 `FindSequenceFunction`

## MapThread,level 的区别

+ 在`mma`中, 其他编程语言的`Map`相当于`Map`以及一系列函数, `Reduce`相当于`Fold`等一系列函数.

+ 在`mma`中,表达式是`m[m,m,m,..]`的形式, 其中每个单元都可以是`m=Head[Sequence]`的形式 (可以无穷无尽地套娃),
所以除了函数作用到`一堆参数列表`上, 还有反向操作, 就是`一堆函数`作用到一个参数上, 相应的函数为:
`Through[p[f,g][args]]`: `f,g`被组织在结构`p[]`中, `p`可以是列表, 也可以是`Plus`, 等等.

+ 还有专门用来对`头部`作用的函数:`Operate`:`Operate[p,f[x,y]]`给出`p[f][x,y]`

```mathematica
In[2]:= MapThread[f, {{{a, b}, {c, d}}, {{u, v}, {s, t}}}]
Out[2]= {f[{a, b}, {u, v}], f[{c, d}, {s, t}]}
```

```mathematica
>In[3]:= MapThread[f, {{{a, b}, {c, d}}, {{u, v}, {s, t}}}, 2]
Out[3]= {{f[a, u], f[b, v]}, {f[c, s], f[d, t]}}
```

## Sow,Reap

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
