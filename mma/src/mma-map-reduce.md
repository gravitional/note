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
+ 求数列公式有个函数`FindSequenceFunction`

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
