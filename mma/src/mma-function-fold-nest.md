# Array,Nest,Fold,Construct,Take

## Array

```mathematica
ArrayFilter[f,array,r]; 将`f` 应用到 `arry` 中 所有范围是`r`的 block 上.
```

### 例子

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
