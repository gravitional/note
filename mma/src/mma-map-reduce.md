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

### my MapIndexed 

自己实现 `MapIndexed`:

```mathematica
mapI[fun_, expr_, index_?ListQ, 1] := MapThread[
  fun[#1, index~Join~#2] &,
  {
   expr, Array[List, Length@expr]
   }]

mapI[fun_, expr_, index_?ListQ, depth_?IntegerQ] := MapThread[
  mapI[fun, #1, index~Join~#2, depth - 1] &,
  {expr,
   Array[List, Length@expr]
   }]
mapI[f, Array[Plus, {2, 3, 4}], {}, 3]
```
