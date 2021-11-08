# 模式匹配和替换

`模式` tutorial/PatternsAndTransformationRules
`常用表达式的模式`  tutorial/PatternsForSomeCommonTypesOfExpression
`模式与匹配 引言`: tutorial/OptionalAndDefaultArguments

## 模式的形式

`Pattern`具有属性`HoldFirst`.

+ `sym:obj` 或者 `Pattern[sym,obj]`; 表示模式对象`obj`, 被分配名称`sym`.
例如`type:1|2`表示名为`type`的模式, 取值为`1`或者`2`.
    + 名称`sym`必须是一个符号.
    + 对象`obj`可以是任何模式对象.
    + 当被用于替换规则时, 在右边出现的任何`sym`, 都替换以左边匹配到的表达式.
    + 操作符`:`的优先级相对较低. 因此表达式`x:_+_`被解释为`x:(_+_)`, 而不是`(x:_)+_`.
    + 形式`s_`等同于`s:_`. 同样, `s_h` 等同于 `s:_h`; `s__`等同于`s:__`, 以此类推.

***
表达式 `F[a,b,c...]`

+ `_, Blank[]`, 有且只有一个的表达式序列.
+ `Blank[h]`, 表示头部为`h`的模式,
+ `_h` 中`h`本身不能再包含模式对象.
+ `__, BlankSequence[]`, 一个或多个表达式序列
+ `f[x_, x_]` 表示函数中两个相同的自变量, `f[x_,y_]`表示函数中任意两个变量, 可以相同, 也可以不同.

```mathematica
s : _ | __ // FullForm
Pattern[s,Alternatives[Blank[],BlankSequence[]]]
```

头部也可以是表达式,
所以`_`也就是`Blank[]`可以指带`f[a,b,c...][x,y,z...]`

```mathematica
MatchQ[f[a,b,c][x,y,z],x_]
True
```

## 限制模式

`对模式施加限制` tutorial/PuttingConstraintsOnPatterns
`限制模式` tutorial/PuttingConstraintsOnPatterns

Wolfram 语言中提供了对模式进行限制的一般方法, 通过在模式后面加 `/;condition` 来实现. 此运算符 `/;` 可读作"斜杠分号", "每当"或"只要", 其作用是当所指定的 `condition` 值为 `True` 时模式才能使用.

## Condition, 条件

+ `patt/;test` ; 是一个模式, 只有在`test`的计算结果为`True`时才进行匹配.
+ `lhs:>rhs/;test` ; 表示一个规则, 只有在`test`的计算结果为`True`时才使用.
+ `lhs:=rhs/;test` ; 表示一个定义, 只有在`test`产生`True` 时才会使用.

+ `test`中使用的所有模式变量也必须出现在`patt`中
+ `lhs:=Module[{vars},rhs/;test]` 允许在`test`和`rhs`之间共享局部变量. 你也可以在`Block`和`With`中使用同样的结构.

对于 `Test` 返回的其他非 Boolean 值, 一律认为不匹配. 例如:

```mathematica
aa /. (x_ /; 1 -> 1)
Out: aa
```

## PatternTest

```mathematica
PatternTest[p,test] 或者  p ? Test
是一个模式对象, 代表任何与`p`相匹配的表达式, 并且要满足在`p`上应用`test`给出`True`.
```

### 细节

+ `test[pval]` 的结果如果不是`True`, 就算作失败, 对于任何其他结果.
+ 操作符 `?` 的优先级很高, 甚至优于`@`. 因此 `_^_?t` 是`_^(_?t)`, 而不是`(_^_)?t`.
+ 在 `__?test` 这样的形式中, 当应用`test`时, 由 `__` 匹配到的序列中的每个元素, 必须都产生 `True`.
+ `PatternTest`有属性`HoldRest`, 也就是不计算 `test`

### 例子

```mathematica
f[x_] := Condition[ppp[x], x > 0]
```

当`x > 0`为真的时候, 才进行函数的定义.

+ 判断是否为数值对象:

```mathematica
NumericQ[Sin[Sqrt[2]]]
```

+ 找到满足 `NumberQ` 测试的任何表达式 :

```mathematica
Cases[{1, 2, 3.5, x, y, 4}, _?NumberQ]
Out[1]= {1, 2, 3.5, 4}.
```

+ 测试一个列表是否只有`正数`元素:

```mathematica
MatchQ[{1, E, Pi}, {__?Positive}]
Out[1]= True

MatchQ[{1, I, 0}, {__?Positive}]
Out[2]= False
```

+ 创建一个只适用于`数字参数`的定义:

```mathematica
f[x_?NumericQ] := NIntegrate[Sin[t^3], {t, 0, x}]
f[2]
Out[2]= 0.451948

f[(1 + Sqrt[2])/5]
Out[3]=0.0135768

f[a]
Out[4]=f[a]
```

### 范围

+ 用`零`来代替`负数`:

```mathematica
{3, -5, 2, 7, -6, 3} /. _?Negative :> 0
Out[1]= {3, 0, 2, 7, 0, 3}.
```

+ 在一个列表中寻找能被`7`整除的元素:

```mathematica
Cases[Range[0, 70], _?(Divisible[#, 7] &)]
Out[1]= {0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70}
```

能同时被`5`和`7`整除的元素:

```mathematica
Cases[Range[0, 350], _?(Divisible[#, 7] && Divisible[#, 5] &)]
Out[2]= {0, 35, 70, 105, 140, 175, 210, 245, 280, 315, 350}.
```

可以被5或7整除的元素:

```mathematica
Cases[Range[0, 35], _?(Divisible[#, 7] || Divisible[#, 5] &)]
Out[3]= {0, 5, 7, 10, 14, 15, 20, 21, 25, 28, 30, 35}.
```

+ 创建一个函数, 只在给定`非负数`和`素数`时进行求值:

```mathematica
f[n_?NonNegative, p_?PrimeQ] := n^p
f[0, 3]
Out[2]=0

f[2, 4]
Out[3]= f[2, 4]

f[-1, 3]
Out[4]= f[-1, 3]
```

+ 创建一个只对`负数`进行计算的函数:

```mathematica
f[p_?(Negative[#] && PrimeQ[#] &)] := p^p
f[-2]
Out[2]=1/4

f[2]
输出[3]=f[2]

f[-4]
Out[4]=f[-4]
```

+ 在一个复杂的模式上使用 `PatternTest`:

```mathematica
MatchQ[{{a, b}, {c, d}}, {_, _}?MatrixQ]
Out[1]= True

MatchQ[{a, b}, {_, _}?MatrixQ]
Out[2]= False
```

+ 搜索数字, 不允许表达式进行计算:

```mathematica
Cases[Hold[Print[5], Sin, 2 + 2, 7, Sqrt[2]], _?(Function[{n}, NumberQ[Unevaluated@n], HoldAll])]
输出[1]= {7}
```

只使用 `NumberQ` 将允许每个元素都被计算:

```mathematica
Cases[Hold[Print[5], Sin, 2 + 2, 7, Sqrt[2]], _?NumberQ]

5
Out:= {4, 7}
```

### 性质和联系

+ `PatternTest` 将 `测试函数` 应用于`模式`, 这些模式不需要有`名字`;

```mathematica
Cases[{{a, b}, {1, 2, 3}, {{d, 6}, {d, 10}}}, {_, _}?VectorQ]
{{a, b}}
```

+ `Condition` 使用 `模式` 的某些命名的部分, 计算一个`布尔`表达式;

```mathematica
Cases[{{a, b}, {1, 2, 3}, {{d, 6}, {d, 10}}}, {x_,  y_} /; ! ListQ[x] && ! ListQ[y]]
{{a, b}}
```

+ 使用 `Except` 来有效地否定 `PatternTest`:

```mathematica
Replace[{1, 7, "Hi", 3, Indeterminate}, Except[_?NumericQ] :> 0, 1] (* 出了数字, 都换成 0*)
{1, 7, 0, 3, 0}
```

## 可能的问题

`PatternTest` 对潜在的匹配进行计算, 即便表达式处于`held`中.

```mathematica
MatchQ[Hold[2 + 3], Hold[_?IntegerQ]] (*这里仍然能匹配到*)
True
```

根据通常的计算流程, 表达式在被 `test` 函数检测之前, 可能会被计算过.

```mathematica
MatchQ[Hold[2 + 3], Hold[_?(IntegerQ[Unevaluated@#] &)]]
True
```

+ 使用 `Function` 的三参数形式, 指定 `HoldAll` 属性; 或使用 `Condition` 来编写模式, 来防止参数被计算:

```mathematica
MatchQ[Hold[2 + 3],  Hold[_?(Function[{n}, IntegerQ[Unevaluated@n], HoldAll])]]
False
MatchQ[Hold[2 + 3], Hold[n_] /; IntegerQ[Unevaluated@n]]
False
```

## 重复的模式

`重复模式` tutorial/Introduction-Patterns
`expr..` 重复一次或多次的模式或表达式
`expr...` 重复零次或多次的模式或表达式

`规则与模式`:
guide/RulesAndPatterns
guide/Patterns

Wolfram 语言符号编程范式的核心, 是任意符号模式转换规则的概念. Wolfram 语言的模式语言方便的描述了一系列各种类型的表达式, 让程序变得易读, 简洁且高效.

## 无序模式

```mathematica
MatchQ[{2, 1}, {OrderlessPatternSequence[1, 2]}]
```

## 应用替换规则

+ `Replace`
+ `ReplaceAll`
+ `ReplaceRepeated`
+ `ReplacePart`
+ `Dispatch[{lhs1->rhs1,lhs2->rhs2}]`: 不影响替换结果, 但可以加速替换.

`mma`中的`表达式`都可以表示成`树`. 不同的函数具体的操作流程不同.

+ `ReplaceAll` 检查表达式的每一项, 尝试使用给出的所有规则, 然后继续遍历下一项.
+ 使用`第一个`匹配的规则, 对作用过的项, 不再使用后续的替换规则, 也不再检查它的子项.
+ 对给出的替换规则, 只进行一次上面的遍历过程.
+ 如果没有规则适用, 则返回原来的表达式.
+ `ReplaceAll[rules][expr]` 等价于 `ReplaceAll[expr,rules]`.

解释的更详细点就是:

+ 在树的根部--第`0`层, 也就是从整个表达式开始是否有规则可以使用.
+ 然后去往第一层的第一个... 尝试每个规则, 如果可以替换就替换, 然后第二个, 遍历完就深入到下一层等等.
+ 对于某个子表达式, `ReplaceAll`使用可用的第一个规则, 然后跳过这个子集, 不再尝试更多的规则.
也就是`ReplaceAll`替换它可以替换的最大子表达式, 然后继续下一项.
+ `ReplaceAll` 仅对一个表达式应用特定规则列表一次.
如果替换规则没有嵌套, 应该可以保证完全替换. 否则应该使用`ReplaceRepeated`.

```mathematica
{x, x^2} /. {x^2 -> 2, x -> 1} (* 结果是 {1,2}, 使用 x^2 -> 2 的规则 *)
{x, x^2} /. {x -> 1, x^2 -> 2} (* 结果还是 {1,2}, 与规则列表给出的次序无关*)
{x, x^2, y} /. {x^2 -> y, x -> 3, y -> 1}  (* 结果是 {3, y, 1}, 只应用一次规则列表*)
```

+ `ReplaceRepeated`重复应用`ReplaceAll`,  直到表达式不再变化为止.
+ `Replace` with level spec `All` 将会尝试替换每个子表达式 exactly 一次.
+ `ReplacePart`: 替换某些位置上的子表达式.

## Dispatch

```mathematica
Dispatch[{ lhs1-> rhs1, lhs2-> rhs2,    ... }]
生成一个规则列表的`优化分发表`表示. (optimized dispatch table representation)
`Dispatch` 产生的对象可以用来作为 `expr/.rules` 中的规则
```

### 详细

+ 使用 `Dispatch` 永远不会影响获得的结果, 但可能会使长的`规则列表`的应用更快.
+ 当你计算像 `expr/.rules` 这样的表达式时, `规则列表`通常是按`顺序`被扫描的.
像 `a[1]->a1` 和 `a[2]->a2` 这样的规则不可能同时被应用, 所以不需要都被显式地扫描.
`Dispatch` 生成一个`dispatch table`, 该`表`使用`hash codes`来指定,
针对特定的输入表达式, 哪些`规则集`实际上需要进行`扫描`.
+ 在合适的情况, 用`=`和`:=`赋值产生的`规则列表`, 会使用`dispatch table`自动优化.
+ `Dispatch` 被 `AtomQ` 等函数视为 `raw object`, 以及用于模式匹配的时候.
+ `Normal` 将 `Dispatch` 转换为规则的列表.

### 例子

从`规则列表`中创建`dispatch table`:

```mathematica
rules = {a -> b, b -> c, c -> a, d -> e, e -> d};
dispatch = Dispatch[rules]

(*替换的结果是一样的:*)

{a, b, c, d, e} /. rules
{a, b, c, d, e} /. dispatch
```

+ 对于一个长的规则列表, 调度可能要快得多:

```mathematica
Short[rules = Table[x[i] -> RandomInteger[{1, i}], {i, 10000}]]
dispatch = Dispatch[rules]

Timing[Table[x[i] /. rules, {i, 10000}];]
Out[3]= {5.47537, Null}

Timing[Table[x[i] /. dispatch, {i, 10000}];]
Out[4]={0.013203, Null}
```

### 范围

使用 `Dispatch` 与基于`模式`的规则

```mathematica
disp = Dispatch[{1 -> a, 2 -> b, c_?NumericQ -> \[Infinity]}]

{1, 2, 3, Pi, Sin[x], Cos[3]} /. disp
Out[2]= {a, b, \[Infinity], \[Infinity], Sin[x], \[Infinity]}
```

从关联中创建`dispatch`:

```mathematica
Dispatch[<|a -> b, c -> d|>]
Out[1]= Dispatch[{a -> b, c -> d}]
```

### 性质和联系

`Dispatch` 是 `AtomQ`:

```mathematica
AtomQ[Dispatch[{a -> b, b -> c, c -> a, d -> e, e -> d}]]
Out[1]= True
```

使用 `Normal` 将一个`dispatch`转换为正常的`规则列表`:

```mathematica
Normal[Dispatch[{a -> b, b -> c, c -> a, d -> e, e -> d}]]
Out[1]= {a -> b, b -> c, c -> a, d -> e, e -> d}]
```

## 字符串模式

`字符串处理`: `字符串模式` tutorial/WorkingWithStringPatterns

StringExpression objects can be used in many string manipulation functions, including StringReplace, StringCases, StringSplit, and StringMatchQ.

`MatchQ`不能识别`StringExpression`, 即字符串表达式. 需要用`StringMatchQ`才可以识别. 例如:

```mathematica
MatchQ["adbasdd", __ ~~ "dd"]
StringMatchQ["adbasdd", __ ~~ "dd"]
```

`DeleteCases`不能删除字符串模式,可能是由于它使用`MatchQ`做匹配.

+ `LetterQ`

比较字符串时忽略大小写, 这个功能在`StringMatchQ`的选项中:
`StringMatchQ["acggtATTCaagc", __ ~~ "aT" ~~ __, IgnoreCase -> True]`

在字符串匹配中, `x_`只匹配单个字符, `characters`,`StringExpression[pattern...]`可以用来表示模式序列, 有各种各样对应正则表达式功能的函数.

比如

+ `StartOfString`   字符串开头
+ `EndOfString`   字符串结尾
+ `StartOfLine`   行的开始
+ `EndOfLine`   行的结束
+ `WordBoundary`   boundary between word characters and others
+ `Except[WordBoundary]`   anywhere except a word boundary

***

`字符串模式`: tutorial/StringPatterns
`文本标准化` guide/TextNormalization
`StringDelete`
`StringReplace`:替换字符串
`字符串运算`: guide/StringOperations
`正则表达式`:RegularExpression

## 匹配表达式序列

### Cases

```mathematica
Cases[{e1,e2,...},pattern]
```

+ `Cases`的第一个参数不需要有Head`List`.
+ 当在一个关联上使用时, `Cases` 根据它们的`Value`挑出元素.
+ `Cases[expr,pattern:>rhs]` 只在找到模式时计算`rhs`.
+ `Cases[pattern][list]` 相当于 `Cases[list,pattern]`.
+ `Cases` 使用标准的层次指定.
    + `n`; `1`到`n`层
    + `Infinity` ; 第`1`层到最后一层
    + `{n}` ; 只包含第`n`层
    + `{n1,n2}` ;  `n1`层到`n2`层.
+ `Cases`的默认`levelspec`是`{1}`.
    + 正的层`n`包含`expr`中所有需要`n`个指标指定的子表达式.
    + 负数层`-n`包括`expr`中所有深度为`n`的部分.
    + `-1`层由数字, 符号和其他没有子部分的对象组成.
    + `0`层对应于整个表达式.
+ 通过选项设置`Heads->True`, `Cases`会查看表达式的头部和头部的子项.
+ `Cases`以`depth`优先的方式遍历`expr`的各个部分, 先访问叶子再访问根部.

```mathematica
Cases[{1, 1, f[a], 2, 3, y, f[8], 9, f[10]}, _Integer] (*第一层的头部被去掉*)
{1, 1, 2, 3, 9}
Cases[f[a, 2, g[1], 3, d], _Integer, Infinity]
{2,1,3}
```
