# 关联 Associations

+ `Associations` 具有属性 `HoldAllComplete`. 
直接对关联使用替换规则不会进行运算, 可以使用`Query`语法, 对关联的键和值进行替换.

+ `HoldAllComplete`: 不得以任何方式修改或查看函数的所有参数. 
不展开`Sequence`, 不移除`Unevaluated`, 不使用`UpValue`, 内部`Evaluate`无效

```mathematica
f[a] = 12; tea = <|a -> f[b]|>;
out: <|a -> f[a]|>
Query[All, ReplaceAll[b -> a]]@tea
out: <|a -> 12|>
```

传递的变量最好用 `Association` 形式.

### level

在结构相关的函数中, 关联被算作 **一个**  `level` , 而不是两个.

```bash
Level[<|a -> x, b -> y|>, {1}]
out: {x,y}
```

### 关联的子集

关联可以使用`assoc["key"]`的方法提取值, 也就是像函数一样.
当关联为多层嵌套的时候, 可以用`assoc["key1", "key2"]`这样的语法直接提取深层次关联的值.
这种方法适用于提取`单个`关联元素.

***
关联也可以也可以用`Part`函数, 通过`key`访问`value`, 即

+ 用`assoc[[ Key["key"] ]]`语法提取值, `"key"`为字符串时, 可以直接用`assoc[[ "key" ]]`.
+ 返回子集用`assoc[[ {"key1","key2"} ]]`, 结果是一个子关联,

```bash
<|"a" -> 5, "b" -> 6, 2 -> b, 1 -> a|>[[1 ;; 3]]
out:<|"a" -> 5, "b" -> 6, 2 -> b|>
```

***

但是`Association`也可以使用位置访问, 如下面的例子. 但是结果对于不同的参数形式, 将会不同.

```mathematica
temp = <|a -> 1, b -> 2, c->3|>;
temp[[1]](* 关联的第一部分 *)
out: 1
temp[[{1,2}]] (* 取关联的一部分 *)
out: <|a -> 1, b -> 2|>
temp[[All]]
out: <|a -> 1, b -> 2, c -> 3|>
```

这跟`Part`的一般行为一致, 当`Part`的参数是`{i,j,k,...}`的形式时, 结果也会带有原先的头部.

两种语法的`索引速度`没有太大的区别, 对速度影响最大的是系统缓存: `ClearSystemCache[]`

## 其他Key函数

可应用到`Keys` 上的函数:

+ `KeySort`, `KeySortBy` ; 按`keys`对关联进行排序
+ `KeyTake`, `KeyDrop` ; 在一个关联中`取出`, `存入`特定的`keys`
+ `KeySelect` ; 根据 对 `keys`的谓词, 来选择元素
+ `KeyMap` ; 在关联中的`keys`上 `map` 一个函数
+ `KeyValueMap` ; 在关联中的`键`和`值`上 map 一个函数
+ `KeyMemberQ`,`KeyFreeQ`

### 修改键,KeyMap

将函数应用于关联的`键`, 使用`KeyMap` 函数, 例如:

```mathematica
In[1]:= KeyMap[f, <|a -> 1, b -> 2, c -> 3|>]
Out[1]= <|f[a] -> 1, f[b] -> 2, f[c] -> 3|>
```

### 修改关联元素,AssociateTo,Append

这两个命令都可以修改关联的值, 但是有区别.

`AssociateTo`不能用来修改显式关联, 也就是必须先把关联绑定到一个变量上, 再用`AssociateTo`去修改. 如果直接修改, 会报错:

  AssociateTo::wrsym: Symbol Association is Protected.

```mathematica
assoc = Association[{a -> 1, b -> 2, c -> 3}]
AssociateTo[assoc, a -> 11]
```

而 `Append`可以直接修改显式关联的值.

```mathematica
Append[<|1 -> a, 2 -> b|>, 3 -> d]
```

`Append`也可以同时增加多个键:

```mathematica
Append[<|1 -> a, 2 -> b|>, {3 -> d, 4 -> e}]
```

## 匹配关联子集,KeyValuePattern

+ `KeyValuePattern[{patt1,...}]` ; 是一个模式对象, 表示一个关联, 或规则列表, 它能匹配到每个 `patti`的元素, 也就是包含的关系.
+ 匹配 `patti` 的元素可以在关联或规则列表中以任何顺序出现.
+ `patti` 按照它们出现的顺序进行匹配.
+ 如果只有一个模式`patt`, `KeyValuePattern[patt]`等同于`KeyValuePattern[{patt}]`.
+ `KeyValuePattern[{}]` ; 匹配任何`关联`或`规则`的列表.

关联的特点是: 元素的存取可以不依赖于次序; `KeyValuePattern` 的正是契合这一特点的`模式`对象.

### 详细

`KeyValuePattern` 可以匹配到关联中的 `任何元素`.

```mathematica
MatchQ[<|a -> 1, b -> 2, c -> 3|>, KeyValuePattern[b -> 2]]
Out[1]:= True
```

它也适用于`rule`s的列表:

```mathematica
MatchQ[{a -> 1, b -> 2, c -> 3}, KeyValuePattern[b -> 2]]
Out[2]= True
```

`KeyValuePattern` 匹配在关联中任何地方出现的元素:

```mathematica
Replace[<|a -> 1, b -> 2, c -> 3|>, KeyValuePattern[{a -> x_, c -> y_}] -> {x, y}]
Out[1]= {1, 3}
```

匹配任何具有两个条目的关联:

```mathematica
MatchQ[<|a -> 1, b -> 2|>, KeyValuePattern[{_, _}]]
Out[1]= 真
```

### 范围

+ `KeyValuePattern` 区分 `Rule` 和 `RuleDelayed`:

```mathematica
MatchQ[<|a->1|>, KeyValuePattern[{a :>1}]]
Out[1]= False
```

+ 从一个 `关联` 中提取符合`rule`的键:

```mathematica
ReplaceAll[<|a->1, b->2, c->3|>, KeyValuePattern[{x_ -> 1}] :> x]
Out[1]= a
```

+ 根据`condition`提取`规则`:

```mathematica
ReplaceAll[<|a -> 1, b -> 2|>, KeyValuePattern[{x_ -> y_?EvenQ}] :> (x -> y)]
Out[1]= b -> 2
```

+ 指定 `rule`s 的 `Keys` 具有某种关系, 也可以改成等价的 `PatternTest` 语法:

```mathematica
MatchQ[<|1 -> a, 2 -> b|>, KeyValuePattern[{x_ -> _, y_ -> _}] /; x + 1 == y]
```

+ `关联`中的每个`规则`最多被匹配一次:

```mathematica
ReplaceAll[<|a -> 1, b -> 2, c -> 3|>, KeyValuePattern[{_ -> x_, _ -> y_}] :> {x, y}]
Out[1]= {1, 2}
```

+ `KeyValuePattern` 中的`单个`模式, 不需要写在列表中:

```mathematica
MatchQ[<|a -> 1|>, KeyValuePattern[x_ -> y_]]
Out[1]= True

MatchQ[<|a -> 1|>, KeyValuePattern[{x_ -> y_}]]
Out[2]=True
```

+ 空的 `KeyValuePattern` 匹配任何 `关联` 或 `rule`的列表:

```mathematica
MatchQ[<||>, KeyValuePattern[{}]]
Out[1]=True

MatchQ[<|a -> 1|>, KeyValuePattern[{}]]
Out[2]=True

MatchQ[{}, KeyValuePattern[{}]]
Out[3]=True

MatchQ[{a -> 1}, KeyValuePattern[{}]]
Out[4]=True
```

### 应用

对`关联列表`应用`过滤器`(filter), 挑选出特定元素:

```mathematica
Cases[{
    <|"PartOfSpeech" -> "Noun", "Number" -> "Singular"|>,
    <|"PartOfSpeech" -> "Verb"|>},
KeyValuePattern[{"PartOfSpeech" -> "Noun"}]]

Out[1]= {<|"PartOfSpeech" -> "Noun", "Number" -> "Singular"|>}
```

### 属性和关系

+ `KeyValuePattern` 以任意顺序匹配:

```mathematica
MatchQ[<|a -> 1, b -> 2|>, KeyValuePattern[{_ -> 2, _ -> 1}]]
Out[1]= True
```

而写成`关联`形式的模式, 必须按准确的顺序匹配规则:

```mathematica
MatchQ[<|a -> 1, b -> 2|>, <|_ -> 2, _ -> 1|>]
Out[2]= False
```

+ 由 `KeyValuePattern` 匹配的表达式, 可能包含`指定规则`以外的其他规则.

```mathematica
MatchQ[<|a -> 1, b -> 2|>, KeyValuePattern[{a -> _}]]
Out[1]= True
```

写成`关联`形式的模式, 将尝试精确匹配所有规则:

```mathematica
MatchQ[<|a -> 1, b -> 2|>, Association[a -> _]]
Out[2]= False

MatchQ[<|a -> 1, b -> 2|>, Association[a -> _, b -> _]]
Out[3]= True
```

+ 由于 `KeyValuePattern` 匹配不依赖顺序, 可以有旁观规则,
如果再让它可以匹配任意个元素, 那么这个模式将太过宽泛而失去意义,
所以 `KeyValuePattern` 中的 `BlankNullSequence` 只匹配`一条规则`:

```mathematica
ReplaceAll[<|a -> 1, b -> 2, c -> 3|>, KeyValuePattern[{x___}] :> {x}]
Out[1]= {a -> 1}

ReplaceAll[<|a -> 1, b -> 2, c -> 3|>,
 KeyValuePattern[{x_, y__}] :> {x, y}]
Out[2]= {{a -> 1}, {b -> 2}}
```

+ 与之相反, 显式`关联`模式执行精确匹配,
所以规定在含有`关联`的模式中, `BlankNullSequence` 可以匹配多个规则:

```mathematica
ReplaceAll[<|a -> 1, b -> 2, c -> 3|>,  Association[x___, y_] :> {{x}, {y}}]
Out[2]= {{a -> 1, b -> 2}, {c -> 3}}
```

### 可能的问题

`KeyValuePattern` 中的每个模式可能只匹配一条规则.

```mathematica
ReplaceAll[<|a -> 1, b -> 2, c -> 3|>, KeyValuePattern[{x___}] :> {x}]
Out[1]= {a->1}

ReplaceAll[<|a -> 1, b -> 2, c -> 3|>, KeyValuePattern[{x_, y__}] :> {x, y}]
Out[2]= {a -> 1, b -> 2}
```

`空列表`或`空关联`,可由 `BlankNullSequence` 或 `RepeatedNull` 模式匹配.

```mathematica
MatchQ[<||>, KeyValuePattern[{___}]]
Out[1]= True

MatchQ[<||>, KeyValuePattern[{}]]
Out[3]= True
```

`KeyValuePattern` 只匹配`关联`或`规则列表`, 即使没有指定模式参数.

```mathematica
MatchQ[{a}, KeyValuePattern[{}]]
Out[1]= False

MatchQ[{a}, KeyValuePattern[{_}]]
Out[2]= False

MatchQ[{a -> b}, KeyValuePattern[{}]]
Out[3]=True
```
