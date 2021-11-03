# Dataset 数据集

`Dataset[data] `: 表示一个基于列表和关联的层次结构的数据集.

+ 数据集不仅可以表示完整的矩形多维数据阵列, 还可以表示任意的树形结构, 对应于具有任意层次结构的数据.
+ 根据它所包含的数据, 数据集对象通常显示为一个表格或网格元素.
+ 像`Map`, `Select`等函数可以直接应用于数据集, 方法是`Map[f,dataset]`, `Select[dataset,crit]`等.
+ 数据集对象中的子集可以通过`dataset[[parts]]`获得.
+ 数据集对象也可以通过编写`dataset[query]`来使用专门的查询语法进行查询.

>二维数据集的显示模式, 只有内嵌关联的 `Key` 都是字符串的时候, 才会自动排版成表格的形式. 例如:

```mathematica
dataset = Dataset[{
   <|"a" -> 1, "b" -> "x", "c" -> {1}|>,
   <|"a" -> 2, "b" -> "y", "c" -> {2, 3}|>}]
```

## JoinAcross 与 SQL

[连接查询](https://www.liaoxuefeng.com/wiki/1177760294764384/1179610888796448)

`JoinAcross` 有效地实现了类似 `SQL JOIN`的方法, 将两个表`ai`和 `bj`中的行通过`spec`指定的列连接起来.

有`RIGHT OUTER JOIN`, 就有`LEFT OUTER JOIN`, 以及`FULL OUTER JOIN`. 它们的区别是:

+ `INNER JOIN`只返回同时存在于两张表的行数据, 由于`students`表的`class_id`包含`1`, `2`, `3`, `classes` 表的`id`包含`1`, `2`, `3`, `4`.
所以, `INNER JOIN`根据条件`s.class_id = c.id`返回的结果集仅包含`1`, `2`, `3`.
+ `RIGHT OUTER JOIN` 返回右表都存在的行. 如果某一行仅在右表存在, 那么结果集就会以`NULL`填充剩下的字段.
+ `LEFT OUTER JOIN` 则返回左表都存在的行. 如果我们给`students`表增加一行, 并添加`class_id=5`, 由于`classes`表并不存在`id=5`的行, 所以, `LEFT OUTER JOIN`的结果会增加一行, 对应的`class_name`是`NULL`.
+ 最后, 我们使用`FULL OUTER JOIN`, 它会把两张表的所有记录全部选择出来, 并且, 自动把对方不存在的列填充为`NULL`.

## 数据集结构

虽然列表和关联的任意嵌套是可能的, 但二维(表格)形式是最常用的.

+ 列表的列表; 没有指定行名称和列名称的表格
+ 关联的列表; 带有列名称的表格

+ 数据集按行优先, row--wise, 的方式解释嵌套的列表和关联, 因此第1层(最外层)的数据被解释为表格的行, 而第2层被解释为列.
+ 有名行和有名列分别对应于第`1`和第`2`层的关联, 其键是字符串, 被当成名称. 无名的行和列对应于这些级别的列表.
+ 一个数据集的行和列可以通过`Transpose[dataset]`来交换.

`Dataset`具有很多选项, 详见帮助页面 `Dataset Options`.

+ 当您将鼠标悬停在元素上时, 可以在数据集底部读取元素的位置.
+ `MaxItems` 的设置可以如下给出:
    + `m`; 显示 `m` 行
    + `m1,m2,..mn`; 在数据集级别 `i` 显示`mi` 项
    + `Automatic`; 显示默认的项目数.

+ 在 `HiddenItems` 列表中, 后面的设置会覆盖前面的设置.
+ `ItemDisplayFunction` 和 `HeaderDisplayFunction` 可以单独设置一个纯函数, 应用到项目上生产显示. 这些函数采用三个参数: 项目`value`, 项目`position`和包含项目的`dataset`.
+ 在 `ItemStyle` 和 `HeaderStyle` 设置中的某些位置, 显式规则可能会被解释为 `i->spec`. 如果要使用`style`选项, 请使用 `Directive` 包装规则.
+ 在 `Alignment`, `HeaderAlignment`, `ItemSize` 和 `HeaderSize` 等可能是列表值的选项中, 如果可能, 顶层列表被解释为单个选项值, 否则解释为对数据集不同`Level`的依次设置.
+ 如果规则的左侧不是`列表`, 则该设置将应用于任何位置, 只要它包含了左侧作为键或索引.
+ 若纯函数`f` 返回设置, 则`f`可以用来设定值. 该设置由 `f[item,position,dataset]` 给出.

## 切片操作

`dataset[[part]]`或`Part[dataset,part]`的语法可以用来提取数据集的部分. 可以从`Part`的所有通常规范提取数据集的切片.
与`Part`的普通行为不同, 如果一个数据集的指定子部分不存在, 那么在结果中的那个地方就会产生`Missing["PartAbsent",...]`.

下面的部分操作常用于从表格数据集中提取行.

+ `dataset[["name"]]` ; 提取一个已命名的行(如果适用).
+ `dataset[[{"name1",...}]` ;提取一组命名的行
+ `dataset[[1]]` ; 提取第一行
+ `dataset[[n]]` ; 提取第`n`行
+ `dataset[[-1]]` ; 提取最后一行
+ `dataset[[m;;n]]` ; 提取`m`到`n`行
+ `dataset[[{n1,n2,...}]`提取一组有编号的行

以下部分操作常用于从表格式数据集中提取列.

+ `dataset[[All, "name"]]` ;  提取一个已命名的列(如果适用).
+ `dataset[[All,{"name1",...}]` ; 提取一组命名的列
+ `dataset[[All,1]]` ;  提取第一列
+ `dataset[[All,n]]` 提取第`n ; `个列
+ `dataset[[All,-1]]` ;  提取最后一列
+ `dataset[[All,m;;n]]` ;  提取m至n列
+ `dataset[[All,{n1,n2,...}]` ;  提取一个列的子集

和`Part`一样, 行和列的操作也可以结合起来. 一些例子包括

+ `dataset[[n,m]]`; 取出位于第`n`行和第`m`列的单元.
+ `dataset[[n, "colname"]]`; 提取`n`行中命名的列的值
+ `dataset[["rowname", "colname"]]`; 提取指定行和列的单元格.

以下操作可用于移除行和列的标签, 有效地将关联变成列表.

+ `dataset[[Values]]` ; 删除行的标签
+ `dataset[[All,Values]]` ; 从列中移除标签
+ `dataset[[Values,Values]]` ; 从行和列中移除标签

## 数据集查询

查询语法`dataset[op1,op2, ...]`可以认为是`Part`语法的扩展, 即按照层次连续应用函数.
允许在数据上使用聚合和转换, 以及获取数据的子集.
因为是`Part`语法的扩展, 所以在切片的时候, 键是`Key["xxx"]`的形式, 而不是单纯一个键的名字`"aaa"`.

一些常见的查询形式包括:

+ `dataset[f]`; 将`f`应用于整个表.
+ `dataset[f, All]` ; 同样是`f`应用于整个表, 事实上, 不使用这种写法.
参见下方, `dataset[All,{All->f}]` 才是将`f`函数应用到每一列. 也没有 `dataset[All,All->f]`这种语法, 因为这样`Mathematica`会以为`All`是一个选项.

+ `dataset[All,f]` 将`f`应用于表中的每一行.
+ `dataset[All,All,f]` ; 将`f`应用于表中的每个单元格.
+ `dataset[f,n]` ; 提取第`n`列, 然后对其应用`f`.
+ `dataset[f, "name"]` ; 提取命名列, 然后对其应用`f`.
+ `dataset[n,f]` ; 提取第`n`行, 然后将`f`应用到它.
+ `dataset["name",f]` ; 提取命名行, 然后对其应用`f`.
+ `dataset[{n->f}]` ;  将`f`函数`Map`到仅第`n`行.
+ `dataset[All,{n->f}]` ; 将`f`函数`Map`到仅第`n`列.

一些更具体的查询形式包括.

+ `dataset[Counts, "name"]` ; 给出命名列中不同数值的计数
+ `dataset[Count[value], "name"]` ; 给出指定列中数值的出现次数
+ `dataset[CountDistinct, "name"]` ; 计算指定列中的不同值的数量.
+ `dataset[MinMax, "name"]` ;  给出指定列中的最小和最大值.
+ `dataset[Mean, "name"]` ; 给出指定列的平均值.
+ `dataset[Total, "name"]` ; 给出指定列的总价值
+ `dataset[Select[h]]` ; 提取那些满足条件h的记录
+ `dataset[Select[h]/*Length]` ; 计算满足条件`h`的行的数量
+ `dataset[Select[h], "name"]` ;  选择行, 然后从结果中提取指定的列.
+ `dataset[Select[h]/*f, "name"]` ; 选择行, 提取指定的列, 然后对其应用`f`.
+ `dataset[TakeLargestBy["name",n]]` ; 给出命名的列中最大的`n`行记录.
+ `dataset[TakeLargest[n], "name"]` ; 给出命名列中最大的`n`个值

## 下降和上升查询运算符

Descending and Ascending Query Operators

在表达式 `dataset[op1,op2, ...]` 中, 查询操作符`opi`被有效地应用于依次深入的数据层, 但是任何给定的操作符都可以在 `深入`数据或 `浮出`数据的时候应用.
构成数据集查询的运算符分为以下几大类, 分别具有`ascending`和`Descending`行为.

+ `All,i,i;;j, "key",...` ; `Descending`; `part`运算符
+ `Select[f],SortBy[f],...` ; `Descending` ; `filtering`运算符
+ `Counts,Total,Mean,...` ; `ascending`  ; `aggregation`运算符
+ `Query[...],... ` ; `ascending` ; `subquery`运算符
+ `Function[...],f`; `ascending` ; 任意函数

+ `descending`运算符被应用于原始数据集的相应部分, 然后再应用后续运算符到更深的层次上.
+ `下降`运算符的特点是, 当应用在某一层次时, 不会改变更深层次的数据结构. 这就保证了后续运算符遇到的子表达式的结构, 与原始数据集的相应层次是相同的.
+ 最简单的`下降`运算符是`All`, 它选择了某一层次的所有部分, 因此使该层次的数据结构没有变化. `All`可以安全地被替换成任何其他的`下降`运算符, 从而产生另一个有效的查询.
+ 当所有后续的`上升`和`下降`运算符被应用到更深的层次后, 才应用`上升`运算符. `下降`运算符对应于原始数据的层次, 而`上升`运算符对应于结果的层次.
+ 与`下降`运算符不同, `上升`运算符不一定保留它们所操作数据的结构. 除非一个运算符被特别确认为是`descending`, 否则它被假定为`ascending`.

### 子集运算符

`descending` part 操作符指定了在应用任何后续操作符到更深层次之前, 要在一个层次上取哪些元素.

+ `All` ;`对列表或关联的每个部分应用后续运算符
+ `i;;j` ; 抽取`i`到`j`部分并对每个部分应用后续操作符
+ `i` ; 只取第`i`部分, 并对其应用后续操作符
+ `"key",Key[key]` ; 在一个关联中取 `key` 对应的值, 并对其应用后续运算符.
+ `Values` ; 取关联中的值, 并对每个值应用后续运算符.
+ `{part1,part2,...}` ; 取给定的部分并对每个部分应用后续运算符

### 筛选运算符

`descending` filtering 运算符指定了在将后续运算符应用到更深层次之前, 如何在一个层次上重新排列或过滤元素.

+ `Select[test]` ; 只取列表或关联中满足`test`的部分, 作用在关联时, 按`Value`选择.
+ `SelectFirst[test] ` ; 取出满足`test`的第一个部分.
+ `KeySelect[test] ` ; 选取关联中那些键值满足`test`的部分.
+ `TakeLargestBy[f,n],TakeSmallestBy[f,n]` ;  取出使`f[elem]`为最大或最小的`n`个元素, 按照排序顺序.
+ `MaximalBy[crit],MinimalBy[crit]` ; 取出能够使`crit`最大或最小的元素.
+ `SortBy[crit] ` ; 按照`crit`的顺序对部分进行排序
+ `KeySortBy[crit]` ; 根据键对关联中的项目进行排序, 按照`crit`的顺序.
+ `DeleteDuplicatesBy[crit]` ; 根据`crit`提取不重复的项目.
+ `DeleteMissing ` ; 删除`Head`为`Missing`的元素.

语法 `op1/*op2`可以用来将两个或更多的过滤操作符结合成一个操作符, 但仍在一个层次上操作.

### 聚合运算符 `/*`, `@*`

`ascending` aggregation 运算符将后续运算符应用到更深层次的结果合并或汇总.

+ `Total` ; 是结果中所有数量的总和
+ `Min, Max` ; 给出结果中的最小, 最大数量
+ `Mean,Median,Quantile,...`给出结果的统计摘要, Quantile: 分位数
+ `Histogram,ListPlot,... `对结果进行可视化计算.
+ `Merge[f]` ; 使用函数`f`合并结果中具有相同键的关联子项.
+ `Catenate[{list1,list2}]` ; 将列表或关联的元素串联起来, 对于关联, 取出`Values`, 参数可以是列表和关联的混合.
+ `Join[list1,list2,n]`; 连接具有相同头部的列表或任意表达式, 在`n`层. 参数的头部必须相同.
+ `Counts` ; 给出关联, 统计结果中的值的次数
+ `CountsBy[crit]` ;  给出关联, 根据`crit`来计算数值的出现次数.
+ `CountDistinct` 给出结果中不同元素的个数.
+ `CountDistinctBy[crit]` ; 根据`crit`给出结果中不同元素的个数.
+ `TakeLargest[n],TakeSmallest[n]` ; 取最大或最小的`n`个元素.

语法 `op1/*op2`可以用来将两个或更多的聚合运算符组合成一个运算符, 但仍在一个层次上操作.
类似地算符有 `op1 @* op2`, 它们的区别在于结合次序:

`/*` 连接的算符, `从左到右`依次作用到对象上,
`@*` 连接的算符, `从右到左`依次作用到对象上,
并注意到 `/*` 和 `@*` 的优先级很高, 如果用它们组合匿名函数,
需要适当的括号`()`把函数包起来, 以避免使用非预期的结合方式, 如下所示:

```mathematica
(f[#]&) /*  (g[#]&)  /* (h[#]&)
```

### 子查询运算符 Query

`ascending subquery` 运算符, 在后续运算符应用到更深的层次之后, 才执行子查询.

+ `Query[...]` ;  对结果进行子查询
+ `{op1,op2,...}` ; 对结果一次性应用多个运算符, 产生一个列表
+ `<|key1->op1, key2->op2,...|>` ; 产生一个新的关联: 将 `key1`, `key2` 用作新的`键`,  将 `op1`, `op2` 应用到中间结果得到的值, 作为新的 `value`.
如果原数据的 ascending 是关联, 得到的结果还是关联,  `key` 将更新为指定的`key`.
+ `{key1->op1,key2->op2,...}` ; 使用给定的 `key1`, `key2` 选择结果的特定子集, 然后对它们作用对应的运算符`op1`, `op2`, ...,
如果原数据的 ascending 是关联, 得到的结果还是关联, 并且保持 `key` 不变.

+ 当一个或多个`descending`运算符与一个或多个`ascending`运算符组成时(如`desc/*asc`), 先应用`descending`部分, 然后应用后续运算符到更深的层次, 最后, 再应用`ascending`算符到该层的结果.

在`Query`表达式被应用之前, `mathematica`将它 "编译"为普通 `Wolfram Language` 函数及算符的组合. 要看一个`Query`表达式的编译形式, 请使用`Normal`:

```mathematica
Query[All, f] // Normal
Out[3]= Map[f]
Query[f, g] // Normal
Query[GroupBy["a"], Total] // Normal
```

### 结构更新语法

`{}`和`<||>`两种语法非常方便:

+ 制作一个数据集:

```mathematica
data = {
   <|"a" -> 1, "b" -> "x", "c" -> {1}|>,
   <|"a" -> 2, "b" -> "y", "c" -> {2, 3}|>,
   <|"a" -> 3, "b" -> "z", "c" -> {3}|>,
   <|"a" -> 4, "b" -> "x", "c" -> {4, 5}|>,
   <|"a" -> 5, "b" -> "y", "c" -> {5, 6, 7}|>,
   <|"a" -> 6, "b" -> "z", "c" -> {}|>};
```

将算符依次应用到各 column 上

```mathematica
Query[All, {"a" -> f, "b" -> g, "c" -> h}] @ data
```

+ 通过指定计算每一列的运算符来构建一个新的表格.

```mathematica
Query[All, "c" /* <|"ctotal" -> Total, "clength" -> Length|>] @ data
```

+ 使用相同的技巧重新命名列

```
Query[All, <|"A" -> "a", "B" -> "b", "C" -> "c"|>] @ data
```

+ 通过一个计分函数, 取出得分最高的`row`:

```mathematica
Query[MaximalBy[Length[#c] &]] @ data
```

## Query

`Query` 是 `Part`的一种推广, 它们具有相似的语法.
对于规则的嵌套矩阵, 从最外层的括号数起, 如果元素`a`外面嵌套有`n`个括号, 那么提取元素`a`, 需要`n`个指标组成的序列.
对于关联, 给出裸的指标获取`Value`, 给出括号包裹的指标`{1}`获取子关联.

`Query[op1,op2,...]`是 `Dataset` 的查询语法, 可以对`Dataset`对象作用, 也可以对由列表和关联组成的任意嵌套表达式作用.

例如:

```mathematica
Query["b", Total] @ <|"a" -> {1, 2}, "b" -> {3, 4}|>
```

+ 算符`[op1,op2,...]`是这样作用于后面的数据集的: `op1`作用于数据整体, `op2`作用于数据第一层等等. 但同时还要考虑算符的结构性质.
+ `下降`算符在深入数据更深层次的时候依次作用, 如果`op1`, `op2`均为下降算符, `op2`作用于`op1`产生的结果的下一层次.
+ `上升`算符在浮出数据时候才作用. 深入数据维度的时候会消耗`下降`算符, 等到`下降`算符用尽, 则会开始向数据集的浅层上浮,
这个时候才应用`上升`算符, 即使在算符序列中`上升`算符的次序在前面.
例如`Query[...,asc,dsc,...]` : 即使上升算符`asc`在前, 也先保持这一层数据不变, 应用后面的下降算符`dsc...`
在返回的过程中, 对`dsc`这一层的所有数据再应用`asc`.
+ 可以使用`@*`以及`/*`构造复合算符. 例如`f@*g@*h`, `h`先作用到数据上. `/*`的情况刚好相反, `f/*g/*h`中`f`最先作用.
复合算符作用在数据的同一层上, 同时遵守`上升下降`规则.
+ 普通的函数都被当成`上升算符`. `上升算符`的特点其实就是可能会改变数据的结构, 例如让这层数据坍缩到一点, 如`Total`, `Count`算符等等.

+ 其中 `op1,op2,....`依次连续作用到更深的层次上. 但是任何给定的算符都可以在`下降`到`expr`或者`上升`到`expr`的时候被应用.
Mathematica 采用的是树状数据结构. 下降和上升都是相对于这种数据结构而言的.
+ 一般来说, 部分指定和过滤运算符是`下降`运算符. 聚合运算符, 子查询运算符和任意函数是`上升`运算符.
+ `Query[][expr]`返回`expr`, 即空算符返回所有.
+ 特殊的`下降`运算符`GroupBy[spec]`将在它出现的层次上引入一个新的关联, 可以从现有的查询中插入或删除, 而不影响其他运算符的行为. 也就是不进行上升或者下降.
+ `GroupBy["string"]`的语法可以作为`GroupBy[Key["string"]]`的同义词来使用. 同样的语法也可用于`SortBy`, `CountsBy`, `MaximalBy`, `MinimalBy`和`DeleteDuplicatesBy`.

### 特殊运算符

特殊的降序运算符: `GroupBy[spec]`, 将在它出现的层次上引入一个新的关联, 可以从现有的查询中插入或删除, 而不影响后续运算符.
在`Mathematica`中, 关联被视为单层结构.

### 语法捷径

诸如`CountsBy`, `GroupBy`和`TakeLargestBy`这样的函数通常将另一个函数作为其参数之一.
在处理数据集中的关联时, 通常会使用这种 `by`函数来查询表中某一列的值.
为了方便起见, 数据集查询允许在这种情况下用`"字符串"`来表示`Key["字符串"]`的语法.

例如, 查询运算符`GroupBy["string"]`在执行前会自动改写为`GroupBy[Key["string"]]`.
类似地, `GroupBy[dataset, "string"]`表达式被改写为`GroupBy[dataset,Key["string"]]`.

### 查询行为

+ 只要可能, 会使用类型推导来确定一个查询是否会成功. 被推断为失败的操作将导致返回一个`Failure`对象而不执行查询.
+ 默认情况下, 如果在查询过程中产生了任何消息, 查询将被终止, 并返回一个包含消息的`Failure`对象.
+ 当查询返回结构化数据(例如列表或关联, 或这些数据的嵌套组合)时, 结果将以另一个`Dataset`对象的形式给出.
否则, 结果将以普通Wolfram语言表达式的形式给出.
+ 关于`Dataset`查询的特殊行为的更多信息, 请参见`Query`的功能页面.

## 导出数据集

`Normal`可以用来将任何数据集对象转换为其底层数据, 通常是列表和关联的组合.

数据集对象可以通过`Export["file.ext",dataset]` 或 `ExportString[dataset, "fmt"]`来导出,  支持以下格式:

+ `"CSV`: 以逗号分隔的数值表
+ `"TSV"`: 以制表符分隔的数值表
+ `"JSON"`: `JSON`表达式, 其中关联表示成对象
+ `"Package"`: 人类可读的 Wolfram 语言表达式
+ `"MX"`: 打包的二进制协议

+ `SemanticImport` 可用于将文件导入为数据集对象.

## GroupBy

+ `GroupBy[elem1,  elem2 , ...},   f]` ; 将`elem_i`分组, 结果为一个关联, `键`是互不相同的 `f[elem_i]`, `值` 是映射到同一个`f[elem_i]`的`elem_i`.
+ `GroupBy[{elem1,  elem2 , ...},  fk -> fv]` ; 根据 `fk[elem_i]`, 将 `fv[elem_i]` 分组.
+ `GroupBy[elem1,  elem2 , ...},  {f1, f2, f3}]` ; 形成嵌套关联, 在第 `i` 层使用 `f_i` 进行分组.
+ `GroupBy[elem1,  elem2 , ...},   spec, red]` ; 应用函数 `red` 当作 `reduce` 方法, 作用到生成的`值的列表`.
+ `GroupBy[spec]` ; 代表 `GroupBy` 的运算符形式, 可以应用于表达式.

### 详细

+ `GroupBy` 是 `map reduce` 操作套路 的推广.
+ `GroupBy[list,f]` 给出一个`关联`, 其键是独立的 `f[elem_i]`, 其值是 `list` 列表的`子列表`.
+ `GroupBy[assoc,f]` 给出一个`关联`, 其键是独立的 `f[elem_i]`, 其值是关联 `assoc` 的 `子关联`.
+ `GroupBy[spec][expr]` 等同于 `GroupBy[expr,spec]`.

### 例子

通过使用几个`函数`, 对元素进行迭代分组:

```mathematica
GroupBy[Range[10], {PrimeQ, OddQ}]
Out[1]= <|True -> <|False -> {2}, True -> {3, 5, 7}|>, False -> <|True -> {1, 9}, False -> {4, 6, 8, 10}|>|>
```

+ 通过使用几个函数, 对一个关联的`值`进行分组:

```mathematica
GroupBy[<|a -> 1, b -> 2, c -> 4|>, {EvenQ, PrimeQ}]
Out[1]= <|True -> <|True -> <|b -> 2|>, False -> <|c -> 4|>|>, False -> <|False -> <|a -> 1|>|>|>
```

+ 按`首元素`分组, 并计算相应最后一个元素的平均值:

```mathematica
GroupBy[{{a, x}, {b, v}, {a, y}, {a, z}, {b, w}},  First -> Last, Mean]
Out[1]= <|a -> 1/3 (x + y + z), b -> (v + w)/2|>
```

+ 对于迭代分组, Reduce 函数`g` 将作用在最终的小组上, 参数被放在括号中:

```mathematica
GroupBy[{<|1 -> a1, 2 -> b1, 3 -> c1|>, <|1 -> a2, 2 -> b2, 3 -> c2|>}, {Key[1], Key[2]}, g]
Out:=<|
a2 -> <|b2 -> g[{<|1 -> a2, 2 -> b2, 3 -> c2|>}]|>,
a1 -> <|b1 -> g[{<|1 -> a1, 2 -> b1, 3 -> c1|>}]|>
|>
```

### 范围

根据对儿数据的首元素, 将末元素分组:

```mathematica
GroupBy[{{a, 10}, {b, 20}, {a, 30}}, First -> Last]
Out[1]= <|a -> {10, 30}, b -> {20}|>
```

使用`组合器`(combiner)函数来组合结果中的值:

```mathematica
GroupBy[{{a, b}, {a, c}, {b, c}}, First -> Last, Total]
Out[1]= <|a -> b + c, b -> c|>

GroupBy[{{a, 10}, {b, 20}, {a, 30}}, First -> Last, Total]
Out[2]= <|a -> 40, b -> 20|>
```

使用`Key`指定键和值:

```mathematica
GroupBy[{<|1 -> a, 2 -> c|>, <|1 -> b, 2 -> c|>}, Key[2] -> Key[1]]
Out[1]= <|c -> {a, b}|>
```

使用不同的函数来分别提取`键`和`值`:

```mathematica
GroupBy[{{a, b}, {a, c}, {b, c}}, f -> g]
Out[1]= <|f[{a, b}] -> {g[{a, b}]}, f[{a, c}] -> {g[{a, c}]}, f[{b, c}] -> {g[{b, c}]}|>

GroupBy[{{a, b}, {a, c}, {b, c}}, f -> Last]
Out[2]= <|f[{a, b}] -> {b}, f[{a, c}] -> {c}, f[{b, c}] -> {c}|>

GroupBy[{{a, b}, {a, c}, {b, c}}, First -> Last]
Out[3]= <|a -> {b, c}, b -> {c}|>
```

Use the operator form of Extract to specify the key or the value:

使用 `Extract` 的算符形式来指定`键`或`值`.

```mathematica
GroupBy[{{{a}, b}, {{a}, d}}, Extract[{1, 1}]]
Out[1]= <|a -> {{{a}, b}, {{a}, d}}|>

GroupBy[{{{a}, b}, {{a}, d}}, Extract[2] -> Extract[{1, 1}]]
Out[2]= <|b -> {a}, d -> {a}|>
```

用 `Key`来指定 `key`s 和 `value`s:

```mathematica
GroupBy[{<|1 -> a, 2 -> c|>, <|1 -> b, 2 -> c|>}, Key[2] -> Key[1]]
Out[1]= <|c -> {a, b}|>
```

缺失的 `keys` 用 `Missing` 代替:

```mathematica
GroupBy[{<|1 -> a, 2 -> b|>, <|1 -> a, 3 -> c|>}, Key[2]]
Out[1]= <|b -> {<|1 -> a, 2 -> b|>},  Missing["KeyAbsent", 2] -> {<|1 -> a, 3 -> c|>}|>
```

将`函数`应用于关联的`值`, 进行分组:

```mathematica
GroupBy[<|a -> 1, b -> 2, c -> 4|>, EvenQ -> (# + 1 &)]
Out[2]= <|False -> <|a -> 2|>, True -> <|b -> 3, c -> 5|>|>
```

### 应用

使用 `GroupBy` 对 `数据集` 中的`行`进行分组, 引入新的关联层次:

```mathematica
titanic = ExampleData[{"Dataset", "Titanic"}]

(*将乘客按等级分组.*)
titanic[GroupBy["class"]]

(*按性别分组, 然后再按等级分组:*)
titanic[GroupBy["sex"], GroupBy["class"]]
```

### 性质和关系

`GroupBy` 返回一个关联, 而 `GatherBy` 返回一个列表:

```mathematica
GroupBy[{{a, b}, {a, c}, {b, c}}, First]
Out[1]= <|a -> {{a, b}, {a, c}}, b -> {{b, c}}|>

GatherBy[{{a, b}, {a, c}, {b, c}}, First]
Out[2]= {{{a, b}, {a, c}}, {{b, c}}}
```

按`f`分组等同于按`f->Identity`分组:

```mathematica
GroupBy[{a, b, a}, f]
Out[1]= <|f[a] -> {a, a}, f[b] -> {b}|>
GroupBy[{a, b, a}, f -> Identity]
Out[2]= <|f[a] -> {a, a}, f[b] -> {b}|>
```

## Extract

+ `Extract[expr,pos]`; 提取 `expr` 在  指定位置 `pos` 的部分.
+ `Extract[expr,{pos1, pos2, ...}]` ; 提取 `expr` 的部分的列表.
+ `Extract[expr,pos,h] ` ;  提取 `expr` 的部分内容, 在`求值`前, 用头部 `h` 包住每个部分.
+ `Extract[pos] ` ; 代表 `Extract` 的运算符形式, 可以应用于表达式.

### Details

+ `Extract[expr,{i,j,...}]` 等同于 `Part[expr,i,j,...]` .
+ `Extract`所使用的`位置指定`, 与 `Position` 所返回的 `位置指定` 具有相同的形式, 并可以在 `MapAt` 和 `ReplacePart` 等函数中使用.
+ 你可以使用 `Extract[expr,...,Hold]` 来提取部分, 而保持不计算的形式.
+ 如果 `expr` 是一个 `SparseArray` 对象, `Extract[expr,...]` 会提取相应普通数组中的部分.
+ `Extract` 对 `Association` 对象起作用, 使用与 `Part` 相同的`keys`规范.
+ `Extract[pos][expr]` 相当于 `Extract[expr,pos]`.

## Catenate,Join

```mathematica
Join[list1, list2, ... ];  连接共享同一`头部`的列表, 或其他表达式
Join[list1, list2, ..., n];  连接每个 list_i 中的第 n 层对象

Catenate[{list1, list2, ... }]; 产生一个单一的列表, 其中有来自 list_i 的所有元素依次排列
Catenate[{assoc1, assoc2, ...}];  产生一个列表, 按照`values` 在关联中出现的次序排列.
```

+ `Join` 中的 `list_i`不需要有 `List` 头部, 但必须都有相同的`头部`.
+ `Join` 在 `Association` 对象上工作, 保留与任何给定键相关的`最后一个`值.
+ `Join` 在 `SparseArray` 对象上工作, 有效地连接了相应的普通列表.
+ `Join[list1, list2, ... , n]` 通过有效地连接 `list_i` 中的所有连续的 `n`级元素, 来处理锯齿状数组. (ragged arrays)

+ `Catenate[{expr1, expr2, ...}]` 允许 `expr_i` 是任何`列表`和`关联`的混合物.
+ 任何带有` Missing` 头部的 `exp_i` 都会被忽略掉.
+ 对于一个关联 `assoc`, `Catenate[assoc]` 被认为是 `Catenate[Values[assoc]]`.

### 例子

+ 中缀形式:

```mathematica
{a, b, c}~Join~{x, y}~Join~{u, v, w}
Out[1]= {a, b, c, x, y, u, v, w}
```

+ 连接两个关联:

```mathematica
Join[<|a -> b|>, <|c -> d, a -> e|>]
Out[1]= <|a -> e, c -> d|>
```

`Catenate` 关联:

```mathematica
Catenate[{<|a -> 1, b -> 2|>, <|c -> 3, d -> 4|>}]
Out[1]= {1, 2, 3, 4}
```

+ `Catenate` 有效地压平了高维数组的前两层:

```mathematica
Catenate[ConstantArray[1, {2, 2, 2}]]
Out[1]= {{1, 1}, {1, 1}, {1, 1}, {1, 1}}.
```

+ `Catenate` 剥离键, 连接所有的值.

```mathematica
Catenate[{{1, 2}, <|a -> 1, b -> 2|>}]
Out[1]= {1, 2, 1, 2}
```

+ `Catenate` 不会覆盖`重复键`的`值`:

```mathematica
Catenate[{<|a -> 1, b -> 2|>, <|c -> 3, a -> 5|>}]
Out[1]= {1, 2, 3, 5}.
```

### 性质和关系

`Join[list1, list2, ...]`  等价于 `Flatten[{list1, list2, ...}, 1]` :

```mathematica
Join[{1, 2}, {{a, b}, {c, d}}, {3, 4, 5}]
Out[1]= {1, 2, {a, b}, {c, d}, 3, 4, 5}

Flatten[{{1, 2}, {{a, b}, {c, d}}, {3, 4, 5}}, 1]
Out[2]= {1, 2, {a, b}, {c, d}, 3, 4, 5}
```

### Neat examples

不断 `double` 一个列表, 通过与自己连接:

```mathematica
NestList[Join[#, #] &, {x}, 4]
Out[1]= {{x}, {x, x}, {x, x, x, x}, {x, x, x, x, x, x, x, x}, {x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}}
```
