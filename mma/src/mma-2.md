# mathemtatica-2

## 常用概念

+ Wolfram 系统文件的结构: tutorial/WolframSystemFileOrganization

+ `前端令牌`--`guide/FrontEndTokens` :
Wolfram语言允许通过发送适当的前端令牌, 从内核以脚本的方式执行任何前端命令. 如`保存`,`打开`文件等等.
除了所有标准菜单命令, 还包括默认前端菜单配置无法直接访问的`tokens`.

+ `模块` tutorial/HowModulesWork
Wolfram 语言中模块的基本工作方式非常简单.
任何模块每一次使用时, 就产生一个新符号去代表它的每一个局部变量.
新符号的名字被唯一地给定, 它不能跟任何其它名字冲突. 命名的方法是在给定的局部变量后加 `$`, 并给出唯一的序号.
从全局变量 `$ModuleNumber` 的值可以找到序列号. 该变量计算 `Module` 的任何形式所使用的总次数.
`Module` 中产生形如 `x$nnn` 的符号去代表每个局部变量.

`mathematica` 的脚本格式文件, 即 `.wl`,在保存时并不会保存输出信息.
所以适合用来保存源代码, 配合 `git` 使用.
`.m`, `.wls`文件特性和`.wl`基本相同, `.wls` 会自动加上一条 `shebang` 行.

    #!/usr/bin/env wolframscript

而普通的 `.nb` 文件则会保存计算输出信息, 在存档之前最好使用
`单元`--`删除所有输出` 清理输出, 这样可以减小体积.

***
`张量` tutorial/SymmetrizedArrays

mma 可以处理张量对称性, 使用以下函数

例如:

```mathematica
SymmetrizedArray[{{1, 2} -> a, {2, 3} -> b}, {3, 3},  Antisymmetric[{1, 2}]]
Symmetric[{1, 2, 3}]
Antisymmetric[{1, 2}]
Symmetrize[{{a, b}, {c, d}}, Antisymmetric[{1, 2}]]
```

`TensorSymmetry` : 给出张量在 `slots` 的置换下的对称性.
`Symmetric[All]`表示对所有指标对称, `Symmetric[{}]`表示没有对称性.

***
张量运算

内积:`Dot`

对于两个一般的张量 $T[i_1,i_2,\cdots ,i_n]$ and $U[j_1,j_2,\cdots,j_m]$, 应用`Dot[T,U]`将得到张量

$$\sum_k T[i_1,i_2,\cdots ,i_{n-1},k] * U[k,j_2,\cdots,j_m].$$

当然, 这要求$T$的最后一个指标$i_n$和$U$的第一个指标$j_1$相等, `Dot`运算始终可以理解为缩并这两个指标.
结果是一个$m+n-2$阶张量.

张量缩并:`TensorContract`

```mathematica
TensorContract[T, {{2, 3},{1,4}}]
```

分别缩并张量`T`的`2,3`, `1,4`指标.

***
`多项式`相关 tutorial/FindingTheStructureOfAPolynomial

找到某一个变量的最高次幂:

```mathematica
Exponent[1 + x^2 + a x^3, x]
```

提取多项式的系数:

```mathematica
CoefficientList[1 + 6 x - x^4, x]
```

给出一个多项式中的单项式:

```mathematica
MonomialList[(x + 1)^5, x, Modulus -> 2]
```

还可以为结果指定顺序, 可以指定的顺序有:

"Lexicographic", "DegreeLexicographic", "DegreeReverseLexicographic", "NegativeLexicographic", "NegativeDegreeLexicographic", "NegativeDegreeReverseLexicographic", or an explicit weight matrix

***
`李群生成元`

参考[stackexchange](https://mathematica.stackexchange.com/questions/159014/calculate-representations-of-sun-generators||calculate-representations-of-sun-generators), 计算`SU(3)`群的生成元在`d`维的表示.

实际上, 结构常数的思想不是先规定好它们的取值, 再找到合适的基.
这个过程是反过来的: 通常先规定基满足一些良好的性质, 再根据这些基计算结构常数.
一般是要求这些矩阵是稀疏矩阵, 并且对于某种内积正交.

在 Mathematica 中, 可以通过 `SparseArray` 设置稀疏矩阵.
下面的代码构造了`SU(3)`生成元的3–维基, 它们对于 `Frobenius` 内积是正交的.  当然, 该方法可以推广到任意维度.

```bash
n=3;
a=1/Sqrt[2] Flatten[Table[
SparseArray[{{i,j}->I,{j,i}->I},{n,n}],{i,1,n},{j,i+1,n}],1];
b=1/Sqrt[2] Flatten[Table[
SparseArray[{{i,j}->-1,{j,i}->1},{n,n}],{i,1,n},{j,i+1,n}],1];
c=DiagonalMatrix@*SparseArray/@Orthogonalize[Table[SparseArray[{{i}->I,{i+1}->-I},{n}],{i,1,n-1}]];
basis=Join[a,b,c];
MatrixForm/@basis
```

***
`插图`: `Inset[obj,pos,opos,size]`

把一个图像插入到外层图像的特定位置, 并指定插图的大小

***
`过程式编程`

guide/ProceduralProgramming
guide/FlowControl

普通的流程控制可以使用`Return[expr]`, 复杂的可以使用`Throw`,`Catch`

由于`Return`一般在控制结构比如`If`中使用, `Return`会退出`If`, 以及`If`外面那层函数, 但对于嵌套函数的情形, 只退出最内的一层函数.

语句组:
`CompoundExpression[exp1,exp2,...]`
例如:`Print[x]; Print[y]`, 按顺序执行.

返回值可以由`Return[expr]`控制, 但是需要有一个外层函数结构.

***
也可以使用`Throw`,`Catch`控制过程.
他们是成对使用的, `Throw[value,tag]`负责跳出过程, 返回当前`value`, 对应的`Catch`可以接住`value`,
可以用`tag`指定`Throw` and `Catch`如何匹配.
如:

```mathematica
Module[{u},
 Catch[
  Throw[a, u],
  u]
 ]
```

***
`离散数学` guide/DiscreteMathematics

***
`流和文件`:  tutorial/FilesAndStreams

***
盒子 Boxes

像Wolfram语言中的所有其他内容一样, 笔记本最终是符号表达式.  `mma` 的高维结构是用 "Box" 来实现的.

参考:

guide/LowLevelNotebookStructure
tutorial/RepresentingTextualFormsByBoxes

***
`Box` 转换与自定义

`Box`有自己的语法, 可以查看一个显示的表达式对应的`Box`表达式, 也可以对后者再次排版.
也就是说笔记本中显示的二维格式,与 low-level 的 `Box` 之间, 可以进行转换, 基本的转换有:

+ `DisplayForm[expr]`:将`expr`中的 `low-level` 框符表示成显式的二维格式.
+ `ToBoxes[expr,form]`: 给出对应于特定`form`的`Box`.

`ToBoxes` 会计算`expr`, 而`MakeBoxes`不计算`expr`

***
`Box` 自定义输出格式

一般很少需要修改这些规则.
主要原因是 `Wolfram` 语言已经为许多`operators`的输入和输出建立了内置规则, 而该`operators`本身并未为其分配特定的含义.
也就是预置了很多可以用, 但没有数学规则, 只有排版规则的`operators`.

可以用来自定义输出的函数有:
`MakeBoxes`: 底层版本
`Format`: 上层版本

`mma` 在输出计算结果的时候, 会使用`MakeBoxes`从表达式构建二维结构(`Box`).
`MakeBoxes`是Wolfram系统会话(`sessions`)中用于将表达式转换为`boxes`的`low-level`函数.
所以可以通过定义表达式的`MakeBoxes`上值来自定义输出.

另一方面, `MakeBoxes`也会使用通过`Format`添加的排版规则:

***
利用`MakeBoxes` 自定义输出格式:

```mathematica
gplus /: MakeBoxes[gplus[x_, y_, n_], StandardForm] :=  RowBox[{MakeBoxes[x, StandardForm],
SubscriptBox["\[CirclePlus]", MakeBoxes[n, StandardForm]],
MakeBoxes[y, StandardForm]}]
gplus[a, b, m + n]
```

***
利用`Format`自定义输出格式:

`Format[f[...]]:=rhs` 定义`f`的输出格式像是`rhs`.
`Format[expr,form]`:对特定`form`如`StandardForm`指定自定义格式.

```mathematica
Format[f[x_, y_, z__]] := f[x, ...]
```

`Format`的下值将被优先使用, 然后才使用跟`MakeBoxes`相关的上值.
`MakeBoxes`可以理解成是`Format`的底层版本.
不过一个重要的区别是`MakeBoxes`不会计算它的参数, 所以你可以只定义排版规则, 而不必担心这些表达式将会被如何计算.

此外, `Format`会自动在计算结果上再次调用`Format`, 而`MakeBoxes`不会.
所以你需要在需要排版的子表达式上,手动再次调用`MakeBoxes`.

当排版的时候,

`RawBoxes[boxes]`直接插入`boxes`到已有的`Box`结构中, 不检查错误, 由前端直接渲染.

***
辅助信息: `TagBox`

在不同排版格式之间转换的时候, 参数信息可能会丢失.
此时可以使用`TagBox`,`TagBox`提供了一种在 Wolfram 语言`input`和`output`中存储隐藏信息的方法.
`TagBox[bbb,tag]`构建的`Box`和`bbb`一样, 但可以包含额外信息`tag`. 一般是函数的头部.

比如:

```mathematica
ToBoxes[InverseFunction[f], StandardForm]
out: TagBox[SuperscriptBox["f",   RowBox[{"(", RowBox[{"-", "1"}], ")"}]], InverseFunction,  Editable -> False]
```

此外, `InterpretationBox`提供了一种在Wolfram语言`output`中存储隐藏信息的方法.

`InterpretationBox[boxes,expr]`
是一个底层`box`构建, 显示和`boxes`一样, 但如果在输入中, 被理解成`expr`

***
脚本中的原始字符格式

mathematica 的层次结构

```mathematica
guide/LowLevelNotebookStructure
tutorial/StringRepresentationOfBoxes
tutorial/RepresentingTextualFormsByBoxes
```

内核--前端,
`box`--笔记本,
字符串表示--框符
`\[name]`

最核心的概念是, 一切皆是表达式, 对于内核来说, 它接受的全部是类 `LISP` 的表达式 语言.
就类似于纯字符界面的交互方式, 只有 `LISP` 表达式, 在这个层次上, 考虑的是输入的编码问题.
往上, 包括图形, 二维化表示等等, 这个层面上, 出现 框符 的概念,
二维化框符的显示, 带来了一批处理显示相关的表达式, 它们是`mma`的排版层.
框符不仅有 `LISP` 表示, 而且有字符串表示.

***
`框符的字符串表示` tutorial/StringRepresentationOfBoxes

区分原始框符和其代表的表达式.

+ `\(input\)`  原始框符(即仅仅是一个二维化结构式)
+ `\!\(input\)`  框符的意义(二维化结构式的数学意义)

如果将一个 `StandardForm` 单元的内容复制到另一个如文本编辑器的程序中,
Wolfram 系统将在必要时生成一个 `\!\(...\)` 形式.
这样做是为了当讲这个格式的内容重新复制回 Wolfram 系统中时, 该 `StandardForm` 单元的原始内容会自动再次生成.
如果没有 `\!`, 则仅得到对应于这些内容的原始框符.

在选项的默认设置下, 贴入 Wolfram 系统笔记本中的 `\!\(...\)` 格式自动显示成二维格式.

***
字符串中嵌入二维框件结构.

+ `"\(input\)"`  一个原始字符串
+ `"\!\(input\)"`  含有框符的字符串

+ `\(box1,box2,...\)`  `RowBox[box1,box2,...]`
+ `box1\^box2`  SuperscriptBox[box1,box2]
+ `box1\_ box2`  SubscriptBox[box1,box2]
+ `box1\_box2\% box3`  SubsuperscriptBox[box1,box2,box3]
+ `box1\& box2`  OverscriptBox[box1,box2]
+ `box1\+box2`  UnderscriptBox[box1,box2]
+ `box1\+box2\% box3`  UnderoverscriptBox[box1,box2,box3]
+ `box1\/box2`  FractionBox[box1,box2]
+ `\@box`  SqrtBox[box]
+ `\@box1\%box2`  RadicalBox[box1,box2]
+ `` form\` box ``  FormBox[box,form]
+ `\*input`  构建来自 input 的框符

***
控制输入被解释的方式.

+ `\!\(input\)`  解释当前形式中的输入
+ `` \!\(form\`input\) ``  使用指定形式解释输入

***
文本格式的框符表示

tutorial/RepresentingTextualFormsByBoxes

tutorial/FormattedOutput

Wolfram 语言中的所有文本和图形格式最终是用框符的嵌套集合来表示的.
通常这些框符中的元素对应于要放在二维相对位置处的对象.

这里是对应于表达式 `a+b` 的框符:

```mathematica
In[1]:= ToBoxes[a+b]
Out[1]=  RowBox[{a,+,b}]
```

`DisplayForm` 表明这些框符是如何显示的:

```mathematica
In[2]:= DisplayForm[%]
Out[2]//DisplayForm=  a+b
```

`DisplayForm[boxes]` 表明 `boxes` 被显示的格式

***
一些基本的框符类型.

+ `"text"`  原样的文本
+ `RowBox[{a,b,...}]`  一行框符或字符串 a,b,...
+ `GridBox[{{a1,b1,...},{a2,b2,...},...}]`   一个框符网
+ `SubscriptBox[a,b]`  下标
+ `SuperscriptBox[a,b]`  上标
+ `SubsuperscriptBox[a,b,c]`  上下标
+ `UnderscriptBox[a,b]`  底标
+ `OverscriptBox[a,b]`  顶标
+ `UnderoverscriptBox[a,b,c]`  顶底标
+ `FractionBox[a,b]`  分式 `a/b`
+ `SqrtBox[a]`  平方根 `Sqrt[a]`
+ `RadicalBox[a,b]`  `b` 次方根 `Power[a, (b)^-1]`

***
修改框符的外观.

+ `StyleBox[boxes,options]`  按指定选项的设置显示 `boxes`
+ `StyleBox[boxes,"style"]`  按指定样式显示 `boxes`

***
控制框符的解释.

+ `FormBox[boxes,form]`  用与指定格式有关的规则解释 `boxes`
+ `InterpretationBox[boxes,expr]`  将 `boxes` 当作表达式 `expr` 的表示形式
+ `TagBox[boxes,tag]`  用 `tag` 引导 `boxes` 的解释
+ `ErrorBox[boxes]`  指出错误并不再对 `boxes` 进行解释

***
`输入语法` tutorial/InputSyntax

各种输入表达式的特殊方式

`!command` 执行外部命令,只在命令行有效.

***
`无内置定义的算符`tutorial/OperatorsWithoutBuiltInMeanings

有几百个记号没有内部(`built-in`)定义, 也就是没有和函数绑定.
可以用来构建自己的记号. 如:

```mathematica
CirclePlus[x,y]
TildeTilde[x,y]
Therefore[x,y]
LeftRightArrow[x,y]
Del[x]
Square[x]
AngleBracket[x,y,...]
x,y]
Superscript[x, y]
UnderBar[x]
SubPlus[x]
SubMinus[x]
SubStar[x]
SuperPlus[x]
SuperMinus[x]
SuperStar[x]
SuperDagger[x]
Overscript[x,y]
Underscript[x,y]
OverBar[x]
OverVector[x]
OverTilde[x]
OverHat[x]
OverDot[x]
UnderBar[x]
```

`Wolfram` 语言遵循一般约定, 对于某个算符, 和其有关的函数和这个算符的名字相同, 例如:

函数是 `Congruent[x,y,...] `
符号的名字是: `\[Congruent]`

一般有对应关系如下:

+ `x \[name]  y` ->  `name[x, y]`
+ `\[name] x` -> `name[x]`
+ `\[Leftname] x,y,...` -> `\[Rightname]  name[x, y, ...]`

***
快捷键

`Ctrl+Shift+B`, 选中配对的括号

***
将定义与不同的符号相关联

在 Wolfram 语言中, `f[args]=rhs` 或 `f[args]:=rhs` 会将对象 `f` 和你的定义相关联. 也就是说, 当输入 `?f` 时, 就会显示该定义. 一般我们把符号 `f` 作为标头的表达式称为 `f` 的下值 (`downvalue`).

Wolfram 语言也支持上值 (upvalue), 上值可以把不直接作为标头的符号与定义相关联.

比如, 在定义 `Exp[g[x_]]:=rhs` 中, 一种可能是把定义与符号 `Exp` 相关联, 认为它是 `Exp` 的下值. 但是从组织或效率的角度来看未必是最好的方式.

较好的方式是将 `Exp[g[x_]]:=rhs` 与 `g` 关联起来, 即对应为 `g` 的上值.

```mathematica
f[args]:=rhs    定义 f 的下值
f[g[args],...]^:=rhs    定义 g 的上值
```

***
`$Assumptions` 假设

`$Assumptions` 的初始设置为 `True`.

+ 设置全局假定: `$Assumptions = a > 0`
+ 局部添加假定: `Assuming[b < 0, Refine[Sqrt[a^2 b^2]]]`
+ 局部改变假定: `Block[{$Assumptions = a < 0 && b < 0}, Refine[Sqrt[a^2 b^2]]]`
+ 取消全局假定: `$Assumptions = True`

***
`Assumptions`(假设)

典型的缺省设置为`Assumptions:>$Assumptions`.

假设可以是方程, 不等式或定义域指定, 或者是这些内容的列表或逻辑组合.

***
`底层的输入输出规则`:tutorial/LowLevelInputAndOutputRules

`MakeBoxes[expr,form]` 按指定格式构造代表 `expr` 的框符
`MakeExpression[boxes,form]` 构造与 `boxes` 对应的表达式

***
`Needs` and `Get`

属性和关系

```mathematica
Once[Get[package]] 类似于 Needs[package]:
Once[Get["EquationTrekker`"]]
```

***
`图形结构`: tutorial/TheStructureOfGraphics

基本思想是 Wolfram 语言用图形基元的集合表示所有图形.
图形基元包括代表图像基本元素的 `Point` (点), `Line` (线) 和 `Polygon` (多边形), 以及 `RGBColor` 和 `Thickness` 等指令.

在 Wolfram 语言中,  每个完整的图形块都用图形对象表示. 图形对象的种类很多, 分别对应于不同类型的图形. 每类图形对象都有确定的头部以表明它的类型.

+ `Graphics[list]` 生成二维图形
+ `Graphics3D[list]` 生成三维图形

***
`Grid` 玄学用法

设置指定项的背景:

```mathematica
Grid[Table[x, {4}, {7}],
Background -> {None, None, {{1, 1} -> Pink, {3, 4} -> Red}}]
```

设置网格区域的背景:

```mathematica
Grid[Table[x, {4}, {7}],
Background -> {None, None, {{1, 1} -> Pink, {3, 4} -> Red}}]
```

使格子中的文本不换行, 指定`ItemSize->Full`.

***
`上设置延迟` ref/UpSetDelayed

把 `rhs` 赋为 `lhs` 的延迟值, 并将这种赋值和在 lhs 中层 1 出现的符号相关联.

***
上值 下值 tutorial/AssociatingDefinitionsWithDifferentSymbols
值集的操作, 上值 下值: tutorial/ManipulatingValueLists
表达式的计算 tutorial/EvaluationOfExpressionsOverview
标准计算流程 tutorial/TheStandardEvaluationProcedure
计算 非标准计算 tutorial/Evaluation

***
算符 tutorial/Operators

***
基本几何区域 guide/GeometricSpecialRegions

***
不显示out in cell label`

SetOptions[EvaluationNotebook[], ShowCellLabel -> False];

***
取表达式的一部分 tutorial/PartsOfExpressions

***
将多个函数应用到同一个变量 `Through`

***
属性 tutorial/Attributes
操作 Operate

***
函数列表(按字母顺序排列)guide/AlphabeticalListing

***
`稀疏数组`: 线性代数 tutorial/SparseArrays-LinearAlgebra
`求解线性系统`: tutorial/SolvingLinearSystems

***
`函数的上值和下值` tutorial/AssociatingDefinitionsWithDifferentSymbols

***
`参数序列`: Sequence[]

***
`常用记号和表示惯例`: tutorial/SomeGeneralNotationsAndConventions
`通过名称操作符号和内容`: tutorial/ManipulatingSymbolsAndContextsByName

***
`交叉连接`JoinAcross

ref/JoinAcross
tutorial/LevelsInExpressions
tutorial/Introduction-Patterns

***
`可选变量与默认变量`:ref/$SummaryBoxDataSizeLimit

***
`摘要框`: SummaryBoxDataSizeLimit

***
`张量对称性`: tutorial/TensorSymmetries

***
`下标索引`: ref/Indexed

***
`构造变量的具有索引的集合`

v = ToExpression["x" <> ToString[#]] & /@ Range[5]

***
`曲线拟合`
tutorial/CurveFitting

***
`在计算过程中收集表达式`: Sow Reap

tutorial/CollectingExpressionsDuringEvaluation

***
`数值运算`: tutorial/NumericalCalculations

***
`绘图工具`:tutorial/InteractiveGraphicsPalette

***
`DeleteCases`: 当应用于 `Association` 上时, `DeleteCases` 根据它们的值删除元素.

***
`#name`: `#name` 是 #["name"] 的简短形式:

```mathematica
In[1]:= #x &[<|"x" -> a, "y" -> b|>]
Out[1]= a
```

***
`NestList`: 嵌套列表, ref/NestList

***
颜色,  `guide/Colors`

结合新的可编程的符号颜色和精心选择的美学颜色参数, Wolfram 语言在图形和其它形式的显示方面, 采用更灵活, 更引人注目的方案来设置色彩和透明度.

***
`输出的样式和字体`: tutorial/StylesAndFontsInOutput

***
`格式化输出`: tutorial/FormattedOutput#218124152

我们首先了解一下与显示大量表达式相关的框符生成器, 然后再介绍几种超出了简单数学排版的方式, 这些方式能用于生成漂亮的格式化输出.

***
`图形和声音`: tutorial/GraphicsAndSoundOverview

***
`算子运算, 即泛函`:ref/Operate

***
`ctrl+space`跳出子表达式

***
`求和`: Sum, ref/Sum

***
`符号张量`: guide/SymbolicTensors

张量是线性计算的基本工具, 它把向量和矩阵推广到更高的阶数.
Mathematica 9 引入强大的方法来以代数方法操作任意阶数的对称张量. 它同时处理以分量数组给出的张量和特定张量域的成员给出的符号张量.

***
`函数组合和操作符表单`: guide/FunctionCompositionAndOperatorForms

Wolfram语言的符号结构使得可以以符号式合并和操作的"操作符"的创建变得轻松形成操作的"管道"并且应用到参数. 某些内置函数也直接支持一个"令行禁止"的条件表单, 它们在其中可以直接作为符号操作符给出.

***
`上下文`: tutorial/Contexts

总是给变量或者定义选用尽可能清楚的名称是一个好思想. 但这样做有时会导致变量名很长.
在 Wolfram 语言中, 可以用上下文来组织符合名. 在引入与其它符号不冲突的变量名的 Wolfram 语言程序包中上下文特别有用. 在编写或者调用 Wolfram 语言程序包时, 就需要了解上下文.
其基本的思想是任何符号的全名为两部分: 上下文和短名. 全名被写为 ``context`short``, 其中 ` 是倒引号或重音符字符( ASCII 二进制代码 96), 在 Wolfram 语言中称为上下文标记.

***
`计算用的时间约束`:TimeConstrained

ref/TimeConstrained

`TimeConstrained[expr,t,failexpr]`如果没有达到时间限制, 返回 `failexpr`.

***
`特殊函数`: tutorial/SpecialFunctions

Wolfram 系统包括了标准手册中所有的数学物理中常见的特殊函数. 下面将依次讨论各类函数.

***
`有理函数`: guide/RationalFunctions

Wolfram 语言可以有效地处理单变量和多变量的有理函数, 通过内置函数直接执行标准的代数变换.

***
以任一个变量为主组合表达式

```mathematica
FactorTerms[expr,x]
Collect[expr,x]
```

对于含有一个变量的表达式, 可以选择用项的和或者乘积等来表示. 而对于含有多个变量的表达式, 则有更多的可供选择的形式, 例如, 以任一个变量为主组合表达式.

***
`以不同形式表示表达式`:tutorial/PuttingExpressionsIntoDifferentForms

***
`假设 前提条件 变量约束`:Assumptions

`Simplify` 中的选项 `Assumptions` 可以使用诸如 `Assumptions -> {m > 0, \[Mu] > 0, Di > 0}]` 的规则列表

***
`生成列表`: 生成长度为`n`, 元素为 `f[i]` 的列表, ref/Array

***
Apply (@@)(应用)

ref/Apply

Apply[f,expr]
或 f@@expr  用 f 替换 expr 的头部.

***
连接列表:

`Join[list1,list2]`
把列表或其它享有相同头部的表达式连接在一起.

***
`Replace` 指定层数

`Replace[f[f[f[f[x]]]], f[x_] :> g[x], {0, 2}]`, 后面可以选`All`, 全部替换

Repeated [ expr_, {5} ]   or Repeated [ expr, {min,max} ]

可以具体制定重复的次数, 精确的!

***
`读写 Wolfram 系统的文件`: tutorial/ReadingAndWritingWolframSystemFiles

***
`Timing`: 表达式计算用的时间

Timing[f = Fourier[RandomReal[1, 2^16]];]

***
`Part` :  ref/Part

***
倒引号`` ` ``: 数字标记/记号

***
Array

$$ \text{Array}\left[f,\left\{n_1,n_2,\text{Null}\right\},\left(
\begin{array}{cc}
a_1 & b_1 \\
a_2 & b_2 \\
\end{array}
\right)\right] $$

***
`控制大表达式的显示`:  tutorial/ControllingTheDisplayOfLargeExpressions

***
`将求和换成列表`: tev3 = tev2 /. Plus -> List;

***
`Reap`收割, 收获

`Reap[expr]`
给出表达式 expr 的值, 以及在计算中已经应用 `Sow` 的所有表达式. 使用 `Sow[e]` 或具有不同标记的 `Sow[e,tag, i]]` "散布"的表达式在不同列表中给出.

***
`产生 C 和 Fortran 表达式`: tutorial/GeneratingCAndFortranExpressions

***
`修改图形的局部和全局方式`

给定一个图形基元列表后, Wolfram 语言提供了两种方式去修改最终的图形. 首先, 可以在图形基元列表中插入一些图形指令, 例如 RGBColor, 以修改随后列表中的图形基元. 用这种方式, 用户可以指定如何修改一个给定的图形基元列表.

通过插入图形指令, 可以指定图形基元的显示方式. 然而, 用户往往经常会希望通过全局修改来改变整个图形的显示. 使用图形选项可以达到这一目的.

通过增加图形选项 Frame 用户可以修改图形的整体外观:

```mathematica
Show[%, Frame -> True]
```

***
`确定图形块的完全形式`

对 `Axes` 等图形选项, Wolfram 语言的前端会自动画出用户需要的坐标轴等对象. 这些对象由选项值表示, 而非被确定的图形基元列表表示. 然而, 用户会需要要找到代表这些对象的图形基元列表. 函数 `FullGraphics` 给出不使用任何选项的情况下, 生成图形的完整的图形基元列表.

***
`UpSetDelayed`
`:=`
`=`
`^:=`

`Delayed` and 不带 `Delayed` 的最重要区别就是, 定义时计算, 还是调用的时候计算.
也就是不带 `Delayed` 容易受到全局变量的影响, 带 `Delayed` 更加接近函数式编程

***
一个变量的值具有许多类型, 见:ref/Set

OwnValues
DownValues
SubValues
UpValues
DefaultValues
NValues
FormatValues
Definition

`上值`:`UpValue`

`^:=` 定义上值(`upvalue`), 它的方式和使用一个标签的相同:

```mathematica
In[1]:= g /: f[g[x_]] := f1[x]

In[2]:= f[h[x_]] ^:= f2[x]

In[3]:= {UpValues[g], UpValues[h]}

Out[3]= {{HoldPattern[f[g[x_]]] :> f1[x]}, {HoldPattern[f[h[x_]]] :> f2[x]}}
```

一个标签仅定义一个上值(upvalue), `^:=` 执行所有符号的定义:

```mathematica
In[1]:= g /: f1[g[x_], h[y_]] := gh[x y]

In[2]:= f2[g[x_], h[y_]] ^:= gh[x y]

In[3]:= {UpValues[g], UpValues[h]}

Out[3]= {{HoldPattern[f1[g[x_], h[y_]]] :> gh[x y],
HoldPattern[f2[g[x_], h[y_]]] :> gh[x y]}, {HoldPattern[f2[g[x_], h[y_]]] :>
  gh[x y]}}
```

进行定义时, 计算立即赋值的右边:

```mathematica
In[1]:= rand[int] ^= Random[Integer];

In[2]:= {rand[int], rand[int]}

Out[2]= {0, 0}
```

每次使用定义时, 每次计算延迟定义的右边:

```mathematica
In[3]:= rand[real] ^:= Random[Real]

In[4]:= {rand[real], rand[real]}

Out[4]= {0.409393, 0.730688}
```

***
`调试不完全数组`

如果一个数组, 用Dimension 测试的结果是不完全数组,
如何找出是哪里的结构不完全呢.
可以把数组中的每一项都替换成$1$再显示, 这样可以比较方便的看出来.

***
`transpose 参数确定方法`

写下数组的维数, 比如`{6, 8, 3}`
在下面标出 转置后想得到的数组维数目次序,
比如`{3, 1, 2}`,

则transpose参数设置即是`{3, 1, 2}`

***
`可选变量与默认变量`

总之, 默认变量用`x_:v`, 可选变量用`p|PatternSequence[]`
有时需要定义具有默认值的函数. 即省略某些变量时, 其值就用设定的默认值代替. 模式 `x_:v` 就表示省略时值为 `v` 表示的变量.
一些 Wolfram 语言常用函数的变量具有系统设定的默认值, 此时不能明确给出 `x_:v` 中的默认值, 而是可用 `x_.` 来使用其系统设定的默认值.
有时不对一个可选变量分配默认值是方便的;这样的变量可以使用 `PatternSequence[]` 来指定.

`p|PatternSequence[]` 不具有默认值的可选模式 `p`

***
`内嵌单元`: 在文本中插入公式的话, 用插入-排版-内嵌单元 选项

***
`多行排列`:`Multicolumn[list,cols]`

是一个对象, 其格式为将列表的元素排列在具有指定列数的网格中

***
图例中的布局函数一定要加一个括号, 如果使用匿名函数的话, 例如:
`SwatchLegend[63, Range[5], LegendLayout -> (Multicolumn[***
, 1] &)]`

***
`单元结构`:

对应于单元的表达式.

+ `Cell[contents,"style"]`  有特殊风格的单元
+ `Cell[contents,"style1","style2`  有多个风格的单元
+ `Cell[contents,"style",options]`  有额外选项设置的单元

Wolfram 系统实际上用`样式定义单元`来定义样式. 这些单元可以放在另外的样式定义笔记本中, 也可以包含在一个笔记本的选项中. 不论哪一种情形, 都可以在标准笔记本前端中用 `编辑样式表` 菜单项访问它们.

***
`单元选项`

Wolfram 语言提供了大量的单元选项, 这些选项都可以在前端中通过 选项设置 菜单项访问.
它们可以在单个单元层上直接设置, 也可以在高层设置而让单个单元去继承.

***
`计算选项`

| 选项 | 典型默认值 | explanation |
|----|----|----|
| Evaluator | "Local" | 用于计算的内核名 |
| Evaluatable | False | 单元内容是否被计算
| CellAutoOverwrite | False | 产生新输出时是否覆盖以前的输出 |
| GeneratedCell | False | 该单元是否从内核产生 |
| InitializationCell |  False | 打开笔记本时是否自动计算该单元 |

Wolfram 语言可以对笔记本中的每个单元指定不同的计算方式.
但是常常是在笔记本层设置选项 Evaluator, 这一般是用前端中的 内核配置选项 菜单项完成.

***
`文本和字体选项`

***
`表达式输入和输出选项`

Wolfram 语言中常用特殊字符有许多别名.
`InputAliases` 可以给更多的特殊字符或其它类型的 Wolfram 语言输入定义别名. 形如 `"name"->expr` 的规则决定 `Esc name Esc` 应该在输入时立即被 expr 替换.

`别名`用明确的 `Esc` 字符定界. 选项 `InputAutoReplacements` 指定一些类型的输入序列即使设有明显的定界符也应该被立即替代. 例如, 在默认设置下, `->` 立即被 `->` 替换. 可以用形如 `"seq"->"rhs"` 的规则去指定输入中出现的 `seq` 立即用 `rhs` 替换.

***
`笔记本选项`

改变笔记本选项的方式.

+ `\[FilledSmallSquare]` 用选项设置菜单交互式的改变选项.
+ `\[FilledSmallSquare]` 使用内核的 `SetOptions[obj,options]`.
+ `\[FilledSmallSquare]` 用 `CreateWindow[options]` 产生具有指定选项的新笔记本.

这里创建一个笔记本, 并显示在具有细框的 `400x300` 窗口中:

```mathematica
CreateWindow[WindowFrame -> "ThinFrame", WindowSize -> {400, 300}]
```

***
`前端的全局选项`

在标准笔记本前端中, 可以设置许多 Wolfram 系统全局选项.
默认情况下, 这些选项的值保存在一个"偏好文件"中, 当重新运行 Wolfram 系统时就自动再次使用.
这些选项包含可以用偏好设置对话框生成的设置.

***
`前端全局选项的一些类型`

+ 风格定义  新笔记本使用的默认样式定义
+ 文件位置  搜索笔记本和系统文件的目录
+ 数据输出选项  如何用各种格式输出数据
+ 字符编码选项  如何对特殊字符编码
+ 语言选项  文本使用的语言
+ 信息选项  如何处理 Wolfram 系统产生的信息
+ 对话设置  对话框中的选择
+ 系统配置  对特定计算机系统的私有选项

可以用 `Options[$FrontEnd,name]` 从内核访问前端全局选项.
但通常在前端使用"选项设置"去交互式地访问这些选项.

***
`框符的字符串表示`

Wolfram 语言提供了一种简洁的方式来用字符串表示框符. 在将框符的设定作为普通文本进行导入和导出时, 这种方式尤其方便.

***
`区分原始框符和其代表的表达式`

+ `\(input\)` 原始框符
+ `\!\(input\)` 框符的意义

如果将一个 StandardForm 单元的内容复制到另一个如文本编辑器的程序中,
Wolfram 系统将在必要时生成一个`\!\(...\)`形式. 这样做是为了当讲这个格式的内容重新复制回 Wolfram 系统中时, 该 StandardForm 单元的原始内容会自动再次生成.
如果没有 `\!`, 则仅得到对应于这些内容的原始框符.

在选项的默认设置下, 贴入 Wolfram 系统笔记本中的 `\!\(...\)` 格式自动显示成二维格式.

***
`字符串中嵌入二维框件结构`

+ `"\(input\)"` 一个原始字符串
+ `"\!\(input\)"` 含有框符的字符串

Wolfram 语言通常将出现在字符串内的 `\(...\)` 格式与其它的一系列字符一样对待.
通过插入 `\!` 可以令 Wolfram 语言将这个格式视作它所代表的框符.
通过这种方式可以在普通字符串内嵌入框符结构.

Wolfram 语言把这当成一个普通字符串:

```mathematica
In[13]:= "\( x \^ 2 \)"
Out[13]= \( x \^ 2 \)
```

`!\` 告诉 Wolfram 语言这个字符串含有框符:

```mathematica
In[14]:= "\!\( x \^ 2 \)"
Out[14]= x^2
```

***
`模块化和事物的命名`: tutorial/ModulesAndLocalVariables

***
`Wolfram 系统的结构`: tutorial/RunningTheWolframSystemOverview

Wolfram 系统是一个模块化的软件系统, 其执行运算的内核与处理用户交互的前端是互相分离的. Wolfram 系统的基本部分:

+ Wolfram 语言内核 实际执行运算的部分
+ Wolfram 系统前端 处理与用户交互的部分

这样的设计比整体结构有许多优势. 例如, Wolfram 系统前端可运行在具有增强图形处理能力的本地计算机上, 而 Wolfram 语言内核可运行在更快地远程计算机上.
或运行多个内核只需一个前端.

最常见的 Wolfram 系统工作方式是使用交互式文档称为笔记本.
笔记本把具有文字, 图形, 面板和其它资料的输入和输出放在一起.
用户使用笔记本既可进行运算, 也可作为表达或发布自己的结果的工具.

其它常见的 Wolfram 系统界面包括基于文本的界面和 Wolfram Symbolic Transfer Protocol (WSTP) 接口.

Wolfram 系统的常见界面种类.

+ 笔记本界面 交互式文档
+ 基于文本的界面 由键盘输入的文本
+ WSTP 接口 与其它程序通讯

Wolfram 系统的一个重要特点是它不仅能与人交互, 还能和其它程序交互. 这个功能是通过 WSTP 来实现的.
它是外部程序和 Wolfram 语言内核之间的标准双向通讯协议.
在众多可用的 WSTP 兼容的程序中, 一些被用来作为 Wolfram 系统的前端. 这些前端常常提供自己特有的用户界面, 并把 Wolfram 语言内核纯粹作为嵌入的计算引擎.

**您应该意识到笔记本是 Wolfram 系统 "前端" 的一部分. **

Wolfram 语言内核--实际执行计算的部分,  既可以和前端一样运行在本地计算机上, 也可以通过网络运行在其它的计算机上.
在大多数情况下, 直到您用 Wolfram 语言进行计算时,  内核才开始启动.

***
`使用命令行界面`

在某些情况, 您不需要使用笔记本前端, 而需要更直接的与 Wolfram 语言内核交互,
为此, 您可以使用基于文本的界面, 您键入键盘的文本会直接进入内核.
值得注意的是, 虽然文本界面可以使用 Wolfram 语言内核的大部分功能, 但是不具备图形功能和与 Wolfram 语言前端动态交互的能力.

启动 Wolfram 语言内核 in `windows`: `wolframscript.exe`

***
`Wolfram 系统会话`

在每一个阶段, Wolfram 系统将给出提示 `In[n]:=` 告诉您, 它准备接受输入.
然后, 您可以敲入输入, 并以 `Enter` 或 `Return` 结束输入.

请注意您无需键入提示 `In[n]:=`, 只需键入提示后的文本.

当您敲入输入, Wolfram 系统将处理并产生结果, 输出结果时将其标记为`Out[n]=`.

***
`编辑输入的默认键`

+ `Ctrl+A`,`Home` 将光标移到输入的开始
+ `Ctrl+E`,`End` 将光标移到输入的结尾
+ `Ctrl+H`,`Backspace` 删除光标之前的字符
+ `Ctrl+D`,`Delete` 删除光标之后的字符
+ `Ctrl+G`,`Ctrl+C`,`Esc` 取消或者删除输入
+ `Ctrl+K` 删除从光标到输入末尾的内容
+ `Ctrl+D`,`Ctrl+Z` 终止内核
+ `Left`,`Right` 移动光标
+ `Up`,`Down` 调用前面的输入
+ `Enter` 计算输入或者增加另一行
+ `PageUp`,`PageDown` 跳到输入历史的第一项或者最后一项

当您的输入很长时, 您也可以在几行内给出. Wolfram 系统将自动读取连续的行直至它收到一个完整的表达式. 因此, 例如, 当您在一行输入一个前括弧或者双引号时, Wolfram 系统将继续读取连续的输入行, 直至它看到相应的后括弧或者双引号.

有时候, 您可能想要删除输入, 并且重新开始, 而不是计算已经输入的内容. 若要取消, 删除输入的内容, 使用 `Ctrl+G`. 或者如果您想要删除光标位置后的输入, 使用 `Ctrl+K`.

键盘快捷键列表在上面的表格中给出.

***
`启用原始的基于文本的界面`

通常情况下, 当 Wolfram 语言内核运行于基于文本的界面时, 它会提供其他工具, 例如命令行编辑器或命令行历史, 这些在前面章节已讨论过.
为了操作这些工具, 内核使用特殊的底层指令控制你使用的字符终端或终端仿真器.

在某些情况下, 你可能想阻止内核这样做,
比如你的终端不支持某些命令行编辑器需要的底层指令, 或比如你需要非交互式地运行内核作为更大型命令的一部分.

要在原始模式上运行 Wolfram 语言内核, 使用 `-rawterm` 命令行开关.
当在原始模式下, 内核累积所有直接从键盘上收到的字节到输入缓存以便进一步解析和诠释.

```mathematica
$ wolfram -rawterm
Mathematica 12.0 for Linux x86 (64-bit)
Copyright 1988-2018 Wolfram Research, Inc.

In[1]:=
```

***
终止内核: `Quit[]`

在输入提示符下输入 `Quit[]` 退出 Wolfram 系统.
您也可以敲入`Ctrl+D` 或 `Ctrl+Z` 退出 Wolfram 系统,
如果输入行为空, `Ctrl+D` 将终止 Wolfram 系统.

`Ctrl+D`, `Ctrl+Z` 或`Quit[]` 退出 Wolfram 系统

***
`Wolfram 系统会话`

reference: guide/WolframSystemSessions
reference: tutorial/RunningTheWolframSystemOverview

***
mathematica 中的前端概念:

Wolfram 系统的组件
Kernel(内核)- 核心计算引擎
Front end(前端)-笔记本界面系统

***
`执行计算`

`shift+enter` 计算输入
`ctrl+shift+enter`  在当前位置计算选择表达式
`expr;`   计算表达式, 但不输出结果
(= 在输入的开始部分) 使用自由格式语言输入

***
会话历史

`%n`  第n个输出表达式
`ctrl+l` 复制一个输入单元

***
消息

`Off` 关掉一个消息或一群消息
`Quiet` "安静"的运行一次计算, 不输出消息

***
会话信息

+ `Directory` 当前目录
+ `Names`  已知符号的名称
+ `Notebooks `
+ `MemoryInUse`
+ `SystemInformation`
+ `DateString`

***
`定制会话`

`$Post` 后处理,是一个全局变量, 如果设置, 它的值应用到每个输出表达式.
`$Path` 默认路径,给出在试图找到一个外部文件时搜索的缺省目录列表.

***
使用文本界面 tutorial/UsingATextBasedInterface

***
`计划任务`

NotebookEvaluate: 计算 `notebook`  中所有可计算单元

`NotebookEvaluate` 返回指定笔记本中最后一次计算返回的值.
`NotebookEvaluate` 可以采用 `NotebookObject` 或者指定一个笔记本文件的文件名. 如果一个文件名指代的是一个当前打开的笔记本, 那么计算进行到打开的笔记本中.
可以给定如下选项:

+ `InsertResults` `False` 是否在笔记本中插入结果
+ `EvaluationElements` `All` 计算哪些单元

缺省情况下, `NotebookEvaluate` 显示运算中产生的各种信息的方式与怎样调用 `Get` 类似.
消息, 输出及运算中产生的其他信息将会被放置在调用 `NotebookEvaluate` 的单元的输出中, 而不是指定笔记本的输出. 笔记本中现有的输出单元不会被更新或删除.

`NotebookEvaluate[notebook,InsertResults->True]` 处理运算中产生的信息和输出的方式与处理shift-enter运算的方式一样.
消息, 输出及运算中产生的其他信息将与输出一起放置在笔记本中,
会取代现有输出或其他相关单元.

当 `NotebookEvaluate[notebook, InsertResults->True]` 用于一个未打开的文件上时, Wolfram 系统将打开文件, 完全计算它, 保存并且关闭该文件.
`NotebookEvaluate[notebook, InsertResults->False]` 将使得笔记本完全不被修改.
笔记本的单元在对话子进程中计算.
在一个打开的笔记本上使用 `NotebookEvaluate` 将导致笔记本中出现子进程计算的可见部分.
笔记本将继续在屏幕上更新, 而它的单元正在被计算.
与 `Get` 不同, 出现在一个可执行单元中的语法将不阻止 `NotebookEvaluate` 对其他输入进行计算.
`NotebookEvaluate[notebook, EvaluationElements->Automatic]` 只计算初始化单元.
这与把笔记本保存为程序包文件等价, 或者与自动产生的程序包等价, 并且在所得的程序包文件上使用 `Get`.
当在一个程序包文件上运行时, `NotebookEvaluate` 等价于 `Get`. `InsertResults` 选项将被忽略.

***
`对话`: tutorial/Dialogs

在标准交互式进程中, 可以用 `Wolfram` 语言命令 `Dialog` 去建立一个**子进程**或者**对话.** 在进行计算的过程中, 可以用对话与 `Wolfram` 语言相互作用.
像 "计算的跟踪" 中提到的一样, `TraceDialog` 在一个表达式计算的过程中的指定点自动调用 `Dialog`. 另外, 在计算过程中要中断 `Wolfram` 语言时, 一般用对话检查它的状态.

***
启动对话和从对话返回.

+`Dialog[]` 启动 `Wolfram` 语言对话
+`Dialog[expr]` 启动对话, 将 `expr` 作为 `%` 的当前值
+`Return[]` 从对话返回, 取 `%` 的当前值作为返回值
+`Return[expr]` 从对话返回, 取 `expr` 作为返回值
