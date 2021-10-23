# mathematica snippets

+ 参数序列比较短的时候可以直接用`List[...]`,
+ 参数比较长, 但不是巨大规模时, 用关联收集参数.
+ 参数规模非常大时, 列表可以节省空间, 关联可以提高查找效率.

## 代码片段

### position

计算列表中特定元素的位置:

```mathematica
pos[e_, f_] :=
 Module[{posR},
  posR[expr_, form_, i_] :=
   DeleteCases[(*从列表末尾开始递归*)
    If[! AtomQ@expr[[i]],(*如果不是基元,递归下一层*)
     Join[posR[expr, form, i - 1],(*还未处理的部分*)
      Prepend[#, i] & /@
       posR[expr[[i]], form, Length@expr[[i]]]],(*如果是基元*)
     If[i > 1,(*如果还没有循环到 first 元素*)
      If[MatchQ[expr[[i]], form],
       Append[posR[expr, form, i - 1], {i}],(*匹配则添加指标*)
       posR[expr, form, i - 1] (*不匹配则循环上一个元素*)],(*如果循环到了 First 元素*)
      If[MatchQ[First@expr, form], {{1}}, {{Missing}}](*递归结束点,匹配返回{1},
      否则返回 Missing*)]], {___, Missing}(*删除失败的匹配*)];
  posR[e, f, e // Length]]
(*+++++++++++++++++++++测试+++++++++++++++++++++*)
Echo[test = RandomInteger[9, {5, 5, 5}]];
pos[test, 1] // AbsoluteTiming
Position[test, 1] // AbsoluteTiming
```

### Benchmark

```mathematica
Needs["Benchmarking`"]
BenchmarkReport[]
```

### 初始化单元

+ 计算环境变量

```mathematica
Once[
If[
(* if $ScriptCommandLine==={}, the environment is frontend*)
SameQ[$ScriptCommandLine,{}],
(*if execute in the frontend mode, refresh the title name*)
CompoundExpression[
(*文件绝对路径*)
filename=NotebookFileName[],
(*单元对象,第一个单元*)
cell`title=First[Cells[]],
(*刷新第一个单元的名字*)
NotebookWrite[cell`title,Cell[Last[FileNameSplit[filename]],"Title"]],
(*if execute in commandline mode, print a ready message*)
git`root`dir=First[StringCases[NotebookDirectory[],StartOfString~~((WordCharacter|":"|"\\")..)~~"octet.formfactor"]]
(*add the base git root dir*)
],
CompoundExpression[
Print["Ready to execute this script"]
]
]
]
```

+ 引入命令行参数的初始化

```mathematica
initial parameters

++++++++++++++++++++++++++++++++++++++++++++

模拟命令行输入,调试使用

parameter`marker, "Bars","Fences","Points", "Ellipses","Bands"

input`simulation={"C:\\octet.formfactor\\Numeric.series-o1.rencon3\\
f.figure.series-full.rencon3.strange.baryons-all.band.wl",
"full",0.90,1.50,1,"Bands",0.1};

++++++++++++++++++++++++++++++++++++++++

引入命令行参数, 1 用作实际脚本运行, 2用作调试

If[
SameQ[$ScriptCommandLine,{}],
(*if execute in the frontend mode, refresh the title name*)
input`cml=input`simulation,
(*if execute in commandline mode, use $ScriptCommandLine as parameters*)
input`cml=$ScriptCommandLine
];

+++++++++++++++++++++++++++++++++

Print["----------------------------","\n","the parameter order, lambda, ci is","\n","----------------------------"];

{
file`name,
parameter`order,
parameter`lambda0,
parameter`ci,
curve`opacity,
parameter`marker,
mark`opacity
}={
input`cml[[1]],input`cml[[2]],
ToExpression[input`cml[[3]]],
ToExpression[input`cml[[4]]],
ToExpression[input`cml[[5]]],
ToString[input`cml[[6]]],
ToExpression[input`cml[[7]]]
}

Print["----------------------------"];

git`root`dir=StringCases[ExpandFileName[file`name],StartOfString~~((WordCharacter|":"|"\\")..)~~"octet.formfactor"][[1]]
```

### 自定义笔记本的字体

在笔记本执行下面命令

```mathematica
AbsoluteOptions[EvaluationNotebook[], StyleDefinitions];(*笔记本字体设置*)
style`my = Notebook[{
   Cell[StyleData[
     StyleDefinitions ->
      FrontEnd`FileName[{"Book"}, "Textbook.nb",
       CharacterEncoding -> "UTF-8"]]],
   Cell[StyleData["Section"], FontFamily -> "Noto Sans CJK SC Bold",
    FontSize -> 16, FontWeight -> "Bold", FontSlant -> "Plain",
    FontVariations -> {"StrikeThrough" -> False,
      "Underline" -> False}],
   Cell[StyleData["Subsection"],
    FontFamily -> "Noto Sans CJK SC Black", FontSize -> 13,
    FontWeight -> "Heavy", FontSlant -> "Plain",
    FontVariations -> {"StrikeThrough" -> False,
      "Underline" -> False}],
   Cell[StyleData["Subsubsection"],
    FontFamily -> "Noto Sans CJK SC Bold", FontSize -> 11,
    FontWeight -> "Bold", FontSlant -> "Plain",
    FontVariations -> {"StrikeThrough" -> False,
      "Underline" -> False}],
   Cell[StyleData["Text"], FontFamily -> "Noto Sans CJK SC Regular",
    FontSize -> 12, FontWeight -> "Plain", FontSlant -> "Plain",
    FontVariations -> {"StrikeThrough" -> False,
      "Underline" -> False}]
   },
  Visible -> False,
  StyleDefinitions -> "PrivateStylesheetFormatting.nb"
  ];
SetOptions[EvaluationNotebook[], StyleDefinitions -> style`my];
```

## ColorData

+ `ColorData["scheme"][par]` 或 `ColorData["scheme",par]` 给出指定颜色方案中对应于参数值 `par` 的 `RGBColor` 对象.
+ `ColorData["scheme"]` 给出一个 ColorDataFunction 对象.
+ 默认情况下, 颜色梯度有一个范围从0到1的单一参数.
+ `ColorData["collection"]` 给出名称集合中的颜色方案列表.

### 获得集合的列表:

```mathematica
ColorData[]
{"Gradients", "Indexed", "Named", "Physical"}
```

### 求出属于每个集合的方案:

```mathematica
ColorData["Named"]
{"Atoms", "Crayola", "GeologicAges", "HTML", "Legacy", "WebSafe"}
```

### 梯度是一个连续颜色函数, 它适用于 ColorFunction:

```mathematica
DensityPlot[y + Sin[x^2 + 3 y], {x, -3, 3}, {y, -3, 3},
ColorFunction -> ColorData["SunsetColors"]]
```

### 索引的方案包含具体值相关的颜色列表:

```mathematica
ColorData[3, "ColorList"]
```

## MapThread level 的区别

```mathematica
In[2]:= MapThread[f, {{{a, b}, {c, d}}, {{u, v}, {s, t}}}]
Out[2]= {f[{a, b}, {u, v}], f[{c, d}, {s, t}]}
```

```mathematica
In[3]:= MapThread[f, {{{a, b}, {c, d}}, {{u, v}, {s, t}}}, 2]
Out[3]= {{f[a, u], f[b, v]}, {f[c, s], f[d, t]}}
```

## Grid temple

```mathematica
Grid[(*start grid *)
 Prepend[(*for prepend names horizontal*)
  MapThread[Prepend,(*for prepend names vertical *)
   {
   (*start the data to display*)
    datalist,
    (*end the data to display*)
    (*start prepend names vertical*)
    name`vertical
    (*end prepend names vertical *)
    }
   ],
   (*start prepend names horizontal*)
  {"", name`horizontal}
  (*end prepend names horizontal*)
  ]
 , Frame -> {All, All}
 , Spacings -> {2, 2}
 , Background -> {
 None, (* color x direction: x1, x2, x3...*)
 {{None, None}}(* color y direction: y1, y2, y3...*)
 }
 ](*end grid *)
```

## Grid 背景颜色玄学用法

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

## 不显示行号

```mathematica
SetOptions[EvaluationNotebook[], ShowCellLabel -> False];
```
