## ColorData

>`ColorData["scheme"][par]` 或 `ColorData["scheme",par]` 给出指定颜色方案中对应于参数值 `par` 的 `RGBColor` 对象.

>`ColorData["scheme"]` 给出一个 ColorDataFunction 对象. 

>默认情况下, 颜色梯度有一个范围从0到1的单一参数. 

>`ColorData["collection"]` 给出名称集合中的颜色方案列表. 

### 获得集合的列表：

```mathematica
ColorData[]
{"Gradients", "Indexed", "Named", "Physical"}
```

### 求出属于每个集合的方案：

```mathematica
ColorData["Named"]
{"Atoms", "Crayola", "GeologicAges", "HTML", "Legacy", "WebSafe"}
```

### 梯度是一个连续颜色函数, 它适用于 ColorFunction：

```mathematica
DensityPlot[y + Sin[x^2 + 3 y], {x, -3, 3}, {y, -3, 3}, 
ColorFunction -> ColorData["SunsetColors"]]
```

### 索引的方案包含具体值相关的颜色列表：

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

设置指定项的背景：

```mathematica
Grid[Table[x, {4}, {7}], 
 Background -> {None, None, {{1, 1} -> Pink, {3, 4} -> Red}}]
```

设置网格区域的背景：

```mathematica
Grid[Table[x, {4}, {7}], 
 Background -> {None, None, {{1, 1} -> Pink, {3, 4} -> Red}}]
```

## 不显示行号

```mathematica
SetOptions[EvaluationNotebook[], ShowCellLabel -> False];
```







