# 格式化输出

tutorial/FormattedOutput

自从版本3以来, Wolfram 语言提供了对任意数学排版和布局的有力支持.
基于所谓的框符语言(box language), 它允许笔记本自身作为 Wolfram 语言的表达式.

尽管这种框符语言非常强大, 在实际操作时却很难被用户直接使用.
从版本6, 出现了框符语言的高层界面, 免去了直接使用框符, 却仍保持排版和布局的强大功能.
这个新层的函数被称为`框符生成器`, 但用户不必意识到框符语言.

我们首先了解一下与显示大量表达式相关的框符生成器,
然后再介绍几种超出了简单数学排版的方式, 这些方式能用于生成漂亮的格式化输出.

## 样式化输出(Styling Output)

Wolfram 系统前端支持文字处理器中出现的所有惯用样式机制, 然而, 在生成的结果中自动访问这些样式机制曾经非常困难.

为了解决这个问题, 创建了函数 `Style`. 无论何时对一个 `Style` 表达式进行计算, 其输出结果将以给定的样式特征显示 .

可以将任何表达式封装 `Style`. 下面这个例子通过 `Style` 选用不同的字体灰度和颜色对质数和合数进行显示.

```mathematica
In[1]:= Table[If[PrimeQ[i], Style[i, Bold], Style[i, Gray]],{i,1,100}]
```

有上百种格式化选项可以与 `Style` 结合使用(更完整的列表请参见关于 `Style` 的参考资料), 此处列出的是最常见的几个选项.

| 菜单 | `Style[]` 选项 | `Style[]` 指令 |
| ----- | ----- | ----- |
| `格式 -> 尺寸 -> 14` | `FontSize->14` | `14` |
| `格式 -> 字体颜色 -> 灰色` | `FontColor->Gray` | `Gray` |
| `格式 -> 字体效果 -> 粗体` | `FontWeight->Bold` | `Bold` |
| `格式 -> 字体效果 -> 斜体` | `FontSlant->Italic` | `Italic` |
| `格式 -> 背景颜色 -> 黄色` | `Background->Yellow` |
| `格式 -> 字体`  | `FontFamily->"Times"` |  |
| `格式 -> 样式 -> Subsection` | `"Subsection"` |   |

`Style` 可以被任意嵌套, 在有冲突时最内层具有最高的优先权.
这里用 `Style` 封装整个列表从而对列表中的所有元素应用一种新的字体.

```mathematica
In[2]:= Style[%, FontFamily->"Helvetica"]
```

要求一段输出的样式与文本一样也是很常见的. 将代码所用的字体用于文本可能会看上去非常奇怪.
为此, 有一个函数`Text`能够使其参数将永远以文本字体呈现.

## 网格布局(Grid Layout)

在 Wolfram 语言中, 这种布局的基本函数是 `Grid`. `Grid` 的布局功能很灵活, 能够任意调整对齐方式, 框架元素(frame elements), 以及跨度元素(spanning element)等.

再次观察将质数和合数进行不同显示的 Style 例子.

```mathematica
In[4]:= ptable=Table[If[PrimeQ[i], Style[i, Bold], Style[i, Gray]],{i,1,100}];
```

要将此放入一个 `Grid` 中, 首先使用 `Partition` 将这个含有`100`个元素的列表转换成一个`10*10`数组.
也可以给 `Grid` 一个参差不齐的的阵列(列表中的元素为长度不一的列表)

```mathematica
In[5]:= Grid[Partition[ptable,10]]
```

注意到各列按中心对齐, 且无框架线, 使用 `Grid` 的选项可以很容易的改变这两项的设置.

```mathematica
In[6]:= Grid[Partition[ptable,10], Alignment -> Right, Frame -> True, Background -> LightBlue]
```

***

有几项与 Grid 相关的有用的结构, 其中一个是 `Column`, 它可以取出一列水平的元素将其垂直排列. 用 Grid 完成这一任务会稍显笨拙. 这里是一个简单的例子, 可以将一列选项按列显示.

```mathematica
In[7]:= Column[Options[Column]]
Out[7]= Alignment->{Left,Baseline}
AllowedDimensions->Automatic
AllowScriptLevelChange->True
...
```

如何水平列出一列事物呢?
那种情况下, 你要问的主要的问题是, 是否要得到的显示像一行数字或文本一样换行,
还是要所有元素保持在一行.
在后一种情况, 可以将 Grid 应用于1*n 的阵列.

```mathematica
In[8]:= Grid[{Range[15]!}]
```

但注意到在这个例子中, 整个网格收缩, 以便它与现有窗口的宽度适合.
因此网格中有的元素自身能够进行多行换行.
这是由于 Grid 的默认 `ItemSize` 选项. 如果想要允许一个网格的元素具有自然宽度, 要将 `ItemSize` 设置为 `Full`.

```mathematica
In[9]:= Grid[{Range[15]!},ItemSize->Full]
Out[9]= 1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000
```

当然, 这时整个网格过宽而不能在一行中被容纳(除非将这个窗口设置的很宽), 因此网格中有的元素无法被看到. 这把我们带到另一种横向布局函数:  `Row` .

给定元素的列表, `Row` 将使整体结果按自然方式自动换行, 就像一个文本行或数学行一样.

```mathematica
In[10]:= Row[Range[15]!]
Out[10]= 126241207205040403203628803628800399168004790016006227020800871782912001307674368000
```

正如您所看到的,  默认时 `Row` 不在元素间留空格. 但若给出第二个参数, 则表达式将被插入元素之间. 可以使用任何形式的表达式, 此处用的是一个逗号.

```mathematica
In[11]:= Row[Range[15]!, ","]
Out[11]= 1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,6227020800,87178291200,1307674368000
```

如果调整笔记本窗口的尺寸, 将看到设置为 `ItemSize->Automatic` 时的 `Grid` 其行为仍与 `Row` 不同, 每一个在不同的情形中都有用.

## 将输出用作输入

这是一个很好的机会来指出 `Style`, `Grid` 及其它框符生成器在输出中是持久的.
如果所取的一段输出中的某些格式是由 ``Style`` 或者 `Grid` 创建并作为输入被再次使用, 则 ``Style`` 或 `Grid` 表达式将在输入表达式中出现.

将这个具有许多嵌入样式的 `Grid` 命令的输出用作某个输入表达式.

```mathematica
In[12]:= Grid[Partition[Take[ptable,16],4], Alignment -> Right, Frame -> True, Background -> LightBlue]
Out[12]= ...

In[13]:= (Out[12]+5)^3//Expand
Out[13]=
```

请注意这仍然是一个网格, 仍是蓝色的, 其元素仍然像以前一样为粗体或灰色.
还要注意, 表达式中有 `Grid` 和 `Style` 起到了干预效果, 否则会给一个矩阵添加一个标量, 并将结果进行乘幂.

这种区分是非常重要的, 因为往往希望不要将这些复合结构以某种方式自动解释.
但是若想摆脱这些封装并获得你的数据, 这也是很容易做到的.

```mathematica
In[15]:= % //. {Grid[a_,___]:>a,Style[a_,___]:>a}
Out[15]= {{216,343,512,729},{1000,1331,1728,2197},{2744,3375,4096,4913},{5832,6859,8000,9261}}
```

## 特殊网格条目(Special Grid Entries)

为了让二维布局更灵活,  `Grid` 接受 `SpanFromLeft` 等一些特殊符号作为条目.
条目 `SpanFromLeft` 表明, 紧靠左边的网格条目既占用自己的空间也占用跨越字符的空间.

类似的还有 `SpanFromAbove` 和 `SpanFromBoth`. 详细信息请参见 "Mathematica 中的网格, 行和列" 一节.

```mathematica
In[16]:= Grid[{
{1,2,3,4,5},
{6,7,SpanFromLeft,SpanFromLeft,10},
{11,SpanFromAbove,SpanFromBoth,SpanFromBoth,15},
{16,17,18,19,20}}, Frame->All]
Out[16]=...
```

这种方法可以用来创建复杂的跨度设置. 用键盘进行下列输入需要很长的时间.
幸运的是, 您可以在 `插入->表格/矩阵` 子菜单中使用 合并 和 分开 交互地创建此表.

用 `InputForm` 如何进行键盘输入.

我们已经看到了如何作为一个整体或针对个别列或行, 在网格中进行对齐方式和背景的设置.
我们还没有看到的是如何针对单个元素对设置进行覆盖.

假设您希望您的整个网格中除一些特殊元素外都有相同的背景, 一个方便的方法将每一个这种元素封装在 `Item` 中, 然后指定 `Item` 的选项, 覆盖 `Grid` 中相应的选项.

```mathematica
In[18]:= Grid[Partition[Table[If[PrimeQ[i],Item[i, Background -> LightYellow],i],{i,1,100}],10], Background->LightBlue]
Out[18]=...
```

也可以通过 `Style` 来覆盖该选项, 但 `Item` 的目的是使覆盖的方式知道 `Grid` 的二维布局.
注意到在前面的输出中, 一旦两个黄色单元彼此相邻, 则两者之间没有蓝色空格, 这只能通过 `Item` 实现.

不仅仅是 `Background` , 对于 `Item` 的所有选项均是如此.
现在来看 `Frame` 选项, 如果只想在某些特定元素周围加框架, 而其它部位不加,
您很可能认为必须在这些元素自己的 `Grid` 中进行 `Frame->True` 的设置来完成.
(在下一小节中我们将学习一种更简单的方法来给任意一个表达式加框架.)

```mathematica
In[19]:= Grid[Partition[Table[If[PrimeQ[i],Grid[{{i}},Frame->True],i],{i,1,100}],10]]
Out[19]=...
```

但请注意相邻框架元素不分享它们的边界.
相比之下, 下面使用 `Item` , 有足够的信息画出不必要的框架元素.

注意现在`2`和`11`的框架交于一点, 以及`2`和`3`的框架如何共享一个像素的线, 而这又完全与`13`和`23`的左边框对齐. 这就是 `Item` 的强大功能.

```mathematica
In[20]:= Grid[Partition[Table[If[PrimeQ[i],Item[i, Frame -> True],i],{i,1,100}],10]]
Out[20]=...
```

## 框架和标签

为一个表达式添加框架或标签可以通过 `Grid` 完成, 但添加框架在概念上比一般的二维布局简单得多, 所以有相应更简单的方法来达到这个目的.
例如 `Framed` 是一个简单的函数, 用于在任意表达式周围绘制一个框架, 这样做可以将注意力吸引到表达式的各个部分.

```mathematica
In[21]:= Table[If[PrimeQ[i],Framed[i, Background -> LightYellow],i],{i,1,100}]
Out[21]=...
```

`Labeled` 也是这样的一个函数, 它允许在给定表达式周围的任意一处加标签. 此处我们给上一小节的 `Grid` 例子加上图例. (`Spacer` 是为留空格设计的函数.)

```mathematica
In[22]:= Labeled[
Grid[Partition[ptable,10], Alignment -> Right, Frame -> True],
Text[Row[{Style["\[Bullet] Prime",Bold], Style["\[Bullet] Composite",Gray]},Spacer[15]]]]
Out[22]=
```

`Panel` 是另一个构建框架的函数, 它使用底层操作系统的面板框架.
这不同于 `Frame`, 因为不同的操作系统可能会使用阴影, 圆角或用于面板框架的花哨的图形设计元素.

```mathematica
In[23]:= Panel[Labeled[
 Grid[Partition[ptable, 10], Alignment -> Right, Frame -> True],
 Text[Row[{Style["\[Bullet] Prime", Bold],
    Style["\[Bullet] Composite", Gray]}, Spacer[15]]]]]
Out[23]=
```

注意 `Panel` 对于字体类和字体尺寸也有自己的定义, 因此 `Grid` 的内容改变字体类和尺寸, `Text` 也改变字体尺寸.
(尽管如此 `Text` 关于字体类有自己的定义, 且保持 Wolfram 语言中的文本字体.)
在关于 `BaseStyle` 选项的小节中, 我们将对此进行较深入地探讨.

最后应该指出的是, `Panel` 自身有一个可选的第二个参数, 用于指定一个或多个标签, 它会自动在面板以外定位, 还有一个可选的第三个参数, 用于给出该位置的细节.
详见 Panel 的参考资料.

```mathematica
In[24]:= Panel[ptable, "Primes and Composites"]
Out[24]=
```

```mathematica
In[25]:= Panel[ptable, {"Primes and Composites"}, {{Bottom,Right}}]
Out[25]=
```

## 其它注释

到目前为止所提到的注释都有一个非常明确的可视组件. 还有一些注释在用户需要它们之前实际上是不可见的.
例如 Tooltip 不改变其第一个参数的显示, 只有当您将鼠标指针在显示部分移动时, 第二个参数才作为一个提示条(tooltips)出现.

```mathematica
In[26]:= Table[Tooltip[i,Divisors[i]],{i,1,100}]
Out[26]=
```

`Mouseover` 也属于这类函数, 但不是在提示中显示结果, 它使用的屏幕区域与后来鼠标指针在上移动的区域相同.
如果这两个显示的大小不同, 那么效果会不和谐, 因此使用大小相近的显示, 或者使用 `Mouseover` 中的 ImageSize 选项给两个中较大的显示留出空间, 无论正在显示哪一个.

```mathematica
In[27]:= Table[Mouseover[i,Framed[Divisors[i], Background->LightYellow]],{i,1,100}]
Out[27]=
```

与 `Tooltip` 类似的是 `StatusArea` 和 `PopupWindow`.
`StatusArea` 在笔记本的状态区域(status area)显示额外信息, 该区域通常在左下角, 而 `PopupWindow` 将在点击时将额外信息显示在一个新窗口中.

```mathematica
In[28]:= Table[StatusArea[i,Divisors[i]],{i,1,100}]
Out[28]=
```

```mathematica
In[29]:= Table[PopupWindow[i,Divisors[i]],{i,1,100}]
Out[29]=
```

最后, 您可以通过成对使用 `Annotation` 和 `MouseAnnotation` 为一个注释指定一个任意位置.

```mathematica
In[30]:= Table[Annotation[i,Divisors[i],"Mouse"],{i,1,100}]
Dynamic[MouseAnnotation[]]
Out[30]=
```

当使用的注释仅仅通过在屏幕的一个区域移动鼠标指针而触发时, 考虑用户是很重要.
移动鼠标不应该引发长时间的计算或很多的视觉混乱. 但是谨慎的使用注释可以给用户带来很大的帮助.

最后, 请注意所有这些注释在图形中也同样适用. 因此, 你可以提供提示条(tooltips)或鼠标悬停(mouseovers)协助用户了解您所创建的复杂图形.
其实, 连 `ListPlot` 或者 `DensityPlot` 这样的可视化函数也支持 `Tooltip`. 详细情况请参见参考资料.

```mathematica
In[32]:= Graphics[{LightBlue,EdgeForm[Gray],Tooltip[CountryData[#,"SchematicPolygon"],#]&/@CountryData[]}, ImageSize->Full]
Out[32]=
```

## 默认样式

正如我们在"框架和标签"小节中所看到的, 对 `Panel` 等的创建实际上类似于 `Style`, 因为它们设置了一个能使一组默认样式应用于其内容的环境.

这可以通过明确的 `Style` 命令进行覆盖, 也可以被 `Panel` 自身通过 `BaseStyle` 选项覆盖. `BaseStyle` 可以被设置为一种样式, 或者是一列样式指令, 正如在 `Style` 中的用法一样, 并且这些指令成为该 `Panel` 范围内的默认环境.

我们已经看到, `Panel` 在默认情况下使用对话框字体系列和尺寸, 但是这可以用 `BaseStyle` 选项进行覆盖.

```mathematica
In[33]:= Panel[Range[10]]
Out[33]= {1,2,3,4,5,6,7,8,9,10}
In[34]:= Panel[Range[10], BaseStyle -> {"StandardForm"}]
Out[34]= {1,2,3,4,5,6,7,8,9,10}
```

事实上, 几乎所用的框符生成器都有一个 `BaseStyle` 选项. 例如这里是一个默认字体颜色为蓝色的网格,
注意灰色的元素保持灰色, 因为内部的 `Style` 封装胜过外围 `Grid` 的 `BaseStyle`.
(这是选项可继承性的主要特征之一, 它超出了本文的讨论范围.)

```mathematica
In[35]:= Grid[Partition[ptable, 10], BaseStyle -> {FontColor -> Blue}]
Out[35]=
```

## 默认选项

假设您有一个表达式, 多次出现同一框符生成器, 如一个 `Framed` 或 `Panel`, 您想将它们全部改变, 使之含有相同的选项集合.
在函数每一次出现时都添加相同的选项集可能会非常繁琐,  幸好这里有一个更简单的方法.

`DefaultOptions` 是 `Style` 的一个选项, 当被设置成形如 `head->{opt->val,...}` 的一列元素时, 该选项会用所给选项设置一个环境, 作为给定框符生成头部的默认环境.

在整个 `Style` 的封装内这些选项都是激活状态, 但只针对于相关联的框符发生器.

假设您有一个表达式含有某些 `Framed` 项, 你希望所有这些项都用相同背景和框架样式画出.

```mathematica
In[36]:= Table[If[PrimeQ[i],Framed[i],i],{i,1,100}]
Out[36]=
```

事实上, 这个输入太短, 不能看到该语法的优越性. 但是如果您手动指定同一列表, 就可看出它的优越性.

```mathematica
In[37]:= biglist = {1,Framed[2],Framed[3],4,Framed[5],6,Framed[7],8,9,10,Framed[11],12,Framed[13],14,15,16,Framed[17],18,Framed[19],20,21,22,Framed[23],24,25,26,27,28,Framed[29]
,...,100}
```

现在在每一个 `Framed` 的封装内插入 `Background` 和 `FrameStyle` 选项会非常耗时, 尽管您一定能做到(或者通过写一段程序来为您完成).

而使用 `DefaultOptions`, 您可以有效的设置一个环境, 使得所有的 `Framed` 封装都使用您对 `Background` 和 `FrameStyle` 的设置.

```mathematica
In[38]:= Style[biglist, DefaultOptions -> {Framed -> {Background -> LightYellow, FrameStyle->Blue}}]
Out[38]=
```

这种方法可以方便地建立遵循统一样式的结构, 而不必将样式在多处进行指定, 这可以产生相对清晰的代码和更小的文件, 也更易于维护.

## 数学排版

没有格式化输出的讨论将是不完整的, 至少要提及数学语法中所特有的格式结构.

```mathematica
In[39]:= {Subscript[a,b],Superscript[a,b],Underscript[a,b],Overscript[a,b], Subsuperscript[a,b,c], Underoverscript[a,b,c]}
```

我们将不对此进行详细, 但我们会指出, 这些结构在内核中没有任何内置的数学意义.

例如, `Superscript[a,b]` 不会被解释为 `Power[a,b]`, 尽管两者的显示相同.
因此, 您可以在格式化输出时将这些作为结构元素使用, 而不必担心它们的意义会影响您的显示.

```mathematica
In[40]:= Table[Row[{i,Row[Superscript @@@ FactorInteger[i],"*"]},"=="],{i,100}]
```

## 使用框符语言(Box Language)

最后一点说明是, 对于已经很熟悉框符语言的用户可能偶尔会发现, 这些框符生成器在您构建自己的底层框符时会产生阻碍,
然而, 通过一个简单的漏洞, 您可以将有效的框符直接显示在输出中: `RawBoxes`.

```mathematica
In[41]:= {a,b,RawBoxes[SubscriptBox["c","d"]],e}
Out[41]= {a,b,Subscript[c, d],e}
```

正如和其它所有漏洞一样, `RawBoxes` 给了您更高的灵活性, 但它也可以让您搬起石头砸自己的脚, 请小心使用.

## TextString

`TextString[expr]`; 将`expr`转换成人类可读的字符串表示.
`TextString[expr]` 还支持一些特殊功能, 例如`TextString[Now]`格式化现在的时间.

## Dividers,Frame

`Dividers` 的相当于对已有 `Frame` 规定的补充:

```mathematica
Grid[Table[x, {4}, {7}], Dividers -> {{2 -> Red, -2 -> Blue}, {2 -> Red, -2 -> Blue}}, Frame -> True]
```

`FrameStyle` 将为网格中的所有线条设置默认样式:

```mathematica
Grid[Table[x, {4}, {7}],  Dividers -> {{2 -> Red, -2 -> Blue}, {2 -> Red, -2 -> Blue}}, Frame -> True, FrameStyle -> Thickness[5]]
```
