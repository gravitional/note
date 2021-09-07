# mathematica

## 坐标变换

+ 一般性的教程在：tutorial/ChangingCoordinateSystems

+ 使用`CoordinateChartData`查看某种坐标的数据, 如查看球坐标的度规:

```mathematica
CoordinateChartData["Spherical", "Metric", {r, \[Theta], \[CurlyPhi]}] 
```

+ 使用`CoordinateTransformData`查看坐标变换的数据, 例如新旧坐标间的映射矩阵, 以及新旧正交基之间的变换矩阵. 

```mathematica
CoordinateTransformData[ "Spherical" -> "Cartesian", "Mapping", {r, \[Theta], \[CurlyPhi]}]
```

+ 使用`TransformedField`对函数进行坐标变换, 得到新的坐标系下的函数. 例如:

```mathematica
TransformedField["Polar" -> "Cartesian",  r^2 Cos[\[Theta]], {r, \[Theta]} -> {x, y}]
```

+ 使用`CoordinateTransform`对离散的点进行坐标变换, 例如：

```mathematica
CoordinateTransform[ "Polar" -> "Cartesian", {r, \[Theta]}]
CoordinateTransform[{"Cartesian" -> "Polar", 2}, {1, -1}]
```

## 图像处理

### 图像平滑

guide/ImageRestoration

图像邻域处理
guide/ImageFilteringAndNeighborhoodProcessing

## 颜色

### ColorData

mathematica 有一些默认的颜色集合, 可以通过 `ColorData` 调用, 可以用整数指定索引, 例如`ColorData[63]`. 
在`LineLegend`这样的函数中, 如果在颜色参数的位置给一个整数, 会自动调用`ColorData[..]`.

## 群论

### 置换群

#### Permute,置换操作

置换可以表示成置换列表的形式, 即对`{1,2,3,..}`的重排结果.
也可以表示成不相连循环的形式, 即类似`{123}{45}{78}`, 每一组内部独立进行轮换.

+ `Permute[expr,perm]` ; 根据置换 `perm` 对 `expr`元素的位置进行排列. 
+ `Permute[expr,gr]` ; 返回 `expr` 在置换群`gr`的元素作用下形式. 

+ `Permute`适用于任何非原子表达式, 对表达式的第一层进行操作. 
+ `permute`对表达式的元素进行重新排序, 但不改变其长度. 
+ 置换`perm`可以用不相连的轮换形式给出, 或者以置换列表的形式给出.
+ 当`perm`以循环形式给出时 `Cycles[cyc1,cyc2,...]}]`, 轮换 `{p1,p2}`以循环的方式移动`expr`的元素, 使`expr[[pi]`被移动到位置`p(i+1)`.
+ 当`perm` 以置换列表的形式给出, 其结果等同于使用 `Permute[expr, PermutationCycles[perm] ]`. 
+ 置换群 `gr` 可以由生成元指定, 头部为 `PermutationGroup`, 或者用名称指定, 头部为 `SymmetricGroup`, `AlternatingGroup`, ...

```mathematica
Permute[{a, b, c, d, e}, Cycles[{{1, 3, 2}}]]
{b, c, a, d, e}
```

#### Cycles,轮换的表示

+ `Cycles[ {cyc1, cyc2, ... }]` ; 代表一个具有不相连轮换`cyci`的置换.
+ 置换中的循环`cyc1]`是用正整数列表的形式给出的, 代表置换作用的位置.
+ 循环`{p1, p2, ..., pn}` 代表把`pi`映射到`p(i+1)`. 最后一个点`pn`被映射到`p1`.
+ 不包括在任何循环中的点被认为映射到它们自己.
+ 循环必须是不相交的, 也就是说, 它们必须没有公共点, 参数必须都是正整数.
+ 循环对象被自动正规化, 通过丢弃空循环和单一循环, 旋转每个循环使最小的点先出现. 并按第一个点对多个循环排序. 
+ `Cycles[{}]`代表恒等置换.

#### 置换形式的转换

+ `PermutationCycles[perm]`; 给出置换 `perm` 的不相交的轮换表示.

+ 输入的 `perm` 可以是一个置换列表, 也可以是不相连的循环.
+ 置换列表是连续正整数`{1,2,..,n}`的重新排序.
+ `PermutationCycles[perm]`返回一个表达式, 头部是`Cycles`, 并包含循环的列表, 每个循环的形式为`{p1, p2, ..., pn}`, 它表示`pi`到`p(i+1)`的映射.
最后一点`pn`被映射到`p1`.
+ `PermutationCycles[perm,h]`返回一个头部为`h`的表达式.
+ `PermutationCycles`的结果自动正规化, 通过旋转每个循环使最小的点出现在前面, 并通过首位的点对循环进行排序.

```mathematica
PermutationCycles[{2, 5, 3, 6, 1, 8, 7, 9, 4, 10}]
Cycles[{{1, 2, 5}, {4, 6, 8, 9}}]
```

+ `PermutationList[perm]` ; 给出置换`perm` 的置换列表形式.
 
+ `PermutationList[perm,len]`; 返回一个长度为 `len` 的置换列表.
+ 置换`perm`的输入形式可以是置换列表, 也可以是不相连的循环.
+ `PermutationList` 也可以作用到 `SparseArray` 对象上.

```mathematica
PermutationList[Cycles[{{3, 2}, {1, 6, 7}}]]
{6, 3, 2, 4, 5, 7, 1}
```

#### 生成置换组合

`Permutations[list]`; 生成 `list` 中元素的所有可能的排列组合的列表. 
`Permutations[list,n]` ; 给出所有最多包含 `n` 个元素的排列组合. 
`Permutations[list,{n}]` ; 给出所有正好包含 `n` 个元素的排列组合. 
`Signature[list]`; 给出置换的`signature`, 这个置换能把列表中的元素恢复成标准顺序.

### 置换的运算

`PermutationProduct `
`InversePermutation`
`PermutationPower`
`FindPermutation`

## 笔记本底层结构

### 通过内核操作笔记本

tutorial/ManipulatingNotebooksFromTheKernel

在 Wolfram 语言笔记本中进行简单运算时, 用标准 `Wolfram` 语言前端交互功能是非常方便的. 但要进行复杂和系统的运算时, 最好要使用内核

+ `Notebooks[]`   所有打开的笔记本集合
+ `Notebooks["name"]`   所有打开的有指定名的笔记本集合
+ `InputNotebook[]`   用于输入的笔记本
+ `EvaluationNotebook[]`   对这个函数正进行计算的笔记本
+ `ButtonNotebook[]`   可能包含启动这个计算的按钮的笔记本

在 Wolfram 语言中, 笔记本前端中能交互进行的任何工作也可以从内核向前端发送适当的指令进行.

+ `Options[obj]` 给出对应于笔记本对象 obj 的笔记本的所有选项集合
+ `Options[obj,option]` 给出一个选项的设置
+ `AbsoluteOptions[obj,option]` 即使当实际设置是 Automatic 时给出绝对选项值
+ `CurrentValue[obj,option]=rhs` 给出并且设置 option 的值
+ `SetOptions[obj,option->value]` 设置选项的值

这里改变屏幕上当前所选笔记本的尺寸：

```mathematica
SetOptions[InputNotebook[], WindowSize -> {250, 100}]
```

另一方面, 可以使用 CurrentValue 直接获得 WindowSize 的选项值：

```mathematica
CurrentValue[InputNotebook[], WindowSize]
```

这里对 CurrentValue 使用简单的赋值来改变选项：

```mathematica
CurrentValue[InputNotebook[], WindowSize] = {400, 300}
```

#### 通过内核移动笔记本

在任何打开的笔记本中, 前端总是保持当前的选择, 这个选择由一个单元中的文本区域组成, 或者是由这个单元组成. 
通常这个选择在屏幕上是由一个高亮度形式表明. 这个选择也可以在文本的两个字符之间, 或者在两个单元之间, 这时它在屏幕上由两个竖直或水平的插入杠来表明.

#### 查找笔记本的内容

将当前选择移到前一个词 `cell` 出现的位置：

```mathematica
NotebookFind[nb,"cell",Previous]
```

#### 为整个笔记本和当前选择寻找和设置选项

+ `Options[obj,option]`  找出完整笔记本的一个选项值
+ `Options[NotebookSelection[obj],option]`  找出当前选择的值
+ `SetOptions[obj,option->value]` 设置完整的笔记本的一个选项值
+ `SetOptions[NotebookSelection[obj],option->value]`  设置当前选择的值

#### 以整个笔记本为对象的操作:

+ `CreateWindow[]`  产生一个新笔记本
+ `CreateWindow[options]`  产生一个具有指定选项的笔记本
+ `NotebookOpen["name"]`  打开一个已有的笔记本
+ `NotebookOpen["name",options]`  打开一个具有指定选项的笔记本
+ `SetSelectedNotebook[obj]`  选择一个指定的笔记本
+ `NotebookPrint[obj]`  打印一个笔记本
+ `NotebookPrint[obj,"file"]`  将一个笔记本的 PostScript 版本输出到一个文件
+ `NotebookPrint[obj,"!command"]`  将一个笔记本的 PostScript 版本送到一个外部命令
+ `NotebookSave[obj]`  将笔记本的当前版本存入一个文件
+ `NotebookSave[obj,"file"]`  将笔记本存为一个给定文件名的文件
+ `NotebookClose[obj]`  关闭一个笔记本

调用 `CreateWindow[]` 时, 屏幕上出现一个空笔记本.

执行 `SetSelectedNotebook` 和 `NotebookOpen` 等指令时, 就是让 Wolfram 语言改变所看到的窗口. 
在`NotebookOpen` 和 `CreateWindow` 中使用选项设置 `Visible->False` 可以处理笔记本, 但不将它显示在屏幕上.

#### 通过内核操作前端

+ `$FrontEnd`  当前使用的前端
+ `Options[$FrontEnd,option]`  前端全局选项的设置
+ `AbsoluteOptions[$FrontEnd,option]`  选项的绝对设置
+ `SetOptions[$FrontEnd,option->value]`  在前端重设选项
+ `CurrentValue[$FrontEnd, option]`  返回选项值, 当用于赋值的左边时, 允许设置选项.

### 在前端操作全局选项

+ 前端令牌; 通过前端令牌, 用户可以执行通常情况下使用菜单才能实现的内核命令. 前端令牌对于编写操作笔记本的程序特别方便.

`FrontEndToken` 是一个内核命令, 它的变量为前端令牌. `FrontEndExecute` 是一个将其变量发送到前端来执行的内核命令.
例如, 下面命令创建一个新的笔记本.

```mathematica
FrontEndExecute[FrontEndToken["New"]]
```

#### 在前端直接执行笔记本指令

在执行 `NotebookWrite[obj,data]` 等指令时, 向笔记本中插入数据的实际操作是在前端进行的. 但为了估算原来的指令和构造送向前端的适当请求, 还是要使用内核的. 不过, 前端可以直接执行一定量的指令, 而不需涉及内核.

区分指令的内核和前端版本.

```mathematica
NotebookWrite[obj,data] 在内核执行的 NotebookWrite  版本指令
FrontEnd`NotebookWrite[obj,data] 在前端直接执行的 NotebookWrite 版本指令
```

`Wolfram` 语言区分在内核执行的指令和前端直接执行的指令的基本方式是使用上下文.
内核指令通常在 `` System` `` 上下文中, 而前端指令通常在 `` FrontEnd` `` 上下文中.

+ 把表达式发送到前端执行; `FrontEndExecute[expr]` 把 `expr` 发送到前端执行

在书写操纵笔记本的精细复杂的程序时, 这些程序必须在内核执行但对于通过简单按钮所进行的运算, 可以在前端直接执行所需要的所有指令, 甚至不需要运行内核.

#### 复合令牌

复合令牌有可以使用令牌参数来控制其行为的某些方面. 对于复合令牌, `FrontEndToken`的三个参数必须是一个`NotebookObject`, `前端令牌`和选定的`令牌参数`.
例如, 以下命令将选定的笔记本保存为纯文本.

```mathematica
FrontEndExecute[{FrontEndToken[FrontEnd`InputNotebook[], "SaveRenameSpecial", "Text"]}]
```

此外, 使用内核命令也能保存笔记本, 但只能保存为`.nb`格式.
`NotebookSave[notebook,"file"]`; 将笔记本保存到特定的文件当中.

### 前段令牌和命令的对应关系

`FrontEndToken[]`命令的帮助页面提到, `前段令牌`和`菜单项`或`键盘快捷键`之间的映射在`前端文本资源`中定义. 
在笔记本中输入`$InstallationDirectory // SystemOpen` 命令打开安装目录. 例如`/usr/local/Wolfram/Mathematica/12.2/`
再依次打开`../SystemFiles/FrontEnd/TextResources`, 这个目录就是`前端文本资源`.

其中`CommonFrontEndInit.tr`文件定义了笔记本页面菜单栏功能的函数实现. 例如与保存相关的功能定义如下: 

```mathematica
AddFileBrowserFilterPacket["Save", {
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "NBLabel"], FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "Notebook"}]]&, "nb", "*.nb", MacintoshFileTypes->{"TEXT"}],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "CDFLabel"], FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "CDFNotebook"}]]&, "cdf", "*.cdf", MacintoshFileTypes->{"TEXT"}],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "WLLabel"], FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "Package"}]]&, "wl", "*.wl", MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportPackage.nb"],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "MLabel"], FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "Package"}]]&, "m", "*.m", MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportPackage.nb"],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "WLSLabel"], FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "Script"}]]&, "wls", "*.wls", MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportPackage.nb"],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "PlainTextLabel"], FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "PlainText"}]]&, "txt", "*.txt", MacintoshFileTypes->{"TEXT"}],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "LatexLabel"],
          FrontEnd`SaveAs[#1, #2, "TeX"]&,
          "tex", "*.tex", MenuEvaluator->Automatic, MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportTeX.nb"],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "PostScriptLabel"], FEPrivate`FrontEndExecute[NotebookPrint[#2, #1]]&, "ps", "*.ps", MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportVectorFormat.nb"],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "PDFDocLabel"], FEPrivate`FrontEndExecute[NotebookPrint[#2, #1]]&, "pdf", "*.pdf", MacintoshFileTypes->{"PDF "}, OptionsNotebook -> "ExportPDF.nb"],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "RichTextFormatLabel"], FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "RTF"}]]&, "rtf", "*.rtf", MacintoshFileTypes->{"RTF "}],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "WebPageLabel"],
          FrontEnd`SaveAs[#1, #2, "XHTML"]&,
          "html", {"*.htm", "*.html"}, MenuEvaluator->Automatic, MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportXHTML.nb"],
     Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "XMLLabel"],
          FrontEnd`SaveAs[#1, #2, "XHTMLMathML"]&,
          "xml", {"*.xml", "*.htm", "*.html"}, MenuEvaluator->Automatic, MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportXHTMLMathML.nb" ]
}];
```

保存成包文件`.wl`格式的命令为

```mathematica
Item[FEPrivate`FrontEndResource["CommonFrontEndInitDialogs", "WLLabel"], 
FEPrivate`FrontEndExecute[FrontEndToken[#2, "Save", {#, "Package"}]]&, 
"wl", "*.wl", MacintoshFileTypes->{"TEXT"}, OptionsNotebook -> "ExportPackage.nb"]
```

`item`和`FrontEndResource` 是前端相关的函数, 真正执行保存的语句是`FrontEndExecute`. 如果想在笔记本中或脚本中使用, 可以使用如下方式:

```mathematica
FrontEndExecute[FrontEndToken[FrontEnd`EvaluationNotebook[], "Save", {
    StringTrim[NotebookFileName[], ".nb" ~~ EndOfString] <> ".wl", "Package"
    }]]
```

其中`` FrontEnd`EvaluationNotebook[] `` 是当前运行的笔记本. 
`StringTrim[NotebookFileName[], ".nb" ~~ EndOfString] <> ".wl"`替换当前笔记本路径的拓展名为`.wl`. 
`"Package"`指定保存格式为`.wl`格式.

根据[programmatically saveas](https://mathematica.stackexchange.com/questions/6982/is-there-a-way-to-programmatically-do-a-save-as-on-an-init-nb-file-to-save-it)上老哥的说法, 使用下面两条命令效果也一样.

```mathematica
FrontEndExecute[ FrontEndToken[FrontEnd`EvaluationNotebook[],   "SaveRename", {"/../test.wl", "Package"}]]
FrontEndTokenExecute[FrontEnd`InputNotebook[],"SaveRename", {"/../test.wl", "Package"}
]
```

此外老哥还提到:

+ 这相当于手动执行`文件>另存为...`选择`Mathematica软件包(*.m)`, 保存笔记本的`初始化单元`. 而其他单元被保存为`(*注释*)`, 后者在脚本中调用的时候不会执行.
+ 如果您不提供文件路径, 文件将被保存在`$HomeDirectory`中, 这通常不是您想要的地方. 
+ 如果文件不能被保存, 不会发出警告信息. 

## FeynCalc

### FeynArts

[FeynArts in FeynCalc](https://github.com/FeynCalc/feyncalc/wiki/FeynArts)

如果你使用自动安装程序安装`FeynCalc`的稳定版或开发版, 你会被问到是否应该下载和修补最新版本的`FeynArts`. 因此, 不需要额外的步骤. 
然而, 你可能会想更新你的`FeynArts`版本而不重新安装`FeynCalc`. 在这种情况下, 请遵循以下步骤. 

下载最新版本的`FeynArts`, 并将`tarball`解压到

```mathematica
<< FeynCalc`
Print[$FeynArtsDirectory]
```

启动 `Mathematica` 并输入

```mathematica
$LoadFeynArts = True;
<<FeynCalc`; 
```

将出现一个对话框, 询问您是否要对`FeynArts`打补丁. 点击确定, 等到修补过程结束.
重启`Mathematica`内核, 并尝试运行一些示例代码(点击`FeynCalc`加载时出现的横幅上的示例链接). 确保一切都能正确运行, 没有任何警告和错误. 
