# mathematica.md

+ 参数序列比较短的时候可以直接用`List[...]`,
+ 参数比较长, 但不是巨大规模时, 用关联收集参数.
+ 参数规模非常大时, 列表可以节省空间, 关联可以提高查找效率.

## 常用脚本

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

## 使用样式表

tutorial/WorkingWithStylesheets

根据笔记本界面所提供的任意或全部可用选项, Wolfram 系统使用样式表控制笔记本的行为和外观.
样式表是笔记本的各种特殊单元的集合, 它被其他笔记本引用, 或者作为笔记本选项的一部分应用. 在后一种情形中, 我们称样式表作为私有或本地样式表在笔记本内部"嵌入".

***
与样式表相关的函数.

+ `StyleDefinitions` 笔记本选项, 指定具有样式定义的文件能够在笔记本中使用
+ `StyleData["style"]` 样式定义单元的内容的低层表示

样式表是包含 `StyleData` 单元的笔记本. 一个简单的样式表笔记本表达式可以像下面所展示的这个例子这么简单.

```mathematica
Notebook[{
 Cell[StyleData["Section"],
   FontColor->Gray],
 Cell[StyleData["Subsection"],
   FontFamily->"Helvetica"]
 }]
```

上例仅定义了两种样式： `Section` 和 `Subsection`. 两者的定义仅使用了一个选项.
在没有更多信息的情况下, `Section` 和 `Subsection` 单元在工作笔记本中没有任何不同. 它们的大小相同(正常文本), 并具有相同的边幅, 一个将是灰色的, 另一个将是黑色的, 并且它们的字体也将不同.

### 继承

继承是一个与级联样式表相关的概念--从不同层次的引用所获得的格式设置.
样式表中的第一个单元通常引用已有的样式表, 如 Default.nb, 点击它可以看到它引用的是 Core.nb.

该累积效应堆栈的最终是工作文档的实际单元. 如果它应用了一个选项, 如 `FontColor->Red`, 该选项将覆盖整个继承堆栈的 `FontColor` 的其他设置.

+ `$FrontEnd`  笔记本界面中所有选项的默认设置
+ `Core.nb`  定义低层单元外观和行为的基础样式表
+ `Default.nb` 可供用户选择的样式表, 对于新文档通常是 Default.nb
+ `个人样式表 (如果存在)`  编辑文档样式表的结果
+ `样式表中 "Notebook "的局部样式定义 `  应用于笔记本层次的选项的样式表的特殊样式定义
+ `样式表中的样式环境设置`  下节中讨论的环境
+ `笔记本层次的选项设置`  应用于笔记本层次的设置
+ `单元层次的选项设置`  应用于单元层次的设置

### 环境

`StyleData["style","environment"]` 表示在样式环境 "environment" 中样式定义单元的内容

`StyleData` 的第二个参数通常是设置所应用的环境名称.

样式环境提供了一种无需更换样式表即可切换文档设置的方法.
典型的环境包括 `SlideShow` 和 `Printout`. 许多专门的行为依赖于文档的环境设置.
例如, 幻灯片放映能够工作是由于分页设置, 而分页设置在其他情况下与正常工作环境无关.
相似地, 打印操作也受这些相同的分页符的影响, 但 `Printout` 环境将待打印页面中所有内容按比例缩放至一个更合适的尺寸. 字体也被设置为打印所需的较高分辨率.

显示屏和打印环境的设置可以不同, 方法是使用两种不同的菜单：`格式--显示屏环境` 和 `文件--打印设置--打印环境`.
在默认情况下, 文档界面将显示屏环境设置为 `Working` , 将打印环境设置为 `Printout`.
因此, 一定要注意这不是一个"所见即所得"的配置.

### 颜色数据

ColorData["scheme"][par] 或 ColorData["scheme", par]
给出指定颜色方案中对应于参数值 par 的 RGBColor 对象.
ColorData["SiennaTones"]
ColorData["SiennaTones"][0.7]

## Wolframscripts

总的来说,

windows: 建立后缀名为`.wl`的文件, 然后按正常的方法去写`mma`笔记本,
运行的时候用`wolframscript.exe`
用`-print all` 指定输出所有没被`;` 抑制输出的表达式.
用`para1 para2 ...` 传递参数.

参数用下面的变量调用

+ `$CommandLine` -- 一系列字符串给出使用的完整的命令行.
+ `$ScriptCommandLine` -- 为正在运行的脚本准备的一系列命令行参数. 这些参数出现在 `-option` 给出的选项之后.
+ `$ScriptInputString` -- 一个通过标准输入给出脚本输入的字符串. 在脚本的每次迭代中, 选项 `-linewise` 用一行标准输入载入该变量.

```powershell
wolframscript.exe -print all -file .\test.wl para1 para2
```

***
`unix`: 通过加上`#!/usr/bin/env wolframscript -print all`,
运行的时候, 不用输入`wolframscript`,
传递参数的方法不变

```bash
./test.wl para1 para2
```

经常用到的`mma`系统变量,参考

`guide/SystemInformation` : mma 系统信息
`guide/WolframSystemSetup`: 更一般的系统设置

`$InputFileName`: 脚本的绝对路径.
`$Notebooks`：如果是用前端运行的, 则为`True`.
`$BatchInput`: 输入是否来自批处理
`$BatchOutput`:如果在命令行中输出, 则为`True`.
`$CommandLine`: 唤醒环境变量所使用的命令行,
`$ProcessID`:进程ID
`$ParentProcessID`:
`$Username`: 用户的登陆名
`Environment["var"]`:操作系统的环境变量, 如`Environment["HOME"]`

### 脚本文件

Wolfram 语言脚本是一个包含 Wolfram 语言命令的文件, 用户通常可以在一个 Wolfram 语言会话中按顺序计算这些命令.
如果这些命令需要多次重复, 编写一个脚本就是非常有用的. 把这些命令收集在一起确保它们按照特定的顺序计算, 并且没有忽略任何命令. 如果你运行复杂的, 很长的计算, 这么做是很重要的.

当您交互式地使用 Wolfram 语言时, 包含在脚本文件中的命令可以利用 `Get` 计算.
这个函数也可以通过编程在代码或者其它 `.wl` 文件中使用.

***
从一个脚本文件读取命令.

+ `Get["file"]` ; 读入一个文件, 并且运行其中的命令
+ `<<file`; `Get` 的简写形式

对脚本文件的结构没有任何要求. 在该文件中给出的任何 Wolfram 语言命令序列都会按照顺序读入并计算.
如果你的代码比一个普通命令列表更复杂, 你可能可以考虑编写一个更为结构化的程序包, 如"建立 Wolfram 语言程序包"中所述.

当我们不需要用一个交互式的会话时, 即你的脚本封装了需要执行的简单计算时, Wolfram 语言脚本就更有用了.
例如, 你的计算涉及大量计算任务, 如线性代数, 最优化, 数值积分或微分方程的解, 并且当你不使用排版功能, 动态交互或笔记本的时候.

脚本可以存储在一般的 `.wl` 程序包文件或专门的 `.wls` 脚本文件.
两种文件的概念是一样的：Wolfram 语言表达式系列, 起始处带有可选的 "shebang" 行一般用于类 Unix 操作系统(参见 Unix 脚本可执行文件).

文件类型中的唯一不同是它们双击的行为.
双击程序包文件会在笔记本程序包编辑器中打开文件；
双击脚本文件会执行文件, 如果操作系统支持的话.
脚本文件可以在笔记本界面上编辑, 但是必须使用 `文件--打开` 进行开启.

### 在 Local 内核中运行脚本

当从命令行调用 Wolfram 语言内核时, 可以使用脚本文件,  对于内核可执行文件, 一般使用以下位置.

运行 Windows 上的脚本文件.

```mathematica
"%ProgramFiles%\Wolfram Research\Mathematica\12.0\wolfram" -script file.wl
```

```mathematica
D:\mathematica\wolfram -script file.wl
```

在 Mac 上运行脚本文件.

```mathematica
/Applications/Mathematica.app/Contents/MacOS/wolfram -script file.wl
```

在 Linux 上运行脚本.

```mathematica
wolfram -script file.wl
```

`-script` 命令行选项指定 Wolfram 语言内核在一个特殊的脚本上运行, 或者以批处理模式运行.
在这种模式下, 内核读入指定的文件, 并且按顺序计算命令. 通过把输出函数的 `PageWidth` 选项设为 `Infinity` , 内核关闭默认的换行功能, 并且不显示 `In[]` 和 `Out[]` 标签.
当在该模式下运行时, 标准的输入和输出通道 `stdin`, `stdout` 和 `stderr` 不会被重定向, 数值按 `InputForm` 进行格式化.

运行带有 `-script` 选项的 wolfram 等价于利用 `Get` 命令读入文件, 唯一的不同之处是：**在计算文件中最后一个命令之后, 内核停止运行**.
这种行为可能会影响 `Wolfram Symbolic Transfer Protocol (WSTP)` 链接或者通过运行脚本创建的外部进程.

### 使用 WolframScript 运行脚本

脚本可以使用如下的 WolframScript 诠释器进行运行. `-file` 标志是可选的.

使用 WolframScript 运行脚本.

```mathematica
wolframscript -file file.wl
```

wolframscript 已经加入环境变量

WolframScript 会找到最佳的本地内核运行脚本.
如果没有找到任何本地内核, 它会连接至云端并在那里运行脚本.
程序接受任何标志以便控制使用本地还是云端的进行计算.
它还设置 **脚本参数**, 其允许脚本基于启动的形式或收到的输入来改变行为.
使用 WolframScript 的另一个好处是, 输入和输出是完全缓存的, 允许应用各种变换.
在 `WolframScript` 页面(`ref/program/wolframscript`)有对这些额外选项以及范例的详细说明.

在 Windows 和 Linux 上,  `WolframScript` 一般与 Wolfram 系统一起安装.
在 `Mac` 上, 有必要安装与 Wolfram 系统一起绑定的 "Extras" 安装器以便获取 `WolframScript`.
默认情况下, 这些安装器会把 wolframscript 放在 `PATH` 中.

### Unix 脚本可执行文件

类 `Unix` 的操作系统--以及 Windows 中的 `Unix` 环境, 例如 `cygwin` 和 `MinGW` 允许编写可执行的脚本文件, 和普通的可执行程序那样运行.
这可以通过在文件开头放上一个"解释器"行实现.
对于包含 Wolfram 语言命令的脚本也一样.

"解释器"行包括两个字符 `#!`, 它必须是文件中的首两个字符, 然后是执行文件的绝对路径以及其他参数.
为了达到跨平台和机器的最大的兼容性, 建议通过如下所示的帮助器 `/usr/bin/env` 启动WolframScript.
`env` 程序会正确找到 `PATH` 中的 `wolframscript` 并启动之.

***
一个脚本文件的范例.

```mathematica
#!/usr/bin/env wolframscript

(* generate high-precision samples of a mixed distribution *)
Print /@ RandomVariate[MixtureDistribution[
    {1,2},
    {NormalDistribution[1,2/10],
     NormalDistribution[3,1/10]}],
    10,  WorkingPrecision -> 50]
```

如果要把脚本编译成可执行文件, 你需要设置可执行权限.
之后, 就可以通过在一个 shell 提示输入脚本名称, 运行脚本.
***
使脚本可执行, 并且运行它.

```mathematica
chmod a+x script.wls
./script.wls
```

解释器行可能另外包含解释器的其他参数. 可能的参数在 WolframScript 页(`ref/program/wolframscript`)被指定.

***
使用额外参数的解释器行.

```mathematica
#!/usr/bin/env wolframscript -linewise -format XML
```

Wolfram 语言脚本无需含有 `.wl` 或 `.wls` 扩展名.
一个可执行脚本等价于在一个 Unix 操作系统中的任何其它程序的一个全功能程序, 所以它可用于其它脚本中, 在管道中, 根据任务控制的方运行等等.
每个 Wolfram 语言脚本启动自己的 `WolframKernel` 拷贝, 并且不共享变量或者定义.
注意：同步运行 Wolfram 语言脚本可能受同时运行的内核数上的许可证限制所影响.

在一个交互式的 Wolfram 语言会话中, 可以显式地读入可执行脚本文件并且对其进行计算.
如果第一行以 `#!` 字符开始的话, `Get` 命令会正常忽略脚本的第一行.

可以避免使用 `env` 程序, 但是到 `wolframscript` 的路径必须是绝对路径.
启动脚本的操作系统机制没有使用 `PATH` 或其他方法找到文件. 而且, 到诠释器的路径不能包含空格.

### Windows 上的脚本

独立的脚本也可以用在 `Windows` 上.
不像 `Unix` 类的操作系统, 这些脚本必须含有扩展名 `.wls` 以便被识别为 `Wolfram` 语言脚本.
它们可以通过双击从 Windows 浏览器中启动, 以及在命令提示符中敲入名称启动.
Unix 诠释器行, 如果出现的话, 会被机制忽略.

***
从命令提示符中启动脚本, 等价于双击.

```mathematica
> file.wls
```

在命令提示符中, 其他参数加在文件名后传递.
WolframScript 本身看不见这些参数, 但是会以参数形式传递给脚本, 下一章节会详细描述.

***
从命令提示符中启动带有两个额外参数的脚本.

```mathematica
> file.wls arg1 arg2
```

### 脚本参数

当运行一个 Wolfram 语言脚本时, 你可能常常想要通过指定命令行参数, 修改脚本行为.
Wolfram 语言代码可以通过 `$ScriptCommandLine` 访问传递给 Wolfram 语言脚本的参数.
另外, 标准输入的内容可被用为变量 `$ScriptInputString` 中的字符串.

***
变量, 给出关于脚本如何运行的信息.

+ `$ScriptCommandLine`    启用脚本的命令行
+ `$ScriptInputString`    给予脚本的标准输入的内容

***
利用一个命令行参数的脚本文件 `file.wls` 的一个范例.

```mathematica
#!/usr/bin/env wolframscript

(* generate "num" samples of a mixed distribution *)
num  = ToExpression[$ScriptCommandLine[[2]]];
Print /@ RandomVariate[
  MixtureDistribution[
    {1, 2},
    {NormalDistribution[1, 0.2],
     NormalDistribution[3, 0.1]}
  ], num,  WorkingPrecision -> 50]
```

***
运行脚本, 并且指定在 Unix 环境下的样本数.

```mathematica
./file.wls 10
```

***
运行脚本, 并且指定 Windows 中的样本数.

```mathematica
> file.wls 10
```

当在脚本中访问时, `$ScriptCommandLine` 是以脚本名称为第一个元素的一个列表, 列表的其余元素为命令行变量.
`$ScriptCommandLine` 遵循标准的 `argv[]` 习惯.
注意这完全隐藏了到解释器的路径或 `#!` 行传递的任何参数.

因为类 Unix 操作系统执行脚本的方式, `$ScriptCommandLine` 仅仅当 Wolfram 语言内核经过 `wolframscript` 调用时才设置为非空列表.

如果该脚本应该以批处理模式和标准 `Unix` 脚本模式运行, `$ScriptCommandLine` 可以用于决定当前执行模式.
那么, `$ScriptCommandLine` 和 `$CommandLine` 都应该用于访问命令行参数.

### 标准输入, 标准输出

[什么是标准输入, 标准输出(stdin, stdout)](https://blog.csdn.net/sinat_17700695/article/details/91491472)

***
要弄清什么是标准输入输出. 首先需要弄懂什么是IO.

`IO` 的 `I` 是 `Input` 的意思, `O` 是 `output` 的意思.
意味着输入和输出.

更确切的含义是：

+ `I`：从外部设备输入到内存
+ `O`：从内存输出到外部设备

而标准输入和标准输出是干什么的？它们是用于 `IO` 的.
那么它们属于 `IO` 的哪个部分？
内存？还是外部设备？

答案显然是外部设备(逻辑上的外部设备, 为什么？接着看).

***
更具体的含义？

在 linux 操作系统中, 外部设备用什么表示？是用文件.
linux 中一切设备皆是文件！
因此标准输入和输出更具体的含义是文件.

它们是哪两个文件？
它们是 `/dev/stdin` 这个文件和 `/dev/stdout` 这个文件.
也就是说所谓的标准输入和标准输出其实就是两个 linux 下的文件.

***
linux 的文件类型有：

1. 普通文件
1. 字符设备文件
1. 块设备文件
1. 目录文件
1. 链接文件
1. 管道文件
1. 套接字文件

思考一下？它们是什么文件？它们在 `/dev` 目录下, 它们是设备文件吗？

那么所谓的从标准输入读是什么意思？
逻辑上来看：
就是打开 `/dev/stdin` 这个文件, 然后把这个文件里的内容读进来.
输出到标准输出是什么意思？
逻辑上来看：
就是打开 `/dev/stdout` 这个文件, 然后把内容输出到这个文件里去.

为什么是从逻辑上来看？因为它们不是设备文件,所以它们不代表一个设备.
linux里一切皆是文件, 设备是文件, 但是文件不一定是设备！

***
那它们是什么文件？他们是`链接文件`. (可以用` ls -l /dev` 来查看 `l` 开头的就是链接文件. )

什么是链接文件？**文件内容是另一个文件的地址的文件称为链接文件**.

因此, 打开, 读或者写 `/dev/stdin` 和` /dev/stdout` 实际上是打开, 读或者写这两个文件中存放的地址对应的设备文件.

### WolframScript

ref/program/wolframscript
***
名称
wolframscript -- Wolfram 语言命令行脚本解释器

#### 概要

+ `wolframscript -code code [-cloud [cloudbase] | -local [kernelpath]] arg1 ...`
+ `wolframscript -file file|url [-cloud [cloudbase] | -local [kernelpath]] [Subscript[arg, 1] ...]`
+ `wolframscript -api url|uuid|file [-cloud [cloudbase] | -local [kernelpath]] [-args key=value ...]`
+ `wolframscript -function code [-cloud [cloudbase] | -local [-kernelpath]] [-signature type ...] [-args values ...]`

#### 说明

`Wolframscript` 在本地或云端运行 `Wolfram` 语言代码, 函数和已部署的 API,  接受
标准输入
命令行参数
文件
URL
等形式的输入.

#### 命令行代码

在本地 Wolfram Engine 上执行 Wolfram 语言代码 `2+2`：

```mathematica
wolframscript -code 2+2
4
```

在 Wolfram 云中执行 Wolfram 语言代码 `2+2`, 需要时提示输入许可验证：

```mathematica
wolframscript -cloud -code 2+2
4
```

在本地执行 Wolfram 语言代码, 对shell的输入进行转义：

```mathematica
wolframscript -code 'StringReverse["hello"]'
olleh
```

执行代码并把结果放入文件中：

```mathematica
wolframscript -code 'Graphics3D[Sphere[ ]]' -format PNG > file.png
```

#### 文件代码

从文件中执行 Wolfram 语言代码, 返回产生的最终结果：

```mathematica
$ wolframscript -file test.wl
12345
```

从本地文件中获取代码, 但是在云端运行代码：

```mathematica
$ wolframscript -cloud -file test.wl
12345
```

#### 脚本文件

设置为在本地执行 Wolfram 语言代码的文件：

```mathematica
#!/usr/bin/env wolframscript
Print[2+2]
$ ./file.wls
4
```

在 Wolfram 云中执行 Wolfram 语言代码的文件：

```mathematica
#!/usr/bin/env wolframscript -cloud
Print[2+2]
$ ./file.wls
4
```

使用命令行参数的文件：

```mathematica
#!/usr/bin/env wolframscript
Print[ToExpression[$ScriptCommandLine[[2]]]^2]
$ ./file.wls 5
25
```

一个能给出函数的文件, 其参数从命令行中来：

```mathematica
#!/usr/bin/env wolframscript -function -signature City City
GeoDistance[#1, #2]&
$ ./file.wls "New York" London
Quantity[3453.7070027090986, Miles]
```

#### 交互操作

```mathematica
在交互 `REPL` 中运行 Wolfram 语言：

$ wolframscript
Wolfram Language 12.0.0 for Microsoft Windows (64-bit)
Copyright 1988-2019 Wolfram Research, Inc.

In[1]:= 2+2

Out[1]= 4

In[2]:=
```

#### API

#### 运行云端 API

```mathematica
$ wolframscript -api https://wolfr.am/bNvKWq2U -args x=1 y=2
3
```

从云中获取 API 代码, 但在本地运行 API：

```mathematica
$ wolframscript -api https://wolfr.am/bNvKWq2U -local -args x=1 y=2
3
```

#### 更多范例

登录不同的云账户：

```mathematica
$ wolframscript -authenticate
Enter WolframID: example-user@wolfram.com
Password:

Success. Saving connection data.
```

提供证书而不使用提示：

```mathematica
$ wolframscript -username example-user@wolfram.com -password XXXXXX
Success. Saving connection data.
```

断开云端, 清除连接信息：

```mathematica
$ wolframscript -disconnect
```

颠倒输入文件中每行字符串的顺序, 并把结果写入另一个文件中：

```mathematica
$ wolframscript -code 'StringReverse[$ScriptInputString]' -linewise < file1 > file2
```

使用超时限制计算：

```mathematica
$ wolframscript -code 'Do[Print[i];Pause[1], {i,10}]' -timeout 3
1
2
3
$TimedOut
```

输出使用特殊字符：

```mathematica
$ wolframscript -code 'Alphabet["Greek"]' -charset UTF8
```

使用脚本的选项 `-print` 和 `-format` 产生一幅图像：

```mathematica
#!/usr/bin/env wolframscript -print -format PNG
ListLinePlot[RandomFunction[WienerProcess[],{0,10,0.01},10]]
```

```powershell
$ ./file.wls > plot.png
```

使用 `-print All` 选项, 打印执行脚本时产生的每个结果：

```mathematica
#!/usr/bin/env wolframscript -print All
"Using -print All print will each result"
a = 2+2; (* This line won't print because the ; suppresses output *)
a
$ ./file.wls
Using -print All will each result
4
```

创建一个由 `PermissionsKey` 保护的 `API` , 并把密钥传给 `WolframScript` , 以便访问：

```mathematica
In[1]:= CloudDeploy[APIFunction[{"n"->Integer},#n^2&],Permissions->{PermissionsKey["thekey"]->"Execute"}]
Out[1]= CloudObject[https://www.wolframcloud.com/objects/83aa0bc2-8e0c-4ef6-b314-48e0bf283196]
$ wolframscript -api 83aa0bc2-8e0c-4ef6-b314-48e0bf283196 -args n=5 -permissionskey thekey
25
```

检查 WolframScript 的版本：

```mathematica
$ wolframscript -version
WolframScript 1.2.0 for MacOSX-x86-64
```

配置使用特别的 `WolframEngine` ：

```mathematica
$ wolframscript -config WOLFRAMSCRIPT_KERNELPATH=/Applications/Mathematica.app/MacOS/WolframKernel
Configured:WOLFRAMSCRIPT_KERNELPATH=/Applications/Mathematica.app/MacOS/WolframKernel
```

#### WolframScript选项

+ 代码选项

+ `-c|-code code` -- 给出要执行的 Wolfram 语言代码.
+ `-f|-file file` -- 给出含有要执行的 Wolfram 语言代码的文件.
+ `-api url|uuid|file` -- 在指定的 URL 使用 API, 或来自于有指定 UUID 的云端或本地对象, 也可以来自于指定的本地文件. 使用参数 key=value .......
+ `-fun|-function code [-s|-signature type ...] [-args|-- value ...]` --  使用参数为字符串 `value` ...... 的函数, 将其解释为类型 `type` ....... 如果没有给出标记 (signature), 则假设所有参数都是字符串. 标记类型可以是 `$InterpreterTypes` 的任意一种.

+ 执行选项

+ `-o|-cloud [cloudbase]` -- 在云端执行代码, 使用指定的云基 (cloud base). 缺省情况下, cloudbase 为 https://wolframcloud.com.
+ `-l|-local [kernelpath]` -- 在本地执行代码, 使用到 Wolfram Engine 核的指定路径. 缺省情况下, kernelpath 使用本地系统中 Wolfram 语言的最新版本.
+ `-format type` -- 指定输出的格式. 可以使用任意可被 Export 接受的格式.
+ `-charset encoding` -- 对输出使用 encoding. 如果想输出原始字节, 编码可设为 None, 或  $CharacterEncodings 中的任意一项, 除了 "Unicode". 默认情况下, 从终端的语言设置中推断.
+ `-linewise` -- 执行从标准输入读入的每行代码.
+ `-print [all]` -- 当运行脚本时, 将执行脚本最后一行的结果打印出来, 在指定 all 时, 打印所有行的结果.
+ `-timeout seconds [value]` -- 指定执行可以使用的秒数. 如果超出指定时间, 返回 value 的值.
+ `-v|-verbose` --  在执行中打印额外信息.

+ 实用选项

`-h|-help` -- 打印帮助信息.
`-version` -- 打印 WolframScript 版本.
`-auth|-authenticate [wolframid [password]] [-cloud cloudbase]` -- 执行云端许可验证, 指定特定的 Wolfram ID 和密码, 没有给出的情况下提示输入. 对不同的云可以指定不同的验证.
`-username [wolframid]` -- 指定在云端认证使用的 Wolfram ID.
`-password [password]` -- 指定在云端认证使用的密码.
`-permissionskey key` -- 使用权限密钥访问云资源.
`-config|-configure [key=value ...]` -- 通过指定特定配置变量的值对 WolframScript 进行配置.
`-disconnect [-cloud cloudbase]` -- 从云端断开, 并移除验证信息.

#### 详细信息

+ Wolfram 语言脚本

在 `#!wolframscript` 脚本中可以使用所有标准选项.
当设置为 `#!wolframscript -function` ...... 时, 可以在命令行脚本中给出函数的每个参数.
当设置为 `#!wolframscript -api` ...... 时, 可以按 `-key value` ...... 形式在命令行脚本中给出 API 的参数.
可以用 `Exit[code]`指定在执行脚本时退出的代码.
没有设置 `-print` 时, 不传送任何输出到 `stdout`, 除非使用 `Print[expr]` 命令明确指出.
当有选项 `-print` 时, 把脚本最后一行的结果传送到 `stdout`.
当有选项 `-print all` 时, 脚本中每一行命令的结果产生后都被传送到 `stdout`.
`-linewise` 选项可用来多次运行脚本, 每次取一行 `stdin` 作为输入.

+ 命令行输入;  在 Wolfram 语言代码中, 可以用 `$ScriptInputString` 获取按标准输入形式给出的脚本输入. 可以用 `$ScriptCommandLine` 获取命令行中给出的参数.
+ 输出格式; `TotalWidth` 的缺省设置为 `Infinity`.
+ API 参数; 如果 API 支持多个参数, 如 `x-url`, `x-format` 和 `_timeout`, 可将它们用在 `wolframscript -api` 命令中.
+ 代码位置; 在 `wolframscript -api uuid` 中, 如果 `LocalObject["uuid"]` 存在, 就用 `LocalObject["uuid"]`, 否则使用 `CloudObject["uuid"]`.

### 配置和环境变量

#### 缺省配置文件

+ `%APPDATA%\Wolfram\WolframScript\WolframScript.conf` Windows
+ `~/Library/Application\ Support/Wolfram/WolframScript/WolframScript.conf` Macintosh
+ `~/.config/Wolfram/WolframScript/WolframScript.conf` Unix

#### 缺省认证文件夹

+ `\%LOCALAPPDATA%\Wolfram\WolframScript\` Windows
+ `~/Library/Caches/Wolfram/WolframScript/` Macintosh
+ `~/.cache/Wolfram/WolframScript/` Unix

#### WOLFRAM 语言变量

下列变量会在 WolframScript 开始执行时被设置.

+ `$CommandLine` -- 一系列字符串给出使用的完整的命令行.
+ `$ScriptCommandLine` -- 为正在运行的脚本准备的一系列命令行参数. 这些参数出现在 `-option` 给出的选项之后.
+ `$ScriptInputString` -- 一个通过标准输入给出脚本输入的字符串. 在脚本的每次迭代中, 选项 `-linewise` 用一行标准输入载入该变量.

#### 环境变量

+ `WOLFRAMSCRIPT_AUTHENTICATIONPATH` -- 存储验证信息的文件夹.
+ `WOLFRAMSCRIPT_CONFIGURATIONPATH` -- 存储永久配置信息的文件.
+ `WOLFRAMSCRIPT_CLOUDBASE` --  WolframScript 中使用的缺省云基 (cloud base) .
+ `WOLFRAMSCRIPT_KERNELPATH` -- 到缺省的本地可执行 Wolfram Engine 核的路径.

#### 配置变量

+ `WOLFRAMSCRIPT_AUTHENTICATIONPATH` -- 同名的环境变量尚未被设置的情况下, 到存储验证信息的文件的路径.
+ `WOLFRAMSCRIPT_CLOUDBASE` -- 同名的环境变量尚未被设置的情况下, WolframScript 中使用的缺省云基.
+ `WOLFRAMSCRIPT_KERNELPATH` -- 同名的环境变量尚未被设置的情况下, 到缺省的本地可执行 Wolfram Engine 核的路径.

## 文件系统

guide/FileOperations, 比较全的文件系统操作的函数列表
tutorial/FilesStreamsAndExternalOperations#12068

不要直接使用字符串函数操作文件名/文件路径, 这样的到的路径依赖于操作系统, 应该使用 Mathematica 提供的文件系统接口.

+ `$OperatingSystem`; 给出正在运行的操作系统的名称.
+ `$PathnameSeparator` ; 字符串,在构建路径名的时候使用.
`Windows`的默认值时`"\\"`, 其他系统是`"/"`. 在`Windows`中, 像`FileNameSplit`这样的函数默认同时允许`\` 和 `/`.

+ 文件名使用惯例.
  + `name.m`  ; Wolfram 语言源文件
  + `name.nb` ;  Wolfram 系统笔记本文件
  + `name.ma` ;  Wolfram 系统从第3版以前的笔记本文件
  + `name.mx` ;  输出所有 Wolfram 语言表达式
  + `name.exe`;   WSTP 可执行程序
  + `name.tm` ;  WSTP 模版文件
  + `name.ml` ;  WSTP 流文件

+ `$Path`; 默认的目录列表, 用于搜索输入文件的相关目录. 一般来说, 全局变量 `$Path` 被定义为一个字符串的列表, 每个字符串代表一个目录.
每次你要求打开文件时, `Wolfram` 就暂时将这些目录中的依次变成你的当前工作目录, 然后从该目录中尝试找到你要求的文件.

在`$Path`的典型设置中, 当前目录`.`和你的主目录`~`被列在第一位.

+ `DirectoryName["name",n]` ;  给出路径的父目录, `n`代表上升`n`次. 默认情形给出父目录, 不用写`n`. 作用于文件和目录, 不检查目录是否真实存在.
可以用`DirectoryName[...,OperatingSystem->"os"]`给出某种操作系统风格的路径, 选项有 `"Windows"`, `"MacOSX"`, 和 `"Unix"`.

+ `ParentDirectory["dir",n]` ;  给出路径的父目录, `n`代表上升`n`此, 只能作用于目录, 且要求目录真实存在.

+ `$InitialDirectory` ;  是 `Wolfram` 系统启动时的初始目录.
+ `$HomeDirectory` ;  你的主目录, 如果被定义过的话
+ `$BaseDirectory` ;  是 `Wolfram` 系统要加载的全系统文件的基本目录.
+ `$UserBaseDirectory` ;  用于 `Wolfram` 系统加载的用户特定文件的基本目录
+ `$InstallationDirectory` ;  你的 `Wolfram` 系统安装的最高级别目录

`Wolfram` 系统所使用的绝大多数文件都与`操作系统`无关. 然而, `.mx` 和 `.exe` 文件与系统有关.
对于这些文件, 按照惯例, 对不同计算机系统版本的名称进行捆绑, 形式如 `name/$SystemID/name`.

### 笔记本

+ `NotebookFileName[]` ; 给出当前笔记本的完整路径.
+ `NotebookDirectory[]`; 笔记本父目录

+ `NotebookOpen["name"]`;  打开已经存在的笔记本`"name"`, 返回笔记本对象. `"name"`可以是绝对路径.
+ `NotebookOpen["name",options]`; 使用指定的选项打开笔记本.
  + `NotebookOpen[File["path"]]`和`NotebookOpen[URL["url"]]`也被支持.
  + `NotebookOpen`通常会导致一个新的笔记本窗口在你的屏幕上被打开.
  + 如果`NotebookOpen`打开指定的文件失败, 则返回`$Failed`.
  + 若给出相对路径, `NotebookOpen`搜索由前端的全局选项`NotebookPath`指定的目录
  + 若使用选项 `Visible->False` 设置, `NotebookOpen` 将打开带有此选项的笔记本,它永远不会显示在屏幕上.
  + `NotebookOpen` 将当前`selection`初始化设置在笔记本的第一行单元之前.

+ `NotebookSave[notebook]`; 保存特定笔记本的当前版本.
  + `notebook`必须是一个`NotebookObject`.
  + `NotebookSave[notebook]` 将笔记本保存在一个文件中, 文件名由笔记本对象 `notebook` 给出.
  + `NotebookSave` 写入对应的 `Wolfram` 语言表达式, 以及 Wolfram 语言注释, 以便于前端再次读入笔记本.
  + `NotebookSave[notebook, "file"]`, 如果`"file"`存在, 则不加警告地覆盖它.
  + `NotebookSave[notebook,File["file"]]`也被支持.
  + 如果给定选项 `Interactive->True`, 前端将提示用户为笔记本选择一个文件名.

+ `NotebookClose[notebook]`; 关闭指定的笔记本对象.
+ `NotebookClose[] `; 关闭当前在运行的笔记本.
  + `NotebookClose`将使笔记本从你的屏幕上消失, 并将使所有引用该笔记本的笔记本对象失效.
  + 如果给定了选项设置`Interactive->True`, 前端将提示用户是否关闭笔记本而不保存.

### 操作文件和目录

tutorial/FilesStreamsAndExternalOperations#12068
Manipulating Files and Directories

+ `ExpandFileName["name"] `; 将`"name"`文件展开成当前系统规范的绝对路径, 给出相对于你当前目录的名称.
+ 它展开通常的目录指定, 如`.`和 `..`.
+ 它只是对文件名进行操作；它并不实际搜索指定的文件.
+ 它支持 `ExpandFileName[File["name"]]`, 以及`ExpandFileName[URL["file:///path"]]`, 后者将基于文件的`URL`转换为绝对文件名.

+ `AbsoluteFileName["name"]`; 给出`"name"`文件的绝对路径. 与`ExpandFileName`的区别是, 它会进入文件系统, 检查文件是否真实存在.
+ 同样相对于你当前目录的名称, 可以处理目录指定, 如`.`, `..`和 `~`.
+ 它也支持 `AbsoluteFileName[File["name"]]`.

+ `FileNameTake["name"]` ; 从`"name"`的完整路径中提取出最后的文件名.
+ `FileBaseName["file"]`; 给出文件的 basename, 也就是不包括拓展名.
+ `FileExtension["file"] ` ; 给出文件的拓展名.
+ `FileNameDepth["name"] `; 给出文件路径的深度, 文件不必真实存在.

+ `FileNameJoin` ; 从路径列表中组合出完整的文件名
+ `FileNameSplit` ; 将文件的完整路径分割开
+ `FileNameDrop["name",n] `; 去掉文件`"name"`路径的前`n`个片段. 如果是`-n`, 那么去掉从末尾开始的`n`个.
+ `FileExistsQ["name"] ` ; 检查文件, 目录等等是否存在.
+ `ContextToFileName["context"] ` ; 给出 Mathematica 上下文规范对应的文件名.

#### 目录操作

+ `DirectoryQ` ; 测试名称是否对应于实际的目录
+ `ParentDirectory["dir"]` ; 给出父目录
+ `DirectoryName` ; 从一个完整的文件路径中挑选出目录部分

#### 寻找特定文件

+ `FindFile[name] `; 找到指定名称的文件, `Get[name]` 和相关函数使用此函数寻找文件.

+ `FindFile[name]` 在 `$Path` 给出的目录中依次`name`, 返回文件的绝对路径.
+ 如果 `FindFile` 无法找到具有指定名称的文件, 它将返回 `$Failed`.
+ 在 `FindFile[name]`中, 名称可以用 `/` 路径分隔符来指定 (或者在 Windows 中用 `\\`). 它也可以用`` ` ``上下文分隔符来指定.
+ 对于`` name` ``这样的名字, `FindFile` 会搜索 `name.mx` 和 `name.m` 的文件.
+ 如果 `FindFile` 解析到一个目录, 它将在该目录中搜索名为 `Kernel/init.m` 或 `init.m` 的文件.
+ 如果`FindFile`解析为 `name.mx` 形式的对象, 该对象对应于一个目录, 它将搜索一个名称为`name.mx/$SystemID/name.mx`的文件.
+ `Get`, `Needs`, `OpenRead`, `Install` 和其他函数使用 `FindFile` 来寻找要读取的文件.
+ `FindFile[File["file"]]`的语法也支持.

#### 列出文件

+ `FileNames[]`; 列出当前目录中的所有文件

+ 文件名可以是字面字符串,` StringExpression`字符串模式, `RegularExpression`对象, 或`缩略字符串`.
+ 在缩略字符串模式中, `*`代表零个或多个字符的序列. `@`代表非大写字母的一个或多个字符的序列.
+ `FileNames[All]`, `FileNames["*"]`或`FileNames[__]`等同于`FileNames[]`.
+ 在缩略字符串模式中, `Verbatim["s"]`指定字符串 `"s "` 应与`*`和`@`进行字面匹配, 而不是当作通配符.
+ `FileNames[form,dirs,Infinity]` 在`dirs`的所有子目录中寻找文件.
+ `FileNames` 返回的文件列表按照函数 `Sort` 产生的顺序进行排序.
+ `FileNames[forms,dirs,{n}]`只包括正好在第`n`层出现的目录.
+ 除了文件名之外, 匹配模式还可以使用`相对`或`绝对`目录规范.
+ 设置选项`IgnoreCase>True`使`FileNames`忽略文件名大小写的区别.
+ 在默认设置`IgnoreCase>Automatic`下, `FileNames`在`Windows`系统下忽略文件名大小写, 其他系统不忽略.
+ `File["dir"]`可以用来指定一个要搜索的字面目录名.

#### 工作目录设置

+ `SetDirectory["dir"]`; 将当前工作目录设置为 `dir`.
  + `SetDirectory`设置当前工作目录, 然后返回其全名.
  + `SetDirectory` 将当前工作目录压入`DirectoryStack[]`给出的目录栈中.
  + 如果给出相对路径, `SetDirectory` 设置相对于当前工作目录的目录.
  + `SetDirectory[]`等同于`SetDirectory[$HomeDirectory]`.
  + `SetDirectory[File["dir"]]`也被支持.

+ `ResetDirectory[]`; 将当前工作目录重置为之前的值.
  + 对`ResetDirectory`的连续调用会恢复越来越早的工作目录.
  + `ResetDirectory`使用`DirectoryStack[]`给出的目录栈.
  + `ResetDirectory`从目录栈中删除最后一个元素, 并使倒数第二的元素成为当前元素.

+ `DirectoryStack[]`; 给出当前使用的目录序列/目录栈. 其中的目录用绝对路径给出.
每次调用`SetDirectory`会在目录栈中压入一个元素；每次调用`ResetDirectory`会弹出一个元素.

### 读取文本数据

Reading Textual Data

## 流和文件

tutorial/FilesAndStreams

+ 流的类型.
  + `InputStream["name",n]`  从一个文件或者管道的输入
  + `OutputStream["name",n]`  一个文件或管道的输出
+ 输出流的选项.

| 选项名 | 默认值 | explanation |
| ----- | ----- | -----|
| `CharacterEncoding` | `Automatic` | 用于特殊字符的编码|
| `BinaryFormat` | `False` | 是否把文件以二进制格式处理|
| `FormatType` | `InputForm` | 表达式的默认格式|
| `PageWidth` | `78` | 每一行的字符数目|
| `TotalWidth` | `Infinity` | 单个表达式中的最大字符数目|

使用 `Options` 用户可以测试流的选项, 并且使用 `SetOptions` 重设.

### 流和底层的输入输出

tutorial/StreamsAndLowLevelInputAndOutput

文件和管道两者都是一般称为 `流` 的 Wolfram 语言对象的例子.
Wolfram 系统中的流是一个输入或输出源. 许多运算都是在流上进行的.

可以将 `>>` 和 `<<` 作为"高层" Wolfram 语言输入输出函数.
它们的基础是直接在流上工作的**底层输入-输出基元**.
用这些基元, 可以更多地控制 Wolfram 语言的输入和输出.
在编写从文件或管道存取中间数据的 Wolfram 语言程序时, 需要进行精确的输入和输出控制.

把输出写入一个 Wolfram 语言流的底层步骤如下.

+ 首先, 调用 `OpenWrite` 或 `OpenAppend` 去打开这个流, 这告诉Wolfram 语言需要向一个**文件**或**外部程序**写入输出以及输出的形式.
+ 打开一个流之后, 调用 `Write` 或 `WriteString` 向这个流写入一个表达式或字符串序列.
+ 完成后, 用 `Close` 去关闭这个流.

***
Wolfram 语言中的流.

+ `"name"`  用名称指定的文件
+ `"!name"`  用名称指定的命令
+ `InputStream["name",n]`  输入流
+ `OutputStream["name",n]`  输出流

打开一个文件或管道时, Wolfram 系统产生一个"流对象"去指出打开的流与该文件或管道相关.
这个流对象一般包含该 `文件名` 或 管道中 `外部命令名` 和 一个**唯一的数**.

在流对象中使用一个唯一数的原因是可能同时会有几个流与同一个文件或外部程序相联系.
例如, 可以在不同的地方使用同一个外部程序, 每一次都与不同的流相关联.

然而, 打开一个流后, 当仅有一个对象和这个流相关联时,
仍可以用一个简单的文件名或外部程序名取指代这个流.

打开一个输出到文件 tmp 的流：

```mathematica
In[1]:= stmp = OpenWrite["tmp"]
Out[1]= OutputStream["tmp", 5321]
```

向该文件写入一个表达式序列：

```mathematica
In[2]:= Write[stmp, a, b, c]
```

由于仅有一个流与文件 `tmp` 相关联, 所以能简单地用文件名去指代它：

```mathematica
In[3]:= Write["tmp", x]
```

关闭这个流：

```mathematica
In[4]:= Close[stmp]
Out[4]= "tmp"
```

这是向该文件写入的内容：

```mathematica
In[5]:= FilePrint["tmp"]
During evaluation of In[7]:=
abc
x
```

***
底层输出函数.

+ `OpenWrite["file"]`  打开一个输出到文件的流, 清除该文件以前的内容
+ `OpenWrite[]`  打开一个输出到新的临时文件的流
+ `OpenAppend["file"]`  打开一个输出到文件的流, 向已有内容追加数据
+ `OpenWrite["!command"]`  打开一个输出到外部命令的流
+ `Write[stream,exp1,exp2,...]`  将表达式序列写到一个流, 用一个换行结束输出
+ `WriteString[stream,str1,str2,...]`  将字符串序列写到一个流, 没有额外换行
+ `Close[stream]`  告诉 Wolfram 语言该流已经完成

调用 `Write[stream,expr]` 时, 将一个表达式写入一个指定的流.
其默认情形是用 Wolfram 语言输入形式写入该表达式.
当调用 `Write` 写入一个表达式序列时, 这些表达式就一个接一个地写入一个流中.
一般地, 在相继表达式中无空格.
然而, 在写完了所有表达式之后, Write 总用一个换行结束输出.

重新打开文件 `tmp` ：

```mathematica
In[6]:= stmp = OpenWrite["tmp"]
Out[6]= OutputStream["tmp", 5322]
```

向该文件写入一个表达式序列, 然后关闭：

```mathematica
In[7]:= Write[stmp, a^2, 1 + b^2]; Write[stmp, c^3]; Close[stmp]
Out[7]= "tmp"
```

所有表达式用输入形式写入, 同一 `Write` 给出的表达式放在同一行：

```mathematica
In[8]:= FilePrint["tmp"]
During evaluation of In[10]:=
a^21 + b^2
c^3
```

`Write` 提供了写出完整 Wolfram 语言表达式的途径. 有时需要写出较少结构化的数据.
`WriteString` 用来写出任意字符串. 与 `Write` `不同, WriteString` 不换行, 也不加任何字符.

打开一个流：

```mathematica
In[9]:= stmp = OpenWrite["tmp"]
Out[9]= OutputStream["tmp", 5323]
```

向该流写入2个字符串：

```mathematica
In[10]:= WriteString[stmp, "Arbitrary output.\n", "More output."]
```

这里写入另一个字符串, 然后关闭该流：

```mathematica
In[11]:= WriteString[stmp, " Second line.\n"]; Close[stmp]
Out[11]= "tmp"
```

这里是该文件的内容. 这些字符串与给定的完全一样. 只有在明确给出换行符的地方才换行：

```mathematica
In[12]:= FilePrint["tmp"]
During evaluation of In[14]:=
Arbitrary output.
More output. Second line.
```

***
将输出写入一个流列表.

+ `Write[{stream1,stream 2},expr1,...]`  将表达式写入一个流列表
+ `WriteString[{stream1,stream2},str1,...]`  将字符串写入流列表

函数 `Write` 和 `WriteString` 的重要特点之一是它们不仅可以向一个流, 而且可以向一个流列表写入输出.

在使用 Wolfram 语言时, 定义由流列表组成的**通道**是方便的.
简单地令 Wolfram 语言向通道写入时, 就把同一对象写入了几个流之中.

在标准交互式 Wolfram 语言进程中, 有几个常用的输出通道.
它们指定某些类型输出的去向.
例如, `$Output` 指定标准输出的去向, 而 `$Messages` 指定信息的去向. 函数 `Print` 调用 `Write` 在 `$Output` 通道工作.
同理,  `Message` 调用 `Write` 在 `$Messages` 通道工作.
在 "主循环" 中列出了典型 Wolfram 语言进程所使用的通道.

注意, 通过 Wolfram Symbolic Transfer Protocol (WSTP) 运行 Wolfram 语言时使用不同的方式.
所有输出一般写入一个 WSTP 连接中, 但每个输出块以一个表明类型的"小包"出现.

在大部分情况下, Wolfram 语言使用的文件名与外部命令名与计算机操作系统所使用的名称相对应.
但在一些系统中, Wolfram 系统支持各种具有特殊名称的流.

***
一些计算机系统中的特殊流.

+ `"stdout"`  标准输出
+ `"stderr"`  标准错误

特殊流 `"stdout"` 允许将输出送到操作系统提供的标准输出.
但要注意仅能在 Wolfram 语言的简单文本界面中使用.
当与 Wolfram 语言的交互更复杂时, 这种流无法工作, 试图使用这种流会带来很多麻烦.

***
输出流的一些选项.

| 选项名 | 默认值 |exppantion|
| ----- | ----- | ----- |
| `FormatType` | `InputForm` | 默认输出格式 |
| `PageWidth` | `78` | 页按字符数的宽度 |
| `NumberMarks` | `$NumberMarks` | 近似数中是否包含记号`` ` ``
| `CharacterEncoding` | `$CharacterEncoding` | 特殊字符使用的编码 |

许多选项与输出流有关.
第一次用 `OpenWrite` 或 `OpenAppend` 打开输出流时就可以定义这些选项.

这里打开一个流, 指定 `OutputForm` 是默认输出格式：

```mathematica
In[13]:= stmp = OpenWrite["tmp", FormatType -> OutputForm]
Out[13]= OutputStream["tmp", 5324]
```

将表达式写入这个流后关闭它：

```mathematica
In[14]:= Write[stmp, x^2 + y^2, " ", z^2]; Close[stmp]
Out[14]= "tmp"
```

这些表达式按 `OutputForm` 格式写入了这个流：

```mathematica
In[15]:= FilePrint["tmp"]
During evaluation of In[17]:=
 2    2  2
x  + y  z
```

注意, 将一个欲写入流的表达式放在 `OutputForm` 或 `TeXForm` 等 Wolfram 语言格式指令内总可以覆盖对这个流所指定的输出格式.

选项 `PageWidth` 指定 Wolfram 语言文本输出的页宽.
所有输出都分成这种宽度的行.
不需要分行时应设置`PageWidth->Infinity`.
通常, 设定与输出设备相符的 `PageWidth` .
在许多系统中, 运行一个程序去找到这个页宽值.
用 `SetOptions` 可以给出设置 `PageWidth` 的默认规则,
例如, `PageWidth:><<"!devicewidth"`, 这就可以自动运行外部程序找出选项值.

打开一个流, 指定页宽为`20`个字符：

```mathematica
In[16]:= stmp = OpenWrite["tmp", PageWidth -> 20]
Out[16]= OutputStream["tmp", 5325]
```

写一个表达式后关闭该流：

```mathematica
In[17]:= Write[stmp, Expand[(1 + x)^5]]; Close[stmp]
Out[17]= "tmp"
```

表达式分为几行以便每行最多是`20`个字符：

```mathematica
In[18]:= FilePrint["tmp"]
During evaluation of In[20]:=
1 + 5*x + 10*x^2 +
 10*x^3 + 5*x^4 +
 x^5
```

`CharacterEncoding` 选项为一个字符串指定代码, 该代码将在送到 `Write` 或 `WriteString` 给出的流中任意包含这个特殊字符的字符串中使用.
在需要改动国际字符集, 或者需要某一输出设备接收不能处理的字符时,
常常使用 `CharacterEncoding` .

***
操作流的选项.

+ `Options[stream]`  找出对流设置的选项
+ `SetOptions[stream,Subscript[opt, 1]->Subscript[val, 1],...]`  对一个打开的流重写设置选项

打开一个具有默认设置的流：

```mathematica
In[19]:= stmp = OpenWrite["tmp"]
Out[19]= OutputStream["tmp", 5326]
```

改变打开流的 `FormatType` 选项：

```mathematica
In[20]:= SetOptions[stmp, FormatType -> TeXForm];
```

`Options` 显示了对打开的流所设置的选项：

```mathematica
In[21]:= Options[stmp]
Out[21]= {BinaryFormat -> False, FormatType -> TeXForm, PageWidth -> 78,
 PageHeight -> 22, TotalWidth -> \[Infinity], TotalHeight -> \[Infinity],
 CharacterEncoding :> Automatic, NumberMarks :> $NumberMarks,
 Method -> {"File", BinaryFormat -> False}}
```

关闭这个流：

```mathematica
In[22]:= Close[stmp]
Out[22]= "tmp"
```

***
操作标准输出通道的选项.

+ `Options[$Output]`  找出通道 `$Output` 中所有流的选项
+ `SetOptions[$Output,opt1->val1,...]`  对通道 `$Output` 中的所有流设置选项

在进程中的每一处, Wolfram 语言保持一个当前打开的所有输入输出流以及它们选项的列表  `Streams[]` .
有时需要直接查看这一列表, 但除过间接使用 `OpenRead` 等情况下, Wolfram 语言不允许修改这个列表.

### 程序中通常意义的流

[深入理解流, 什么是流](https://blog.csdn.net/qq_25221835/article/details/80776100)

流是个抽象的概念, 是对输入输出设备的抽象,
`Java` 程序中, 对于数据的输入/输出操作都是以"流"的方式进行.
设备可以是文件, 网络, 内存等.

流具有方向性, 至于是输入流还是输出流则是一个相对的概念, 一般以程序为参考, 如果数据的流向是程序至设备, 我们成为输出流, 反之我们称为输入流.

可以将流想象成一个"水流管道", 水流就在这管道中形成了, 自然就出现了方向的概念.

当程序需要从某个数据源读入数据的时候, 就会开启一个输入流, 数据源可以是文件, 内存或网络等等.
相反地, 需要写出数据到某个数据源目的地的时候, 也会开启一个输出流, 这个数据源目的地也可以是文件, 内存或网络等等.

流有哪些分类？

可以从不同的角度对流进行分类：

1. 处理的数据单位不同, 可分为：字符流, 字节流
2. 数据流方向不同, 可分为：输入流, 输出流
3. 功能不同, 可分为：节点流, 处理流

`1.` 和 `2.` 都比较好理解, 对于根据功能分类的, 可以这么理解：

节点流：节点流从一个特定的数据源读写数据.

即节点流是直接操作文件, 网络等的流, 例如 `FileInputStream` 和 `FileOutputStream` , 他们直接从文件中读取或往文件中写入字节流.

***
处理流：“连接”在已存在的流(节点流或处理流)之上通过对数据的处理为程序提供更为强大的读写功能.

过滤流是使用一个已经存在的输入流或输出流连接创建的, 过滤流就是对节点流进行一系列的包装.
例如 `BufferedInputStream` 和 `BufferedOutputStream` , 使用已经存在的节点流来构造, 提供带缓冲的读写, 提高了读写的效率, 以及 `DataInputStream` 和 `DataOutputStream` , 使用已经存在的节点流来构造, 提供了读写Java中的基本数据类型的功能.
他们都属于过滤流.

### 常见流类介绍

节点流类型常见的有：对文件操作的字符流有`FileReader/FileWriter`, 字节流有`FileInputStream/FileOutputStream`.

处理流类型常见的有：缓冲流：缓冲流要“套接”在相应的节点流之上, 对读写的数据提供了缓冲的功能, 提高了读写效率, 同事增加了一些新的方法.

字节缓冲流有`BufferedInputStream`/`BufferedOutputStream`, 字符缓冲流有`BufferedReader`/`BufferedWriter`,
字符缓冲流分别提供了读取和写入一行的方法`ReadLine`和`NewLine`方法.

对于输出的缓冲流, 写出的数据, 会先写入到内存中, 再使用`flush`方法将内存中的数据刷到硬盘.
所以, 在使用字符缓冲流的时候, 一定要先`flush`, 然后再`close`, 避免数据丢失.

***
转换流：用于字节数据到字符数据之间的转换.

仅有字符流`InputStreamReader`/`OutputStreamWriter`.
其中, `InputStreamReader`需要与 `InputStream`“套接”, `OutputStreamWriter`需要与`OutputStream`“套接”.

数据流：提供了读写Java中的基本数据类型的功能.

`DataInputStream`和`DataOutputStream`分别继承自`InputStream`和`OutputStream`, 需要“套接”在`InputStream`和`OutputStream`类型的节点流之上.

对象流：用于直接将对象写入写出.

流类有`ObjectInputStream`和`ObjectOutputStream`,
本身这两个方法没什么, 但是其要写出的对象有要求,
该对象必须实现`Serializable`接口, 来声明其是可以序列化的. 否则, 不能用对象流读写.

还有一个关键字比较重要,  `transient` , 由于修饰实现了`Serializable`接口的类内的属性, 被该修饰符修饰的属性, 在以对象流的方式输出的时候, 该字段会被忽略.

## 格式化输出

tutorial/FormattedOutput

自从版本3以来, Wolfram 语言提供了对任意数学排版和布局的有力支持.
基于所谓的框符语言(box language), 它允许笔记本自身作为 Wolfram 语言的表达式.

尽管这种框符语言非常强大, 在实际操作时却很难被用户直接使用.
从版本6, 出现了框符语言的高层界面, 免去了直接使用框符, 却仍保持排版和布局的强大功能.
这个新层的函数被称为`框符生成器`, 但用户不必意识到框符语言.

我们首先了解一下与显示大量表达式相关的框符生成器,
然后再介绍几种超出了简单数学排版的方式, 这些方式能用于生成漂亮的格式化输出.

### 样式化输出(Styling Output)

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

### 网格布局(Grid Layout)

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

当然, 这时整个网格过宽而不能在一行中被容纳(除非将这个窗口设置的很宽), 因此网格中有的元素无法被看到. 这把我们带到另一种横向布局函数： `Row` .

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

### 将输出用作输入

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

### 特殊网格条目(Special Grid Entries)

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
现在来看 `Frame` 选项, 如果只想在某些特定元素周围加框架, 而其它部位不加, 您很可能认为必须在这些元素自己的 `Grid` 中进行 `Frame->True` 的设置来完成.
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

### 框架和标签

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

### 其它注释

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

### 默认样式

正如我们在"框架和标签"小节中所看到的, 对 `Panel` 等的创建实际上类似于 `Style`, 因为它们设置了一个能使一组默认样式应用于其内容的环境.

这可以通过明确的 `Style` 命令进行覆盖, 也可以被 `Panel` 自身通过 `BaseStyle` 选项覆盖. `BaseStyle` 可以被设置为一种样式, 或者是一列样式指令, 正如在 `Style` 中的用法一样, 并且这些指令成为该 `Panel` 范围内的默认环境.

我们已经看到, `Panel` 在默认情况下使用对话框字体系列和尺寸, 但是这可以用 `BaseStyle` 选项进行覆盖.

```mathematica
In[33]:= Panel[Range[10]]
Out[33]= {1,2,3,4,5,6,7,8,9,10}
In[34]:= Panel[Range[10], BaseStyle -> {"StandardForm"}]
Out[34]= {1,2,3,4,5,6,7,8,9,10}
```

事实上, 几乎所用的框符生成器都有一个 `BaseStyle` 选项. 例如这里是一个默认字体颜色为蓝色的网格, 注意灰色的元素保持灰色, 因为内部的 `Style` 封装胜过外围 `Grid` 的 `BaseStyle`.
(这是选项可继承性的主要特征之一, 它超出了本文的讨论范围.)

```mathematica
In[35]:= Grid[Partition[ptable, 10], BaseStyle -> {FontColor -> Blue}]
Out[35]=
```

### 默认选项

假设您有一个表达式, 多次出现同一框符生成器, 如一个 `Framed` 或 `Panel`, 您想将它们全部改变, 使之含有相同的选项集合. 在函数每一次出现时都添加相同的选项集可能会非常繁琐,  幸好这里有一个更简单的方法.

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

### 数学排版

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

### 使用框符语言(Box Language)

最后一点说明是, 对于已经很熟悉框符语言的用户可能偶尔会发现, 这些框符生成器在您构建自己的底层框符时会产生阻碍,
然而, 通过一个简单的漏洞, 您可以将有效的框符直接显示在输出中：`RawBoxes`.

```mathematica
In[41]:= {a,b,RawBoxes[SubscriptBox["c","d"]],e}
Out[41]= {a,b,Subscript[c, d],e}
```

正如和其它所有漏洞一样, `RawBoxes` 给了您更高的灵活性, 但它也可以让您搬起石头砸自己的脚, 请小心使用.

### TextString

`TextString[expr]`; 将`expr`转换成人类可读的字符串表示.
`TextString[expr]` 还支持一些特殊功能, 例如`TextString[Now]`格式化现在的时间.

## package 包

`package`,`包` : tutorial/SettingUpWolframLanguagePackages

使用`Get`读取`.m`文件, 能够计算文件中的表达式, 上下文中会建立起文件中的变量. 而读取`.nb`文件, 不会建立变量, 只会建立一个笔记本的符号表达式.

`mma` 中写包的大概结构：

```mathematica
BeginPackage["Package`"]    设置 Package` 为当前上下文, 并且把 System` 放进 $ContextPath
f::usage="text", ...    介绍打算要导出的对象(不包括其他对象), 函数名在这里建立后, 它的上下文会是Package`, 可以被外部使用
Begin["`Private`"]    设置当前上下文为  Package`Private`
f[args]=value, ...     给出包中定义的主要内容
End[]   恢复到之前的上下文(此处为 Package`)
EndPackage[]    结束包, 把Package`放到上下文搜索路径中
```

***

+ 查看变量 `$Packages` :提供与当前 `Wolfram` 系统会话中已加载的所有软件包相对应的上下文列表.
+ `` Needs ["context`"] ``:如果指定的上下文尚未在 `$Packages` 中, 则加载适当的文件. 它会自动调用`Get[]`

***
`mma` 的环境变量一共有两部分, 分别叫做`$ContextPath` and `$Context`. 分别是当前上下文列表, 以及当前上下文.
参见: ref/\$ContextPath,  ref/\$Context:

`$ContextPath`类似 `Linux` 的 `$PATH`环境变量. `$Context`类似于`Linux`的当前工作目录.
搜索函数以及变量名称的时候, 先搜索`$ContextPath`中的, 再搜索`$Context`中的. 这和建立包的顺序也是一样的.
`$ContextPath`中的路径按照出现顺序搜索, 前面的会覆盖后面的.

***
`BeginPackage[]` and `Begin[]` 都要配合相应的`EndPackage[]` and `End[]` 使用, 它们的效果不同：

+ ``BeginPackage["abc`"] `` 默认会同时设置 `$Context` and `$ContextPath`, 让会话中只剩下`` abc` `` and `` System` ``两个上下文.
当然, 它也有`` BeginPackage["context`",{"need1`","need2`",... ``这种语法.
+ 而`` Begin["abc`"] `` 不会更改 `$ContextPath`, 它只更改`$Context` 为 `` "abc`" ``.
+ 此外, 调用`` EndPackage[] `` (不需要参数) 结束包时, 会将这个包, 比如`` "abc`" ``添加到`$ContextPath`的前面.
而`` End[] `` 不会更改`$ContextPath`.

### 包的调用

ref: tutorial/ModularityAndTheNamingOfThings#3434

当您创建或使用 `Wolfram Language` 包时, 您经常希望以独立于系统的方式来引用文件. 你可以使用上下文来做到这一点.

在每个计算机系统上都有一个文件系统, 与 Wolfram Language 上下文对应的规则.
当你使用上下文引用一个文件时, Wolfram Language 自动将上下文名称转换为你所在计算机系统的文件名称.

```mathematica
 <<context` 读入对应于指定上下文的文件
```

`Wolfram Language` 的设置是, ``<<name` `` 会自动尝试加载适当版本的文件. 它将首先尝试加载为您的特定计算机系统优化的 `name.mx` 文件.
如果没有找到这样的文件, 那么它将尝试加载一个包含普通 `Wolfram Language` 语法的` name.m` 文件, 这种格式独立于各种计算机系统.

如果名称是一个目录, 那么 `Wolfram Language` 将尝试加载该目录下的初始化文件 `init.m`. `init.m` 文件的目的是为设置包含许多独立文件的 `Wolfram Language` 包提供一种方便的方法.
其目的是让你只需给出`` <<name` ``命令, 然后加载`init.m`来初始化整个软件包, 读取任何必要的其他文件.

### init.m

Wolfram 系统初始化文件`init.m`包含启动代码, 每当 `Wolfram Language` 内核或前端启动时都要运行.

`init.m` 文件的可能位置包括如下.

+ `$BaseDirectory/Kernel` :内核初始化代码, 适用于所有用户
+ `$UserBaseDirectory/Kernel`: 内核初始化代码, 用于当前登录的用户
+ `$BaseDirectory/FrontEnd`: 所有用户的前端初始化代码
+ `$UserBaseDirectory/FrontEnd`:  前端初始化代码, 适用于当前登录的用户

`Wolfram`系统`基础目录`的典型子目录.

+ `Applications`;  Wolfram Language应用程序包
+ `Autoload`; 启动时自动加载的软件包
+ `FrontEnd`; 前端初始化文件
+ `Kernel`; 内核初始化文件
+ `Licensing`; 许可证管理文件
+ `SystemFiles`;  一般系统文件

通过对内核`$Path`变量的默认设置, 只需使用`` <<name` ``命令, 就可以从Wolfram System会话中加载附加组件.
加载 `init.m` 文件时, 可以再调用其他必要的文件或软件包.

通过将附加组件放在 `$BaseDirectory` 或 `$UserBaseDirectory` 的 `Autoload` 子目录下, 你可以让 Wolfram System 在你启动内核或前端时自动加载附加组件.

+ `Kernel/init.m`; 一个初始化文件, 由内核加载.
+ `FrontEnd/init.m`; 一个由前端加载的初始化文件.
+ `Documentation/`; 前端要找到的帮助文档

### $Path

`$Path` 给出了寻找外部文件时要搜索目录的默认列表.

+ 目录和文件名的结构在不同的计算机系统中可能有所不同.
+ `$Path`既用于`Get`调用的文件, 也用于`Install`调用的外部程序.
+ `$Path` 的设置可以通过特定的函数中指定`Path`选项来更改.
+ 目录名是由字符串指定的. 被检测的完整文件名是`FileNameJoin[{directory,name}]的形式`.
+ 在大多数计算机系统中, 以下特殊字符可以在目录名中使用.
  + `.` ; 当前目录
  + `...` ; 层次结构中的上一级目录
  + `~` ; 用户的主目录
+ `$Path` 可以包含嵌套的子列表.

## 关联 Associations

`Associations`具有属性`HoldAllComplete`. 直接对关联使用替换规则不会进行运算, 可以使用`Query`语法, 对关联的键和值进行替换.

+ `HoldAllComplete`: 不得以任何方式修改或查看函数的所有参数. 不展开`Sequence`, 不移除`Unevaluated`, 不使用`UpValue`, 内部`Evaluate`无效

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
这种方法适用于提取单个关联元素.

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

### 关联的匹配

使用`KeyValuePattern[{p1,p2,p3}]`确保匹配到每个模式`p1,p2,p3`, 与实际出现的顺序无关.

```mathematica
MatchQ[<|a -> 1, b -> 2, c -> 3|>, KeyValuePattern[{b -> 2}]]
```

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

### 匹配关联子集,KeyValuePattern

+ `KeyValuePattern[{patt1,...}]` ; 是一个模式对象, 表示一个关联, 或规则列表, 它能匹配到每个 `patti`的元素, 也就是包含的关系.
+ 匹配 `patti` 的元素可以在关联或规则列表中以任何顺序出现.
+ `patti` 按照它们出现的顺序进行匹配.
+ 如果只有一个模式`patt`, `KeyValuePattern[patt]`等同于`KeyValuePattern[{patt}]`.
+ `KeyValuePattern[{}]` 匹配任何关联或规则的列表.

## Dataset 数据集

`Dataset[data] `: 表示一个基于列表和关联的层次结构的数据集.

+ 数据集不仅可以表示完整的矩形多维数据阵列, 还可以表示任意的树形结构, 对应于具有任意层次结构的数据.
+ 根据它所包含的数据, 数据集对象通常显示为一个表格或网格元素.
+ 像`Map`, `Select`等函数可以直接应用于数据集, 方法是`Map[f,dataset]`, `Select[dataset,crit]`等.
+ 数据集对象中的子集可以通过`dataset[[parts]]`获得.
+ 数据集对象也可以通过编写`dataset[query]`来使用专门的查询语法进行查询.

### JoinAcross 与 SQL

[连接查询](https://www.liaoxuefeng.com/wiki/1177760294764384/1179610888796448)

`JoinAcross` 有效地实现了类似 `SQL JOIN`的方法, 将两个表`ai`和 `bj`中的行通过`spec`指定的列连接起来.

有`RIGHT OUTER JOIN`, 就有`LEFT OUTER JOIN`, 以及`FULL OUTER JOIN`. 它们的区别是：

+ `INNER JOIN`只返回同时存在于两张表的行数据, 由于`students`表的`class_id`包含`1`, `2`, `3`, `classes` 表的`id`包含`1`, `2`, `3`, `4`.
所以, `INNER JOIN`根据条件`s.class_id = c.id`返回的结果集仅包含`1`, `2`, `3`.
+ `RIGHT OUTER JOIN` 返回右表都存在的行. 如果某一行仅在右表存在, 那么结果集就会以`NULL`填充剩下的字段.
+ `LEFT OUTER JOIN` 则返回左表都存在的行. 如果我们给`students`表增加一行, 并添加`class_id=5`, 由于`classes`表并不存在`id=5`的行, 所以, `LEFT OUTER JOIN`的结果会增加一行, 对应的`class_name`是`NULL`.
+ 最后, 我们使用`FULL OUTER JOIN`, 它会把两张表的所有记录全部选择出来, 并且, 自动把对方不存在的列填充为`NULL`.

### 数据集结构

虽然列表和关联的任意嵌套是可能的, 但二维(表格)形式是最常用的.

+ 列表的列表; 没有指定行名称和列名称的表格
+ 关联的列表; 带有列名称的表格

+ 数据集按行优先, row--wise, 的方式解释嵌套的列表和关联, 因此第1层(最外层)的数据被解释为表格的行, 而第2层被解释为列.
+ 有名行和有名列分别对应于第`1`和第`2`层的关联, 其键是字符串, 被当成名称. 无名的行和列对应于这些级别的列表.
+ 一个数据集的行和列可以通过`Transpose[dataset]`来交换.

`Dataset`具有很多选项, 详见帮助页面 `Dataset Options`.

+ 当您将鼠标悬停在元素上时, 可以在数据集底部读取元素的位置.
+ `MaxItems` 的设置可以如下给出：
  + `m`; 显示 `m` 行
  + `m1,m2,..mn`; 在数据集级别 `i` 显示`mi` 项
  + `Automatic`; 显示默认的项目数.

+ 在 `HiddenItems` 列表中, 后面的设置会覆盖前面的设置.
+ `ItemDisplayFunction` 和 `HeaderDisplayFunction` 可以单独设置一个纯函数, 应用到项目上生产显示. 这些函数采用三个参数：项目`value`, 项目`position`和包含项目的`dataset`.
+ 在 `ItemStyle` 和 `HeaderStyle` 设置中的某些位置, 显式规则可能会被解释为 `i->spec`. 如果要使用`style`选项, 请使用 `Directive` 包装规则.
+ 在 `Alignment`, `HeaderAlignment`, `ItemSize` 和 `HeaderSize` 等可能是列表值的选项中, 如果可能, 顶层列表被解释为单个选项值, 否则解释为对数据集不同`Level`的依次设置.
+ 如果规则的左侧不是`列表`, 则该设置将应用于任何位置, 只要它包含了左侧作为键或索引.
+ 若纯函数`f` 返回设置, 则`f`可以用来设定值. 该设置由 `f[item,position,dataset]` 给出.

### 切片操作

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

### 数据集查询

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

### 下降和上升查询运算符

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

#### 子集运算符

`descending` part 操作符指定了在应用任何后续操作符到更深层次之前, 要在一个层次上取哪些元素.

+ `All` ;`对列表或关联的每个部分应用后续运算符
+ `i;;j` ; 抽取`i`到`j`部分并对每个部分应用后续操作符
+ `i` ; 只取第`i`部分, 并对其应用后续操作符
+ `"key",Key[key]` ; 在一个关联中取`key`的值, 并对其应用后续运算符.
+ `Values` ; 取关联中的值, 并对每个值应用后续运算符.
+ `{part1,part2,...}` ; 取给定的部分并对每个部分应用后续运算符

#### 筛选运算符

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

#### 聚合运算符

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

#### 子查询运算符 Query

`ascending subquery` 运算符, 在后续运算符应用到更深的层次之后, 才执行子查询.

+ `Query[...]` ;  对结果进行子查询
+ `{op1,op2,...}` ; 对结果一次性应用多个运算符, 产生一个列表
+ `<|key1->op1, key2->op2,...|>` ; 对结果同时应用多个运算符, 产生与给定`key`相关的关联.
+ `{key1->op1,key2->op2,...}` ; 对结果中的特定部分应用不同的运算符.

+ 当一个或多个`descending`运算符与一个或多个`ascending`运算符组成时(如`desc/*asc`), 先应用`descending`部分, 然后应用后续运算符到更深的层次, 最后, 再应用`ascending`算符到该层的结果.

在`Query`表达式被应用之前, `mathematica`将它 "编译"为普通 `Wolfram Language` 函数及算符的组合. 要看一个`Query`表达式的编译形式, 请使用`Normal`:

```mathematica
Query[All, f] // Normal
Out[3]= Map[f]
Query[f, g] // Normal
Query[GroupBy["a"], Total] // Normal
```

***
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

+ 将算符依次应用到各 column 上

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

### Query

`Query` 是 `Part`的一种推广, 它们具有相似的语法.
对于规则的嵌套矩阵, 从最外层的括号数起, 如果元素`a`外面嵌套有`n`个括号, 那么提取元素`a`, 需要`n`个指标组成的序列.
对于关联, 给出裸的指标获取`Value`, 给出括号包裹的指标`{1}`获取子关联.

`Query[op1,op2,...]`是 `Dataset` 的查询语法, 可以对`Dataset`对象作用, 也可以对由列表和关联组成的任意嵌套表达式作用.

例如：

```mathematica
Query["b", Total] @ <|"a" -> {1, 2}, "b" -> {3, 4}|>
```

+ 算符`[op1,op2,...]`是这样作用于后面的数据集的：`op1`作用于数据整体, `op2`作用于数据第一层等等. 但同时还要考虑算符的结构性质.
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

#### 特殊运算符

特殊的降序运算符: `GroupBy[spec]`, 将在它出现的层次上引入一个新的关联, 可以从现有的查询中插入或删除, 而不影响后续运算符.
在`Mathematica`中, 关联被视为单层结构.

#### 语法捷径

诸如`CountsBy`, `GroupBy`和`TakeLargestBy`这样的函数通常将另一个函数作为其参数之一.
在处理数据集中的关联时, 通常会使用这种 `by`函数来查询表中某一列的值.
为了方便起见, 数据集查询允许在这种情况下用`"字符串"`来表示`Key["字符串"]`的语法.

例如, 查询运算符`GroupBy["string"]`在执行前会自动改写为`GroupBy[Key["string"]]`.
类似地, `GroupBy[dataset, "string"]`表达式被改写为`GroupBy[dataset,Key["string"]]`.

#### 查询行为

+ 只要可能, 会使用类型推导来确定一个查询是否会成功. 被推断为失败的操作将导致返回一个`Failure`对象而不执行查询.
+ 默认情况下, 如果在查询过程中产生了任何消息, 查询将被终止, 并返回一个包含消息的`Failure`对象.
+ 当查询返回结构化数据(例如列表或关联, 或这些数据的嵌套组合)时, 结果将以另一个`Dataset`对象的形式给出.
否则, 结果将以普通Wolfram语言表达式的形式给出.
+ 关于`Dataset`查询的特殊行为的更多信息, 请参见`Query`的功能页面.

### 导出数据集

`Normal`可以用来将任何数据集对象转换为其底层数据, 通常是列表和关联的组合.

数据集对象可以通过`Export["file.ext",dataset]` 或 `ExportString[dataset, "fmt"]`来导出,  支持以下格式：

+ `"CSV`: 以逗号分隔的数值表
+ `"TSV"`: 以制表符分隔的数值表
+ `"JSON"`: `JSON`表达式, 其中关联表示成对象
+ `"Package"`: 人类可读的 Wolfram 语言表达式
+ `"MX"`: 打包的二进制协议

+ `SemanticImport` 可用于将文件导入为数据集对象.
