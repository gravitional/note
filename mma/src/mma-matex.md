# mma-matex

[在 Mathematica 里使用 LaTeX 的输出效果](https://github.com/szhorvat/MaTeX/blob/master/README.zh_cn.md)

## 安装

[最新版本]: https://github.com/szhorvat/MaTeX/releases
[TeX系统]: https://tug.org/begin.html
[其官方下载页面]: ghostscript.com/download/gsdnld.html
[Richard Koch 的页面]: https://pages.uoregon.edu/koch/

在 Mathematica 11.3 或更高版本中, 只需运行 `ResourceFunction["MaTeXInstall"][]` 即可安装或升级 `MaTeX`.

在不支持 `resource` 功能的旧版本中, 请遵循手动安装说明:

+ 下载[最新版本][], 以 `.paclet` 文件形式分发, 并使用 `Mathematica` 中的 `PacletInstall` 函数进行安装.
例如, 假设文件 `"MaTeX-1.7.8.paclet"` 已下载到目录 `~/Downloads` 中, 请执行

    ```mathematica
    Needs["PacletManager`"]
    PacletInstall["~/Downloads/MaTeX-1.7.8.paclet"]
    ```

    获取文件路径的最便捷方法是 Mathematica 的 `插入→文件路径` 菜单命令.

+ 确保已安装 [TeX系统][] 和 Ghostscript 9.15 或更高版本.
    + 对于 Windows 和 Linux, 可从 [其官方下载页面][] 获得最新的 Ghostscript.
    + 在 OS X 上, MacTeX 2015 和更高版本已包含兼容版本的 Ghostscript. 如果您使用的不是较旧的 TeX 发行版, 请从 [Richard Koch 的页面][] 获取最新的 Ghostscript.

+ 运行 `` <<MaTeX` `` . 首次加载时, `MaTeX` 会尝试自动进行自我配置.
如果自动配置失败, 它将显示有关如何手动配置 `pdflatex` 和 `Ghostscript` 可执行文件的路径的说明.
注意:  在 Windows 系统上, 请使用命令行 `Ghostscript` 可执行文件, 即名称以 `c` 结尾的文件: `gswin32c.exe` 或 `gswin64c.exe`.

+ 用 `MaTeX ["x ^ 2"]` 测试 `MaTeX`.
+ 打开文档中心, 搜索 `"MaTeX"` 以开始使用.

### 升级或卸载

当已经存在旧版本时, 可以安全地安装新版本.
`` <<MaTeX` `` 将始终加载与您的 Mathematica 版本兼容的最新安装的 MaTeX.

Paclet是一种将任意的Wolfram Language功能捆绑在一起的方式, 可以从服务器上下载并安装在任何Wolfram Language系统中.
一个paclet可以包含大量的元素, 包括新的Wolfram语言功能, LibraryLink modules, stylesheets, palettes, Wolfram系统的设置, 文档 notebooks 或 data 文件.

可以使用以下命令检索所有已安装版本的列表

```mathematica
PacletFind["MaTeX"]
```

可以通过运行 `PacletUninstall` 来卸载列表中的任何项目. 要一次卸载所有版本, 请使用

```mathematica
PacletUninstall["MaTeX"]
```

要查看有关 `Needs` 加载的版本的更多信息, 请使用

```mathematica
PacletInformaton["MaTeX"]
```

注意:  如果你使用的是`MaTeX` 的早期版本, 即非 paclet 分发格式( 1.6.2 版之前), 请通过从以下位置删除 `MaTeX` 目录来卸载它.

```mathematica
SystemOpen@FileNameJoin[{$UserBaseDirectory, "Applications"}]
```

以下函数将自动下载并安装最新版本的 `MaTeX`:

```mathematica
updateMaTeX[] :=
  Module[{json, download, target},
    Check[
      json = Import["https://api.github.com/repos/szhorvat/MaTeX/releases/latest", "JSON"];
      download = Lookup[First@Lookup[json, "assets"], "browser_download_url"];
      target = FileNameJoin[{CreateDirectory[], "MaTeX.paclet"}];
      If[$Notebooks,
        PrintTemporary@Labeled[ProgressIndicator[Appearance -> "Necklace"], "Downloading...", Right],
        Print["Downloading..."]
      ];
      URLSave[download, target]
      ,
      Return[$Failed]
    ];
    If[FileExistsQ[target], PacletManager`PacletInstall[target], $Failed]]
```

在运行了上面的函数定义之后, 只需运行 `updateMaTeX[]`, 然后运行 `` <<MaTeX` ``, 以加载更新的版本.

## 用法

使用 `MaTeX[texcode]` 或 `MaTeX[expression]` 使用 `LaTeX` 进行排版.
后者将自动将 `TeXForm` 应用于 `expression`.

`LaTeX` 代码以数学模式解释.
在 `Mathematica` 字符串中编写 `LaTeX` 代码时, 请记住要转义反斜杠(即当你想表示 `\` 时请输入`\\`), 例如

```mathematica
MaTeX["\\sum_{k=1}^{\\infty} \\frac{1}{k}"]
```

也可以一次性处理多个表达式:

```mathematica
MaTeX[{
  "\\frac{x^2}{\\sqrt{3}}",
  HoldForm[Integrate[Sin[x], {x, 0, 2 Pi}]],
  Expand[(1 + x)^5]
}]
```

批量处理表达式列表仅需要运行一次 `LaTeX` , 因此比分别处理每个表达式要快得多.

有关更多用法说明, 请在文档中心中搜索 "MaTeX".

### 性能说明

`MaTeX` 调用速度的瓶颈是运行 pdflatex 进程, 该过程可能需要一秒钟的时间, 无法进一步加快.
但是, MaTeX 会缓存结果, 从而使得使用相同 TeX 代码的后续调用几乎是瞬时的.
`MaTeX` 还可以使用 LaTeX 一次性运行处理表达式列表, 这比分别处理每个表达式要快得多.

## 详细

[LaTeX typesetting in Mathematica](http://szhorvat.net/pelican/latex-typesetting-in-mathematica.html)
[GitHub上找到]: https://github.com/szhorvat/MaTeX#revision-history
[PSTricks]: tug.org/PSTricks/main.cgi/
[PGFPlots]: http://pgfplots.sourceforge.net/

如果您正在寻找有关 `MaTeX` 的文档, 只需在 `Mathematica` 的文档中心搜索 `"matex"` 就可以了!

`Mathematica` 是一个优秀而灵活的可视化(visualization)工具, 甚至支持显示复杂的数学公式.
然而, 它的排版质量无法与 𝖫a𝖳e𝖷 相提并论. 视觉风格也不太适合列入 LaTeX 文件中.
为了提高我的图片质量, 我写了一个小的 `Mathematica` 软件包, 来简化生成 `LaTeX` 风格的标签: [MaTeX](https://github.com/szhorvat/MaTeX).

更新: 下面的文章已经更新为 MaTeX 1.7.6版本. 更新日志可以在 [GitHub上找到][].

对于这个问题已经有几个解决方案, 比如 [PSTricks][] 或者用 [PGFPlots][] 画轴和标签.
但这些解决方案都不能使以 Mathematica 为中心的工作流程变得简单.
`MaTeX` 使生成 `LaTeX` 类型的表达式变得简单, 如

```mathematica
<<MaTeX`
MateX@HoldForm[Sum[1/k^2, {k, 1, Infinity}]]
```

我最初写这些函数是为了我自己的需要, 但看到其他人也可能对它们感兴趣, 我就把它们打包成了一个包. 欢迎在下面留下评论或建议!

`MaTeX` 按通常的方式加载:

```mathematica
<<MaTeX`
```

当第一次加载 `MaTeX` 时, 它将尝试自动检测 `TeX` 和 `Ghostscript`.
如果失败了, 它将显示关于使用 `ConfigureMaTeX` 命令配置 `pdflatex` 和 `gs` 可执行文件位置的说明.
作为一个例子, 在我的 `OS X` 系统上, 我需要使用以下配置:

```mathematica
ConfigureMaTeX["pdfLaTeX" -> "/Library/TeX/texbin/pdflatex", "Ghostscript" -> "/usr/local/bin/gs"]
```

现有的配置总是可以用 `ConfigureMaTeX[]` 命令来查询.

注意:

+ 在Windows上, 使用带有 `c` 字头的命令行Ghostscript可执行文件, 即 `gswin64c.exe` 而不是 `gswin64.exe`.
+ 如果你愿意, 你可以用 `xelatex` 代替 `pdflatex`. 与普通的 `pdfLaTeX` 不同,  `XeLaTeX` 可以加载任何已安装的系统字体.
+ 可以在任何时候使用 `` MaTeX`Developer`ResestConfiguration[] ``重新运行自动检测. 警告: 这将丢弃现有的配置.
+ 一些用户报告说, 只有在重新启动 `Mathematica` 前端后, 文档才可用.
从帮助菜单中选择 Wolfram Documentation, 然后搜索 "matex" 或 "MaTeX", 就可以访问该文档.

现在 `MaTeX` 应该可以使用了. 用 `MaTeX["x^2"]` 来测试下.

### 使用实例

注意: 在集成文档中还有许多使用例子. 在 Mathematica 的文档中心搜索 "MaTeX", 可以获得这些例子.

首先, 我们需要加载MaTeX:

```mathematica
<<MaTeX`
```

我对它的主要用途是创建图表注释, 如下所示:

```mathematica
texStyle = {FontFamily -> "Latin Modern Roman", FontSize -> 12};

ContourPlot[x^2 + y^4 == 1, {x, -1.2, 1.2}, {y, -1.2, 1.2},
 BaseStyle -> texStyle,
 Epilog -> {
     Arrow[{{0.1, 0.3}, {0.5, 0.80}}],
     Inset[MaTeX["x^2+y^4=1", Magnification -> 2], {0.1, 0.3}, Scaled[{0.5, 1}]]
    }]
```

### 输出绘图

[Latin Modern font]: http://www.gust.org.pl/projects/e-foundry/latin-modern

在这里, 我使用[Latin Modern font][]作为 tick labels , 以便 MaTeX 与 LaTeX 输出在视觉上保持一致.
注意, 在不同的操作系统上, 可能需要用不同的名字来指代同一字体.

我们还可以用 `MaTeX` 来生成漂亮的排版 `frame labels` 和 `frame ticks`.
Mathematica 的默认`frame`和 `axes `样式是 `dark grey`, 而MateX则输出 `black`.
为了保持一致性, 下面的 `BlackFrame` 样式也使框架变成黑色.

```mathematica
Plot[Sin[x], {x, 0, 2 Pi},
Frame -> True, FrameStyle -> BlackFrame,
FrameTicks -> {{Automatic, None},
               {Table[{x, MaTeX[x, "DisplayStyle" -> False]}, {x, Pi/4 Range[0, 8]}], None}},
FrameLabel -> MaTeX /@ {"x", "\\sin x"},
BaseStyle -> texStyle]
```

![输出绘图](http://szhorvat.net/pelican/images/matex1.png)

上面的代码会运行得很慢, 因为处理每一个 `tick label` 都需要单独调用 `LaTeX`, 而且代价很高.
从MaTeX 1.6开始, 我们可以通过将 `MaTeX` 应用于`表达式的列表`来使用更快的批量处理.

```mathematica
Plot[Sin[x], {x, 0, 2 Pi}, Frame -> True, FrameStyle -> BlackFrame,
 FrameTicks -> {{Automatic, None}, {
    With[{ticks = Pi/4 Range[0, 8]},
     (*生成 tick 位置和 latex label 对儿*)
     Thread[{ticks, MaTeX[ticks, "DisplayStyle" -> False]}]], None}},
 FrameLabel -> MaTeX@{"x", "\\sin x"}, BaseStyle -> texStyle]
```

`MaTeX` 函数既可用于字符串, 其中包含数学模式的 `LaTeX` 代码,
也可用于任意的 `Mathematica` 表达式. 它将自动对非字符串表达式应用 `TeXForm`.

当在 Mathematica `字符串` 中编写 TeX 代码时, 记住一定要转义`反斜线`. 因此 `\sum` 必须写成 `\\sum`.
在MaTeX 1.7.3或更高版本中, 有一个避免转义反斜线的技巧, 见MaTeX文档页的 "Neat Examples  "部分.

### 高级用法

#### 选项

+ `"DisplayStyle"`.  默认情况下使用`display`样式. 使用 `"DisplayStyle"->False` 来使用实现`inline`风格.
内联格式看起来像$\sum_{k=1}^\infty \frac{1}{k^2} = \frac{\pi^2}{6}$. 显示样式看起来像$\displaystyle\sum_{k=1}^\infty \frac{1}{k^2} = \frac{\pi^2}{6}$
+ `FontSize`: 用它来设置字体大小. 注意, LaTeX对不同的字体大小使用不同的字形(glyph shapes), 以提高可读性.
根据所使用的字体, 这个选项也许只提供标准尺寸. 作为替代使用 `Magnification` 来按比例缩放.
+ `"Preamble"`.  这是一个包含在 `LaTeX` 序言中的行的`list`. 默认为`{}`, 即空.
    这个选项最方便的是为 session 永久设置, 例如:

    ```mathematica
    SetOptions[MaTeX, "Preamble" -> {"\\usepackage{color,txfonts}"}]
    MaTeX["\\color{red}\\sqrt{x}"]
    ```

+ `"BasePreamble"`. 将包含在LaTeX 序言的`"Preamble"`之前. 默认值为

        {"\\usepackage{lmodern,exscale}", "\\usepackage{amsmath,amssymb}"}

    `AMS` 软件包默认被包括在内, 因为在编译 `Mathematica` 的 `TeXForm` 输出时可能需要它们, 而 `lmodern` 提供矢量字体和灵活的字体大小.
    在大多数系统上, `"\\usepackage{lmodern,exscale}"` 可以省略.

    有第二个前言选项的原因是, 对于大多数应用场景来说, 最好是保留默认在 `"BasePreamble "`中的LaTeX软件包.
    如果有定制需求, 可以设置 `"Preamble"` 而不必特别注意保留这些, 因为它们已经在 `"BasePreamble"` 中了.
    + `Magnification`: 为 `MaTeX` 的输出设置一个缩放系数(scaling factor ).
    ` Magnification`的结果是按比例缩放的, 与 `FontSize` 不同, 后者对小文本使用不同的字形.
    + `ContentPadding`. 如果 `ContentPadding -> True` (默认), 输出的高度将至少是`一行`.
    把它设置为 `False`, 输出的内容就会被裁剪得尽可能紧凑(但请注意, MaTeX总是留有 `1pt` 的空白).
    + `LineSpacing`.  `LineSpacing -> {c, n}` 将行高设置为`FontSize`的 `c` 倍加 `n`points . 默认是 `{1.2, 0}`, 即字体大小的`1.2`倍.
    计算行高的方法在MaTeX 1.2.0中已经改变. 要恢复以前的行为, 请使用 `LineSpacing -> {0, 14.4}`.

### 性能问题

`MaTeX` 需要为它生成的每个 `LaTeX` 表达式调用 `pdflatex`.
每次调用可能需要一秒钟的时间, 当在许多小的表达式上使用MaTeX时, 这可能是令人讨厌的慢速度,
例如, 创建`tick labels`.
作为对这种情况的部分补救, MaTeX对每个结果进行了缓存(caches), 所以第二次调用相同的 `TeX` 代码应该是瞬间完成的.

可以用 `ConfigureMaTeX["CacheSize" -> ...]` 来控制要缓存的表达式的最大数量.
将 `"CacheSize"` 设置为 `0` 以禁用缓存, 或设置为 `Infinity` 以防止限制缓存结果的数量. `ClearMaTeXCache[]` 将清除缓存.

从1.6版本开始, `MaTeX` 可以通过 `LaTeX` 的单次运行来处理一个`表达式列表`.
以一种允许批处理列表的方式构造你的代码(batch-processing of lists), 可以显著提高性能.
例如, 在生成`tick labels`时, 这可能很有用.

注意: `pdflatex` 的性能比 `xelatex` 略好. 在我的系统上, 用 `pdflatex` 调用 `MaTeX` 需要 ~`0.33`秒, 用 `xelatex` 需要~ `0.55`秒.

### 它是如何工作的?

MaTeX 使用 `pdflatex` 和 `standalone`文档类来从 `TeX代码` 创建 `PDF` 文件.
PDF 文件的高度通过 `including a \strut` 来确保至少是一行高(除非使用 `ContentPadding -> False`).
Mathematica 可以导入 `PDF` 文件, 但它只能正确解析简单的文件.
为了避免问题, 在导入之前, 所有的字体字形都要用 `Ghostscript` 转换为轮廓曲线.
这项功能是Ghostscript 9.15的新功能, 因此有版本要求.

### 支持和故障排除

如果 `MaTeX` 不能产生您所期望的输出, 请务必检查 `Mathematica` 文档中心->MaTeX 文档页面-> Possible Issues 部分.
只需在帮助菜单中打开文档中心, 并搜索 "MaTeX".

#### `` Get::noopen: Cannot open MaTeX`. message when loading MaTeX ``

MaTeX 要求 Mathematica 10.0 或更高版本.
在早期版本中, 当试图加载软件包时, 可能会出现这一信息.

如果您在最新的 Mathematica 版本中看到该信息, 说明 MaTeX 没有安装.

#### Cannot typeset systems of equations

`MaTeX` 只支持在内联数学模式(inline math mode)下有效的 `LaTeX` (即`$...$`).
这意味着某些环境, 如 `eqnarray` 或 `split` 将不工作.  但是 `aligned` 可以工作.

```mathematica
MaTeX["
 \\begin{aligned}
 x+y&=z \\\\
 u+v&=w
 \\end{aligned}
"]
```

#### 不能用 `split` 环境排版多行方程

不能用 split 环境排版多行方程式

MaTeX 总是使用 inline math environment, 甚至用于生成 `display` 风格的输出.
这意味着某些环境, 如来自 `amsmath` 的 `split`环境不能工作.
你可能会看到一个错误, 如

```log
! Package amsmath Error: \begin{split} won't work here.
```

一个实用的变通方法是使用 `aligned` 环境, 它在内联数学中确实有效. 例子:

```mathematica
MaTeX["
\\begin{aligned}
(1+x)^{10} = {} & x^{10} + 10 x^9 + 45 x^8 + 120 x^7 + 210 x^6 + {}\\\\
  & + 252 x^5 + 210 x^4 + 120 x^3 + 45 x^2 + 10 x + 1
\\end{aligned}
"]
```

另一个选择是使用 `mathtools` 软件包中的 `multlined`.

#### 新版本的 `MaTeX` 使用了不同的页边距, 现在我的旧图看起来不爽了!

在 `MaTeX 1.2.0` 中, 计算线高的方法已经改变.
要恢复旧的行为(即重新运行用旧版本的MaTeX编写的代码), 使用 `LineSpacing -> {0, 14.4}`.

#### 新版本的MaTeX不再将列表作为一个单元来处理了

MaTeX 1.6 及以后的版本会自动对列表进行向量化(thread).
要强制将列表作为一个单元来处理, 请使用:

```mathematica
MaTeX[TeXForm[list]].
```

#### 升级到OS X 10.11 El Capitan后, MaTeX不再工作了

如果你在 `OS X El Capitan` 或更高版本上使用MacTeX, 必须将 `pdflatex` 或 `xelatex` 的路径设置为

```mathematica
ConfigureMaTeX["pdfLaTeX" -> "/Library/TeX/texbin/pdflatex"]
```

如果你在OS X 10.10或更早的版本中使用了`/usr/texbin/pdflatex` 这个路径, 那么在10.11版本中就不能再使用.
你必须用新的路径重新配置MaTeX.

Ghostscript的路径不变, `/usr/local/bin/gs`.

#### `"MaTeX::texerr : Error while running LaTeX" message`

这个消息可能有几个原因:

**如果它只发生在某些 TeX代码的部分**, 而不是其他部分, 那么它表明你的 `LaTeX代码` 中有一个错误.
MaTeX 试图从 LaTeX 的控制台输出中提取相关的错误信息并报告, 但并不总能成功.
为了使调试这种情况更容易, `MaTeX` 提供了 `"LogFileFunction"` 和 `"TeXFileFunction"` 选项(从1.3.0版本开始).
它们分别应用于 `LaTeX日志` 文件的内容和生成的 `LaTeX代码`:

像这样使用它们:

```mathematica
MaTeX["x^", "LogFileFunction" -> Print, "TeXFileFunction" -> Print]
```

只有当 `MaTeX` 不是在使用先前的缓存结果时, 才会调用所提供的函数.
要强制调用它们, 请使用 `ClearMaTeXCache[]` 清除缓存.

如果对 `MaTeX` 的任何输入都出现这个错误, 即使是一个简单的例子, 如 `MaTeX["x^2"]`,
那么你的设置可能有问题, 或者 `MaTeX` 可能与你的系统不兼容. 请检查以下情况.

+ 你是否安装了所有需要的 `LaTeX` 软件包? MaTeX 需要 `standalone`, `lmodern`, `exscale`, `amsmath` 和 `amssymb`.
所有这些都是大多数TeX发行版的一部分, 但如果选择最小的安装选项, 可能不会被安装. `"LogFileFunction"` 选项(如上所述)可以帮助诊断丢失的软件包.
+ 如果你使用OS X或Windows(仅适用于某些TeX发行版), 确保在 `ConfigureMaTeX` 中, `pdflatex`(或 `xelatex`)可执行文件的名称拼写正确. 它必须是小写的.

    OS X的例子:

        ConfigureMaTeX["pdfLaTeX" -> "/Library/TeX/texbin/pdflatex"]

    是正确的, 只要这确实是你系统中 `pdflatex` 的位置, 就可以工作.

        ConfigureMaTeX["pdfLaTeX" -> "/Library/TeX/texbin/pdfLaTeX"]

    是**错误**的, 会引发error.

    `OS X` 默认使用的是保留大小写但不敏感的文件系统.
    然而, 一些具有 `UNIX` 血统的程序可能无法正常工作, 除非以大小写正确的方式调用它们. `pdflatex` 就是一个明显的例子, 因为[它根据被调用的名字来决定要使用的格式文件][].
    这个问题也可能影响到 `Windows` 上的一些TeX发行版, 比如 `Cygwin` 中的 `TeX Live`.

+ 如果您使用32位Windows版的Mathematica 10.0, `MaTeX` 将无法工作.
这是由于该版本的 `Mathematica` 有一个bug. 作为一种解决方法, 请使用64位的Mathematica版本, 或升级到 `10.x` 的后期版本.
据报道, 10.3.1版本可以正常工作. 我没有关于10.1和10.2的信息.

如果错误显示 `Undefined control sequence: \documentclass`, 那么 `MaTeX` 可能被配置为使用 `pdftex` 而不是 `pdflatex` .
把它配置为使用 `pdflatex` (或另一种兼容的 LaTeX 引擎, 如 xelatex).

#### 加载 `MaTeX` 或使用 `MaTeX` 函数会在Windows上引发`RunProcess::pnfd` error

这可能由两个原因导致:

+ 如果MaTeX的工作目录名称中包含 `非ASCII字符` (如 `é`, `ő`, `α`, `中`等), 在某些版本的Mathematica for Windows中, `RunProcess` 将失败.
这是 Mathematica 的一个bug. 早期的MaTeX版本包含一个解决这个问题的部分方法, 但是, 该方法有可能造成问题, 因此被删除.
从MaTeX 1.7.3开始, 可以手动设置 `MaTeX` 的工作目录.
创建一个路径中只有 `ASCII` 字符的目录, 例如 `C:\temp\MateX`,
然后用 `ConfigureMaTeX["WorkingDirectory"-> "C:\\temp\\MaTeX"]`来指示 `MaTeX` 使用它.
这个设置将被保存下来, 供未来的 sessions 使用.
要恢复到默认行为, 请设置 `ConfigureMaTeX["WorkingDirectory"-> Automatic]`.
更多信息请参见 `ConfigureMaTeX` 的文档.

+ `pdfLaTeX` 和 `Ghostscript` 的路径使用 `/` 作为路径分隔符.
请使用 `ConfigureMaTeX`, 将路径改为使用`反斜线`(`\`)作为分隔符.
请记住, 反斜线必须在 Mathematica 字符串中被转义, 因此, 路径字符串将看起来像 `"C:\\texlive\\2014\\bin\\win64\\pdflatex.exe"`.

#### 当配置为使用 lualatex 时, MaTeX 无法工作

当使用LuaTeX 0.85或更高版本时, 可能需要使用[luatex85]软件包.
包的文档解释了为什么需要这样做.

在 `"Preamble"` 或 `"BasePreamble"` 选项中添加 `"usepackage{luatex85}"`.

#### MaTeX仍然不工作!

下载[这个笔记本][], 并准确地按照上面的指示来计算它.
这将记录一些信息, 我可以用来调试这个问题.
保存该笔记本, 并将其与你所看到的问题的完整描述一起发送给我.

[Gitter聊天室][] 也可用于支持问题.

[它根据被调用的名字来决定要使用的格式文件]: https://tug.org/texinfohtml/web2c.html#Determining-the-memory-dump-to-use
[luatex85]: https://www.ctan.org/pkg/luatex85?lang=en
[这个笔记本]: szhorvat.net/pelican/files/MaTeX-Troubleshooting.nb
[Gitter聊天室]: https://gitter.im/MaTeX-help/Lobby
