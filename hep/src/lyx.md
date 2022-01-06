# lyx

使用 LyX 须知:

1. `LyX` 具有极好的文档--使用它. 点击`Help--Introduction`按钮,将会得到一个简明的介绍. 然后通过`Help-Tutorial`学习使用 `LyX`.
1. `LyX` 是一个`文档处理系统`. 在设计上,它跟通常的文字处理系统不同--它可以让写作变得更容易. 但是区别也不是天翻地覆的,不要怕. 帮助文档会告诉你如何使用.
1. `LyX`的输出更加好看. 使用菜单里的`Document--View[PDF(pdflatex)]`或者点击工具栏上的`眼睛`按钮,亲自试试看.
1. `LyX`可以模仿几乎所有的`LaTex`功能. 并且能够导入`LaTeX`件. `LaTeX`老司机简单浏览下面的教程就可以了. 然后阅读 `LyX for LaTeX Users`一章. (对于别的读者:不用担心,你不必精通`LaTeX`才能使用`LyX`).
1. `LyX` 的许多特性为非英语使用者提供便利. 另外,按键绑定以及工具栏等许多其他特性都是高度可定制的, 参见 `Help->Cuttomization`.
1. `LyX` 主页是[http://www.lyx.org/](http://www.lyx.org/). 可以用来获取有关`LyX`的信息, 订阅 LyX mailing list(s), 参加 LyX Graphical Tour 等等.
2. `Linux` 用户请注意: 请检查 `LaTeX` 发行版 `TeXLive` 的语言包是否安装,否则`LaTeX`可能会报错. 例如在`Linux`发行版`(K,X)Ubuntu`和`Debian`上,`German`语言的包为`texlive-lang-german`. 安装完语言包后, 需要使用`LyX`菜单栏中的`Tools->Reconfigure` 来更新. 重装`TeXLive`等操作之后, 也需要`Tools->Reconfigure`.

## LyX 介绍

### LyX 的理念

`LyX` 是文档准备系统,适合用来打公式,交叉引用,参考文献,索引. lyx 背后的理念是: 指定你的意愿, 而不是具体怎么实现.
Instead of "What You See Is What You Get," the LyX model is "What You See Is What You Mean" or "WYSIWYM."

+ `Emphasized Style` 用于一般强调, 一般论点, 书籍标题, 其他手册各节的名称, 和作者的注释.
+ `Typewriter` 用于程序和文件名, `LyX`代码和功能.
+ `Sans Serif` 用于菜单, 按钮或对话框的名称以及键盘键的名称.
+ `Noun Style` 用于表示人的名字.
+ `Bold` 用于 `LaTeX` 代码.

## LyX 入门

## 创建文档

### 脚注

脚注可以复制粘贴,通过点击工具栏上的按钮, 可以把普通文字变成脚注. 也可以把脚注变成普通文字,通过 `backspace` 或者 `delete`, 去掉脚注框.

### 数学

### 杂项

`LyX` 是高度可定制的, 从窗口外观到输出结果样式都可以按多种方式定制.

大部分配置通过`Tools->Preferences`完成. 有关此的更多信息, 请查看`Help->Customization`. LyX菜单中的操作支持按键绑定.
这意味着您可以通过按`Alt+F`松开手接着按下`O`或使用菜单旁边显示的快捷键绑定(默认为`Ctrl+O`)来执行`File->Open`.
按键绑定也是可配置的. 有关此信息, 请查看`Help->Customization`.

### SVG Converter

安装软件 `inkscape`, 在设置的`Converters`部分, 依次填写以下转换规则

***
From: SVG
To: EPS
Converter: `inkscape $$i --export-eps=$$o`
Important: Press add

***
From: SVG
To: PDF (ps2pdf)
Converter: `inkscape --export-area-drawing $$i --export-pdf=$$o`
Important: Press add

***
(Optionally)
From: SVG
To: PNG
Converter: `inkscape $$i --export-png=$$o`
Important: Press add

***
Press Save

You need to  maybe `reconfigure`  and restart LyX.

## 快捷键

+ `ctrl+M` 插入数学
+ `ctrl+shift+M` 插入display数学
+ `c+R` 查看pdf

### 字体

调整字体外观: `Edit`--`Text Style`  or `Alt-E-S-C`.

`Family`:字体的"整体外观".  可能的选择是,

+ `Roman`: 罗马字体家族.  通常是衬线字体.  这也是默认的`family`.  (快捷键为`Alt+C R`)
+ `Sans Serif`: 无衬线字体. (快捷键为 `Alt + C S`)
+ `Typewriter`: 打字机字体, 即等宽字体, 带有`mono`后缀的字体.  (快捷键为 `Ctrl+Shift+P`)

***
`Series`: 对应于打印的`weight`.  选项有:

+ `Medium`: 中等宽度,  这也是默认`series`.
+ `Bold`: 粗体 (快捷键为`Ctrl+Alt+B`)

***
`Shape`: 顾名思义.  选项有:

+ `Upright`: 直立字体形状.  这也是默认形状.
+ `Italic`:斜体形状.
+ `Slanted`: 倾斜字体形状(它与`italic`不同, 尽管在`LyX`编辑器中可能看不出来).
+ `Small Caps`: 小型大写字母形状.

### 插入 LaTeX 代码

`ctrl+L` or `Insert->Tex code`

## 插入各种特殊格式

+ `lyx note`: 插入`Lyx`备注.
+ `机构标志`: (`Insert->Custom Insets->InstituteMark`)
+ 还可以插入 `Short Author`, `Short Date`,但要在相应的`Author` or `Date`环境中,`Insert`菜单中才有相应条目

## lyx pdf 预览问题

[Lyx, Error Converting to Loadable Format for PDFs](https://tex.stackexchange.com/questions/326244/lyx-error-converting-to-loadable-format-for-pdfs)
[How LyX handles figures](https://wiki.lyx.org/LyX/FiguresInLyX)

要在`LyX`屏幕上查看图像,需要与 `XForms` 或 `Qt GUI` 库兼容的格式,即`bmp`,`gif`,`jpeg`,`pbm`,`pgm`,`ppm`,`tif`,`xbm`或`mng`,`png`和`xpm`.
出现消息

    "Error converting to loadable format"

表示无法将图像转换为`PNG`或任何上面的格式. 一般来说需要增加已知`converters`的列表.

### 添加转换器

可以通过在命令行中运行 `LyX` 来获取有关转换过程的更多信息:

```bash
lyx -dbg graphics
```

如果图形输出有问题,可以从在上述命令生成的输出中找到原因.

要在 `LyX` 屏幕上查看图形, LyX必须从`EncapsulatedPostScript`转换到可加载的图形格式.
在`tools-preference-Converters`部分可以看到配置的转换器机制.
添加新的 `转换器`将导致类似下面的行

```latex
\converter "eps" "png" "my_ps2png $$i $$o" ""
```

被添加到您的`.lyx/preferences`文件中.
该行使用外部程序 `my_ps2png` 定义了从`EncapsulatedPostScript`到`PNG`格式的转换器.
占位符`$$i`和`$$o`由LyX替换为输入文件和输出文件的名称.

如果 `LyX` 无法通过转换路径(可能有许多步骤), 从 `EncapsulatedPostScript` 转换到上面列出的可加载格式中的一种, 则默认为使用 `Shell` 脚本 `convertDefault.sh`.
后者是`ImageMagick` 的 `convert` utility 的简单`wrapper`.
显然,只有在安装了 `convert` 且`convert`可以处理从A格式到B格式的转换时,它才可以工作.

如果经过以上步骤, `LyX` 仍然无法加载图形,它将在图片位置上输出消息"Error converting to loadable format".
如果出现此类消息,则需要增加(augment)已知转换器的列表.

添加`EPS-> PNG`转换器的示例(对于 `Mac OS X`).

+ 安装 `ImageMagick`. 安装后,通过在终端中键入`covert /path/test.eps /path/test.png`(对于Mac OS X),检查将`eps`转换为`png`的转换器是否正常工作.
+ 在`Lyx->Preferences->File Handling->Converters`中, 将 `EPS` 添加到 `LyX` 中的 `PNG` 转换器.
选择 `EPS-> PDF`, 然后将 `格式` 从 `PDF` 更改为`PNG`. 在转换器行中,键入 `convert $$i $$o`, 然后按添加并保存.
+ 将 `convert` 命令所在的路径添加到 `Lyx`. 例如 `/opt/ImageMagick/bin` (for Mac OS X).
转到 `Lyx>Preferences>Paths>Path prefix`, 添加`:/opt/ImageMagick/bin`到路径的末尾. 保存路径并退出 `LyX`.
+ 再次运行Lyx,然后打开包含`eps`图形的文件,现在此文件应该在预览中显示`png`图形.

### Imagemagick 安全策略

[ImageMagick security policy 'PDF' blocking conversion](https://stackoverflow.com/questions/52998331/imagemagick-security-policy-pdf-blocking-conversion)

linux 下 lyx `pdf` 无法预览, 可能是由于`Imagemagick` 的安全策略引起的.
你需要更改`/etc/ImageMagick-7/policy.xml`中 `ImageMagick` 的策略.
例如在 `ArchLinux` 中(2019年5月1日), 以下行未注释:

```xml
<policy domain="coder" rights="none" pattern="{PS,PS2,PS3,EPS,PDF,XPS}" />
```

只需把上面这几行放进`<!--` and `-->`中注释掉,然后pdf转换应该就能工作了.

### ghostscript

如果遇到下列报错:

    gs: symbol lookup error: /usr/lib/x86_64-linux-gnu/libgs.so.9: undefined symbol: cmsCreateContext

则有可能是 ghostscript 版本的问题, 可以先更新到最新版试试.
参考[newer version ghostscript][], 下载 [Ghostscript AGPL 源码][], 解压缩进入源码目录编译, 即:

```bash
# 解压缩
tar xvf ghostscript-x.xx.tar.gz
# 进入目录
sudo ./configure
sudo make install
# 重启终端, 测试
gs -v
ghostscript -v
```

[newer version ghostscript]: https://askubuntu.com/questions/1076846/how-to-install-newer-version-of-ghostscript-on-server-than-provided-from-ubuntu
[Ghostscript AGPL 源码]: https://www.ghostscript.com/releases/gsdnld.html

## lyx preamble

下面是我经常使用的 `lyx` 导言,也即 `latex`导言.

```latex
% 如果没有这一句命令,XeTeX会出错,原因参见
% http://bbs.ctex.org/viewthread.php?tid=60547
% \DeclareRobustCommand\nobreakspace{\leavevmode\nobreak\ }
%%%%%%%%%%%%%%%%%+++++++++++
\usepackage{eso-pic}
% 添加图片命令或者背景到每一页的绝对位置,
% 添加一个或者多个用户命令到 latex 的 shipout rou­tine, 可以用来在固定位置放置输出
\usepackage{hyperref} %处理交叉引用,在生成的文档中插入超链接
%\usepackage[colorlinks,linkcolor=blue]{hyperref}
\usepackage{graphicx} %插入图片,基于graphics,给 \includegraphics 命令提供了key-value 形式的接口,比 graphics 更好用
%%%+++++++++++++++++++++++++++++++
\usepackage{xcolor}
% xcolor 包从 color 包的基本实现开始,提供了独立于驱动的接口,可以设置 color tints, shades, tones, 或者任意颜色的混合
% 可以用名字指定颜色,颜色可以混合, \color{red!30!green!40!blue}
%\definecolor{ocre}{RGB}{243,102,25} %定义一个颜色名称
%\newcommand{\cola}[1]{{\color{blue}{#1}}} %定义一个颜色命令
%%%+++++++++++++++++++++++++++++++
\usepackage{listings} % 在LaTex中添加代码高亮
\definecolor{codegreen}{rgb}{0,0.6,0} %定义各种颜色,给代码着色用
\definecolor{codegray}{rgb}{0.5,0.5,0.5}
\definecolor{codepurple}{rgb}{0.58,0,0.82}
\definecolor{backcolour}{rgb}{0.95,0.95,0.92}
%\lstdefinestyle{<style name>}{<key=value list>}, 存储键值列表
\lstdefinestyle{codestyle1}{
    backgroundcolor=\color{backcolour},
    commentstyle=\color{codegreen},
    keywordstyle=\color{magenta},
    numberstyle=\tiny\color{codegray},
    stringstyle=\color{codepurple},
    basicstyle=\footnotesize,
    breakatwhitespace=false,
    breaklines=true,
    captionpos=b,
    keepspaces=true,
    numbers=left,
    numbersep=5pt,
    showspaces=false,
    showstringspaces=false,
    showtabs=false,
    tabsize=2
}
%%%+++++++++++++++++++++++++++++++
\usepackage{framed} % 在对象周围添加方框,阴影等等,允许跨页
\definecolor{shadecolor}{rgb}{0.96,0.96,0.93}  %定义阴影颜色 shaded环境使用
%%%+++++++++++++++++++++++++++++++
\usepackage{amsmath,amssymb,amsfonts} % 数学字体
\usepackage{mathrsfs} % \mathscr 命令,更花的花体
\usepackage{enumitem} % 提供了对三种基本列表环境:  enumerate, itemize and description 的用户控制.
% 取代  enumerate and mdwlist 包,对它们功能有 well-structured 的替代.
%%%+++++++++++++++++++++++++++++++
\usepackage{hepunits} % 高能物理单位 \MeV \GeV
\usepackage{braket} % 狄拉克 bra-ket notation
\usepackage{slashed} % 费曼 slash 记号 \slashed{k}
\usepackage{bm,bbm}  %\bm 命令使参数变成粗体
% Blackboard variants of Computer Modern fonts.
\usepackage{simplewick} % 在式子上下画 Wick 收缩的包
\usepackage{makeidx}% 用来创建 indexes 的标准包
\usepackage{multirow} % 创建具有多行的 tabular
\usepackage{tikz-feynman}  % 画费曼图用
\usepackage{tikz} %画矢量图用
%%++++++++++++++++++++
\usepackage{mathtools}% 基于 amsmath, 提供更多数学符号,这里用来定义配对的数学符号
\DeclarePairedDelimiter\abs{\lvert}{\rvert} % 定义配对的绝对值命令
%%  amsmath 子包 amsopn 提供了\DeclareMathOperatorfor 命令,可以用于定义新的算符名称
\DeclareMathOperator{\tr}{Tr} %矩阵求迹的符号
\DeclareMathOperator{\diag}{diag} %对角矩阵
\DeclareMathOperator{\res}{Res} %复变函数的留数
\DeclareMathOperator{\disc}{Disc} %定义复变函数不连续符号
\newcommand*{\dif}{\mathop{}\!\mathrm{d}} % 手动定义一个垂直的微分符号
```

## LyX 命令行

概要: `lyx [ command-line switches ] [ name[.lyx] ... ]`

描述: `LyX`太复杂了,无法以`man`页面格式进行完整描述. 如果系统配置正确,则可以在`Help`菜单下的`LyX`中访问完整文档.
`LyX`支持以下命令行switches.

+ `-help`总结LyX的用法
+ `-version`:提供有关LyX构建的版本信息.
+ `-sysdir directory`: 设置系统目录. 通常不需要.
+ `-userdir directory`:设置用户目录. 如果要与其他`lyxrc`设置一起使用LyX,则需要此选项.
+ `-geometry WxH+X+Y`: 设置主窗口的几何形状.
+ `-dbg feature[,feature...]`:  其中`feature`是名称或数字. 使用`lyx -dbg`查看可用的调试功能列表.
+ `-i [--import] fmt file.xxx`:其中,`fmt`是选择的导入格式,而file.xxx是要导入的文件.
+ `-f [--force-overwrite] what`:     其中`what`是"all", "main" or "none"之一. 指定`all`以允许在批量导出期间覆盖所有文件,指定`main`以允许仅覆盖主文件,或`none`以覆盖任何文件. 其他内容被当成`all`, 更之后的命令行输入留待进一步解析.
+ `--ignore-error-message which`: 允许您忽略特定的`LaTeX`错误消息. 请勿用于最终文件! 当前支持的值: `"missing_glyphs"`,Fontspec `"missing glyphs"` error.
+ `-n [--no-remote]`: 即使在另一个`LyX`实例已在运行的情况下,也可以在新实例中打开作为参数传递的文档.
+ `-r [--remote]`: 通过使用lyxpipe,要求一个已经运行的`LyX`实例打开作为参数传递的文档,然后退出. 如果是`lyxpipe`未设置或无法正常运行,则创建了新实例,并且正常继续执行.
+ `-v [--verbose]`:在终端上打印所有产生的外部命令.
+ `-batch`:使`LyX`在不打开`GUI`窗口的情况下运行给定命令. 因此,类似于`lyx -batch -x "buffer-print printer default dvips" myfile.lyx`的命令会导致`LyX`将`myfile.lyx`打印到默认打印机(必须已配置),使用`dvips`和默认打印设置.

***

+ ` -x [--execute] command`:  其中`command`是一个`lyx`命令
+ `-E [--export-to] fmt filename`:其中,`fmt`是选择的导出格式(请参阅`--export`),而`filename`是目标文件名.
+ 请注意,任何其他外部文件名所需的文件(例如图像文件)也将导出到包含文件名的文件夹中(保留相对原始`LyX`文档中嵌入的路径(如果有).

+ ` -e [--export] fmt`:其中`fmt`是选择的导出格式(`latex`, `pdflatex`, `luatex`, `xetex`, `xhtml`, `text`, `lyx`, `ps`, `pdf`, ...).
在`Tools->Preferences->File Handling->File Formats->Short Name`可以查看应该传递那个参数, 短名称一般与`File->Export menu`菜单中的格式名称不同.
要导出到文件的默认输出格式, 使用`default`. `-e`和`-x`开关的顺序很重要.

例如, 要将`.lyx`文件导出成`.pdf`文件,

`default`: 使用文件制定的默认引擎
`pdf`:使用`ps2pdf`引擎转换成`pdf`
`pdf4`:使用`xetex`引擎,
`pdf2`:使用`pdflatex`引擎.

```bash
lyx --export pdf4 *.lyx #将目录中的lyx文件导出成pdf, 使用xetex 引擎.
```
