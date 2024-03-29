# LaTeX 字体

## 方言,Accents

[23.5 Accents](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Accents)

LaTeX 对世界上的许多脚本和语言都有广泛的支持,
通过核心的 `babel` 包提供, 它支持 `pdfLaTeX`, `XeLaTeX` 和 `LuaLaTeX`.
`polyglossia` 包对后两种引擎提供类似的支持.

本节不覆盖这些支持.
这里只列出了用于创建重音字符的核心LaTeX命令.
这里显示的 `\capital...` 命令产生了用于大写字母的替代形式. 这些都是 `OT1` 所不具备的.
下面, 为了使它们更容易被找到, accents 符号都用小写的 `o` 来作为例子.

注意 `\i` 产生无点的 `i`, `\j` 产生无点的 `j`.

## 中文西文数学字体

[GUST]: http://www.gust.org.pl/projects/e-foundry/lm-math
[fontspec]: https://ctan.org/pkg/fontspec
[xeCJK]: https://www.ctan.org/pkg/xecjk
[The TeX Live Guide-2021]: https://www.tug.org/texlive/doc/texlive-en/texlive-en.html

[在 LaTeX 中使用中文](https://jdhao.github.io/2018/03/29/latex-chinese.zh/)
[LaTeX数学公式的默认字体是什么](https://www.zhihu.com/question/30058577/answer/46612848).

`LaTeX` 默认的文章类中的字体是:

    Computer Modern Math(LaTeX时),  Latin Modern Math (XeTeX时)

字体文件的位置可以用`kpsewhich`查看, 此程序在安装`TeXLive`的时候会自动安装.
如果没有安装的话, [GUST][] 可以下载 `Latin Modern Math` 字体, 以及其他字体.

```bash
kpsewhich latinmodern-math.otf
/usr/share/texmf/fonts/opentype/public/lm-math/latinmodern-math.otf
```

`kpsewhich`的介绍可以查看 [The TeX Live Guide-2021][],

+ 一般使用[fontspec][] 包控制 `西文字体` 和 `数学字体`. 用法大概如下:

```latex
\usepackage{fontspec}
% 设置英文字体
\setmainfont{Microsoft YaHei}
\setsansfont{Comic Sans MS}
\setmonofont{Courier New}
% 设置数学字体
\setmathrm{⟨font name⟩}[⟨font features⟩]
\setmathsf{⟨font name⟩}[⟨font features⟩]
\setmathtt{⟨font name⟩}[⟨font features⟩]
\setboldmathrm{⟨font name⟩}[⟨font features⟩]
```

>但有一个问题, `\boldsymbol` 是 `AMS` 系列包中的 `amsbsy` 定义的宏,可以产生粗体数学符号.
>如果用 `fontspec` 设置数学字体为 `latinmodern-math.otf` 字体时,
>没有粗体效果, 而是变成直立体, 原因不明. 可以通过使用`unicode-math`包解决,见下文.

+ 对中文字体的选择可以通过 [xeCJK][] 完成:

```latex
%\usepackage{xeCJK}% 可以用来实现中文断行
\usepackage{ctex} % 中文排版通用框架, 汉字, 标点, 字体字号, 标题, 版式, 数字日期转换
% 设置中文字体
\setCJKmainfont[Mapping=tex-text]{Noto Sans CJK SC}
\setCJKsansfont[Scale=0.7,Mapping=tex-text]{Source Han Sans SC}
\setCJKmonofont[Scale=0.7]{Noto Sans Mono CJK SC}
% 中文断行设置
\XeTeXlinebreaklocale "zh"
\XeTeXlinebreakskip = 0pt plus 1p
```

## 免费数学字体

[free-math-font-survey](https://www.ctan.org/pkg/free-math-font-survey)

本文件是对可用于 `TeX` 和 `LaTeX` 的免费数学字体的调查.
文中提供了每种字体的例子, 获取字体的链接, 以及加载相关 `LaTeX` 软件包的命令.

## fontenc

[fontenc–selecting font encodings](https://www.ctan.org/pkg/fontenc)
[Why should I use \usepackage[T1]{fontenc}](https://tex.stackexchange.com/questions/664/why-should-i-use-usepackaget1fontenc)

`fontenc`指定字体编码(确定使用哪种字体), 而不是输入编码.

`TeX`的默认字体编码(`OT1`)为`7`位,并使用具有`128`个字形的字体,
因此不包括带重音符号的字符作为单个字形. 因此,通过在现有的`o`字形上添加一个重音来制作字母`ö`.
`T1`字体编码是一种8位编码,并使用具有`256`个字形的字体. 因此,`ö`是字体中的实际单个字形.
许多较早的字体也为它们设计了`T1`变体,并且许多较新的字体仅在`T1`中可用.
我认为`Computer Modern`字体最初是`OT1`,而`Latin Modern`是T1.

如果您不使用`\usepackage[T1]{fontenc}`,

+ 包含重音符号的单词不能自动连字,
+ 您无法从输出`(DVI/PS/PDF)`中正确复制和粘贴此类文字,
+ 诸如竖线符号,小于号和大于号的字符会在文本中产生意外的结果.

### lyx 字体选择

在`lyx`中,使用 `xelatex` 进行编译,可以设置 `Document Settings`--`Fonts`--`LaTeX font encoding: None fontenc`
在同一个页面,如果勾选`Use non-Tex fonts`,即可选择系统自带的字体,即可显示中文.

另外,`Document Settings`--`Language`中可设置语言,以及`xeTeX,utf-8`编码.
可以在`Insert`菜单栏中插入`beamer`特有的格式.

## XeTeX和LuaTeX的系统字体配置

[3.4.4 System font conﬁguration for XeTeX and LuaTeX](https://www.tug.org/texlive/doc/texlive-en/texlive-en.html)

手动安装 `TexLive` 时, 也许需要配置环境变量和缓存, 才能让 `TeX` 引擎找到已安装的字体.

`XeTeX` 和 `LuaTeX` 可以使用系统上安装的任何字体, 而不仅仅是 `TeX树` 中的字体.
它们通过相关但不完全相同的方法来完成这些工作.

在 `Windows` 上, 与 `TeX Live` 一起安装的字体会自动通过 `字体名称` 提供给 `XeTeX` 使用.
在 `Mac OS X` 上, 支持字体名称查询需要额外的步骤; 请参见 [MacTeX网页](https://tug.org/mactex).
对于其他 `Unix` 系统, 能够通过字体名称找到 `TeX Live` 提供的字体的程序如下.

为了便于操作, 在安装 `xetex` 软件包时(无论是初始安装还是后来),
脚本会在 `TEXMFSYSVAR/fonts/conf/texlive-fontconfig.conf` 中创建必要的配置文件.
`TEXMFSYSVAR` 变量可通过 `tlmgr conf` 命令 查看.

设置 `TeX Live` 字体供全系统使用(假设你有 `root` 权限), 步骤如下.

+ 将 `texlive-fontconfig.conf` 文件复制到 `/etc/fonts/conf.d/50-texlive.conf`.
+ 运行 `fc-cache -fsv`.

如果你没有root 权限来执行上述步骤, 或者你想让 `TeX Live` 字体只对某个用户可用, 你可以做以下事情.

+ 将 `texlive-fontconfig.conf` 文件复制到 `~/.fonts.conf`, 其中 `~` 是你的主目录.
+ 运行 `fc-cache -fv`.

你可以运行 `fc-list` 来查看系统字体的名称.
咒语 `fc-list : family style file spacing`(所有参数都是字面字符串)显示一些广泛的有趣信息.

## ubuntu texlive 手动配置字体位置

[font not found](https://tex.stackexchange.com/questions/132888/fontawesome-font-not-found)

如果你创建一些 `符号链接`,
就可以避免在安装新版 `TeX Live` 时重复更新配置文件.

作为管理你的 `TeX` 安装的用户(可能指 `root` 或 `sudo`).

```bash
cd /usr/local/texlive
ln -s 2022 current.2022
ln -s current.2022 current
```

配置文件 `50-texlive-fonts.conf` 应该位于 `/etc/fonts/conf.avail` 中,
其中 开头的数字 表示优先级
并在 `/etc/fonts/conf.d` 中建立符号链接.
因此, 作为 `root` 或 `sudo`, 创建 `/etc/fonts/conf.avail/50-texlive-fonts.conf`, 内容如下:

```xml
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <dir>/usr/share/texlive/texmf-dist/fonts/opentype</dir>
  <dir>/usr/share/texlive/texmf-dist/fonts/truetype</dir>
  <dir>/usr/share/texlive/texmf-dist/fonts/type1</dir>
</fontconfig>
```

如果你也想让 `TEXMFLOCAL` 中的字体可用:

```xml
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <dir>/usr/share/texlive/texmf-dist/fonts/opentype</dir>
  <dir>/usr/share/texlive/texmf-dist/fonts/truetype</dir>
  <dir>/usr/share/texlive/texmf-dist/fonts/type1</dir>
  <dir>/usr/local/texlive/texmf-local/fonts/opentype</dir>
  <dir>/usr/local/texlive/texmf-local/fonts/truetype</dir>
  <dir>/usr/local/texlive/texmf-local/fonts/type1</dir>
</fontconfig>
```

然后以 `root` 身份或以 `sudo` 身份:

```bash
cd /etc/fonts/conf.d
# 创建符号链接, 指向 conf.avail 中的配置文件
ln -s .../conf.avail/50-texlive-fonts.conf
fc-cache -s
```

如果你更新了 `TeX Live` 或者安装了新的版本, 你可以简单地调整符号链接并运行 `fc-cache -fs`.
事实上, 在许多情况下, 即使你不运行 `fc-cache`, 缓存也会相对快速地得到更新,
因为当你更新系统时, 你会安装一些 `字体包`, 或其他由 `发行版包管理器` 更新的软件包.
请注意, 对于某些应用程序, 你还需要运行 `mkfontscale` 和 `mkfontdir`.
所以简单来说就是:

```bash
sudo mkfontscale && mkfontdir && fc-cache -fv
```

## linxu 字体配置 conf.d/README

这个目录中的每个文件都是一个 `fontconfig` 配置文件.
`Fontconfig` 扫描这个目录, 加载所有形式为 `[0-9][0-9]*.conf` 的文件.
这些文件通常被安装在 `/usr/share/fontconfig/conf.avail`
然后在这里建立 `符号链接`, 使它们可以很容易地被安装,  然后通过调整 `符号链接` 来 启用/禁用.

这些文件是按 `数字顺序` 加载的, 配置的结构 导致了以下的使用约定

以...开头的文件:    包含:

+ 00 到 09;   字体目录
+ 10 到 19;   系统渲染默认值(AA, 等).
+ 20 到 29;   字体渲染选项
+ 30 到 39;  字体族替换(family substitution)
+ 40 到 49; 通用识别, 映射 族->通用 (generic identification, map family->generic)
+ 50-59;   alternate config file loading
+ 60-69;   generic aliases, map generic->family
+ 70-79;    select font (adjust which fonts are available)
+ 80-89;    match target="scan" (modify scanned patterns)
+ 90到99;  font synthesis

## 编译 The TeX Book

[Electronic version of Knuth TeXbook](https://tex.stackexchange.com/questions/110836/electronic-version-of-knuths-texbook)
[texbook.tex 编译](https://blog.csdn.net/virhuiai/article/details/7796097)

CTAN 上有 Knuth [TheTeXBook](https://www.ctan.org/pkg/texbook) 的源代码,
可以下载下来编译. 注意版权问题, 不要随便传播

>The source has "protection" against use to produce a document:
>such use is only allowed with the permission of the Copyright holder and of the publisher (Addison-Wesley).

直接编译不能通过, 要修改前四行

```latex
\loop\iftrue
  \errmessage{This manual is copyrighted and should not be TeXed}\repeat
\pausing1 \input manmac
\ifproofmode\message{Proof mode is on!}\pausing1\fi
```

注释成

```latex
%\loop\iftrue
%  \errmessage{This manual is copyrighted and should not be TeXed}\repeat
\input manmac
%\ifproofmode\message{Proof mode is on!}\pausing1\fi
```

用 `tex` 或 `etex` 命令编译, 编译过程可能遇到几个坑.

### 字体问题

第一个坑是字体, `tex` 可能找不到 `Arial` 字体,
参考 [安装 TeX 字体](https://www.tug.org/fonts/fontinstall.html),

使用脚本 [getnonfreefonts](https://www.tug.org/fonts/getnonfreefonts/)  安装字体

```bash
getnonfreefonts  --sys --lsfonts
```

`--sys` 和 `--user` 分别表示安装到系统(全局)或者只安装到当前 `用户`
例如 ubuntu 如果使用的是 `apt` 里的 texlive, 需要使用 `sudo` 运行这个脚本.

```bash
wget https://www.tug.org/fonts/getnonfreefonts/install-getnonfreefonts
sudo texlua install-getnonfreefonts
```

安装完成之后:

```bash
getnonfreefonts --sys --help # 显示帮助
getnonfreefonts --sys -a # 安装所有字体
```

如果不想使用别人的脚本, 也可以手动安装: [Installing TeX fonts](https://www.tug.org/fonts/fontinstall.html)

+ 安装完成后, 查看安装位置

```bash
sudo getnonfreefonts --sys -l
# 例如 ubuntu 上输出
----------------------------------------------
Installation directory: /usr/local/share/texmf
----------------------------------------------
```

然后 对应目录中的  `.tfm` font metric 文件:

```bash
ls /usr/local/share/texmf/fonts/tfm/urw/arial
# 例如 ubuntu 上输出
ua1b8a.tfm  ua1b8r.tfm  ua1bi8a.tfm  ...
```

打开 `texbook.tex` 源码, 搜索

    \font\pearsonkluj=arial at 9pt

这一行, 将 `arial` 改成安装好的字体, 例如 `ua1r8a`,
名称的含义可以在 宏包的 `README` 中找到: [urw-arial](https://www.ctan.org/pkg/urw-arial)
`ua1`是名称, `r` 表示 `regular`, `8` 表示点数等等.

改好之后保存, 重新用

```bash
tex texbook.tex  #编译文档
sudo dvipdfmx texbook # dvi转换成pdf
```

如果失败可能是没有加 `sudo`, 权限不够

### 其他参考书

[tex书籍](https://tex.stackexchange.com/a/121405)

除了 `TeXbook` 之外, 还有一些合法的 `pdf` , 其中有类似的内容:

+ [TeX for the Impatient](ftp://tug.org/tex/impatient/book.pdf)
+ [TeX by Topic](http://eijkhout.net/texbytopic/texbytopic.html)
+ [Making TeX Work](http://makingtexwork.sourceforge.net/mtw/)
+ A Gentle Introduction to TeX by Michael Doob: texdoc gentle.
