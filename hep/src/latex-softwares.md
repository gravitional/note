# LaTeX 辅助软件

[LaTeX 辅助软件--Clerk Ma](https://www.zhihu.com/question/20585795/answer/15575381)

## Ghostscript

[主页](http://www.ghostscript.com/) 授权形式: GPL/商业授权 操作系统: Win/Mac/Lin

![Ghostscript](https://pic2.zhimg.com/80/36516ada30291f61f0b67b86224c52ed_720w.jpg?source=1940ef5c)

`Ghostscript` 是PostScript以及PDF/PDL语言的解释器.
这个软件没有我们常常用到的GUI窗口, 只有在看 ps 文件或者 pdf 文件的时候会产生GUI输出.
`Ghostscript` 在很多 Linux/BSD平台下都有, Mac OS X下的版本则包含在TeX Live中, Windows版本分为32位的和64位的.

`Ghostscript` 现在最大的用途是来转换 eps/ps/pdf 文件的.
TeX Live下面很多处理pdf/eps的工具就是在后台调用了`Ghostscript`.
上面的 logo 就是使用 `Ghostscript` 生成的 png 图片(原始文件为Ghost.eps).
在转换前, 打开终端或者cmd, 输入:

```bash
gswin32 -dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -dEPSCrop -r300 -sOutputFile=a.png Ghost.eps
```

我这个是在 Windows 下运行的, 如果是其他平台, 可将 `gswin32` 换成 `gs` 执行.
这个命令实际上只有后五个参数是有用的.
`-sDEVICE` 是输出形式, 我们选择了 `png16m` 则会生成 `png` 图片,
如果改为 `pdfwrite` 则会生成 `pdf` 文件, 即为:

```bash
gswin32 -dSAFER -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -dEPSCrop -sOutputFile=Ghost.pdf Ghost.eps
```

回到第一个例子上来:

+ `-dEPSCrop` 是处理 `EPS` 文件的,
是将 `EPS` 进行裁剪进行输出, 类似的 `EPS` 处理选项还有 `-dEPSFitPage` ,
这个是将 `EPS` 处理成适合如 `A4` 页面大小等的选项.

+ `-r300` 是指的输出的分辨率, 可以调低, 如 `-r72` 就调成 `72` 了.
另外还有有反锯齿的选项 `-dTextAlphaBits` 和 `-dGraphicsAlphaBits`,
这两个选项可以填选 `1, 2, 4` 这三个值, 如 `-dTextAlphaBits=4`, 值越大, 效果越好.

+ `-oOutputFile` 指的是生成的文件名.
最后一项是 `Ghost.eps` , 其实也可以是以 `ps` 或者 `pdf` 文件名结尾的文件.

`Ghostscript` 可以在某些情况下来当作阅读器, 比如我在Gentoo下没有安装Evince的情况. .
Ghostscript 读取 pdf 文件的速度略微慢些,
但是处理某些非嵌入字体pdf文件的时候会非常正确, 很少会遇到问题.
Ghostscript 最大的长处还是在 `ps`/`eps` 文件的处理上,
使用LaTeX插图用 `eps` 文件的很多, 处理eps文件基本上是每天都要做的工作,
能掌握一点 `Ghostscript` 会舒服很多.

## KLatexFormula

主页: http://klatexformula.sourceforge.net/ 授权形式: GPL 操作系统: Win/Mac/Lin

![KLatexFormula](https://pic3.zhimg.com/80/41189c5f005041df8f1b86516dfb143c_720w.jpg?source=1940ef5c)

`KLatexFormula` 是一款用来生成截图的工具,
我个人推荐大家在知乎编辑框插图的时候使用这个程序, 幸好这个是全平台都有的.
上面的截图是一个数学公式, 但是这个工具其实还可以用来做别的, 比如生成化学式什么的.
我在这里直接贴一个教程出来好了.

首先, 安装 `Ghostscript`, 这个程序是来利用 `Ghostscript` 来生成图片的.
`KLatexFormula` 支持生成 `png, jpeg, pdf, eps, bmp, ico, ppm, tif, tiff, xbm` 和 `xpm` 文件.
一般情况下, `KLatexFormula` 启动的时候是这个样子的:

![qidong](https://pic3.zhimg.com/80/ff7ffb365b8d84d3910f53df54c78a9e_720w.jpg?source=1940ef5c)

点击 `Evaluate` 侧第三个按钮可以得到完整的窗口:

![window](https://pica.zhimg.com/80/1aab68698100d47c8e59fba2fcd86619_720w.jpg?source=1940ef5c)

我们点击一下 `Settings...` 进行设定:

![settings](https://pica.zhimg.com/80/7af0d8ed725e1895904818dab8578da4_720w.jpg?source=1940ef5c)

一般情况下, 上面的 `latex` 和 `dvips` 是已经设定好了的,
需要设定的 `gs` 这个选项, 点击右侧的 `Browse` 可以直接选取 `gs` 所在的路径,
`gs` 分为 `gswin32.exe` 和 `gswin32c.exe`,
前者运行时会有个窗口在你桌面上闪来闪去的, 一旦报错就会出现窗口,
后者的话不会闪窗, 但是报错看不到, 这个设定, 看各位的口味了.

顺带说说latex的设定, 这个需要使用默认生成dvi文件的程序,
如 `platex.exe`, `uplatex.exe` 和 `dvilualatex.exe`.
如果选择了 `dvilualatex.exe` 的话, 可以顺手玩玩 `Lua`的数学库:

![lua](https://pic1.zhimg.com/80/50703a5cf74c192b5ccd4e07fb12d739_720w.jpg?source=1940ef5c)

其实要是生成 `dvi` 的话, 也就 `uplatex.exe` 以及 `latex.exe` 的话可以直接在图片中放入汉字,
这一点是dvi本身的原因, 所以建议大家能不用汉字就不用汉字.
前者内建了汉字支持, 后者需要使用 `CJK` 包.

我们需要看看右侧的面板.
`LaTeX` 那个标签, 如果默认点选 `Use Math mode` 的话,
直接在左侧那个文本框中输入公式好了, 如果不点选, 可以按照平时写 `LaTeX` 文件那样:

![mode](https://pic1.zhimg.com/80/8f8c7b968d6923561e4e165e41821ff2_720w.jpg?source=1940ef5c)

右侧面板中的 `Images & Margins` 则可以设定生成图片的分辨率, 如:

![images](https://pic3.zhimg.com/80/0ca2180e3b93db874f4b8fa414625fd5_720w.jpg?source=1940ef5c)

上面的那个 `Colors` 就不介绍了.
左侧 `Quit` 上面的预览窗口有 `DRAG`, `COPY` 和 `SAVE` 三个选项, 选择 `SAVE` 可以保存到本地.

### XyMTeX

下面介绍一个生成化学公式的东西: `XyMTeX`. 这个包在 `CTAN` 上更新不到,
只能到 [nifty.com](http://homepage3.nifty.com/xymtex/fujitas3/xymtex/indexe.html) 上下载,
这个包曾经在一段时期上传到CTAN 里, 但是后来就停止了, 估计是藤田眞作没什么时间去处理上传的事情.
`XyMTeX` 这个包很稳定, 上次更新也就是在2010年的时候.
将 `XyMTeX` 安装好之后可以按照下图进行试验:

![XyMTeX](https://pic1.zhimg.com/80/b4906b1b388597f47a60d5f4c45c1afd_720w.jpg?source=1940ef5c)

如果懒得玩, 其实可以换用别的工具来(如 ChemDraw, 欢迎大家折磨@赵世奇),
没必要用LaTeX来折磨自己, 另外TeX Live下面会有一个 `Chemfig` 的包,
这个包也是用来做化学公式的, 但是个人推荐 `XyMTeX`.

最后顺带提一下,  `Setting` 里面可以换这个软件的皮肤的, 即 `Plugins` 下的 `Skin` 下拉选项, 如选择 `Papyrys` 效果如下:

![Skin](https://pica.zhimg.com/80/92b74bc5ca34b4d02909b16da95331cc_720w.jpg?source=1940ef5c)

Linux 版和 Mac 版本可能会略微有些不同, 但是大同小异啦!

如果想带汉字的话, 可以按照如下步骤:

+ 先开编辑器, 按照你的习惯生成 `pdf`, 记住在源码中一定要写入 `\pagestyle{empty}`
+ 第二, 使用终端或者 `cmd` 对这个 `pdf` 进行处理, 例如此文件为 `a.pdf`, 需要运行 `pdfcrop a.pdf`, 接下来会生成 `a-crop.pdf` 的文件
+ 第三, 就是将 `a-crop.pdf` 转换成图片了, 可按照 `Ghostscript` 那种办法, 也可以用下面的 `MuPDF` 中的工具

## MuPDF

[主页](http://www.mupdf.com/)  授权: GPL/商业授权 操作系统: Win/Lin/Mac/Andoid/iOS

![MuPDF](https://pica.zhimg.com/80/2e4bedd51924749396aafd17b5ba241f_720w.jpg?source=1940ef5c)

`MuPDF` 的开发者们其实就是Ghostscript的开发者, `MuPDF`是一个PDF/XPS/CBZ的解释器.
在PC端上看着不像阅读器, 但是Android/iOS版本就是个阅读器的样子.
`MuPDF`加个壳之后就是一个pdf阅读器, 如著名的SumatraPDF阅读器就是给`MuPDF`加了一个壳.
Android下的很多开源pdf阅读器其实也就是给`MuPDF`加壳.

从开发者角度来看, 这个`MuPDF`的代码看着很清爽, 注释清晰明快,
并且也没有poppler那样庞大臃肿的风格.
`MuPDF`对字体的处理比poppler要好很多.

`MuPDF` 提供了一些pdf处理的小工具,
如 `pdfdraw, pdfinfo, pdfclean, pdfextract, pdfposter, pdfshow`.
这些在Win下则不是一样的名字, `pdfdraw` 叫做 `mudraw`,
后五者则被合并到 `mubusy` 里面(想想busybox).
对我来说, MuPDF这些小工具就是神一样的存在, 必须膜拜!

+ `pdfextract` 可以把嵌入到 `pdf` 中的字体和图片
全部反编译出来(这可以让我继续我的盗版 `MathTime Pro` 数学字体计划):

![pdfextract](https://pic3.zhimg.com/80/7c7a3a4fc9ea27d41d93b6760105fbef_720w.jpg?source=1940ef5c)

看见没有, 图片和字体全部被反编译出来了, 生成的图片好说, 随便怎么弄都行.
字体呢? 开个 `Fontforge` 字体玩去吧. 哈哈.

+ `pdfinfo` 命令呢, 会把 `pdf` 的一些信息提取出来,
虽然是从 `pdf` 代码中相应文字直接贴到终端或cmd输出的, 但是速度很快.

![pdfinfo](https://pic1.zhimg.com/80/e1faaed0082ebaaf5e0d27a8a704155c_720w.jpg?source=1940ef5c)

+ `pdfclean` 是用来修复一些损坏了的 `pdf` 的. 我手上没有坏的pdf, 暂无测试.

+ `pdfdraw` 可以将 `pdf` 转换为图片, 此命令参数如下:

![pdfdraw](https://pica.zhimg.com/80/fcba5652896b3672bc3008a4310abbd6_720w.jpg?source=1940ef5c)

如运行

    mudraw -r 300 -o %d.png a-crop.pdf

则可以生成分辨率为 `300` 的图片.

另外还有一些 `pdf` 工具是比较不错的, 如 [pdftk](http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/).
