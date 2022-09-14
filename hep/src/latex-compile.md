# latex 文档编译处理

## latexmk 选项

一般来说, `latexmk` 的通用`cmd`命令形式为:

`latexmk [options] [file]`

所有的选项可以用单个`-`连字符,也可以用双连字符`--`引入,e.g., "latexmk -help" or "latexmk --help".

***
注意:

除了文档里列出的选项, `latexmk`认识几乎所有the options recognized by the latex, pdflatex programs (and their relatives),
在当前的 TexLive and MikTeX 发行版中.

这些程序的一些选项还会引起 latexmk 的特殊 action or behavior,在本文档中有解释.否则,它们被直接传递给latex or pdflatex.
run `latexmk -showextraoptions`给出选项列表,这些选项被直接传递给latex or pdflatex.

***
注意:

"Further processing" 意味着需要运行其他程序,或者再次运行`latex`(etc),如果没有 `errors` 的话.
如果你不想让`latex`在遇到错误的时候停下,应该使用 latexmk's option `-interaction=nonstopmode`

`-xelatex`  使用`xelatex`编译
`-pv `   - preview document.  (Side effect turn off continuous preview)
` -pv-`   - turn off preview mode
`-pvc`   - preview document and continuously update.  (This also turns  on force mode, so errors do not cause latexmk to stop.)
(Side effect: turn off ordinary preview mode.)
`-pvc-`  - turn off -pvc

`-view=default` - viewer is default (dvi, ps, pdf)
`-view=ps`      - viewer is for ps
`-view=pdf`     - viewer is for pdf

`-bibtex`       - use bibtex when needed (default)
`-bibtex-`      - never use bibtex

`-cd`    - Change to directory of source file when processing it

`-recorder` - Use -recorder option for (pdf)latex (to give list of input and output files)
` -recorder-` - Do not use -recorder option for (pdf)latex

***
简单传递的命令

`-error-line=n` set the width of context lines on terminal error messages
`-half-error-line=n`      set the width of first lines of contexts in terminal error messages

`-file-line-error `       enable `file:line:error` style messages
`-halt-on-error`          stop processing at the first error
`-interaction=STRING`     set interaction mode (STRING=batchmode/nonstopmode/scrollmode/errorstopmode)
`-synctex=NUMBER`         generate `SyncTeX` data for previewers if nonzero

## 安装latex包

[Ubuntu/Mint下LaTeX宏包安装及更新](https://blog.csdn.net/codeforces_sphinx/article/details/7315044)

一般使用texlive的包管理工具,否则需要手动安装:

1. Get the package from [CTAN](http://www.ctan.org/CTAN) or wherever.
2. 如果其中有一个文件是`.ins` 结尾的,打开终端,执行命令`latex foiltex.ins`,就获得了安装需要的包.大多数 latex 包没有打包,所以可以跳过这一步.
3. 现在你需要决定,这个包要安装给所有用户使用,还是only for you.
4. 在*nix 系统上(OSX),给所有用户使用,安装到`local` TeX tree, 给自己使用,安装到`user`TeX tree.

查看`texmf.cnf`文件,它通常在`$TEXMF/web2c`文件夹,但是可以用`kpsewhich texmf.cnf`定位.

`local` Tree 的位置在 `TEXMFLOCAL` variable 中定义,通常像是`/usr/local/share/texmf`.
`user`  Tree 的位置在`TEXMFHOME`中定义,通常像是`$HOME/texmf` or `$HOME/.texliveXXXX`

如果这些变量没有定义,你需要手工指定.修改`local` Tree 可能需要 root 权限.建议修改 user tree, 因为在升级的时候,不会被覆盖.这样在备份系统的时候,可以一起备份.

现在,你需要告诉 Latex 有新文件.这取决于 LaTex 发行版.

1. 对于 TeXLive,运行`texhash`,可能需要 root 权限
2. 对于MikTeX,运行 `Settings (Admin)` and press the button marked `Refresh FNDB`

5. 最后,你需要告诉 LyX 有新的包可以使用.在LyX 中,运行 `Tools->Reconfigure` and then restart LyX

现在,新的文档 class 可以选择了,`Document->Settings->Document Class`.

## latex包安装方式2

首先要找到默认宏包所在目录,一般是:

```bash
/usr/share/texmf/tex/latex
/usr/share/texmf-texlive/tex/latex
```

1. 如果是安装一个新的宏包,就直接把宏包的压缩文件扔进第一个目录下,直接解压就行,注意解压后的文件里可能有安装说明,照着安装说明做就是了.
如果是更新一个宏包,一般都可以在第二个目录下找到,把原先的宏包重命名成`*-backup`,再解压新下载的宏包压缩文件,同时如果有安装说明的话,也照着做.
2. 之后要对宏包重新标记下,终端下执行

```bash
# texhash
```

`Log off/Log in`后,就完成了~

## latex pdf 裁剪

`texlive` 自带了一个叫做 `pdfcrop` 的 `perl` 脚本

使用方法如下:

`pdfcrop --margins 3 --clip input.pdf output.pdf; ` 或者

```bash
pdfcrop --clip --bbox '120 480 570 830' input.pdf output.pdf;
pdfcrop --clip --bbox '60 660 516 775' moban.pdf moban_crop.pdf && evince moban_crop.pdf  # 国科大试卷的裁减参数
```

四个数字的含义是, 以左下角为原点, 给出 `left bottom right top` 的数值(左下--右上), 单位是打印点 -- `point`:

    1 point = 0.3527 mm = 1/72 inch
    A4纸张 210mm * 297mm = 595.4 point * 842.1 point

## bundledoc打包LaTeX宏包

[使用bundledoc打包LaTeX宏包依赖](https://lttt.vanabel.cn/2018/06/01/shiyongbundledocdabaolatexhongbaoyilai.html)

脚本的使用.
新建test.tex文档如下:

```latex
\RequirePackage{snapshot}
Hello world!
```

这里的关键是需要第一行的 `\RequirePackage{snapshot}` 
产生宏包依赖列表 `test.dep`. 
例如上面的代码产生的宏包依赖列表为:

```latex
\RequireVersions{
  *{application}{TeX}     {1990/03/25 v3.x}
  *{format} {LaTeX2e}     {2017-04-15 v2.e}
  *{package}{snapshot}    {2002/03/05 v1.14}
...
}
```

接着, 我们需要在 `test.tex` 同目录下创建 `bundledoc` 配置文件 `bundledoc.cfg`

```conf
# basic config file for use of arlatex + bundledoc
bundle: (arlatex --document=$BDBASE.tex $BDINPUTS | tar -cvf - $BDINPUTS | gzip --best > $BDBASE-all.tar.gz )
sink:   > /dev/null 2>&1
find:   kpsewhich -progname=latex $BDINPUTS
```

最后, 如果你使用修改后的脚本的话, 运行

```bash
bundledoc --config=bundledoc.cfg --verbose test.dep
```

则得到打包后的压缩文件 `test-all.tar.gz`
需要注意的是, 我们需要压缩文件的软件 `tar` 与 `gzip`.
