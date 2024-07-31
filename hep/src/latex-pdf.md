# latex pdf 操作

[如何用Latex合并多个pdf文件](https://blog.csdn.net/zimosangtian/article/details/102993430)
[Merge two PDF files output by LaTeX](https://tex.stackexchange.com/questions/8662/merge-two-pdf-files-output-by-latex)

## 合并多个pdf文件

### LaTeX

use Herbert's answer: the [pdfpages](http://ctan.org/pkg/pdfpages) package

```latex
\documentclass{article}% or something else
\usepackage{pdfpages}
\begin{document}
\includepdf[pages=-]{paper1}
\includepdf[pages=-]{paper2}

\end{document}
```

You could also keep the document page sizes by adding a option:

```latex
\includepdf[pages=-,fitpaper]{paper1}
\includepdf[pages=-,fitpaper]{paper2}
```

And not to repeat yourself use this:

```latex
\includepdfset{pages=-,fitpaper}
\includepdf{paper1}
\includepdf{paper2}
\includepdf{paper3}
\includepdf[fitpaper=false]{paper4} // you can add document specific options
\includepdf{paper5}
\includepdf{paper6}
\includepdfset{} // to put default values back
```

### Command Line

pdftk

```bash
$ pdftk 1.pdf 2.pdf 3.pdf cat output 123.pdf
```

+ GhostScript(via Macworld)

```bash
gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=merged.pdf source1.pdf source2.pdf source3.pdf etc.pdf
```

+ PDFJAM is a suite of scripts that uses LaTeX and pdfpages on the backend. (via Uwe Hermann)

```bash
pdfjoin foo1.pdf foo2.pdf --outfile bar.pdf
```

+ stapler is a pure Python alternative to pdftk.

```bash
stapler cat in1.pdf in2.pdf out.pdf
```

+ PyMuPDF is a Python binding for MuPDF – "a lightweight PDF and XPS viewer".

```bash
python -m fitz join -o output.pdf file1.pdf file2.pdf
```

+ qpdf is a command-line tool and C++ library that performs content-preserving transformations on PDF files.

```bash
qpdf --empty --pages file1.pdf file2.pdf -- output.pdf
```

### GUI

Preview (Mac only) example
Acrobat Pro (non-free) video example
GUIPDFTK
PDFCreator (Win only, free, Open Source, acts like a printer ⇒ no hyperlinks etc.)
PDF Mod (Linux, free software)
PDF-Shuffler (Linux, free software)
PDFsam (JRE - Windows, Linux, Mac, free and non-free versions)

[This question](https://tex.stackexchange.com/q/5842/1402) is very similar although the questioner didn't realize it.

## pdf 页面尺寸

一英寸中有72个单位.
一页纸大小宽 612 个单位, 高792个单位.
默认的坐标会乘上100与PDF的规范匹配.

[sample pdf](./sample.txt)
