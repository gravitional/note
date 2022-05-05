# unicode-math 宏包

[unicode-math]: https://ctan.org/pkg/unicode-math

## unicode 数学字体

[mathspec]: https://ctan.org/pkg/mathspec
[change the math italic font in XeTeX/fontspec]: https://tex.stackexchange.com/questions/11058/how-do-i-change-the-math-italic-font-in-xetex-fontspec
[Error:Extended mathchar used as mathchar]: https://tex.stackexchange.com/questions/431013/error-extended-mathchar-used-as-mathchar-when-using-bm
[Theunicode-mathpackage]: https://mirrors.bfsu.edu.cn/CTAN/macros/unicodetex/latex/unicode-math/unicode-math.pdf

其他指定数学字体的包有: [mathspec][], 以及 [unicode-math][].
`stackexchange`上有关于 [change the math italic font in XeTeX/fontspec][] 的讨论. 作者给出的示例代码为:

```latex
\documentclass{article}
\usepackage{unicode-math}
\setmainfont{Georgia}
\setmathfont{xits-math.otf}
\setmathfont[range=\mathit]{Georgia Italic}
\begin{document}
Hello $a+b=c$
\end{document}
```

载入`unicode-math`包,并使用`\boldsymbol`时会报错: [Error:Extended mathchar used as mathchar][],
解决方案是不使用`\bm`,`\boldsymbol`命令,而使用`\symbf`,`\symcal`等命令,
见[Theunicode-mathpackage][] 包说明 Page 15.
`unicode-math`引入了一些新的命令,例如:

```latex
\symbb, \symbbit, \symcal, \symscr, \symfrak, \symsfup, \symsfit,
\symbfsf, \symbfup, \symbfit, \symbfcal, \symbfscr, \symbffrak, \symbfsfup, \symbfsfit
```

用来表示单个粗体数学符号, 跟粗体普通文字是不同的,
粗体普通文字使用`latex`中通常的`\mathbb, \mathbbit, \mathcal`等命令. 例子是:

```latex
\documentclass{article}
\usepackage{unicode-math}
\setmainfont{XITS}
\setmathfont{XITS Math}

\begin{document}
This is a simple math example with a variable $k$.
This works $\symbfit{k}$.
What I actually need is this: $\mathbfcal{X}$ and $\symbf{\Theta}$.
Compare with $\mathcal{X}$ and $\Theta$.
\end{document}
```

## Double-struck

page 14, 5.4.2

`double-struck` 风格(也被称为 `黑板粗体`) 由直立的拉丁字母 {𝕒-𝕫,𝔸ℤ}, 数字 𝟘-𝟡, 求和符号 ⅀ 和四个希腊字母组成: {ℽ ℼ ℾ ℿ}.

虽然 `\symbb{sum}` 确实产生了双写的求和符号, 但它的极限没有正确对齐.
因此, 建议使用字面字符或控制序列 `\Bbbsum` 来代替.
还有五个拉丁语斜体双写字母: ⅅ ⅆ ⅇ ⅈ ⅉ .
这些字母可以用 `\mathbbit` 样式开关来访问, 如果不是用它们的字面字符或控制序列的话, 但要注意只有这五个字母会有预期的输出.

## 与 mathtool 冲突

[Underbrace using STIX2 math font](https://tex.stackexchange.com/questions/521394/underbrace-using-stix2-math-font)

`mathtools` 软件包包含对 `LaTeX内核` 中定义的 `underbrace` 和 `overbrace` 的修复.
如果我们比较这两个定义:

```latex
%%% LaTeX内核
\DeclareRobustCommand\underbrace[1]{\mathop{\vtop{\m@th\ialign{##\crcr
   $\hfil\displaystyle{#1}\hfil$\crcr
   \noalign{\kern3\p@\nointerlineskip}%
   \upbracefill\crcr\noalign{\kern3\p@}}}}\limits}

%%% mathtools.sty
\def\underbrace#1{\mathop{\vtop{\m@th\ialign{##\crcr
   $\hfil\displaystyle{#1}\hfil$\crcr
   \noalign{\kern.7\fontdimen5\textfont2\nointerlineskip}%
   \upbracefill\crcr\noalign{\kern.5\fontdimen5\textfont2}}}}\limits}
```

我们看到它们几乎是一样的, 但是 `mathtools` 改进了定义, 没有使用固定的 `3pt` 长度, 而是用当前的 `数学字体` 设置来调整它.

然而, `unicode-math` 中的 `underbrace` 的定义是:

```latex
\UnicodeMathSymbol{"023DF}{\underbrace}{\mathunder}{bottom curly bracket (mathematical use)}
```

翻译成更低级别的:

```latex
\underbrace=\protected macro:
#1->\mathop {\Umathaccent bottom 7\symoperators "023DF\scan_stop: {{}#1}}\limits
```

如果你在 `unicode-math` 之后加载 `mathtools`, 会发生的情况是:
`Unicode` 定义的 `\underbrace` 被 `mathtools` 完成的上述修正所覆盖.
请注意, 你没有得到任何错误:
事实上, `\upbracefill` 使用的数学字符在 `unicode-math` 中并不对应于传统 `LaTeX` 数学字体中的 `brace片段`.

总结: `mathtools` 对传统的数学字体做了很好的修复, 但是当 `unicode-math` 被加载时, 它不应该这样做.
在 `mathtools` 被更新之前, 解决方案是在 `unicode-math` **之前** 加载它.
