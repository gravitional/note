# physics 宏包

[physics – Macros supporting the Mathematics of Physics](https://www.ctan.org/pkg/physics)

这个软件包的目标是使物理学方程的排版更简单, 更快, 更容易被人阅读.
为了达到这个目的, 本软件包中使用的名称, 力图使每个命令的目的一目了然, 并消除了阅读和编辑 `physics` 代码时的任何歧义.
从实用的角度来看, 有一套定义明确的 `快捷方式` 来访问这些命令的 `长形式` 是很方便的.
因此, 下面列出的命令是以它们的 `长名称` 来定义的, 然后以默认的 `命令缩写` 来明确显示.
这些 `命令缩写` 的目的是使人们容易记住名称, 和它们所代表的内容.

## crash, unicode-math

由于 `physics` 宏包使用 `amsmath` 的 `\boldsymbol` 命令,
`\boldsymbol` 与 `unicode-math` 包存在冲突, 会产生类似下面的报错:

    ! Improper alphabetic constant.

所以重定义 `boldsymbol` 命令.

```latex
\documentclass{article}
\usepackage{unicode-math}
\renewcommand{\boldsymbol}{\symbf} %重定义 ams 包的黑体
\newcommand*{\sbf}{\symbf} %粗体数学符号或矢量符号
\setmainfont{XITS}
\setmathfont{XITS Math}

\begin{document}
This is a simple math example with a variable $k$.
This works $\symbfit{k}$.
What I actually need is this: $\mathbfcal{X}$ and $\symbf{\Theta}$.
Compare with $\mathcal{X}$ and $\Theta$.
\end{document}
```

## crash, siunitx

`physics` 宏包中定义的 `\qty` 命令和 `siunitx` 宏包的 `\qty` 存在冲突,
所以在使用 `physics` 时, 使用 `siunitx` 包第二版的命令, 如 `SI`, `si` 来避免冲突.
