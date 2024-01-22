# latex 编译错误

## file ended while scanning

[file ended while scanning use of xxx](https://blog.csdn.net/WangJiankun_ls/article/details/78671250)

出现这个问题的原因是使用某些命令时,
给出的参数不完整或者漏了半个大括号.

举例:

```latex
\begin{document}
第一行  第二行  第三行
\section{第一部分  第四行  第五行
\end{document}
```

上边这种情况明显时 `\section` 命令中丢失了半个大括号, 加上即可.
有的命令需要两个参数时, 如果漏掉一个, 也会出现这种情况, 例如 `\frac`

```latex
$\frac{1}{x+1$
```

编译上边的命令, 会提示
`File ended while scanning use of \@frac`,
明显是第二个参数不完整.

## Theorem style plain already defined

[Package ntheorem Error: Theorem style plain already defined](https://blog.csdn.net/lcly17/article/details/124599989)

这可能是因为ntheorem包和amsthm包冲突导致的.

```latex
\usepackage[thmmarks,amsmath]{ntheorem}
```

解决方法: 不要两个都用, 删去一个包的引用即可.

注意有的模板类文件(.cls)可能会预先引入其中某个包,
例如China Communication的模板文件(ccjnl.cls)就预先引入了`amsthm`包
这个时候就只能使用 `amsthm` 包而不能使用 `ntheorem` 包了.

## unexpected EOF in PK file

[dvips: ! Bad PK file](https://tex.stackexchange.com/questions/13322/dvips-bad-pk-file)

具体原因未知, 在 tlmgr 中重新运行 `操作` 下面的
regenerate filename database
regenerate formats
regenerate fontmaps
再重启, 就好了
