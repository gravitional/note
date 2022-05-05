# biblatex

[biblatex如何使用-刘海洋](https://www.zhihu.com/question/275094287/answer/380685215)

`biblatex` 的优势体现在多个方面, 例如:

1. 文献数据结构全面, 可引用的内容更多.
用 `bibtex` 一般只能引用编号, 配合 `natbib` 宏包和特定的 `bst` 可以引用作者, 年代.
而 `biblatex` 还可以引用标题, `URL` 或者其他你想引用的文献信息.
因为文献信息以较完整的数据结构保存在生成的 `bbl` 文件中, 所以引用的内容更多. 这是相比 `BibTeX` 的一个基本设计上的不同.

2. 输出控制由 `LaTeX` 代码完成, 开发容易.
`bst` 的语言比较晦涩, 开发难度大一些, `biblatex` 开发就容易多了,
所以现在第三方 `biblatex` 格式出现得比较多, 用户自己调整相比改 bst 也容易一些.

3. 献提取与排序使用功能更强的 `biber`.
`biber` 支持 `Unicode`, 支持按汉字排序, 支持按文献标题排序, 编辑甚至翻译者排序, 等等, 这都是 BibTeX 无法提供的.

另外, `biblatex` 开发晚得多, 所以直接提供的功能也就多很多.
当然, 也因为出现得晚, 期刊投稿接受的就相对少一些.

怎么用关键还是看文档. 在 `biblatex` 文档所在的目录下面其实有一堆示例文件. 例子:

```latex
\documentclass{ctexart}

\usepackage[style=gb7714-2015]{biblatex}
\addbibresource{数据库名.bib}

\begin{document}

引用 \cite{XXX}

\printbibliography

\end{document}
```

排版用

```bash
latex 文件名; biber 文件名; latex 文件名; latex 文件名;
```

即把以往的 `bibtex` 命令换成 `biber`.

如果用 `latexmk` 命令排版的话, 会自动识别.
