# latex 封面 页码

[thispagestyle](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#g_t_005cthispagestyle)

## 页码

### `\thispagestyle`

用法: `\thispagestyle{style}`

工作方式与 `\pagestyle` 相同(请参阅\pagestyle), 除了它仅更改为当前页面的样式.
该声明具有 global scope, 因此其效果不受 大括号 或 环境 的限制.

通常章或节的第一页有不同的风格.
例如, 如下 LaTeX图书文档的第一章的第一页是 `plain` 样式的,
这是默认的样式(请参阅页面样式).

```latex
\documentclass{book}
\pagestyle{headings}
\begin{document}
\chapter{First chapter}
...
\chapter{Second chapter}\thispagestyle{empty}
...
```

plain 样式上有一个页码, 位于页脚的中心.
为使页面完全为空白,
命令 `\thispagestyle{empty}` 紧跟在第二个 `\chapter`后面.
