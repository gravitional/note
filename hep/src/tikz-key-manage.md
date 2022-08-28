# key 管理

## key 简述

`p975`;

`key` 就是`tikz`中的各种关键字, 
它们是通过包`pgfkeys`管理的. `\usepackage{pgfkeys}`.

`key`的例子: `/tikz/coordinate system/x`, 或者只用写`/x`. 
这种写法类似于`Unix`上的文件路径. 可以使用绝对路径, 也可以使用相对路径.
`tikz`的`key`名称经常包含空格.
调用`key`使用`\pgfkeys`命令. `pgfkeys`接受`key=value`形式的参数. 
例如:

```latex
\pgfkeys{/my key=hallo, /your keys=something\strange, others=something else}
```

可以在`key`中存储一些`code`, 通常用`handler`来管理`key`的内容. 例如:

```latex
\pgfkeys{/my key/.code=The value is '#1'.}
\pgfkeys{/my key=hi!}
```

有许多`handler`可以用来定义`key`. 例如, 我们可以定义一个具有多个参数的`key`:

```latex
\pgfkeys{/my key/.code 2 args=The values are '#1' and '#2'.}
\pgfkeys{/my key={a1}{a2}}
```

常用`handler`有:

+ `.default`:提供默认值.
+ `/.value required`:必须指定参数.
+ `/.value forbidden`:不能指定参数.

`tikz`的所有`key`都以`/tikz`开头. 
然而不必每次都显式加上这个前缀, 可以使用`/.cd`来声明默认目录.

```latex
\pgfkeys{/tikz/.cd,line width=1cm, linecap=round}
```

当处理一个`key`时, 除了直接执行某些代码, 
也可以递归调用另一些`keys`, 这种`key`被称为`styles`. 
`styles`本质上就是`key`的列表. 例如:

```latex
\pgfkeys{/a/.code=(a:#1)}
\pgfkeys{/b/.code=(b:#1)}
\pgfkeys{/my style/.style={/a=foo,/b=bar,/a=#1}}
\pgfkeys{/my style=wow}
```

`.styles`也可以带有参数`#1`, 如同普通的`.code`.

## The Key Tree

`p975`;

`key`的组织方式类似`Unix`. `/a/b.../x`中, 
前面的`/a/b.../`是`路径`, 后面的`x`是`名称`.
`pgfkeys`包中有一些内部命令, 如`\pgfkeyssetvalue`, `\pgfkeyslet`等等. 
但是通常用户不需要直接调用这些命令.

简要提一下`\def`, `\let`,`\edef`三个命令: [What is the difference between \let and \def](https://tex.stackexchange.com/questions/258/what-is-the-difference-between-let-and-def)
`\let`相当于直接赋值, 右边的式子计算之后赋给左边. 
`\def`命令相当于mma中的`SetDelay`即`:=`, 会在被调用时重新计算.
`\edef`是`expand def`的缩写, 也就是赋值之前右边的式子会被展开.

## Key Handlers

`handler`相当于`key`的方法, 或者构造函数之类的.
p984, 常用的 `key handler`:

+ `<key>/.cd`: 设置默认路径为`key`.
+ `<key>/.is family`: 当使用`key`时, 
当前路径被设置为`key`,效果同`<key>/.cd`.
+ `<key>/.default=<值>` :设置`key`的默认值. 
例如`\pgfkeys{/width/.default=1cm}`

+ `<key>/.value required`: 表明必须赋值.
+ `<key>/.value forbidden`: 表明禁止赋值.
+ `<key>/.code=<代码>`: 添加代码, 可以有一个参数`#1`
+ `<key>/.code 2 args=<代码>`:表示有两个参数`{#1}{#2}`, 
如果第二个未给出, 将会是空字符. 如果传入`first`, 
第一个参数将是`f`, 第二个将是`irst`, 所以需要用`{}`包裹起来.

+ `<key>/.code n args={<m>}{<代码>}`:
传入`m`个参数`{#1}{#2}{#3}...`, `m`为`0~9`, 
不能多也不能少, 空格也可以作为参数.

+ `<key>/.code args={<参数模式>}{<代码>}`:
可以指定任意的参数模式, 比如`(#1/#2)`,
调用对应的写成`(first/second)`, 空格将被去掉.

+ `<key>/.add code={<前缀代码>}{<后缀代码>}`: 将代码添加到已经存在的`key`.
+ `<key>/.prefix code=<前缀代码>`: 类似`.add code`, 但只添加前缀.
+ `<key>/.append code=<后缀代码>`: 类似`.add code`, 但只添加后缀.

其中 `.code 2 args`和`.code args`的区别见下面的例子:

```latex
\pgfkeys{/page size/.code 2 args={\paperheight=#2\paperwidth=#1}}
\pgfkeys{/page size={30cm}{20cm}}
% 同样的定义, 使用 .code args
\pgfkeys{/page size/.code args={#1 and #2}{\paperheight=#2\paperwidth=#1}}
\pgfkeys{/page size=30cm and 20cm}
```

## Defining Styles

`.style`是`key`的列表, 它的`handler`和`key`基本相同, 
只需要作替换`.code`->`.style`

# 为 node 自动添加名称标签

ref: [为 node 自动添加名称标签](https://zhuanlan.zhihu.com/p/429321732)

用 `tikz` 绘图, 经常定义 `named node`/`coordinate`.
但 `node` 多了之后, 对着输出结果尝试调整绘图时, 
难以快速知道某个图形对应的 `node` 的名字.

`tikz-auto-mark-nodes.tex` 提供了 在 `node` 斜上方自动添加 `node` 名称的功能 .
源码见项目 [muzimuzhi/latex-examples](https://github.com/muzimuzhi/latex-examples).

## 应用示例 1

把它应用于同项目下例子 `tikz-example-flowchar2-fit-a4paper.tex` 的效果:

+ 在导言区添加

    ```latex
    \input tikz-auto-mark-nodes 
    ```
    
    然后给 `tikzpicture` 环境添加选项 `auto mark`:

    ![流程图](https://pic3.zhimg.com/80/v2-5a0f823ea8c81967a77b258eacbdc9de_720w.jpg)

+ 为了减少重叠, 调整角度后

    ```latex
    \input tikz-auto-mark-nodes
    \tikzset{
      every auto mark/.append style={
        pin position=20
      }
    }

    % \begin{tikzpicture}[..., auto mark]
    ```

## 实现简介

+ 每个标签是一个 `node pin`, 包含文字和指向该 `node` 的线.
+ 通过 patch `node` 相关代码, 记录有名字的 `node` 的信息, 
包括 `node` name 和 `node` shape.

+ 在每个 `tikzpicture` 末尾, 使用记录的信息, 
通过 `\node also[pin={[<pin options>]<text>}](<node name>)` 集中绘制.

## 可配置项

每个 `auto mark` 都会应用选项 `every auto mark` 
和 `every auto <shape> mark`,  初始值为:

```latex
every auto mark/.style={
  font=\ttfamily,
  rotate=45,
  red, anchor=west,
  pin position=45,
},
every auto coordinate mark/.style={
  blue, anchor=east, pin position=180+45
},
```

+ `pin` 里的文字由 `\tikzAutoMarkText` 控制, 
初始定义为 `\tikzNodeName`

+ 在 `every ... auto mark` 这类选项和 `\tikzAutoMarkText` 的定义中, 
可以使用 `\tikzNodeShape` 和 `\tikzNodeName` 来
表示/代替 当前 `node` 的形状和名称.

## 应用示例 2

来自 `pgfmanual Sec. 5 "Tutorial: Diagrams as Simple Graphs"`

```latex
\documentclass{article}
\usepackage{tikz}
\usetikzlibrary{graphs,shapes.misc}
\tikzset{ampersand replacement=\&,point/.style={coordinate}}

\input tikz-auto-mark-nodes
\tikzset{
  every auto coordinate mark/.append code={%
    \expandafter\ifodd\expandafter\@gobble\tikzNodeName\else
      \pgfkeysalso{pin distance=6ex}% distance doubled
    \fi
  }
}

\begin{document}
\def\matrixcontent{
  % First row:
  \& \& \& \& \& \& \&  \& \& \& \& \node (plus) [terminal] {+};\\
  % Second row:
  \node (p1) [point]  {}; \&    \node (ui1)   [nonterminal] {unsigned integer}; \&
  \node (p2) [point]  {}; \&    \node (dot)   [terminal]    {.};                \&
  \node (p3) [point]  {}; \&    \node (digit) [terminal]    {digit};            \&
  \node (p4) [point]  {}; \&    \node (p5)    [point]  {};                      \&
  \node (p6) [point]  {}; \&    \node (e)     [terminal]    {E};                \&
  \node (p7) [point]  {}; \&                                                    \&
  \node (p8) [point]  {}; \&    \node (ui2)   [nonterminal] {unsigned integer}; \&
  \node (p9) [point]  {}; \&    \node (p10)   [point]       {};\\
  % Third row:
  \& \& \& \& \& \& \&  \& \& \& \& \node (minus)[terminal] {-};\\
}

\tikzset{
  nonterminal/.style={
    % The shape:
    rectangle,
    % The size:
    minimum size=6mm,
    % The border:
    very thick,
    draw=red!50!black!50,         % 50% red and 50% black,
                                  % and that mixed with 50% white
    % The filling:
    top color=white,              % a shading that is white at the top...
    bottom color=red!50!black!20, % and something else at the bottom
    % Font
    font=\itshape
  },
  terminal/.style={
    % The shape:
    rounded rectangle,
    minimum size=6mm,
    % The rest
    very thick,draw=black!50,
    top color=white,bottom color=black!20,
    font=\ttfamily},
  skip loop/.style={to path={-- ++(0,#1) -| (\tikztotarget)}}
}
\tikzset{terminal/.append style={text height=1.5ex,text depth=.25ex}}
\tikzset{nonterminal/.append style={text height=1.5ex,text depth=.25ex}}

\begin{tikzpicture}[skip loop/.style={to path={-- ++(0,#1) -| (\tikztotarget)}},
                    hv path/.style={to path={-| (\tikztotarget)}},
                    vh path/.style={to path={|- (\tikztotarget)}},
                    auto mark]
  \matrix[row sep=1mm,column sep=2mm] { \matrixcontent };

  \graph {
    (p1) -> (ui1) -- (p2) -> (dot) -- (p3) -> (digit) -- (p4)
         -- (p5)  -- (p6) -> (e) -- (p7) -- (p8) -> (ui2) -- (p9) -> (p10);
    (p4) ->[skip loop

=-5mm]  (p3);
    (p2) ->[skip loop=5mm

]   (p5);
    (p6) ->[skip loop=-11mm] (p9);
    (p7) ->[vh path]         (plus)  -> [hv path] (p8);
    (p7) ->[vh path]         (minus) -> [hv path] (p8);
  };
\end{tikzpicture}
\end{document}
```

+ 为了避免重叠, 这里为名为 `p2, p4, ..., p<odd num>` 的 `coordinate` 设置了更大的 `pin distance`.
+ 修改 `\tikzAutoMarkText` 的一个例子, 让它显示 `<first four chars of node shape>:<node name>:`

    ```latex
    \def\pickFirstFour#1#2#3#4#5\stop{#1#2#3#4}
    \renewcommand\tikzAutoMarkText{%
      \expandafter\pickFirstFour\tikzNodeShape\stop:\tikzNodeName}
    ```
