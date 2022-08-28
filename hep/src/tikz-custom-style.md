# tikz自定义

## 自定义 style

```latex
\begin{tikzpicture}[abcde/.style={
  double distance=10pt,
  postaction={
  decorate,
  decoration={
      markings,
      mark=at position .5 with {\arrow[blue,line width=1mm]{>}},
    }
  }
  }]

  \begin{feynman}
  ...
  \end{feynman}

  \draw [help lines] grid (3,2);
  \draw[abcde]  (a1) -- (b1);

\end{tikzpicture}
```

## 指定线型

`page173`; Graphic Parameters: Line Width, Line Cap, and Line Join

`/tikz/dash pattern=<dash pattern>`
`/tikz/dashed` :  指定虚线模式的简写

## 指定节点形状,node shape

`p224`; Nodes and Their Shapes

比如

```tikz
\begin{tikzpicture}
\draw (0,0) node[minimum size=2cm,draw] {square};
\draw (0,-2) node[minimum size=2cm,draw,circle] {circle};
\end{tikzpicture}
```

`p785`: 72 Shape Library, 形状库

`p229`: 17.2.3 Common Options: 
Separations, Margins, Padding and Border Rotation: 给出了 `node` 一些几何参数的选项

`p730`: 63 Pattern Library : `node` 可以使用 `pattern` 填充, 某种图形模式.

## 添加任意装饰

`p191`; Arrows 箭头
`p196`; 指定箭头大小, 形状
`p212`; Reference: Arrow Tips 与定义箭头形状参考

```tikz
\usetikzlibrary{arrows.meta}
```

`p365`; Decorated Paths 装饰路径
`p646`; Arbitrary Markings 添加任意装饰

>Decoration markings
>`marking`可以被认为是"小图片", 
>或更准确地说是放置的`some scope contents`, 放置在路径的某个位置"上".

假设`marking`为简单的十字.  
可以用以下代码产生:

```latex
\draw (-2pt,-2pt) -- (2pt,2pt);
\draw (2pt,-2pt) -- (-2pt,2pt);
```

如果我们将此代码用作路径上 `2cm` 处的 `marking`,  则会发生以下情况:
`pgf`先确定沿路径`2cm`的位置.  
然后将坐标系平移到此处并旋转它, 使`x`轴正向与路径相切.
然后创建一个保护用的`scope`, 
在内部执行上述代码--最后路径上出现一个 叉子 形状.

`marking`允许在路径上放置一个或多个装饰.
除了后面讲的少数情况, `decoration`摧毁路径输入, 
也就是说, 计算完成, 再作完装饰之后, 路径就消失了.
一般需要`postaction`来添加装饰, 
`postaction` 表示完成路径绘制之后再进行操作.

```latex
\begin{tikzpicture}[decoration={
        markings,% 启用 markings
        mark=% mark 的形状
        at position 2cm
        with
          {
            \draw (-2pt,-2pt) -- (2pt,2pt);
            \draw (2pt,-2pt) -- (-2pt,2pt);
          }
      }
  ]
  \draw [help lines] grid (3,2);
  \draw [postaction={decorate}] (0,0) -- (3,1) arc (0:180:1.5 and 1); % postaction 表示画完路径之后再装饰, 再摧毁路径.
\end{tikzpicture}
```

## Pics:复用图形组件

p263, `pic`是`picture`的简称.
通过预先定义的图片名字, 可以在指定的地方复用图形. 例如先定义一个海鸥的形状:

```latex
\tikzset{
seagull/.pic={
% Code for a "seagull". Do you see it?...
\draw (-3mm,0) to [bend left] (0,0) to [bend left] (3mm,0);
}
}
```

使用`\tikzset`定义的是全局的, 整个文档都可以调用. 然后调用它:

```latex
\tikz \fill [fill=blue!20]
(1,1)
-- (2,2) pic {seagull}
-- (3,2) pic {seagull}
-- (3,1) pic [rotate=30] {seagull}
-- (2,1) pic [red] {seagull};
```

### 定义新的Pic类型

如`pic`命令说明中所述, 要定义新的`pic`类型, 您需要

1. 定义一个路径前缀为`/tikz/pics`的`key`,
2. 将`/tikz/pics/code`设置为`pic`的`code`.

这可以使用`.style` handler 实现:

```latex
\tikzset{
  pics/seagull/.style ={
      % 当调用 seagull 的时候, 下面的代码会设置 seagull 的 code key:
      code = { %
          \draw (...) ... ;
        }
    }
}
```

一些简单的情况下, 可以直接使用`.pic` handler,

```latex
\tikzset{
    seagull/.pic = {
    \draw (...) ... ;
  }
}
```

此`handler`只能对带有`/tikz/`前缀的`key`一起使用, 
因此通常应将其用作`TikZ`命令或`\tikzset`命令的选项.
它使用`<key>`的路径, 并把其中的`/tikz/`替换为`/tikz/pics/`.
最终得到一个`style`, 能够执行`code = some code`.

大多数情况下, `.pic` handler足以设置`keys`.
但是, 在某些情况下确实需要使用第一个版本:

+ 当您的图片类型需要设置`foreground`或`background`代码时.
+ 如果给`key`提供了复杂的参数. 例如:

```latex
\tikzset{
    pics/my circle/.style = {
    background code = { \fill circle [radius=#1]; }
  }
}
\tikz [fill=blue!30]
```

这里给`my circle`使用了参数.

```latex
<key>/.code n args={<m>}{<代码>}
```

传入`m`个参数`{#1}{#2}{#3}...`, `m`为`0~9`, 不能多也不能少, 空格也可以作为参数.

## 颜色

### 透明度

`page 353`: 23 Transparency

常用透明度选项:

```latex
/tikz/draw opacity=<value>
/tikz/fill opacity=<value>
```

`<value>`除了数字取值`[0,1]`之外, 还可以下列预设:

```latex
/tikz/transparent
/tikz/ultra nearly transparent
/tikz/very nearly transparent
/tikz/nearly transparent
/tikz/semitransparent
/tikz/nearly opaque
/tikz/very nearly opaque
/tikz/ultra nearly opaque
/tikz/opaque
```
