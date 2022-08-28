# tikz Graph Drawing

`p416` ; 27 Introduction to Algorithmic Graph Drawing

算法图形绘制(Algorithmic graph drawing)
( 或以下简称为图形绘制) 是以算法计算 图形节点 在 
页面上的位置的过程, 以便使图形 `看起来漂亮`.

它的想法是, 作为人类用户( 或者你碰巧是一台机器, 并且碰巧在阅读这篇文档) , 
只需要指定哪些节点, 以及哪些连线存在于图中.

此外, 你可以添加一些 `提示`, 比如 `这个节点应该在中心附近` 或 `这条边很重要`.
你并不指定节点和边的确切位置, 这是留给图形绘制算法的事情(graph drawing algorithm). 
该算法将你对图形的描述作为输入, 然后决定节点应该放在页面何处.

自然, Graph 是一门(黑色的)艺术. 确切地说, 没有 `完美`的方法来绘制图形.

与纯粹用`TEX`实现的`pgf`和`TikZ`的其他部分不同, 
图形绘制算法太过复杂, 无法直接用`TEX`实现.
与`pgf`和`TikZ`的其他部分不同的是, 图形绘制算法过于复杂, 无法直接在`TEX`中实现.
相反, 编程语言`Lua`被用于 图形绘制库所使用的编程语言--
这种编程语言已被集成到最近的`TEX`版本中. 这意味着:

+ 作为图形绘制引擎的用户, 你可以在你的文档中以通常的方式运行`TEX`, 而不需要调用`外部程序`.
+ 非常容易为`Tikz`实现新的会图算法, 因为可以使用`Lua`, 而不需要`TeX`的编程知识.

## 使用 Graph Drawing 系统

通常, 图形绘制引擎的 `用户` 只需在他们的图片上添加一个`选项`,  即可调用图形绘制算法.
这是一个典型的例子, 其中`layered layout`选项告诉`TikZ `图
应该使用所谓的`layered graph drawing algorithm` 来绘制(`来摊开`)(这些都将在后面解释). )

在所有的例子中, `节点`的位置, 只有当所有的`节点`都被创建, 
并指定了`边`之后才被计算.
例如, 在最后一个例子中, 如果没有选项 `spring electrical layout`, 
所有的节点都会被放在彼此的上面.

## Graph Drawing 系统的层次

尽管下面几节介绍的`图形绘制`系统是作为`pgf`的一部分开发的, 
但它可以独立于`pgf`和`TikZ`使用.
它可以被任意的程序使用, 只要后者可以运行`Lua`. 
为了实现这一点, 图形绘制系统由三层组成.

1. 在 `底部` 我们有`algorithmic layer`. 这一层是用`Lua`写的, 
包含了所有的图形绘制算法. 有趣的是, `选项`也必须在这一层声明,
所以一个算法连同它使用的所有`选项`可以而且必须完全在这一层指定. 
如果你打算实现一个新的图形绘制算法, 你将只对这一层的功能感兴趣.
算法通过一个定义明确的接口与图形绘制系统 `交流`, 
该接口封装在`InterfaceToAlgorithms`类中.

1. 在 `顶部`我们有`显示层`. 这个层实际上不是`图形绘制系统`的一部分. 
相反, 它是一个 `显示` 图形的软件, `TikZ`只是这种软件的一个例子.
另一个例子是一个`图形编辑器`, 它使用`图形绘制系统`来布置图形它的子图. 
还有比如`命令行`工具, 用于绘制`文件`中描述的图形.
最后, 你也可能希望使用图形绘制系统作为子程序来渲染在更大的程序中产生的图形.

由于显示层的不同实现可能是相当异质的, 
所有的显示层必须通过一个特殊的接口与`图形绘制系统`进行通信, 
该接口被封装在类`InterfaceToDisplay`中.
这个类的主要工作是提供一组方法, 用于指定一个图形有某些`节点`和`边`, 
并且为它们设置某些选项.
然而, 这个接口也允许你`查询`所有被算法声明的选项, 包括它们的文档.
这样一来, 编辑器或命令行工具可以列出所有`图形绘制算法`, 
以及它们应该如何配置.

1. `算法层`和`显示层`通过`绑定层`  "绑定" 在一起.
关于待画图形的大部分`bookkeeping`(记录)工作是由`图形绘制系统`完成的, 
与使用的`算法`和`显示层`无关. 但有些事情仍然是针对具体`显示层`的.
例如, 一些算法可能会创建新的`节点`, 而这些`算法`可能需要知道这些节点有多大.
为此, 在`算法`运行期间, `显示层`必须被 `查询`, 
而实现这一`回调`(callback)是`绑定层`的工作.
通常情况下, `绑定层`实现了从`图形绘制系统`到`显示层`的 `后向` 通信.
而`显示层`的接口类只提供了从`显示层`调用的函数, 但它们不会 `talk back`.

所有与`图形绘制`有关的文件, 都位于`generic/pgf`的`graphdrawing`子目录下.

## 28 在TikZ中使用 Graph Drawing

```latex
\usetikzlibrary{graphdrawing} % LATEX and plainTEX
\usetikzlibrary[graphdrawing] % ConTEXt
```

这个包提供了自动绘制图形的功能. 它要求文件使用`LuaTeX`排版. 
这个包应该在`LuaTEX 0.54`或更高版本中使用.

### 选择布局和库

当你加载`graphdrawing`库时, 图形绘制引擎被初始化. 
这个库提供了图形绘制的基本框架, 包括本节中描述的所有`选项`和`键`.
然而这个库并没有加载任何实际的图形绘制算法. 
为此, 你需要使用以下命令, 它是由`graphdrawing`库定义的.

```latex
\usegdlibrary{<list of libraries>}
```

该命令用于加载特殊的图形绘制库(命令名称中的`gd`代表 `graph drawing`).
`<list of libraries>`是一个用`逗号`分隔的列表, 
其中包括用`Lua`编程语言编写的库. (这就是为什么需要一个特殊的命令).

详细来说, 这个命令做了以下工作. 对于`<list of libraries>`中的每一个`<name>`:

1. 检查`LuaTEX`是否可以对库文件`pgf.gd.<name>.library`调用`require`.
`LuaTEX`的文件搜索机制将以通常的方式搜索`texmf-trees`, 
文件名中的`点`被转换成`目录斜线`.
1. 如果上述方法失败, 尝试`require`字符串`pgf.gd.<name>`.
2. 如果失败, 尝试`require`字符串`<name>.library`.
3. 如果失败, 尝试要求字符串`<name>`. 如果这也失败, 打印一条错误信息.

上述情况的纯粹作用如下.
`图形绘制算法`的作者可以将多种算法捆绑在一起,  
通过创建一个 `...xyz/library.lua` 文件, 
该文件内部对所有包含声明的文件调用`require` .

另一方面, 如果一个`图形绘制算法`完全适合存放在一个文件中, 
也可以直接使用 `\usegdlibrary` 读取.

```latex
\usetikzlibrary{graphdrawing}
\usegdlibrary{trees,force}
```

不同的图形绘制库在下面的第`30`至`35`节中有记录.

注意, 除了图形绘制库之外, 你可能还希望加载普通的`TikZ`库`graphs`. 
它提供了强大的`graph` path 命令, 以其易于使用的语法来指定图形.
但你可以独立于`graphs`使用`图形绘制引擎`, 
例如结合`child`或`edge`语法使用. 下面是典型的设置:

```latex
\usetikzlibrary{graphs, graphdrawing}
\usegdlibrary{trees, layered}
```

设置好之后, 你必须指定`图形绘制引擎`使用哪种布局算法, 
应用到哪个`scope`中的节点.
通常情况下, 你只需在`graph`path 操作中添加一个以`... layout`结尾的`选项`即可, 
然后让图形绘制完成它的魔法

```latex
\usetikzlibrary {graphs,graphdrawing} \usegdlibrary {layered}
\tikz [rounded corners]
\graph [layered layout, sibling distance=8mm, level distance=8mm]
{
  a -> {  b,  c -> { d, e }
  } ->  f ->  a};
```

每当你使用这样的布局选项时, 你可以:

+ 以通常的方式创建`节点`. 节点将被完全创建, 
但会被藏在一个内部表格中. 这意味着可以使用所有`TikZ`中对节点的选项.
你还可以为一个节点命名并在以后引用它.
+ 使用`graph`命令的语法(使用`--`, `<-`, `->`或`<->`)创建`edges`, 或者使用`edge`命令, 或使用`child`命令.
然而, 这些边不会立即被创建. 相反, 基础层的命令`\pgfgdedge`将被调用, 
它存储了 `所有关于边缘的信息`.
`edges`的实际绘制只有在所有`节点`都被定位后才会发生.
+ 大多数可以传递给`edge`的`键`都会像预期的那样工作. 
特别是, 你可以使用通常的`node`语法将标签添加到`边上`.
+ `lable`和`pin`选项可以以通常的方式用于`图形绘制 scope`内的节点. 
只是, `lable`和`pin`在节点的定位中不会起任何作用, 它们是在节点最终被定位后加入.
+ 同样, 可以使用通常的`隐式定位语法`, 将`节点`放置在`edge`上.

下面是一些不会工作的东西.

+ 只有使用`graph`语法, `edge`命令或`child`命令创建的`edges`, 
才能正确地将它们的连接信息传递给`基础层`.
当你在`图形绘制scope`内写下 `\draw (a)--(b);`时, 其中 `a` 和 `b` 是在该`scope`内创建的节点, 你会得到一个错误消息/结果会看起来不对.
原因是通常的`--`不会被图形绘制引擎`捕捉到`, 
因此, `--`试图立即连接两个不存在的节点(除了在一些内部表中).

+ `edges`的选项被执行两次: 一次是当`edge`被 `\pgfgdedge` 命令 "检查 "时
(使用一些魔法来防止副作用), 然后在`edge`被实际创建时再执行一次.
幸运的是, 在几乎所有的情况下, 这不会是一个问题;
但是如果你在你的`edge`选项里施展了邪恶的魔法, 你必须祈求佛祖保佑了. 
(不要整烂活, 顺便说一下)

如果你对`细节`感兴趣, 请看第29节.


## 19声明图 Specifying Graphs

`page 269`; 19 Specifying Graphs

在本节中, 我们所说的图是指一组结点和一些边
(有时也称为弧, 取决于连接方式), 如下面的图. 例如:

```latex
\usetikzlibrary {graphs}
\tikz \graph { a -> {b, c} -> d };
```

图的结点是正常的`TikZ`节点, `edge`也是节点之间普通的连线. 
`graphs`库中没有任何东西是你不能用普通的 `\node`和`edge`命令来做的.
它的主要优势是只需指定哪些节点和边是存在的. 
在画布为节点上寻找 "好位置 "的问题留给了`图形绘制算法`.
算法在本手册的第四部分描述, 这些算法不是`graphs`库的一部分;

事实上, 这些算法也可以优化`edge`和`node`命令创建的图形, 而不需要调用`graphs`库.
例如可以 load `layered` 图形绘制库, 
使用`\tikz[layered layout,...`, 然后使用`LuaTeX`编译, 同样可以得到较好的排版.

### 节点链

画`graph`的基本方法是写出一个节点链,

```latex
\usetikzlibrary {graphs}
\tikz [every node/.style = draw]
\graph {   foo -> bar -> blub;a -> b -> c;};
```

节点文字和`->`交替排列, `->`算符用来产生箭头. 
多条链之间用分号`;`或逗号`,`分隔.
节点名称默认等于其中的文字. 可以显式更改这种设定. 
通过`as` key, 或者在节点后面加上slash`/`.

```latex
\usetikzlibrary {graphs}
\tikz \graph {
x1/$x_1$ -> x2 [as=$x_2$, red] -> x34/{$x_3,x_4$};
x1 -> [bend left] x34;
};
```

要使用特殊符号当作节点的名字,如`,`或虚线, 需要用引号`"`包裹. 例如

```latex
\usetikzlibrary {graphs}
\tikz \graph {
"$x_1$" -> "$x_2$"[red] -> "$x_3,x_4$";
"$x_1$" ->[bend left] "$x_3,x_4$";
};
```

### chain 组

可以把多条`chain`放在一个`{}`中, 形成一个 chain group. 
这样可以实现同时连接多个node.
前一个`node`或者`group`的`exit points`
将连接到后一个`node`or `group`的`entry points`.

```latex
\usetikzlibrary {graphs}
\tikz \graph {
a -> { b -> c, d -> e} -> f};
```

树形图可以通过添加`tree layout`来使图形更加美观. 
同时需要在导言区加上

```latex
\usetikzlibrary {graphdrawing}
\usegdlibrary {trees}
```

### Edge标签和风格

可以给`->`connector 提供选项, 来定制风格.

```bash
\usetikzlibrary {graphs}
\tikz \graph {
  a ->[red] b --[thick] {c, d};
};
```

使用`quotes`语法, see Section 17.10.4, 可以方便的添加标签:

```latex
\usetikzlibrary {graphs,quotes}
\tikz \graph {
a ->[red, "foo"] b --[thick, "bar"] {c, d};
};
```

如果想给不同的`edge`指定不同的标签, 
可以去掉给`--`提供的选项, 转而给`node`提供选项.
用`>`来表示进入`node`的`edge`, 
用`<`表示从`node`出射的`edge`.

```latex
\usetikzlibrary {graphs,quotes}
\tikz \graph {
a [< red] -> b -- {c [> blue], d [> "bar"']};
};
```

`"bar"'`后面的单引号`'`表示翻转标签`bar`的位置到下面. 
使用这种语法可以创建带有特殊标签的树图.

```latex
\usetikzlibrary {graphs,quotes}
\tikz
  \graph [edge quotes={fill=white,inner sep=1pt},
    grow down, branch right, nodes={circle,draw}] {
    "" -> h [>"9"] -> { c [>"4"] -> { a [>"2"], e [>"0"] },j [>"7"]}
};
```

### node 集合

当你在`graph`命令中写下`节点`文本时, 默认会创建一个新的节点, 
除非这个节点已在同一个图形命令创建过.
特别是, 如果节点已经在`graph`的范围外声明过, 
则会创建一个同名的新节点. 这并不总是理想的行为.
你可能希望使用已经定义好的节点画新的`graph`, 而不是在`graph`内部重新定义.
为此, 只需在节点名称周围加上圆括号`()`. 
这将导致创建对已经存在的节点的`引用`.

```latex
\usetikzlibrary {graphs}
\tikz {
  \node (a) at (0,0) {A};
  \node (b) at (1,0) {B};
  \node (c) at (2,0) {C};
}
\graph { (a) -> (b) -> (c) };
```

你甚至可以更进一步. 一群节点可以通过添加选项
`set=<node set name>`来标记为属于一个`节点集`.
然后, 在`graph`命令中, 
你可以通过在`节点集`名称的周围加上括号`()`, 来引用这些节点.

```latex
\usetikzlibrary {graphs,shapes.geometric}
\tikz [new set=my nodes] {
\node [set=my nodes, circle, draw] at (1,1) {A};
\node [set=my nodes, rectangle, draw] at (1.5,0) {B};
\node [set=my nodes, diamond, draw] at (1,-1) {C};
\node (d)[star, draw] at (3,0) {D};
\graph { X -> (my nodes) -> (d) };
}
```

### Graph 宏

由于图形中经常存在重复使用的部分, 
为了便于指定这样的图形, 你可以定义`图形宏`.
一旦定义了`图形宏`, 你就可以使用图形的名称来复制它.

```latex
\usetikzlibrary {graphs.standard}
\tikz \graph { subgraph C_n [n=5, clockwise] -> mid };
```

`graphs.standard`库定义了许多这样的图,
包括`n`个节点上的 complete bipartite graph `Kn,m`, 
具有shores sized `n`和`m`, `n`个节点上的循环`Cn`, `n`个节点上的路径`Pn`,
以及`n`个节点上的独立集合`In`.

### Graph表达式和颜色类

当使用`graph`命令构建图时, 它递归构建的, 将较小的图拼成较大的图.
在这个递归的拼合过程中, 图的节点会被`隐含`地着色 (概念上), 
你也可以显式地为单个节点指定颜色, 甚至可以在指定(specify)图形时改变颜色.
所有具有相同颜色的节点形成一个所谓的`颜色类`(color class).

`颜色类`的强大之处在于, 特殊的`连接运算符`(connector operator)
允许你在具有特定颜色的节点之间添加边.
例如, 在组的开头添加`clique=red`会使所有的节点被标记为(概念上)`红色`, 
这些节点将会被连接成一个`clique`.
同样地,  `complete bipartite={red}{green}`
将在所有`red`节点和所有`green`节点之间增加边.

更高级的`connector`, 比如`蝴蝶 connector`, 
允许你以一种花哨的方式在`颜色类`之间添加边.

```latex
\usetikzlibrary {graphs}
\tikz [x=8mm, y=6mm, circle]
\graph [nodes={fill=blue!70}, empty nodes, n=8] {
subgraph I_n [name=A] --[butterfly={level=4}]
subgraph I_n [name=B] --[butterfly={level=2}]
subgraph I_n [name=C] --[butterfly]
subgraph I_n [name=D] --
subgraph I_n [name=E]
};
```
