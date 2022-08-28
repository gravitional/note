# node 文字节点

## node 指定

`tikz-feynman`包中的顶点相当于`node`, 
`node`的特点是需要添加文字(可以为空白--`{}`), 也就是类似下面这种. 
在`node`周围会留下空白, 其实是`node`占据的空间.

```latex
\node[above right =0.7 and 4.2 of a1] {text}
```

参考 p229, 17.2.2 Predefined Shapes.
如果不想画出`node`, 只是给坐标分配名称, 例如`(x)`, 
并希望传播子可以直接连接`(x)`.  可以使用`coordinate`.
它的效果类似于使用了`(x.center)`, 不会把路径断开成几段. 
可以完整的连接起来, 或者给包围的区域上色.

类似于

```latex
\coordinate[right =2.2  of a1] (a3); % 泡泡起点
```

这样后面不需要有`{text}`.

p224 基本语法:

```latex
\path ... node <foreach statements>  [<options>] (<name>) at (<coordinate>) : <animation attribute>
={<options>} {<node contents>} ...;
```

各部分规范的顺序.  在`node`和`{<node contents>}`之间的所有内容都是可选的. 
如果有`<foreach>`语句,它必须首先出现,紧接在`node`之后.
除此之外,节点规范的所有其他元素
( `<options>` ,`name`,`coordinate` 和 `animation attribute` )的顺序都是任意的,
实际上,这些元素中的任何一个都可能多次出现
(尽管对于`name`和`coordinate`,这没有意义).
例如

```latex
\vertex (a2) at (0,0){2};
```

***
`at` p158

```latex
\path ... circle[<options>] ...;
/tikz/at=<coordinate>
```

如果在`<options>`内部显式设置了此选项
(或通过`every circle`样式间接设置), 则`<coordinate>`将用作圆的中心而不是当前点.  
在一个封闭范围内给某个值设置`at`无效.

`page 785`; 72 Shape Library: 可以指定`node`的形状, 有预定义的各种形状.
`page 563`; Part V Libraries:  从这里开始是各种库, 有预定义的各种命令.

## node位置的相对指定

`page 240`;

```latex
/tikz/below=<specification>
/tikz/left=<specification>
/tikz/right=<specification>
/tikz/below left=<specification>
/tikz/below right=<specification>
/tikz/above left=<specification>
```

`/tikz/above left=<specification>`类似`above`,但是`<shifting parti>`的指定更加复杂.

1. 当`<shifting part>`形式为`<number or dimension> and <number or dimension>`的时候
(注意中间有个`and`), 先向左移动, 再向右移动
(通常这令人满意, 除非你使用了`x` and `y`选项, 修改了`xy`--坐标系的单位矢量. )

2. 当`<shifting part>`形式为`<number or dimension> `时, 也就是只给出一个参数, 
向对角线方向(135度方向)移动$\frac{1}{2}\sqrt{2}cm$. 
按照数学的说法, 就是按照$l_{2}-norm$理解, 相当于极坐标中的半径. 
而`<number or dimension> and <number or dimension>`是按照$l_{1}-norm$理解.

## node 引用外域node

`page 260`; 17.13 Referencing Nodes Outside the Current Picture

## 引用不同图片中的节点

可以在图片`A`中引用图片`B`中的`node`( 但不是很trivial) .
这意味着你可以创建图片和一个`节点`, 
稍后你可以从其他位置画一条线(`edge`)到这个`节点`.

要引用不同图片中的节点, 请按以下步骤操作.

1. 你需要在所有包含`目标节点`的图片上添加`remember picture`选项, 
同时也要在所有包含`起始节点`的图片上添加`remember picture`选项.
2. 您需要为路径或整个图片添加`overlay`选项, 这些图片包含对`域外节点`的引用.
(这个选项可以关闭`bounding box`的计算) .
3. 你需要使用一个支持`remember picture`的`驱动程序`, 
`pdfLaTeX`是支持的, 并且你需要运行`TeX`两次.

关于幕后操作的更多细节, 见第107.3.2节. 让我们来看看这些选项的效果.

```latex
/tikz/remember picture=<boolean> 无默认, 初始值为 false
```

这个选项告诉`TikZ`, 它应该尝试记住`当前图片`在页面上的位置. 
这种尝试可能会失败, 这取决于使用的是哪种后端驱动程序.
另外, 即使记忆成功, 这个位置可能也只在第二次运行`TeX`程序才能用. 
如果可以记忆的话, 你可以考虑

```latex
\tikzset{every picture/.append style={remember picture}}
```

来使`TikZ`记住所有图片. 这将在`.aux`文件中为每张图片添加一行记录--通常不会很多.

然后, 你就不必担心记忆图片的问题了.

```latex
/tikz/overlay=<boolean> 默认为 true
```

这个选项主要是为了在引用其他图片中的节点时使用, 
但你也可以在其他情况下使用它在其他情况下使用.
这个选项的作用是, 在计算当前图片的`边界框`时, 
不会考虑到当前`scope`范围内的所有对象.

你需要在所有包含`域外节点`引用的路径( 或者至少是路径的所有部分) 指定这个选项.
否则, `TikZ`会试图使当前的图片足够大, 
以涵盖其他图片中的`节点`. 然而, 在`TeX`的第二次运行中, 
这将创建一个更大的图片, 导致图片越来越大.
除非你知道自己在做什么, 
否则我建议在所有包含`域外引用`的图片上指定`overlay`选项.

现在让我们看几个例子. 
这些例子只有在用支持`remember picture`的驱动程序处理文档时才有效.

在当前文本中, 我们放置两张图片, 包含名为`n1`和`n2`的节点,

```latex
\tikz[remember picture] \node[circle,fill=red!50] (n1) {};
\tikz[remember picture] \node[fill=blue!50] (n2) {};
```

为了连接这些节点, 我们使用`overlay`和 `remember picture` 选项创建另一张图片.

```latex
\begin{tikzpicture}[remember picture,overlay]
\draw[->,very thick] (n1) -- (n2);
\end{tikzpicture}
```

注意, 最后一张图片似乎是空的. 
实际上它的大小为零, 并且包含在它边界之外的箭头.
最后一个例子, 我们将另一张图片中的`节点`连接到前两个`节点`.
在这里, 我们只给连线提供了`overlay`选项, 
我们不希望把这条线算作图片的一部分.

```latex
\begin{tikzpicture}[remember picture]
  \node (c) [circle,draw] {Big circle};
  \draw[overlay,->,very thick,red,opacity=.5] (c) to [bend left]  (n1)  (n1) -| (n2);
\end{tikzpicture}
```

## 引用当前页节点--绝对定位

有一个特殊的节点叫做`current page`, 可以用来访问当前页.
它是一个长方形的节点, 其`south west`锚点是页面的`左下角`, 
`north east`锚点是页面的`右上角`.

这个节点在内部是以特殊的方式处理的, 
你可以引用它, 就像它已经被`记忆过`一样.
因此, 通过给图片增加`remember picture`和`overlay`选项, 
你可以在页面上`绝对定位`节点.

第一个例子将文本放在当前页面的左下角.

```latex
\begin{tikzpicture}[remember picture]
  \node[xshift=1cm,yshift=1cm] at (current page.south west)
              [text width=7cm, fill=red!20,rounded corners,above right]
              {
                This is an absolutely positioned text in the lower left corner. No shipout-hackery is used.
              }
\end{tikzpicture}
```

下一个例子在页面中间添加圆圈.

```latex
\begin{tikzpicture}[remember picture]
  \draw [line width=1mm,opacity=.25]
  (current page.center) circle (3cm);
\end{tikzpicture}
```

最后一个例子在页面上方叠加文字( 取决于例子的位置, 也可能出现在下一页).

```latex
\begin{tikzpicture}[remember picture,overlay]
\node [rotate=60,scale=10,text opacity=0.2] at (current page.center) {Example};
\end{tikzpicture}
```

## Late 代码和Late 选项

给节点提供的所有选项只影响本节点自己. 
虽然这在大多数情况下是件好事, 但你有时可能想让选项在 `以后`产生影响.
反过来说, 你有时可能会注意到一些选项只应该在`后期`被添加到节点上. 
于此,可以使用下面这个版本的`node`路径命令:

```latex
\path ...  node also[<late options>](<name>) ... ;
```

请注意, `<name>`是强制性的, 不能在这里给出节点文本. 
另外, 选项和节点标签的顺序必须如上.

节点`<name>`必须已经存在. `<late options>`是在局域`scope`中执行的. 
这些选项中的大多数不会有任何影响, 因为你不能改变节点的外观.
也就是说, 你不能用`late`选项把一个红色节点变成绿色节点.
然而, 在`<late options>`选项里面给出`append after command`
和`prefix after command`选项 (直接或间接地)确实能达到预期的效果.
给定的路径被执行, `\tikzlastnode` 被设置为 determined node.

所有这些的净效果是, 
例如,  你可以提供`label`选项, 为已被创建的节点添加标签:

```latex
\begin{tikzpicture}
\node [draw,circle] (a) {Hello};
\node also [label=above:world] (a);
\end{tikzpicture}
```

正如 Section 14 所解释的, 
你可以使用选项`append after command`和`prefix after command`在节点后添加路径. 
下面的宏可能很有用.

+ `\tikzlastnode`; 展开为路径上的最后一个节点. 
你也可以用下面的`选项`来代替`node also`语法.

```latex
/tikz/late options=<options> (无默认值)
```

这个选项可以在`路径`上给出( 但不能作为`节点`命令的参数) , 
其效果与`node alos`命令相同.
在`<options>`中, 你应该使用`name`选项来指定你希望添加`late option`的节点

```latex
\begin{tikzpicture}
\node [draw,circle] (a) {Hello};
\path [late options={name=a, label=above:world}];
\end{tikzpicture}
```
