# svg

[SVG教程](https://developer.mozilla.org/zh-CN/docs/Web/SVG/Tutorial)

## 引言

可缩放矢量图形(Scalable Vector Graphics, `SVG`), 是一种用于描述二维的矢量图形, 基于 `XML` 的标记语言. 
作为一个基于文本的开放网络标准, `SVG`能够优雅而简洁地渲染不同大小的图形, 并和 `CSS`, `DOM`, `JavaScript` 和 `SMIL` 等其他网络标准无缝衔接. 
本质上, `SVG` 相对于图像, 就好比 `HTML` 相对于文本. 

`SVG` 图像及其相关行为被定义于 `XML` 文本文件之中, 这意味着可以对它们进行搜索, 索引, 编写脚本以及压缩. 此外, 这也意味着可以使用任何文本编辑器和绘图软件来创建和编辑它们. 
和传统的点阵图像模式, 像 `JPEG` 和 `PNG` 不同, `SVG`格式提供的是矢量图, 这意味着它的图像能够被无限放大而不失真或降低质量, 并且可以方便地修改内容. 

`SVG` 是由万维网联盟(W3C)自 1999 年开始开发的开放标准. 

本教程旨在解释SVG内部的技术细节. 
如果你希望绘制非常漂亮的图形, 你可以在[Inkscape的文档页面](https://inkscape.org/)上获取更多有用的资源. 
另外还有一个比较好的SVG介绍：[W3C的SVG入门](https://www.w3.org/Graphics/SVG/IG/resources/svgprimer.html). 

`SVG`可以通过定义必要的线和形状来创建一个图形, 也可以修改已有的位图, 或者将这两种方式结合起来创建图形. 
图形和其组成部分可以形变(be transformed), 合成, 或者通过滤镜完全改变外观. 

### 基本要素

HTML提供了定义标题, 段落, 表格等等内容的元素. 与此类似, `SVG` 也提供了一些元素, 用于定义圆形, 矩形, 简单或复杂的曲线. 
一个简单的`SVG`文档由`<svg>`根元素和基本的形状元素构成. 另外还有一个`g`元素, 它用来把若干个基本形状编成一个组. 

从这些开始, `SVG`可以变得更加复杂. `SVG`支持渐变, 旋转, 动画, 滤镜效果, 与`JavaScript`交互等等功能, 
但是所有这些额外的语言特性, 都需要在一个定义好的图形区域内实现. 

正式开始之前, 你需要基本掌握`XML`和其它标记语言比如说`HTML`, 如果你不是很熟悉`XML`, 这里有几个重点一定要记住：

+ `SVG`的元素和属性必须按标准格式书写, 因为`XML`是区分大小写的(这一点和`HTML`不同)
+ `SVG`里的属性值必须用引号引起来, 就算是数值也必须这样做. 

`SVG`是一个庞大的规范, 本教程主要涵盖基础内容. 掌握了这些内容之后, 你就有能力使用[元素参考](https://developer.mozilla.org/en-US/docs/Web/SVG/Element)
和[接口参考](https://developer.mozilla.org/zh-CN/docs/Web/API/Document_Object_Model), 学习其他你需要知道的内容. 

### 贝塞尔曲线 Bezier curve

[贝塞尔曲线](https://developer.mozilla.org/zh-CN/docs/Web/SVG/Tutorial/Paths#bezier_curves)

## 入门

### 简单示例

让我们直接从一个简单的例子开始, 看一下下面代码：

```xml
<svg version="1.1"
     baseProfile="full"
     width="300" height="200"
     xmlns="http://www.w3.org/2000/svg">
  <rect width="100%" height="100%" fill="red" />
  <circle cx="150" cy="100" r="80" fill="green" />
  <text x="150" y="125" font-size="60" text-anchor="middle" fill="white">SVG</text>
</svg>
```

复制并粘贴代码到文件`demo1.svg`. 然后用`Firefox`打开该文件.  它将会呈现出来. 绘制流程包括以下几步：

+ 从`svg`根元素开始：
  + 应舍弃来自 `(X)HTML` 的`doctype`声明, 因为基于`SVG`的`DTD`验证导致的问题比它能解决的问题更多. 
  + `SVG 2`之前, `version`属性和`baseProfile`属性用来供其他类型的验证识别`SVG`的版本. `SVG 2`不推荐使用`version`和`baseProfile`这两个属性. 
  + 作为`XML`的一种方言, `SVG`必须正确的绑定命名空间 (在`xmlns`属性中绑定).  请阅读[命名空间速成页面](https://developer.mozilla.org/zh-CN/docs/Web/SVG/Namespaces_Crash_Course)获取更多信息. 
+ 绘制一个完全覆盖图像区域的矩形 `<rect/>`, 把背景颜色设为红色.
+ 一个半径`80px`的绿色圆圈`<circle/>`绘制在红色矩形的正中央 (向右偏移`150px`, 向下偏移`100px`). 
+ 绘制文字`"SVG"`. 文字被填充为白色,  通过设置居中的锚点把文字定位到期望的位置：在这种情况下, 中心点应该对应于绿色圆圈的中点. 还可以精细调整字体大小和垂直位置, 确保最后的样式是美观的. 

### SVG文件的基本属性

+ 最值得注意的一点是元素的渲染顺序. `SVG`文件全局有效的规则是`后来居上`, 越后面的元素越可见. 
+ `web`上的`svg`文件可以直接在浏览器上展示, 或者通过以下几种方法嵌入到`HTML`文件中：
  + 如果`HTML`是`XHTML`并且声明类型为`application/xhtml+xml`, 可以直接把`SVG`嵌入到`XML`源码中. 
  + 如果`HTML`是`HTML5`并且浏览器支持`HTML5`, 同样可以直接嵌入`SVG`. 然而为了符合`HTML5`标准, 可能需要做一些语法调整. 
  + 可以通过 `object` 元素引用`SVG`文件：

   ```xml
     <object data="image.svg" type="image/svg+xml" />
   ```
 
  + 类似的也可以使用 iframe 元素引用SVG文件：

   ```xml
   <iframe src="image.svg"></iframe>
   ```

  + 理论上同样可以使用 `img` 元素, 但是在低于4.0版本的 `Firefox` 中不起作用. 
  + 最后 `SVG` 可以通过`JavaScript`动态创建并注入到`HTML DOM`中.  这样具有一个优点, 可以对浏览器使用替代技术, 在不能解析`SVG`的情况下, 可以替换创建的内容. 
阅读this dedicated article 以深入了解该话题. 
+ `SVG`如何处理大小和单位将在下一页详解. 

### SVG 文件类型

`SVG`文件有两种形式. 普通`SVG`文件是包含`SVG`标记的简单文本文件. 推荐使用`.svg`(全部小写)作为此类文件的扩展名. 

由于在某些应用(比如地图应用等)中使用时, SVG文件可能会很大, SVG标准同样允许`gzip`压缩的SVG文件. 推荐使用`.svgz`(全部小写)作为此类文件扩展名 . 
不幸的是, 如果服务器是微软的`IIS`服务器, 使`gzip`压缩的`SVG`文件在所有的可用`SVG`的用户代理上可靠地起作用是相当困难的, 
而且`Firefox`不能在本地机器上加载`gzip`压缩的`SVG`文件.  除非知道处理发布内容的`web`服务器可以正确的处理`gzip`, 否则要避免使用`gzip`压缩的`SVG`. 

#### 关于web服务器的小提示

如果你已经知道了如何创建基本SVG文件, 下一步就是把它们上传到web服务器, 但是在这阶段有一些陷阱. 对于普通SVG文件, 服务器应该会发送下面的`HTTP`头：

```xml
Content-Type: image/svg+xml
Vary: Accept-Encoding
```

对于`gzip`压缩的SVG文件, 服务器应该会发送下面的`HTTP`头：

```xml
Content-Type: image/svg+xml
Content-Encoding: gzip
Vary: Accept-Encoding
```

可以利用`Network Monitor panel`或者`web-sniffer.net`之类的网站来检查服务器是否给SVG文件发送正确的HTTP头.
向`web-sniffer.net`提交你的一个`SVG`文件的链接, 然后查看 `HTTP` 响应头. 如果发现服务器没有发送上述的响应头部值, 那么你应该联系你的服务器供应商. 
如果不能说服他们为SVG修正服务器配置, 可能还有一些我们可以自行解决的办法.  
请阅读SVG维基的server configuration page以找到一些简单的解决方案. 

服务器配置错误是SVG加载失败的常见原因, 所以一定要确保你的服务器配置正确. 
如果不能把服务器配置成给SVG文件发送正确的响应头, 这时Firefox很有可能把该文件的标记显示成文本或乱码, 甚至会要求查看者选择打开文件的应用程序. 
