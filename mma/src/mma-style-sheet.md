# 使用样式表

tutorial/WorkingWithStylesheets

根据笔记本界面所提供的任意或全部可用选项, Wolfram 系统使用样式表控制笔记本的行为和外观.
样式表是笔记本的各种特殊单元的集合, 它被其他笔记本引用, 或者作为笔记本选项的一部分应用. 在后一种情形中, 我们称样式表作为私有或本地样式表在笔记本内部"嵌入".

***
与样式表相关的函数.

+ `StyleDefinitions` 笔记本选项, 指定具有样式定义的文件能够在笔记本中使用
+ `StyleData["style"]` 样式定义单元的内容的低层表示

样式表是包含 `StyleData` 单元的笔记本. 一个简单的样式表笔记本表达式可以像下面所展示的这个例子这么简单.

```mathematica
Notebook[{
 Cell[StyleData["Section"],
   FontColor->Gray],
 Cell[StyleData["Subsection"],
   FontFamily->"Helvetica"]
 }]
```

上例仅定义了两种样式:  `Section` 和 `Subsection`. 两者的定义仅使用了一个选项.
在没有更多信息的情况下, `Section` 和 `Subsection` 单元在工作笔记本中没有任何不同. 它们的大小相同(正常文本), 并具有相同的边幅, 一个将是灰色的, 另一个将是黑色的, 并且它们的字体也将不同.

## 继承

继承是一个与级联样式表相关的概念--从不同层次的引用所获得的格式设置.
样式表中的第一个单元通常引用已有的样式表, 如 Default.nb, 点击它可以看到它引用的是 Core.nb.

该累积效应堆栈的最终是工作文档的实际单元. 如果它应用了一个选项, 如 `FontColor->Red`, 该选项将覆盖整个继承堆栈的 `FontColor` 的其他设置.

+ `$FrontEnd`  笔记本界面中所有选项的默认设置
+ `Core.nb`  定义低层单元外观和行为的基础样式表
+ `Default.nb` 可供用户选择的样式表, 对于新文档通常是 Default.nb
+ `个人样式表 (如果存在)`  编辑文档样式表的结果
+ `样式表中 "Notebook "的局部样式定义 `  应用于笔记本层次的选项的样式表的特殊样式定义
+ `样式表中的样式环境设置`  下节中讨论的环境
+ `笔记本层次的选项设置`  应用于笔记本层次的设置
+ `单元层次的选项设置`  应用于单元层次的设置

## 环境

`StyleData["style","environment"]` 表示在样式环境 "environment" 中样式定义单元的内容

`StyleData` 的第二个参数通常是设置所应用的环境名称.

样式环境提供了一种无需更换样式表即可切换文档设置的方法.
典型的环境包括 `SlideShow` 和 `Printout`. 许多专门的行为依赖于文档的环境设置.
例如, 幻灯片放映能够工作是由于分页设置, 而分页设置在其他情况下与正常工作环境无关.
相似地, 打印操作也受这些相同的分页符的影响, 但 `Printout` 环境将待打印页面中所有内容按比例缩放至一个更合适的尺寸. 字体也被设置为打印所需的较高分辨率.

显示屏和打印环境的设置可以不同, 方法是使用两种不同的菜单: `格式--显示屏环境` 和 `文件--打印设置--打印环境`.
在默认情况下, 文档界面将显示屏环境设置为 `Working` , 将打印环境设置为 `Printout`.
因此, 一定要注意这不是一个"所见即所得"的配置.