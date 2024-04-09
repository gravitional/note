# Visual studio

## 自定义快捷键

[default-keyboard-shortcuts](https://learn.microsoft.com/zh-cn/visualstudio/ide/default-keyboard-shortcuts-in-visual-studio)
[快速切换 头/源文件 (.h/.cpp)](https://blog.csdn.net/woloveguojia/article/details/122364839)

菜单栏>`工具`>`选项`>`环境`>`键盘`,
在下面 "显示命令包含" 里面输入关键字 : `编辑器上下文菜单.代码窗口.切换标题代码文件`
最后在 `按快捷键` 中输入按下自己的组合键,
可以选择配置 `全局属性`, 或者限定快捷键的生效范围.

## 常用快捷键

`Ctrl+k, ctrl+o`; `.h`/`.cpp` 切换
`Ctrl+Q`; general 查找
`Ctrl+,`; 查找符号，文件等等
`Ctrl+Shift+T`; 打开文件列表

### 编辑

[代码编辑器功能](https://docs.microsoft.com/zh-cn/visualstudio/ide/writing-code-in-the-code-and-text-editor?view=vs-2022)

`Ctrl+F4`: 关闭当前窗口.
`Ctrl+Alt+F12`: 查找符号结果.
`Ctrl+.`/`Alt+Enter`; 快速操作
`Ctrl+D`; 复制行
`Ctrl+L`; 删除行
`Alt+Shift+]`; 选中块结构
`Shift+F10`; 展开上下文菜单(光翼展开doge)

### 视图

`ctrl+shift+,/.`; 放大缩小编辑器字体

### 解决方案资源管理器

`Ctrl+[,s`; 与活动文档同步

### VS IntelliSense

`C+J`; 列出成员
`C+S+Space`; 参数信息
`C+K,C+I`; 快速信息(手动控制弹出信息框)
`C+Space`; 完成单词
`C+k, C+S`; 外侧代码
`C+K,C+X`; 插入片段

### 生成;Build

+ `Ctrl+B`; 生成当前项目
+ `Ctrl+Shift+B`; 生成解决方案

### 调试

+ `Alt+*(小键盘)`; 将编辑器窗口切换到运行位置.
+ `F7`; 切换到代码窗口.

+ `C+A+C`: 切换到调用堆栈(Call Stack)工具窗口
+ `C+A+B`: 断点窗口(BreakPoint)
+ `C+A+V,A`: 自动变量窗口
+ `C+A+V,L`: 局域变量窗口
+ `C+A+L`:解决方案资源管理器
+ `C+S+E`: 资源视图
+ `C+A+W`: 监视1
+ `C+A+K`: 调用层次窗口
+ `` c+` ``: 终端

+ `Ctrl+F9`: 禁用断点
+ `C+S+F9`; 禁用所有断点

+ `C+A+F2`; 显示诊断工具, 查看内存,CPU占用,以及运行时间

### 书签

+ `C+k, C+w`; 书签窗口
+ `C+k,C+k`: 切换书签 on/off
+ `c+k,c+n`: 下一个书签
+ `c+k,c+p`: 上一个书签
+ `c+k,c+L`: 清除所有书签
+ `c+k,c+H`: 切换任务列表, add/remove

### 注释

+ `c+k,c+/`: 行注释
+ `c+s+/`: 块注释
+ `S+A+L,S+A+J`: 联接行
+ `c+k,c+\`; 删除水平空白

+ `c+A+o`: 输出

## notes

+ `INFO()` 函数, 在 `logger.h` 中定义打印信息.
+ 类和函数为大驼峰.

+ `compElement.h` 中有较多的注释.

## XML注释

[注释 (C++)](https://docs.microsoft.com/zh-cn/cpp/cpp/comments-cpp?view=msvc-170)
[Visual Studio可以自动生成注释](https://cloud.tencent.com/developer/news/601818)
[为文档生成项插入 XML 注释](https://docs.microsoft.com/zh-cn/visualstudio/ide/reference/generate-xml-documentation-comments?view=vs-2022)
[XML 文档注释](https://docs.microsoft.com/zh-cn/dotnet/csharp/language-reference/xmldoc/)

如果你正在使用 `Doxygen` 或者 `XML` 文档注释,
那么Visual Studio v16.6 Preview 2 了一项称之为 `注释自动生成`(automatic comment stub generation)的新特性,
这项特性将和现有的快速参考(QuickInfo),
参数帮助(Parameter Help)和成员列表提示(Member List tooltip)等特性一起,
为开发者提供流畅的编码体验.

### 自动生成注释的基本骨架

在XML文档注释中, 有一些每次都必须要输入的关键字, 例如 `summary`, `param` 等.
Visual Studio可以通过一种十分自然的方式来自动生成注释骨架,
开发者只需要在注释骨架上填写相应的内容即可.

在默认情况下, 注释自动生成只会在 `XML` 文档注释上起作用.
开发者在为一个函数编写注释的时候,
可以通过输入三个斜杠(`///`)或者使用快捷键(`Ctrl + /`)来产生注释骨架.

![有图有真相](https://ask.qcloudimg.com/http-save/developer-news/g12ddmld1g.gif)

如果我使用 `Doxygen` 呢?

如果你不使用 `XML注释`, 而是 `Doxygen`, 那么也不用太担心,
Visual Studio已经为你安排的明明白白了.
可以在 `Visual Studio` 的设置里自由切换你喜欢的注释系统类型,
具体的设置路径: `[工具/选项/文本编辑器/C/C++/编码风格/常规]`.
当然, 你也可以直接通过快捷键(`Ctrl + Q`)来唤醒搜索框, 并输入关键字 `Doxygen` 来快速定位.
设置界面如下图所示:

![img](https://ask.qcloudimg.com/http-save/developer-news/9rhcryy5yy.jpeg?imageView2/2/w/1620)

你可以在上面的设置界面选项触发自动生成注释的代码,
比如可以选择三个斜杠(`///`)或者 `/**`, 这完全跟随你的喜好来定.
当然, 快捷键(`Ctrl+/`)也会一直存在, 如果你是一个快捷键达人, 那么你应该挺喜欢这种方式.

同时, 还可以通过修改 `[.editorconfig]` 配置文件,
来在文件夹级别或者文件级别上来指定注释系统的各项配置参数,
下图是可以添加到 `[.editorconfig]` 的配置项:

![img](https://ask.qcloudimg.com/http-save/developer-news/vjjqnekc6j.jpeg?imageView2/2/w/1620)

为了得到一个 `[.editorconfig]` 配置文件, 你可以在上面的设置界面,
点击按钮 `[Generate .editorconfig file from settings]`,
那么Visual Studio将会基于现有的配置来生成一个新的配置文件,
然后你就可以自由的修改这个配置文件了.
这种方式的好处是, 你可以为不同的文件夹或者文件设置不一样的注释风格,
这非常适合于不同开发团队之间的代码风格兼容.

+ 自动生成Doxygen注释

![img](https://ask.qcloudimg.com/http-save/developer-news/zvxq13zp9n.gif)

写注释的一个好处
有些人可能会问, 为什么要辛辛苦苦的写那些没用的注释呢?请先看看下图:

![img](https://ask.qcloudimg.com/http-save/developer-news/liui6nu6zl.gif)

有感觉了吗?你编写的XML或者Doxygen注释, Visual Studio已经可以自动识别到了,
并且在你调用某个函数的时自动给你信息提示. 这项特性, 非常适合那些写完一个函数就忘记怎么调用的人, 比如我.

## 任务列表

[使用任务列表](https://docs.microsoft.com/zh-cn/visualstudio/ide/using-the-task-list?view=vs-2022)

### 使用任务列表

使用 `任务列表`  跟踪使用 TODO 和 HACK 或自定义令牌等令牌的代码注释,
还能管理直接导向代码中的预定义位置的快捷方式.  单击列表中的项以转到其在源代码中的位置.

### 任务列表窗口

当 `任务列表` 打开后, 它将显示在应用程序窗口的底部.

若要打开 任务列表, 请选择 `查看>任务列表`, 或从键盘按 `Ctrl+\, T`.

![Task List window](https://docs.microsoft.com/zh-cn/visualstudio/ide/media/vs2015_task_list.png?view=vs-2022)

要更改列表的排序顺序, 请选择任意列的标头.
若要进一步优化搜索结果, 请按住 Shift 键单击另一个列标头.
另一种方法是, 在快捷菜单上选择"排序方式" , 然后选择一个标头.
若要进一步优化搜索结果, 请按住 Shift 并选择另一个标头.

要显示或隐藏列, 在快捷菜单上选择 `显示列` .  选择要显示或隐藏的列.
要更改列的顺序, 请将任意列标头拖动到所需的位置.

### 自定义令牌

默认情况下, Visual Studio 包含以下令牌:

    HACK, TODO, UNDONE 和 UnresolvedMergeConflict.

令牌不区分大小写.  你也可以创建自己的自定义令牌.
创建自定义令牌:

+ 在 "工具" 菜单上, 选择 "选项" .
+ 打开 "环境" 文件夹, 然后选择 "任务列表".
+ 将显示"任务列表"选项页.
    ![Visual Studio Task List](https://docs.microsoft.com/zh-cn/visualstudio/ide/media/vs2015_task_list_options.png?view=vs-2022)

+ 在"名称" 文本框中, 输入令牌名称, 如"BUG".
+ 在 "优先级别" 下拉列表中, 为新令牌选择默认优先级别.
+ 选择"添加".

>提示
>输入名称后将启用"添加"按钮.  必须先输入名称, 然后再单击"添加".

## 查看汇编代码

[在 Visual Studio 调试器中查看反汇编代码](https://learn.microsoft.com/zh-cn/visualstudio/debugger/how-to-use-the-disassembly-window?view=vs-2022)

若要启用 `反汇编` 窗口, 请在 `工具` > `选项` > `调试` 下, 选择 `启用地址级调试`.

若要在调试期间打开 `反汇编` 窗口, 请选择 `窗口`>`反汇编`或按 `Alt+8`.

若要打开或关闭可选信息, 请在"反汇编"窗口中单击右键, 然后在快捷菜单中设置或清除所需的选项.

左边距中的黄色箭头表示当前执行点.
对于本机代码, 该执行点对应于 CPU 的 `程序计数器`.
该位置显示程序中将要执行的下一条指令.

## utf-8 选项

[/utf-8(将源字符集和执行字符集设置为 UTF-8)](https://learn.microsoft.com/zh-cn/cpp/build/reference/utf-8-set-source-and-executable-character-sets-to-utf-8)

语法
`/utf-8`

### 备注

可以使用 `/utf-8` 选项将源字符集和执行字符集指定为使用 UTF-8 编码的字符集.
它等效于在命令行上指定 `/source-charset:utf-8 /execution-charset:utf-8`.
任意这些选项还会默认启用 `/validate-charset` 选项.  有关受支持的代码页标识符和字符集名称的列表, 请参阅代码页标识符.

默认情况下, Visual Studio 会检测字节顺序标记,
以确定源文件是否采用编码的 Unicode 格式, 例如 `UTF-16` 或 `UTF-8`.
如果未找到字节顺序标记,
则除非已使用 `/utf-8` 或 /source-charset 选项指定代码页, 否则假定在当前用户代码页中对源文件进行编码.
Visual Studio 允许采用多种字符编码中的任意一种保存 C++ 源代码.
有关源字符集和执行字符集的信息, 请参阅语言文档中的字符集.

在 Visual Studio 中或以编程方式设置此选项
在 Visual Studio 开发环境中设置此编译器选项
打开项目"属性页" 对话框.  有关详细信息, 请参阅在 Visual Studio 中设置 C++ 编译器和生成属性.

选择 `配置属性` > `C/C++` > `命令行`属性页.

在"附加选项"中, 添加 `/utf-8` 选项以指定首选编码.

选择"确定"以保存更改 .
