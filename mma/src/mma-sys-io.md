# 系统io,文件操作

guide/FileOperations, 比较全的文件系统操作的函数列表
tutorial/FilesStreamsAndExternalOperations#12068

不要直接使用字符串函数操作文件名/文件路径, 这样的到的路径依赖于操作系统, 应该使用 Mathematica 提供的文件系统接口.

+ `$OperatingSystem`; 给出正在运行的操作系统的名称.
+ `$PathnameSeparator` ; 字符串,在构建路径名的时候使用.
`Windows`的默认值时`"\\"`, 其他系统是`"/"`. 在`Windows`中, 像`FileNameSplit`这样的函数默认同时允许`\` 和 `/`.

+ 文件名使用惯例.
    + `name.m`  ; Wolfram 语言源文件
    + `name.nb` ;  Wolfram 系统笔记本文件
    + `name.ma` ;  Wolfram 系统从第3版以前的笔记本文件
    + `name.mx` ;  输出所有 Wolfram 语言表达式
    + `name.exe`;   WSTP 可执行程序
    + `name.tm` ;  WSTP 模版文件
    + `name.ml` ;  WSTP 流文件

+ `$Path`; 默认的目录列表, 用于搜索输入文件的相关目录. 一般来说, 全局变量 `$Path` 被定义为一个字符串的列表, 每个字符串代表一个目录.
每次你要求打开文件时, `Wolfram` 就暂时将这些目录中的依次变成你的当前工作目录, 然后从该目录中尝试找到你要求的文件.

在`$Path`的典型设置中, 当前目录`.`和你的主目录`~`被列在第一位.

+ `DirectoryName["name",n]` ;  给出路径的父目录, `n`代表上升`n`次. 默认情形给出父目录, 不用写`n`. 作用于文件和目录, 不检查目录是否真实存在.
可以用`DirectoryName[...,OperatingSystem->"os"]`给出某种操作系统风格的路径, 选项有 `"Windows"`, `"MacOSX"`, 和 `"Unix"`.

+ `ParentDirectory["dir",n]` ;  给出路径的父目录, `n`代表上升`n`此, 只能作用于目录, 且要求目录真实存在.

+ `$InitialDirectory` ;  是 `Wolfram` 系统启动时的初始目录.
+ `$HomeDirectory` ;  你的主目录, 如果被定义过的话
+ `$BaseDirectory` ;  是 `Wolfram` 系统要加载的全系统文件的基本目录.
+ `$UserBaseDirectory` ;  用于 `Wolfram` 系统加载的用户特定文件的基本目录
+ `$InstallationDirectory` ;  你的 `Wolfram` 系统安装的最高级别目录

`Wolfram` 系统所使用的绝大多数文件都与`操作系统`无关. 然而, `.mx` 和 `.exe` 文件与系统有关.
对于这些文件, 按照惯例, 对不同计算机系统版本的名称进行捆绑, 形式如 `name/$SystemID/name`.

## 笔记本

+ `NotebookFileName[]` ; 给出当前笔记本的完整路径.
+ `NotebookDirectory[]`; 笔记本父目录

+ `NotebookOpen["name"]`;  打开已经存在的笔记本`"name"`, 返回笔记本对象. `"name"`可以是绝对路径.
+ `NotebookOpen["name",options]`; 使用指定的选项打开笔记本.
    + `NotebookOpen[File["path"]]`和`NotebookOpen[URL["url"]]`也被支持.
    + `NotebookOpen`通常会导致一个新的笔记本窗口在你的屏幕上被打开.
    + 如果`NotebookOpen`打开指定的文件失败, 则返回`$Failed`.
    + 若给出相对路径, `NotebookOpen`搜索由前端的全局选项`NotebookPath`指定的目录
    + 若使用选项 `Visible->False` 设置, `NotebookOpen` 将打开带有此选项的笔记本,它永远不会显示在屏幕上.
    + `NotebookOpen` 将当前`selection`初始化设置在笔记本的第一行单元之前.

+ `NotebookSave[notebook]`; 保存特定笔记本的当前版本.
    + `notebook`必须是一个`NotebookObject`.
    + `NotebookSave[notebook]` 将笔记本保存在一个文件中, 文件名由笔记本对象 `notebook` 给出.
    + `NotebookSave` 写入对应的 `Wolfram` 语言表达式, 以及 Wolfram 语言注释, 以便于前端再次读入笔记本.
    + `NotebookSave[notebook, "file"]`, 如果`"file"`存在, 则不加警告地覆盖它.
    + `NotebookSave[notebook,File["file"]]`也被支持.
    + 如果给定选项 `Interactive->True`, 前端将提示用户为笔记本选择一个文件名.

+ `NotebookClose[notebook]`; 关闭指定的笔记本对象.
+ `NotebookClose[] `; 关闭当前在运行的笔记本.
    + `NotebookClose`将使笔记本从你的屏幕上消失, 并将使所有引用该笔记本的笔记本对象失效.
    + 如果给定了选项设置`Interactive->True`, 前端将提示用户是否关闭笔记本而不保存.

## 操作文件和目录

tutorial/FilesStreamsAndExternalOperations#12068
Manipulating Files and Directories

+ `ExpandFileName["name"] `; 将`"name"`文件展开成当前系统规范的绝对路径, 给出相对于你当前目录的名称.
+ 它展开通常的目录指定, 如`.`和 `..`.
+ 它只是对文件名进行操作;它并不实际搜索指定的文件.
+ 它支持 `ExpandFileName[File["name"]]`, 以及`ExpandFileName[URL["file:///path"]]`, 后者将基于文件的`URL`转换为绝对文件名.

+ `AbsoluteFileName["name"]`; 给出`"name"`文件的绝对路径. 与`ExpandFileName`的区别是, 它会进入文件系统, 检查文件是否真实存在.
+ 同样相对于你当前目录的名称, 可以处理目录指定, 如`.`, `..`和 `~`.
+ 它也支持 `AbsoluteFileName[File["name"]]`.

+ `FileNameTake["name"]` ; 从`"name"`的完整路径中提取出最后的文件名.
+ `FileBaseName["file"]`; 给出文件的 basename, 也就是不包括拓展名.
+ `FileExtension["file"] ` ; 给出文件的拓展名.
+ `FileNameDepth["name"] `; 给出文件路径的深度, 文件不必真实存在.

+ `FileNameJoin` ; 从路径列表中组合出完整的文件名
+ `FileNameSplit` ; 将文件的完整路径分割开
+ `FileNameDrop["name",n] `; 去掉文件`"name"`路径的前`n`个片段. 如果是`-n`, 那么去掉从末尾开始的`n`个.
+ `FileExistsQ["name"] ` ; 检查文件, 目录等等是否存在.
+ `ContextToFileName["context"] ` ; 给出 Mathematica 上下文规范对应的文件名.

### 目录操作

+ `DirectoryQ` ; 测试名称是否对应于实际的目录
+ `ParentDirectory["dir"]` ; 给出父目录
+ `DirectoryName` ; 从一个完整的文件路径中挑选出目录部分

### 寻找特定文件

+ `FindFile[name] `; 找到指定名称的文件, `Get[name]` 和相关函数使用此函数寻找文件.

+ `FindFile[name]` 在 `$Path` 给出的目录中依次`name`, 返回文件的绝对路径.
+ 如果 `FindFile` 无法找到具有指定名称的文件, 它将返回 `$Failed`.
+ 在 `FindFile[name]`中, 名称可以用 `/` 路径分隔符来指定 (或者在 Windows 中用 `\\`). 它也可以用`` ` ``上下文分隔符来指定.
+ 对于`` name` ``这样的名字, `FindFile` 会搜索 `name.mx` 和 `name.m` 的文件.
+ 如果 `FindFile` 解析到一个目录, 它将在该目录中搜索名为 `Kernel/init.m` 或 `init.m` 的文件.
+ 如果`FindFile`解析为 `name.mx` 形式的对象, 该对象对应于一个目录, 它将搜索一个名称为`name.mx/$SystemID/name.mx`的文件.
+ `Get`, `Needs`, `OpenRead`, `Install` 和其他函数使用 `FindFile` 来寻找要读取的文件.
+ `FindFile[File["file"]]`的语法也支持.

### 列出文件

+ `FileNames[]`; 列出当前目录中的所有文件

+ 文件名可以是字面字符串,` StringExpression`字符串模式, `RegularExpression`对象, 或`缩略字符串`.
+ 在缩略字符串模式中, `*`代表零个或多个字符的序列. `@`代表非大写字母的一个或多个字符的序列.
+ `FileNames[All]`, `FileNames["*"]`或`FileNames[__]`等同于`FileNames[]`.
+ 在缩略字符串模式中, `Verbatim["s"]`指定字符串 `"s "` 应与`*`和`@`进行字面匹配, 而不是当作通配符.
+ `FileNames[form,dirs,Infinity]` 在`dirs`的所有子目录中寻找文件.
+ `FileNames` 返回的文件列表按照函数 `Sort` 产生的顺序进行排序.
+ `FileNames[forms,dirs,{n}]`只包括正好在第`n`层出现的目录.
+ 除了文件名之外, 匹配模式还可以使用`相对`或`绝对`目录规范.
+ 设置选项`IgnoreCase>True`使`FileNames`忽略文件名大小写的区别.
+ 在默认设置`IgnoreCase>Automatic`下, `FileNames`在`Windows`系统下忽略文件名大小写, 其他系统不忽略.
+ `File["dir"]`可以用来指定一个要搜索的字面目录名.

### 工作目录设置

+ `SetDirectory["dir"]`; 将当前工作目录设置为 `dir`.
    + `SetDirectory`设置当前工作目录, 然后返回其全名.
    + `SetDirectory` 将当前工作目录压入`DirectoryStack[]`给出的目录栈中.
    + 如果给出相对路径, `SetDirectory` 设置相对于当前工作目录的目录.
    + `SetDirectory[]`等同于`SetDirectory[$HomeDirectory]`.
    + `SetDirectory[File["dir"]]`也被支持.

+ `ResetDirectory[]`; 将当前工作目录重置为之前的值.
    + 对`ResetDirectory`的连续调用会恢复越来越早的工作目录.
    + `ResetDirectory`使用`DirectoryStack[]`给出的目录栈.
    + `ResetDirectory`从目录栈中删除最后一个元素, 并使倒数第二的元素成为当前元素.

+ `DirectoryStack[]`; 给出当前使用的目录序列/目录栈. 其中的目录用绝对路径给出.
每次调用`SetDirectory`会在目录栈中压入一个元素;每次调用`ResetDirectory`会弹出一个元素.

## 读取文本数据

Reading Textual Data

### 文件导出导入

tutorial/ImportingAndExporting
tutorial/FilesStreamsAndExternalOperations

管理文件系统的函数非常多. 常用的是:

+ `Put,Get`: 处理 `Mathemtica` 语言输入的 `.wl`, `.m` 格式.
    + `Put[exp1,exp2,..."filename"]`; 把一系列表达式`expi`写入到`"filename"`文件中.
    + `Put`在创建文件时默认使用字符编码 `PrintableASCII`.添加选项`CharacterEncoding->编码`来指定一个不同的编码.

+ `Export,Import`: 处理非常多的外部格式, 具有大量选项. 默认编码是`ASCII`.
    + `Export["dest.ext",expr]`; 将数据导出到`dest.ext`文件中, 根据文件的后缀名转换成相应的格式.
    + `Export[dest,expr,"format"]`; 显式指定保存的格式.

+ `ReadString, WriteString` ; 将表达式转换成字符串, 再写入文件. 或者将文件以字符串的形式读入.
+ `ImportString`, `ExportString`; 从字符串中读取数据, 或者将表达式转换成数据.
+ `BinaryRead,BinaryRead`; 读写二进制数据

### 字符串导出导入

常用函数

+ `Import`
+ `Export`
+ `ImportString`
+ `ExportString`

选项 默认值 含义

+ `"TextDelimiters"` ; 字符串或者字符串列表, 给非数字(一般是字符串)分界
+ `"FieldSeparators"`, `{" ","\t"}`;给columns分界的字符串
+ `"LineSeparators"`, `{"\r\n","\n","\r"}` ;  给rows分界的字符串
+ `"Numeric"`, `True`; 如果可能的话, 是否把数据导入成数字

例子

```mathematica
ExportString[{1, "text", 2, 3},
 "Table",
 "TextDelimiters" -> {"<", ">"},
 "LineSeparators" -> "\n",
 "FieldSeparators" -> " "
 ]
 ```

`wolframscripts` 结合`shell` 使用时, 传递参数最好用字符串, 不会改变结构.
在 `mma` 脚本内部, 使用 `ToString` 和 `ToExpression` 进行转化, 为了保险, 可以增加`InputForm`选项.

***
命令行输出的时候, 可以用

```
ExportString[RandomReal[10, {4, 3}], "Table"]
```

还有`"List"`格式
