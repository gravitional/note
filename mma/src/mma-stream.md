# 流和文件

tutorial/FilesAndStreams

+ 流的类型.
    + `InputStream["name",n]`  从一个文件或者管道的输入
    + `OutputStream["name",n]`  一个文件或管道的输出
+ 输出流的选项.

| 选项名 | 默认值 | explanation |
| ----- | ----- | -----|
| `CharacterEncoding` | `Automatic` | 用于特殊字符的编码|
| `BinaryFormat` | `False` | 是否把文件以二进制格式处理|
| `FormatType` | `InputForm` | 表达式的默认格式|
| `PageWidth` | `78` | 每一行的字符数目|
| `TotalWidth` | `Infinity` | 单个表达式中的最大字符数目|

使用 `Options` 用户可以测试流的选项, 并且使用 `SetOptions` 重设.

## 流和底层的输入输出

tutorial/StreamsAndLowLevelInputAndOutput

文件和管道两者都是一般称为 `流` 的 Wolfram 语言对象的例子.
Wolfram 系统中的流是一个输入或输出源. 许多运算都是在流上进行的.

可以将 `>>` 和 `<<` 作为"高层" Wolfram 语言输入输出函数.
它们的基础是直接在流上工作的**底层输入-输出基元**.
用这些基元, 可以更多地控制 Wolfram 语言的输入和输出.
在编写从文件或管道存取中间数据的 Wolfram 语言程序时, 需要进行精确的输入和输出控制.

把输出写入一个 Wolfram 语言流的底层步骤如下.

+ 首先, 调用 `OpenWrite` 或 `OpenAppend` 去打开这个流, 这告诉Wolfram 语言需要向一个**文件**或**外部程序**写入输出以及输出的形式.
+ 打开一个流之后, 调用 `Write` 或 `WriteString` 向这个流写入一个表达式或字符串序列.
+ 完成后, 用 `Close` 去关闭这个流.

***
Wolfram 语言中的流.

+ `"name"`  用名称指定的文件
+ `"!name"`  用名称指定的命令
+ `InputStream["name",n]`  输入流
+ `OutputStream["name",n]`  输出流

打开一个文件或管道时, Wolfram 系统产生一个"流对象"去指出打开的流与该文件或管道相关.
这个流对象一般包含该 `文件名` 或 管道中 `外部命令名` 和 一个**唯一的数**.

在流对象中使用一个唯一数的原因是可能同时会有几个流与同一个文件或外部程序相联系.
例如, 可以在不同的地方使用同一个外部程序, 每一次都与不同的流相关联.

然而, 打开一个流后, 当仅有一个对象和这个流相关联时,
仍可以用一个简单的文件名或外部程序名取指代这个流.

打开一个输出到文件 tmp 的流:

```mathematica
In[1]:= stmp = OpenWrite["tmp"]
Out[1]= OutputStream["tmp", 5321]
```

向该文件写入一个表达式序列:

```mathematica
In[2]:= Write[stmp, a, b, c]
```

由于仅有一个流与文件 `tmp` 相关联, 所以能简单地用文件名去指代它:

```mathematica
In[3]:= Write["tmp", x]
```

关闭这个流:

```mathematica
In[4]:= Close[stmp]
Out[4]= "tmp"
```

这是向该文件写入的内容:

```mathematica
In[5]:= FilePrint["tmp"]
During evaluation of In[7]:=
abc
x
```

***
底层输出函数.

+ `OpenWrite["file"]`  打开一个输出到文件的流, 清除该文件以前的内容
+ `OpenWrite[]`  打开一个输出到新的临时文件的流
+ `OpenAppend["file"]`  打开一个输出到文件的流, 向已有内容追加数据
+ `OpenWrite["!command"]`  打开一个输出到外部命令的流
+ `Write[stream,exp1,exp2,...]`  将表达式序列写到一个流, 用一个换行结束输出
+ `WriteString[stream,str1,str2,...]`  将字符串序列写到一个流, 没有额外换行
+ `Close[stream]`  告诉 Wolfram 语言该流已经完成

调用 `Write[stream,expr]` 时, 将一个表达式写入一个指定的流.
其默认情形是用 Wolfram 语言输入形式写入该表达式.
当调用 `Write` 写入一个表达式序列时, 这些表达式就一个接一个地写入一个流中.
一般地, 在相继表达式中无空格.
然而, 在写完了所有表达式之后, Write 总用一个换行结束输出.

重新打开文件 `tmp` :

```mathematica
In[6]:= stmp = OpenWrite["tmp"]
Out[6]= OutputStream["tmp", 5322]
```

向该文件写入一个表达式序列, 然后关闭:

```mathematica
In[7]:= Write[stmp, a^2, 1 + b^2]; Write[stmp, c^3]; Close[stmp]
Out[7]= "tmp"
```

所有表达式用输入形式写入, 同一 `Write` 给出的表达式放在同一行:

```mathematica
In[8]:= FilePrint["tmp"]
During evaluation of In[10]:=
a^21 + b^2
c^3
```

`Write` 提供了写出完整 Wolfram 语言表达式的途径. 有时需要写出较少结构化的数据.
`WriteString` 用来写出任意字符串. 与 `Write` `不同, WriteString` 不换行, 也不加任何字符.

打开一个流:

```mathematica
In[9]:= stmp = OpenWrite["tmp"]
Out[9]= OutputStream["tmp", 5323]
```

向该流写入2个字符串:

```mathematica
In[10]:= WriteString[stmp, "Arbitrary output.\n", "More output."]
```

这里写入另一个字符串, 然后关闭该流:

```mathematica
In[11]:= WriteString[stmp, " Second line.\n"]; Close[stmp]
Out[11]= "tmp"
```

这里是该文件的内容. 这些字符串与给定的完全一样. 只有在明确给出换行符的地方才换行:

```mathematica
In[12]:= FilePrint["tmp"]
During evaluation of In[14]:=
Arbitrary output.
More output. Second line.
```

***
将输出写入一个流列表.

+ `Write[{stream1,stream 2},expr1,...]`  将表达式写入一个流列表
+ `WriteString[{stream1,stream2},str1,...]`  将字符串写入流列表

函数 `Write` 和 `WriteString` 的重要特点之一是它们不仅可以向一个流, 而且可以向一个流列表写入输出.

在使用 Wolfram 语言时, 定义由流列表组成的**通道**是方便的.
简单地令 Wolfram 语言向通道写入时, 就把同一对象写入了几个流之中.

在标准交互式 Wolfram 语言进程中, 有几个常用的输出通道.
它们指定某些类型输出的去向.
例如, `$Output` 指定标准输出的去向, 而 `$Messages` 指定信息的去向. 函数 `Print` 调用 `Write` 在 `$Output` 通道工作.
同理,  `Message` 调用 `Write` 在 `$Messages` 通道工作.
在 "主循环" 中列出了典型 Wolfram 语言进程所使用的通道.

注意, 通过 Wolfram Symbolic Transfer Protocol (WSTP) 运行 Wolfram 语言时使用不同的方式.
所有输出一般写入一个 WSTP 连接中, 但每个输出块以一个表明类型的"小包"出现.

在大部分情况下, Wolfram 语言使用的文件名与外部命令名与计算机操作系统所使用的名称相对应.
但在一些系统中, Wolfram 系统支持各种具有特殊名称的流.

***
一些计算机系统中的特殊流.

+ `"stdout"`  标准输出
+ `"stderr"`  标准错误

特殊流 `"stdout"` 允许将输出送到操作系统提供的标准输出.
但要注意仅能在 Wolfram 语言的简单文本界面中使用.
当与 Wolfram 语言的交互更复杂时, 这种流无法工作, 试图使用这种流会带来很多麻烦.

***
输出流的一些选项.

| 选项名 | 默认值 |exppantion|
| ----- | ----- | ----- |
| `FormatType` | `InputForm` | 默认输出格式 |
| `PageWidth` | `78` | 页按字符数的宽度 |
| `NumberMarks` | `$NumberMarks` | 近似数中是否包含记号`` ` ``
| `CharacterEncoding` | `$CharacterEncoding` | 特殊字符使用的编码 |

许多选项与输出流有关.
第一次用 `OpenWrite` 或 `OpenAppend` 打开输出流时就可以定义这些选项.

这里打开一个流, 指定 `OutputForm` 是默认输出格式:

```mathematica
In[13]:= stmp = OpenWrite["tmp", FormatType -> OutputForm]
Out[13]= OutputStream["tmp", 5324]
```

将表达式写入这个流后关闭它:

```mathematica
In[14]:= Write[stmp, x^2 + y^2, " ", z^2]; Close[stmp]
Out[14]= "tmp"
```

这些表达式按 `OutputForm` 格式写入了这个流:

```mathematica
In[15]:= FilePrint["tmp"]
During evaluation of In[17]:=
 2    2  2
x  + y  z
```

注意, 将一个欲写入流的表达式放在 `OutputForm` 或 `TeXForm` 等 Wolfram 语言格式指令内总可以覆盖对这个流所指定的输出格式.

选项 `PageWidth` 指定 Wolfram 语言文本输出的页宽.
所有输出都分成这种宽度的行.
不需要分行时应设置`PageWidth->Infinity`.
通常, 设定与输出设备相符的 `PageWidth` .
在许多系统中, 运行一个程序去找到这个页宽值.
用 `SetOptions` 可以给出设置 `PageWidth` 的默认规则,
例如, `PageWidth:><<"!devicewidth"`, 这就可以自动运行外部程序找出选项值.

打开一个流, 指定页宽为`20`个字符:

```mathematica
In[16]:= stmp = OpenWrite["tmp", PageWidth -> 20]
Out[16]= OutputStream["tmp", 5325]
```

写一个表达式后关闭该流:

```mathematica
In[17]:= Write[stmp, Expand[(1 + x)^5]]; Close[stmp]
Out[17]= "tmp"
```

表达式分为几行以便每行最多是`20`个字符:

```mathematica
In[18]:= FilePrint["tmp"]
During evaluation of In[20]:=
1 + 5*x + 10*x^2 +
 10*x^3 + 5*x^4 +
 x^5
```

`CharacterEncoding` 选项为一个字符串指定代码, 该代码将在送到 `Write` 或 `WriteString` 给出的流中任意包含这个特殊字符的字符串中使用.
在需要改动国际字符集, 或者需要某一输出设备接收不能处理的字符时,
常常使用 `CharacterEncoding` .

***
操作流的选项.

+ `Options[stream]`  找出对流设置的选项
+ `SetOptions[stream,Subscript[opt, 1]->Subscript[val, 1],...]`  对一个打开的流重写设置选项

打开一个具有默认设置的流:

```mathematica
In[19]:= stmp = OpenWrite["tmp"]
Out[19]= OutputStream["tmp", 5326]
```

改变打开流的 `FormatType` 选项:

```mathematica
In[20]:= SetOptions[stmp, FormatType -> TeXForm];
```

`Options` 显示了对打开的流所设置的选项:

```mathematica
In[21]:= Options[stmp]
Out[21]= {BinaryFormat -> False, FormatType -> TeXForm, PageWidth -> 78,
 PageHeight -> 22, TotalWidth -> \[Infinity], TotalHeight -> \[Infinity],
 CharacterEncoding :> Automatic, NumberMarks :> $NumberMarks,
 Method -> {"File", BinaryFormat -> False}}
```

关闭这个流:

```mathematica
In[22]:= Close[stmp]
Out[22]= "tmp"
```

***
操作标准输出通道的选项.

+ `Options[$Output]`  找出通道 `$Output` 中所有流的选项
+ `SetOptions[$Output,opt1->val1,...]`  对通道 `$Output` 中的所有流设置选项

在进程中的每一处, Wolfram 语言保持一个当前打开的所有输入输出流以及它们选项的列表  `Streams[]` .
有时需要直接查看这一列表, 但除过间接使用 `OpenRead` 等情况下, Wolfram 语言不允许修改这个列表.

## 程序中通常意义的流

[深入理解流, 什么是流](https://blog.csdn.net/qq_25221835/article/details/80776100)

流是个抽象的概念, 是对输入输出设备的抽象,
`Java` 程序中, 对于数据的输入/输出操作都是以"流"的方式进行.
设备可以是文件, 网络, 内存等.

流具有方向性, 至于是输入流还是输出流则是一个相对的概念, 一般以程序为参考, 如果数据的流向是程序至设备, 我们成为输出流, 反之我们称为输入流.

可以将流想象成一个"水流管道", 水流就在这管道中形成了, 自然就出现了方向的概念.

当程序需要从某个数据源读入数据的时候, 就会开启一个输入流, 数据源可以是文件, 内存或网络等等.
相反地, 需要写出数据到某个数据源目的地的时候, 也会开启一个输出流, 这个数据源目的地也可以是文件, 内存或网络等等.

流有哪些分类?

可以从不同的角度对流进行分类:

1. 处理的数据单位不同, 可分为: 字符流, 字节流
2. 数据流方向不同, 可分为: 输入流, 输出流
3. 功能不同, 可分为: 节点流, 处理流

`1.` 和 `2.` 都比较好理解, 对于根据功能分类的, 可以这么理解:

节点流: 节点流从一个特定的数据源读写数据.

即节点流是直接操作文件, 网络等的流, 例如 `FileInputStream` 和 `FileOutputStream` , 他们直接从文件中读取或往文件中写入字节流.

***
处理流: "连接"在已存在的流(节点流或处理流)之上通过对数据的处理为程序提供更为强大的读写功能.

过滤流是使用一个已经存在的输入流或输出流连接创建的, 过滤流就是对节点流进行一系列的包装.
例如 `BufferedInputStream` 和 `BufferedOutputStream` , 使用已经存在的节点流来构造, 提供带缓冲的读写, 提高了读写的效率, 以及 `DataInputStream` 和 `DataOutputStream` , 使用已经存在的节点流来构造, 提供了读写Java中的基本数据类型的功能.
他们都属于过滤流.

## 常见流类介绍

节点流类型常见的有: 对文件操作的字符流有`FileReader/FileWriter`, 字节流有`FileInputStream/FileOutputStream`.

处理流类型常见的有: 缓冲流: 缓冲流要"套接"在相应的节点流之上, 对读写的数据提供了缓冲的功能, 提高了读写效率, 同事增加了一些新的方法.

字节缓冲流有`BufferedInputStream`/`BufferedOutputStream`, 字符缓冲流有`BufferedReader`/`BufferedWriter`,
字符缓冲流分别提供了读取和写入一行的方法`ReadLine`和`NewLine`方法.

对于输出的缓冲流, 写出的数据, 会先写入到内存中, 再使用`flush`方法将内存中的数据刷到硬盘.
所以, 在使用字符缓冲流的时候, 一定要先`flush`, 然后再`close`, 避免数据丢失.

***
转换流: 用于字节数据到字符数据之间的转换.

仅有字符流`InputStreamReader`/`OutputStreamWriter`.
其中, `InputStreamReader`需要与 `InputStream`"套接", `OutputStreamWriter`需要与`OutputStream`"套接".

数据流: 提供了读写Java中的基本数据类型的功能.

`DataInputStream`和`DataOutputStream`分别继承自`InputStream`和`OutputStream`, 需要"套接"在`InputStream`和`OutputStream`类型的节点流之上.

对象流: 用于直接将对象写入写出.

流类有`ObjectInputStream`和`ObjectOutputStream`,
本身这两个方法没什么, 但是其要写出的对象有要求,
该对象必须实现`Serializable`接口, 来声明其是可以序列化的. 否则, 不能用对象流读写.

还有一个关键字比较重要,  `transient` , 由于修饰实现了`Serializable`接口的类内的属性, 被该修饰符修饰的属性, 在以对象流的方式输出的时候, 该字段会被忽略.
