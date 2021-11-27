# package 包

`package`,`包` : tutorial/SettingUpWolframLanguagePackages

+ 使用 `Get` 读取 `.m`, `.wl`, `.wls` 文件时,文件中保存的表达式将被计算, 上下文中会建立起文件中的变量.
+ 读取 `.nb` 文件时, 不会运行其中保存的表达式, 只会建立起整个笔记本的符号表达式, (in lisp terms: 不进行进一步展开.)

## 环境变量和工作目录

参见: ref/\$ContextPath,  ref/\$Context:

`mma` 的环境变量一共有两部分, 分别叫做 `$ContextPath` 和 `$Context`.
分别叫做当前 `上下文列表`, 以及当前`上下文`.

+ `$ContextPath` 类似 Linux 的  `$PATH` 环境变量. `$Context` 类似于 Linux 的当前工作目录.
搜索函数以及变量名称的时候, 先搜索`$ContextPath`中的, 再搜索`$Context`中的. 这也和建立包的顺序一致.
`$Context` 可以看作是, 在文件搜索路径上附加了 `.`(当前工作目录).

如果遇到符号名冲突, 就应该用全名指定符号, 或者修改 ``Globla` `` 上下文中的符号名称, 因为 `package` 中的你一般无法修改.

+ `$ContextPath`中的路径按照出现顺序搜索, 也就是说, 前面的会覆盖后面的.

+ 在 Wolfram Language 会话的任何时刻, 总是有一个当前上下文 `$Context`.
你可以通过给出它们的 `短名称` 来引用这个上下文中的符号,
除非该符号被 `$ContextPath` 上具有相同 `短名称` 的符号所遮蔽(shadow).
即如果先在 `$ContextPath` 找到具有给定名称的符号, 它将被使用, 而不是当前 `context` 中的同名符号.

## 建立包

`BeginPackage[]` 和 `Begin[]`, 要配合相应的`EndPackage[]` and `End[]` 使用, 它们的效果不同:

+ `` BeginPackage["abc`"] `` 默认会同时设置 `$Context` 和 `$ContextPath`,
    让会话中只剩下 `` abc` `` and `` System` `` 两个上下文.
    当然, 它也有 `` BeginPackage["context`",{"need1`","need2`",... `` 这种语法, 可以引入其他上下文.

+ 而 `` Begin["abc`"] `` 不会更改 `$ContextPath`, 它只更改 `$Context` 为 `` "abc`" ``.

+ 此外, 调用 `` EndPackage[] `` 结束包时(不需要参数), 会将这个包, 比如 `` "abc`" `` 添加到 `$ContextPath` 的首元素,
这样当你读入一个包之后, 它里面的符号就会被优先使用. 而 `` End[] `` 不会更改 `$ContextPath`.

所以 mma  中建立包的大概流程为:

```mathematica
BeginPackage["Package`"]    设置 Package` 为当前上下文, 并且把 System` 放进 $ContextPath
f::usage="text", ...    介绍打算要导出的对象(不包括其他对象), 函数名在这里建立后, 它的上下文会是Package`, 可以被外部使用
Begin["`Private`"]    设置当前上下文为  Package`Private`
f[args]=value, ...     给出包中定义的主要内容
End[]   恢复到之前的上下文(此处为 Package`)
EndPackage[]    结束包, 把Package`放到上下文搜索路径中
```

+ `$Packages` 环境变量:提供与当前 `Wolfram` 系统会话中已加载的所有软件包相对应的上下文列表.
+ `` Needs ["context`"] ``:如果指定的上下文尚未在 `$Packages` 中, 则加载适当的文件.
它会根据情况, 自动调用且只调用一次 `Get[]`.

## 包的文件

ref: tutorial/ModularityAndTheNamingOfThings#3434

当您创建或使用 `Wolfram Language` 包时, 您经常希望以独立于系统的方式来引用文件.
你可以使用 `上下文`(Context)来做到这一点.

基本的思路是: 在每个计算机系统上都约定好了规则 -- 如何把本地文件系统, 与 Wolfram Language `上下文` 相对应.
当你使用`上下文`引用文件时, Wolfram Language 自动将 `上下文` 名称转换为特定计算机系统的 `文件路径`.

```mathematica
<<context` 读入对应于指定上下文的文件
```

例如导入 Wolfram System 的标准包

```mathematica
<< Quaternions`
```

通常情况下, `` <<name` `` 寻找的次序

+ `name.mx` ; `DumpSave` 格式的文件
+ `name.mx/$SystemID/name.mx` ; 特定计算机系统的 DumpSave 格式
+ `name.m`  ; Wolfram Language 源代码格式
+ `name/init.m`; 特定目录的初始化文件
+ `dir/...`; `$Path` 中指定的其他目录的文件

`Wolfram Language` 的设置是, ``<<name` `` 会自动尝试加载适当版本的文件.
它将首先尝试加载为您的特定计算机系统优化的 `name.mx` 文件.
如果没有找到, 那么它将尝试加载 ` name.m` 文件, 它包含普通的 `Wolfram Language` 语法, 这种格式独立于各种计算机系统.

如果`name`是个`目录`, 那么 `Wolfram Language` 将尝试加载该目录下的初始化文件 `init.m`.
`init.m` 文件的目的提供一种方便的方法, 如果是`package` 中包含多个独立的文件
这样, 你只需给出 `` <<name` `` 命令,  然后加载`init.m` 来初始化整个软件包, 读取任何需要的其他文件.

### 自动导入 Package

其他教程已经讨论了使用 `<<package` 和 `Needs[package]` 显式加载 Wolfram Language 软件包的方法.

然而, 有时你可能想设置 Wolfram Language, 以便在需要特定的包时自动加载它.
你可以使用 `DeclarePackage` 来给出在某个特定包中定义的 `符号`.
然后, 当这些符号之一被实际使用时, Wolfram Language 将自动加载定义该符号的`包`.

```mathematica
DeclarePackage["context`", {"name1", "name2", ...}]
如果使用了 `name_i` 中的任意一个, 它隶属的包会被自动被载入.
```

当你建立了大量的 Wolfram 语言包时, 创建一个额外的 `"名称文件"` 通常是个好主意,
它包含了一连串的 `DeclarePackage` 命令, 指定了在使用特定名称时要加载的`packages`.
在一个特定的 Wolfram System 会话中, 你只需要显式加载 `"名称文件"`.
然后, 所有其他的包将在需要的时候将会自动加载.

`DeclarePackage` 的工作方式是: 立即用你指定的名称创建符号, 但给这些符号都添加一个特殊属性 `Stub`.
每当 Wolfram Language 找到一个带有 `Stub` 属性的符号时, 它就会自动加载与该符号的`上下文`对应的`package`, 以试图找到该符号的`定义`.

## mma 初始化文件的位置

ref/file/init.m

Wolfram 系统初始化文件`init.m`包含启动代码, 每当 `Wolfram Language` 内核或前端启动时都要运行.

`init.m` 文件的可能位置如下:

+ `$BaseDirectory/Kernel` :内核初始化代码, 适用于所有用户
+ `$UserBaseDirectory/Kernel`: 内核初始化代码, 用于当前登录的用户
+ `$BaseDirectory/FrontEnd`: 所有用户的前端初始化代码
+ `$UserBaseDirectory/FrontEnd`:  前端初始化代码, 适用于当前登录的用户

#### 详细

+ 用户自定义初始化文件位于 `$UserBaseDirectory`,  系统范围初始化文件位于 `$BaseDirectory`,
    由于先读系统配置, 再读用户配置, 所以任何冲突的符号最后都依照用户的定义.
+ `Wolfram` 系统也加载 `Autoload` 目录中的 `init.m` 文件.
+ 要避免加载 `内核初始化文件`, 请使用内核命令行选项 `-noinit`.
+ 要在 `init.m` 之外, 指定其他文件用于内核初始化, 使用内核命令行选项 `-initfile file`, 其中 `file` 是额外的初始化文件.
+ 内核的模板初始化文件如果不存在, 将会被自动创建.
+ 如果`前端选项`在 Wolfram System 运行时被改变, 用户专属的前端初始化文件将被自动更新以反映这些改变. 系统范围的前端初始化文件将保持不变.

+ `Wolfram` 系统 `BaseDirectory` 的典型子目录:
    + `Applications`;  Wolfram Language 应用程序包
    + `Autoload`; 启动时自动加载的软件包
    + `FrontEnd`; 前端初始化文件
    + `Kernel`; 内核初始化文件
    + `Licensing`; 许可证管理文件
    + `SystemFiles`;  一般系统文件

通过对内核 `$Path` 变量的默认设置, 只需使用 `` <<name` `` 命令, 就可以从 Wolfram System 会话中加载`附加组件`.
加载 `init.m` 文件时, 可以再调用其他必要的文件或软件包.

通过将附加组件放在 `$BaseDirectory` 或 `$UserBaseDirectory` 的 `Autoload` 子目录下,
你可以让 Wolfram System 在你启动内核或前端时, 自动加载 `附加组件`.

+ `Kernel/init.m`;  初始化文件, 由内核加载.
+ `FrontEnd/init.m`; 由前端加载的初始化文件.
+ `Documentation/`; 前端要找到的帮助文档

### $Path,搜索路径

`$Path` 给出了, 搜索外部文件所使用的`搜索目录`的默认列表.

+ 在不同的计算机系统中, `目录` 和 `文件名` 的结构可能有所不同.
+ `$Path` 既用于 `Get` 调用文件的场景, 也用于 `Install` 调用外部程序.
+ 如果特定函数具有`Path`选项, 可以覆盖`$Path` 的设置.
+ `目录名` 是由字符串指定的. 被测试的完整文件名是`FileNameJoin[{directory,name}]` 的形式.
+ 在大多数计算机系统中, 以下特殊字符可用于给出 `目录名`.
    + `.` ; 当前目录
    + `..` ; 层次结构中的上一级目录
    + `~` ; 用户的主目录
+ `$Path` 可以包含嵌套的子列表.

+ `Get`, `Needs`, `OpenRead`, `Install` 和 其他函数使用 `FindFile` 寻找文件, 然后加载(load).
+ `FindFile` 递归搜索 `$Path` 中的目录, 也就是遍历子目录搜索给定的文件名,
目录中浅层的文件名会覆盖深层的文件名, 如果想找到深层的文件, 可以添加目录前缀进行区分.
例如:

    ```mathematica
    FindFile["a.txt"]
    FindFile["coes/a.txt"]
    ```

### 基本例子

+ 添加一个`目录`, 用于查找其中的文件, 但优先级在所有 `系统默认值` 之后:

    ```mathematica
    AppendTo[$Path, FileNameJoin[{$HomeDirectory, "MyPackages"}]];
    ```

+ 添加一个`目录`, 用于查找其中的文件, 但优先级在任何 `系统默认值` 之前:

    ```mathematica
    PrependTo[$Path, FileNameJoin[{$HomeDirectory, "MyPackages", "LookHereFirst"}]];
    ```

## 读取文件,Get

```mathematica
<<name; 读入文件, 计算其中的每个表达式, 并返回最后一个表达式.
Get[stream] ; 从 stream(流)中读取, 计算其中的每个表达式并返回最后一个表达式.
Get["file", "key"]; 读取一个用 Encode["source", "file", "key"] 编码的文件.
```

### Details and Options

+ 如果 `name` 是 Wolfram Language 上下文的名称, 即结尾字符是 `` ` ``(上下文标记字符), 那么 `Get` 将处理该名称以查找要读取的文件.
+ 对于 `` "context`" `` 形式的名称, `Get` 将默认搜索以下文件.
    + `context.mx`; `DumpSave` 格式的文件
    + `context.mx/$SystemID/context.mx`; 本地计算机系统的 `DumpSave` 格式文件
    + `context.wl`; Wolfram 语言源代码格式的文件
    + `context/Kernel/init.wl`; 特定目录的内核初始化文件
    + `context/init.wl`; 特定目录的一般初始化文件
    + `context.m`; 采用 Wolfram 语言源代码格式的文件
    + `context/Kernel/init.m`; 某特定目录的内核初始化文件
    + `context/init.m`; 某特定目录的一般初始化文件

+ 对于 `` "context`subcontext`" `` 形式的名称, `Get` 默认会在名为 `"context"` 的目录下搜索 `` "subcontext`" ``.
+ 如果`name`是一个文件名称, 任何 `文件扩展名` 都必须明确包括在内.
+ ` <<"name "` 等同于 `<<name`. 可以省略双引号, 如果`name` 只包含

        字母, 数字,  `  /   .  \  !   -  _  :  $  *  ~  ?

    在 "Operator Input Forms" 中有更详细的描述.
+ 可以给出以下选项:
    `CharacterEncoding`; `$CharacterEncoding`; raw 字符使用什么编码
    `Method`; `Automatic`; 读取`数据流`时使用的方法
   `Path`;  `$Path`;    用于搜索给定文件的目录

+ Wolfram Language 输入文件的`语法错误`以标准形式报告:

        filename: line: syntax error in expr

    即使检测到语法错误, `Get` 也会继续尝试读取文件.
    然而, 如果检测到错误, `$Context` 和 `$ContextPath` 将被重置为调用 `Get` 时的值.

+ `Get`可以读取 `.nb` 笔记本文件, 返回代表这些文件的 底层 box 结构.
+ `Get[CloudObject[...]]` 可以用来从云端获取文件.
+ `Get[LocalObject[...]` 可以用来从本地持久性存储中获取文件.
+ `Get[Databin[...]]` 可以获得 Wolfram Data Drop 中的一个 Databin 的内容.
+ 当对本地文件进行操作时, 全局变量 `$Input` 和 `$InputFileName` 在执行 `Get` 时被分别设置为`文件名`和被读文件的`完整路径`.
+ 有了 `Method` 选项, `stream`就会用给定的输入流方法打开. 它覆盖 `Get` 解析文件名的默认方式.
`Method` 选项的值可以是 `$InputStreamMethods` 的任何成员.

### 选项

#### Path

默认情况下, 在搜索文件时会查询 `$Path`上的所有目录:

```mathematica
Get["ExampleData/language"]
Out[1]= 22 a b + 56 c + 13 a d
```

强制只搜索当前目录:

```mathematica
Get["ExampleData/language", Path -> "."]
Out[2]= $Failed
```

### 属性和关系

+ `` FindFile["context`"] `` 给出了 ``Get["context`"]`` 将加载的文件:

    ```mathematica
    FindFile["EquationTrekker`"]
    Out[1]= "/Applications/Mathematica.app/Contents/AddOns/Packages/".
    EquationTrekker/Kernel/init.m"
    ```

    加载该文件

    ```mathematica
    Get[%]
    ```

    验证 `EquationTrekker` 包是否真的被加载:

    ```mathematica
    Names["EquationTrekker`*"]
    Out[3]= {"DifferentialEquationTrek", "EquationTrekker", ...
    ```

+ 确保`初始化文件`只被读取一次:

    ```mathematica
    Once[Get["init.m"]]
    ```
