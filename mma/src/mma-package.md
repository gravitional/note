# package 包

`package`,`包` : tutorial/SettingUpWolframLanguagePackages

使用`Get`读取`.m`文件, 能够计算文件中的表达式, 上下文中会建立起文件中的变量. 而读取`.nb`文件, 不会建立变量, 只会建立一个笔记本的符号表达式.

`mma` 中写包的大概结构:

```mathematica
BeginPackage["Package`"]    设置 Package` 为当前上下文, 并且把 System` 放进 $ContextPath
f::usage="text", ...    介绍打算要导出的对象(不包括其他对象), 函数名在这里建立后, 它的上下文会是Package`, 可以被外部使用
Begin["`Private`"]    设置当前上下文为  Package`Private`
f[args]=value, ...     给出包中定义的主要内容
End[]   恢复到之前的上下文(此处为 Package`)
EndPackage[]    结束包, 把Package`放到上下文搜索路径中
```

***

+ 查看变量 `$Packages` :提供与当前 `Wolfram` 系统会话中已加载的所有软件包相对应的上下文列表.
+ `` Needs ["context`"] ``:如果指定的上下文尚未在 `$Packages` 中, 则加载适当的文件. 它会自动调用`Get[]`

***
`mma` 的环境变量一共有两部分, 分别叫做`$ContextPath` and `$Context`. 分别是当前上下文列表, 以及当前上下文.
参见: ref/\$ContextPath,  ref/\$Context:

`$ContextPath`类似 `Linux` 的 `$PATH`环境变量. `$Context`类似于`Linux`的当前工作目录.
搜索函数以及变量名称的时候, 先搜索`$ContextPath`中的, 再搜索`$Context`中的. 这和建立包的顺序也是一样的.
`$ContextPath`中的路径按照出现顺序搜索, 前面的会覆盖后面的.

***
`BeginPackage[]` and `Begin[]` 都要配合相应的`EndPackage[]` and `End[]` 使用, 它们的效果不同:

+ ``BeginPackage["abc`"] `` 默认会同时设置 `$Context` and `$ContextPath`, 让会话中只剩下`` abc` `` and `` System` ``两个上下文.
当然, 它也有`` BeginPackage["context`",{"need1`","need2`",... ``这种语法.
+ 而`` Begin["abc`"] `` 不会更改 `$ContextPath`, 它只更改`$Context` 为 `` "abc`" ``.
+ 此外, 调用`` EndPackage[] `` (不需要参数) 结束包时, 会将这个包, 比如`` "abc`" ``添加到`$ContextPath`的前面.
而`` End[] `` 不会更改`$ContextPath`.

## 包的调用

ref: tutorial/ModularityAndTheNamingOfThings#3434

当您创建或使用 `Wolfram Language` 包时, 您经常希望以独立于系统的方式来引用文件. 你可以使用上下文来做到这一点.

在每个计算机系统上都有一个文件系统, 与 Wolfram Language 上下文对应的规则.
当你使用上下文引用一个文件时, Wolfram Language 自动将上下文名称转换为你所在计算机系统的文件名称.

```mathematica
 <<context` 读入对应于指定上下文的文件
```

`Wolfram Language` 的设置是, ``<<name` `` 会自动尝试加载适当版本的文件. 它将首先尝试加载为您的特定计算机系统优化的 `name.mx` 文件.
如果没有找到这样的文件, 那么它将尝试加载一个包含普通 `Wolfram Language` 语法的` name.m` 文件, 这种格式独立于各种计算机系统.

如果名称是一个目录, 那么 `Wolfram Language` 将尝试加载该目录下的初始化文件 `init.m`. `init.m` 文件的目的是为设置包含许多独立文件的 `Wolfram Language` 包提供一种方便的方法.
其目的是让你只需给出`` <<name` ``命令, 然后加载`init.m`来初始化整个软件包, 读取任何必要的其他文件.

## init.m

Wolfram 系统初始化文件`init.m`包含启动代码, 每当 `Wolfram Language` 内核或前端启动时都要运行.

`init.m` 文件的可能位置包括如下.

+ `$BaseDirectory/Kernel` :内核初始化代码, 适用于所有用户
+ `$UserBaseDirectory/Kernel`: 内核初始化代码, 用于当前登录的用户
+ `$BaseDirectory/FrontEnd`: 所有用户的前端初始化代码
+ `$UserBaseDirectory/FrontEnd`:  前端初始化代码, 适用于当前登录的用户

`Wolfram`系统`基础目录`的典型子目录.

+ `Applications`;  Wolfram Language应用程序包
+ `Autoload`; 启动时自动加载的软件包
+ `FrontEnd`; 前端初始化文件
+ `Kernel`; 内核初始化文件
+ `Licensing`; 许可证管理文件
+ `SystemFiles`;  一般系统文件

通过对内核`$Path`变量的默认设置, 只需使用`` <<name` ``命令, 就可以从Wolfram System会话中加载附加组件.
加载 `init.m` 文件时, 可以再调用其他必要的文件或软件包.

通过将附加组件放在 `$BaseDirectory` 或 `$UserBaseDirectory` 的 `Autoload` 子目录下, 你可以让 Wolfram System 在你启动内核或前端时自动加载附加组件.

+ `Kernel/init.m`; 一个初始化文件, 由内核加载.
+ `FrontEnd/init.m`; 一个由前端加载的初始化文件.
+ `Documentation/`; 前端要找到的帮助文档

## $Path

`$Path` 给出了寻找外部文件时要搜索目录的默认列表.

+ 目录和文件名的结构在不同的计算机系统中可能有所不同.
+ `$Path`既用于`Get`调用的文件, 也用于`Install`调用的外部程序.
+ `$Path` 的设置可以通过特定的函数中指定`Path`选项来更改.
+ 目录名是由字符串指定的. 被检测的完整文件名是`FileNameJoin[{directory,name}]的形式`.
+ 在大多数计算机系统中, 以下特殊字符可以在目录名中使用.
    + `.` ; 当前目录
    + `...` ; 层次结构中的上一级目录
    + `~` ; 用户的主目录
+ `$Path` 可以包含嵌套的子列表.
