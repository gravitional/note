# Compiler Command-Line Syntax

[Compiler Command-Line Syntax](https://learn.microsoft.com/en-us/cpp/build/reference/compiler-command-line-syntax)

`CL` 命令行使用以下语法:

```powershell
CL [option...]  file... [option | file]...  [lib...]    [@command-file]   [/link link-opt...]
```

下表描述了 `CL` 命令的输入.

条目 意义

+ `option`; 一个或多个 [CL选项](https://learn.microsoft.com/en-us/cpp/build/reference/compiler-options).
注意, 所有选项都适用于所有指定的源文件.
选项由正斜杠(/)或破折号(-)指定.
如果 `选项` 需要一个参数, 该选项的描述记录了该选项和参数之间是否允许有空格.
选项名称(除了/HELP选项)是区分大小写的. 更多信息请参见 [CL选项的顺序](https://learn.microsoft.com/en-us/cpp/build/reference/order-of-cl-options).

+ `file`; 一个或多个源文件, `.obj文件` 或 `库` 的名称.
CL编译源文件并将 `.obj` 文件和 `库` 的名称传递给 `链接器`. 更多信息, 请参见 [CL文件名语法](https://learn.microsoft.com/en-us/cpp/build/reference/cl-filename-syntax).

+ `lib`;  一个或多个库名. `CL` 将这些名称传递给链接器.

+ `command-file` 一个包含多个选项和文件名的文件. 更多信息, 请参见 [CL命令文件][].

+ `link-opt`; 一个或多个 [MSVC 链接器选项][], `CL` 将这些选项传递给链接器.

你可以指定任何数量的 `选项`, `文件名` 和 `库名`,
只要命令行上的字符数不超过 `1024`, 这是操作系统规定的限制.

关于 `cl.exe` 的返回值的信息, 见 [cl.exe的返回值][].

>注意
>命令行输入的1024个字符的限制并不保证在未来的Windows版本中保持不变.

[CL命令文件]: https://learn.microsoft.com/en-us/cpp/build/reference/cl-command-files
[MSVC 链接器选项]: https://learn.microsoft.com/en-us/cpp/build/reference/linker-options
[cl.exe的返回值]: https://learn.microsoft.com/en-us/cpp/build/reference/return-value-of-cl-exe

## LINK input file

[LINK input files](https://learn.microsoft.com/en-us/cpp/build/reference/link-input-files)

你向链接器提供的文件中包含
`objects`, `import ` 和 标准库, `resources`, `module definitions` 和 `command input`.
`LINK` 不使用文件扩展名来对文件的内容进行假设.
相反, LINK 检查每个输入文件, 以确定它是什么类型的文件.

命令行上的 `Object files` 是按照它们在 `命令行上出现的顺序` 来处理的.
`库` 也是按 `命令行顺序` 搜索的, 但有以下注意事项:
当从 `库A` 中引入 object 文件时, 未解决(unresolved )的 `符号` 首先在 `库A` 中搜索,
然后是 `命令行` 和 [/DEFAULTLIB(指定默认库)][] 指令中列出的库,
然后是 命令行开头处 的任何库.

[/DEFAULTLIB(指定默认库)]: https://learn.microsoft.com/en-us/cpp/build/reference/defaultlib-specify-default-library

>注意
> `LINK` 不再接受分号(或任何其他字符)作为 `response 文件` 和 `order 文件` 中 注释 的开始.
> 分号只被认为是 `模块定义文件`(`.def`)中注释的开始.

LINK使用以下类型的输入文件.

+ [.obj files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-obj-files-as-linker-input)
+ [.netmodule files](https://learn.microsoft.com/en-us/cpp/build/reference/netmodule-files-as-linker-input)
+ [.lib files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-lib-files-as-linker-input)
+ [.exp files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-exp-files-as-linker-input)
+ [.def files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-def-files-as-linker-input)
+ [.pdb files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-pdb-files-as-linker-input)
+ [.res files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-res-files-as-linker-input)
+ [.exe files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-exe-files-as-linker-input)
+ [.txt files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-txt-files-as-linker-input)
+ [.ilk files](https://learn.microsoft.com/en-us/cpp/build/reference/dot-ilk-files-as-linker-input)

### See also

[MSVC linker reference](https://learn.microsoft.com/en-us/cpp/build/reference/linking)
[MSVC linker options](https://learn.microsoft.com/en-us/cpp/build/reference/linker-options)

## /link (Pass Options to Linker)

将一个或多个 `链接器选项` 传递给链接器.

+ 语法

```powershell
/link linker-options
```

+ 参数

    linker-options

要传递给链接器的链接器选项或选项.

+ 备注
`/link` 选项和它的 `链接器选项` 必须出现在任何 `文件名` 和 `CL选项` 之后.
在 `/link` 和任何 `链接器选项` 之间需要一个空格.
更多信息, 请参见 [MSVC 链接器参考](https://learn.microsoft.com/en-us/cpp/build/reference/linking).

+ 示例

这个命令行示例编译了hello.cpp并将其链接到现有的对象文件there.obj.
然后它向链接器传递了一个额外的/VERSION命令.

```powershell
cl /W4 /EHsc hello.cpp there.obj /link /VERSION:3.14
```

### 要在Visual Studio开发环境中设置这个编译器选项

`IDE` 通常会发送单独的命令来 `编译` 和 `链接` 你的代码.
你可以在你的 `项目属性` 页中设置 `链接器选项`.

+ 打开项目的 `属性页` 对话框. 详情请见 [在Visual Studio中设置C++编译器和构建属性](https://learn.microsoft.com/en-us/cpp/build/working-with-project-properties).

+ 选择 `配置属性` > `链接器` 文件夹.
+ 修改一个或多个属性. 选择 "确定 "来保存你的修改.

### 要以编程方式设置该编译器选项

这个编译器选项不能以编程方式改变.

## 设置编译器和构建特性

[Set compiler and build properties](https://learn.microsoft.com/en-us/cpp/build/working-with-project-properties)
