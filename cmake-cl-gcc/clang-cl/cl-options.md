# Compiler Options

[Compiler Options](https://learn.microsoft.com/en-us/cpp/build/reference/compiler-options)

`cl.exe`工具 用来控制 `微软C++`(MSVC) 的 `C` 和 `C++` 编译器和链接器.
(compilers and linker)
`cl.exe` 只能在支持微软 Windows Visual Studio 的操作系统上运行.

>注意
>你只能从 Visual Studio `开发人员命令提示符`(developer command prompt)中启动这个工具.
你不能从 `系统命令提示符` 或 `文件管理器` 中启动它 (除非添加到环境变量)
更多信息, 请参见[从命令行使用MSVC工具集](https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line).

`编译器` 产生 `通用对象文件格式`(COFF)对象(`.obj`)文件.
`链接器` 产生可执行(`.exe`)文件 或 `动态链接库`(DLLs).

所有的 `编译器` 选项都是区分大小写的.
你可以使用 `slash`(`/`) 或 `破折号`( `-` )来指定 `编译器选项`.

要执行 `编译` 但不执行 `连接`, 请使用 [/c选项](https://learn.microsoft.com/en-us/cpp/build/reference/c-compile-without-linking) .

## 寻找编译器选项

要找到一个特定的 `编译器选项`, 请看下面的列表之一.

+ [按字母顺序排列的编译器选项](https://learn.microsoft.com/en-us/cpp/build/reference/compiler-options-listed-alphabetically)
+ [按类别列出的编译器选项](https://learn.microsoft.com/en-us/cpp/build/reference/compiler-options-listed-by-category)

## 指定编译器选项

每个 `编译器选项` 的主题都讨论了如何在开发环境中设置它.
有关在 `开发环境之外` 指定选项的信息, 请参见.

+ [MSVC编译器命令行语法](https://learn.microsoft.com/en-us/cpp/build/reference/compiler-command-line-syntax)
+ [CL命令文件](https://learn.microsoft.com/en-us/cpp/build/reference/cl-command-files)
+ [CL环境变量](https://learn.microsoft.com/en-us/cpp/build/reference/cl-environment-variables)

## 相关的构建工具

[MSVC链接器选项][] 也会影响你的程序的构建方式.

[MSVC链接器选项]: https://learn.microsoft.com/en-us/cpp/build/reference/linker-options

### 参见

[C/C++构建参考](https://learn.microsoft.com/en-us/cpp/build/reference/c-cpp-building-reference)
[CL调用链接器](https://learn.microsoft.com/en-us/cpp/build/reference/cl-invokes-the-linker)
