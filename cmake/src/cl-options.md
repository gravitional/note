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

## 常用选项

[常用cl编译命令参数解释](https://www.cnblogs.com/findumars/p/7433780.html)
[微软 CL.exe 编译器选项](https://blog.csdn.net/u011471873/article/details/53129431)

+ `/c` 编译但不链接.
+ `/I` 指定头文件的目录
+ `/C` 在编译期间保留代码注释, 这里和 `/I` 连在一起使用, `/IC`

首先介绍一个概念, VC中有个 `PDB` 文件, 
全称是 Program Database, 用来存放程序信息的小的数据库文件.
编译 Debug 版本时, 调试信息需要保留, 
我们可以选择直接将调试信息写到 .obj 文件中, 或者存到.pdb文件中.

+ `/Z7` 不产生 `.pdb`文件, 将所有调试信息存入 `.obj` 文件中
+ `/Zi` 和 `/ZI` 都产生 `.pdb` 文件, 不过 `/ZI` 支持"编辑继续调试"功能, 
(the edit and continue feature), 看上去更酷
+ `/ZI` 有一些边际效应, 会禁止 `#pragma optmize` 指令, 也不能和 `/clr` 一起用.

+ `/nologo`- 已经无效, 自己生成命令行的时候, 没必要用了.

+ `/W3` 一种警告级别, VC提供了很多警告级别,
自己编译的话, 直接用 `/Wall` 最好.

+ `/WX-` 估计是和:NO的意思相同, 也就是不启用该功能 
+ `/WX` 的意思是将warning转变成error, 这样强迫消除所有的warning, 
如果和 `/Wall`一起使用, 那是最好的.

+ `/sdl` 是对代码进行安全检查, 如果发现相关警告, 转变成错误输出

+ `/Od` 禁止优化
+ `/Oy-` 禁止该选项, 该选项如果没有 `-` 号, 
则会在 x86 上编译时忽略frame-pointer, 起到加速程序的作用.  frame-pointer, 我暂时不知道是啥.

+ `/D` 预处理定义, 后面可以跟不同的参数都是宏啊, 比如

```powershell
/D WIN32 /D _DEBUG /D _CONSOLE /D _UNICODE /D UNICODE
```

+ `/Gm`; 启用最小化重新编译, `VC` 用 `.idb` 保留了上次编译的缓存信息, 包括文件依赖关系. 
下次编译时可以使用 `.idb` 文件用来检查, 跳过不需要重新编译的文件.
+ `/EH` 异常处理模式, 后面可以接一些参数, 通常都用 `/EHsc`

+ `/RTC` 运行时错误检查
+ `/MDd` 和上面一个都很重要, 使用Debug版本的多线程运行时动态库
+ `/GS` 缓冲区安全检查

+ `/fp:precise` 是和浮点数相关
+ `/Zc:wchar_t`; 指定wchar_t是native type
+ `/Yc "stdafx.h"`; 指定 `stdafx.h` 为预编译头文件

+ `/Fp"Debug\HelloWorld.pch"` 指定预编译文件, 
这样 `staafx.h` 编译出的内容放在 `HelloWorld.pch` 中, 可以大大提高编译速度. 
因为VC中的预编译文件很大, 每次重新编译很耗时.
+ `/Fo"Debug\\"` 指定 `.obj` 文件存放在Debug目录下
+ `/Fd"Debug\vc110.pdb"`; 指定pdb文件路径, pdb前面已经介绍过了.

+ `/Gd` 仅用于x86平台. 如果C++函数没有显式指定 `__stdcall` 或者 `__fastcall`, 就采用 `__cdecl`
+ `/Tp`; 指定C++源文件
+ `/analyze-`; 这是关闭代码分析功能
+ `/errorReport:prompt`; 提示内部错误信息

### 优化

+ `/O1`; 创建小代码.
+ `/O2`; 创建快速代码.
+ `/Ob`; 控制内联展开.
+ `/Od`; 禁用优化.
+ `/Og`; 使用全局优化.
+ `/Oi`; 生成内部函数.
+ `/Os`; 代码大小优先.
+ `/Ot`; 代码速度优先.
+ `/Ox`; 使用最大优化 (/Ob2gity /Gs).
+ `/Oy`; 省略帧指针. (仅限 x86)

### 代码生成

+ `/arch` 使用 SSE 或 SSE2 指令生成代码. (仅限 x86)
+ `/EH` 指定异常处理模型.
+ `/favor` 生成为特定 x64 结构或为 AMD64 和 64 位内存扩展技术 (EM64T) 结构中的特定宏结构进行了优化的代码.
+ `/fp` 指定浮点行为.
+ `/GR` 启用运行时类型信息 (RTTI).
+ `/Qpar` 启用标有 `#pragma loop()` 指令循环的自动并行化.

### 输出文件

+ `/FA` 列表配置文件的程序集.
+ `/Fa` 创建列表文件的程序集.
+ `/Fd` 重命名程序数据库文件.
+ `/Fe` 重命名可执行文件.
+ `/Fi` 指定预处理输出文件名.
+ `/Fm` 创建映射文件.
+ `/Fo` 创建对象文件.
+ `/Fp` 指定预编译头文件名.
+ `/FR` 生成浏览器文件.
+ `/Fx` 插入的代码与源文件合并.

### 调试

+ `/GS` 检查缓冲区安全.
+ `/GZ` 与 `/RTC1` 相同
+ `/RTC` 启用运行时错误检查.
+ `/Wp64` 检测 64 位可移植性问题.

### 预处理器

+ `/AI` 指定在解析传递到 `#using` 指令的文件引用时搜索的目录.
+ `/D` 定义常数和宏.
+ `/E` 将预处理器输出复制到标准输出.
+ `/I<dir>` 在 `<dir>` 中搜索包含文件(include).

+ `/u` 移除所有的预定义宏.
+ `/X` 忽略标准包含目录.

### 语言

+ `/openmp` 在源代码中启用 `#pragma omp`.
+ `/vmb` 对指向成员的指针使用最佳的基.
+ `/vmg` 对指向成员的指针使用完全一般性.
+ `/vmm` 声明多重继承.
+ `/vms` 声明单一继承.
+ `/vmv` 声明虚拟继承.
+ `/volatile` 选择 `volatile` 关键字如何解释.

### 链接

+ `/F` 设置堆栈大小.
+ `/LD` 创建动态链接库.
+ `/LDd` 创建调试动态链接库.
+ `/LN` 创建 MSIL 模块.
+ `/link` 将指定的选项传递给 LINK.

### 预编译头

+ `/Y-` 忽略当前生成中的所有其他预编译头编译器选项.
+ `/Yc` 创建预编译头文件.
+ `/Yd` 将完整的调试信息放在所有对象文件中.
+ `/Yu` 在生成期间使用预编译头文件.

### 杂项

+ `/?` 列出编译器选项.
+ `@` 指定响应文件.
+ `/analyze` 启用代码分析.
+ `/c` 编译但不链接.
+ `/doc` 将文档注释处理到一个 XML 文件中.

+ `/FC` 显示源代码文件的完整路径传递给诊断文本的 cl.exe.
+ `/HELP` 列出编译器选项.
+ `/J` 更改默认的 char 类型.
+ `/kernel` 编译器和链接器将创建在内核中执行的二进制文件.
+ `/MP` 同时生成多个源文件.
+ `/nologo` 取消显示登录版权标志.
+ `/showIncludes` 显示所有的列表在编译时包含文件.
+ `/Tc, /TC` 指定 C 源文件.
+ `/Tp, /TP ` 指定 C++ 源文件.
+ `/V` 设置版本字符串.
+ `/Wall` 启用所有警告, 包括默认情况下禁用的警告.
+ `/W` 设置警告等级.
+ `/w` 禁用所有警告.
+ `/WL` 在从命令行编译 C++ 源代码时启用错误消息和警告消息的单行诊断.
