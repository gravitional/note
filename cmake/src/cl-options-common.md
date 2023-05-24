# 常用选项

[常用cl编译命令参数解释](https://www.cnblogs.com/findumars/p/7433780.html)
[微软 CL.exe 编译器选项](https://blog.csdn.net/u011471873/article/details/53129431)

## 编译

+ `/c` 编译但不链接.
+ `/I` 指定头文件的目录
+ `/C` 在编译期间保留代码注释, 这里和 `/I` 连在一起使用, `/IC`

### 额外包含目录

[/I (额外的包括目录)](https://learn.microsoft.com/en-us/cpp/build/reference/i-additional-include-directories)

+ 语法; `/I 目录`
+ 目录将被添加到搜索 `include files` 的列表.
`/I` 和 `目录` 之间的 `空格` 是可选的.
包含空格的目录必须用双引号括起来, 目录可以是 `绝对路径` 或 `相对路径`.
+ `备注`; 要添加一个以上的目录, 请多次使用此选项.
只有在找到指定的包含文件时, 才会停止搜索目录.
+ 你可以在同一命令行中, 使用这个选项, 连同 `/X` (忽略标准包含路径) 选项.

#### 搜索次序

`#include` 指令可用 `双引号`(或本地优先)的形式指定, 例如,

```cpp
#include "local.h".
```

或者用角括号(或include-path-first)形式指定, 例如

```cpp
#include <iostream>
```

编译器会按照以下顺序搜索目录:

+ 如果 `#include` 指令是用双引号形式指定的, 它首先搜索本地目录.
搜索从与 `#include` 指令所在文件的 `当前目录`开始.
如果找不到该文件, 它将在当前打开的 `include 文件` 所属的目录中搜索, 搜索的顺序与打开的顺序相反.
搜索从 `父类include文件` 的目录开始, 然后向任何 `祖类include文件` 的目录上溯.
+ 如果 `#include` 指令是以角括号形式指定的,
或者 `本地目录` 搜索失败, 那么将搜索使用 `/I` 选项指定的目录,
按照命令行上指定的顺序.

+ 在 `INCLUDE环境变量` 中指定的目录.

#### 要在Visual Studio开发环境中设置这个编译器选项

打开项目的 `属性页` 对话框.
详情请见 [Visual Studio中设置C++编译器和构建属性](https://learn.microsoft.com/en-us/cpp/build/working-with-project-properties).

选择 `配置属性`>`C/C++`>`常规属性`页.
`Configuration Properties`>`C/C++`>`General`, 修改 `额外包含目录` 属性.
你可以在这个属性中一次指定多个目录, 目录必须用分号(`;`)分开.

#### 要以编程方式设置这个编译器选项

参见 [AdditionalIncludeDirectories](https://learn.microsoft.com/en-us/dotnet/api/microsoft.visualstudio.vcprojectengine.vcclcompilertool.additionalincludedirectories).

#### 例子

下面的命令按照以下顺序查找 `main.c` 所要求的包含文件:
首先, 如果使用双引号指定, 将搜索 `本地文件`.
接下来, 继续在 `\include` 目录中搜索,
然后是 `\my\include` 目录,
最后是分配给 `INCLUDE` 环境变量的目录, 按照从左到右的顺序.

```cmd
CL /I \include /I\my\include main.c
```

### 调试信息

首先介绍一个概念, `VC` 中有个 `PDB` 文件,
全称是 Program Database, 用来存放程序信息的小的数据库文件.
编译 Debug 版本时, 调试信息需要保留,
我们可以选择直接将调试信息写到 `.obj` 文件中, 或者存到 `.pdb` 文件中.

+ `/Z7` 不产生 `.pdb`文件, 将所有调试信息存入 `.obj` 文件中
+ `/Zi` 和 `/ZI` 都产生 `.pdb` 文件, 不过 `/ZI` 支持 `编辑继续调试` 功能,
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

## 优化

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

## 代码生成

+ `/arch` 使用 SSE 或 SSE2 指令生成代码. (仅限 x86)
+ `/EH` 指定异常处理模型.
+ `/favor` 生成为特定 x64 结构或为 AMD64 和 64 位内存扩展技术 (EM64T) 结构中的特定宏结构进行了优化的代码.
+ `/fp` 指定浮点行为.
+ `/GR` 启用运行时类型信息 (RTTI).
+ `/Qpar` 启用标有 `#pragma loop()` 指令循环的自动并行化.

## 输出文件

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

## 调试

+ `/GS` 检查缓冲区安全.
+ `/GZ` 与 `/RTC1` 相同
+ `/RTC` 启用运行时错误检查.
+ `/Wp64` 检测 64 位可移植性问题.

## 预处理器

+ `/AI` 指定在解析传递到 `#using` 指令的文件引用时搜索的目录.
+ `/D` 定义常数和宏.
+ `/E` 将预处理器输出复制到标准输出.
+ `/I<dir>` 在 `<dir>` 中搜索包含文件(include).

+ `/u` 移除所有的预定义宏.
+ `/X` 忽略标准包含目录.

## 语言

+ `/openmp` 在源代码中启用 `#pragma omp`.
+ `/vmb` 对指向成员的指针使用最佳的基.
+ `/vmg` 对指向成员的指针使用完全一般性.
+ `/vmm` 声明多重继承.
+ `/vms` 声明单一继承.
+ `/vmv` 声明虚拟继承.
+ `/volatile` 选择 `volatile` 关键字如何解释.

## 链接

+ `/F` 设置堆栈大小.
+ `/LD` 创建动态链接库.
+ `/LDd` 创建调试动态链接库.
+ `/LN` 创建 MSIL 模块.
+ `/link` 将指定的选项传递给 LINK.

## 预编译头

+ `/Y-` 忽略当前生成中的所有其他预编译头编译器选项.
+ `/Yc` 创建预编译头文件.
+ `/Yd` 将完整的调试信息放在所有对象文件中.
+ `/Yu` 在生成期间使用预编译头文件.

## 杂项

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

## /std 指定C++ std版本

[/std](https://learn.microsoft.com/en-us/cpp/build/reference/std-specify-language-standard-version)

启用指定版本的 C或C++语言标准 中支持的C和C++语言功能.

### 语法

+ `/std:c++14`
+ `/std:c++17`
+ `/std:c++20`
+ `/std:c++latest`
+ `/std:c11`
+ `/std:c17`
