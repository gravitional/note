# windows import lib 导入库

[How does the Import Library work? Details?](https://stackoverflow.com/questions/3573475/how-does-the-import-library-work-details)

与 DLL 文件的链接可以在 link time *implicitly*进行,
也可以在 run time *explicitly*进行.

无论哪种方式, DLL 最终都会加载(loaded)到进程的内存空间中,
其所有导出的入口点(exported entry points)都可供应用程序使用.

如果在运行时显式使用,
则需要使用 `LoadLibrary()` 和 `GetProcAddress()` 手动加载 DLL,
并获取需要调用的函数指针.

如果在构建程序时 `隐式链接`,
then stubs for each DLL export used by the program
get linked in to the program from an import library,
and those stubs get updated
as the EXE and the DLL are loaded when the process launches.
(是的, 我在这里简化了很多......).

这些 stub 需要来自某个地方,
而在微软的工具链中, 它们来自一种特殊形式的 `.LIB` 文件, 称为 *import library*.
所需的 `.LIB` 文件通常与 `DLL` 文件同时生成,
并包含从 DLL 导出的每个函数的 stub.

令人困惑的是, 同一库的静态版本也会以 `.LIB` 文件的形式发布.
除了作为 DLL 的导入库的 LIB 文件通常比 对应的静态 LIB 文件要小
(通常小得多)之外, 并没有什么简单的方法可以将它们区分开来.

顺便提一下, 如果使用 GCC 工具链, 实际上并不需要 import lib来配合 DLL.
移植到 Windows 的 Gnu 链接器版本可以直接理解 DLL,
并能在 运行过程中合成(synthesize)大部分所需的存根.

## 更新

如果你想知道所有的螺母和螺栓到底在哪里, 到底发生了什么, MSDN 总是能帮到你.
Matt Pietrek 的文章
[深入了解 Win32 可移植可执行文件格式](http://msdn.microsoft.com/en-us/magazine/cc301805.aspx)非常全面地概述了 EXE 文件的格式及其加载和运行方式.
自 2002 年发表在 `MSDN Magazine` 上以来,
这篇文章已经更新, 涵盖了 .NET 和更多内容.

此外, 了解程序究竟使用了哪些 DLL 也很有帮助.
这方面的工具是 Dependency Walker, 又名 `depends.exe`.
它的一个版本包含在 Visual Studio 中,
但最新版本可从其作者处获得,
网址是 [walker](http://www.dependencywalker.com/).

它可以识别链接时指定的所有 DLL(包括早期加载和延迟加载),
还可以运行程序, 并在运行时注意加载的任何其他 DLL.

## 更新 2

我重写了之前的一些文字, 以便在重读时加以澄清,
并使用了隐式链接和显式链接的术语, 以便与 MSDN 保持一致.

因此, 我们有三种方法可以让程序使用库函数.
显而易见的后续问题是 "如何选择哪种方式?"

静态链接 是程序本身的主要链接方式.
所有对象文件都被列出, 并由链接器收集到 `EXE` 文件中.
在此过程中, 链接器会处理一些细小的杂事,
比如修正对全局符号的引用, 以便你的模块可以调用彼此的函数.
库也可以静态链接.
组成库的对象文件由库管理器收集在一个 .LIB 文件中,
链接器会在该文件中搜索包含所需符号的模块.
静态链接的一个效果是, 只有程序使用到的库中的模块才会被链接到程序中,
其他模块将被忽略.
例如, 传统的 C 数学库包括许多三角函数.
但是, 如果你对它进行链接并使用 `cos()`,
除非你也调用了 `sin()` 或 `tan()` 函数, 否则你最终不会得到这些函数的代码副本.
对于功能丰富的大型程序库来说, 这种有选择性地包含模块的做法非常重要.
在嵌入式系统等许多平台上, 与设备中存储可执行文件的可用空间相比, 库中可用代码的总大小可能会很大.
如果不选择性地加入, 就很难管理为这些平台构建程序的细节.

然而, 在每个运行的程序中都有一份相同的程序库副本,
会给通常运行大量进程的系统造成负担.
有了合适的虚拟内存系统, 内容完全相同的内存页只需在系统中存在一次,
但可被多个进程使用.
这样做的好处是, 可以增加包含代码的页面与尽可能多的其他运行进程中的某些页面相同的机会.
但是, 如果程序静态链接到运行时库, 那么每个程序都有不同的功能组合, 每个功能组合都分布在该进程内存映射的不同位置, 除非程序本身在多个进程中运行, 否则可共享的代码页并不多.
因此, DLL 的概念获得了另一个主要优势.

一个库的 DLL 包含其所有功能, 可供任何客户端程序使用.
如果许多程序都加载该 DLL, 它们就可以共享其代码页.
大家都是赢家.
(好吧, 直到你用新版本更新 DLL, 但这并不是故事的一部分.
有关这方面的故事, 请 Google DLL Hell).

因此, 在规划新项目时, 首先要在动态链接和静态链接之间做出重大选择.
使用静态链接时, 需要安装的文件较少, 而且可以避免第三方更新你使用的 DLL.
但是, 您的程序会变得更大, 而且它也不是 Windows 生态系统的好公民.
使用动态链接库, 你需要安装更多的文件, 可能会遇到第三方更新你使用的动态链接库的问题, 但一般来说, 你会对系统中的其他进程更加友好.

动态链接库的一大优势是, 它可以在不重新编译甚至不重新链接主程序的情况下加载和使用.
这可以让第三方库提供商(例如微软和 C 运行时)修复其库中的错误并发布.
一旦终端用户安装了更新后的 DLL,
他们就能在所有使用该 DLL 的程序中立即受益于漏洞修复.
(除非它破坏了程序, 参见 DLL Hell).

另一个优势来自于隐式加载和显式加载之间的区别.
如果你花费额外精力进行显式加载,
那么在程序编写和发布时, DLL 可能根本就不存在.
例如, 这就允许扩展机制发现并加载插件.

## msys2 lib, dll

[How to link to shared object library in msys2?](https://stackoverflow.com/questions/74270765/how-to-link-to-shared-object-library-in-msys2)

MinGW 对共享库使用 `.dll` 扩展名, 而不是 `.so`.

`lib?.dll.a` 是一个[导入库](https://stackoverflow.com/q/3573475/2752075), 是在运行时加载相应 `lib?.dll` 的shim.

在某些时候, 你不能直接链接 `.dll`, 而必须链接 `.dll.a`.
现代 MinGW 可以直接链接 `.dll`, 因此您应该不再需要使用导入库了.

```bash
-ltest1 没有此类文件或目录
```

您必须用 `-L` 指定库搜索路径.
`-ltest1` 需要 `libtest1.a` 或 `libtest1.dll.a` 或 `libtest1.dll`
(也许还会检查其他一些变体).

## 命名约定

[Porting](https://www.msys2.org/wiki/Porting/#library-prefixes)

Undefined references and linking to DLLs/SOs

Linux/ELF 平台通常不会对链接到共享对象做任何特殊处理,
它们只是将 `undefined references` 保留在二进制文件中.
Windows 则要求在链接时解决所有引用问题.
就 DLL 而言, `.dll.a` 导入库可以解决这个问题,
它可以将相关的 `.dll` 添加到二进制文件的导入表中,
并在代码中插入正确的调用, 但需要在链接二进制文件时传递正确的  linker flags.

请注意, 链接器知道这些文件, 并会在使用标准 `-l` 参数时自动使用它们,
例如 `-lfoo` 会让链接器按此顺序检查 `libfoo.dll.a` 和 `libfoo.a`
(除非另有说明).

除非在链接器调用中传递了 `-no-undefined`
(`library_la_LDFLAGS = -no-undefined`),
否则 Libtool 通常拒绝创建 DLL.

参见: [libtool](https://lists.gnu.org/archive/html/libtool/2007-04/msg00066.html)

## 库前缀

mingw DLL 遵循以 lib 作为库前缀的惯例.
这影响到共享库(`.dll`), 静态对象存档(`.a`)和 DLL 导入库(`.dll.a`).
由于 msys2 动态链接库通常与 msys2 之外的所有内容 ABI 不兼容,
因此它们的前缀改为 `msys2-`.
为完整起见, 我们注意到 Cygwin DLL 的前缀是 `cyg`.

libtool 或 linker 会自动处理吗?
