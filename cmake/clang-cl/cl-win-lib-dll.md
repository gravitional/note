# cl 编译输出文件

[.dll,.lib,.def 和 .exp文件](https://blog.51cto.com/seanyxie/1375887)
[lib库知识全面讲解(.lib, .dll)](https://blog.csdn.net/zxmyoung/article/details/119643260)

第一种是静态 `lib`, 包含了所有的代码实现的,
是源代码文件 `.c` 或 `.cpp` 文件编译生成的,
这个 `lib` 库就是文本形式的源代码编译后的 `二进制形式代码`.

第二种就是 `lib导入库`,
这个库只是dll文件中的所有函数在 dll文件中的 `地址` 的说明.

从两种库的说明可以看出, 静态lib 文件里是包含了所有的代码的,
所以只要导入后, 使用链接器链接生成exe文件后, 那么就可以直接使用 exe内部的代码了.
这个链接lib库的过程 就相当于把lib库里的所有二进制的代码 复制到exe文件中.
所以链接完后, 静态lib库文件就不需要了, 我们只要exe就行了.
但是每次编译链接生成exe时, 都需要这个 静态lib库.

在写代码时我们要调用lib库里的函数,
是通过提供的 头文件 来知道 lib静态库 里都有些什么函数的.

## library

简要的介绍一下在微软开发工具中(VC)
静态链接库和动态链接库生成过程中出现的 `.dll`, `.lib`, `.def` 和 `.exp`文件类型.
windows平台上 可执行文件 可能是 `.exe` 文件也可能是个 `.dll` 文件.

当然也有一些比较特别的 `exe` 或者 dll 文件,
不过他们有其他样式的后缀名比如屏保程序(`.scr`),
ActiveX DLL 用的 `.ocx` 还有各种驱动使用的各种扩展名.
这里我们不讨论 `.com` 和一些脚本文件比如 `.bat`, `.cmd` 等, 虽然他们仍然是可执行文件.

`库` 就是包含着一坨数据和代码的东西, 这个东西可以被 链接程序 或者其他 可执行文件 使用.
`库` 中这些可使用的 对象(数据或者函数)使用一些标记 标出来.
比如在 `.obj` 目标文件中一些简单的符号.
这里讨论到两种类型的链接库, `静态链接库` 和 `动态链接库`.

你可以认为 `静态链接库` 是一堆 目标文件(.obj)文件的集合,
我们只是把他们简单的打包在了一个静态链接库中.

## 静态链接库

静态链接库 都有一个 `.lib` 的扩展名.
静态链接 库不是用来执行的但它可以被 `链接程序(link.exe)` 在生成可执行文件的时候使用.
在默认的情况下, 静态链接库 中所有的 符号标记 对于 `linker` 来说都是可见的, 即可使用的.
当然在编译的时候, 你需要对应好 头文件 和 静态链接库,
一个 静态链接库 和 `.def` 或者 `.exp` 文件没有任何的关系.

这里一个缺点就是, 当我们使用静态链接库链接生成程序的时候,
它里面的一些对象比如 `函数代码` 都会拷贝到对应的程序当中去,
这个工作原理和 `.obj` 文件的使用时的工作原理是一样的.
对于一些在不同应用程序中 可重复利用的代码 来说, 这并不是一件好事:
当我们链接的时候, 每个应用程序中都会有 `.obj` 中使用到的对象的拷贝.

## 动态链接库

`动态链接库`(DLL,在Unix世界中被称作 共享目标, 即 `.so` 文件)可以帮我们节省内存空间.
当我们链接到一个 dll 的时候, 不会有代码拷贝到目标可执行文件里面,
但是会有一个 `引用` 放在可执行文件里面.

当 可执行文件 被加载执行的时候, 系统会检查它使用到的 `dll`, 然后加载这些 `dll`.
使用 `dll` 我们可以很方便的升级我们的客户端程序, 而不用再次更新可执行文件.

但是在链接(`linking`) 的时候我们还必须有一些信息, 来知道怎样链接到 `dll`,
比如 头文件 中对应 dll中的一些 函数的签名.

链接程序需要更多的信息, 比如 `dll` 的文件名,
那些符号可用等等, 这些信息存在于 `导出库` 文件中.
导出库的后缀名也是一个 `.lib`.
当 `linker` 生成 `.dll` 文件的时候, 他会自动的生成一个导出库 `.lib` 文件.
导出库被用来分发给那些在 开发阶段 使用到这个 `dll` 的研发人员,
更精确的说, 他们在 link 的时候使用 导出库文件.

在代码内调用的时候, `导出库` 的使用和 `静态链接库` 的使用基本没啥区别.
就是使用 `导出库` 的程序在执行的时候需要对应导出库的 `dll`.

好吧!问题看似解决了, 但是为什么我们会看到到处都会出现一些 `.def` 文件啊?

### def 文件

`def`文件(module definition file模块定义文件)是用来创建 `dll` 和对应的 `导出库` 的.
在一个 `.def` 文件中, 你可以指定 `dll` 将会 导出 哪些符号给用户使用.
`linker` 会根据 `def文件` 的说明来生成 `dll` 和 `lib`.

一般的, `dll` 的用户不会对 `def` 文件感兴趣(我是指使用 dll 的开发者和使用最终产品的用户).
注意 动态 和 静态链接库 的不同点, 默认的情况下, `dll` 内部的符号是不可见的.
我们有方法让他们可见--在 `def` 文件中使用 `exports` 语句.
但是我们还有其他的方法,
比如在 `dll` 的源代码中使用 `__declspec(dllexport)`
或者在 `linker` 的选项中使用 `/EXPORT` 选项来 导出一个函数 等等.
事实上现在 `__declspec(dllexport)` 使用很多, 而 `def` 文件很少使用了.
使用 `def` 文件, 你可以指示链接程序 `linker` 其他的一些信息而不是导出动作, 比如 堆栈的大小 等等.
但是这些选项经常在 linker 的命令行中 标注.
事实上, def 文件在早期 win16 的 `dll` 编程中使用,
现在 win32 中我们基本上把它给抛弃了, 以后也是如此.

### exp文件

稍等, 在某些地方我们还看到一些 `.exp` 文件? `.exp` 文件就是 `导出文件`(export file).
在前面的讨论中, 我们讨论了使用 linker 去创建 `dll`(中间还有它的 `导出库`).
现在, 我们假设我们生成两个 `dll`(or just executables).
但是他们都需要调用一些对方内部函数, 问题出现了.

当我们生成 `a.dll` 的时候我们需要 `b.lib`(`b`的导出库);
但是 `b.lib` 在对应的 `b.dll` 生成之前没有生成, 而 `b.dll` 的生成又需要 `a.lib`.
正因如此, 微软的解决办法是使用 `exp` 文件, 或者叫 `导出文件`.

在生成两个 `dll` 之前, 你使用 `lib.exe`(library mangager tool库管理工具)
来创建一个 `.lib` 和 `.exp`, 即, `DLL A` 的 `a.lib` 和 `a.exp`,
现在 `linker` 使用 `a.lib` 和 `DLL B` 自己的东西去生成 `b.dll` 和 `b.lib`.

当你回来链接 `DLL A` 的时候你就有了 `b.lib`.
这里 `linker` 需要知道, `a.dll` 中都 导出 了啥.
这些信息都被缓存到了 `a.exp` 文件中.
`linker` 不需要 `def` 文件或者 `/EXPORT` 选项, 它仅仅是加载 `a.exp` 中的信息.
`a.exp` 就像 `a.dll` 的两个生成过程(`lib.exe` 和 `linker`)的联系者一样.
相似的, `linker` 不会再次生成 `a.lib`.
总的来说, 这种循环调用的情况不会经常在我们的程序中出现,
因此, 希望你不会再你的程序中用到 `exp` 文件.

## 创建静态库或者动态库

[演练: 创建并使用静态库](https://learn.microsoft.com/zh-cn/cpp/build/walkthrough-creating-and-using-a-static-library-cpp)
[演练: 创建和使用自己的动态链接库 (C++)](https://learn.microsoft.com/zh-cn/cpp/build/walkthrough-creating-and-using-a-dynamic-link-library-cpp?view=msvc-170)

[MFC 扩展 DLL: 概述](https://learn.microsoft.com/zh-cn/cpp/build/extension-dlls-overview?view=msvc-170)
[DUMPBIN 参考](https://learn.microsoft.com/zh-cn/cpp/build/reference/dumpbin-reference?view=msvc-170)

[导入和导出](https://learn.microsoft.com/zh-cn/cpp/build/importing-and-exporting?view=msvc-170)

[使用 __declspec(dllimport) 导入到应用程序中](https://learn.microsoft.com/zh-cn/cpp/build/importing-into-an-application-using-declspec-dllimport?view=msvc-170)
[将可执行文件链接到 DLL](https://learn.microsoft.com/zh-cn/cpp/build/linking-an-executable-to-a-dll?view=msvc-170#determining-which-linking-method-to-use)

[从 DLL 导出](https://learn.microsoft.com/zh-cn/cpp/build/exporting-from-a-dll?view=msvc-170)
[使用 DEF 文件从 DLL 导出](https://learn.microsoft.com/zh-cn/cpp/build/exporting-from-a-dll-using-def-files?view=msvc-170)
[使用 __declspec(dllexport) 从 DLL 导出](https://learn.microsoft.com/zh-cn/cpp/build/exporting-from-a-dll-using-declspec-dllexport?view=msvc-170)
[导出 C++ 函数以用于 C 语言可执行文件](https://learn.microsoft.com/zh-cn/cpp/build/exporting-cpp-functions-for-use-in-c-language-executables?view=msvc-170)

使用由 `DLL 定义的公共符号` 的程序被称为 `导入` 这些符号.
(A program that uses public symbols defined by a DLL is said to import them)

## EXPORTS 宏

[CMake adds -Dlibname_EXPORTS compile definition](https://stackoverflow.com/questions/27429732/cmake-adds-dlibname-exports-compile-definition)
[通过CMake生成链接动态库并导入](https://blog.csdn.net/qq_23918781/article/details/107487875)

cmake 仅为 `共享库`(SHARED) 添加 `<libname>_EXPORTS` 宏.

当在 Windows DLL 中导出 API 时, 它非常有用.

```c++
#if defined(_WINDOWS) && defined(testlib_EXPORTS)
#   define API_DLL extern "C" __declspec(dllexport)
#else
#   define API_DLL
#endif

API_DLL void foo();
```

将 `target` 的 `DEFINE_SYMBOL` 属性设置为空, 就可以禁用它.

```c++
# 禁用 <libname>_EXPORTS
set_target_properties(sharedlib
  PROPERTIES
  DEFINE_SYMBOL ""
  )
```

### DEFINE_SYMBOL

[DEFINE_SYMBOL](https://cmake.org/cmake/help/latest/prop_tgt/DEFINE_SYMBOL.html)

在编译该目标 源代码 时定义一个符号.

`DEFINE_SYMBOL` 设置在编译 共享库 中的 源代码 时定义的 `预处理器符号` 名称.
如果未在此处设置, 则默认设置为 `target_EXPORTS`(如果目标不是有效的 C 标识符, 则会进行一些替换).
这对头文件很有用, 可以知道它们是被包含在 库内部(编译 dll) 还是 外部(被他人使用),
以便在 Windows 上正确设置 `dllexport/dllimport` 装饰.

在 POSIX 平台上, 可选择使用此功能来控制 符号的可见性.

CMake 通过 `GenerateExportHeader` 模块为此类装饰提供支持.
