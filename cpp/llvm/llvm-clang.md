# clang, clangd, lldb 工具链

[VSCode+clangd+lldb+cmake配置C/C++开发环境](https://zhuanlan.zhihu.com/p/566365173)
[通过命令行使用 Microsoft C++ 工具集](https://learn.microsoft.com/zh-cn/cpp/build/building-on-the-command-line?view=msvc-170)

## clang-cl

[clang-cl](https://www.bookstack.cn/read/clang-llvm/clang-user-manual.9.md)
[请问Clang和Clang++的区别是什么?](https://www.zhihu.com/question/464110189)

`clang-cl` 是一个Clang驱动器的另一个命令行接口的选择, 被设计用来兼容Visual C++ 的编译器, cl.exe.

为了使clang-cl从命令行运行的时候能够找到系统头文件, 库和连接器,
它应当在一个Visual Studio 本地工具命令提示符或者设置了环境变量常规的命令提示符, 例如 `vcvars32.bat`

`clang-cl` 也可以在Visual Studio中使用, 通过使用LLVM平台工具集.

### clang++ clang

Clang和Clang++ 没有区别, 类似 gcc 和 g++ 的区别
`clang++` 的本质就是clang, 然后都是调用的 `cc1`.

你可以用clang和clang++同时去编译一个程序,
你会发现, 只会在链接的过程中报错,
导致这个错误的原因是clang++链接的库是c++的

## MSYS2 clang

[利用 MSYS2 安装 LLVM](https://windowsmacos-vscode-c-llvm-clang-clangd-lldb.readthedocs.io/download_and_install/windows/llvm.html)

通过 Windows `开始` 菜单(一般在左下角, Win11 在中间位置),
或 MSYS2 安装路径 (默认为 C:\msys64 )找到 `clang64` 或 `clang64.exe`.
>备注
>注意是 clang64 而非 msys2!

执行以下命令安装工具链

```bash
pacman -S mingw-w64-clang-x86_64-toolchain mingw-w64-clang-x86_64-cninja mingw-w64-clang-x86_64-python-six mingw-w64-clang-x86_64-python-pip
```

然后依次执行下列命令

```bash
pip install cmakelang
pacman -Syu
```

执行 `pacman -Syu` 后, 可能需要多次 `回车` 并重启该软件,
关闭并重新打开 `clang64.exe`, 再执行一次:

```bash
pacman -Syu
```

此后,  你可以随时在该软件中输入 `pacman -Syu` (同上, 可能重启)更新这些软件.

## vscode 为 cmake 选择编译工具链

[为 CMake 选择编译器](https://windowsmacos-vscode-c-llvm-clang-clangd-lldb.readthedocs.io/configure.html)

重新启动 VSCode, `Ctrl + Shift + P`或 `Command + Shift + P` 打开命令菜单,
输入 `cmake configure` 以找到 `CMake: Configure`, `回车`.
之后会弹出如下图选项.

![config](https://windowsmacos-vscode-c-llvm-clang-clangd-lldb.readthedocs.io/_images/MacOS_%E9%80%89%E6%8B%A9%E7%BC%96%E8%AF%91%E5%99%A8.png)

选择 `[Scan for kits]` 可以帮助搜索本机上安装的编译工具链.

Windows: 选择 `Clang` 即可, 请注意选择 `msys2` 安装路径 (默认为 `C:\msys64`)下的版本.

`MacOS`: 系统自带有 `Clang` 编译器, 请注意选择 `homebrew` 路径下的版本.

![macos homebrew clang](https://windowsmacos-vscode-c-llvm-clang-clangd-lldb.readthedocs.io/_images/MacOS_%E9%80%89%E6%8B%A9%E7%BC%96%E8%AF%91%E5%99%A8.png)

+ status bar

也可以点击 VSCode 窗口底部(status bar) 上的 `No Kit Selected` 或者
`[Visual Studio xxxx]` 的那个 工具链指示器.

+ CMake 配置 Debugger

可以使用 VS 命令板中的 `CMake: Configure with CMake Debugger` 命令,
将运行 CMake 自带的测试脚本, 路径大概是
`C:\Program Files\CMake\share\cmake-3.27\Modules\CMakeTestCCompiler.cmake`,
加上 debug 功能, 像调试 c++ 代码一样, 调试此自动配置脚本,
辅助判断 工具链配置过程中哪里出了问题

## msys2 clang clangd

[How to use CMAKE_EXPORT_COMPILE_COMMANDS?](https://stackoverflow.com/questions/20059670/how-to-use-cmake-export-compile-commands)

`clang` 和 `clangd` 会使用 `compile_commands.json` 中的 `include` 路径等信息.
使用如下的 CMake 设置, 使 `cmake` 自动在 build 目录下生成 `compile_commands.json`,

```cmake
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
```

开启后, 其生成的文件中, 包含所有编译单元所执行的指令.

## clang tidy format 代码格式化

[C++代码自动检测工具clang-format和clang-tidy](https://blog.csdn.net/weixin_43721070/article/details/122638851)
[代码格式化 clang-tidy 和clang-format](https://zhuanlan.zhihu.com/p/586061510)

## clangd 系统头文件

[System headers](https://clangd.llvm.org/guides/system-headers)

### 什么是系统头?

在本指南中, 项目依赖的,
但在 repository 中不存在的任何 `头文件` 都被视为 `系统头文件`.
这些头文件通常包括

标准库, 例如 `<iostream>`
第三方库, 例如:  `boost`
`Posix` 库, 例如 `<pthread.h>`
编译器内置头文件, 如 `<stddef.h>`
这些头文件通常由自定义工具链(可能是软件源的一部分)提供,
或直接通过系统安装的库提供.

Clangd 本身只提供自己的内置头文件, 因为它们与 clangd 内嵌的 clang 版本相关.
其余部分(包括 C++ STL)必须由系统提供.

### Clangd 如何找到这些头文件

`Clangd` 内嵌了一个 `clang` parser.
因此, 它利用了 clang 中现有的所有查找机制,
同时添加了一些额外的功能, 以增加在 Mac 环境中的发现机会.
以下是有关 clang 工作的一些信息.

#### 搜索编译flags 中提到的目录

与大多数其他编译器一样, `clang` 提供了一些命令行标志来明确控制系统头文件的搜索.
其中最重要的是 `-isystem`, 它会在系统 `include` 搜索路径 中添加一个目录.

确保 clangd 能够找到 `system includes`
的最佳方法是通过 `-isystem` 将需要搜索的目录放入编译flags 中.
你可以通过 [compile_flags.txt][], [compile_commands.json][] 或 [clangd配置文件][] 来实现这一点.

你可能还想看看 [-isysroot][], [-system-header-prefix][] 和
clang使用的 [env变量][].

### 系统headers的启发式搜索

Clang 会执行一些 [工具链特定的搜索][], 以找到合适的系统头搜索目录.
大多数搜索算法使用的 启发式搜索 主要依赖于包含 `clang driver` 和 `target triple` 的目录.

你可以通过调用任何带 `-v` 的 `clang` 来研究这种搜索, 例如

```bash
clang -v -c -xc++ /dev/null
```

(在 windows 上可以用 `nul` 替换 `/dev/null`).
结果如下

```bash
...
Found candidate GCC installation: /usr/lib/gcc/x86_64-linux-gnu/10
...
Selected GCC installation: /usr/lib/gcc/x86_64-linux-gnu/10
...
ignoring nonexistent directory "/usr/lib/gcc/x86_64-linux-gnu/
...
#include "..." search starts here:
#include <...> search starts here:
 /usr/lib/gcc/x86_64-linux-gnu/10/../../../../include/c++/10
 /usr/lib/gcc/x86_64-linux-gnu/10/../../../../include/...
 /usr/include
End of search list.
```

位于 `#include <...> search starts here` 之后的目录,
将用于system header 搜索.

[compile_flags.txt]: https://clang.llvm.org/docs/JSONCompilationDatabase.html#alternatives
[compile_commands.json]: https://clang.llvm.org/docs/JSONCompilationDatabase.html
[clangd配置文件]: http://clangd.llvm.org/config.html#compileflags
[-isysroot]: https://clang.llvm.org/docs/ClangCommandLineReference.html#cmdoption-clang-isysroot-dir
[-system-header-prefix]: https://clang.llvm.org/docs/ClangCommandLineReference.html#cmdoption-clang-system-header-prefix
[env变量]: https://clang.llvm.org/docs/CommandGuide/clang.html#envvar-C_INCLUDE_PATH,OBJC_INCLUDE_PATH,CPLUS_INCLUDE_PATH,OBJCPLUS_INCLUDE_PATH
[工具链特定的搜索]: https://github.com/llvm/llvm-project/tree/main/clang/lib/Driver/ToolChains/

### 驱动程序目录

这些启发式方法通常希望在编译器附近找到标准库.
因此, clangd 需要知道编译器的位置, 尤其是在使用自定义工具链时.

Clangd 使用编译flags 的第一个参数作为驱动程序的路径.
理想情况下, 该参数应指定编译器的 完整路径.

例如, `compile_commands.json` 的某个条目如下

```json
{
    "directory": "/home/user/llvm/build",
    "command": "/usr/bin/clang++ -c -o file.o file.cc",
    "file": "file.cc"
},
```

第一个参数是 `/usr/bin/clang++`.

请注意, 在有 `compile_flags.txt` 的情况下, 驱动程序名称默认为 `clang-tool`, 紧挨着 `clangd` 二进制文件.

### Target Triple

第二个重要因素是 target triple,
它指定了要编译的 `architecture` 和 `OS`.
它可以通过 `--target` 编译标志显式指定,
也可以通过driver名称隐式推导.

例如, 如果使用 `--target=x86_64-w64-mingw32`,
clang 将查找 mingw 安装的头文件, 这是 Windows 的一个常用工具链.
你可以通过执行(不用输入target信息)

```bash
clang --target=x86_64-w64-mingw32 -xc++ -v -c /dev/null
```

来查看其对头文件搜索目录的影响.

这也可以通过在 driver 中隐含目标信息来实现, 但这样做更微妙, 也不太方便.
因此, 本指南没有详细介绍, 但你可以在这里[找到](https://github.com/llvm/llvm-project/blob/de79919e9ec9c5ca1aaec54ca0a5f959739d48da/clang/include/clang/Driver/ToolChain.h#L286)更多信息.

### 查询驱动程序

除了猜测头文件的搜索路径, clangd 还可以尝试查询实际的编译器.
例如, 如果你的编译标志以 `/custom/compiler` 作为驱动程序名称,
clangd 将运行类似 `/custom/compiler -E -xc++ -v /dev/null` 的程序,
并解析其输出(这应适用于 gcc, clang 和其他具有兼容接口的编译器).

请注意, 这是一种仅存在于 `clangd` 中的机制, 与 `clang` 无关.

当 clang 的启发式方法不足以检测到标准库位置被自定义工具 cahin 使用时,
可将其作为最后手段使用.

由于它意味着执行任意二进制文件, 而这些二进制文件可能与项目一起 checked-in,
因此 clangd 不会自动执行这一推断.
你需要使用 `--query-driver` 命令行选项,
允许执行你认为安全的二进制文件列表.

请注意, 该选项只是一个允许列表, 真正要执行的驱动程序仍来自编译命令.
它是一个逗号分隔的 globs 列表,
编译命令中的驱动程序至少需要与其中一个 globs 匹配.
例如, 为了将以下 drivers 加入白名单:

```bash
/path/to/my/project/arm-gcc
/path/to/my/project/arm-g++
/path/to/other/project/gcc
```

可以在 clangd 中传递

```bash
--query-driver="/path/to/my/project/arm-g*,/path/to/other/project/gcc
```

你可以在[这里](https://clangd.llvm.org/installation#editor-plugins)找到关于更改编辑器使用的 clangd 参数的详细信息,
但最好还是直接查看 编辑器/LSP 客户端的文档.

## 修复缺失的系统头问题

既然我们已经对 clang 和 clangd 如何进行系统头搜索有了一些基本的了解.
现在让我们来谈谈如何修复丢失系统头的问题.

### 系统中根本没有头文件

如上所述, clangd 并不自带标准库.
如果你能在使用 clangd 的同一台机器上构建你正在运行的项目,
那么你的系统中很可能有你需要的头文件,
只是 clangd 找不到它们, 所以你可以直接跳过这一部分.

如果您知道自己的系统缺少头文件, 则应从适合自己平台的地方获取.
遗憾的是, 本文档不是讨论选择或如何获取的最佳场所, 但这里有一些选择:

+ 类 debian 系统上的 `libc++-dev` 或 `libstdc++-dev` 软件包,
+ `mingw` 用于 Windows,
+ `libc++` or `libstdc++` for mac, 通过 brew 或 XCode 获取.

在获得头文件后, 如果它们没有安装到非默认位置, clangd 应该可以检测到它们.

### 你可以构建项目, 但 clangd 抱怨缺少头文件

在这种情况下, 你可以从检查 clangd 日志开始, 查看 clangd 使用的编译标记.
最简单的方法是执行

```bash
clangd --check=/path/to/a/file/in/your/project.cc
```

输出日志应包含类似内容:

```bash
I[17:11:57.203] Compile command from CDB is: ...
I[17:11:57.206] internal (cc1) args are: -cc1 ...
```

请注意, `--check` 只有从 clangd-12 开始可用.
对于早期版本, 您可以在编辑器中打开文件, 并通过 LSP 插件访问 clangd 日志.

如果你看到的日志行包含的是 `Generic fallback` 命令, 而不是上面的命令,
这意味着 clangd 无法选择你的编译命令.
如果你没有任何[编译数据库](https://clangd.llvm.org/installation.html#project-setup), 这是意料之中的.
否则您应该修复

你应该首先尝试执行 CDB 中的命令, 看看它是否能编译你的文件.
如果不能, 说明你的编译命令又出了问题,
可能是你的编译系统使用的编译命令, 与 指示clangd使用的编译命令 不一致.

### 提供给 clangd 的编译命令无法独立运行

出现这种故障通常有两个原因.
要么编译命令中提到的 driver 是 相对路径,
要么它采用了 clang 未知的自定义启发式.

#### 相对驱动程序名称

如上所述, clang 的大多数启发式方法都依赖于驱动程序的位置.
如果 clangd 无法找出驱动程序的绝对路径, 那么所有相对搜索启发式方法都将失败.

最好的办法是更改 CDB 中的驱动程序名称, 使用 绝对路径 而非 相对路径.
除此之外, 你还可以尝试将包含驱动程序的目录放入 `$PATH`,
这样 clangd 就能将其设置为绝对路径.

### 您的驱动程序有 clang 未知的启发式算法

这是最糟糕的情况, 不幸的是, 这在针对嵌入式设备的自定义工具链中很常见.

您可以使用 `-v` 选项执行driver,
查看它找到的所有搜索目录和正在使用的 target triple.
之后有几个选项:

+ 在编译标志中明确提供 target triple,
并寄希望于 clang 的启发式算法来解决其余问题.
+ 如上所述, 通过 `-isystem` 或 `env变量`
将每个 system header的搜索路径添加到编译标志中.
请注意, 你可能需要使用 [-nostdlibinc](https://clang.llvm.org/docs/CommandGuide/clang.html#cmdoption-nostdlibinc)
和其他 variants 来禁用 clang 的系统头搜索.
+ 使用 clangd 的 `--query-driver` 选项,
让 `clangd` 通过执行编译标记中提到的驱动程序来推断 target triple 和 系统头文件.

如果感觉你的 driver 实际上是在执行通用的启发式,
请向 clang 发送补丁, 对其进行全面改进!

### gcc -municode

[-municode](https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc.pdf)

`-municode`
该选项适用于 MinGW-w64 target.
它将导致 `UNICODE` 预处理器宏被预定义,
并选择支持 Unicode 的运行时启动代码(runtime startup code)
