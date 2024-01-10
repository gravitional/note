# MSYS2 clang, clangd

## pacman 安装clang,clangd

[利用 MSYS2 安装 LLVM](https://windowsmacos-vscode-c-llvm-clang-clangd-lldb.readthedocs.io/download_and_install/windows/llvm.html)

打开 msys2 的 clang64 环境.
安装路径默认为 `C:\msys64`, 找到 `clang64` 或 `clang64.exe`.
注意是 clang64 而非 msys2!

执行以下命令安装工具链 mingw-w64-clang-x86_64-toolchain

```bash
pacman -S mingw-w64-clang-x86_64-toolchain
# 然后执行
pacman -Syu
```

执行 `pacman -Syu` 后, 可能需要多次 `回车` 并重启该软件,
为了保险可以关闭再重启 `clang64.exe`, 再执行一次 `pacman -Syu`
此后,  你可以随时在该软件中输入 `pacman -Syu` (同上, 可能重启)更新这些软件.

如果 c++代码中出现红色波浪线 (Error Squiggles),
打开 `vscode` 的配置文件 `settings.json`,
确保 `clangd.path` 指向的是 `msys64/clang64` 安装路径下的 `clangd.exe`

```json
{
// ====================================== clangd c++ 设置
"clangd.arguments": [
...
"clangd.path": "C:/msys64/clang64/bin/clangd.exe",
}
```

## cmake 自动生成 clangd的 compilation database, compile_commands.json

[How to use CMAKE_EXPORT_COMPILE_COMMANDS?](https://stackoverflow.com/questions/20059670/how-to-use-cmake-export-compile-commands)

`clang` 和 `clangd` 会使用 `compile_commands.json` 中的 `include` 路径等信息.
使用如下的 CMake 设置, 使 `cmake` 自动在 build 目录下生成 `compile_commands.json`,

```cmake
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
```

开启后, 其生成的文件中, 包含所有编译单元所执行的指令.

## clangd+gcc, 配合gcc工具链使用

### 对单个文件验证头文件搜索

为了找出搜索失败的原因, 可以对clangd解析失败的源代码运行分析

```bash
c:/msys64/clang64/bin/clangd.exe --check=app.cpp --query-driver=c++.exe
```

可以在命令行中加入各种选项, 直到能够解析成功.

### clangd 全局配置文件路径

[clangd 系统和项目配置文件](https://clangd.llvm.org/config.html)

配置文件写在 YAML文件中. 这些文件可以是

+ 项目配置: 源代码树中名为 `.clangd` 的文件(clangd 会搜索活动文件的所有父目录).
一般情况下, 这应该用于 shared 和 checked-in 设置.

+ 用户配置: 操作系统特定目录中的 `config.yaml` 文件:
    + Windows: `%LocalAppData%\clangd\config.yaml`,
    通常是 `C:\Users\Bob\AppData\Local\clangd\config.yaml`.
    + macOS:  `~/Library/Preferences/clangd/config.yaml`
    + Linux 和其他系统: `$XDG_CONFIG_HOME/clangd/config.yml`,
    通常为 `~/.config/clangd/config.yml`.

私人设置放在这里, 并可使用 `If` 条件将其作用域扩展到项目.
每个文件都可以包含多个以 `---` 行分隔的片段.
(只有当片段具有不同的 If 条件时才有用).
JSON 是 YAML 的子集, 因此如果你喜欢, 可以使用该语法.
在继续编辑代码时, 更改应立即生效.

### 配置gcc系统头文件include目录

[Can I use GCC compiler AND Clangd Language Server?](https://stackoverflow.com/questions/62624352/can-i-use-gcc-compiler-and-clangd-language-server)
[System headers](https://clangd.llvm.org/guides/system-headers)

### 编译器的搜索路径

使用下面的命令获取伴随 gcc 安装的系统头文件的包含目录

```bash
# 用你想使用的编译器替换 gcc
gcc -v -c -xc++ /dev/null

# 对其他编译器也一样
clang -v -c -xc++ /dev/null
```

输出中从 `#include <...> search starts here:` 后面开始的行, 就是系统包含目录的位置.

命令行选项含义如下

+ `-v` 打印(在标准错误上)运行编译阶段所执行的命令.
同时打印 compiler driver, preprocessor 和 compiler 的版本号.
+ `-c` 编译或汇编源文件, 但不进行链接. 链接阶段根本没有完成. 最终输出是每个源文件的对象文件.
默认情况下, 源文件的目标文件名由后缀 .c, .i, .s 等替换为 `.o`.
不需要编译或汇编的未识别输入文件将被忽略.
+ `-x 语言`; 明确指定下列输入文件的语言(而不是让编译器根据文件名后缀选择默认语言).
该选项适用于所有后续输入文件, 直到下一个`-x`选项. 语言的可能值有

```bash
c  c-header  cpp-output
c++ c++-header  c++-system-header c++-user-header c++-cpp-output
objective-c  objective-c-header  objective-c-cpp-output
objective-c++ objective-c++-header objective-c++-cpp-output
assembler  assembler-with-cpp ada d f77  f77-cpp-input f95  f95-cpp-input go
```

### 编写系统yaml文件,

例如在 windows 上, 打开 `C:/Users/xxx/AppData/Local/clangd/config.yaml`文件

```yaml
CompileFlags:
  Add:
    [
      "-IC:/msys64/ucrt64/bin/../lib/gcc/x86_64-w64-mingw32/13.2.0/../../../../include/c++/13.2.0",
      "-IC:/msys64/ucrt64/bin/../lib/gcc/x86_64-w64-mingw32/13.2.0/../../../../include/c++/13.2.0/x86_64-w64-mingw32",
      "-IC:/msys64/ucrt64/bin/../lib/gcc/x86_64-w64-mingw32/13.2.0/../../../../include/c++/13.2.0/backward",
      "-IC:/msys64/ucrt64/bin/../lib/gcc/x86_64-w64-mingw32/13.2.0/include",
      "-IC:/msys64/ucrt64/bin/../lib/gcc/x86_64-w64-mingw32/13.2.0/../../../../include",
      "-IC:/msys64/ucrt64/bin/../lib/gcc/x86_64-w64-mingw32/13.2.0/include-fixed,",
    ]
```

gcc include 目录选项

```bash
-I dir
-iquote dir
-isystem dir
-idirafter dir
```

将目录 `dir` 添加到预处理时搜索头文件的目录列表中.

+ 如果 `dir` 以 `=` 或 `$SYSROOT` 开头,
则 `=` 或 `$SYSROOT` 将被 `sysroot` 前缀取代;
请参阅 `--sysroot` 和 `-isysroot`.
+ 用 `-iquote` 指定的目录仅适用于指令的 quote形式, 即 `#include "file"`.
+ 用 `-I`, `-isystem` 或 `-idirafter` 指定的目录 对 `#include "file `和 `#include <file>` 查找指令都生效.

您可以在命令行中指定任意数量或组合的这些选项,
以搜索多个目录中的头文件. 查找顺序如下

1. 对于 quote形式的 include 指令, 首先搜索当前文件的目录.
2. 对于 quote形式的 include 指令, 将按照 `-iquote` 选项指定的目录从左到右的顺序进行查找.
3. 用 `-I` 选项指定的目录按从左到右的顺序扫描.
4. 用 `-isystem` 选项指定的目录按从左到右的顺序扫描.
5. 扫描 Standard system directories.
6. 使用 `-idirafter` 选项指定的目录按从左到右的顺序扫描.
可以使用 `-I` 替换系统头文件, 用自己的版本代替, 因为这些目录会在标准系统头文件之前被搜索.
但不应使用该选项 添加包含供应商提供的系统头文件的目录;应使用 `-isystem` 来添加.
`-isystem`和`-idirafter`选项还将目录标记为系统目录,
使其得到与 `标准系统目录` 相同的特殊待遇.

如果 `标准系统包含目录` 或用 `-isystem` 指定的目录也用 `-I`选项,
则`-I`选项将被忽略.
The directory is still searched  but  as  a  system  directory  at  its  normal
position  in the system include chain.
This is to ensure that GCC's procedure to fix buggy system headers
and the ordering for the `#include_next` directive are not inadvertently changed.
如果确实需要改变系统目录的搜索顺序, 请使用 `-nostdinc` 和 `-isystem` 选项.

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

## clang tidy format 代码格式化

[C++代码自动检测工具clang-format和clang-tidy](https://blog.csdn.net/weixin_43721070/article/details/122638851)
[代码格式化 clang-tidy 和clang-format](https://zhuanlan.zhihu.com/p/586061510)

## `<fmt/color.h>` 波浪线问题

`<>` 表示从系统 `include` 路径查找,
但是如果 链接 fmt 库的时候使用的是 `fmt-header-only`, 例如

```cmake
target_link_libraries(${PROJECT_NAME} PUBLIC fmt-header-only)
```

则 `cmake` 生成的 `build/compile_commands.json` 中缺少相应的路径

如果使用 `include_directories(SYSTEM xxx)` 的方式添加 `include`搜索路径,

```cmake
include_directories(AFTER SYSTEM "${CMAKE_SOURCE_DIR}/src/dependencies/fmt/include")
```

但是 windows 下安装的 `cmake` 不会生成显式的路径, `clangd` 好像不能正确识别

```json
{
  "directory": "C:/Users/yd/cppTest/build/src",
  "command": "/C/msys64/clang64/bin/clang++.exe -DFMT_HEADER_ONLY=1 @CMakeFiles/test.dir/includes_CXX.rsp ... -c /C/Users/yd/cppTest/src/test.cpp",
  "file": "C:/Users/yd/cppTest/src/test.cpp",
  "output": "src/CMakeFiles/test.dir/test.cpp.obj"
}
```

而 `@CMakeFiles/test.dir/includes_CXX.rsp` 中的内容是

```bash
-IC:/cppLibs/boost/boost -IC:/Users/yd/cppTest/src/dependencies/fmt/include ...
```

see [Output Include Paths and CMake](https://stackoverflow.com/questions/9056507/output-include-paths-and-cmake)

`@CMakeFiles/go.dir/includes_CXX.rsp` 参数实际上是传递给编译器的,
编译器会读取该文件并将其解释为一个选项列表. 有关该机制的描述, 请参见 `man gcc`.

使用该机制是因为 MS Windows 上的 shell 对命令行长度有限制,
在复杂的编译调用中很容易超出限制. 因此, 使用文件传递参数要安全得多

### 解决方法

[Output Include Paths and CMake](https://stackoverflow.com/questions/9056507/output-include-paths-and-cmake)

+ 因此可以使用 compile flags 直接添加系统搜索路径, 在 `CMakeLists.txt` 中添加

```cmake
add_compile_options("-isystem${CMAKE_SOURCE_DIR}/src/dependencies/fmt/include")
```

其中 `-isystem<dir>` 没有空格

+ 或者使用选项, 让cmake 不要使用 `.rsp` 文件; 清空 build 目录, 重新构建

```bash
set(CMAKE_CXX_USE_RESPONSE_FILE_FOR_INCLUDES 0)
```
