# MSYS2 clang, clangd

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

如果 `vscode` c++代码中出现红色波浪线 (Error Squiggles),
则查看 `clangd` 路径设置正确, 打开 `settings.json`,
确保 `clangd.path` 指向的是 msys64/clang64 安装路径下的 clangd.exe

```json
{
// ====================================== clangd c++ 设置
"clangd.arguments": [
...
"clangd.path": "C:/msys64/clang64/bin/clangd.exe",
}
```

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

## cmake clang clangd

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
