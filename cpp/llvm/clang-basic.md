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
