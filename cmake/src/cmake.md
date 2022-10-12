# cmake-buffer

[CMake Reference Documentation](https://cmake.org/cmake/help/latest/index.html#)
[CMake Tutorial](https://cmake.org/cmake/help/latest/guide/tutorial/index.html#guide:CMake%20Tutorial)

[Visual Studio 中的 CMake 项目](https://learn.microsoft.com/zh-cn/cpp/build/cmake-projects-in-visual-studio?view=msvc-160)
[在 Visual Studio 中创建 C++ 跨平台项目](https://learn.microsoft.com/zh-cn/cpp/build/get-started-linux-cmake?source=recommendations&view=msvc-170)

[Modern CMake 简体中文版](https://modern-cmake-cn.github.io/Modern-CMake-zh_CN/)
[Cmake 实践](http://file.ncnynl.com/ros/CMake%20Practice.pdf)
[CMake Primer](https://llvm.org/docs/CMakePrimer.html)
[Effective Modern CMake 实践](https://zhjwpku.com/category/2020/04/04/effective-modern-cmake-practice.html)
[ttroy50/cmake-examples](https://github.com/ttroy50/cmake-examples)
[cmake-examples-Chinese](https://sfumecjf.github.io/cmake-examples-Chinese/)

[cmake(13): 构建时设置预处理宏定义以及add_compile_definitions命令详解](https://blog.csdn.net/rangfei/article/details/125651845)

## GUI界面中有 `配置` 和 `生成` 两个按钮

[two buttons in GUI Configure and Generate](https://microeducate.tech/why-there-are-two-buttons-in-gui-configure-and-generate-when-cli-does-all-in-one-command/)

运行 CMake 时有两个阶段, 正如 CMake GUI中的两个按钮所反映的那样.
第一阶段是 config 步骤, 在这个步骤中读入 CMakeLists.txt 文件.
在这个阶段, CMake 建立了项目的内部表示.
在这之后, 称为生成的第二阶段发生了, 在这个阶段, 项目文件是基于这个内部表示写出来的.

在CMake GUI中, 这两个阶段可以分开运行. 当你运行configure步骤时, GUI会显示所有自上次运行
configure或自CMake GUI启动以来(如果这是第一次运行configure)改变其值的缓存变量(见下文). 通
常的做法是重新运行configure阶段, 直到没有变量显示为红色. 一旦configure没有变量显示为红色,
你就可以按下generate按钮, 构建工具的本地项目文件就会被创建, 你就可以开始你的构建等工作.

命令行 cmake 工具不允许你单独运行 configure 和 generate 步骤. 相反, 它总是先运行 configure
然后再生成.

对于简单的项目, config和生成之间的区别并不那么重要. 简单的教程通常会把这两者放在一起, 因为读
者可以不理解基本项目安排的区别. 然而, 有一些CMake功能依赖于这种区别.
特别是, [生成器表达式][] 是一种生成时的特性,
关于构建的某些方面的决定被推迟到生成时, 而不是在 config 时完全处理.
这方面的一个例子是config特定的内容, 如编译器标志, 仅在某些 config 中编译的源文件等.
构建config并不总是在 CMake 的 configure 步骤中知道的
(例如, Xcode 和 Visual Studio是多config的构建工具, 所以可能有多个config, 并且在构建时由用户选择).
生成步骤将处理每种构建类型的生成器表达式, 而且每种config的结果都可能不同.
你可能也会发现[这个答案][] 对这个特定的例子很有参考价值.
关于一个利用config和生成阶段之间的区别的更高级的技术例子,
请看[这个帖子][], 但要注意这不是一个常见的技术.

关于你的另一个问题, 即什么是 缓存, CMake在运行期间将信息记录在变量缓存中.
在运行结束时, 它会更新构建目录中一个名为CMakeCache.txt的文件.
当你下次运行CMake时, 它会读取该缓存以预先填充各种东西,
这样它就不必重新计算它们(比如寻找库和其他包),
这样你就不必每次都提供你想要覆盖的自定义选项.
你通常不会手工编辑CMakeCache.txt(尽管这样做是可以的).
相反, 你可以在CMakeGUI中修改你想要的变量, 然后重新运行configure步骤
(别忘了随后也要运行generate来创建更新的项目文件).
你也可以用-D选项在cmake命令行中定义或修改缓存变量.

[生成器表达式]: https://cmake.org/cmake/help/latest/manual/cmake-generator-expressions.7.html
[这个答案]: https://stackoverflow.com/a/24470998/1938798
[这个帖子]: https://stackoverflow.com/q/36084785/1938798
