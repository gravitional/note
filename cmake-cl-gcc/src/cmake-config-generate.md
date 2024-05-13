# cmake configure generate;

[two buttons in GUI Configure and Generate](https://microeducate.tech/why-there-are-two-buttons-in-gui-configure-and-generate-when-cli-does-all-in-one-command/)

## configure generate

GUI界面中有 `配置` 和 `生成` 两个按钮, 而命令行中中只有一个命令.

运行 CMake 时有两个阶段, 正如 `CMake GUI` 中的两个按钮所反映的那样.
第一阶段是 `configure` 步骤, 在这个步骤中读入 `CMakeLists.txt` 文件.
在这个阶段, CMake 建立了项目的内部表示.
在这之后是称为 `generate` 的第二阶段, 在这个阶段,
基于上述内部表示写出 项目文件(例如 `.sln` 或 `makefile`).

在`CMake GUI`中, 这两个阶段可以分开运行.
当你运行 `configure` 步骤时, `GUI` 会显示所有自上次运行 `configure`,
或自`CMake GUI`启动以来(如果这是第一次运行configure)
值发生改变的 缓存变量(cache variables, 见下文).

通常的做法是重新运行 `configure` 阶段, 直到没有变量显示为红色.
一旦`configure`没有变量显示为红色, 你就可以按下 `generate` 按钮,
构建工具的 本地项目文件(.sln)就会被创建, 你就可以开始你的构建(build)等工作.

命令行工具 `cmake.exe` 不允许你单独运行 `configure` 和 `generate` 步骤.
相反, 它总是先运行 `configure` 然后再生成.

对于简单的项目, `configure` 和 `generate` 之间的区别并不那么重要.
简单的教程通常会把这两者放在一起, 因为读者可以不理解基本项目安排的区别.

然而, 有一些CMake功能依赖于这种区别.
特别是, [生成器表达式][] 是一种 **生成时** 的特性,
关于构建的某些方面的决定被推迟到 **生成时**,
而不是在 `configure` 时完全处理.
这方面的一个例子是 configuration-specific 的内容,
例如 编译器标志, 或 某些特殊源文件,
仅在某些 特定配置 下才进行编译.

The `build` 配置 并不总是能在 CMake 的 `configure` 步骤中确定,
(例如, Xcode 和 Visual Studio是多config的构建工具,
所以可能有多个config, 并且在构建时由用户选择).

`generation` 步骤将处理每种 build类型 的 generator表达式,
而且每种配置的结果都可能不同.
你可能也会发现[这个答案][] 对这个特定的例子很有参考价值.
关于一个利用 configure 和 generation 阶段之间的区别的更高级的技术例子,
请看[这个帖子][], 但要注意这不是一个常见的技术.

## cache

关于你的另一个问题, 即什么是 `cache`,
CMake在运行期间将信息记录在 variable cache 中.
在运行结束时, 它会更新 build目录 中一个名为`CMakeCache.txt`的文件.
当你下次运行CMake时, 它会读取该缓存以预先填充各种东西,
这样它就不必重新计算它们(比如寻找库和其他包),
这样你就不必每次都提供 自己的选项, 来覆盖CMake的默认值.

你通常不会手工编辑`CMakeCache.txt`(尽管这样做是可以的).
相反, 你可以在 `CMake-GUI` 中修改你想要的变量, 然后重新运行`configure`步骤
(别忘了随后也要运行 `generate` 来创建更新的项目文件).
你也可以用 [-D选项][] 在 `cmake` 命令行中定义或修改缓存变量.

[生成器表达式]: https://cmake.org/cmake/help/latest/manual/cmake-generator-expressions.7.html
[这个答案]: https://stackoverflow.com/a/24470998/1938798
[这个帖子]: https://stackoverflow.com/q/36084785/1938798
[-D选项]: https://cmake.org/cmake/help/latest/manual/cmake.1.html
