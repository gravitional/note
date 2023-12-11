# gnu make

## make verbose

[9.8 Summary of Options](https://www.gnu.org/software/make/manual/html_node/Options-Summary.html)

### `--debug[=options]`

除正常处理外, 打印调试信息.
可以选择不同级别和类型的输出.
如果没有参数, 则打印 `basic` 级别的调试信息.
以下是可能的参数; 只考虑输入的第一个字符, 参数值必须以逗号或空格分隔.

+ `a`(all)
启用所有类型的调试输出.
这相当于使用"-d".

+ `b`(basic)
基本调试会打印发现过时的每个目标, 以及构建是否成功.

+ `v`(verbose)
比 "基本 "调试高一个级别, 包括关于哪些 makefile 已被解析, 无需重建的先决条件等信息.
该选项也可启用 "基本 "信息.

+ `i`(implicit)
打印每个目标的隐式规则搜索信息.
该选项也可启用 "基本 "信息.

+ `j`(jobs)
打印调用特定子命令的详细信息.

+ `m`(makefile)
默认情况下, 重制 makefile 时不启用上述信息.
该选项也会在重建 makefile 时显示信息.
请注意, "全部 "选项会启用该选项.
该选项还启用了 "基本 "信息.

+ `p`(print)
打印要执行的配方, 即使配方通常是无声的(由于 .SILENT 或 '@').
同时还将打印 makefile 名称和定义配方的行号.

+ `w`(why)
通过显示哪些先决条件比目标文件更新, 解释为什么每个目标文件都必须重新制作.

+ `n`(none)
禁用当前启用的所有调试.
如果在此之后遇到其他调试标记, 它们仍将生效.

## CMake VERBOSE

[make VERBOSE=1 documentation?](https://stackoverflow.com/questions/71666185/make-verbose-1-documentation)
[VERBOSE](https://cmake.org/cmake/help/latest/envvar/VERBOSE.html)
[build-tool-mode](https://cmake.org/cmake/help/latest/manual/cmake.1.html#build-tool-mode)
[CMAKE_NO_VERBOSE](https://cmake.org/cmake/help/latest/envvar/CMAKE_NO_VERBOSE.html#envvar:CMAKE_NO_VERBOSE)

之所以 GNU make 文档中 "明显没有" 变量 `VERBOSE`,
是因为 VERBOSE 变量对 make 没有任何特殊意义.

也许某些 makefile 会对 `VERBOSE` 做一些有趣的事情, 但那是因为特定的 `makefile` 在编写时对该变量做了特殊处理.
其他 `makefile` 会忽略它, 或者它的意义与你想象的不同.

例如, 由 `CMake` 创建的 `makefile` 在设置 `VERBOSE` 变量时会有特殊的行为,
但由 `automake` 创建的 makefile 则完全不理会它.
在 automake 制作的 makefile 中, 使用的是 `V=1`, 而不是 `VERBOSE=1`.

+ `VERBOSE`
3.14 版新增功能.
当您开始实际构建项目时, 激活 CMake 和您选择的构建工具的冗长输出.
请注意, 任何给定值都会被忽略.
它只是被检查是否存在.
另请参阅 Build Tool Mode 和 CMAKE_NO_VERBOSE 环境变量

+ `CMAKE_NO_VERBOSE`
3.14 版新增功能.
当设置 `VERBOSE` 环境变量时, 禁用 CMake 的冗长输出.
只有当你开始实际构建项目时, 你所选择的构建工具才会继续打印冗长输出.

+ `cmake --build -v, --verbose`

如果支持, 启用包括将要执行的联编命令在内的详细输出.
如果设置了 VERBOSE 环境变量或 CMAKE_VERBOSE_MAKEFILE 缓存变量, 则可以省略此选项.

+ `--`
将其余选项传递给 native 工具.
