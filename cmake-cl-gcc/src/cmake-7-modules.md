# make 模块

[cmake-modules(7)](https://cmake.org/cmake/help/latest/manual/cmake-modules.7.html)

The modules listed here are part of the CMake distribution. Projects may provide further modules; their location(s) can be specified in the CMAKE_MODULE_PATH variable.

Utility Modules
These modules are loaded using the include() command.

这里列出的模块是 CMake 分发的一部分.
项目可以提供更多的模块; 它们的位置可以在 [CMAKE_MODULE_PATH][] 变量中指定.

[CMAKE_MODULE_PATH]: https://cmake.org/cmake/help/latest/variable/CMAKE_MODULE_PATH.html#variable:CMAKE_MODULE_PATH

## 工具模块

这些模块是使用 `include()` 命令加载的.

+ AndroidTestUtilities
+ BundleUtilities
+ CheckCCompilerFlag
+ CheckCompilerFlag
+ CheckCSourceCompiles
+ CheckCSourceRuns
+ ...

## Find 模块

这些模块搜索第三方软件. 它们通常通过 `find_package()` 命令调用.

+ FindALSA
+ FindArmadillo
+ FindASPELL
+ FindAVIFile
+ FindBacktrace
