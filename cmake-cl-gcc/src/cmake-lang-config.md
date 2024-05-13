# `CMAKE_<LANG>_FLAGS_<CONFIG>`

[CMAKE_<LANG>_FLAGS_<CONFIG>](https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_FLAGS_CONFIG.html)

构建 `<LANG>` 语言 的 `<CONFIG>` 配置时,
使用的 language-wide 的 flags.

这些flags 将传递给相应configuration中 编译器的所有调用.
这包括 对compiling的调用 和 linking的调用.

该变量中的标志将在 `CMAKE_<LANG>_FLAGS` 变量中的标志之后传递.
在 compiling的调用中,
这两个变量中的标志将在 `add_compile_options()` 和 `target_compile_options()` 等命令添加的标志之前传递.
在 linking的调用中, 它们将在 `add_link_options()` 和 `target_link_options()` 等命令添加的标志之前传递.

## compilers and tools

[Compilers and Tools](https://gitlab.kitware.com/cmake/community/
-/wikis/doc/cmake/Useful-Variables#compilers-and-tools)

`BUILD_SHARED_LIBS`:
如果设置为 `ON`, 则默认情况下所有库都作为共享库构建.

```cmake
set(build_shared_libs on)
```

`cmake_build_type`:
当使用 single-configuration generator 例如 `Makefile` generator.
控制 build type 的变量, 大小写不敏感.

当使用单配置生成器时, CMake 会默认创建以下变量:

+ None; 使用`CMAKE_C_FLAGS` 或 `CMAKE_CXX_FLAGS`
+ Debug; 使用 `CMAKE_C_FLAGS_DEBUG` 或 `CMAKE_CXX_FLAGS_DEBUG`
+ Release; 使用 `CMAKE_C_FLAGS_RELEASE` 或 `CMAKE_CXX_FLAGS_RELEASE`
+ `RelWithDebInfo`; 使用 `CMAKE_C_FLAGS_RELWITHDEBINFO` 或
`CMAKE_CXX_FLAGS_RELWITHDEBINFO`, 表示 Release with debug info.

+ `MinSizeRel`; 使用 `CMAKE_C_FLAGS_MINSIZEREL` 或
`CMAKE_CXX_FLAGS_MINSIZEREL`, 表示 Minimum size Release.

您可以使用这些默认编译标志(或对其进行修改),
方法是在命令行 configuration 时设置`CMAKE_BUILD_TYPE` 变量.
或在 ccmake GUI 中设置 `CMAKE_BUILD_TYPE` 变量.
这些标志的默认值会随编译器的不同而改变.
如果 CMake 不知道您的编译器, 内容将为空.

例如您在使用 `Makefile` 生成器, 可以创建自己的编译类型:

```cmake
SET(CMAKE_BUILD_TYPE distribution)
SET(CMAKE_CXX_FLAGS_DISTRIBUTION "-O3")
SET(CMAKE_C_FLAGS_DISTRIBUTION "-O3")
```

请注意在cmake configuration时,
`CMAKE_BUILD_TYPE` 并不初始化为可读的值.
这是因为用户可以在 build时 自由选择编译类型.

如果您需要动态获取 build时的目录, 请使用 `$<CONFIG>`.
