# cmake-vars

## [CMAKE_SOURCE_DIR](https://cmake.org/cmake/help/latest/variable/CMAKE_SOURCE_DIR.html#cmake-source-dir)

到 `源码树顶层`(top level of the source tree)的路径.

+ 含有多个文件夹(多个 CMakeLists.txt) 的情况下, 
  指的是 **最外层的 CMakeLists.txt** 的目录.
+ 这是到 `当前CMake源代码树` 顶层的 `完整路径`.
  对于 in-source 构建, 这与 `CMAKE_BINARY_DIR` 相同.

当以 [cmake -P][] 脚本模式运行时,
`CMake` 会将 `CMAKE_BINARY_DIR`, `CMAKE_SOURCE_DIR`, `CMAKE_CURRENT_BINARY_DIR` 和 `CMAKE_CURRENT_SOURCE_DIR`
变量设置为当前工作目录.

[cmake -P]: https://cmake.org/cmake/help/latest/manual/cmake.1.html#cmdoption-cmake-P

## [CMAKE_BINARY_DIR](https://cmake.org/cmake/help/latest/variable/CMAKE_BINARY_DIR.html#cmake-binary-dir)

通往 `build tree` 顶层的路径.

这是到当前CMake构建树顶层的完整路径.
对于源内构建, 这与 `CMAKE_SOURCE_DIR` 相同.

当以 `cmake -P` 脚本模式运行时,
`CMake` 会将 `CMAKE_BINARY_DIR`, `CMAKE_SOURCE_DIR`, `CMAKE_CURRENT_BINARY_DIR` 和 `CMAKE_CURRENT_SOURCE_DIR`
变量设置为当前工作目录.

## [CMAKE_CURRENT_SOURCE_DIR](https://cmake.org/cmake/help/latest/variable/CMAKE_CURRENT_SOURCE_DIR.html#cmake-current-source-dir)

当前正在处理的 `源文件目录` 的路径.
这是当前正在被 `cmake` 处理的源目录的完整路径.

含有多个文件夹(多个 CMakeLists.txt) 的情况下,
指的是 当前处理的子项目的 CMakeLists.tx 的目录.

当以 `cmake -P` 脚本模式运行时,
CMake 会将变量 `CMAKE_BINARY_DIR`, `CMAKE_SOURCE_DIR`,
`CMAKE_CURRENT_BINARY_DIR` 和 `CMAKE_CURRENT_SOURCE_DIR` 设置为当前工作目录.

## [PROJECT_SOURCE_DIR](https://cmake.org/cmake/help/latest/variable/PROJECT_SOURCE_DIR.html#project-source-dir)

这是**当前目录作用域**或**其父目录作用域** 中最后一次调用 `project()` 命令的源目录.
请注意, 在 **子目录作用域** 的
(即当前`CMakeList.txt` 中的 `add_subdirectory()` 命令引入的子文件中的) `project()` 不会影响它.

## PROJECT_BINARY_DIR

项目 build目录的 完整路径.
这是最新 `project()` 命令的二进制目录.

## [RUNTIME_OUTPUT_DIRECTORY](https://cmake.org/cmake/help/latest/prop_tgt/RUNTIME_OUTPUT_DIRECTORY.html#runtime-output-directory)

构建 RUNTIME target文件的输出目录.

该属性指定 runtime target files 的生成目录.
属性值可以使用生成器表达式.
除非使用了生成器表达式,
否则多配置生成器(Visual Studio, Xcode, Ninja Multi-Config)会将每个配置的 子目录 追加到指定目录.

如果在创建目标时设置了 `CMAKE_RUNTIME_OUTPUT_DIRECTORY` 变量, 则该属性将由该变量的值初始化.
另请参阅 `RUNTIME_OUTPUT_DIRECTORY_<CONFIG>` target 属性.

## [aux_source_directory](https://cmake.org/cmake/help/latest/command/aux_source_directory.html#aux-source-directory)

查找目录中的所有源文件.

```cmake
aux_source_directory(<dir> <variable>)
```

收集指定目录中 所有源文件 的名称, 并将 *列表* 存储在所提供的 `<variable>` 中.
此命令适用于使用 `显式模板实例化` 的项目(explicit template instantiation).
模板实例化文件可以存储在 `Templates` 子目录中,
并使用此命令自动收集, 以避免手动列出所有实例化文件.

很自然可以想到, 使用该命令避免手写 **库或可执行目标** 的源文件列表.
但 `CMake` 生成的编译系统, 无法知道何时添加了新源文件的 编译系统.
通常情况下, 生成的 **build系统**(例如MSVC .sln项目) 会知道何时需要重新运行 CMake,
因为 `CMakeLists.txt` 文件被修改以添加新源文件.

简单的说:如果添加新的源文件到目录中, 而没有修改 CMake 文件,
则必须手动重新运行 CMake 才能生成 包含新文件的 **build system**(.sln 文件).

## [CMAKE_PREFIX_PATH](https://cmake.org/cmake/help/latest/variable/CMAKE_PREFIX_PATH.html#cmake-prefix-path)

以分号分隔的目录列表,
其中指定了 `find_package()`, `find_program()`, `find_library()`, `find_file()`
和 `find_path()` 命令要搜索的 安装 prefixes.

每条命令都会添加其文档中指定的 相应子目录(如 bin, lib 或 include).

默认情况下, 该目录为空.它由项目设置.

还有一个环境变量 `CMAKE_PREFIX_PATH`, 可用作搜索前缀的附加列表.

另请参阅 CMAKE_SYSTEM_PREFIX_PATH, CMAKE_INCLUDE_PATH,
CMAKE_LIBRARY_PATH, CMAKE_PROGRAM_PATH, and CMAKE_IGNORE_PATH.

## [CMAKE_MODULE_PATH](https://cmake.org/cmake/help/latest/variable/CMAKE_MODULE_PATH.html)

**以分号分隔的**目录列表, 使用**正斜线**(`/`) 表示,
指定在检查 CMake 自带的默认模块之前,
由 `include()` 或 `find_package()` 命令加载的 CMake 模块的搜索路径.

默认值为空.它应由 project 自己设置.
