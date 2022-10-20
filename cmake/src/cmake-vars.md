# cmake-vars

## CMAKE_SOURCE_DIR

到 `源码树顶层`(top level of the source tree)的路径.

含有多个文件夹(多个 CMakeLists.txt) 的情况下,
指的是最外层的 CMakeLists.tx 的目录.

这是到 `当前CMake源代码树` 顶层的 `完整路径`.
对于 in-source 构建, 这与 `CMAKE_BINARY_DIR` 相同.

当以 [cmake -P][] 脚本模式运行时,
`CMake` 会将 `CMAKE_BINARY_DIR`, `CMAKE_SOURCE_DIR`, `CMAKE_CURRENT_BINARY_DIR` 和 `CMAKE_CURRENT_SOURCE_DIR`
变量设置为当前工作目录.

[cmake -P]: https://cmake.org/cmake/help/latest/manual/cmake.1.html#cmdoption-cmake-P

## CMAKE_BINARY_DIR

通往 `build tree` 顶层的路径.

这是到当前CMake构建树顶层的完整路径.
对于源内构建, 这与 `CMAKE_SOURCE_DIR` 相同.

当以 `cmake -P` 脚本模式运行时,
`CMake` 会将 `CMAKE_BINARY_DIR`, `CMAKE_SOURCE_DIR`, `CMAKE_CURRENT_BINARY_DIR` 和 `CMAKE_CURRENT_SOURCE_DIR`
变量设置为当前工作目录.

## CMAKE_CURRENT_SOURCE_DIR

当前正在处理的 `源文件目录` 的路径.
这是当前正在被 `cmake` 处理的源目录的完整路径.

含有多个文件夹(多个 CMakeLists.txt) 的情况下,
指的是 当前处理的子项目的 CMakeLists.tx 的目录.

当以 `cmake -P` 脚本模式运行时,
CMake 会将变量 `CMAKE_BINARY_DIR`, `CMAKE_SOURCE_DIR`,
`CMAKE_CURRENT_BINARY_DIR` 和 `CMAKE_CURRENT_SOURCE_DIR` 设置为当前工作目录.
