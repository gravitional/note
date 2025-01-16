# find_package(), module 模式

## package INCLUDE_DIRS, *_DIRS, *_LIBRARIES,

[Standard way of knowing if find_package defines INCLUDE_DIR or INCLUDE_DIRS][def4]

事实上的标准是 `*_LIBRARIES` 和 `*_DIRS`, 即复数变量名是 **result variables**.
因此, 它们只能被读取, 而不能被写入.

而且, 对于编写良好的现代 FindModule 来说,
即使是这些变量也很少对用户有用, 因为导入的目标将包含所有相关信息.
因此, 用户只需执行以下操作, 而无需直接使用结果变量:

```c
add_executable(myexe OpenGL::GL)
add_library(mylibrary PUBLIC OpenGL::GL)
```

也就是说, add_executable 的时候,
cmake 会自动读取 `OpenGL::GL` 中的 include 目录的信息,
并用适当的选项 例如 `-IXXX` 调用编译器 和 链接器.

不过, 了解如何使用 FindModule 的正确方法是直接阅读文档,
这些文档要么在 docs 中明确列出,
要么在 `FindModule.cmake` 文件开头的 CMake 注释中列出.

所有标准 module 文件都在 CMake 安装子目录 `Modules` 中,
而且大多数(全部?)有 CMake 文档.
位置提示变量通常被命名为 `*_INCLUDE_DIR` 和 `*_LIBRARY`,
您可以在调用 `CMakeLists.txt` 中的 `find_package()` 之前设置它们,
也可以用 `cmake -D` 或 `cmake-gui` 或 `ccmake` 设置它们.

## ref

[def4]: https://stackoverflow.com/questions/45471859/standard-way-of-knowing-if-find-package-defines-include-dir-or-include-dirs