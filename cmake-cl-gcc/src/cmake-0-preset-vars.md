# cmake 预定义变量

## [CMAKE_CURRENT_LIST_FILE][def]

当前正在处理的 `listfile` 的完整路径.
在 CMake 处理项目中的 `listfiles` 时, 该变量将始终设置为当前正在处理的 listfiles.
该值具有 dynamic scope, 也就是 runtime scope.
当 CMake 开始处理 源文件 `A` 中的命令时, 它会将此变量设置为 `A` 的位置.
当 CMake 完成处理文件中的命令时, 它将恢复之前的值.

因此, 宏或函数中的变量值是调用堆栈最底层的文件, 也就是 caller 所在的 `CMakeLists.txt`
而不是包含宏或函数定义的文件.
另请参阅 CMAKE_PARENT_LIST_FILE.

## [INCLUDE_DIRECTORIES][def2]

preprocessor include文件 搜索目录列表.

该属性指定到目前为止向 `target_include_directories()` 命令提供的目录列表.
除了接受来自该命令的值外, 还可以使用 `set_property()` 命令直接在任何目标上设置值.
目标的初始值来自 `INCLUDE_DIRECTORIES` 目录属性的值.
使用`include_directories()` 命令, 可以同时调整 directory and target 属性值.

generators 使用此属性值为编译器设置 include paths.

相对路径不应直接添加到此属性中. 请使用上述命令之一来处理相对路径.

INCLUDE_DIRECTORIES 的内容可以使用 cmake-generator-expressions(7),
语法为 `$<...>`. 有关可用的表达式, 请参阅 cmake-generator-expressions(7) 手册.
请参阅 cmake-buildsystem(7) 手册, 了解定义 buildsystem 属性的更多信息.

## [INTERFACE_INCLUDE_DIRECTORIES][def3]

库所需的公共 include 目录列表.

目标可填充此属性以发布针对目标头文件编译所需的 include 目录. 
target_include_directories() 命令使用给定的 PUBLIC 和 INTERFACE 关键字值填充该属性. 
项目也可以直接获取和设置该属性.

当使用 `target_link_libraries()` 指定目标依赖关系时, 
CMake 将从所有目标依赖关系中读取此属性, 以确定消费者的联编属性.

INTERFACE_INCLUDE_DIRECTORIES 的内容可以使用 "生成器表达式", 
语法为 `$<...>`. 有关可用的表达式, 请参见 cmake-generator-expressions(7) 手册. 
有关定义 buildsystem 属性的更多信息, 请参阅 cmake-buildsystem(7) 手册.

include目录 的使用要求 通常在 联编树 和 安装树 之间有所不同. 
`BUILD_INTERFACE` 和 `INSTALL_INTERFACE` 生成器表达式可用于描述基于使用位置的不同使用要求. 
在 `INSTALL_INTERFACE` 表达式中, 允许使用相对路径, 并且相对于安装前缀进行解释. 例如

```cmake
target_include_directories(mylib INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/mylib>
  $<INSTALL_INTERFACE:include/mylib>  # <prefix>/include/mylib
)
```

## ref

[def]: https://cmake.org/cmake/help/latest/variable/CMAKE_CURRENT_LIST_FILE.html
[def2]: https://cmake.org/cmake/help/latest/prop_tgt/INCLUDE_DIRECTORIES.html
[def3]: https://cmake.org/cmake/help/latest/prop_tgt/INTERFACE_INCLUDE_DIRECTORIES.html