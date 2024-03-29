# 子目录中使用多个CMake文件

[TOC]

许多大型项目由不同的库和二进制文件组成. 本文利用多个 `CMakeLists.txt`文件组织这些库和文件.

## Introduction

本示例说明如何包含子项目.
顶级 `CMakeLists.txt` 调用子目录中的 `CMakeLists.txt` 来创建以下内容:

+ sublibrary1 - 一个静态库
+ sublibrary2 - 只有头文件的库
+ subbinary - 一个可执行文件

文件树如下:

```bash
$ tree
.
├── CMakeLists.txt
├── subbinary
│   ├── CMakeLists.txt
│   └── main.cpp
├── sublibrary1
│   ├── CMakeLists.txt
│   ├── include
│   │   └── sublib1
│   │       └── sublib1.h
│   └── src
│       └── sublib1.cpp
└── sublibrary2
    ├── CMakeLists.txt
    └── include
        └── sublib2
            └── sublib2.h
```

+ `CMakeLists.txt` - 最高层的CMakeLists.txt
+ `subbinary/CMakeLists.txt` - 生成可执行文件的CMakeLists.txt
+ `subbinary/main.cpp` - 可执行文件的源文件
+ `sublibrary1/CMakeLists.txt` - 生成静态库的CMakeLists.txt
+ `sublibrary1/include/sublib1/sublib1.h` - 头文件
+ `sublibrary1/src/sublib1.cpp` - 源文件
+ `sublibrary2/CMakeLists.txt` - 生成仅有头文件的库的CMakeLists.txt
+ `sublibrary2/include/sublib2/sublib2.h` - 头文件

>提示
>在此示例中, 我已将每个项目 的头文件移至 `include` 目录下的子文件夹,
>而 target 的 `include` 文件夹作为根目录.
>这是防止文件名冲突的一个好主意, 因为在 使用的时候名称是
>
>```cpp
>#include "sublib1/sublib1.h"`
>```
>
>如果您为其他用户安装库, 则默认安装位置为 `/usr/local/include/sublib1/sublib1.h`.

## 添加子目录

`CMakeLists.txt` 文件可以 `add` 和 `调用`
其他包含 `CMakeLists.txt` 文件的子目录.

```cmake
add_subdirectory(sublibrary1)
add_subdirectory(sublibrary2)
add_subdirectory(subbinary)
```

## 引用子项目目录

使用 `project()` 命令创建项目时, `CMake` 将自动创建许多变量,
这些变量可用于引用有关该项目的详细信息.
这些变量然后可以由其他 `子项目` 或 `主项目` 使用.
例如, 要引用其他项目的源目录(当前可用的).

```cmake
${sublibrary1_SOURCE_DIR}
${sublibrary2_SOURCE_DIR}
```

`CMake` 中有一些变量会自动创建:

Variable    Info

+ `PROJECT_NAME`;        当前 `project()` 设置的项目的名称.
+ `CMAKE_PROJECT_NAME`;  由 `project()` 命令设置的第一个项目的名称, 即 `顶层项目`.
+ `PROJECT_SOURCE_DIR`;  当前项目的 `源文件` 目录.
+ `PROJECT_BINARY_DIR`;  当前项目的 `构建` 目录.
+ `name_SOURCE_DIR`;     在此示例中, 创建的源目录为 `sublibrary1_SOURCE_DIR`, `sublibrary2_SOURCE_DIR`, 和 `subbinary_SOURCE_DIR`
+ `name_BINARY_DIR`;     本工程的二进制目录是 `sublibrary1_BINARY_DIR`, `sublibrary2_BINARY_DIR`,和 `subbinary_BINARY_DIR`

## Header only Libraries

如果您有一个库被创建为 `仅头文件` 的库, 
则 `cmake` 支持 `INTERFACE` 目标, 以允许创建没有 `任何build输出` 的目标.  
可以[从here找到更多详细信息](https://cmake.org/cmake/help/v3.4/command/add_library.html#interface-libraries)

```cmake
add_library(${PROJECT_NAME} INTERFACE)
```

创建目标时, 您还可以使用 `INTERFACE` 范围包含该目标的 `目录`.
`INTERFACE` 范围用于指定, 链接到 `此目标` 的其他目标需要依赖 `此目录`,
但编译 `此目标` 本身不需要显式指定此目录(可能本身就在一起).

```cmake
target_include_directories(${PROJECT_NAME}
    INTERFACE ${PROJECT_SOURCE_DIR}/include)
```

## 引用子项目中的库

如果子项目创建了一个 `库`(library),
则其他项目可以通过在 `target_link_libraries()` 命令中调用该 `项目的名称` 来引用该库.
这意味着您不必引用新库的完整路径, 而是将其添加为 `依赖项`.

```cmake
target_link_libraries(subbinary
    PUBLIC
        sublibrary1
)
```

或者, 您可以创建 `别名目标`, 该目标允许您在上下文(其实就是某个目标的绰号)中引用该目标.

```cmake
add_library(sublibrary2)
add_library(sub::lib2 ALIAS sublibrary2)
```

To reference the alias, just it as follows:

```cmake
target_link_libraries(subbinary
    sub::lib2
)
```

`别名` 可以将多个功能相关的 `目标` 进行组织.

## 包含子项目中的目录

从 cmake v3 开始从子项目添加库时, 无需将项目 `include` 目录添加到二进制文件的 `include` 目录中.

创建库时, 这由 `target_include_directories()` 命令中的作用域控制.

```cmake
target_include_directories(${PROJECT_NAME}
    PUBLIC ${PROJECT_SOURCE_DIR}/include
)
```

在此示例中, 因为子二进制可执行文件链接了 `sublibrary1` 和 `sublibrary2` 库,

```cmake
target_link_libraries(${PROJECT_NAME}
    sub::lib1
    sub::lib2
)
```

当库使用 `PUBLIC` 或 `INTERFACE` 范围导出时,
将自动包含 `${sublibrary1_SOURCE_DIR}/include` 和 `${sublibrary2_SOURCE_DIR}/include` 文件夹.
(这个地方涉及到 `PUBLIC` 和 `INTERFACE` 的使用, 本电子书的 `CMake-scope` 是讲这个的)

## 构建示例

```cpp
$ mkdir build
$ cd build/
$ cmake ..

$ make
```
