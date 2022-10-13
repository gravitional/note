# Including 第三方库, Third Party Library

英文github地址: https://github.com/ttroy50/cmake-examples
CMake英文官方教程:  https://cmake.org/cmake/help/latest/guide/tutorial/index.html

[TOC]

## 一 文件树

```cpp
├── CMakeLists.txt
├── main.cpp
```

### 1.1 main.cpp

```cpp
#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/filesystem.hpp>
/*Boost库是为C++语言标准库提供扩展的一些C++程序库的总称, 由Boost社区组织开发,
维护. Boost库可以与C++标准库完美共同工作, 并且为其提供扩展功能.
*/
int main(int argc, char *argv[])
{
    std::cout << "Hello Third Party Include!" << std::endl;

    // use a shared ptr
    boost::shared_ptr<int> isp(new int(4));

    // trivial use of boost filesystem
    boost::filesystem::path path = "/usr/share/cmake/modules";
    if (path.is_relative())
    {
        std::cout << "Path is relative" << std::endl;
    }
    else
    {
        std::cout << "Path is not relative" << std::endl;
    }

    return 0;
}
```

## 1.2 CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.5)

# Set the project name
project (third_party_include)
# find a boost install with the libraries filesystem and system
#使用库文件系统和系统查找boost install
find_package(Boost 1.46.1 REQUIRED COMPONENTS filesystem system)
#这是第三方库, 而不是自己生成的静态动态库
# check if boost was found
if(Boost_FOUND)
    message ("boost found")
else()
    message (FATAL_ERROR "Cannot find Boost")
endif()

# Add an executable
add_executable(third_party_include main.cpp)

# link against the boost libraries
target_link_libraries( third_party_include
    PRIVATE
        Boost::filesystem
)
```

## Boost 安装与使用

安装使用
[B2 User Manual, Boost 安装使用](https://www.boost.org/doc/libs/1_80_0/tools/build/doc/html/index.html#bbv2.installation)
[Boost:Getting Started on Windows](https://www.boost.org/doc/libs/1_80_0/more/getting_started/windows.html)
[cmake之find_package 知乎](https://zhuanlan.zhihu.com/p/561027129)
[CMake find_package 指定路径](https://blog.csdn.net/weixin_43742643/article/details/113858915)

让 `find_package()` 到指定路径找包, 有三种方法:

+ 设置 DIR

```cmake
set(Torch_DIR ~/libtorch-1.0.0)
find_package(Torch required)
```

+ 设置 PATHS

```cmake
set(Torch required PATHS ~/libtorch-1.0.0)
```

+ 指定 `DCMAKE_PREFIX_PATH`; `cmake` 时, 指定 `DCMAKE_PREFIX_PATH`

```cmake
cmake -DCMAKE_PREFIX_PATH="~/libtorch-1.0.0" ..
```

## 二 CMake解析

几乎所有不平凡的项目都将要求包含第三方库, 头文件或程序.
`CMake` 支持使用 `find_package()` 函数查找这些工具的路径.
这将从 `CMAKE_MODULE_PATH` 中的文件夹列表中搜索格式为 `"FindXXX.cmake"` 的CMake模块.
在linux上, 默认搜索路径将是 `/usr/share/cmake/Modules`.
在我的系统上, 这包括对大约142个通用第三方库的支持.

此示例要求将 Boost 库安装在默认系统位置.
如果不在默认位置, 可以使用下列形式显式指定

```cmake
find_package(Boost 1.46.1 REQUIRED COMPONENTS filesystem system PATHS "C:/cppLibs/Boosts")
```

### 2.1 Finding a Package

如上所述, `find_package()` 函数将从 `CMAKE_MODULE_PATH`
中的文件夹列表中搜索 `"FindXXX.cmake"` 中的CMake模块.
find_package参数的确切格式取决于要查找的模块.
这通常记录在 `FindXXX.cmake` 文件的顶部.

```cmake
find_package(Boost 1.46.1 REQUIRED COMPONENTS filesystem system)
```

参数:

+ `Boost` - 库名称, 这是用于查找模块文件FindBoost.cmake的一部分
+ `1.46.1` - 需要的boost库最低版本
+ `REQUIRED` - 告诉模块这是必需的, 如果找不到会报错
+ `COMPONENTS` - 要查找的库列表. 从后面的参数代表的库里找boost

可以使用更多参数, 也可以使用其他变量.  在后面的示例中提供了更复杂的设置.

### 2.2 Checking if the package is found

大多数被包含的包将设置变量 `XXX_FOUND`, 该变量可用于检查软件包在系统上是否可用.
在此示例中, 变量为 `Boost_FOUND`:

```cmake
if(Boost_FOUND)
    message ("boost found")
    include_directories(${Boost_INCLUDE_DIRS})
else()
    message (FATAL_ERROR "Cannot find Boost")
endif()
```

### 2.3 Exported Variables

找到包后, 它会自动导出变量, 这些变量可以通知用户在哪里可以找到库, 头文件或可执行文件.
与XXX_FOUND变量类似, 它们与包绑定在一起, 通常记录在 `FindXXX.cmake` 文件的顶部.
本例中的变量

+ `Boost_INCLUDE_DIRS` - boost头文件的路径

在某些情况下, 您还可以通过使用 `ccmake` 或 `cmake-gui` 检查缓存来检查这些变量.

### 2.4 Alias/Imported targets别名/导入目标

大多数 modern CMake库 在其模块文件中导出别名目标.
导入目标的好处是它们也可以填充包含目录和链接的库.

例如, 从CMake v3.5开始, `Boost` 模块支持此功能.
与使用自己的别名目标相似, 模块中的别名可以使引用找到的目标变得更加容易.
对于Boost, 所有目标均使用 `Boost::` 标识符, 然后使用子系统名称导出.  例如, 您可以使用:

```cmake
Boost::boost for header only libraries
Boost::system for the boost system library.
Boost::filesystem for filesystem library.
```

与您自己的目标一样, 这些目标包括它们的依赖关系,
因此与 `Boost::filesystem` 链接将自动添加 `Boost::boost` 和 `Boost::system` 依赖关系.

要链接到导入的目标, 可以使用以下命令:

```cmake
target_link_libraries( third_party_include
      PRIVATE
          Boost::filesystem
  )
```

### 2.5 Non-alias targets

尽管大多数现代库都使用导入的目标, 但并非所有模块都已更新.
如果未更新库, 则通常会发现以下可用变量:

+ `xxx_INCLUDE_DIRS` - 指向库的包含目录的变量.
+ `xxx_LIBRARY` - 指向库路径的变量.

然后可以将它们添加到您的 `target_include_directories` 和 `target_link_libraries` 中, 如下所示:

```cmake
# Include the boost headers
target_include_directories( third_party_include
    PRIVATE ${Boost_INCLUDE_DIRS}
)

# link against the boost libraries
target_link_libraries( third_party_include
    PRIVATE
    ${Boost_SYSTEM_LIBRARY}
    ${Boost_FILESYSTEM_LIBRARY}
)
```

## 三 构建示例

```bash
$ mkdir build
$ cd build/
$ cmake .. 
$ ./third_party_include
```
