# cmake 预编译库, 引入编译的库, install tree, include 路径, 链接属性 public, interface

c++ 项目 引入 3rd library 时, 可以通过源码引入,
也可以单独预先编译之后, 生成安装目录结构, 然后将安装目录复制到项目中,
在 `CMakeLists.txt` 中添加合适的命令导入到项目.

## 引入源码

### git submodule

如果 3rd lib 提供了3rd的 `CMakeLists.txt`, 方便集成,
可以添加为 git 子仓库的方式集成.

例如, 首先拉取项目

```bash
git submodule add git@github.com:fmtlib/fmt.git git_repos/fmt
```

然后在自己的主项目下的 `CMakeLists.txt` 中添加

```cmake
add_subdirectory(fmt)
# 或者项目的自定义宏命令, 添加一些额外检查
AddSubmodule(fmt)
```

### 直接拷贝源代码引入

如果 3rd lib 没有提供3rd的 `CMakeLists.txt`, 比如是 header-only 库(通常为 cxx模板库),
则可以自己手动添加一个简单的 `CMakeLists.txt` 用于集成, 例如:

```cmake
# https://github.com/klmr/cpp11-range
# header-only
project(cpp11_range)
add_library(${PROJECT_NAME} INTERFACE)
add_library(klmrRange::header ALIAS ${PROJECT_NAME})

target_include_directories(${PROJECT_NAME}
    INTERFACE ${PROJECT_SOURCE_DIR}
)
```

通过此 `CMakeLists.txt` 文件, 创建了一个 interface library,
编译的时候不会此target, 目的仅仅是让别的项目依赖它,
写代码的时候可以 `#include` 头文件, 即

```cpp
#include "range.hpp"
...
```

## 引入 install tree

+ 引入 预编译好的二进制 install tree, imported target
+ `xxxConfig.cmake` or `xxx-Config.cmake` 中的 `xxx` 就是库的名称,
也就是 `find_package()` 应该填充的参数名称.
+ ``

如果 3rd lib 体积比较大, 通常是引入编译好的二进制 目录树.
如果 3rd lib 也是用 cmake 构建工程,
则 build 之后, 使用 `cmake --install` 安装到预先指定的位置, 例如

```bash
cmake --install . --config release
```

可以得到 install files tree.
以 sundials 为例:

```bash
├───bin
    ├───sundials_arkode.dll
...
├───examples
│   ├───arkode
...
├───include
│   ├───arkode
...
└───lib
    └───cmake
        └───sundials
            ├───SUNDIALSConfig.cmake
            ├───SUNDIALSConfigVersion.cmake
            ├───SUNDIALSTargets-release.cmake
            ├───SUNDIALSTargets.cmake
```

注意路径 `/lib/cmake/sundials/` 中存放的就是 cmake 导入库相关的文件,
这种 `xxxConfig.cmake` 的导入方式 叫做 [Config 模式][def],
依赖此 3rd lib 时, cmake 会从 `xxxConfig.cmake` 中获取 3rd lib 的 exported target 信息.

### [find_package()][def2]

通常我们使用 `find_package()` 来集成 install tree 到主项目中,
大概流程如下, 含义见注释

```sh
# 将 install tree 添加到 cmake 搜索路径
list(PREPEND CMAKE_PREFIX_PATH "${my_bin_repos}/sundials-msvc-x64")
message(STATUS "[ymy sundials] CMAKE_PREFIX_PATH: ${CMAKE_PREFIX_PATH}")
# `REQUIRED` 表示如果找不到就报错退出;
# `CONFIG` 表示直接使用 config 模式, 跳过 module 模式
find_package(SUNDIALS REQUIRED CONFIG)
# 将 3rdlib 中的 target 即 dll 添加到 链接依赖
set(PRJ_MY_SUNDIALS_LIBS SUNDIALS::kinsol_shared SUNDIALS::cvode_shared)
if(SUNDIALS_FOUND)
    list(APPEND PRJ_LINK_LIBRARIES_PUBLIC ${PRJ_MY_SUNDIALS_LIBS})
endif()
```

根据 [Config Mode Search Procedure][def3],
>CMake 会为软件包构建一组可能的 安装前缀.
>在每个前缀下都会搜索多个目录以查找配置文件.
>下表显示了搜索到的目录.
>每个条目都适用于 Windows (W), UNIX (U) 或 Apple (A) 协议下的 install tree:

这里只讨论一些常见的情况.
下面的 `<prefix>` 表示 `${CMAKE_PREFIX_PATH}` 中的某个候选的搜索路径
对于 windows, 关注以下目录

```bash
<prefix>/
<prefix>/(cmake|CMake)/
<prefix>/<name>*/(lib/<arch>|lib*|share)/cmake/<name>*/
# .//lib/cmake/sundials/SUNDIALSConfig.cmake
```

参考上面给出的 `SUNDIALS` 的目录结构,
`find_package(SUNDIALS REQUIRED CONFIG)` 匹配到最后一种情况,
其中第一个 `/<name>*/` 匹配时被忽略掉.

### 不同配置, release, debug

会看到类似下面的语句

```sh
# Load information for each installed configuration.
file(GLOB _cmake_config_files "${CMAKE_CURRENT_LIST_DIR}/qjsConfig-*.cmake")
foreach(_cmake_config_file IN LISTS _cmake_config_files)
  include("${_cmake_config_file}")
endforeach()
```

`qjsConfig-*.cmake` 表示 不同的配置会写到不同的 `.cmake` 文件中, 例如

```sh
qjsConfig-debug.cmake
qjsConfig-release.cmake
```

### `xxxTargets.cmake` 说明: interface include, link 目录

+ `${CMAKE_CURRENT_LIST_FILE}` 返回当前正被处理的 `CMakeLists.txt` 的 full path.
+ `get_filename_component(... PATH)` 用于获取文件所在目录,
连续调用也就是连续获取上层目录, 最终到达 install tree 的根目录.
+ `set_target_properties` 设置了 exported target 的 interface include 目录,
也就是当 用户项目依赖此 target 时, 需要 include 以搜索头文件的目录.

```c
# Compute the installation prefix relative to this file.
get_filename_component(_IMPORT_PREFIX "${CMAKE_CURRENT_LIST_FILE}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
get_filename_component(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
if(_IMPORT_PREFIX STREQUAL "/")
  set(_IMPORT_PREFIX "")
endif()

# Create imported target SUNDIALS::core_shared
add_library(SUNDIALS::core_shared SHARED IMPORTED)

set_target_properties(SUNDIALS::core_shared PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${_IMPORT_PREFIX}/include"
)
```

### `xxxConfig.cmake`说明: install tree 包含的 exported target

`xxxTargets.cmake` 中包含以下语句, 例如

```c
### ------- Import SUNDIALS targets
include("${CMAKE_CURRENT_LIST_DIR}/SUNDIALSTargets.cmake")
```

`xxxTargets.cmake` 中定义了 install tree 包含的 imported target,
也就是用户项目可以 链接到/依赖于 的 库(.lib,.dll 等).
包含的语句样例为:

```c
foreach(_cmake_expected_target IN ITEMS SUNDIALS::core_shared SUNDIALS::nvecserial_shared SUNDIALS::nvecmanyvector_shared SUNDIALS::nvecopenmp_shared SUNDIALS::sunmatrixband_shared SUNDIALS::sunmatrixdense_shared ...)
  list(APPEND _cmake_expected_targets "${_cmake_expected_target}")
  if(TARGET "${_cmake_expected_target}")
    list(APPEND _cmake_targets_defined "${_cmake_expected_target}")
  else()
    list(APPEND _cmake_targets_not_defined "${_cmake_expected_target}")
  endif()
endforeach()
```

`IN ITEMS` 后面的是 cmake expected target,
也就是 正常情况下, 编译没有出错时, cmake 期望能够 导出的 target.

## ref

[def]: https://cmake.org/cmake/help/latest/manual/cmake-packages.7.html#package-layout
[def2]: https://cmake.org/cmake/help/latest/command/find_package.html#find-package
[def3]: https://cmake.org/cmake/help/latest/command/find_package.html#config-mode-search-procedure