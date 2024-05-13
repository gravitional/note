# cmake 导入 3rd lib

[C++工程: 总结 CMake 添加第三方库依赖方式git](https://www.jianshu.com/p/f181b5bd0a63)

以 [jsoncpp][] 为例, 介绍几种引入第三方库的方式

[jsoncpp]: https://github.com/open-source-parsers/jsoncpp

## 代码依赖

这种方式是把第三方库的完整代码直接添加到我们的项目中,
当做项目代码的一部分进行编译, 这种方式会把第三方代码和我们的代码混在一起,
并不推荐使用.
首先我们需要到 [jsoncpp][] 下载需要的头文件和实现代码, 放到项目当中.

### 工程文件目录

```bash
├── CMakeLists.txt
├── jsoncpp
│   ├── include
│   │   └── json
│   │       ├── autolink.h
|   │       ...
│   ├── json_batchallocator.h
│   ...
└── main.cpp
```

### CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.17)
project(includes_full_code)
set(CMAKE_CXX_STANDARD 14)

# 包含头文件
include_directories(./jsoncpp/include)
set(jsoncpp jsoncpp/json_reader.cpp jsoncpp/json_writer.cpp jsoncpp/json_value.cpp)

# 添加可执行代码
add_executable(includes_full_code main.cpp ${jsoncpp})
```

测试使用的 `main.cpp`

后面的示例的 `main.cpp` 都是一样

```cpp
#include <iostream>
#include "json/json.h"
int main() {
    Json::Value json;
    json["name"] = "Wiki";
    json["age"] = 18;
    std::cout << json.toStyledString() << std::endl;
    return 0;
}
```

完整代码: [includes_full_code_exmaple](https://github.com/taoweiji/cpp-cmake-example/tree/master/includes_full_code)

## 内部工程依赖

这种方式和上面 代码依赖 的方式类似, 不同的是 内部工程依赖
会把第三方库的管理职责交给第三方库工程CMakeLists.txt文件,
这种方式的好处是职责分明, 是最常用的依赖方式.

### 工程文件目录

目录结果和上面的案例相似, 不同的是jsoncpp文件夹多了一个 CMakeLists.txt 文件

```bash
├── CMakeLists.txt
├── jsoncpp
│   ├── CMakeLists.txt
│   ├── include
│   │   └── json
│   │       ├── autolink.h
│   │       ...
│   ├── json_batchallocator.h
│   ...
└── main.cpp
```

### jsoncpp/CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.17)
project(jsoncpp)
add_library(${PROJECT_NAME} json_reader.cpp json_value.cpp json_writer.cpp)
# PUBLIC: self 和 user 都使用 include 目录
target_include_directories(${PROJECT_NAME} PUBLIC ${PROJECT_SOURCE_DIR}/include)
```

### CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.17)
project(multi_cmakelists)

# 添加子工程
add_subdirectory(jsoncpp)
add_executable(${PROJECT_NAME} main.cpp)

# 链接子工程
target_link_libraries(${PROJECT_NAME} jsoncpp)
```

完整代码: [multi_cmakelists_example](https://github.com/taoweiji/cpp-cmake-example/tree/master/multi_cmakelists)

这种方式除了引入第三方依赖, 通常我们也会用这种方式来管理项目中的各个子模块,
每个模块都有独立的CMakeLists.txt文件, 从而实现子工程的单独引用,
源码请看 [subdirectory_example](https://github.com/taoweiji/cpp-cmake-example/tree/master/subdirectory).

## find_library: 编译库方式引入

[find_library](https://cmake.org/cmake/help/latest/command/find_library.html)

这种方式是用来依赖 *已经打包好的二进制文件*,
这种方式也分为静态库(`.a`, `.lib`)和动态库(`.so`, `.dll`)方式引入,
这种方式也可以查找本机已经安装好的库,
比如 Android 的 log 库就是通过这种方式引入.

### 生成.a文件

运行上面的 **内部工程依赖** 案例后,
我们我们可以从项目中找到编译好的
multi_cmakelists/cmake-build-debug/jsoncpp/libjsoncpp.a 文件.

### 工程文件目录

和上面不同的是, 这里只需要导入 `jsoncpp` 的头文件和 `.a` 文件.

```bash
├── CMakeLists.txt
├── jsoncpp
│   ├── include
│   │   └── json
│   │       ├── autolink.h
│   │       ...
│   └── libjsoncpp.a
└── main.cpp
```

### CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.17)
project(find_library_example)

# 头文件包含目录
include_directories(jsoncpp/include)
add_executable(${PROJECT_NAME} main.cpp)

# 搜索库文件, NAMES 指定文件名, 地址将存储到变量 jsoncpp_lib
find_library(jsoncpp_lib NAMES jsoncpp PATHS ./jsoncpp)
target_link_libraries(${PROJECT_NAME} ${jsoncpp_lib})
```

完整代码: [find_library_example](https://github.com/taoweiji/cpp-cmake-example/tree/master/find_library)

这种方式在 Android 开发很常见, 比如我们引入xlog实现日志打印就可以通过这种方式实现, 代码参考 [xlog_example](https://github.com/taoweiji/cpp-cmake-example/tree/master/xlog).

## FetchContent

[FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) 是 cmake 3.11.0 版本开始提供的功能,
可以非常方便用来添加第三方依赖.

### 工程文件目录

```bash
├── CMakeLists.txt
└── main.cpp
```

### CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.17)
project(fetch_content_example)

include(FetchContent)
#FetchContent_Declare(jsoncpp
#        GIT_REPOSITORY https://github.com/open-source-parsers/jsoncpp.git
#        GIT_TAG 1.9.4)
# 建议使用压缩包的方式依赖, 下载速度更快
FetchContent_Declare(jsoncpp
        URL https://github.com/open-source-parsers/jsoncpp/archive/1.9.4.tar.gz)
FetchContent_MakeAvailable(jsoncpp)

add_executable(${PROJECT_NAME} main.cpp)
target_link_libraries(${PROJECT_NAME} jsoncpp_lib)
```

建议通过压缩包的方式引入, 因为直接引入git仓库可能会很慢.

完整代码: [fetch_content_example](https://github.com/taoweiji/cpp-cmake-example/tree/master/fetch_content)

Android SDK 的 CMake 的默认版本是3.10.2, 并不支持FetchContent, 如果想在Android开发中使用需要安装3.11.0以上版本的cmake, 为了降低团队的协同成本, 并不建议在 Android 工程使用, 建议使用内部工程的方式引入.

## CPM

[CPM.cmake](https://github.com/cpm-cmake/CPM.cmake) 是在 FetchContent 的基础上封装而来, 相比 FetchContent 更加简单易用,
使用CPM需要到 CPM.cmake 下载cmake目录的文件 CPM.cmake, get_cpm.cmake和testing.cmake, 添加到项目当中.

### 工程文件目录

```bash
├── CMakeLists.txt
├── cmake
│   ├── CPM.cmake
│   ├── get_cpm.cmake
│   └── testing.cmake
└── main.cpp
```

```cmake
CMakeLists.txt
cmake_minimum_required(VERSION 3.17)
project(cpm_example)
include(cmake/CPM.cmake)
#CPMAddPackage(
#        GIT_REPOSITORY https://github.com/open-source-parsers/jsoncpp.git
#        GIT_TAG 1.9.4)
# 建议使用压缩包的方式依赖, 下载速度更快
CPMAddPackage(
        NAME jsoncpp
        URL https://github.com/open-source-parsers/jsoncpp/archive/1.9.4.tar.gz)

add_executable(${PROJECT_NAME} main.cpp)
target_link_libraries(${PROJECT_NAME} jsoncpp_lib)
```

这种方式的细节不需要我们自己处理, 都交给了CPM解决, 这种方式也同样不建议在 Android 工程使用.

完整代码: [cpm_example](https://github.com/taoweiji/cpp-cmake-example/tree/master/cpm)

## find_package

[Cmake命令之find_library介绍](https://www.jianshu.com/p/8a64c77343cb)

find_package 是 cmake 3.19.0 版本开始提供的功能, 可以非常方便添加,
这种方式主要是从本机上查找已经安装好的库, 需要提前通过命令安装.

```cmake
find_library (<VAR> name [path1 path2 ...])
```

+ `<var>`用于存储该命令执行的结果, 也就是找到的库的全路径(包含库名):
    1. `<var>`可以是普通变量(需要指定NO_CACHE选项),
    也可以是缓存条目(意味着会存放在CMakeCache.txt中,
    不删除该文件或者用set重新设置该变量, 其存储的值不会再刷新);
    2. 当库能被找到, `<var>`会被存放正常的库路径,
    当库未被找到, `<var>`中存放的值为 `<var>-NOTFOUND`.
    只要`<var>`中的值不是`<var>-NOTFOUND`,
    那么即使多次调用 `find_library`, `<var>` 也不会再刷新;
+ `name`用于指定待查找的库名称, 库名称可以使用全称,
例如 `libmymath.a`(优先会当成全名搜索);
也可以不带前缀(例如前缀lib)和后缀(例如Linux中的`.so`, `.a`, Mac中的`.dylib`等), 直接使用 `mymath`;
+ `path` 用于指定库的查找的路径;

### 安装jsoncpp

*nix平台源码编译

```bash
# 拉取代码
git clone https://github.com/open-source-parsers/jsoncpp
cd jsoncpp
mkdir -p build/debug
cd build/debug
# 生成Makefile
cmake -DCMAKE_BUILD_TYPE=release -DBUILD_STATIC_LIBS=OFF -DBUILD_SHARED_LIBS=ON -DARCHIVE_INSTALL_DIR=. -DCMAKE_INSTALL_INCLUDEDIR=include -G "Unix Makefiles" ../..
# 安装
make && make install
如果提示没有安装cmake, 需要自行安装cmake
```

### 工程文件目录

```bash
├── CMakeLists.txt
└── main.cpp
```

### CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.17)
project(find_package_example)
find_package(jsoncpp REQUIRED)
add_executable(${PROJECT_NAME} main.cpp)
target_link_libraries(${PROJECT_NAME} jsoncpp_lib)
```

完整代码: [find_package_example](https://github.com/taoweiji/cpp-cmake-example/tree/master/find_package)

使用这种方式是需要有个大前提, 电脑必须已经安装好了对应的库,
否则无法正常工作, 这种方式只有在特定的场景下使用, 比如调用电脑的opencv, openssl.

## git submodule

这种方式是利用git的submodule实现, 推荐Android使用,
通过git添加另外一个仓库的依赖, 可更新另外一个仓库的依赖, 但是代码不会包含进来.

```bash
# 在A仓库添加B仓库依赖, 操作完后需要提交上去
git submodule add https://github.com/taoweiji/B.git
```

A仓库拉取及 `submodule` 仓库的更新

```bash
git clone https://github.com/taoweiji/A.git
git submodule init && git submodule update
```

## Android 动态依赖

[C++工程: 以 xlog 为例介绍 Android NDK 如何依赖第三方C++动态库](https://www.jianshu.com/p/ad2d0e4958e4)

## 导入编译好的目标文件 IMPORTED

[CMake应用: 模块化及库依赖](https://zhuanlan.zhihu.com/p/373363335)

在前面介绍的命令add_subdirectory其实是相当于通过源文件来构建项目所依赖的目标文件,
但是CMake也可以通过命令来导入已经编译好的目标文件.

### 导入库文件

[add_libraray](https://cmake.org/cmake/help/latest/command/add_library.html#imported-libraries)

```cmake
add_library(<name> <type> IMPORTED [GLOBAL])
```

创建名为 `<name>` 的 `IMPORTED` 目标库.
构建时不会生成任何规则, `IMPORTED` 目标属性为 `True`.
`<name>` 的生效作用域在创建它的目录及其下级目录中, 但是可用 `GLOBAL` 选项扩展到全局.
它可以像项目中创建的任何目标一样被引用.
`IMPORTED` 库可以方便地从 `target_link_libraries()` 等命令中引用.
有关导入库的详细信息, 可通过设置名称以 `IMPORTED_` 和 `INTERFACE_` 开头的属性来指定.

```cmake
add_library(math STATIC IMPORTED)
set_property(TARGET math PROPERTY
    IMPORTED_LOCATION "./lib/libmath.a")
```

对于库文件的路径, 也可以使用 `find_library` 命令来查找,
比如在 `lib` 目录下查找math的 `Realse` 和 `Debug` 版本:

```cmake
find_library(LIB_MATH_DEBUG NAMES mathd HINTS "./lib")
find_library(LIB_MATH_RELEASE NAMES math HINTS "./lib")
```

对于不同的编译类型,
可以通过`IMPORTED_LOCATION_<CONFIG>`来指明不同编译类型对应的库文件路径:

```cmake
add_library(math STATIC IMPORTED GLOBAL)
set_target_properties(math PROPERTIES
  IMPORTED_LOCATION "${LIB_MATH_RELEASE}"
  IMPORTED_LOCATION_DEBUG "${LIB_MATH_DEBUG}"
  IMPORTED_CONFIGURATIONS "RELEASE;DEBUG"
)
```

导入成功以后, 就可以将该库链接到其他目标上, 但是导入的目标不可以被 `install`.

这里以导入静态库为例, 导入动态库或其他类型也是类似的操作,
只需要将文件类型 `STATIC` 修改成对应的文件类型即可.

### 导入可执行文件

这个不是那么常用, 和导入库文件类似的:

```cmake
add_executable(demo IMPORTED)
set_property(TARGET demo PROPERTY
    IMPORTED_LOCATION "./bin/demo")
```

## CMake 拷贝运行时库 dll

[如何通过cmake自动拷贝运行所需dll到executable目录](https://www.jianshu.com/p/47370c584356)

给你的 `library` 定义一个带 `IMPORTED` 属性的target,
同时需要并为此定义 `lib` 和 `dll` 路径属性, 需要区分 `debug` 和 `release`.

```cmake
add_library(sdl2 SHARED IMPORTED GLOBAL)
set_property(TARGET sdl2 PROPERTY IMPORTED_IMPLIB_RELEASE "${SDL_ROOT_PATH}/lib/SDL2.lib")
set_property(TARGET sdl2 PROPERTY IMPORTED_LOCATION_RELEASE "${SDL_ROOT_PATH}/bin/SDL2.dll")
set_property(TARGET sdl2 PROPERTY IMPORTED_IMPLIB_DEBUG "${SDL_ROOT_PATH}/lib/SDL2d.lib")
set_property(TARGET sdl2 PROPERTY IMPORTED_LOCATION_DEBUG "${SDL_ROOT_PATH}/bin/SDL2d.dll")
```

链接此库用上面定义 `target` 的名字即可,
这样会自动隐藏背后的不同版本的lib库文件.

```cmake
target_link_libraries(YourProg sdl2 ...)
```

在 `CMakeLists.txt` 定义一个定制的 `build step`,
来自动拷贝运行所需的dll文件:

```cmake
add_custom_command (TARGET YourProg POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    $<TARGET_FILE:sdl2> $<TARGET_FILE_DIR:YourProg>)
```

通常, 当你的项目集成了很多第三方库, 以上这么干会把CMakeLists.txt搞得很复杂,
因此推荐得方式还是得写FindXXX.cmake,
并在FindXXX.cmake里创建对应的target,
关于如何写可以参考[这篇文章](https://www.jianshu.com/p/3d90d05ed7cd).

### 多个dll的情况

有些第三方库提供 **已编译好的库文件** 且是 **多个**, 这时候上面的 target 不再能拷贝dll,
而且会报错 `unknwon target`, 对于这种第三方库的target得如下定义:

```cmake
# 如果是在 ffmpeg 子目录的 CMakeLists.txt 中,
# 加上 GLOBAL 使 ffmpeg 全局可用
add_library(ffmpeg INTERFACE IMPORTED GLOBAL)
set_target_properties(ffmpeg PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${ffmpeg_INCLUDE_DIRS}"
        INTERFACE_LINK_LIBRARIES "${ffmpeg_LIBRARIES}"
        IMPORTED_LOCATION "${ffmpeg_LIBRARY_DLLS}")
```

拷贝 `DLL` 文件的 `command` 得改成如下方式:

```cmake
# 在主项目的 CMakeLists.txt 中
get_target_property(DLLS ffmpeg IMPORTED_LOCATION)
add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${DLLS} $<TARGET_FILE_DIR:${PROJECT_NAME}>)
```

`${DLLS}` 即对应所有dll文件路径总和.
其实, Qt 打包时候拷贝 `DLL` 也是同样这个做法:

```cmake
add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        $<TARGET_FILE:Qt5::Core> $<TARGET_FILE_DIR:${PROJECT_NAME}>
        $<TARGET_FILE:Qt5::Widgets> $<TARGET_FILE_DIR:${PROJECT_NAME}>
        ... etc ...)
```
