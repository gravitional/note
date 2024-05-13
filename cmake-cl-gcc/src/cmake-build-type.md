# Cmake 指定构建类型 Release

[CMake 指定构建类型Debug/Release](https://blog.csdn.net/weixin_39766005/article/details/122439200)

## 单配置生成器CMAKE_BUILD_TYPE

`CMake` 可以配置构建类型, 例如: `Debug`, `Release等`,
控制生成构建系统使用的配置变量 是 `CMAKE_BUILD_TYPE` .
该变量默认为空, `CMake` 识别的值为:

+ `Debug`: 用于在没有优化的情况下, 使用带有调试符号构建库或可执行文件.
+ `Release`: 用于构建的优化的库或可执行文件, 不包含调试符号.
+ `RelWithDebInfo`: 用于构建较少的优化库或可执行文件, 包含调试符号.
+ `MinSizeRel`: 用于不增加目标代码大小的优化方式, 来构建库或可执行文件.

## CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

project(setType LANGUAGES CXX)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE release CACHE STRING "Build Type" FORCE)
endif()

message(STATUS "Build type:${CMAKE_BUILD_TYPE}")
message(STATUS "Debug configuration:${CMAKE_CXX_FLAGS_DEBUG}")
message(STATUS "release configuration:${CMAKE_CXX_FLAGS_RELEASE}")
message(STATUS "release configuration with debug info:${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
message(STATUS "minimal release configuration:${CMAKE_CXX_FLAGS_MINSIZEREL}")
```

## 构建

### Release

直接 `cmake ..` 构建:

### Debug

通过 `-D` 来设置 `CMAKE_BUILD_TYPE` :

```bash
cmake -D CMAKE_BUILD_TYPE=debug ..
```

## 复合配置生成器CMAKE_CONFIGURATION_TYPES

CMake支持复合配置生成器,
可以使用 `CMAKE_CONFIGURATION_TYPES` 变量对这些生成器的可用配置类型进行调整,
`CMAKE_CONFIGURATION_TYPES` 变量接受一个值列表.

使用 `CMAKE_BUILD_TYPE` 并不能修改VS的配置, 用VS打开后默认是 `debug x64`,
而使用 `CMAKE_CONFIGURATION_TYPES` 可以修改VS的默认配置.

### Release版本VS2019 x64

```bash
cmake .. -D CMAKE_CONFIGURATION_TYPES="Release"
```

### Release/Debug版本VS2017 x64

+ cmake configure

```bash
cmake .. -G"Visual Studio 15 2017 Win64" -D CMAKE_CONFIGURATION_TYPES="Release;Debug"
```

该命令将为 `Release` 和 `Debug` 配置生成一个构建树.
然后可以使用 `--config` 标志来决定构建这两个中的哪一个.

+ 编译Release

```bash
cmake --build . --config Release
```

+ 编译Debug

```bash
cmake --build . --config Debug
```
