# cmake 生成多种构建

[Can Cmake generate a single makefile that supports both debug and release](https://stackoverflow.com/questions/10083427/can-cmake-generate-a-single-makefile-that-supports-both-debug-and-release)

## scheme1

据我所知, 使用一套构建脚本无法实现这一目标.
不过, 你可以在工作区中建立两个子目录:

```bash
build/
build/debug
build/release
```

然后执行

```bash
cd build

cd build/debug
cmake -DCMAKE_BUILD_TYPE=Debug .../..
make

cd ../release
cmake -DCMAKE_BUILD_TYPE=Release .../..
make
```

如有必要, 可以在联编目录中添加另一个联编脚本, 如下所示

```bash
#!/bin/sh
cd debug && make && cd .
cd release && make && cd .
```

## scheme2

这可以通过 `ADD_CUSTOM_TARGET` 命令来实现.

例如, 如果您想在 `makefile` 中同时添加 debug目标和 release目标, 
请在 `CMakeLists.txt` 文件中添加以下内容:

```cmake
ADD_CUSTOM_TARGET(debug
  COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Debug ${CMAKE_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target all
  COMMENT "Creating the executable in the debug mode.")

ADD_CUSTOM_TARGET(release
  COMMAND ${CMAKE_COMMAND} -DCMAKE_BUILD_TYPE=Release ${CMAKE_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target all
  COMMENT "Creating the executable in the release mode.")
```

然后, 在使用 cmake 配置后, 
可以运行 `make debug` 编译 debug目标,
并运行 `make release` 在同一目录下编译 release目标.

## cmake 脚本 CMAKE_BUILD_TYPE

[cmake-debug和release模式](https://blog.csdn.net/weixin_43708622/article/details/108252550)

```cmake
# 条件判断
if(CMAKE_BUILD_TYPE AND (CMAKE_BUILD_TYPE STREQUAL "Debug"))
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -O0")
    message("Debug mode:${CMAKE_CXX_FLAGS_DEBUG}")
    add_executable(test_debug ${src_dirs})

elseif(CMAKE_BUILD_TYPE AND (CMAKE_BUILD_TYPE STREQUAL "Release"))
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wall -O3")
    message("Release mode:${CMAKE_CXX_FLAGS_RELEASE}")
    add_executable(test_release ${src_dirs})
else()
    message("else:${CMAKE_BUILD_TYPE}")
    message("else:${CMAKE_CXX_FLAGS_RELEASE}")
    add_executable(test_release ${src_dirs})
endif()
```
