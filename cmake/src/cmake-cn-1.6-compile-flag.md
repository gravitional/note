# CMake(六) Compile Flags

本CMake系列是依据github上的cmake-examples进行翻译总结.
同时对于不懂的地方进行总结与标注. 希望本系列能节省你学习CMake的时间.

英文github地址: https://github.com/ttroy50/cmake-examples
CMake英文官方教程:  https://cmake.org/cmake/help/latest/guide/tutorial/index.html

[TOC]

首先说一下什么是编译标志(或者 叫编译选项).
可执行文件的生成离不开编译和链接, 那么如何编译,
比如编译时使用C++的哪一个标准?这些编译设置都在CMAKE_CXX_FLAGS变量中. (C语言编译选项是CMAKE_C_FLAGS)

设置的方法总共有三种, 分别为本文2.1, 2.2, 以及2.3

## 一 文件树

```cpp
├── CMakeLists.txt
├── main.cpp
```

## 1.1 main.cpp

```cpp
#include <iostream>

int main(int argc, char *argv[])
{
   std::cout << "Hello Compile Flags!" << std::endl;

   // only print if compile flag set
#ifdef EX2
  std::cout << "Hello Compile Flag EX2!" << std::endl;
#endif

#ifdef EX3
  std::cout << "Hello Compile Flag EX3!" << std::endl;
#endif

   return 0;
}
```

### 1.2 CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.5)
# 强制设置默认C++编译标志变量为缓存变量,
# 如CMake(五) build type所说, 该缓存变量被定义在文件中,
# 相当于全局变量, 源文件中也可以使用这个变量
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DEX2 -EHsc" CACHE STRING "Set C++ Compiler Flags" FORCE)

project (compile_flags)

add_executable(cmake_examples_compile_flags main.cpp)
#为可执行文件添加私有编译定义
target_compile_definitions(cmake_examples_compile_flags
    PRIVATE EX3
)
#命令的具体解释在二  CMake解析中, 这里的注释只说明注释后每一句的作用
```

给编译器加上 `/EHsc`(`-EHsc`) 选项可以防止输出警告. 参考:
[warning C4530: 使用了 C++ 异常处理程序, 但未启用展开语义](https://blog.csdn.net/wu10188/article/details/124706627)

## 二 CMake解析

### 2.1 设置每个目标编译标志

在现代CMake中设置 `C++标志` 的 **推荐方法** 是专门针对某个 目标(target) 设置标志,
可以通过 `target_compile_definitions()` 函数设置某个目标的编译标志.

```cmake
target_compile_definitions(cmake_examples_compile_flags
    PRIVATE EX3
)
```

如果这个目标是一个`库`(在这里指的是 `cmake_examples_compile_flags`),
且编译器在编译目标时添加定义 `-DEX3`,
并且选择了范围 `PUBLIC` 或 `INTERFACE, `
那么, 定义 `-DEX3` 也将包含在链接了此目标(`cmake_examples_compile_flags`)的所有可执行文件中.

注意, 本语句使用了 `PRIVATE`, 所以编译选项不会传递.

对于编译器选项, 还可以使用 `target_compile_options()` 函数.
此处谢谢github用户[HamsterCoderSim的更正](https://github.com/HamsterCoderSim).

```cmake
target_compile_options(<target> [BEFORE]
  <INTERFACE|PUBLIC|PRIVATE> [items1...]
  [<INTERFACE|PUBLIC|PRIVATE> [items2...] ...])
```

它用来给 `target` 添加编译选项,
`target` 指的是由  `add_executable()` 产生的 `可执行文件`,或 `add_library()` 添加进来的库.

`<INTERFACE|PUBLIC|PRIVATE>` 指的是 `[items...]` 选项可以传播的范围,
`PUBLIC` and `INTERFACE` 会传播 `<target>` 的 [INTERFACE_COMPILE_DEFINITIONS][] 属性,
`PRIVATE` and `PUBLIC` 会传播 `target` 的 [COMPILE_DEFINITIONS][] 属性.

### 2.2 设置默认编译标志

默认的 `CMAKE_CXX_FLAGS` 为 `空` 或包含适用于 构建类型 的标志.
要设置其他默认编译标志, 如下使用:

```cmake
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DEX2" CACHE STRING "Set C++ Compiler Flags" FORCE)
```

强制设置默认C++编译标志变量为 `缓存变量`, 如CMake(五) build type所说,
该 缓存变量 被定义在文件中, 相当于全局变量, `源文件` 中也可以使用这个变量.
这个变量原本包含的参数仍然存在, 只是添加了 `EX2`.

`CACHE STRING "Set C++ Compiler Flags" FORCE` 命令
是为了强制将 `CMAKE_CXX_FLAGS` 变量放到 `CMakeCache.txt` 文件中

`"${CMAKE_CXX_FLAGS} -DEX2"` 这个字符串可以保留原有的 `CMAKE_CXX_FLAGS` 中的参数,
额外添加了一个 `EX2` 参数.
注意写法: `空格`, 并且参数前加了 `-D`

类似于设置 `CMAKE_CXX_FLAGS`, 还可以设置其他选项:

+ 设置C编译标志: `CMAKE_C_FLAGS`
+ 设置链接标志: `CMAKE_LINKER_FLAGS`.

### 2.3 设置CMake标志与构建类型

类似, 可以使用以下方法设置全局 C编译器标志.

+ 利用ccmake或者gui
+ 在cmake命令行中:

```bash
$ cmake .. -DCMAKE_CXX_FLAGS="-DEX3"
```

## 2.4 区别

+ 2.2方法的设置 `CMAKE_C_FLAGS` 和 `CMAKE_CXX_FLAGS` 将为 `该目录`
或所有包含的 `子目录` 中的所有 `目标` 全局设置一个编译器标志.
现在不建议使用该方法, 首选使用 `target_compile_definitions` 函数.
+ 2.1方法是被建议的, 只为这个目标设置编译选项 .
+ 2.3 设置的也是 全局编译器选项.

## 三 构建示例

```bash
$ mkdir build
$ cd build/
$ cmake ..
$ make VERBOSE=1
```

[INTERFACE_COMPILE_DEFINITIONS]: https://cmake.org/cmake/help/v3.0/prop_tgt/INTERFACE_COMPILE_DEFINITIONS.html#prop_tgt:INTERFACE_COMPILE_DEFINITIONS
[COMPILE_DEFINITIONS]: https://cmake.org/cmake/help/v3.0/prop_tgt/COMPILE_DEFINITIONS.html#prop_tgt:COMPILE_DEFINITIONS
