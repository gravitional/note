# CMake(六) Compile Flags

本CMake系列是依据github上的cmake-examples进行翻译总结.
同时对于不懂的地方进行总结与标注. 希望本系列能节省你学习CMake的时间.

学习方式是在实践中利用github上的example学习,
同时对于不懂的地方主要通过翻译官方手册学习, 其次是查找博客上的私人理解.

因为每一个example都是一个工程, 所以讲解时会利用文件树解释每一个文件里的语法.

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
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DEX2" CACHE STRING "Set C++ Compiler Flags" FORCE)

project (compile_flags)

add_executable(cmake_examples_compile_flags main.cpp)
#为可执行文件添加私有编译定义
target_compile_definitions(cmake_examples_compile_flags
    PRIVATE EX3
)
#命令的具体解释在二  CMake解析中, 这里的注释只说明注释后每一句的作用
```

