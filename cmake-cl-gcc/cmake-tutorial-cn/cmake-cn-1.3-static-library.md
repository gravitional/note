# 静态库, Static Library

[CMake(三)Static Library](https://sfumecjf.github.io/cmake-examples-Chinese/01-basic/1.3%20%20Static%20Library.html)

本文自己创建库的操作, 应该暂时用不到. 但是关于如何添加路径, 链接库的命令, 还是需要掌握的.

## 文件树

```bash
├── CMakeLists.txt
├── include
│   └── static
│       └── Hello.h
└── src
    ├── Hello.cpp
    └── main.cpp
```

### 1.1 Hello.h

```cpp
/*声明了Hello类, Hello的方法是print(),*/

#ifndef __HELLO_H__
#define __HELLO_H__

class Hello
{
public:
    void print();
};

#endif
```

### 1.2 `Hello.cpp`

```cpp
/*实现了Hello::print()*/
#include <iostream>

#include "static/Hello.h"

void Hello::print()
{
    std::cout << "Hello Static Library!" << std::endl;
}
```

### 1.3 `main.cpp`

```cpp
#include "static/Hello.h"

int main(int argc, char *argv[])
{
    Hello hi;
    hi.print();
    return 0;
}
```

### 1.4 CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.5)
project(hello_library)
############################################################
# Create a library
############################################################
#库的源文件Hello.cpp生成静态库hello_library
add_library(hello_library STATIC
    src/Hello.cpp
)
target_include_directories(hello_library
    PUBLIC
        ${PROJECT_SOURCE_DIR}/include
)
# target_include_directories为一个目标(可能是一个库library也可能是可执行文件)添加头文件路径.
############################################################
# Create an executable
############################################################
# Add an executable with the above sources
#指定用哪个源文件生成可执行文件
add_executable(hello_binary
    src/main.cpp
)
#链接可执行文件和静态库
target_link_libraries( hello_binary
    PRIVATE
        hello_library
)
#链接库和包含头文件都有关于scope这三个关键字的用法.
```

## CMake解析

### 2.1 创建静态库

`add_library()` 函数用于从某些源文件创建一个库, 默认生成在构建文件夹.
写法如下:

```cmake
add_library(hello_library STATIC
    src/Hello.cpp
)
```

在 `add_library` 调用中包含了源文件, 用于创建名称为 `libhello_library.a` 的静态库.

>NOTE
>如前面的示例所述, 将源文件直接传递给 `add_library` 调用, 这是 modern CMake 的建议.
>(而不是先把 `Hello.cpp` 赋给一个变量)

## 2.2 添加头文件所在的目录

使用 `target_include_directories()` 添加了一个 `目录`,
这个目录是 `库` 所包含的 `头文件` 的目录, 并设置 `库属性` 为 `PUBLIC`.

```cmake
target_include_directories(hello_library PUBLIC
    ${PROJECT_SOURCE_DIR}/include)
```

This will cause the included directory used in the following places:

使用这个函数后, 这个`目录`会在以下情况被调用:

+ 编译 `hello_library` 这个库的时候
  因为这个库 `hello_library` 由 `Hello.cpp` 生成,
  `Hello.cpp` 中函数的定义在`Hello.h` 中, `Hello.h` 在此 `include` 目录下,
  所以显然编译这个库的时候, 这个目录会用到
+ 欲编译其他目标 `A`(`库`或者`exe`), 而 `A` 链接到(依赖于)库 `hello_library`.

### 2.2.1 private pubic interface的范围详解

大家如果去搜索, 会发现解释杂乱无章. 大部分解释是这样:

>下面的 `目标`(target), 指当前的 `编译目标`, 在这里就是 `hello_library`.
>`依赖` 指的是编译 `当前目标` 需要的其他 `库`, `文件` 等等, 在这里是 `include/Hello.h`.
>为什么要显式声明 `hello_library` 依赖于 `include/Hello.h` 呢, 因为它们被放在不同文件夹了.
>
>如果 `目标` 的 `头文件`中 包含了 `依赖` 的 `头文件`(`源文件`间接包含), 那么这里就是 `PUBLIC`
>如果 `目标` 仅 `源文件`中 包含了 `依赖` 的 `头文件`, 那么这里就是 `PRIVATE`
>如果 `目标` 的 `头文件` 包含依赖, 但 `源文件` 未包含, 那么这里就是 `INTERFACE`
>
>或者是这样: 当创建动态库时,
>
>如果 `源文件`(例如 .CPP)中包含第三方头文件, 但是 `头文件`(例如 .hpp)中不包含该第三方文件头, 采用 `PRIVATE`.
>如果 `源文件` 和 `头文件` 中都包含该第三方文件头, 采用 `PUBLIC`.
>如果 `头文件` 中包含该第三方文件头, 但是 `源文件`(例如CPP)中不包含, 采用 `INTERFACE`.

我个人认为上面的说法是错误的.
正确理解: 称当前 `编译目标(target)` 为 `目标(库)`, 链接了(依赖于) `目标` 的 `其他目标`(库或者可执行程序)称为 `user`

+ `PRIVATE`; `目录` 将被添加到 `目标(库)` 的 include路径 中.
+ `INTERFACE`; `目录` 不会被添加到 `目标(库)` 的 include路径 中, 而是 `user` 的 include路径 中
+ `PUBLIC`; `目录` 既被添加到 `目标(库)` 的 include路径 中, 同时添加到 `user` 的 include路径 中

也就是说, 根据 `目标(库)` 是否包含这个路径, 以及 `user` 是否包含这个路径, 可以分为三种scope.
有点像 C++ 类继承关系中的 private, protect.

### ​建议!

对于公共的头文件, 最好在 `include` 文件夹下建立子目录.
传递给函数 `target_include_directories()` 的目录, 应该是所有包含目录的根目录, 然后在这个根目录下建立不同的文件夹, 分别写头文件.

这样使用的时候, 不需要写 `${PROJECT_SOURCE_DIR}/include`, 而是直接选择对应的文件夹里对应头文件.
下面是例子: `#include "static/Hello.h"` 而不是 `#include "Hello.h"`
使用此方法意味着在项目中使用多个库时, 头文件名冲突的可能性较小.

## 2.3 链接库

创建将使用这个库的可执行文件时, 必须告知编译器需要用到这个库.
可以使用 `target_link_libraries()` 函数完成此操作.
`add_executable()` 连接源文件, `target_link_libraries()` 连接库文件.

```cmake
add_executable(hello_binary
    src/main.cpp
)

target_link_libraries( hello_binary
    PRIVATE
        hello_library
)
```

这告诉 `CMake` 在链接期间将 `hello_library` 链接到 `hello_binary` 可执行文件.
同时, 这个被链接的库(`hello_library`)如果有 `INTERFACE` 或者 `PUBLIC` 属性的 include目录,
那么, 这个 include目录 也会被传递 (propagate) 给这个 可执行文件.

An example of this being called by the compiler is

```bash
/usr/bin/c++ CMakeFiles/hello_binary.dir/src/main.cpp.o -o hello_binary -rdynamic libhello_library.a
```

[官方英文文档关于这三个范围的理解](https://cmake.org/cmake/help/v3.0/command/target_include_directories.html)

对于 `target_link_libraries( hello_binary PRIVATE hello_library )`
这个命令中的 scope 关键字, `private`, `public` 以及 `interface`.
可以举例理解:

+ `public` 是说, 如果此工程被 `link` 了, 那此工程的 `target_link_libraries` 指定的 `lib` 也会被链
+ `private` 是说, 你 `link` 的这些 `libs` 不会被暴露出去.

+ 比如你的工程 `B` 是个 `dll`, `public` 连接了 `C`, `D`,
这个时候你的 `A.exe` 要链接 `B`, 那么它也会链接 `C`和 `D`
+ 如果 `B`是 `private` 链接了 `C`, `D`, 那么 `A` 链 `B` 的时候, 不会链 `C`和 `D`

那么, `A.exe` 链接 `B` 的时候, 其实也有 `public` 和 `private` 的选项,
但是因为没有其他东西链接A, 所以不起作用.
所以设置这个属性, 主要是针对当前工程被其它工程 link(引用)的设置,

对于 `hello_binary`, 它不是库, 所以不会被链接. 直接 `private`, 自己用这个库就行.

三 构建示例

```bash
$ mkdir build
$ cd build
$ cmake ..

$ make
$ ls
CMakeCache.txt  CMakeFiles  cmake_install.cmake  hello_binary  libhello_library.a  Makefile
$ ./hello_binary
```
