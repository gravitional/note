# Building with ninja

[TOC]

[The Ninja build system](https://ninja-build.org/manual.html)

## 介绍

如前所述, `CMake` 是一个元构建系统, 可用于为许多其他构建工具创建构建文件.
这个例子展示了如何让CMake使用ninja构建工具.

## 文件树

```bash
$ tree
.
├── CMakeLists.txt
├── main.cpp
```

+ `CMakeLists.txt` - CMake命令
+ `main.cpp` - 一个简单的"Hello World" cpp文件.

## 解析

### 生成器

[cmake-generators](https://cmake.org/cmake/help/v3.0/manual/cmake-generators.7.html)

CMake 负责为底层构建(build)系统编写 输入文件(例如 `Makefiles`).
运行 `cmake --help` 会显示可用的生成器(generators).
对于 cmake v2.8.12.2, 我的系统上支持的生成器包括:

```bash
Generators

The following generators are available on this platform:
  Unix Makefiles              = Generates standard UNIX makefiles.
  Ninja                       = Generates build.ninja files (experimental).
  CodeBlocks - Ninja          = Generates CodeBlocks project files.
  CodeBlocks - Unix Makefiles = Generates CodeBlocks project files.
  Eclipse CDT4 - Ninja        = Generates Eclipse CDT 4.0 project files.
  Eclipse CDT4 - Unix Makefiles
                              = Generates Eclipse CDT 4.0 project files.
  KDevelop3                   = Generates KDevelop 3 project files.
  KDevelop3 - Unix Makefiles  = Generates KDevelop 3 project files.
  Sublime Text 2 - Ninja      = Generates Sublime Text 2 project files.
  Sublime Text 2 - Unix Makefiles
                              = Generates Sublime Text 2 project files.Generators
```

如[本文][] 所指定, CMake包括不同类型的生成器, 例如命令行, IDE和其他生成器.

## Command-Line Build Tool Generators 命令行编译工具生成器

这些 generators 用于命令行构建工具, 例如 `Make` 和 `Ninja`.
在使用 CMake 生成 build system 之前, 必须先配置所选的工具链(tool chain).

支持的生成器包括:

+ Borland Makefiles
+ MSYS Makefiles
+ MinGW Makefiles
+ NMake Makefiles JOM
+ Ninja; ninja用的
+ Unix Makefiles; make用的
+ Watcom WMake

## IDE Build Tool Generators

这些 generators 用于自带编译器 的IDE.  示例是Visual Studio和Xcode.
The supported generators include:

+ Visual Studio 6
+ Visual Studio 7
+ Visual Studio 7 .NET 2003
+ Visual Studio 8 2005
+ Visual Studio 9 2008
+ Visual Studio 10 2010
+ Visual Studio 11 2012
+ Visual Studio 12 2013
+ Xcode

## Extra Generators

这些 generators, 用于生成供其他 `IDE` 工具一起使用的 configuration, 并且必须包含(included)在IDE或 命令行生成器中.

The supported generators include:

+ CodeBlocks
+ CodeLite
+ Eclipse CDT4
+ KDevelop3
+ Kate
+ Sublime Text 2

>Note; 安装ninja的命令
>
>```bash
>sudo apt-get install ninja-build
>```

## 调用生成器Calling a Generator

使用 `-G` 参数来调用 `CMake` 的生成器

```cmake
cmake .. -G Ninja
```

完成上述操作后, `CMake` 将生成所需的 `Ninja` 构建文件,
可以使用 `ninja` 命令运行生成的文件

```bash
$ cmake .. -G Ninja
$ ls
build.ninja  CMakeCache.txt  CMakeFiles  cmake_install.cmake  rules.ninja
```

## 构建示例

```bash
$ mkdir build.ninja

$ cd build.ninja/

$ cmake .. -G Ninja

$ ninja -v
$ ls
build.ninja  CMakeCache.txt  CMakeFiles  cmake_install.cmake  hello_cmake  rules.ninja

$ ./hello_cmake
Hello CMake!
```

[本文]: https://stackoverflow.com/questions/25941536/what-is-a-cmake-generator
