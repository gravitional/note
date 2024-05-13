# 导入第三方库到目标

[TOC]

## 介绍

[IMPORTED]: https://cmake.org/cmake/help/v3.6/prop_tgt/IMPORTED.html#prop_tgt:IMPORTED

如先前在第三方库中所述,
较新版本的 `CMake` 允许您使用导入的 `ALIAS` [IMPORTED][] 目标链接第三方库.

本教程中的文件树如下:

```bash
$ tree
.
├── CMakeLists.txt
├── main.cpp
```

+ `CMakeLists.txt` - CMake指令
+ `main.cpp` - 源文件

## 环境版本

+ CMake v3.5+
+ 安装在默认位置的boost库

## 解析

### 导入目标

`Imported targets` 是 `FindXXX` 模块导出的只读 Target.

在CMake命令中包含boost这个库:

```cmake
  target_link_libraries( imported_targets
      PRIVATE
          Boost::filesystem
  )
```

作用是自动链接 `Boost::filesystem` 和 `Boost::system` 库, 同时还包括 Boost 头文件目录.

## 编译例子

```bash
$ mkdir build
$ cd build/
$ cmake ..
$ make
$ ./imported_targets
```
