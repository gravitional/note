# 设置C++ 标准

自 C++11 和 C++14 发行以来, 普遍做法是调用编译器以使用这些标准.
随着CMake的发展, 有了新的使用这些标准的方式.

以下示例显示了设置 C++ 标准的不同方法, 以及提供哪些版本的CMake.

+ `common-method`; 可以与大多数版本的CMake一起使用的[简单方法](https://sfumecjf.github.io/cmake-examples-Chinese/01-basic/i-common-method).
+ `cxx-standard`; 使用 CMake v3.1 中引入的 [CMAKE_CXX_STANDARD变量](https://sfumecjf.github.io/cmake-examples-Chinese/01-basic/ii-cxx-standard).
+ `compile-features`; 使用 CMake v3.1 中引入的 [target_compile_features函数](https://sfumecjf.github.io/cmake-examples-Chinese/01-basic/iii-compile-features).

## C++ Standard Common Method

此示例显示了设置C 标准的常用方法,可以与大多数版本的CMake一起使用.
但是, 如果有CMake的最新版本建议使用其他更便捷的方法.

文件结构如下:

```bash
A-hello-cmake$ tree
.
├── CMakeLists.txt
├── main.cpp
```

+ CMakeLists.txt - 包含 CMake 命令.
+ main.cpp - A simple "Hello World" cpp file targeting C++11.

## Concepts

### Checking Compile flags

可以把 flags 传递给函数 `CheckCXXCompilerFlag`, `Cmake` 将检查编译器是否支持使用这些标志.
然后它将检查结果存入你指定的变量.例如:

```cmake
include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
```

这个例子将使用 flag `-std=c11` 编译程序, 然后把结果存储到变量 `COMPILER_SUPPORTS_CXX11` 中.
欲使用这个函数, 必须先使用 `include(CheckCXXCompilerFlag)` 包含这个函数.

### Adding the flag

一旦确定了编译器是否支持 `标志`, 即可使用标准 cmake 方法将此标志添加到 `target`.
在此示例中, 我们使用 `CMAKE_CXX_FLAGS` 将 `flag` (c++标准)传播给所有目标.

```cmake
if(COMPILER_SUPPORTS_CXX11)#
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
elseif(COMPILER_SUPPORTS_CXX0X)#
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
else()
    message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support. Please use a different C++ compiler.")
endif()
```

上面的示例仅检查 `编译标志` 的gcc版本,
并能在不支持的情况下, 从 `C+11` 回滚到 `c++` 标准化之前的 `c++0x` 标志.
在实际使用中, 您可能需要检查 `C14`, 或以不同的方式设置 `编译方法`, 例如 `-std = gnu11`

## Building the Examples

```bash
$ mkdir build
$ cd build
$ cmake ..

$ make VERBOSE=1
```

## Windows C++ 11标志

[c++11 - Windows Makefile中的cl选项是否接受-std=c++11标志](https://www.coder.work/article/6207443)
[/std (Specify Language Standard Version]: https://learn.microsoft.com/en-us/cpp/build/reference/std-specify-language-standard-version?view=msvc-170

请参见 [/std (Specify Language Standard Version][],
`cl` 仅接受选项

+ `/std:c++14`
+ `/std:c++17`
+ `/std:c++20`
+ `/std:c++latest`
+ `/std:c11`
+ `/std:c17`

默认情况下已启用 `C++11` 功能.
