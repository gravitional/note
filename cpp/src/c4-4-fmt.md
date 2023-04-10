# fmt 库

[fmt: 现代的 C++ 字符串格式化库, 实现了 C++20 的特征](https://www.jianshu.com/p/fdca0fde50ac)

## 导入依赖

[c++ fmt 库安装和使用示例, clion配置](https://www.jianshu.com/p/fdca0fde50ac)

下载源码, 可以先 `clone` 到你的项目中去, [fmt](https://github.com/fmtlib/fmt) ,
我放到的是项目的 dependencies 目录
然后在 `CMakeList.txt` 中加上这两句:

```cmake
add_subdirectory(dependencies/fmt EXCLUDE_FROM_ALL)
target_link_libraries(项目名字 fmt-header-only)
```

其中 `EXCLUDE_FROM_ALL` 表示将这个项目移除出 `make` 列表.
接着是链接 `fmt-header-only` 这个库, 使用 `源码` 和 `生成的库文件`,

## fmt Usage

[Usage with CMake](https://fmt.dev/latest/usage.html)
You can add the fmt library directory into your project and include it in your CMakeLists.txt file:

```cmake
add_subdirectory(fmt)
```

or

```cmake
add_subdirectory(fmt EXCLUDE_FROM_ALL)
```

to exclude it from `make`, `make all`, or `cmake --build .` .

You can detect and use an installed version of `{fmt}` as follows:

```cmake
find_package(fmt)
target_link_libraries(<your-target> fmt::fmt)
```

Setting up your target to use a header-only version of fmt is equally easy:

```cmake
target_link_libraries(<your-target> PRIVATE fmt::fmt-header-only)
```
