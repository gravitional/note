# cmake 构建例子

## [quickjs](https://github.com/quickjs-ng/quickjs)

+ 打开 CMake GUI, 修改安装位置,

```cmake
CMAKE_INSTALL_PREFIX -> C:/Users/qingz/Downloads/install-demo
```

然后分两遍构建:

+ 首先勾选

```cmake
BUILD_EXAMPLES
BUILD_QJS_LIBC
BUILD_STATIC_QJS_EXE
```

不勾选 `BUILD_SHARED_LIBS`,
这样的到 `qjs.exe`, `qjs.lib`,

+ 第二遍再勾选 `BUILD_SHARED_LIBS`
这样得到动态库 `qjs.dll`

安装使用命令:

```bash
cmake --install . --config release
```