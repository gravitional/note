# 如何用cmake链接 Windows上的 静态运行库 STATIC RUNTIME LIBRARY

用户可能希望在 Windows 上链接到 `static runtime library`, 以避免 链接器错误, 以及与其他库不兼容.
链接到  `静态运行时库` 意味着 runtime  被链接到 最终的 `exe` 中.

你必须用 `CMake` 从源码(source) 构建才能做到这一点.
见[用CMake构建HDF5或用CMake构建HDF4](https://portal.hdfgroup.org/display/support/Building+HDF4+with+CMake).

在构建前对源代码做如下修改.

+ 编辑 `./<HDF source>/config/cmake/UserMacros/Windows_MT.cmake` 文件.
根据需要设置正确的选项 (`/MTd`, `/MT`, ......).
打开 `BUILD_STATIC_CRT_LIBS` 选项:

```cmake
option (BUILD_STATIC_CRT_LIBS "Build With Static CRT Libraries" ON)
```

编辑 `./<HDF source>/UserMacros.cmake` 文件,
添加如  `Window_MT.cmake` 文件顶部所示的 `include` 行(并添加正确路径).

```cmake
INCLUDE(path_to_file/WINDOWS_MT.cmake)
```

然后你就可以构建 `HDF` 了.

## Preconditions

[BUILDING HDF5 WITH CMAKE](https://portal.hdfgroup.org/display/support/Building+HDF5+with+CMake)

必须已经下载了 `CMake HDF5` 源码发布文件(用于Unix或Windows).

你可以通过以下两种方式获得 `CMake HDF5` 源文件.

+ 从 Support Portal 的[Downloads页面][] 中选择一个特定的HDF5版本(向下滚动以查看版本).
在特定的版本页面上, 看到 "Files" 下的表格.
选择 `CMake-hdf5-N.N.N.tar.gz` 或 `CMake-hdf5.N.N.zip` 文件.

>注意: 用户不应使用HDF5-1.10.3之前的1.10版本.
>更多信息请参见HDF5-1.10版本的软件变化页面.

+ 在HDF Group网站上查看[最新的HDF5源代码](https://www.hdfgroup.org/downloads/hdf5/source-code/).
(向下滚动以查看Cmake版本).

必须安装[CMake](http://www.cmake.org/).
配置脚本至少需要3.12版本的CMake, 推荐使用3.15版本.

+ 在目录路径名称中不能使用空白的空格, 因为这将导致构建失败.

+ (可选)在 `Windows` 上, 应该安装 `NSIS` 或 `WiX`, 以便用 `CPack` 创建安装镜像.
`NSIS`将创建`.exe` 的安装程序. `WiX` 将创建 `.msi` 安装程序.

[Downloads页面]: https://portal.hdfgroup.org/display/support/Downloads

## 编译应用程序

### 用于构建应用程序的CMake脚本

我们提供了简单的脚本来构建具有不同语言和选项的应用程序.
参见[用于构建应用程序的CMake脚本](https://portal.hdfgroup.org/display/support/CMake+Scripts+for+Building+Applications).

要想获得更完整的脚本(并帮助解决问题), 请看下面与 HDF5 示例一起提供的脚本.

### HDF5 示例

可以通过编译 包含 `CMake源代码` 的 `HDF5`示例程序 来验证构建.
如果用 `Visual Studio` 构建, 你也可以使用与这些例子一起创建的 `解决方案文件`(vs 项目文件).
这对诊断如何编译你的应用程序的问题有帮助.

源文件中提供了包含 示例的压缩文件, 名字为 `HDF5Examples-*`.
解压文件, 找到 `HDF5Examples` 目录, 按照 `Using_CMake.txt` 中的说明来构建示例.

一般来说, 用户必须首先将 `HDF5_DIR` 环境变量设置为 `HDF5` 的 `CMake` 配置文件的安装位置. 
例如, 在 `Windows` 上可以设置以下路径.

```powershell
HDF5_DIR=C:/Program Files/HDF_Group/HDF5/1.N.N/cmake
```

你可以看看 `HDF5` 示例中提供的 `find_package`, 了解如何编译一个应用程序. 
请注意, `FindHDF5.cmake` 不是由 HDF Group 提供的, 也不能由HDF集团修复.

## Troubleshooting

[HOW TO CHANGE HDF5 CMAKE BUILD OPTIONS](https://portal.hdfgroup.org/display/support/How+to+Change+HDF5+CMake+Build+Options)
[Troubleshooting](https://portal.hdfgroup.org/display/support/Building+HDF5+with+CMake)

+ 我可以成功地构建 `HDF5`, 但是 `findHDF5.cmake` 包没有填充 `HDF5_LIBRARIES` 变量.
如何使用构建好的的HDF5库?

`HDF Group` 不提供 `FindHDF5.cmake` 包.
HDF集团不能改变它. 然而, 你可以使用 `find_package`.
请参阅下面关于如何使用 `find_package` 的问题.

+ 如何在 `HDF5` 中使用 `find_package`?

要使用 `find_package` , 你首先需要确保 `HDF5_DIR` 设置正确.
关于设置这个环境变量, 请参见源代码中 `USING_HDF5_CMake.txt` 文件中的前提条件.

关于如何在 `HDF5` 中使用 `find_package`, 请参见随这些例子提供的 `CMakeLists.txt` 文件.

请注意, `find_package` 的调用改变为 require  `shared` 或 `static`.

```cmake
FIND_PACKAGE(HDF5 COMPONENTS C HL NO_MODULE REQUIRED shared)
FIND_PACKAGE(HDF5 COMPONENTS C HL NO_MODULE REQUIRED static)
```

以前, `find_package` 的调用方式是.

```camke
find_package(HDF5 COMPONENTS C HL No_module Required)
```

+ 我的平台/编译器不包括在内. 我还能使用 配置文件吗?
是的可以, 但你必须编辑 `HDF5config.cmake` 文件并更新变量.

    ctest_cmake_generator

其他变量可能会被更新以提供信息, 但不是必须的(例如, `SITE_OS_BITS`).
你的电脑上的生成器可以通过键入下列命令来查看.

```bash
cmake --help
```

+ 如果构建失败了, 我应该怎么办?

我在构建过程中收到了一个错误,
而且构建和压缩的 HDF5 二进制文件没有像我预期的那样在 `CMake-hdf5-N/build` 目录中.
我如何确定问题出在哪里?

如果错误不清楚, 那么你可能要做的第一件事是将构建脚本中 `ctest` 的 `-V` 选项改为 `-VV`(两个大写`V`).
然后删除构建目录, 重新运行构建脚本, 输出应该更详细.

如果错误仍然不清楚, 那么检查日志文件.
你可以在CMake-hdf5-N的构建目录中找到这些文件. 例如, 在 Unix 上, 日志文件将在.

   CMake-hdf5-N/build/Testing/Temporary/

configure, test 和 build 都有日志文件.

+ 如果我需要 rebuild  软件, 我该怎么做?
如果你必须重建 `HDF5`, 请先删除构建目录.

+ 库已经建立, 但没有二进制文件. 我应该怎么做?
要安装或打包二进制文件, 请在 `build/` 目录中运行 `make install` 或 `cpack`.
