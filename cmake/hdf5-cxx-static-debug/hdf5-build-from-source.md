# 如何用cmake链接 Windows上的 静态运行库 STATIC RUNTIME LIBRARY

用户可能希望在 Windows 上链接到静态运行时库, 以避免链接器错误和与其他库不兼容.
链接到  static runtime library 意味着 runtime  被链接到最终的exe中.

你必须用 CMake 从源码(source) 构建才能做到这一点.
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

+ 从 Support Portal 的[Downloads页面][]中选择一个特定的HDF5版本(向下滚动以查看版本).
在特定的版本页面上, 看到 "Files" 下的表格.
选择 `CMake-hdf5-N.N.N.tar.gz` 或 `CMake-hdf5.N.N.zip` 文件.

>注意: 用户不应使用HDF5-1.10.3之前的1.10版本.
>更多信息请参见HDF5-1.10版本的软件变化页面.

+ 在HDF Group网站上查看[最新的HDF5源代码](https://www.hdfgroup.org/downloads/hdf5/source-code/).
(向下滚动以查看Cmake版本).

必须安装[CMake](http://www.cmake.org/).
配置脚本至少需要3.12版本的CMake, 推荐使用3.15版本.

+ 在目录路径名称中不能使用空白的空格, 因为这将导致构建失败.

+ (可选)在Windows上, 应该安装NSIS或WiX, 以便用CPack创建一个安装镜像.
NSIS将创建一个.exe的安装程序. WiX将创建一个.msi安装程序.

[Downloads页面]: https://portal.hdfgroup.org/display/support/Downloads

## 改变编译选项

[HOW TO CHANGE HDF5 CMAKE BUILD OPTIONS](https://portal.hdfgroup.org/display/support/How+to+Change+HDF5+CMake+Build+Options)

[Troubleshooting](https://portal.hdfgroup.org/display/support/Building+HDF5+with+CMake)

My platform/compiler is not included. Can I still use the configuration files?

Yes, you can but you will have to edit the HDF5config.cmake file and update the variable:

   CTEST_CMAKE_GENERATOR
Other variables may be updated for informational purposes but are not required (for example, SITE_OS_BITS).

The generators for your platform can be seen by typing:
cmake --help
