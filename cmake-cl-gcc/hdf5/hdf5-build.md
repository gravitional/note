# hdf5 build cmake, 2025,0326

[hdf5 源代码下载地址](https://github.com/HDFGroup/hdf5)
编译过程需要用到 zlib, 首先下载 zlib，编译安装
[zlib 下载地址](https://github.com/madler/zlib/releases/tag/v1.3.1)

## build zlib

下载 zlib release 版本源码包, .tar.gz 或者 .zip 格式，解压，
然后在 Cmake Gui 界面中打开，
设置 src 和  build 目录位置, 例如

```bash
# where is the source code 
C:/Users/qingz/Downloads/zlib-1.3.1
# where to build the binaries
C:/Users/qingz/Downloads/zlib-1.3.1/build
```

(如果CMake界面选项开启了分组--Grouped),
在CMAKE那一节找到 `CMAKE_INSTALL_PREFIX`，当前直接搜索也可以,
修改成你想要的安装目录,例如 `C:/cppLibs/zlib`
对应命令行选项 `-DCMAKE_INSTALL_PREFIX=C:/cppLibs/zlib`

然后就是 Configure, Generate, Open Project, 切换到项目的 Release 版本, build.

### build hdf5

类似地, 下载 hdf5 源码, 设置 src 和 build 目录,
修改 CMAKE变量,

+ 点击 `Add Entry`, 添加 `CMAKE_PREFIX_PATH`, 
type 为 `PATH`, value 为 `${CMAKE_PREFIX_PATH};C:/cppLibs/zlib`
这一步是为了设置 zlib 的路径, 让 CMake 可以找到 zlib 的 lib 和 include.

+ 同样，搜索 `CMAKE_INSTALL_PREFIX`, 修改为 `C:/cppLibs/hdf5`,

然后 Configure, Generate, build 相同。

```nu
$env.HDF5_DIR = 'C:/cppLibs/hdf5-1.10.5/';
$env.Path = ['C:/cppLibs/hdf5-1.10.5/bin/','C:/cppLibs/hdf5-1.10.5/lib/','C:/cppLibs/hdf5-1.10.5/include/'] ++ $env.Path
# $env.RUST_BACKTRACE = 1;
cargo build
```
