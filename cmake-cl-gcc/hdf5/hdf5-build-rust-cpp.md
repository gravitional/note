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

## [hdf5-1.14.6.tar.gz cmake 构建](https://github.com/HDFGroup/hdf5)

github Release 页面可以下载最新源码.
帮助文档在 项目根目录 `release_docs` 下面,
CMake 构建对应的说明文件是 `release_docs/INSTALL_CMake.txt`,

首先下载一些额外依赖库

+ External compression plugin libraries
  + [hdf5_plugins.tar.gz](https://github.com/HDFGroup/hdf5_plugins)

+ External compression szip and zlib libraries:
  + [ZLIB](https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz)
  + [ZLIBNG](https://github.com/zlib-ng/zlib-ng/archive/refs/tags/2.2.2.tar.gz)
  + [LIBAEC](https://github.com/MathisRosenhauer/libaec/releases/download/v1.1.3/libaec-1.1.3.tar.gz)

其中 ZLIB 和 ZLIBNG 选择一个就行; `LIBAEC` 又称为 `Szip`.

+ 新建一个目录, 把这些压缩包都放进去, 我放在 download 目录下

+ 查询文档之后, 选择如下的 CMake 配置:
下面是对应的 nushell 下的 cmake 命令行,
其他命令行类似, 可能需要续行符

+ 构建CMake项目

```bash
(
cmake -G "Visual Studio 17 2022" -A x64 -DCMAKE_INSTALL_PREFIX='C:/Users/qingz/Downloads/hdf5-install'
# ----------------- on
-DCMAKE_BUILD_TYPE:STRING=Release
-DBUILD_SHARED_LIBS:BOOL=ON # 构建动态库
-DHDF5_BUILD_HL_LIBS:BOOL=ON
-DHDF5_BUILD_CPP_LIB:BOOL=ON
-DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON
-DHDF5_ENABLE_SZIP_SUPPORT:BOOL=ON
-DHDF5_BUILD_TOOLS:BOOL=ON
-DHDF5_BUILD_UTILS:BOOL=ON
# --------------- off
-DHDF5_BUILD_EXAMPLES:BOOL=OFF
-DHDF5_BUILD_TESTING:BOOL=OFF
-DHDF5_BUILD_DOCS:BOOL=OFF
# ------------ use local disk files
-DTGZPATH:STRING='C:/Users/qingz/ownloads'
-DZLIB_USE_LOCALCONTENT:BOOL=ON
-DLIBAEC_USE_LOCALCONTENT:BOOL=ON
-DPLUGIN_USE_LOCALCONTENT:BOOL=ON
# ----------- Zlib, Szip(aka libaec)
-DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING="TGZ"
-DZLIB_TGZ_NAME:STRING="zlib-1.3.1.tar.gz" #or ZLIBNG_TGZ_NAME:STRING="zlibng_src.ext"
-DLIBAEC_TGZ_NAME:STRING="libaec-1.1.3.tar.gz"
# ----------- hdf5 plugins
-DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING="TGZ"
-DPLUGIN_TGZ_NAME:STRING="hdf5_plugins-1.14.tar.gz"
-B . -S ..
)
```

+ 编译, 链接

```bash
cmake --build . --config Release -j
```

+ 安装

```bash
cmake --install . --config release
```

## msys2 上编译

安装依赖库

```bash
pacman -S mingw-w64-x86_64-zlib mingw-w64-x86_64-libaec
```

```bash
args=(
 -G Ninja
 -DCMAKE_BUILD_TYPE:STRING=Release
 -DBUILD_SHARED_LIBS:BOOL=ON
 -DHDF5_BUILD_HL_LIBS:BOOL=ON
 -DHDF5_BUILD_CPP_LIB:BOOL=ON
 -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=ON
 -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=ON
 -DHDF5_BUILD_TOOLS:BOOL=ON
 -DHDF5_BUILD_UTILS:BOOL=ON
 -DHDF5_BUILD_EXAMPLES:BOOL=OFF
 -DHDF5_BUILD_TESTING:BOOL=OFF
 -DHDF5_BUILD_DOCS:BOOL=OFF
 -DPLUGIN_TGZ_NAME:STRING="hdf5_plugins-1.14.tar.gz"
 -B . -S ..
)
# cmake "${args[@]}" # for bash
cmake $args
```
