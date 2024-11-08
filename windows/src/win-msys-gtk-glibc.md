# windows 编译 gtk+, glibc

[glib:windows下基于MSYS2环境编译glib2的过程](https://blog.csdn.net/10km/article/details/80399355)

## 安装依赖库

有了MSYS2安装依赖库就非常简单了, 以下是通过pacman安装所有编译glib所需要的工具和依赖库.

```bash
# 安装依赖库和必须的工具
pacman --needed --noconfirm -S automake autoconf make libtool unzip glib2-devel intltool pcre-devel   \
            mingw-w64-x86_64-toolchain mingw-w64-x86_64-pcre

# 可选工具用于生成文档
#pacman --needed --noconfirm -S gtk-doc
```

## 下载glib

[GNOME/glib](https://github.com/GNOME/glib)

```bash
# 从github上下载2.54.3版本的源码
wget https://github.com/GNOME/glib/archive/2.54.3.zip -O glib-2.54.3.zip
# 源码解压缩
unzip glib-2.54.3.zip || exit -1
```

## 编译

```bash
cd glib-2.54.3
# 第一次要执行autogen.sh才会生成./configure, 后续可以直接执行./configure来修改编译配置
./autogen.sh --prefix=/your/install/path
# 编译并安装到prefix指定的位置
make install -j8
```

## 生成MSVC import library (.lib)

问题来了, 在MSYS2下编译用的是 MinGW 编译器,
生成的导入库(import library)都后缀是 `.dll.a`, MSVC怎么使用呢?
其实MinGW生成的import library,MSVC是可以直接用的, 直接添加到msvc工程就可以.
但如果你是用cmake来组织项目,
在MSVC编译环境下 cmake 的 `find_library` 是找不到后缀为 `.dll.a` 的 `import library`.

怎么办呢? 其实 glib 的 Makefile 是支持在编译时生成MSVC的 `.lib .def` 文件的.
随便打开一个 glib 的 Makefile (比如 glib/Makefile), 就可以找到生成 `.lib` 的代码:

```bash
glib-2.0.lib: libglib-2.0.la glib.def
    $(AM_V_GEN) lib.exe -machine:X64 -name:libglib-2.0-$(LT_CURRENT_MINUS_AGE).dll -def:$(builddir)/glib.def -out:$@
```

可以看到 Makefile 生成 `.lib` 是调用MSVC的 `lib.exe`(在MSVC bin文件夹下)程序来完成的.
而默认情况下, `MSYS2` 环境中搜索路径($PATH)中没有 `MSVC` 编译器的安装位置(`bin`).
所以这一条命令不能执行, 不能生成 `lib`.
解决的办法就是执行编译之前将 `MSVC` 编译器的安装位置(`bin`)加入 `$PATH`:

```bash
# 以Visual Studio 2015为例
PATH="/C/Program Files (x86)/Microsoft Visual Studio 14.0/VC/bin":$PATH
```

`make install` 安装后的目录结构 `.dll.a`, `.lib` 都有了.

```bash
glib-2.54.3-x86_64
  ├── bin
  │   ├── gdbus.exe
  │   ├── ...
  │   └── libgthread-2.0-0.dll
  ├── include
  │   ├── gio-win32-2.0
  │   └── glib-2.0
  ├── lib
  │   ├── ...
  │   ├── libgthread-2.0.dll.a
  │   ├── libgthread-2.0.la
  │   └── pkgconfig
  └── share
      ├── aclocal
      |...
      ├── glib-2.0
      └── locale
```

整个安装, 下载, 编译过程的完整脚本的 [gitee仓库位置](https://gitee.com/l0km/faceapi/blob/master/faceapi-rpc-cpp/dependencies/msys2-build-glib.sh)
