# cmake basic

[gitlab.kitware.com](https://gitlab.kitware.com/cmake/cmake)

## 从源码安装

### 安装准备

[cmake Source distributions:](https://cmake.org/download)
[Ubuntu安装openssl-devel](https://blog.csdn.net/daxiyou/article/details/79349889)

您需要安装 C++ 编译器(支持 C++11)和 `make`.

+ 在 `Ubuntu 22.04 WSL2` 上安装, 使用系统提供的 `gcc-11`工具链;
或者使用自定义工具链, 需要配置运行库的路径

+ 需要安装openssl-devel

```bash
sudo apt install libssl-dev
```

### 编译代码

+ 运行 CMake 源代码目录下的 `bootstrap` 脚本.
    + 你可以使用 `--help` 选项查看支持的选项.
    + 您可以使用 `--prefix=<install_prefix>` 选项指定 CMake 的自定义安装目录.
+ 一旦安装成功, 运行 `make` 和 `make install`.

```bash
mkdir build && cd build
#配置构建
../bootstrap  --prefix=$HOME/myProg/cmake --parallel=4
make -j 4 # 构建 in 4 threads
make install # 安装
```

例如, 如果您只想从源代码编译和安装 CMake,
您可以直接在源代码树中编译: :

```bash
./bootstrap && make && sudo make install
```

或者, 如果您计划开发 CMake 或运行测试套件, 可创建单独的构建树:

```bash
mkdir build && cd build
../bootstrap && make
```

### Windows

[CMake Download Page]: https://cmake.org/download
[MSYS2]: https://www.msys2.org/

在 Windows 下编译 CMake 有两种方法:

1. 使用 VS 2015 或更高版本的 MSVC 编译.
您需要下载并安装 CMake 的二进制版本.
您可以从 [CMake Download Page][] 二进制版本.
然后按照 *使用 CMake 构建 CMake* 的说明.

2. 在 MSYS2 下使用 MinGW Bootstrap.
下载并安装 [MSYS2][]. 然后安装所需的构建工具: :

```bash
pacman -S --needed git base-devel mingw-w64-x86_64-gcc
```

并按上述步骤 bootstrap.

## 使用 CMake 构建 CMake

[运行 CMake]: https://cmake.org/runningcmake
[Sphinx]: https://sphinx-doc.org

使用 **基于CMake的构建系统**, 您可以像构建其他项目一样构建 CMake:
在此 CMake 的源代码上运行 **已安装的CMake**,
并使用您偏好的选项和生成器. 然后构建并安装.

有关操作说明, 请参阅 [运行 CMake][] 文档.

要编译文档, 请安装 [Sphinx][] 并用以下命令配置 `CMake`:

```bash
-DSPHINX_HTML=ON # build html, or
-DSPHINX_MAN=ON # build man page
```

如果不能自动找到该工具, 请添加 `-DSPHINX_EXECUTABLE=/path/to/sphinx-build`
