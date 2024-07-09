# deal.ii.md

[dealii/dealii](https://github.com/dealii/dealii)

## 什么是 deal.II

deal.II 是一个 C++ 程序库, 主要用于使用 自适应有限元(adaptive finite elements) 求解 偏微分方程(partial differential equations).
它采用了最先进的编程技术, 为您提供了一个现代界面, 使您可以使用所需的 复杂数据结构 和 算法.

简短版:
假设你已经解压了 `.tar.gz` 文件到 `/path/to/dealii/sources` 目录.
然后就可以 configure, compile 并 install `deal.II` 库, 方法如下

```bash
# 新建 make 文件目录
mkdir build-msvc && cd build-msvc
# windows 下路径分隔符用 / 或者 \\, 指定构建类型为 Release; 构建目录为 ., 源代码目录为 ..
cmake -B . -S .. -G "Visual Studio 17 2022"  -DCMAKE_INSTALL_PREFIX=C:/cppLibs/deal.ii -DCMAKE_BUILD_TYPE=Release
# 构建项目; 或使用 make install; 或 make -j<N> install
cmake --build . -j 10
# 测试 make test
```

要从 repository 构建, 请先执行以下命令:

```bash
$ git clone https://github.com/dealii/dealii
$ cd dealii
```

### linux 下构建

```bash
# 新建 make 文件目录
mkdir build-msvc && cd build-msvc
# windows 下路径分隔符用 / 或者 \\, 指定构建类型为 Release; 构建目录为 ., 源代码目录为 ..
cmake -B . -S .. -G 'Unix Makefiles' -DCMAKE_INSTALL_PREFIX=/home/tom/cppLibs -DCMAKE_BUILD_TYPE=Release
# 构建项目; 或使用 make install; 或 make -j<N> install
cmake --build . -j 10
# 若 Makefile 由CMake创建, 可以运行 `make help` 查看可以编译的 target

# 测试 make test
```

然后像之前一样继续.
详细的 ReadME 可以在 `./doc/readme.html`, `./doc/users/cmake_user.html` 或 [dealii.org][] 上找到.

## 开始

教程 位于安装目录的 `examples/` 下.
有关教程步骤的信息, 请访问 `./doc/doxygen/tutorial/index.html` 或 [dealii.org][].
deal.II 支持在 GDB 内漂亮地打印 deal.II 对象.
有关如何设置的说明, 请参见 `contrib/utilities/dotgdbinit.py` 或 新文档页面(在 "用户信息" 下).

更多信息, 请访问 `./doc/index.html` 或 [dealii.org][].

## Docker 映像

Docker images based on the Ubuntu operating system are available on Docker Hub.
You can use any of the available version (list of available tags) by running, for example:

```bash
$ docker run --rm -t -i dealii/dealii:master-focal
```

The above command would drop you into an isolated environment,
in which you will find the latest version of deal.II (master development branch) installed under /usr/local.

[dealii.org]: https://www.dealii.org/

## 支持的 System Architectures

### 我能在 Windows 平台上使用 deal.II吗?

deal.II 在开发时考虑到了类似 Unix 的环境, 这在有关构建系统和编译器的许多地方都有所体现.
在许多地方都显示了构建系统和编译器支持.
尽管如此, 如果您有一台 Windows 机器, 还是有多种方法让 deal.II 运行.

#### 在 Windows 子系统中运行 deal.II. Windows Subsystem for Linux

在 Windows Linux 子系统中运行 deal.II
Windows 10 增加了一个兼容性层, 可在 Windows 上运行 Linux 二进制程序.
详见[维基百科页面](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux).
这意味着您不必使用 虚拟化 或 双启动 来安装全功能 Linux 发行版!
我们将在 [Windows 的维基页面上](https://github.com/dealii/dealii/wiki/Windows) 总结安装过程.

从deal.II 8.4.0开始, 我们试验性地支持微软Visual Studio(2013和2015).
更多详情, 请参阅 [Windows单独页面](https://github.com/dealii/dealii/wiki/Windows).

#### 通过 virtual box 运行 deal.II

试用 deal.II 的最简单方法是在 premade virtual machine.
您可以从以下网址下载 VirtualBox 的虚拟机
[http://www.math.clemson.edu/~heister/dealvm/](http://www.math.clemson.edu/~heister/dealvm/)
然后在 windows 中运行.

请注意, 您的体验取决于您的机器有多强大.
建议使用建议使用 4GB 以上的内存. 最好安装本地 Linux(见下文).

#### 用 Ubuntu 双启动你的机器

作为 Windows 用户安装 Linux 的最简单方法就是双启动.
双启动意味着你只需在电脑上安装第二个操作系统
双启动的意思是, 你只需在电脑上安装第二个操作系统, 然后在启动机器时选择启动哪一个.
大多数版本的 Linux 都支持将自己安装为第二个操作系统.
其中一个例子就是将 Ubuntu 安装程序用于 Windows.
这个安装程序会以安全, 完全可逆的方式自动双启动你的系统.
只需按照 [install-ubuntu-with-windows](http://www.ubuntu.com/download/desktop/install-ubuntu-with-windows)

如果将来您想从系统中删除 Ubuntu, 请从 Windows 程序管理器(旧版本和新版本中的添加-删除程序)中删除 Ubuntu,
从 Windows 程序管理器(旧版本中的 "添加-删除程序 "和新版本中的 "程序和功能")中删除 Ubuntu,
就像卸载其他程序一样.
