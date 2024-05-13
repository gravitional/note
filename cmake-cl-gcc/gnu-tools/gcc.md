# [gcc](https://gcc.gnu.org/)

## ubuntu 安装gcc 特定版本

ubunt上[安装某个特定版本的 gcc 套装, 并启用](https://askubuntu.com/questions/1500017/ubuntu-22-04-default-gcc-version-does-not-match-version-that-built-latest-defaul)

例如安装 gcc-10

```bash
sudo apt install gcc-10 g++-10

sudo ln -s -f /usr/bin/gcc-10 /usr/bin/gcc
sudo ln -s -f /usr/bin/g++-10 /usr/bin/g++
```

## [下载安装](https://gcc.gnu.org/wiki/InstallingGCC)

通过[镜像地址](https://gcc.gnu.org/mirrors.html) 下载源码压缩包,

### 安装 GCC

本页旨在提供指导, 避免在安装 GCC 时出现一些常见问题,
官方安装文档位于 GCC 主文档的 [安装 GCC](http://gcc.gnu.org/install/index.html) 部分.
注意: 这些安装文档指的是开发主干, 已发布版本的安装说明包含在发布源代码中.

对于大多数人来说, 安装 GCC 的最简单方法是安装为操作系统制作的软件包.
GCC 项目不提供 GCC 的预制二进制文件, 只提供源代码, 但所有 GNU/Linux 发行版都包含 GCC 的软件包.
基于 BSD 的系统将 GCC 包含在它们的端口集合中.
对于其他操作系统, [安装 GCC: 二进制文件](http://gcc.gnu.org/install/binaries.html)
页面列出了一些 GCC 二进制文件的第三方来源.

如果找不到适合自己系统的二进制文件, 或者需要比现有版本更新的版本, 则需要从源代码编译 GCC 才能安装.

### 构建 GCC

很多人在没有正确阅读 [安装文档](http://gcc.gnu.org/install/index.html)
的情况下就匆忙尝试构建 GCC, 结果犯了以下一个或多个常见错误:

+ 不要在源代码目录下运行 `./configure`, 这是不支持的.
您需要在源代码目录之外, 在为编译创建的单独目录中运行 configure
(这是一个[常见问题](https://gcc.gnu.org/wiki/FAQ#configure))
+ 如果 GCC 动态链接到 `GMP`, `MPFR` 或 `MPC` 支持库,
那么在构建 gcc 和使用已安装的编译器时,
相关共享库都必须位于动态链接器的路径中(这也是常见问题之一)

### 支持库

请参阅 [安装GCC: 编译GCC所需的软件的先决条件](http://gcc.gnu.org/install/prerequisites.html).
如果您的操作系统中尚未安装 GMP, MPFR 和 MPC 支持库, 那么有 两种简单的方法 和 一种困难且容易出错的方法.
出于某种原因, 大多数人选择了困难的方法. 简单的方法是

+ 如果操作系统提供足够新的版本, 使用操作系统软件包管理系统将支持库安装到标准系统位置.
对于基于 Debian 的系统, 包括 Ubuntu, 应安装 `libgmp-dev`, `libmpfr-dev` 和 `libmpc-dev` 软件包.
对于基于 RPM 的系统(包括 Fedora 和 SUSE), 应安装 `gmp-devel`, `mpfr-devel` 和 `libmpc-devel`
(或 SUSE 上的 mpc-devel)软件包. 这些软件包会将库和头文件安装到标准系统目录中, 以便在编译 GCC 时自动找到它们.

```bash
sudo apt install libgmp-dev libmpfr-dev libmpc-dev
```

+ 或者, 在解压缩 GCC 源代码压缩包后, 只需在 GCC 源代码目录下运行 `./contrib/download_prerequisites` 脚本.
这将下载支持库并创建符号链接, 使其作为 GCC 编译过程的一部分自动编译.
如果想在不使用 ISL 的情况下编译 GCC, 请在脚本中设置 GRAPHITE_LOOP_OPT=no,
因为 ISL 仅适用于可选的 Graphite 循环优化.

不推荐使用的困难方法是下载 GMP, MPFR 和 MPC 的源代码, 然后将它们配置并安装到非标准位置,
然后使用

```bash
--with-gmp=/some/silly/path/gmp --with-mpfr=/some/silly/path/mpfr --with-mpc=/some/silly/path/mpc
```

配置 `GCC`, 然后强制设置

```bash
LD_LIBRARY_PATH=/some/silly/path/gmp:/some/silly/path/mpfr:/some/silly/path/mpc/lib
```

对于不了解动态链接器如何在运行时查找库的人来说, 这样做是愚蠢的, 而且会带来很大的问题.
请勿这样做. 如果使用 --with-gmp 或 --with-mpfr 或 --with-mpc 选项时 GCC 编译失败,
那么你可能就不应该使用这些选项.

### 配置

请参阅[安装 GCC: 配置](http://gcc.gnu.org/install/configure.html), 查看完整文档.
在源代码目录外运行 `srcdir/configure`(而不是运行 `./configure`)的一个主要好处是,
源代码目录不会以任何方式被修改, 因此如果编译失败或想重新配置并再次编译,
只需删除 `objdir` 中的所有内容并重新开始即可.

例如, 配置和编译 GCC 4.6.2(支持 C, C++, Fortran 和 Go)应该很简单:

```bash
tar xzf gcc-4.6.2.tar.gz
cd gcc-4.6.2
./contrib/download_prerequisites
cd ..
mkdir objdir
cd objdir
# --disable-multilib 禁用 32 位 build
$PWD/../gcc-14.1.0/configure --prefix=$HOME/gcc-14.1.0 --enable-languages=c,c++,fortran,go --disable-multilib
make
make install
```

make 这一步需要很长时间. 如果你的电脑有多个处理器或内核,
你可以使用 `make -j 4`(或更高的数字以获得更多并行性)并行编译, 从而加快编译速度.

在我的台式机上, 使用了

```bash
make -j 12 #等于物理核心数目, 太大的数字会导致内存不足
```

如果编译失败, 而 `configure` 命令又有很多复杂的选项, 你应该尝试删除选项, 保持简单.
不要添加大量你不理解的 `configure` 选项, 它们可能是导致编译失败的原因.
