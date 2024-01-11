# openBLAS LAPACK

[open blas user manual](https://github.com/OpenMathLib/OpenBLAS/wiki/User-Manual)

## openblas 包含 LAPACK

[Link against openblas; do I need still Lapack?](https://stackoverflow.com/questions/32925267/link-against-openblas-do-i-need-still-lapack)

OpenBLAS 文件夹中的 changelog.txt 上写道

```log
* 修补了 LAPACK, 修复了 114, 117, 118 号错误.
(http://www.netlib.org/lapack/bug_list.html)
```

在此页查看[源代码布局](https://github.com/xianyi/OpenBLAS/wiki/Developer-manual),

简单地说, openBLAS 编译时会同时编译 LAPACK,
但编译脚本只提供单一库 `libopenblas.so`.

在 msys2 下, 软连接可能有问题, 编译器找不到动态库中的函数,
简单的方法是手动复制

```bash
# 在 /openBLAS/lib 目录下
cp -a libopenblasp-r0.3.26.a libopenblas.a
cp -a libopenblasp-r0.3.26.a liblapack.a
cp -a libopenblas.dll.a liblapack.dll.a
# 在 /openBLAS/bin 目录下
cp -a libopenblas.dll liblapack.dll
```

## 编译 openBLAS, LAPACK

### 正常编译

输入 `make` 自动检测 CPU 类型.
or type `make TARGET=xxx` to set target CPU,
e.g. `make TARGET=NEHALEM`.
The full target list is in file TargetList.txt.

```bash
# 编译动态库版本
make DYNAMIC_ARCH=1
# 安装
make DYNAMIC_ARCH=1 install
```

### 安装到目录

```bash
make install PREFIX=your_installation_directory
```

默认目录为 `/opt/OpenBLAS`.
请注意, 在构建过程中传递给 make 的任何标志也应传递给 make install,
以避免出现任何安装错误, 例如某些头文件未被正确复制.
更多信息, 请阅读 [安装指南](https://github.com/OpenMathLib/OpenBLAS/wiki/Installation-Guide).

### openBLAS 中 LAPACK 相关的faq

[OpenBLAS/wiki/faq](https://github.com/OpenMathLib/OpenBLAS/wiki/faq)

+ OpenBLAS 和 GotoBLAS 有什么区别?
我们还添加了一些次要功能, 例如支持 "make install",
不使用 LAPACK 进行编译, 以及将 LAPACK 版本升级到 3.4.2.

+ OpenBLAS 支持稀疏矩阵和/或向量吗?
OpenBLAS 只实现了标准(稠密)BLAS 和 LAPACK 函数, 以及英特尔 MKL 所普及的少数扩展.
某些情况下可以使用 GEMV 或 AXPBY 等函数, 但一般情况下建议使用 SuiteSparse 等专用软件包(可以使用 OpenBLAS 或类似软件包进行标准操作).

+ 由于 linkerf error "multiple definition of `dlamc3_'", 我的构建失败了.
问题出在哪里?
如果缺少 GNU 补丁或我们的 LAPACK 补丁无法应用, 就会出现这种链接器错误.
背景:  OpenBLAS 实现了某些 LAPACK 函数的优化版本, 因此我们需要禁用参考版本.
如果这个过程失败, 我们就会重复实现相同的函数.

+ 我的构建工作很顺利, 通过了所有测试, 但运行 make lapack-test 却出现了故障
某些 LAPACK 测试, 尤其是 xeigtstz 中的测试, 会尝试在堆栈上分配大约 10MB 的资源.
你可能需要使用 `ulimit -s` 更改系统的默认限制, 以允许这样做.

### 在Mint/Ubuntu/Debian中替换系统BLAS/更新APT OpenBLAS

Debian 和 Ubuntu LTS 版本提供的 OpenBLAS 软件包在首次发布后没有更新,
在某些情况下, 人们可能希望使用更新版本的 OpenBLAS, 例如, 获得对更新 CPU 的支持.
Ubuntu 和 Debian 提供了 "替代 "机制,
可在全系统范围内轻松替换 BLAS 和 LAPACK 库.

成功编译 OpenBLAS 后(`DYNAMIC_ARCH` 设置为 1)

```bash
make clean
make DYNAMIC_ARCH=1
make DYNAMIC_ARCH=1 install
```

可以重定向 BLAS 和 LAPACK 替代程序, 使其指向源代码构建的 OpenBLAS 首先,
您必须安装 NetLib LAPACK 参考实现(以便有替代程序):

```bash
sudo apt install libblas-dev liblapack-dev
```

然后, 我们就可以为新构建的库设置替代方案了:

```bash
sudo update-alternatives --install /usr/lib/libblas.so.3 libblas.so.3 /opt/OpenBLAS/lib/libopenblas.so.0 41 \
   --slave /usr/lib/liblapack.so.3 liblapack.so.3 /opt/OpenBLAS/lib/libopenblas.so.0
```

或者删除重定向, 转回 APT 提供的 BLAS 实现顺序:

```bash
sudo update-alternatives --remove libblas.so.3 /opt/OpenBLAS/lib/libopenblas.so.0
```

在最近的发行版中, 库的安装路径已改为包含主机架构名称,
如 `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3` 或 `libblas.so.3.x86_64-linux-gnu`.
使用 `update-alternatives --display libblas.so.3` 查找您系统的布局.

### 我构建了 OpenBLAS 以用于其他软件, 但该软件找不到它

Openblas 安装为名为 `libopenblas.so` 的单一库,
而某些程序可能会搜索单独的 `libblas.so` 和 `liblapack.so`,
因此您可能需要创建适当的符号链接

```bash
ln -s libopenblas.so libblas.so
ln -s libopenblas.so liblapack.so
```

或直接复制一份.还要确保安装位置
(通常为 /opt/OpenBLAS/lib 或 /usr/local/lib)位于系统的库搜索路径中.

### 使用 gcc 的 `-fbounds-check` 编译 OpenBLAS 实际上会在程序中引发中止.

这是由于对 C 和 FORTRAN 函数之间将字符作为参数传递的
(非正式)标准的不同解释造成的.
由于两种语言存储文本的方法不同,
当 C 语言调用 Fortran 时, 文本长度会作为 "隐形" 附加参数传递.

从历史上看, 当文本只有一个字符时, 并不需要这样做,
因此像与 OpenBLAS 捆绑的 Reference-LAPACK 这样的旧代码不会这样做.
最近, gcc 的检查方式有所改变, 要求使用附加参数,
但现有的 LAPACK(以及许多其他代码库)是否需要调整以及如何调整, 尚未达成一致意见.
(在实际编译时, gcc 大部分时间都在回溯并提供兼容性选项
--因此 OpenBLAS Makefile 中的默认编译设置在 gfortran 选项中添加了
`-fno-optimize-sibling-calls` 以防止在 "受影响 "版本中编译错误).
有关详细信息和链接, 请参见问题跟踪中的 ticket 2154)
