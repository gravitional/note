# PETSc

## 配置PETSc 编译脚本

[Configuring PETSc](https://petsc.org/release/install/install/#doc-config-faq)

使用 msys2 环境中的 python 运行 configure 脚本
注意事项:

+ 使用 `--with-mpi-dir` 时,
不要为上述内容指定 `--with-cc`, `--with-fc` 等, 这样 mpicc/ mpif90 就会从 mpi-dir 中读取!
+ 这里的两个难点是
  1) 确保 PETSc 配置选择正确的 Python 安装, 因为 MSYS2 MinGW shell 中有多个可用的 Python, 以及
  2) 告诉 PETSc MS-MPI mpiexec 在哪里.
我们还建议不要使用共享库, 因为这样更容易创建独立的二进制文件.
+ msys2 下指定安装路径, c盘路径 `c:/xx` 要写成 `/c/xxx`

## msys2, ucrt64, make, 使用 fblaslapack 包

需要使用 PETSc configure 从网上下载的 fblaslapack 包,
然后编译出的静态库在 `petsc-xxx/arch-xxx/externalpackages/git.fblaslapack` 下面
`libflapack.a`, `libfblas.a`
将它们加入到系统的 Lib 搜索路径, 不然会报错

```bash
/usr/bin/python ./configure --prefix='/c/cppLibs/PETSc' \
--with-cc=/ucrt64/bin/mpicc \
--with-cxx=/ucrt64/bin/mpicxx \
--with-fc=/ucrt64/bin/mpif90 \
--download-fblaslapack \
--with-mpiexec='/C/Program\ Files/Microsoft\ MPI/Bin/mpiexec' \
--with-shared-libraries=0 --with-debugging=0 --with-64-bit-indices \
COPTFLAGS='-O3 -march=native -mtune=native' \
CXXOPTFLAGS='-O3 -march=native -mtune=native' FOPTFLAGS='-O3 -march=native -mtune=native'
```

## linux, 使用openBLAS编译的 `libopenblas.a`

```bash
python3 ./configure --prefix=/home/tom/myLibs/PETSc \
--with-blas-lib=/home/tom/myLibs/openBLASLAPACK/lib/libopenblas.a \
--with-lapack-lib=/home/tom/myLibs/openBLASLAPACK/lib/libopenblas.a \
--with-shared-libraries=1 --with-debugging=0 --with-64-bit-indices \
COPTFLAGS='-O3 -march=native -mtune=native' \
CXXOPTFLAGS='-O3 -march=native -mtune=native' FOPTFLAGS='-O3  -march=native -mtune=native'
```

配置脚本最后会输出 build 命令, 执行编译和安装

```bash
make PETSC_DIR=/c/Users/yd/Downloads/petsc-3.20.3 PETSC_ARCH=arch-mswin-c-opt all -j
```

安装

```bash
make PETSC_DIR=/c/Users/yd/Downloads/petsc-3.20.3 PETSC_ARCH=arch-mswin-c-opt PREFIX=/c/cppLibs/PETSc install
```

## 配置说明

### 编译器

+ 重要
如果未指定编译器, configure 会按以下顺序自动在用户的
`$PATH` 中查找可用的 MPI 或常规编译器:

1. mpicc/mpicxx/mpif90
2. gcc/g++/gfortran
3. cc/CC 等.

使用选项 `--with-cc/--with-cxx/--with-fc` 分别指定 c, c++ 和 fortran 编译器:

```bash
./configure --with-cc=gcc --with-cxx=g++ --with-fc=gfortran
```

+ 重要事项
最好使用 MPI compiler wrappers.
具体方法是指定 `--with-cc=mpicc` 或 `--with-mpi-dir`
(而不是 `--with-cc=gcc`)

```bash
./configure --with-cc=mpicc --with-cxx=mpicxx --with-fc=mpif90
```

或以下命令(但不要使用 `--with-cc=gcc`)

```bash
./configure --with-mpi-dir=/opt/mpich2-1.1
```

有关如何选择特定 MPI 编译器包装器
或 MPI 编译器包装器使用的特定编译器的详细信息,
请参阅 [MPI](https://petsc.org/main/install/install/#doc-config-mpi).

如果没有或不需要 Fortran 编译器, 请使用

```bash
./configure --with-fc=0
```

如果没有或不需要 c++ 编译器, 请使用

```bash
./configure --with-cxx=0
```

`configure` 默认在调试模式下编译 PETSc.
可以使用配置选项 `--with-debugging=0` 切换到优化模式
(我们建议调试和优化编译使用不同的 `$PETSC_ARCH`, 例如 `arch-debug` 和 `arch-opt`, 这样您只需更改 `$PETSC_ARCH` 的值即
可在调试代码和运行性能之间切换).
更多详情, 请参阅[多重安装文档](https://petsc.org/main/install/multibuild/#doc-multi).

此外, 还可以使用 `COPTFLAGS`, `FOPTFLAGS` 和 `CXXOPTFLAGS` 选项指定更合适的优化标志.
例如, 当使用带有相应优化标志的 gnu 编译器时, 可使用

```bash
./configure --with-cc=gcc --with-cxx=g++ --with-fc=gfortran --with-debugging=0 COPTFLAGS='-O3 -march=native -mtune=native' CXXOPTFLAGS='-O3 -march=native -mtune=native' FOPTFLAGS='-O3 -march=native -mtune=native' --download-mpich
```

>警告
configure 无法检测某些编译器的编译器库.
在这种情况下, 可以使用 LIBS 选项指定额外的系统/编译器库:

```bash
./configure --LIBS='-ldl/usr/lib/libm.a'.
```

## BLAS/LAPACK

这些软件包提供 PETSc 使用的一些基本 数值内核.
`configure` 会自动在某些标准位置查找 BLAS/LAPACK,
在大多数系统中, 您无需在 configure 命令中提供任何有关 BLAS/LAPACK 的信息.

可以使用以下选项让 `configure`脚本 自动下载/安装 BLAS/LAPACK:

+ 当存在 fortran 编译器时

```bash
./configure --download-fblaslapack
```

或者在没有 Fortran 编译器的情况下进行配置, 即 `--with-fc=0`:

```bash
./configure --download-f2cblaslapack
```

或者也可以使用其他选项, 如以下选项之一:

```bash
./configure --with-blaslapack-lib=libsunperf.a
./configure --with-blas-lib=libblas.a --with-lapack-lib=liblapack.a
./configure --with-blaslapack-dir=/soft/com/packages/intel/13/079/mkl
```

## PETSc 添加扩展包

[PETSc 编译安装教程](https://nscc.mrzhenggang.com/petsc)

安装扩展包
PETSc添加扩展的方式有2中:

1. 通过 --with-xx-dir 的方式指定已经安装过的目录
2. 通过 --download-xx 的方式自动下载包并编译安装(需要联网)
3. 通过 --download-xx=/path/to/package.tar.gz 指定安装包的路径, 扩展包可以去 https://ftp.mcs.anl.gov/pub/petsc/externalpackages/ 下载

```bash
# configure
./configure prefix=$HOME/software/petsc/3.14.1-gcc49-mpich \
--with-cc=mpicc --with-cxx=mpicxx --with-fc=mpif90 \
--with-mpiexec=/usr/bin/yhrun \
--download-fblaslapack=/vol-th/software/petsc/externalpackages/fblaslapack-3.4.2.tar.gz \
--download-fftw=/vol-th/software/petsc/externalpackages/fftw-3.3.8.tar.gz \
--download-hdf5=/vol-th/software/petsc/externalpackages/hdf5-1.12.0.tar.bz2 \
--download-netcdf=/vol-th/software/petsc/externalpackages/netcdf-4.5.0.tar.gz \
--download-metis=/vol-th/software/petsc/externalpackages/metis-5.1.0-p3.tar.gz \
--with-debugging=0 \
COPTFLAGS='-O3 -march=native -mtune=native' \
CXXOPTFLAGS='-O3 -march=native -mtune=native' \
FOPTFLAGS='-O3 -march=native -mtune=native'
```

测试

```bash
cd xxx/petsc-3.14.1/src/ksp/ksp/tutorials

# 编译
make ex50
# 运行
./ex50  -da_grid_x 4 -da_grid_y 4 -mat_view
```

## msys2 PETSc

msys2 安装的 PETSc 有多个版本,

补充的 `petsc-build` 包包含经过提炼的 PETSc 生成树.
某些软件包(如 SLEPc)在编译时需要它.
普通用户应使用 `petsc` 软件包.

该软件包提供多种库编译方式, 其区别在于
3 个字符的后缀 XYZ 区分, 如下所示:

* X 是基于 Netlib 的 SDCZ 符号的标量类型:
    - A 表示多精度或中性精度构建
    - S 表示单精度实数
    - D 表示双精度实数
    - C 表示单精度复数
    - Z 表示双精度复数

* Y 为执行模式:
    - S 表示顺序代码
    - M 表示 MPI 并行代码
    - T 表示多线程代码, 裸线程或 OpenMP, OpenACC 等.
    - H 表示使用 CUDA, OpenCL 等的异构代码.

* Z 是构建类型:
    - O 表示优化构建
    - G 表示调试构建

后缀用于静态和动态库, 以及 PkgConfig .pc 文件.
后缀 ZMO 表示优化的 MPI 并行双精度复杂库.
使用 `pkg-config petsc-zmo --cflags` 命令来获取特定于编译的编译标志.
