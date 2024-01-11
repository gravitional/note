# PETSc

## 配置PETSc    编译脚本

[Configuring PETSc](https://petsc.org/release/install/install/#doc-config-faq)

使用 msys2 环境中的 python 运行 configure 脚本
注意事项:

+ Do not specify --with-cc, --with-fc etc for the above when using --with-mpi-dir - so that mpicc/ mpif90 will be picked up from mpi-dir!
+ The two difficulties here are:
  1) make sure PETSc configure picks up the proper Python installation, as there are more than one available in a MSYS2 MinGW shell and
  2) tell PETSc where is MS-MPI mpiexec. We also recommend not to use shared libraries as it is easier to create standalone binaries that way.
+ msys2 下指定安装路径, c盘路径 `c:/xx` 要写成 `/c/xxx`

on msys2 ucrt64

```bash
/usr/bin/python ./configure --prefix='/c/cppLibs/PETSc' \
--with-cc=/ucrt64/bin/mpicc \
--with-cxx=/ucrt64/bin/mpicxx \
--with-fc=/ucrt64/bin/mpif90 \
--with-blas-lib=/c/cppLibs/openBLASLAPACK/lib/libopenblas.a \
--with-lapack-lib=/c/cppLibs/openBLASLAPACK/lib/liblapack.a \
--with-mpiexec='/C/Program\ Files/Microsoft\ MPI/Bin/mpiexec' \
--with-shared-libraries=0 --with-debugging=0 --with-64-bit-indices \
COPTFLAGS='-O3 -march=native -mtune=native' \
CXXOPTFLAGS='-O3 -march=native -mtune=native' FOPTFLAGS='-O3 -march=native -mtune=native'
```

on linux

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
