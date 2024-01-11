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
--with-mpiexec='/C/Program\ Files/Microsoft\ MPI/Bin/mpiexec' \
--with-shared-libraries=0 --with-debugging=0 --with-64-bit-indices COPTFLAGS='-O3 -march=native -mtune=native' CXXOPTFLAGS='-O3 -march=native -mtune=native' FOPTFLAGS='-O3 -march=native -mtune=native'
```

on linux

```bash
python3 ./configure --prefix=/home/tom/myLibs/PETSc \
--with-blas-lib=/home/tom/myLibs/openBLASLAPACK/lib/libopenblas.a \
--with-lapack-lib=/home/tom/myLibs/openBLASLAPACK/lib/libopenblas.a \
--with-shared-libraries=1 --with-debugging=0 --with-64-bit-indices \
COPTFLAGS='-O3 -march=native -mtune=native' CXXOPTFLAGS='-O3 -march=native -mtune=native' FOPTFLAGS='-O3  -march=native -mtune=native'
```

配置脚本最后会输出 build 命令, 执行编译和安装

```bash
make PETSC_DIR=/c/Users/yd/Downloads/petsc-3.20.3 PETSC_ARCH=arch-mswin-c-opt all -j
```

安装

```bash
make PETSC_DIR=/c/Users/yd/Downloads/petsc-3.20.3 PETSC_ARCH=arch-mswin-c-opt PREFIX=/c/cppLibs/PETSc install
```
