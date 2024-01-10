# openBLAS

[open blas user manual](https://github.com/OpenMathLib/OpenBLAS/wiki/User-Manual)

## Compile the library

Normal compile
type `make` to detect the CPU automatically. or
type `make TARGET=xxx` to set target CPU,
e.g. `make TARGET=NEHALEM`.
The full target list is in file TargetList.txt.

## 编译 LAPACK

[下载lapack](https://www.netlib.org/lapack/)

```bash
cd ~/lapack-3.10.1
cp make.inc.example make.inc
make blaslib -j
make cblaslib -j
make lapacklib -j
make lapackelib -j
```

编译完成.

## 添加路径

1. 静态库路径, 检查下~/lapack-3.10.1下是否有

```bash
librefblas.a, libcblas.a, liblapack.a, liblapacke.a
```

这几个文件, 没有的话重新编译一次.

2. 头文件路径

```bash
cblas头文件路径: ~/lapack-3.10.1/CBLAS/include
lapacke头文件路径: ~/lapack-3.10.1/LAPACKE/include
```

添加到环境变量

```bash
## 这里xxx填自己的存放路径
export LIBRARY_PATH="$LIBRARY_PATH:/xxx/lapack-3.10.1"
export C_INCLUDE_PATH="$C_INCLUDE_PATH:/xxx/lapack-3.10.1/LAPACKE/include:/xxx/lapack-3.10.1/CBLAS/include"
```

## so文件, 动态库

因为我需要用python调用C生成的动态库(.so文件),
而C写动态库时会调用lapack的函数,
也就是编译动态库时需要链接lapack的静态库(.a文件).

默认的lapack编译选项不支持上述操作, 需要进行修改.
打开~/lapack-3.10.1/make.inc文件,
在下列几个设置加上-fPIC,
然后重新 `make blaslib, cblaslib , lapacklib , lapackelib` 即可.

```makefile
CFLAGS = -O3 -fPIC
FFLAGS = -O2 -frecursive -fPIC
FFLAGS_NOOPT = -O0 -frecursive -fPIC
```
