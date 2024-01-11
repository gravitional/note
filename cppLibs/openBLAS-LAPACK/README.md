# openBLAS LAPACK 测试例子

## openBLAS

[Code examples](https://github.com/OpenMathLib/OpenBLAS/wiki/User-Manual#code-examples)

+ [测试例子1](test_cblas_dgemm.c)

```bash
gcc -o test_cblas_open test_cblas_dgemm.c -I /your_path/OpenBLAS/include/ -L/your_path/OpenBLAS/lib -lopenblas -lpthread -lgfortran
# 如果 include 和 lib 已经添加到环境变量, 可以使用
gcc -o test_cblas_open test_cblas_dgemm.c -lopenblas -lpthread -lgfortran
```

+ [测试例子2](time_dgemm.c)

```bash
gcc -o time_dgemm time_dgemm.c -static -lopenblas

# 测试格式 ./time_dgemm <m> <n> <k>, 例如
./time_dgemm.exe 5 6 8
```

## LAPACK

[Examples: calling LAPACKE functions from a C program.](https://netlib.org/lapack/lapacke.html)

+ [测试例子3](test_dgels_row.c)

```bash
gcc -o test_dgels_row test_dgels_row.c -static -llapack -lgfortran -lquadmath
```

+ [测试例子4](test_dgels_column.c)

```bash
gcc -o test_dgels_column test_dgels_column.c -static -llapack -lgfortran -lquadmath

# 显式指定链接方式, 并指定 rpath 搜索路径
gcc -o test_dgels_column test_dgels_column.c \
-Wl,-rpath=/c/cppLibs/openBLASLAPACK/bin \
-Wl,-Bstatic -lgfortran -lm -Wl,-Bdynamic -llapack
```

+ [测试例子5](test_qr.c)

```bash
gcc -o test_qr test_qr.c -Wl,-Bdynamic -llapack
```

+ [测试例子6](test_qr2.c)

```bash
gcc -o test_qr2 test_qr2.c -Wl,-Bdynamic -llapack
```
