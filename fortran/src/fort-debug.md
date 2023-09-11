# fortran bug

## print* 卡住 freeze, stuck

[Print to standard output from a function defined in an Fortran module](https://stackoverflow.com/questions/24923076/print-to-standard-output-from-a-function-defined-in-an-fortran-module)

您的程序执行了所谓的 `递归 IO`(recursive IO) - 对 `plgndr` 的初始调用是在 IO 语句
(print 语句)的输出项列表中(直接输出到控制台)--
在该函数中, 您还试图执行另一条 IO 语句(输出到控制台).
这是不允许的--参见 F2003 的 9.11p2 和 p3 或 F2008 的 9.12p2.

解决办法是在主程序中将 `函数调用` 与 `IO语句` 分开, 即

```fortran
REAL :: a_temporary
...
a_temporary = plgndr(1,2,0.1)
PRINT *, a_temporary
```

F2008 中的其他替代方法(但不包括 F2003, 因此第一段中的`()`部分)
包括将函数的输出导向不同的逻辑单元,
注意 `WRITE (*, ...` 和 `PRINT ...` 引用的是同一个单元.

在 F2008 中, 您还可以用带有 message 的 `STOP` 语句来替换 `WRITE` 语句
信息必须是常量, 所以无法报告问题的值.

无意中调用 `递归IO` 的可能性是某些编程风格不鼓励在函数中执行 `IO` 的部分原因.

## 标准输入和输出

[Standard input and output units in Fortran 90?](https://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90)

如果您使用的是 Fortran 2003 编译器,
其 intrinsic module `iso_fortran_env` 定义了变量 `input_unit`, `output_unit` 和 `error_unit`,
它们分别指向标准输入, 标准输出和标准错误.

我倾向于使用类似

```fortran
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                          stdout=>output_unit, &
                                          stderr=>error_unit
#else
#define stdin  5
#define stdout 6
#define stderr 0
#endif
```

在我的输入/输出 routines 中.

当然, 这意味着要对源文件进行预处理.
使用 `ifort` 时, 请在编译源代码时使用 `-fpp` 标志,
或将源文件扩展名从 `.f` 改为 `.F` 或从 `.f90` 改为 `.F90`.

除此以外, 您还可以编写自己的, 非内在的 `iso_fortran_env` 模块,
如果您没有 Fortran 2003 编译器的话,
在本例中, 他们使用了 module:

```fortran
module iso_fortran_env

  ! Nonintrinsic version for Lahey/Fujitsu Fortran for Linux.
  ! See Subclause 13.8.2 of the Fortran 2003 standard.

  implicit NONE
  public

  integer, parameter :: Character_Storage_Size = 8
  integer, parameter :: Error_Unit = 0
  integer, parameter :: File_Storage_Size = 8
  integer, parameter :: Input_Unit = 5
  integer, parameter :: IOSTAT_END = -1
  integer, parameter :: IOSTAT_EOR = -2
  integer, parameter :: Numeric_Storage_Size = 32
  integer, parameter :: Output_Unit = 6

end module iso_fortran_env
```

正如其他答案所指出的, `0`, `5` 和 `6` 通常是 `stderr`, `stdin` 和 `stdout`(Linux 上的 ifort 也是如此),
但 Fortran 标准并未对此进行定义.
使用 `iso_fortran_env` 模块是可移植地写入这些单元的正确方法.
