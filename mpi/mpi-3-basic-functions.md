# MPI的6个基本函数

[两小时入门MPI与并行计算(三): MPI的6个基本函数](https://zhuanlan.zhihu.com/p/357551507)

本章讲解 `MPI` 的6个基本函数, 掌握了这六个基本函数, 就能开始写一个MPI程序了.
考虑到读者可能有人擅长C++, 有人擅长Fortran.
因此在讲这些函数接口时, 我会将两种语言的接口和参数类型都附上.
如果只是应用MPI处理科学计算的问题, 很多参数不用深究其意义, 甚至不需要记忆,
只需要在写或读代码的时候随时查手册即可.
这6个基本函数基本可以实现大部分功能, 因此有必要对这6个函数有比较准确的理解.

## MPI_Init

任何MPI程序都应该首先调用该函数.  此函数不必深究,
只需在MPI程序开始时调用即可(必须保证程序中 `第一个调用的MPI函数` 是这个函数).

```cpp
call MPI_INIT() # Fortran
MPI_Init(&argc, &argv) //C++ & C
```

`Fortran` 版本调用时不用加任何参数, 而 `C` 和 `C++` 需要将 `main函数` 里的两个参数传进去,
因此在写 `main函数` 的主程序时, 应该加上这两个形参.

```cpp
int main(int *argc,char* argv[])
{
    MPI_Init(&argc,&argv);
}
```

## MPI_Finalize

任何 `MPI` 程序结束时, 都需要调用该函数. 切记Fortran在调用 `MPI_Finalize` 的时候,
需要加个参数 `ierr` 来接收返回的值, 否则计算结果可能会出问题甚至编译报错.
在Fortran中 `ierr` 为 `integer` 型变量. 该函数同第一个函数, 都不必深究, 只需要求格式去写即可.

```cpp
call MPI_Finalize(ierr) # Fortran
MPI_Finalize() //C++
```

## MPI_COMM_RANK

```cpp
call MPI_COMM_RANK(comm, rank)
int MPI_Comm_Rank(MPI_Comm comm, int *rank)
```

该函数是获得当前进程的 `进程标识`, 如 `进程0` 在执行该函数时, 可以获得返回值0.
可以看出该函数接口有两个参数, 前者为进程所在的通信域, 后者为返回的进程号.
通信域可以理解为给进程分组, 比如有 `0-5` 这六个进程.
可以通过定义 `通信域`, 来将比如 `[0,1,5]` 这三个进程分为一组,
这样就可以针对该组进行"组"操作, 比如 `规约` 之类的操作.
这类概念会在之后的MPI进阶一章中讲解.
`MPI_COMM_WORLD` 是 `MPI` 已经预定义好的通信域,
是一个包含所有进程的通信域, 目前只需要用该通信域即可.

在调用该函数时, 需要先定义一个整型变量如 `myid`, 不需要赋值.
将该变量传入函数中, 会将该进程号存入 `myid` 变量中并返回.

比如, 让 `进程0` 输出 `Hello`, 让 `进程1` 输出 `Hi` 就可以写成如下方式.

```fortran
Program main
    use mpi
    implicit none
    integer :: myid
    MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WOLRD,myid)
    if (myid==0) then
        print *, "Hello!"
    end if
    if (myid==1)
        print *, "Hi!"
    end if
    MPI_FINALIZE()
end Program
```

C和C++版本如下

```cpp
#include <iostream>
#include <mpi.h>
int main(int argc, char* argv[])
{
    int myid;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    if (myid == 0)
    {
        printf("Hello!");
    }
    if (myid == 1)
    {
        printf("Hi!");
    }
    MPI_Finalize();
}
```

## MPI_COMM_SIZE

该函数是获取该通信域内的 `总进程数`,
如果通信域为 `MPI_COMM_WORLD`, 即获取总进程数, 使用方法和 `MPI_COMM_RANK` 相近.

```cpp
MPI_COMM_SIZE(comm, size)
int MPI_Comm_Size(MPI_Comm, int *size)
```

## MPI_SEND

该函数为发送函数, 用于进程间发送消息,
如 `进程0` 计算得到的结果A, 需要传给进程1, 就需要调用该函数.

```cpp
call MPI_SEND(buf, count, datatype, dest, tag, comm)
int MPI_Send(
    type* buf, int count, MPI_Datatype,
    int dest, int tag, MPI_Comm comm)
```

该函数参数过多, 不过这些参数都很有必要存在.

这些参数均为传入的参数, 其中 `buf` 为你需要传递的数据的起始地址,
比如你要传递一个数组A, 长度是5, 则 `buf` 为数组A的首地址.
`count` 即为长度, 从首地址之后 `count` 个变量.
`datatype` 为变量类型, 注意该位置的 `变量类型` 是 `MPI` 预定义的变量类型,
比如需要传递的是 `C++` 的 `int` 型, 则在此处需要传入的参数是 `MPI_INT`, 其余同理.
`dest` 为接收的进程号, 即被传递信息进程的进程号. `tag` 为信息标志, 同为整型变量,
发送和接收需要tag一致, 这将可以区分同一目的地的不同消息.
比如 `进程0` 给 `进程1` 分别发送了 `数据A` 和 `数据B`, `tag` 可分别定义成 `0` 和 `1`,
这样在 `进程1` 接收时同样设置 `tag0` 和 `1` 去接收, 避免接收混乱.

## MPI_RECV

该函数为 `MPI` 的接收函数, 需要和 `MPI_SEND` 成对出现.

```cpp
call MPI_RECV(buf, count, datatype, source, tag, comm, status)
int MPI_Recv(
    type* buf, int count, MPI_Datatype,
    int source, int tag, MPI_Comm comm, MPI_Status *status)
```

参数和 `MPI_SEND` 大体相同, 不同的是 `source` 这一参数, 这一参数标明从哪个进程接收消息.
最后多一个用于返回状态信息的参数 `status`.

在C和C++中, `status` 的变量类型为 `MPI_Status`, 分别有三个域,
可以通过 `status.MPI_SOURCE`, `status.MPI_TAG` 和 `status.MPI_ERROR` 的方式调用这三个信息.
这三个信息分别返回的值是所收到数据发送源的进程号, 该消息的 `tag` 值和接收操作的错误代码.

在Fortran中, `status` 的变量类型为长度是 `MPI_STATUS_SIZE` 的整形数组.
通过 `status(MPI_SOURCE)`, `status(MPI_TAG)` 和 `status(MPI_ERROR)` 来调用.

`SEND` 和 `RECV` 需要成对出现, 若两进程需要相互发送消息时,
对调用的顺序也有要求, 不然可能会出现死锁或内存溢出等比较严重的问题,
具体在之后的对等模式这一章中详细介绍.

## Example

发送和接收这两个函数参数过多, 初学可能看不懂部分参数的意义以及使用方法,
在学了这六个函数之后, 再来看本系列第一章提到的一个简单的例子,
这个例子就把这六个函数都使用上了.

```cpp
//第一章提到的案例, 具体描述可以回看第一章
int main(int argc, char* argv[])
{
    //第一章提到的案例, 具体描述可以回看第一章
    MPI_Init(&argc, &argv);
    int myid;
    int s{ 0 };
    int s1{ 0 };
    int A[4] = { 0 };
    int comm_tag = 99;
    MPI_Status status;
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);        //得到的变量myid即为当前的进程号
    //假设要求和的数组为A={[1,1,1,1],[2,2,2,2]}
    if (myid == 0)
    {
        memset(A, 1, sizeof(int));   //将数组A全赋值为1
    }
    else if (myid != 0)
    {
        memset(A, 2, sizeof(int));   //将数组A全赋值为2
    }
    //以上部分是将数组的两行分别存储到进程0和进程1上
    for (int i = 0; i < 4; i++)
    {

        s = s + A[i];
    }
    if (myid == 1)
    {
        MPI_Send(&s, 1, MPI_INT, 0, comm_tag, MPI_COMM_WORLD);
        //将求和结果s发送到进程0
    }
    if (myid == 0)
    {
        MPI_Recv(&s1, 1, MPI_INT, 1, comm_tag, MPI_COMM_WORLD, &status);
        //用s1这个变量来存储从进程1发送来的求和结果
        s = s + s1;
    }
    printf("now the s is: %d\n", s);
    MPI_Finalize();
}
```

## 总结

这一章主要介绍了MPI的最基本的六大接口, 理解了这六个函数接口,
就可以写一个最基本的MPI并行程序了, 下一章详细讲解如何写一个MPI的基本程序, 通过编程实践,
将会进一步加深对这些函数接口的理解.
