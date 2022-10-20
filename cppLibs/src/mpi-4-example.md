# 从0开始写出一个MPI并行程序

​ [​两小时入门MPI与并行计算(四): 从0开始写出一个MPI并行程序](https://zhuanlan.zhihu.com/p/399150417)

## 前言

在学习了六个基本函数之后, 我们就可以开始尝试着手编写一个基本的MPI程序.
本文的目的是让初次
接触MPI的新手可以打开编辑器, 实现一个简单的算法. 希望由此可以举一反三去思考之前常用的串行
程序, 并尝试将其并行化. 因为面向的是从0开始的初学者, 本文将会使用比较详细的注释来解释每一
步的操作. 有条件的初学者可以打开编辑器一步一步的尝试去实现下文的算法. 算法分析

本文选择使用一个比较简单且经典的算例, 即求$\pi$值. 根据简单的求定积分公式如下
$$\int_0^1 \frac{1}{1+x^2} \mathrm{d}x=\frac{\pi}{4}$$.
令
$$f(x)=\frac{4}{1+x^2} $$.
把等号右边的 `4` 乘到左边即可得到$\pi$的表达式
$$\pi=\int_0^1 \frac{4}{1+x^2} \mathrm{d}x =\int_0^1 f(x) \mathrm{d}x $$

将积分离散化之后, 即可计算$\pi$的值. 即在0到1之间等分出来N个矩形, 求出每个矩形的面积, 累加之
后就可以近似等于f(x)在0到1上积分的值. N越大则近似越准确. 写成数学表达式则是如下形式

将公式推导至上式, 就可以开始计算了.
显然, 如果按照我们熟悉的串行程序的计算方式, 只需要一个简单的for循环即可解决, 即

```cpp
for(i=1;i<=N;i++)
{
    sum=sum+f((i-0.5)/N)
}
sum=sum/N
```

然而, 这个循环进行了N次, 并且N次计算都是独立过程. 如果我们启用m个进程, 那么自然而然可以在
每个进程上的循环次数减少到N/m次. 因此, 本文的并行程序设计目的就是为了实现把独立循环拆解到
多个进程上. 代码实现

在编写并行程序前, 不妨先检查下MPI是否已经安装完成并添加到环境变量之中.

```bash
which mpicc
```

然后可以开始创建一个pi.c文件, 并在其中编写代码. 在开头引入头文件, 并加入MPI程序的基本框
架, 并且定义好f(x).

```cpp
#include "mpi.h"
#include <stdio.h>
double f(double);
double f(double x)
{
    return (4.0/(1.0+x*x));
}
int main(int argc,char *argv[])
{
    MPI_Init(&argc,&argv);

    MPI_Finalize();
}
```

此时不妨将该文件编译一下, 确保编译器和MPI库的链接没有问题, 环境也是ok的.

```bash
mpicc pi.c
```

没有报错的话, 就可以继续开始编写代码. 紧接着我们需要获得总进程数, 和各自进程的进程号, 并分
别存入变量numprocs和myid变量之中. 这样在后面我们就可以通过if语句来区分不同进程并分别对其进
行操作了, 随后将其进行输出.

```cpp
#include "mpi.h"
#include <stdio.h>

int main(int argc,char *argv[])
{
    int myid,numprocs;

    MPI_Init(&argc,&argv);

    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    printf("Process %d of %d\n", myid, numprocs);

    MPI_Finalize();
}
```

此时, 如果将我们这个程序进行编译运行就可以看到输出了. mpicc的编译指令和gcc用法类似, -o就是
从代码直接生成到可执行文件. 我们在运行并行程序的可执行文件时, 通过使用mpirun的方式, 可以指
定分配多少个进程. 而直接使用./pi.exe的方式执行的话默认是使用一个进程, 各位不妨可以试一
下. -np后面就是分配的总进程数目, 以6个进程为例.

```cpp
mpicc -o pi.exe pi.c
mpirun -np 6 pi.exe
```

执行后, 可以在屏幕中打印出结果. 由于各进程执行速度的快慢不同, 因此执行输出语句的先后顺序具
有不确定性. 每次运行该程序的输出顺序也是不确定的. 因此我们在编写并行程序时, 如果想对计算结
果进行有序输出, 往往需要将多个进程的结果汇总到某一个指定进程, 由指定进程统一进行输出.

```log
Process 1 of 6
Process 2 of 6
Process 3 of 6
Process 4 of 6
Process 0 of 6
Process 5 of 6
```

接下来就到了计算的核心部分, 以4个进程为例, 我们想把100次循环计算分给4个进程. 最符合直觉的
分配方式就是让0进程计算1-25, 1进程计算26-50, 2进程计算51-75, 3进程计算76-100. 然而这样在代
码实现中是有麻烦的. 因为我们的4个进程是分别执行的同一个代码, 存储的变量名虽然相同, 但是所
对应的值是不同的. 采用如下的分配方式会使各个进程的逻辑实现是相同的.

```cpp
n = 100
h = 1.0 / (double) n;
sum = 0.0;
for (i = myid + 1; i <= n; i += numprocs)
{
    x = h * ((double)i - 0.5);
    sum += f(x);
}
```

这样的话, 每个进程所计算的循环轮数都是以总进程数为差的等差数列. 而不同进程的myid是不一样
的, 这样就不会产生重复计算, 而且实现过程比较简洁. 计算完成后, 每个进程都的sum变量都只包括
了各自进程所分配的那一部分的求和结果. 因此还需要把各个进程的结果汇总到一起并相加. 当然, 我
们可以选择使用上一章提到的MPI_SEND和MPI_RECV函数, 但这在实现起来会很复杂. 在这里, 我们可以
考虑使用规约操作.

```cpp
MPI_Reduce(&mypi, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
```

该函数可以理解为在通信域MPI_COMM_WORLD中, 将各个进程的MPI_DOUBLE型变量起始地址为&mypi长度
为1发送到缓冲区, 并执行MPI_SUM操作, 将结果返回至0进程的&pi地址中. 用直白的话讲就是将域内每
一个进程的mypi相加求和, 并将所得的结果存入0进程中的pi变量. 规约函数中有很多操作, 比如求
和, 求积, 取最大值, 取最小值等等, 具体可以查阅手册, 这一函数会使得我们的并行程序编写变得很
方便.

到此, 该程序的主体部分已经基本编写完成了. C版本的完整代码如下, Fortran的实现过程也是类似
的, 习惯使用Fortran的学习者可以参考本文自行编写.

```cpp
#include "mpi.h"
#include <stdio.h>
double f(double);
double f(double x)
{
    return (4.0/(1.0+x*x));
}
int main(int argc,char *argv[])
{
    int myid, numprocs;
    int n, i;
    double mypi, pi;
    double h, sum, x;
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myid);
    printf("Process %d of %d.\n", myid, numprocs);
    n = 100;
    h = 1.0 / (double) n;
    sum = 0.0;
    for (i = myid + 1; i <= n; i += numprocs)
    {
        x = h * ((double)i - 0.5);
        sum +=f(x);
    }
    mypi = h * sum;
    MPI_Reduce(&mypi, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    if (myid == 0)
    {
        printf("The result is %.10f.\n",pi);
    }
    MPI_Finalize();
}
```

## 总结

该算法虽然简单, 但却反映了很多MPI的并行程序编写的共性.
该算法在每次循环中是计算独立的, 因此拆分其循环的思路比较直观.
面对相对复杂的计算场景, 设计出了两种程序设计模式,
分别是对等模式和主从模式, 下文将会对其进行一一介绍.
