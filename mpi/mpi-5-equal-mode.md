# mpi 对等模式

[对等模式(实现Jacobi迭代的并行计算)](https://zhuanlan.zhihu.com/p/358530365)

本部分内容将分为两章介绍两种基本的并行程序设计模式, `对等模式` 和 `主从模式`.
顾名思义, `对等模式` 即在设计算法时将各进程的地位视为相等,
`主从模式` 则把各进程的地位视作不相等, 有 `主进程` 和 `从进程` 之分.

在对等模式中, 各个进程的功能基本相同, 因此用 `SPMD`(单程序多数据)程序可以更好的表达.
在讲解对等模式时, 常用 `Jacobi` 迭代作为范例.
Jacobi迭代在数值计算上是比较常用的算法,
本文只考虑其算法本身, 不考虑该方法的其他意义.
因此后文只介绍其算法的计算规则, 具体算法使用背景可自行检索.

由于时间原因, 本文的伪代码是使用 Fortran 写的, 之后会补充上C的版本.

## Jacobi迭代算法介绍

通俗的讲, `Jacobi迭代` 就是用上下左右周围的四个点取平均值来得到新的点, 即
$$A(i,j)=0.25*[A(i-1,j)+A(i+1,j)+A(i,j-1)+A(i,j+1)]$$

每一轮迭代计算所用的数值都是上一轮的结果,
即还需要额外声明一个变量来储存当前这一轮迭代的结果, 然后在迭代完成后刷新矩阵A的值.
以下为 `Jacobi迭代` 的串行实现的Fortran伪代码.

```fortran
program Jacobi
    integer,parameter :: N=15
    real :: A(N+1,N+1),B(N+1,N+1) !假设计算规模是16x16的矩阵
    integer :: step !step为迭代的次数
    ! 略去赋值部分
    do k=1,step
        do i=2,N
            do j=2,N
                B(i,j)=0.25*(A(i-1,j)+A(i+1,j)+A(i,j+1)+A(i,j-1))
            end do
        end do
        do i=2,N
            do j=2,N
                A(i,j)=B(i,j)
            end do
        end do
    end do
    end program
```

上述的伪代码可以看出, 在同一轮迭代中,
计算任意一点之间都是相互独立的, 代码的局部性很好,
在这种情况下, 就很适合把代码改成并行来提速.
下文将一步步从串行代码改写为并行代码.

## MPI编写Jacobi迭代算法

假设我们的 `A矩阵` 是 `16x16` 的, 可以考虑使用4个进程来完成该Jacobi迭代.
因为所迭代的矩阵A是一个二维数据, 可以看做一个面板, 而面板上的每一点计算都是独立的.
所以很自然的就可以想到将这个面板划分成四块, 每个进程计算相应部分的点, 即如下图所示.

![img](https://pic1.zhimg.com/80/v2-9dc31174baa33b2bf808704611d24ee8_720w.jpg)

而这样又产生了新的问题, 即算边界点时, 需要相邻进程进程的边界值.
比如计算进程0最右侧一列的点时, 需要进程1最左侧一列点的值.
从图中我们可以看出, 进程1和进程2所执行的操作是一样的,
都需要从左右两进程接收数据, 也需要向左右两侧发送数据.
而进程0和进程3则是只用往一侧发送和接收数据.
但因为我们所编写的是SPMD程序, 所有进程执行的都是同一个代码文件.
为了保持一致性和代码的可读性, 在每一块的左右边界各加上一列.

```fortran
! 进程0-2向右侧的邻居接收数据
if(myid<3) then
    call MPI_RECV(A(1,mysize+2),totalsize,MPI_REAL,myid+1,10,MPI_COMM_WORLD,status,ierr)
end if
! 进程1-3向左侧的邻居发送数据
if(myid>0) then
    call MPI_SEND(A(1,2),totalsize,MPI_REAL,myid-1,10,MPI_COMM_WORLD,ierr)
end if
! 进程0-2向右侧的邻居发送数据
if(myid<3) then
    call MPI_SEND(A(1,mysize+1),totalsize,MPI_REAL,myid+1,10,MPI_COMM_WORLD,ierr)
end if
! 进程1-3向左侧的邻居接收数据
if(myid>0) then
    call MPI_RECV(A(1,1),totalsize,MPI_REAL,myid-1,10,MPI_COMM_WORLD,status,ierr)
end if
```

其中, `MPI_RECV` 和 `MPI_SEND` 的第一个参数是所接收或传递的数组的起始位置, 而第二个参数是数组的长度.
`Fortran` 对数组的存储是按列存储, 所以如果以 `A(1,1)` 为起始位置, 长度为 `totalsize`,
而每一进程中的 `A矩阵` 是 `16×6` 的, 因此所传递的就是 `A(1:16,1)`, 即最左侧的一列. 其他都是同理.

通过观察可以看出, 本代码的实现难度主要在通信部分.
因为需要处理好 `SEND` 和 `RECV` 的依赖关系, 而依赖关系的错误编写会使程序产生死锁或者内存溢出等致命BUG.
通俗的讲, 如果两个进程都在等待对方发送消息, 这样程序执行到这一步时就会卡住.
如果两个进程都在给对方进程发送, 而此时两个进程都没有接收指令, 也同样会出现问题.

## 死锁和内存溢出

`MPI_SEND` 和 `MPI_RECV` 所采用的是 MPI的标准通信模式,
即是否对发送的数据进行缓存不是由程序员决定, 而是由MPI决定.
通信分为 `阻塞通信` 和 `非阻塞通信`, 通俗的讲, `阻塞通信` 即为通信时只能做通信这一件事,
而非阻塞通信则为该进开始发送消息时, 不必等消息发送完成, 即可继续执行下一步指令.

在阻塞通信时, 若 `MPI_SEND` 和 `MPI_RECV` 顺序不当则会发生死锁的现象,
例如 `进程0` 和 `进程1` 要相互发送消息. 而 `进程0` 和 `进程1` 都先进行接收操作,
则两个进程都会等待消息的发送,
而不会执行下一步的发送操作, 此时程序就卡住不动, 也就是死锁现象.

```fortran
if(myid==0) then
    call MPI_RECV(...,...,...,1,tag,...,...,...) !等待接收
    call MPI_SEND(...,...,...,1,tag,...,...)
end if
if(myid==1) then
    call MPI_RECV(...,...,...,0,tag,...,...,...) !等待接收
    call MPI_SEND(...,...,...,0,tag,...,...)
end if
```

而如果在 `进程0` 和 `进程1` 中, 都先执行了 `SEND` 操作时, 也会出现错误.
因 `进程0` 和 `进程1` 都会像系统缓冲区发送数据.
而当系统缓冲区空间不足时, 则会出现溢出的现象, 有很大的危险性.

因此, 在使用 `SEND` 和 `RECV` 的时候, 为了保证程序的安全性, 需要匹配 `SEND` 和 `RECV`.
将上述代码修改成如下方式即可顺利运行.

```fortran
if(myid==0) then
    call MPI_SEND(...,...,...,1,tag,...,...)
    call MPI_RECV(...,...,...,1,tag,...,...,...)
end if
if(myid==1) then
    call MPI_RECV(...,...,...,0,tag,...,...,...)
    call MPI_SEND(...,...,...,0,tag,...,...)
end if
```

## 捆绑发送接收

在代码规模大且程序结构复杂的情况下, 匹配 `SEND` 和 `RECV` 需要额外花费较多精力,
有没有一种方法能更简便的方式来编写呢?
在该算法的应用场景下, 发送和接收操作是成对出现的, 因此可以将发送和接收操作捆绑起来.
接下来将介绍 `MPI_SEDNRECV` 和 `MPI_SENDRECV_REPLACE` 函数.

`进程0` 和 `进程3` 只有一侧边界需要传输数据, 因此处理起来会和进程1和进程2不同.
若不考虑进程0和进程3的情况, 该部分通信可写成如下形式.

```fortran
do i=1,steps
! 从左向右传递数据
    call MPI_SENDRECV(A(1,mysize+1),totalsize,MPI_REAL,myid+1,10,A(1,1),totalsize,MPI_REAL,MYID-1,10,MPI_COMM_WORLD,status,ierr)
! 从右向左传递数据
    call MPI_SENDRECV(A(1,2),totalsize,MPI_REAL,myid-1,10,A(1,mysize+2),totalsize,MPI_REAL,myid+1,10,MPI_COMM_WORLD,status,ierr)
end do
```

能看出来代码的简洁程度立刻就提升了, 然而我们还需要处理进程0和进程3这两个特殊情况.
有两种思路, 一种是使用 `if语句` 将 `进程0` 和 `进程3` 单独编写,
还有一种思路是引入 `进程拓扑` 和 `虚拟进程`, 使 `进程0`和 `进程3` 与进程1和进程2在形式上保持一致.
用这种方式可以保证代码更加简洁可读.

虚拟进程 `MPI_PROC_NULL` 是一个假想的进程, 其存在有助于编写时的方便.
当一个真实进程向虚拟进程发送和接收数据时, 会立刻执行一个空操作.
该进程的引入可以很好的简化边界的代码. 不仅可以使代码编写变得简单, 也使代码的可读性大大提高.

为了使用虚拟进程, 需要在 `进程0` 和 `进程3` 额外进行一下标记.
引入 `right` 和 `left` 来对每一个进程左右两边进行记录.

```fortran
if(myid > 0) then
    left=myid-1
else
    left=MPI_PROC_NULL
end if
if(myid < 3) then
    right=myid+1
else
    right=MPI_PROC_NULL
end if
do i=1,steps
    call MPI_SENDRECV(A(1,mysize+1),totalsize,MPI_REAL,right,tag1,A(1,1),totalsize,MPI_REAL,left,tag1,MPI_COMM_WORLD,status,ierr)
    call MPI_SENDRECV(A(1,2),totalsize,MPI_REAL,left,tag2,A(1,mysize+2),totalsize,MPI_REAL,right,tag2,MPI_COMM_WORLD,status,ierr)
end do
```

## 总结

本文介绍了对等模式的基本概念, 并用对等模式的方式改写了串行的 `Jacobi迭代` 算法.
介绍了 `MPI_SENDRECV` 函数, 并介绍了虚拟进程的概念.
初学者可以试着结合上述的代码, 试着把其完整的代码编写出来.
该部分的完整版Fortran代码见文末附录, 稍后会更新C代码.

通过实践, 我们会发现下面的代码在输出结果时,
各进程输出各自的矩阵A, 在时间顺序上是不确定的, 打印出来的矩阵A是乱序的.
因为各进程计算的速度是不确定的, 先计算完的进程就先执行输出语句.
若想按顺序完整的输出最终的矩阵A, 需要将各个进程中的结果汇总到一个进程中,
由一个进程负责输出, 这就涉及到了主从进程的思想,
下一章将用 `矩阵相乘` 这一简单的例子来讲解主从模式.

## 附录

用Fortran90实现的完整代码

```fortran
program main
    use mpi
    implicit none
    integer,parameter :: steps = 10
    integer,parameter :: totalsize = 16
    integer,parameter :: mysize = 4
    integer :: n,myid,numprocs,i,j,rc
    integer :: left,right,tag1,tag2
    real :: A(totalsize,mysize+2),B(totalsize,mysize+2)
    integer :: begin_col,end_col,ierr
    integer :: status(MPI_STATUS_SIZE)
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierr)
    print *, "Process ", myid,"of ",numprocs,"is alive!"
    do j=1,mysize+2
        do i=1,totalsize
            A(i,j)=0.0
        end do
    end do
    if(myid==0) then
        do i=1,totalsize
            A(i,2)=8.0
        end do
    end if
    if(myid==3) then
        do i=1,totalsize
            A(i,mysize+1)=8.0
        end do
    end if
    do i=1,mysize+2
        A(1,i)=8.0
        A(totalsize,i)=8.0
    end do
    if(myid > 0) then
        left=myid-1
    else
        left=MPI_PROC_NULL
    end if
    if(myid < 3) then
        right=myid+1
    else
        right=MPI_PROC_NULL
    end if
    tag1=3
    tag2=4
    do n=1,steps
        call MPI_SENDRECV(A(1,mysize+1),totalsize,MPI_REAL,right,tag1,&
                    A(1,1),totalsize,MPI_REAL,left,tag1,MPI_COMM_WORLD,status,ierr)
        call MPI_SENDRECV(A(1,2),totalsize,MPI_REAL,left,tag2,&
                    A(1,mysize+2),totalsize,MPI_REAL,right,tag2,MPI_COMM_WORLD,status,ierr)
        begin_col=2
        end_col=mysize+1
        if(myid==0) then
            begin_col=3
        end if
        if(myid==3) then
            end_col=mysize
        end if
        do j=begin_col,end_col
            do i=2,totalsize-1
                B(i,j)=(A(i,j+1)+A(i,j-1)+A(i+1,j)+A(i-1,j))*0.25
            end do
        end do
        do j=begin_col,end_col
            do i=2,totalsize-1
                A(i,j)=B(i,j)
            end do
        end do
    end do
    do i=2,totalsize-1
        print *, myid,(a(i,j),j=begin_col,end_col)
    end do
    call MPI_FINALIZE(rc)
end program
```
