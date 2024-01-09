# 主从模式

[主从模式(实现矩阵乘法)](https://zhuanlan.zhihu.com/p/361005273)

上一章中, 可以发现, 各进程执行程序的速度是不确定的.
用多个进程输出和打印结果, 顺序是无法保证的.
本文将介绍另一种并行程序的设计模式—— `主从模式`.
因此, 我们可以在逻辑上规定一个 `主进程`,
用于将数据发送给各个进程, 再收集各个进程所计算的结果.

在介绍 `主从模式` 时, 矩阵相乘是一个既简单又具有典型性的一个例子.
所以本文就以矩阵相乘这一典型的例子来做介绍.
其中, `A` 设置为 `100×100` 的矩阵, `b` 设定为 `100×1` 的矩阵,
来计算 `矩阵A` 和 `矩阵b` 相乘.

在学线性代数时, 我们知道按矩阵相乘的计算法则,
每一个元素的计算都是独立的, 因此可以分别独立的计算,
`A矩阵` 的任一行和 `b矩阵` 这一列相乘并求和的过程,
因此很自然的可以将这些计算分配给不同的进程中,
再将结果进行汇总即可.

矩阵相乘是数值计算中最基本的模块, 学习该例子,
主要是为了理解主从模式的并行程序的设计逻辑而非相乘本身,
我们手写的矩阵乘法计算速度,
基本是不会快过诸如 `Petsc` 或 `Numpy` 等这种成熟的科学计算库,
这些成熟的计算库都在各个层面对计算进行了优化.

接下来, 将分别讲解主进程和从进程执行的过程.

## 主进程

首先, 在主进程中, 定义好需要计算的 `矩阵A` 和 `b`.

```fortran
do i=1,cols
    b(i)=1
    do j=1,rows
        a(i,j)=i
    end do
end do
```

接下来要做的是数据分发工作, 将 `A矩阵` 的不同 `行` 和 `b` 传递给 `从进程`.
为了程序的可扩展性, 考虑到所用的 `进程数` 可能比 `矩阵A` 的行数要小,
因此还需要定义 `numsent` 这一变量来计算已发送的行数.

如下可以看到, `主进程` 在发送数据时,
针对 `矩阵A` 的不同行, 和 `矩阵b` 采用了不同的 `API`,

针对 `矩阵A` 采用了之前提到过的 `MPI_SEND`,
而针对 `矩阵b`, 使用了 `MPI_BCAST`.
这是因为每个 `从进程` 都接收相同的 `矩阵b`,
如果一对一的去发送 `矩阵b` 会多此一举, 不仅程序变得复杂, 运行也会更慢.

这时我们可以考虑使用 `广播` 这一操作,
即主进程将 `矩阵b` 向 `通信域` 内所有进程广播一下`矩阵b`,
然后 `从进程` 就可以都接收到`矩阵b`这一变量了.

```fortran
call MPI_BCAST(b,cols,MPI_DOUBLE_PRECISION,master,MPI_COMM_WORLD,ierr)
do i=1,min(numprocs-1,rows)
    do j=1,cols
        buffer(j)=a(i,j)
    end do
    call MPI_SEND(buffer,cols,MPI_DOUBLE_PRECISION,i,i,MPI_COMM_WORLD,ierr)
    numsent=numsent+1
end do
```

在执行完发送步骤后, 需要将计算结果收回.

`从进程` 计算的结果用 `ans` 存储,
在发送时, 所标注的 `tag` 和 `矩阵的行标` 是相同的,
因此直接用 `c(anstype)=ans` 来在对应位置存储结果.

`sender` 用于记录, 已经将结果发送回 `主进程` 的 `从进程号`,
因其已经发送回 `主进程`, 即可代表该 `从进程` 已经处于空闲状态.
在之后的发送中, 就向空闲的进程继续发送计算任务.

在每次循环中, 都判断一次 `numsent` 和 `rows` 的关系, 用于判断是否每一行都发送完成.
当都发送完之后, 向 `从进程` 发送一个 `空信息`,
`从进程` 接收到空信息时, 即执行 `MPI_FINALIZE` 来结束.

```fortran
do i=1,row
    call MPI_RECV(ans,1,MPI_DOUBLE_PRECISION,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,status,ierr)
    sender=status(MPI_SOURCE)
    anstype=status(MPI_TAG)
    c(anstype)=ans
    if(numsent<rows) then
        do j=1,cols
            buffer(j)=a(numsent+1,j)
        end do
        call MPI_SEND(buffer,cols,MPI_DOUBLE_PRECISION,sender,numsent+1,MPI_COMM_WORLD,ierr)
        numsent=numsent+1
    else
        call MPI_SEND(1.0,0,MPI_DOUBLE_PRECISION,sender,0,MPI_COMM_WORLD,ierr)
    end if
end do
```

因此, `主进程` 主要干了三件事,
`定义数据`, `发送数据` 和 `接收计算结果`, 分别对应上述三块代码.

## 从进程

`从进程` 首先需要接收 `主进程` 广播的 `矩阵b`.

```fortran
call MPI_BCAST(b,cols,MPI_DOUBLE_PRECISION,master,MPI_COMM_WORLD,ierr)
```

`从进程` 的计算模块放入一个循环中,
直到 `矩阵A` 的所有行都计算完成后,
`主进程` 会发送一个 `tag` 为 `0` 的空消息,
当收到这个 `空tag` 时, 跳出循环, 即完成了计算任务.

```fortran
do while(1)
    call MPI_RECV(buffer,cols,MPI_DOUBLE_PRECISION,master,MPI_ANY_TAG,MPI_COMM_WORLD,status,ierr)
    if(status(MPI_TAG/=0)) then
        row=status(MPI_TAG)
        ans=0.0
        do i=1,cols
            ans=ans+buffer(i)*b(i)
        end do
        call MPI_SEND(ans,1,MPI_DOUBLE_PRECISION,master,row,MPI_COMM_WORLD,ierr)
    else
        exit
    end if
end do
```

## 总结

综上, `主从模式` 的并行程序框架基本可以写成如下范式.

```fortran
program main
    use mpi
    implicit none
    ...
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
    if (myid==0) then
        call master() !主进程的程序代码
    else
        call slave()  !从进程的程序代码
    end if
    call MPI_FINALIZE(rc)
    end program
```

理解了这两种并行程序的范式, 结合这二者的思想, 通常已经足以满足大部分计算需求了.
前面所讲的基本的 `API理论` 上足够表达各式各样的编程逻辑, 但在有些计算场景会及其繁琐.

比如本文所用到的 `广播函数`,
虽然 `MPI_SEND` 也能实现相同功能, 但 `BCAST` 极大的简化了程序编写的复杂程度.
`MPI` 中还有很多诸如此类的API, 在理解了基本函数的 `API` 后,
只需稍微阅读手册即可理解那些复杂的API, 并应用在你的计算场景之中.

## 版权声明

写该教程的初衷是帮助初学者入门. 因此本文欢迎转载或引用,
但请标明转载或引用并说明原文出处, 附上本文的链接, 并私信告知我.
未经允许不可用以商业用途.
