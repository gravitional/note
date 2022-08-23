# 主从模式

[主从模式(实现矩阵乘法)](https://zhuanlan.zhihu.com/p/361005273)

上一章中, 可以发现, 各进程执行程序的速度是不确定的. 用多个进程输出和打印结果, 顺序是无法保
证的. 本文将介绍另一种并行程序的设计模式——主从模式. 因此, 我们可以在逻辑上规定一个主进程,
用于将数据发送给各个进程, 再收集各个进程所计算的结果.

在介绍主从模式时, 矩阵相乘是一个既简单又具有典型性的一个例子. 所以本文就以矩阵相乘这一典型
的例子来做介绍. 其中, A设置为100×100的矩阵, b设定为100×1的矩阵, 来计算矩阵A和矩阵b相乘. 在
学线性代数时, 我们知道按矩阵相乘的计算法则, 每一个元素的计算都是独立的, 因此可以分别独立的
计算A矩阵的任一行和b矩阵这一列相乘并求和的过程, 因此很自然的可以将这些计算分配给不同进程的
中, 再将结果进行汇总即可.

矩阵相乘是数值计算中最基本的模块, 学习该例子主要是为了理解主从模式的并行程序的设计逻辑而非
相乘本身, 我们手写的矩阵乘法计算速度基本是不会快过诸如Petsc或Numpy等这种成熟的科学计算库,
这些成熟的计算库都在各个层面对计算进行了优化.

接下来, 将分别讲解主进程和从进程执行的过程.

## 主进程

首先, 在主进程中, 定义好需要计算的矩阵A和b.

```fortran
do i=1,cols
    b(i)=1
    do j=1,rows
        a(i,j)=i
    end do
end do
```

接下来要做的是数据分发工作, 将A矩阵的不同行和b传递给从进程. 为了程序的可扩展性, 考虑到所用
的进程数可能比矩阵A的行数要小, 因此还需要定义numsent这一变量来计算已发送的行数.

如下可以看到, 主进程在发送数据时, 针对矩阵A的不同行和矩阵b采用了不同的API, 针对矩阵A采用了
之前提到过的MPI_SEND, 而针对矩阵b, 使用了MPI_BCAST. 这是因为每个从进程都接收相同的矩阵b,
如果一对一的去发送矩阵b会多此一举, 不仅程序变得复杂, 运行也会更慢. 这时我们可以考虑使用广
播这一操作, 即主进程将矩阵b向通信域内所有进程广播一下矩阵b, 然后从进程就可以都接收到矩阵b
这一变量了.

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

在执行完发送步骤后, 需要将计算结果收回. 从进程计算的结果用ans存储, 在发送时, 所标注的tag和
矩阵的行标是相同的, 因此直接用c(anstype)=ans来在对应位置存储结果. sender用于记录已经将结果
发送回主进程的从进程号, 因其已经发送回主进程, 即可代表该从进程已经处于空闲状态. 在之后的发
送中, 就向空闲的进程继续发送计算任务. 在每次循环中, 都判断一次numsent和rows的关系, 用于判
断是否每一行都发送完成. 当都发送完之后, 向从进程发送一个空信息, 从进程接收到空信息时, 即执
行MPI_FINALIZE来结束.

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

因此, 主进程主要干了三件事, 定义数据, 发送数据和接收计算结果, 分别对应上述三块代码.从进程

从进程首先需要接收主进程广播的矩阵b.

```fortran
call MPI_BCAST(b,cols,MPI_DOUBLE_PRECISION,master,MPI_COMM_WORLD,ierr)
```

从进程的计算模块放入一个循环中, 直到矩阵A的所有行都计算完成后, 主进程会发送一个tag为0的空
消息, 当收到这个空tag时, 跳出循环, 即完成了计算任务.

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

综上, 主从模式的并行程序框架基本可以写成如下范式.

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
前面所讲的基本的API理论上足够表达各式各样的编程逻辑, 但在有些计算场景会及其繁琐.
比如本文所用到的广播函数, 虽然MPI_SEND也能实现相同功能, 但BCAST极大的简化了程序编写的复杂程度.
MPI中还有很多诸如此类的API, 在理解了基本函数的API后,
只需稍微阅读手册即可理解那些复杂的API, 并应用在你的计算场景之中.

## 版权声明

写该教程的初衷是帮助初学者入门. 因此本文欢迎转载或引用, 但请标明转载或引用并说明原文出处, 附上本文的链接, 并私信告知我. 
未经允许不可用以商业用途. 
