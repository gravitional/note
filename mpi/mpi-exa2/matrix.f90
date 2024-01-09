program main
use mpi
implicit none
!考虑到所用进程数可能 < 矩阵A的行数,  因此定义 numsent 来表示已发送的行数
real :: ans
integer :: n,myid,numprocs,i,j,rc, numsent = 10
integer :: left,right,tag1,tag2
real :: A(totalsize,mysize+2),B(totalsize,mysize+2)
integer :: begin_col,end_col,ierr
integer :: status(MPI_STATUS_SIZE) ! MPI 状态

! 主进程主要干三件事: 定义数据, 发送数据和接收计算结果
SUBROUTINE master()
    !!1 在主进程中, 定义好需要计算的矩阵A和b.
    do i=1,cols
        b(i)=1
        do j=1,rows
            a(i,j)=i
        end do
    end do
    !!2 每个从进程都接收相同的矩阵b,
    ! 考虑使用广播操作, 即主进程将矩阵b向通信域内所有进程广播一下矩阵b,
    ! 然后从进程就可以都接收到矩阵b这一变量了.
    call MPI_BCAST(
        b,
        cols,
        MPI_DOUBLE_PRECISION,
        master,
        MPI_COMM_WORLD,
        ierr)
    !!2 矩阵A采用了之前提到过的 MPI_SEND, 发送每行的数据
    do i=1,min(numprocs-1,rows)
        do j=1,cols
            buffer(j)=a(i,j)
        end do
        ! 发送矩阵A 的行数据
        call MPI_SEND(
            buffer,
            cols, !使用矩阵行数作为 tag
            MPI_DOUBLE_PRECISION,
            i,i,
            MPI_COMM_WORLD,
            ierr)
        numsent=numsent+1 ! 记录已发送的行数
    end do

    !!3 在执行完发送步骤后, 需要将计算结果收回
    do i=1,row
        call MPI_RECV(
            ans,
            1,
            MPI_DOUBLE_PRECISION,
            MPI_ANY_SOURCE,
            MPI_ANY_TAG,
            MPI_COMM_WORLD,
            status,
            ierr)
        ! sender用于记录已经将结果发送回主进程的从进程号
        ! 因其已经发送回主进程, 即可代表该从进程已经处于空闲状态
        ! 在之后的发送中, 就向空闲的进程继续发送计算任务
        sender=status(MPI_SOURCE)
        !在发送时, 所标注的tag和矩阵的行标是相同的,
        !因此直接用c(anstype)=ans来在对应位置存储结果
        anstype=status(MPI_TAG)
        c(anstype)=ans
        ! numsent 是已发送行, 用于判断是否发送完所有行
        if(numsent<rows) then
            do j=1,cols
                buffer(j)=a(numsent+1,j)
            end do
            !在执行完发送步骤后, 需要将计算结果收回
            call MPI_SEND(
                buffer,
                cols,
                MPI_DOUBLE_PRECISION,
                sender,
                numsent+1,
                MPI_COMM_WORLD,
                ierr)
            numsent=numsent+1
        else
            !当都发送完之后, 向从进程发送一个空信息,
            !从进程接收到空信息时, 即执行MPI_FINALIZE来结束.
            call MPI_SEND(
                1.0, 0, MPI_DOUBLE_PRECISION,
                sender,0,MPI_COMM_WORLD
                ,ierr)
        end if
    end do
    return
end SUBROUTINE

! 从进程函数
SUBROUTINE slave()
    ! 从进程首先需要接收主进程广播的矩阵b
    do while(1)
        call MPI_RECV(
            buffer,cols,MPI_DOUBLE_PRECISION,
            master,MPI_ANY_TAG,MPI_COMM_WORLD,
            status,ierr)
        ! 直到矩阵A的所有行都计算完成后, 主进程会发送一个tag为0的空消息,
        if(status(MPI_TAG/=0)) then
            row=status(MPI_TAG)
            ans=0.0
            do i=1,cols
                ans=ans+buffer(i)*b(i)
            end do
            call MPI_SEND(
                ans,1,MPI_DOUBLE_PRECISION,
                master,row,MPI_COMM_WORLD,
                ierr)
        ! 当收到这个空tag时, 跳出循环, 即完成了计算任务.
        else
            exit
        end if
    end do
end SUBROUTINE

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
if (myid==0) then
    call master() !主进程的程序代码
else
    call slave()  !从进程的程序代码
end if
call MPI_FINALIZE(rc)
end program