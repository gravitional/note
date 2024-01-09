program main
    use mpi
    implicit none
    integer,parameter :: steps = 10  !迭代次数
    integer,parameter :: totalsize = 16 !矩阵维度
    integer,parameter :: mysize = 4   ! MPI 分块数目
    integer :: n,myid,numprocs,i,j,rc
    integer :: left,right,tag1,tag2
    real :: A(totalsize,mysize+2),B(totalsize,mysize+2)
    integer :: begin_col,end_col,ierr
    integer :: status(MPI_STATUS_SIZE) ! MPI 状态
    call MPI_INIT(ierr) !MPI 初始化
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr) !get rank
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierr) !get processes
    print *, "Process ", myid,"of ",numprocs,"is alive!"
    ! 四块矩阵赋初值, 左右还有两列用于交换
    do j=1,mysize+2
        do i=1,totalsize
            A(i,j)=0.0
        end do
    end do
    ! 第0块赋初值
    if(myid==0) then
        do i=1,totalsize
            A(i,2)=8.0
        end do
    end if
    ! 第3块赋初值
    if(myid==3) then
        do i=1,totalsize
            A(i,mysize+1)=8.0
        end do
    end if
    ! proc all 赋初值
    do i=1,mysize+2
        A(1,i)=8.0
        A(totalsize,i)=8.0
    end do
    !计算相邻 proc 矩阵
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
    ! MPI 发送接收 tag
    tag1=3
    tag2=4
    ! 雅可比迭代
    do n=1,steps
        ! 绑定发送接收, proc:第5列  -> rihgt proc: 第1列
        call MPI_SENDRECV(
            A(1,mysize+1),
            totalsize,
            MPI_REAL,
            right,
            tag1,
            !---------------------------------------
            &A(1,1),
            totalsize,
            MPI_REAL,
            left,
            tag1,
            MPI_COMM_WORLD,
            status,ierr
            )
            ! 绑定发送接收, proc:第2列  -> left proc: 第6列
        call MPI_SENDRECV(
            A(1,2),
            totalsize,
            MPI_REAL,
            left,
            tag2,
            !---------------------------------------
            &A(1,mysize+2),
            totalsize,
            MPI_REAL,right,
            tag2,
            MPI_COMM_WORLD,
            status, ierr
            )
        ! all proc 矩阵的起始列，结束列
        begin_col=2
        end_col=mysize+1
        ! proc 0
        if(myid==0) then
            begin_col=3
        end if
        ! proc 3
        if(myid==3) then
            end_col=mysize
        end if
        ! 在每个 proc matrix 内部, 去除首位列, 迭代内部列
        do j=begin_col,end_col
            ! in each proc matrix, 去除首位行, 迭代内部行
            do i=2,totalsize-1
                B(i,j)=(A(i,j+1)+A(i,j-1)+A(i+1,j)+A(i-1,j))*0.25
            end do
        end do
        ! 将数据从缓冲变量 提取到 原变量
        do j=begin_col,end_col
            do i=2,totalsize-1
                A(i,j)=B(i,j)
            end do
        end do
    end do
    do i=2,totalsize-1
        print *, myid,(A(i,j),j=begin_col,end_col)
    end do
    call MPI_FINALIZE(rc)
end program