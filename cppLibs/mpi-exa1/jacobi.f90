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
    ! dsf
    if(myid==0) then 
        do i=1,totalsize 
            A(i,2)=8.0 
        end do 
    end if 
    ! sdf
    if(myid==3) then 
        do i=1,totalsize 
            A(i,mysize+1)=8.0 
        end do 
    end if 
    ! dsfd
    do i=1,mysize+2 
        A(1,i)=8.0 
        A(totalsize,i)=8.0 
    end do 
    !sdfs
    if(myid > 0) then 
        left=myid-1 
    else 
        left=MPI_PROC_NULL 
    end if 
    !dd
    if(myid < 3) then 
        right=myid+1 
    else 
        right=MPI_PROC_NULL 
    end if 
    tag1=3 
    tag2=4 
    !dsfsdfds
    do n=1,steps 
        call MPI_SENDRECV(A(1,mysize+1),totalsize,MPI_REAL,right,tag1,& 
                    A(1,1),totalsize,MPI_REAL,left,tag1,MPI_COMM_WORLD,status,ierr) 
        call MPI_SENDRECV(A(1,2),totalsize,MPI_REAL,left,tag2,& 
                    A(1,mysize+2),totalsize,MPI_REAL,right,tag2,MPI_COMM_WORLD,status,ierr) 
        !sdfas
        begin_col=2 
        end_col=mysize+1 
        if(myid==0) then 
            begin_col=3 
        end if 
        if(myid==3) then 
            end_col=mysize 
        end if 
        !!
        do j=begin_col,end_col 
            do i=2,totalsize-1 
                B(i,j)=(A(i,j+1)+A(i,j-1)+A(i+1,j)+A(i-1,j))*0.25 
            end do 
        end do 
        !!
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