#include "mpi.h"
#include <iostream>

int main(int argc, char *argv[])
{
    int steps = 10; //迭代次数
    const int totalsize = 15;
    const int mysize = 4;
    double A[totalsize][mysize + 2];
    double B[totalsize][mysize + 2];
    MPI_Status status;
    // MPI 初始化
    MPI_Init(&argc, &argv);
    int myid = 0, numprocs = 0;
    MPI_Comm_rank(
        MPI_COMM_WORLD /*MPI_Comm comm*/,
        &myid /*int* size*/);
    MPI_Comm_size(
        MPI_COMM_WORLD, /*MPI_Comm comm*/
        &numprocs /* int* size */);
    std::cout << "Process " << myid << "of " << numprocs << "is alive!";
    for (int j = 0; j < mysize + 2; j++)
    {
        for (int i = 0; i < totalsize; i++)
        {
            A[i][j] = 0.0;
        }
    }
    //
    if (myid == 0)
        for (int i = 0; i < totalsize; i++)
        {
            A[i][2] = 8.0;
        }
    //
    if (myid == 3)
        for (int i = 0; i < totalsize; i++)
        {
            A[i][mysize + 1] = 8.0;
        }
    for (size_t i = 1; i < mysize + 2; i++)
    {
        A[1][i] = 8.0;
        A[totalsize][i] = 8.0;
    }
    //
    int left = 0, right = 0;
    if (myid > 0)
    {
        left = myid - 1;
    }
    else
    {
        left = MPI_PROC_NULL;
    }
    if (myid < 3)
    {
        right = myid + 1;
    }
    else
    {
        right = MPI_PROC_NULL;
    }
    int tag1 = 3, tag2 = 4;
    for (size_t i = 1; i < steps; i++)
    {
        MPI_Sendrecv(
            // void* sendbuf, int sendcount, MPI_Datatype sendtype, int dest
            &A[1][mysize + 1], // onst void *sendbuf
            totalsize,         // int sendcount
            MPI_REAL,          // MPI_Datatype sendtype
            right,             // int dest
            tag1,              // int sendtag,
            &A[1][1],          // void *recvbuf,
            totalsize,         // int recvcount,
            MPI_REAL,          // MPI_Datatype recvtype,
            left,              // int source,
            tag1,              // int recvtag,
            MPI_COMM_WORLD,    // MPI_Comm comm,
            &status            // MPI_Status *status
        );
        //
        MPI_Sendrecv(
            // void* sendbuf, int sendcount, MPI_Datatype sendtype, int dest
            &A[1][2],          // onst void *sendbuf
            totalsize,         // int sendcount
            MPI_REAL,          // MPI_Datatype sendtype
            left,              // int dest
            tag2,              // int sendtag,
            &A[1][mysize + 2], // void *recvbuf,
            totalsize,         // int recvcount,
            MPI_REAL,          // MPI_Datatype recvtype,
            right,             // int source,
            tag2,              // int recvtag,
            MPI_COMM_WORLD,    // MPI_Comm comm,
            &status            // MPI_Status *status
        );
        int begin_col = 2;
        int end_col = mysize + 1;
        //
        if (myid == 0)
        {
            begin_col = 3;
        }
        //
        if (myid == 3)
        {
            end_col = mysize;
        }
        //
        for (size_t j = begin_col; j < end_col; j++)
        {
            for (size_t i = 2; i < totalsize - 1; i++)
            {
                B[i][j] = (A[i][j + 1] + A[i][j - 1] + A[i + 1][j] + A[i - 1][j]) * 0.25;
            }
        }
        //
        for (size_t j = begin_col; j < end_col; j++)
        {
            for (size_t i = 2; i < totalsize - 1; i++)
            {
                A[i][j] = B[i][j];
            }
        }
    }
    for (size_t i = 2; i < totalsize - 1; i++)
    {
        for (size_t j = begin_col; j < end_col; i++)
        {
            std::cout << myid << A[i][j] << std::endl;
        }
    }
    // MPI 收尾
    MPI_Finalize();
}
