#include "mpi.h"
#include <iostream>
#include <iomanip>

int main(int argc, char *argv[])
{
    int steps = 10;                        // 迭代次数
    const int totalsize = 16;              // 矩阵维度
    const int mysize = 4;                  // MPI 分块数目, 4 个 processes
    double A[totalsize][mysize + 2] = {0}; // all proc data matrix
    double B[totalsize][mysize + 2] = {0}; // all proc buffer matrix
    int begin_col = 1;                     // all proc 矩阵的起始列
    int end_col = mysize + 1;              // all proc 矩阵的结束列
    int myid = 0, numprocs = 0;
    int left = 0, right = 0;
    int tag1 = 3, tag2 = 4; // MPI 发送接收 tag
    MPI_Status status;      // MPI 状态
    //----------------------------------
    // MPI 初始化
    MPI_Init(&argc, &argv);
    // 获取rank
    MPI_Comm_rank(
        MPI_COMM_WORLD // MPI_Comm comm,
        & myid         // int* size
    );
    //获取进程数
    MPI_Comm_size(
        MPI_COMM_WORLD, // MPI_Comm comm
        &numprocs       // int* size
    );
    // 打印进程信息
    std::cout << "Process " << myid << " of " << numprocs << " is alive!" << std::endl;
    //----------------------------------
    // 4 proc 矩阵赋初值, 左右额外两列用于交换
    for (int j = 0; j < mysize + 2; j++)
    {
        for (int i = 0; i < totalsize; i++)
        {
            A[i][j] = 0.0;
        }
    }
    // proc 0 额外初值
    if (myid == 0)
        for (int i = 0; i < totalsize; i++)
        {
            A[i][1] = 8.0;
        }
    // proc 3 额外初值
    if (myid == 3)
        for (int i = 0; i < totalsize; i++)
        {
            A[i][mysize] = 8.0;
        }
    // proc all 赋初值
    for (int i = 0; i < mysize + 2; i++)
    {
        A[0][i] = 8.0;
        A[totalsize - 1][i] = 8.0;
    }
    // 计算相邻 proc 矩阵
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
    // 雅可比迭代 整体步数
    for (int i = 0; i < steps; i++)
    {
        // 绑定发送接收, proc:第4列  -> rihgt proc: 第0列
        MPI_Sendrecv(
            &A[0][mysize], // onst void *sendbuf; 最右侧数据, 要发送给下一个 proc
            totalsize,     // int sendcount; 数据长度
            MPI_DOUBLE,    // MPI_Datatype  sendtype; 数据类型
            right,         // int dest
            tag1,          // int sendtag,
            //---------------------------------------
            &A[0][0],       // void *recvbuf; 接收数据位置
            totalsize,      // int recvcount; 接收长度
            MPI_DOUBLE,     // MPI_Datatype  recvtype; 数据类型
            left,           // int source; 接收源
            tag1,           // int recvtag; 接收 tag
            MPI_COMM_WORLD, // MPI_Comm comm; 通信域
            &status         // MPI_Status *status; 发送或接收状态;
        );
        // 绑定发送接收, proc:第1列  -> left proc: 第5列
        MPI_Sendrecv(
            &A[0][1],   // onst void *sendbuf
            totalsize,  // int sendcount
            MPI_DOUBLE, // MPI_Datatype sendtype
            left,       // int dest
            tag2,       // int sendtag,
            //---------------------------------------
            &A[0][mysize + 1], // void *recvbuf,
            totalsize,         // int recvcount,
            MPI_DOUBLE,        // MPI_Datatype recvtype,
            right,             // int source,
            tag2,              // int recvtag,
            MPI_COMM_WORLD,    // MPI_Comm comm,
            &status            // MPI_Status *status
        );
        // proc 0
        if (myid == 0)
        {
            begin_col = 2; // 0th 列是缓冲, 1st 列缺少左边, 所以从 2nd 开始
        }
        // proc 3
        if (myid == 3)
        {
            end_col = mysize - 1; //类似上面
        }
        // 在每个 proc matrix 内部, 去除首位列, 迭代内部列
        for (int j = begin_col; j < end_col; j++)
        {
            // in each proc matrix, 去除首位行, 迭代内部行
            for (int i = 1; i < totalsize - 1; i++)
            {
                B[i][j] = (A[i][j + 1] + A[i][j - 1] + A[i + 1][j] + A[i - 1][j]) * 0.25;
            }
        }
        // 将数据从缓冲变量 提取到 原变量
        for (int j = begin_col; j < end_col; j++)
        {
            for (int i = 1; i < totalsize - 1; i++)
            {
                A[i][j] = B[i][j];
            }
        }
    }
    // 打印迭代结果
    for (int row = 1; row < totalsize - 1; row++)
    {
        std::cout << "proc (" << myid << "):  ";
        for (int col = begin_col; col < end_col; col++)
        {
            std::cout << std::setiosflags(std::ios_base::left)
                      << std::setw(15) << A[row][col]
                      << std::resetiosflags(std::ios_base::left);
        }
        std::cout << std::endl;
    }
    // MPI 收尾
    MPI_Finalize();
}
