#include <mpi.h>
#include <iostream>
#include <iomanip>
#include <algorithm>

// 全局变量初始化
const int rows = 100;
const int cols = 100;
double a[rows][cols] = { 0 }; //矩阵 [rows,cols]
double b[cols] = { 0 };       // 列矩阵
double c[rows] = { 0 };       // 存储结果矩阵
double buffer[cols] = { 0 };  //缓冲变量
int numsent = 0;            // 已发送行数
int numprocs = 0;           //设置进程数
const int m_id = 0;         // 主进程 id
const int end_tag = 0;      // 标志发送完成的 tag
MPI_Status status;          // MPI 状态

// 主进程主要干三件事: 定义数据, 发送数据和接收计算结果
void master()
{
	// 1准备数据
	for (int i = 0; i < cols; i++)
	{
		b[i] = i;
		for (int j = 0; j < rows; j++)
		{
			a[i][j] = i + 1;
		}
	};
	// 2 每个从进程都接收相同的矩阵b,
	// 考虑使用广播操作, 即主进程将矩阵b向通信域内所有进程广播一下矩阵b,
	// 然后从进程就可以都接收到矩阵b这一变量了.
	MPI_Bcast(
		b,             // void* buffer
		cols,          // int count
		MPI_DOUBLE,    // MPI_Datatype datatype
		m_id,          // int root,
		MPI_COMM_WORLD // MPI_Comm comm
	);
	// 矩阵A采用了之前提到过的 MPI_SEND, 发送每行的数据
	// 实际的子进程数是 numprocs - 1 个
	for (int i = 0; i < std::min(numprocs - 1, rows); i++)
	{
		for (int j = 0; j < cols; j++)
		{
			buffer[j] = a[i][j];
		}
		//发送矩阵A 的行数据, 使用矩阵行数作为 tag MPI_DOUBLE,
		MPI_Send(
			buffer,        // const void* buf,
			cols,          // int count,
			MPI_DOUBLE,    // MPI_Datatype datatype,
			i + 1,         // int dest, 0 列发给 rank 1, 以此类推
			i + 1,         // int tag, row number +1
			MPI_COMM_WORLD // MPI_Comm comm
		);
		numsent = numsent + 1; // 记录已发送的行数
	};
	// 3 在执行完发送步骤后, 需要将计算结果收回
	double ans = 0.0;           // 存储结果的元素
	for (int i = 0; i < rows; i++)
	{
		MPI_Recv(
			&ans,           // void* buf,
			1,              // int count,
			MPI_DOUBLE,     // MPI_Datatype datatype,
			MPI_ANY_SOURCE, // int source,
			MPI_ANY_TAG,    // int tag,
			MPI_COMM_WORLD, // MPI_Comm comm,
			&status         // MPI_Status * status
		);
		// sender 用于记录已经将结果发送回主进程的从进程号
		int sender = status.MPI_SOURCE;
		//在发送时, 所标注的 tag = 矩阵的行号+1,
		int rtag = status.MPI_TAG - 1;
		c[rtag] = ans; //用 c(rtag)=ans来在对应位置存储结果
		// numsent 是已发送行, 用于判断是否发送完所有行
		// 因其已经发送回主进程, 即可代表该从进程已经处于空闲状态
		// 在之后的发送中, 就向空闲的进程继续发送计算任务
		if (numsent < rows)
		{
			// 获取下一列
			for (int j = 0; j < cols; j++)
			{
				buffer[j] = a[numsent][j];
			}
			MPI_Send(
				buffer, cols, MPI_DOUBLE,
				sender, numsent + 1, MPI_COMM_WORLD);
			numsent = numsent + 1;
		}
		//当都发送完之后, 向从进程发送一个空信息,
		//从进程接收到空信息时, 即执行MPI_FINALIZE来结束.
		else
		{
			int tmp = 1.0;
			MPI_Send(
				&tmp, 0, MPI_DOUBLE,
				sender, end_tag, MPI_COMM_WORLD);
		}
	};
}

// 子进程
void slave()
{
	//从进程首先需要接收主进程广播的矩阵b
	MPI_Bcast(b, cols, MPI_DOUBLE, m_id, MPI_COMM_WORLD);
	while (1)
	{
		MPI_Recv(
			buffer, cols, MPI_DOUBLE,
			m_id, MPI_ANY_TAG, MPI_COMM_WORLD,
			&status);
		//直到矩阵A的所有行都计算完成后, 主进程会发送 tag 为 end_tag 的空消息,
		if (status.MPI_TAG != end_tag)
		{
			int row = status.MPI_TAG;
			double ans = 0.0;
			for (int i = 0; i < cols; i++)
			{
				ans = ans + buffer[i] * b[i];
			}
			MPI_Send(
				&ans, 1, MPI_DOUBLE,
				m_id, row, MPI_COMM_WORLD);
		}
		else {
			break;
		}
	}
}

int main(int argc, char* argv[])
{
	//----------------------------------
	// MPI 初始化
	MPI_Init(&argc, &argv);
	//获取进程总数
	MPI_Comm_size(
		MPI_COMM_WORLD, // MPI_Comm comm
		&numprocs       // int* size
	);
	// 获取rank
	int myid = 0; // rank number
	MPI_Comm_rank(
		MPI_COMM_WORLD, // MPI_Comm comm,
		&myid           // int* size
	);
	// 打印进程信息
	std::cout << "Process " << myid << " of " << numprocs << " is alive!" << std::endl;
	//----------------------------------
	if (myid == m_id)
	{
		master(); //主进程的程序代码
	}
	else
	{
		slave(); //从进程的程序代码
	}
	// 打印结果
	if (myid == m_id) {
		for (int i = 0; i < rows; i++)
		{
			std::cout << "the ele (" << i << "): "
				<< std::setiosflags(std::ios_base::right)
				<< std::setw(15) << c[i]
				<< std::resetiosflags(std::ios_base::right)
				<< std::endl;
		}
	}
	// MPI 收尾
	MPI_Finalize();
}
