#include <mpi.h>
#include <iostream>
#include <algorithm>
#include<windows.h>
//主进程主要干三件事: 定义数据, 发送数据和接收计算结果

const int cols = 10;
const int rows = 11;
double a[rows][cols] = { 0 }; //矩阵 [rows,cols]
double b[cols] = { 0 };// 列矩阵
double c[cols] = { 0 }; // 结果矩阵
double buffer[cols] = { 0 }; //缓冲变量
double ans = 0.0; // 存储结果
int myid = 0;
const int master_id = 0;
int numprocs = 8;
int numsent = 0; // 已发送行数
MPI_Status status;
int sender;
int anstype;
void master()
{
	for (int i = 0; i < cols; i++)
	{
		b[i] = 1;
		for (int j = 0; j < rows; j++)
		{
			a[i][j] = i + 1;
		}
	}
	// 2 每个从进程都接收相同的矩阵b,
	// 考虑使用广播操作, 即主进程将矩阵b向通信域内所有进程广播一下矩阵b,
	// 然后从进程就可以都接收到矩阵b这一变量了.
	MPI_Bcast(b, //void* buffer
		cols, //int count
		MPI_DOUBLE,//MPI_Datatype datatype
		master_id, //int root,
		MPI_COMM_WORLD//MPI_Comm comm
	);

	// 矩阵A采用了之前提到过的 MPI_SEND, 发送每行的数据
	for (int i = 1; i < min(numprocs, rows); i++)
	{
		for (int j = 0; j < cols; j++)
		{
			buffer[j] = a[i][j];
		}
		//发送矩阵A 的行数据, 使用矩阵行数作为 tag MPI_DOUBLE,
		MPI_Send(
			buffer,//const void* buf,
			cols, //int count,
			MPI_DOUBLE, //MPI_Datatype datatype,
			i, //int dest,
			i, //int tag,
			MPI_COMM_WORLD //MPI_Comm comm
		);
		numsent = numsent + 1; // 记录已发送的行数
	}
	// 3 在执行完发送步骤后, 需要将计算结果收回
	for (int i = 1; i < rows; i++)
	{
		MPI_Recv(
			&ans,//void* buf,
			1, //int count,
			MPI_DOUBLE, //MPI_Datatype datatype,
			MPI_ANY_SOURCE,//int source,
			MPI_ANY_TAG, //int tag,
			MPI_COMM_WORLD, //MPI_Comm comm,
			&status //MPI_Status * status
		);
		// sender用于记录已经将结果发送回主进程的从进程号
		// 因其已经发送回主进程, 即可代表该从进程已经处于空闲状态
		// 在之后的发送中, 就向空闲的进程继续发送计算任务
		sender = status.MPI_SOURCE;
		//在发送时, 所标注的tag和矩阵的行标是相同的,
		//因此直接用 c(anstype)=ans来在对应位置存储结果
		anstype = status.MPI_TAG;
		c[anstype] = ans;
		// numsent 是已发送行, 用于判断是否发送完所有行
		if (numsent < rows)
		{
			for (int j = 0; j < cols; j++)
			{
				buffer[j] = a[numsent][j];
			}
			MPI_Send(
				buffer,
				cols,
				MPI_DOUBLE,
				sender,
				numsent,
				MPI_COMM_WORLD
			);
			numsent = numsent + 1;
		}
		else
			//当都发送完之后, 向从进程发送一个空信息,
			//从进程接收到空信息时, 即执行MPI_FINALIZE来结束.
		{
			int tmp = 1.0;
			MPI_Send(
				&tmp, 0, MPI_DOUBLE,
				sender, master_id, MPI_COMM_WORLD
			);
		}
	}
}

void slave()
{

	while (1)
	{
		MPI_Recv(
			buffer, cols,
			MPI_DOUBLE,
			master_id,
			MPI_ANY_TAG,
			MPI_COMM_WORLD,
			&status);
		//直到矩阵A的所有行都计算完成后, 主进程会发送一个tag为0的空消息,
		if (status.MPI_TAG != 0)
		{
			int row = status.MPI_TAG;
			ans = 0.0;
			for (int i = 0; i < cols; i++)
			{
				ans = ans + buffer[i] * b[i];
			}
			MPI_Send(
				&ans, 1,
				MPI_DOUBLE,
				master_id,
				row,
				MPI_COMM_WORLD
			);
		}
	}
}

int main(int argc, char* argv[])
{

	//----------------------------------
	// MPI 初始化
	MPI_Init(&argc, &argv);
	// 获取rank
	MPI_Comm_rank(
		MPI_COMM_WORLD, // MPI_Comm comm,
		&myid         // int* size
	);
	//获取进程数
	MPI_Comm_size(
		MPI_COMM_WORLD, // MPI_Comm comm
		&numprocs       // int* size
	);
	// 打印进程信息
	std::cout << "Process " << myid << " of " << numprocs << " is alive!" << std::endl;
	//----------------------------------

	if (myid == master_id)
	{
		master(); //主进程的程序代码
	}
	else
	{
		slave(); //从进程的程序代码
	}

	// MPI 收尾
	MPI_Finalize();
}
