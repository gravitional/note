#include "mpi.h"
#include <stdio.h>
double f(double);
double f(double x)
{
    return (4.0 / (1.0 + x * x));
}
int main(int argc, char *argv[])
{
    int myid{0}, numprocs{0};
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    printf("Process %d of %d.\n", myid, numprocs);
    int n = 1000;                            //循环的总次数
    double h = 1.0 / static_cast<double>(n); //循环宽度
    double sum = 0.0;
    for (int i = myid + 1; i <= n; i += numprocs)
    {
        double x = h * ((double)i - 0.5);
        sum += f(x);
    }
    double mypi = h * sum;
    double pi{0};
    MPI_Reduce(&mypi, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
    if (myid == 0)
    {
        printf("The result is %.10f.\n", pi);
    }
    MPI_Finalize();
}