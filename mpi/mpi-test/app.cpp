#include <iostream>
#include <mpi.h>

using namespace std;

int say_hello(int argc, char **argv);

int say_hello(int argc, char **argv) {
  int myid, numprocs;
  int namelen;
  char processor_name[MPI_MAX_PROCESSOR_NAME];
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &myid);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Get_processor_name(processor_name, &namelen);
  cout << "Hello World! Process " << myid << " of " << numprocs << " on "
       << processor_name << "\n";
  MPI_Finalize();

  return 0;
}

int main(int argc, char **argv) {
  say_hello(argc, argv);

  return 0;
}
