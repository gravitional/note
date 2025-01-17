# FindMPI

查找消息传递接口(MPI)的实现.

消息传递接口(MPI)是一个用于编写高性能分布式内存并行应用程序的库, 通常部署在一个集群上.
MPI是一个标准接口(由MPI论坛定义), 有许多实现.

*3.10版中的新内容*:
对模块进行了大修: 许多新的变量, 每个语言组件, 支持更多种类的运行时.

## 使用MPI的变量

该模块公开了C, CXX, MPICXX和Fortran等组件.
每一个都控制着要搜索的各种MPI语言.

CXX和MPICXX之间的区别是:
CXX指的是MPI C API可以从C++中使用,
而MPICXX指的是MPI-2的C++ API, 在MPI-3中又被移除.

根据所启用的组件, 下列变量将被设置.

MPI_FOUND
表示已经找到所有要求的语言的MPI设置的变量.
如果没有指定任何组件, 如果检测到所有启用的语言的MPI设置,
这就是真的. 注意, MPICXX组件不影响这个变量.

MPI_VERSION
在所请求的语言中检测到的MPI的最小版本, 如果没有指定组件, 则检测到所有启用的语言.

这个模块将为你项目中的每一种语言设置以下变量,
其中<lang>是C, CXX或Fortran中的一种.

MPI_<lang>_FOUND
变量表示找到了<lang>的MPI设置,
并且简单的MPI测试程序用所提供的设置进行编译.

MPI_<lang>_COMPILER
用于<lang>的MPI编译器, 如果存在这样的程序.

MPI_<lang>_COMPILE_OPTIONS
用于<lang>中的MPI程序的编译选项, 以;-list的形式给出.

MPI_<lang>_COMPILE_DEFINITIONS
用于<lang>中的MPI程序的编译定义, 以;-list的形式给出.

`MPI_<lang>_INCLUDE_DIRS`
包含MPI头文件的路径(s).

MPI_<lang>_LINK_FLAGS
用于MPI程序的链接器标志.

`MPI_<lang>_LIBRARIES`
所有用于连接MPI程序的库.
*3.9版中的新内容*: 此外, 还定义了以下IMPORTED目标.

MPI::MPI_<lang>
的目标用于使用<lang>的MPI的目标.

+ 下列变量中, 若绑定存在, 则被定义:

MPI_MPICXX_FOUND
表示MPI-2 C++绑定是否存在的变量(在MPI-2中引入, 在MPI-3中移除).

MPI_Fortran_HAVE_F77_HEADER
如果Fortran 77头文件mpif.h可用, 则为真.

MPI_Fortran_HAVE_F90_MODULE
如果可以使用Fortran 90模块mpi来访问MPI(仅MPI-2及以上版本), 则为
真.

MPI_Fortran_HAVE_F08_MODULE
如果Fortran 2008 mpi_f08对MPI程序可用, 则为真(仅MPI-3及更高版
本).

如果可能的话, MPI版本将由这个模块决定.
检测MPI版本的设施是在MPI-1.2中引入的, 因此无法在较早的MPI版本中找到.

`MPI_<lang>_VERSION_MAJOR`
由MPI发行版为<lang>实现的MPI主要版本.

`MPI_<lang>_VERSION_MINOR`
MPI发行版为<lang>实现的MPI的次要版本.

`MPI_<lang>_VERSION`
由MPI发行版为<lang>实现的MPI版本.

注意, 通过mpi.h访问C语言绑定没有变量,
因为MPI标准一直要求这种绑定在C和C++代码中都能工作.

+ 对于运行MPI程序, 该模块设置了以下变量

`MPIEXEC_EXECUTABLE`
用于运行 MPI程序 的可执行程序, 如果存在的话.

`MPIEXEC_NUMPROC_FLAG`
在给 `mpiexec`提供运行的处理器数量之前传递给它的标志.

`MPIEXEC_MAX_NUMPROCS`
要利用的MPI处理器的数量. 默认为主机系统上检测到的处理器数量.

`MPIEXEC_PREFLAGS`
在可执行程序运行前直接传递给mpiexec的标志.

`MPIEXEC_POSTFLAGS`
在其他标志之后传递给mpiexec的标志.
