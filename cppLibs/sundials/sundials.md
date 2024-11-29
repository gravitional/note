# SUNDIALS

`SUNDIALS` 包括用于常微分方程 (ODE) 系统的求解器 `CVODE` 和 `ARKODE`,
用于微分代数 (differential-algebraic, DAE) 系统的 IDA
和用于非线性代数系统(nonlinear algebraic systems)的 KINSOL.

此外, SUNDIALS 还包括 CVODE 和 IDA 的变体,
具有灵敏度分析功能(sensitivity analysis, 使用正向或辅助方法),
分别称为 `CVODES` 和 `IDAS`.
以下列表概括了 `SUNDIALS` 软件包的基本功能:

+ `CVODE`: 基于 `Adams` 和 `BDF` 方法的 刚性和非刚性 ODE系统求解器
$\dot{y}=f(t,y)$
+ `CVODES`, 具有 灵敏度分析功能的 刚性和非刚性 ODE系统 求解器;

+ `ARKODE`, 基于 Runge-Kutta 方法的刚性, 非刚性, 混合刚性-非刚性 和 多周期(multirate) ODE系统求解器 $M(t)\dot{y}=f_1(t,y)+f_2(t,y)$
+ IDA, 基于 BDF 方法的 微分代数系统 求解器 $F(t,y,\dot{y})=0$
+ `IDAS`, 具有 灵敏度分析功能 的 微分代数系统求解器;
+ `KINSOL`, 非线性代数系统 求解器 $F(u)=0$.

suite中的各种软件包有许多共同的组件, 并被组织成一个系列.
图 8.1 给出了高层概览: 求解器软件包, 共享向量, 矩阵,
线性求解器 和 非线性求解器接口(抽象基类)
以及 SUNDIALS 提供的相应 class implementations.

![high level](https://sundials.readthedocs.io/en/latest/_images/sunorg1.png)

对于提供第三方库(即 LAPACK, KLU, SuperLU_MT, SuperLU_DIST, hypre, PETSc, Trilinos 和 Raja)接口的类,
用户需要在 SUNDIALS 之外下载并编译这些软件包.
目录结构如图 8.2 所示.

![3rd packages](https://sundials.readthedocs.io/en/latest/_images/sunorg2.png)

## Using SUNDIALS

如第 7.3 节所述, 组成 SUNDIALS 的六个求解器软件包
(CVODE(S), IDA(S), ARKODE, KINSOL)
都建立在 矢量, 矩阵 和 代数求解器 的通用类/模块之上.
此外, 这六个软件包还利用了其他一些共同的基础架构, 我们将在本节中讨论.

所有SUNDIALS 对象(矢量, 线性和非线性求解器, 矩阵等), 构成一个 SUNDIALS simulation,
它持有一个由 `SUNContext` 类定义的 通用仿真上下文对象 的引用.

`SUNContext` 类/类型在头文件 `sundials/sundials_context.h` 中定义为

```cpp
typedef struct _SUNContext *SUNContext
```

用户在调用任意 `SUNDIALS` 库函数之前,
应先创建一个 `SUNContext` 对象, 方法如下

```cpp
int SUNContext_Create(void *comm, SUNContext *ctx)
```

创建与 执行线程 相关联的 `SUNContext` 对象.
`SUNContext` 类的数据是私有的.

参数
`comm` - MPI 通信器指针, 如果不使用 MPI, 则为 NULL.
`ctx` - `[in,out]`; 成功退出时, 指向新创建的 SUNContext 对象的指针.

返回值
如果发生错误, 返回 <`0`, 否则返回 `0`.

创建的 `SUNContext` 对象应提供给不同 SUNDIALS 类/模块的构造函数例程, 例如

```cpp
SUNContext sunctx;
void* package_mem;
N_Vector x;

SUNContext_Create(NULL, &sunctx);

package_mem = CVodeCreate(..., sunctx);
package_mem = IDACreate(..., sunctx);
package_mem = KINCreate(..., sunctx);
package_mem = ARKStepCreate(..., sunctx);

x = N_VNew_<SomeVector>(..., sunctx);
```

在所有其他 `SUNDIALS` 代码之后, 应调用以下命令释放 `SUNContext` 对象:

```cpp
int SUNContext_Free(SUNContext *ctx)
```

释放 `SUNContext` 对象.

参数:
`ctx` - 指向有效 `SUNContext` 对象的指针, 成功返回时为 `NULL`.
返回值
如果发生错误, 返回 `<0`, 否则返回 `0`.

>警告
使用 `MPI` 时, 必须在调用 `MPI_Finalize` 之前调用 `SUNContext_Free()`.

## 安装

[14.SUNDIALS Installation Procedure](https://sundials.readthedocs.io/en/latest/Install_link.html#installation)

任何 SUNDIALS package 的安装都是通过安装整个 SUNDIALS suite来完成的,
具体步骤如下.
无论下载的文件是否包含 SUNDIALS 的一个或所有求解器, 安装步骤都是一样的.

SUNDIALS 套件(或单个求解器)以压缩包 (`.tar.gz`) 的形式发布.
分发压缩包的名称为 `SOLVER-X.Y.Z.tar.gz`,
其中 SOLVER 为: `sundials`, `cvode`, `cvodes`, `arkode`, `ida`, `idaas` 或 `kinsol` 之一,
`X.Y.Z` 代表版本号(SUNDIALS 套件或单个求解器的版本号).
要开始安装, 首先解压缩并展开源代码, 方法

```bash
tar -zxf SOLVER-X.Y.Z.tar.gz
```

这将把源文件 解压缩到目录 `SOLVER-X.Y.Z`.

从 SUNDIALS 2.6.0 版开始, CMake 是唯一支持的安装方法.
对安装过程的解释从一些常见的看法开始:

1. 本章其余部分将遵循这些约定:

`SOLVERDIR` 是上面创建的 `SOLVER-X.Y.Z` 目录, 即包含 SUNDIALS 源代码的目录.
`BUILDDIR` 是构建 `SUNDIALS` 的(临时)目录.
`INSTDIR` 是安装 `SUNDIALS` exported header files 和 libraries 的目录.
通常, 头文件导出到 `INSTDIR/include` 目录下, 而库则安装在 `INSTDIR/lib` 目录下, `INSTDIR` 在 cmake configuration 时指定.

2. 在 SUNDIALS 基于 CMake 的安装中, 禁止在源代码中构建;
换句话说, 构建目录 `BUILDDIR` 不能与 `SOLVERDIR` 相同, 否则会导致错误.
这样可以防止 "污染" 源代码树, 并允许针对不同的配置和/或选项进行高效的编译.

3. 安装目录 `INSTDIR` 不能与源代码目录 `SOLVERDIR` 相同.

4. 默认情况下, 只有库和头文件会导出到安装目录 `INSTDIR`.
如果用户启用(通过 CMake 的适当开关),
随 `SUNDIALS` 发布的示例将与求解器库一起编译,
但安装步骤会导出(默认在安装目录的子目录下)示例源代码 和 示例输出,
以及自动生成的配置文件, 这些文件会引用 *已安装* 的 SUNDIALS 头文件和库.
因此, 这些 SUNDIALS 示例的 配置文件 可用作您自己问题的 "模板".
CMake 会安装 `CMakeLists.txt` 文件和 `Makefile` 文件(作为选项, 仅适用于 Unix/Linux).
请注意, 这种安装方式还允许在不安装 SUNDIALS 示例的情况下构建它们.
(这可用作对新构建的库的正确性检查).

关于基于 CMake 的安装程序的更多细节, 手动编译的说明,
以及最终安装的库和导出的头文件的路线图, 请参阅第 14.1 节和第 14.2 节.

### 14.1. 基于 CMake 的安装

基于 CMake 的安装提供了与平台无关的构建系统.
CMake 可以从同一个配置文件中生成 Unix 和 Linux Makefile
以及 KDevelop, Visual Studio 和(Apple)XCode 项目文件.
此外, CMake 还提供图形用户界面前端, 允许交互式构建和安装过程.

SUNDIALS SUNDIALS 的编译过程需要 CMake 3.12.0 或更高版本, 以及可用的 C 编译器.
在 Unix 类操作系统上, 它还需要 Make(以及用于 CMake, ccmake 或 cmake-gui 图形用户界面前端的 `curses`, 包括其开发库),
而在 Windows 上则需要 Visual Studio.
虽然许多 Linux 发行版都提供 CMake, 但其中包含的版本可能已经过时.
CMake 会定期添加新功能,
你应该从 [http://www.cmake.org](http://www.cmake.org) 下载最新版本.
CMake 的构建说明(仅适用于类 Unix 系统)可在 CMake 网站上找到.
安装 CMake 后, Linux/Unix 用户可以使用 ccmake 或 cmake-gui(取决于 CMake 的版本), 而 Windows 用户则可以使用 CMakeSetup.

如前所述, 在使用 CMake 配置, 构建和安装 SUNDIALS 时, 始终需要使用单独的构建目录.
虽然 in-source 是可能的, 但 SUNDIALS CMake 脚本明确禁止这样做
(原因之一是, 与 autotools 不同, CMake 不提供 make distclean 程序, 因此很难在源码内编译后清理源码树).
通过确保单独的build目录, 用户只需删除build目录, 就能轻松清理build的所有痕迹.
CMake 会生成一个 `make clean` 来删除编译器和链接器生成的文件.

#### 14.1.1. 在类 Unix 系统上配置, 编译和安装

CMake 的默认配置将build所有包含的求解器和相关示例, 并build静态库和共享库.
`INSTDIR` 默认为 `/usr/local`,
可通过设置 `CMAKE_INSTALL_PREFIX` 变量进行更改.
对 `FORTRAN` 和所有其他选项的支持都被禁用.

CMake 可通过 `cmake` 命令在命令行中使用,
或通过 `ccmake` 命令在基于 curses 的图形用户界面中使用,
或通过 `cmake-gui` 命令在基于 wxWidgets 或 QT 的图形用户界面中使用.
我们将介绍使用文本和图形方法的示例.
在示例中, 假定 SUNDIALS 顶层目录下有相应的源代码, 编译和安装目录:

```bash
$ mkdir (...)/INSTDIR
$ mkdir (...)/BUILDDIR
$ cd (...)/BUILDDIR
```

##### 14.1.1.1. 使用 GUI 构建

通过 ccmake GUI 使用 CMake 遵循一般流程:

1. 选择并修改值, 运行 configure (c 键)
2. 新值用星号表示
3. 要设置变量, 将光标移至变量处并按回车键
    + 如果是布尔值(ON/OFF), 将切换该值
    + 如果是字符串或文件, 则允许编辑字符串
    + 对于文件和目录, 可使用 <tab> 键完成设置

4. 重复上述操作, 直到所有值都设置为所需值, 并可使用生成选项(g 键)
5. 某些变量(高级变量)无法立即显示;要查看高级变量, 请切换到高级模式(t 键)
6. 要搜索变量, 按 `/` 键, 要重复搜索, 按 `n` 键

通过 cmake-gui GUI 使用 CMake 的过程与此类似:

1. 选择并修改值, 点击 `Configure`
2. 第一次点击 `Configure` 时, 请确保选择了合适的生成器(以下假设生成的是 Unix Makfile).
3. 新值用红色高亮显示
4. 要设置变量, 请单击该变量或将光标移至该变量, 然后按回车键
    + 如果是布尔值(ON/OFF), 将选中/取消选中该复选框
    + 如果是字符串或文件, 则允许编辑字符串.
    + 此外, 在条目的最右侧会出现一个省略号按钮....
    单击该按钮将弹出文件或目录选择对话框.
    + 对于文件和目录, 可使用 `<tab>` 键补全
5. 重复上述操作, 直到所有值都设置完毕, 然后单击 `Generate` 按钮
6. 某些变量(高级变量)无法立即显示;要查看高级变量, 请单击 `advanced` 按钮

要使用 `curses GUI` 生成默认配置, 请从 BUILDDIR 输入 ccmake 命令并指向 SOLVERDIR:

```bash
$ ccmake (...)/SOLVERDIR
```

同样, 要使用 wxWidgets 图形用户界面构建默认配置, 可从 BUILDDIR 输入 cmake-gui 命令并指向 SOLVERDIR:

```bash
$ cmake-gui (...)/SOLVERDIR
```

默认的 curses 配置界面如下图所示.

![curses](https://sundials.readthedocs.io/en/latest/_images/ccmakedefault.png)

图 14.1 默认配置屏幕.
注: 初始屏幕为空.
要获取默认配置, 请反复按 `c`键(接受星号表示的默认值), 直到出现 `g` 选项.

如下图所示, 可以通过设置 `CMAKE_INSTALL_PREFIX` 和 `EXAMPLES_INSTALL_PATH` 来更改 `SUNDIALS` 和相应示例的默认 `INSTDIR`.

![_images/ccmakeprefix.png](https://sundials.readthedocs.io/en/latest/_images/ccmakeprefix.png)
图 14.2 更改 SUNDIALS 和相应 EXAMPLES 的 INSTDIR.

按 `g` 键或点击 `generate` 将生成 Makefile,
其中包括在此系统上构建 SUNDIALS 的所有 dependencies 和所有 rules .
回到命令提示符, 现在可以运行

```bash
$ make
```

或更快地并行编译(例如使用 4 个线程), 可以运行

```bash
$ make -j 4
```

要在配置中指定的安装目录下安装 SUNDIALS, 只需运行

```bash
$ make install
```

#### 14.1.1.2. 从命令行构建

在命令行中使用 CMake, 只需通过 cmake 命令指定 CMake 变量设置
以下命令将构建默认配置:

```bash
$ cmake -DCMAKE_INSTALL_PREFIX=/home/myname/sundials/instdir \
>  -DEXAMPLES_INSTALL_PATH=/home/myname/sundials/instdir/examples \
>  ../srcdir
$ make
$ make install
```

### 14.1.2. 配置选项 (Unix/Linux)

基于 CMake 的 SUNDIALS 配置的所有可用选项的完整列表如下.
请注意, 所显示的默认值是 Linux 系统上的典型配置, 仅供参考.

`BUILD_ARKODE`
构建 ARKODE 库
默认值: `ON`

`BUILD_CVODE`
构建 CVODE 库
默认值: `ON`

`BUILD_CVODES`
构建 CVODES 库
默认值: ON

`BUILD_IDA`
构建 IDA 库
默认值: `ON`

`BUILD_IDAS`
构建 IDAS 库
默认值: `ON`

`BUILD_KINSOL`
构建 KINSOL 库
默认值: `ON`

`BUILD_SHARED_LIBS`
构建共享库
默认值: `ON`

`BUILD_STATIC_LIBS`
构建静态库
默认值: `ON`

`CMAKE_BUILD_TYPE`
选择build类型,
选项有 `None`, `Debug`, `Release`, `RelWithDebInfo` 和 `MinSizeRel`
默认值:

注意:
指定 build type 将触发下面相应的 特定build type compiler flag options,
这些选项将附加到 `CMAKE_<language>_FLAGS` 设置的 flags.

`CMAKE_C_COMPILER`
C 编译器
默认值: `/usr/bin/cc`

`CMAKE_C_FLAGS`
C 编译器的 Flags
默认值:

`CMAKE_C_FLAGS_DEBUG`
C 编译器在 debug builds时 使用的flags
默认值: `-g`

`CMAKE_C_FLAGS_MINSIZEREL`
C 编译器在发布最小尺寸版本时使用的标记
默认值: `-Os -DNDEBUG`

`CMAKE_C_FLAGS_RELEASE`
C 编译器在发布版本时使用的标记
默认值: `-O3 -DNDEBUG`

`CMAKE_C_STANDARD`
用于构建 SUNDIALS C 部分的 C 标准.
默认值:  `99`
选项 `90`, `99`, `11`, `17`.

`CMAKE_C_EXTENSIONS`
启用编译器特定的 C 扩展.
默认值: `OFF`

`CMAKE_CXX_COMPILER`
C++ 编译器
默认值: `/usr/bin/c++`

注意
只有启用了需要 C++ 的功能(如 CUDA, HIP, SYCL, RAJA 等)
或启用了 C++ 示例时, 才需要 C++ 编译器.

所有 SUNDIALS 解算器均可在 C++ 应用程序中使用,
无需设置任何额外的配置选项.

`CMAKE_CXX_FLAGS`
C++ 编译器的标志
默认值:

`CMAKE_CXX_FLAGS_DEBUG`
C++ 编译器在调试编译时使用的标志
默认值: `-g`

CMAKE_CXX_FLAGS_MINSIZEREL
C++ 编译器在发布最小尺寸版本时使用的标记
默认值: `-Os -DNDEBUG`

`CMAKE_CXX_FLAGS_RELEASE`
C++ 编译器在发布版本时使用的标记
默认值: `-O3 -DNDEBUG`

`CMAKE_CXX_STANDARD`
用于构建 SUNDIALS C++ 部分的 C++ 标准.
默认值: `11`
选项 `98`, `11`, `14`, `17`, `20`.

`CMAKE_CXX_EXTENSIONS`
启用编译器特定的 C++ 扩展.
默认值: `OFF`

`CMAKE_Fortran_COMPILER`
Fortran 编译器
默认值:  `/usr/bin/gfortran`

注意

只有启用了 Fortran-C 支持 (BUILD_FORTRAN_MODULE_INTERFACE)
或 LAPACK (`ENABLE_LAPACK`) 支持,
才会触发 `Fortran` 支持(以及所有相关选项).

`CMAKE_Fortran_FLAGS`
用于 Fortran 编译器的标志
默认值:

`CMAKE_Fortran_FLAGS_DEBUG`
Fortran 编译器在调试构建时使用的标志
默认值: `-g`

`CMAKE_Fortran_FLAGS_MINSIZEREL`
Fortran编译器在发行版最小尺寸编译时使用的标记
默认值: `-Os`

`CMAKE_Fortran_FLAGS_RELEASE`
Fortran编译器在发布版编译时使用的标记
默认值: `-O3`

`CMAKE_INSTALL_LIBDIR`
安装库的目录.
默认值: 根据系统设置: `lib`, `lib64` 或 `lib/<multiarch-tuple>`

`CMAKE_INSTALL_PREFIX`
安装路径前缀, 作为安装目录的前缀
默认值: `/usr/local`

注意
用户必须有通过此选项指定位置的写入权限.
导出的 SUNDIALS 头文件和库将分别安装在
`CMAKE_INSTALL_PREFIX` 的 `include` 和 `lib` 子目录下.

`ENABLE_CUDA`
Build SUNDIALS CUDA 模块.
默认值: `OFF`

`CMAKE_CUDA_ARCHITECTURES`
指定要编译的 CUDA 架构.
默认值: `sm_30`

`ENABLE_XBRAID`
启用或禁用 ARKStep + XBraid 接口.
默认值: OFF

注意
请参阅第 14.1.4 节中关于启用 XBraid 的其他信息.

`EXAMPLES_ENABLE_C`
构建 SUNDIALS C 示例
默认值 `ON`

`EXAMPLES_ENABLE_CXX`
构建 SUNDIALS C++ 示例
默认值: `OFF`

`EXAMPLES_ENABLE_CUDA`
构建 SUNDIALS CUDA 示例
默认值: `OFF`

注意
您需要启用 CUDA 支持才能构建这些示例.

`EXAMPLES_ENABLE_F2003`
构建 SUNDIALS Fortran2003 示例
默认值:  `ON`(如果 `BUILD_FORTRAN_MODULE_INTERFACE` 为 `ON`)

`EXAMPLES_INSTALL`
安装示例文件
默认为: `ON`

注意
当 SUNDIALS 的任何示例程序被启用 (`EXAMPLES_ENABLE_<language>` 为 `ON`) 时, 此选项就会被触发.
如果用户要求安装示例程序, 那么当前启用的所有 `SUNDIALS` 模块的源代码和示例输出文件,
都将导出到 `EXAMPLES_INSTALL_PATH` 指定的目录.
CMake 配置脚本也会自动生成, 并导出到同一目录.
此外, 如果配置是在类 Unix 系统下完成的,
用于编译示例程序的 `makefile`(使用安装的SUNDIALS库)
也会自动生成并导出到 `EXAMPLES_INSTALL_PATH`.

`EXAMPLES_INSTALL_PATH`
安装示例文件的输出目录
默认值: `/usr/local/examples`

注意
此选项的实际默认值是在 `CMAKE_INSTALL_PREFIX` 下创建的 `examples` 子目录.

`BUILD_FORTRAN_MODULE_INTERFACE`
启用 Fortran 2003 接口
默认值: `OFF`

`ENABLE_GINKGO`
启用 Ginkgo 线性代数库接口.
默认值 `OFF`

`Ginkgo_DIR`
Ginkgo 安装路径.
默认值: None

`SUNDIALS_GINKGO_BACKENDS`
以半冒号分隔的 Ginkgo target architecutres/executors 列表.
目前支持的选项有 `REF`(Ginkgo 参考执行器), `OMP`, `CUDA`, `HIP` 和 `DPC++`.
默认值: `"REF;OMP"`

`ENABLE_KOKKOS`
启用基于 `Kokkos` 的向量.
默认值: `OFF`

`Kokkos_DIR`
Kokkos 安装路径.
默认值: None

`ENABLE_KOKKOS_KERNELS`
启用基于 Kokkos 的密集矩阵和线性求解器.
默认值: `OFF`

`KokkosKernels_DIR`
Kokkos-Kernels 的安装路径.
默认值: None

`ENABLE_HIP`
启用 HIP 支持
默认值:  `OFF`

`AMDGPU_TARGETS`
Specify which AMDGPU processor(s) to target.
默认值:  None

`ENABLE_HYPRE`
启用 `hypre` 支持的标志
默认值 `OFF`

注释
请参阅第 14.1.4 节中关于启用 hypre 的其他信息.

`HYPRE_INCLUDE_DIR`
hypre 头文件的路径
默认: none

`HYPRE_LIBRARY`
指向已安装 `hypre` 的库文件的路径
默认值: none

`ENABLE_KLU`
启用 `KLU` 支持
默认值 `OFF`

备注
请参阅第 14.1.4 节中有关启用 KLU 后构建的其他信息.

`KLU_INCLUDE_DIR`
指向 `SuiteSparse` 头文件的路径
默认: none

`KLU_LIBRARY_DIR`
SuiteSparse 安装库文件的路径
默认值: none

`ENABLE_LAPACK`
启用 `LAPACK` 支持
默认值: none

注意
将此选项设置为 `ON` 会触发额外的 `CMake` 选项.
请参阅第 14.1.4 节中关于启用 `LAPACK` 的其他信息.

`lapack_libraries`
LAPACK (和 BLAS) 库
默认: `/usr/lib/liblapack.so;/usr/lib/libblas.so`

注意
CMake 在搜索默认系统路径之前, 会先搜索 `LD_LIBRARY_PATH` 中的库.

`ENABLE_MAGMA`
启用 MAGMA 支持.
默认值:  `OFF`

注意
将此选项设置为 ON 会触发与 MAGMA 相关的其他选项.

`MAGMA_DIR`
MAGMA 安装根目录的路径.
默认值: none

`SUNDIALS_MAGMA_BACKENDS`
在 SUNDIALS MAGMA 界面下使用哪个 MAGMA 后端.
默认值: `CUDA`

`ENABLE_MPI`
启用 MPI 支持.
这将构建并行 `nvector` 和 `ManyVector` 库的 MPI-aware 版本.
默认值 `OFF`

注意
将此选项设置为 `ON` 将触发多个与 `MPI` 相关的附加选项.

`MPI_C_COMPILER`
`mpicc` 程序
默认值:

`mpi_cxx_compiler`
`mpicxx` 程序
默认:

注意
只有在启用 `MPI` (ENABLE_MPI 为 ON),
和启用 C++ 示例 (`EXAMPLES_ENABLE_CXX` 为 ON) 时才会触发此选项.
默认情况下, 所有 SUNDIALS solvers 都可在 C++ MPI 应用程序中使用,
无需设置除 `ENABLE_MPI` 以外的其他配置选项.

`MPI_Fortran_COMPILER`
`mpif90` 程序
默认值:

注意
只有启用 MPI(ENABLE_MPI 为 ON)
和 Fortran-C 支持(EXAMPLES_ENABLE_F2003 为 ON)时, 才会触发该选项.

`MPIEXEC_EXECUTABLE`
指定运行 MPI 程序的可执行文件
默认值: `mpirun`

注意
只有启用 MPI(`ENABLE_MPI` 为 `ON`)时, 才会触发该选项.

`ENABLE_ONEMKL`
启用 `oneMKL` 支持.
默认值: OFF

`ONEMKL_DIR`
oneMKL 安装路径.
默认值: none

`SUNDIALS_ONEMKL_USE_GETRF_LOOP`;
This advanced debugging option replaces the batched LU factorization
with a loop over each system in the batch
and a non-batched LU factorization.
默认值:  OFF

`SUNDIALS_ONEMKL_USE_GETRS_LOOP`
This advanced debugging option replaces the batched LU solve
with a loop over each system in the batch
and a non-batched solve.
默认值 关闭

`ENABLE_OPENMP`
启用 OpenMP 支持(构建 OpenMP NVector)
默认值: OFF

`ENABLE_PETSC`
启用 PETSc 支持
默认值 OFF

注释
请参阅第 14.1.4 节中有关启用 PETSc 构建的其他信息.

`PETSC_DIR`
PETSc 安装路径
默认: 无

`PETSC_LIBRARIES`
Semi-colon分隔的 `PETSc` 链接库列表.
除非用户提供, 否则将根据 `PETSC_DIR` 中的 `PETSc` 安装自动填充.
默认值: 无

`PETSC_INCLUDES`
Semi-colon 分隔的 `PETSc` 包含目录列表.
除非用户提供, 否则将根据 `PETSC_DIR` 中的 `PETSc` 安装文件自动填充.
默认值: 无

`ENABLE_PTHREAD`
启用 Pthreads 支持(构建 Pthreads NVector)
默认值:  关闭

`ENABLE_RAJA`
启用 RAJA 支持.
默认值 关闭

注意
您需要启用 CUDA 或 HIP 才能构建 RAJA 向量模块.

`SUNDIALS_RAJA_BACKENDS`
如果在构建 SUNDIALS 时支持 RAJA, 则设置 RAJA 后端为目标.
支持的值有 CUDA, HIP 或 SYCL.
默认值:  CUDA

`ENABLE_SUPERLUDIST`
启用 `SuperLU_DIST` 支持
默认值: OFF

注释
请参阅第 14.1.4 节中有关启用 SuperLU_DIST 后构建的其他信息.

`SUPERLUDIST_DIR`
SuperLU_DIST 的安装路径.
默认值: 无

`SUPERLUDIST_OpenMP`
为使用 OpenMP 构建的 `SuperLU_DIST` 启用 SUNDIALS 支持
默认值: 无

注意: `SuperLU_DIST` 必须支持 `OpenMP` 才能运行此选项.
此外, 环境变量 `OMP_NUM_THREADS` 必须设置为所需的线程数.

`SUPERLUDIST_INCLUDE_DIRS`
`SuperLU_DIST` 的包含路径列表
(在典型的 SuperLU_DIST 安装中, 这通常是 `SuperLU_DIST SRC` 目录)
默认值: 无

注释
这是一个高级选项.
建议使用 `SUPERLUDIST_DIR`.

`SUPERLUDIST_LIBRARIES`
SuperLU_DIST 所需的库的 分号分隔列表
默认值: 无

注释
这是一个高级选项.
建议使用 `SUPERLUDIST_DIR`.

`SUPERLUDIST_INCLUDE_DIR`
SuperLU_DIST 头文件的路径
(在典型的 SuperLU_DIST 安装中, 通常是 SuperLU_DIST SRC 目录)
默认值: 无

注释
这是一个高级选项.
该选项已被弃用.
请使用 `SUPERLUDIST_INCLUDE_DIRS`.

`SUPERLUDIST_LIBRARY_DIR`
SuperLU_DIST 安装的库文件的路径
默认值: 无

注释
此选项已被弃用.
请使用 `SUPERLUDIST_DIR`.

`ENABLE_SUPERLUMT`
启用 `SuperLU_MT` 支持
默认值:  关闭
注释
请参阅第 14.1.4 节中有关启用 SuperLU_MT 后构建的其他信息.

`SUPERLUMT_INCLUDE_DIR`
SuperLU_MT 头文件的路径(在典型的 SuperLU_MT 安装中, 这通常是 SuperLU_MT SRC 目录)
默认值: 无

`SUPERLUMT_LIBRARY_DIR`
`SuperLU_MT` 已安装库文件的路径
默认值: 无

`SUPERLUMT_THREAD_TYPE`
必须设置为 Pthread 或 OpenMP, 具体取决于 SuperLU_MT 的编译方式.
默认值:  Pthread

`ENABLE_SYCL`
启用 SYCL 支持.
默认值: OFF
注意
CMake 目前不支持 SYCL 编译器的自动检测,
`CMAKE_CXX_COMPILER` 必须设置为有效的 SYCL 编译器.
目前唯一支持的 SYCL 编译器是英特尔 oneAPI 编译器, 即 `dpcpp` 和 `icpx`.
使用 `icpx` 时,
必须在 `CMAKE_CXX_FLAGS` 中添加 `-fsycl` 标志和任何 ahead of time 编译标志.

`SUNDIALS_SYCL_2020_UNSUPPORTED`
这个高级选项禁止在 SUNDIALS 库和示例中使用 SYCL 2020 标准的某些功能.
这可用于解决某些编译器对 SYCL 2020 支持不完整的情况.
默认值:  OFF

`SUNDIALS_LOGGING_LEVEL`
设置 SUNLogger runtime API 的最大日志记录级别.
设置得越高, 可能记录的输出越多, 性能也可能下降得越多.

选项如下
0 - 不记录日志
1 - 记录错误
2 - 记录错误 + 警告
3 - 记录错误 + 警告 + 信息输出
4 - 记录错误 + 警告 + 信息输出 + 调试输出
5 - 记录以上所有内容, 甚至更多内容(例如, 可记录矢量值变量)
默认值: `0`

`SUNDIALS_LOGGING_ENABLE_MPI`
在 `SUNLogger` 运行时 API 中启用 `MPI` 支持.
也就是说, 使日志记录器具有 MPI 感知, 并能仅在特定 ranks 上输出.
默认值:  OFF
注意
日志记录器可在未开启 `MPI` 支持的 `MPI` 应用程序中使用, 但会在所有行列上输出.

`SUNDIALS_BUILD_WITH_MONITORING`
构建具有细粒度解算器进度和统计监控功能的 SUNDIALS.
这主要用于调试.
默认值为 OFF

警告
即使不使用监控功能, 使用监控功能构建 `SUNDIALS` 也可能会导致轻微的性能下降.

`SUNDIALS_BUILD_WITH_PROFILING`
构建具有 fine-grained profiling 的 SUNDIALS.
默认值:  关闭

警告
剖析会影响性能, 应谨慎启用.

`ENABLE_CALIPER`
启用 CALIPER 支持
默认值:  关闭

注意
使用 Caliper 需要将 `SUNDIALS_BUILD_WITH_PROFILING` 设置为 ON.

`CALIPER_DIR`
Caliper 安装根目录的路径
默认值:  无

`ENABLE_ADIAK`
启用 Adiak 支持
默认值 关闭

`adiak_DIR`
指向 Adiak 安装根目录的路径
默认值:  无

`SUNDIALS_F77_FUNC_CASE`
指定在 Fortran name-mangling 方案中使用的大小写, 选项为: `lower`或 `upper`
默认值: 无

注意
build系统将尝试使用 `Fortran` 编译器来推断 `Fortran` 名称混淆方案.
只有在没有 `Fortran` 编译器的情况下,
或者在无法确定推断或默认(下层)方案的情况下, 才能使用该选项.
如果使用, 还必须设置 `SUNDIALS_F77_FUNC_UNDERSCORES`.

`SUNDIALS_F77_FUNC_UNDERSCORES`
指定在 Fortran 名称混淆方案中附加的下划线个数,
选项包括: `none`, `one` or `two`
默认值:

备注
编译系统将尝试使用 Fortran 编译器推断 Fortran 名称混淆方案.
只有在没有 Fortran 编译器的情况下, 或者在无法确定推断或默认(一种)方案的情况下, 才能使用该选项.
如果使用, 还必须设置 `SUNDIALS_F77_FUNC_CASE`.

`SUNDIALS_INDEX_TYPE`
用于 SUNDIALS 索引的 Integer type.
大小必须与 `SUNDIALS_INDEX_TYPE` 选项提供的大小一致.
默认值:  根据 `SUNDIALS_INDEX_SIZE` 自动确定

注意事项
在过去的 SUNDIALS 版本中, 用户可以将此选项设置为 `INT64_T` 来使用 64 位整数,
或者设置为 `INT32_T` 来使用 `32` 位整数.
从 SUNDIALS 3.2.0 开始, 这些特殊值将被弃用.
对于 SUNDIALS 3.2.0 及以上版本, 用户在大多数情况下只需使用 `SUNDIALS_INDEX_SIZE` 选项.

`SUNDIALS_INDEX_SIZE`
SUNDIALS 中用于索引的整数大小(以比特为单位), 可选项有:  `32` 或 `64`
默认值: 64

注意事项
build系统会尝试找到合适大小的整数类型.
候选的 `64` 位整数类型有(按优先级排序): `int64_t`, `__int64`, `long long` 和 `long`.
候选的 32 位整数类型(按优先级排序)有: `int32_t`, `int` 和 `long`.
高级选项 `SUNDIALS_INDEX_SIZE` 可用于提供此处未列出的类型.

`SUNDIALS_MATH_LIBRARY`
要链接的标准 C 数学库(如 `libm`).
默认值: Unix 系统为 `-lm`, 其他系统为 none

`SUNDIALS_PRECISION`
SUNDIALS 包和类实现中使用的浮点精度, 选项包括: `double`, `single` 或 `extended`.
默认: `double`

`SUNDIALS_INSTALL_CMAKEDIR`
SUNDIALS cmake 文件的安装目录 (相对于 `CMAKE_INSTALL_PREFIX`).
默认值: `CMAKE_INSTALL_PREFIX/cmake/sundials`

`USE_GENERIC_MATH`
链接到 `SUNDIALS_MATH_LIBRARY`,  在 Unix 系统中默认为 `libm`.
默认值:  `ON`

注意
该选项已被弃用.
请使用 `SUNDIALS_MATH_LIBRARY`.

`XBRAID_DIR`
`XBraid` 安装的根目录.
默认值:  关闭

`XBRAID_INCLUDES`
以分号分隔的 XBraid 包含目录列表.
除非用户提供, 否则将根据 `XBRAID_DIR` 中的 `XBraid` 安装目录自动填充.
默认值: 无

`XBRAID_LIBRARIES`
以分号隔开的 XBraid 链接库列表.
除非用户提供, 否则将根据 XBRAID_DIR 中的 XBraid 安装文件自动填充.
默认值: 无

`USE_XSDK_DEFAULTS`
启用 xSDK(更多信息请参阅 https://xsdk.info)默认配置设置.
这将把 `CMAKE_BUILD_TYPE` 设置为 `Debug`,
把 `SUNDIALS_INDEX_SIZE` 设置为 `32`,
把 `SUNDIALS_PRECISION` 设置为 `double`.
默认值为 OFF

### 配置示例

以下示例有助于演示 CMake 配置选项的用法.

要使用默认的 C 和 Fortran 编译器, 默认的 mpicc 和 mpif90 并行编译器配置 SUNDIALS,
启用示例编译, 并在 /home/myname/sundials/ 的子目录下安装库, 头文件和示例源, 请使用

```bash
% cmake \
> -DCMAKE_INSTALL_PREFIX=/home/myname/sundials/instdir \
> -DEXAMPLES_INSTALL_PATH=/home/myname/sundials/instdir/examples \
> -DEXAMPLES_INSTALL_PATH=/home/myname/sundial/instdir/examples
/home/myname/sundials/srcdir

% make install
```

要禁用示例安装, 请使用

```bash
% cmake \
> -DCMAKE_INSTALL_PREFIX=/home/myname/sundials/instdir \
> -DEXAMPLES_INSTALL_PATH=/home/myname/sundials/instdir/examples \
> -DEXAMPLES_INSTALL_PATH=/home/myname/sundial/instdir/examples
> -dexamples_install=off \
> /home/myname/sundials/srcdir

% make install
```

### 14.1.4 使用外部库

SUNDIALS 套件包含许多选项, 以便在开发解决方案时实现灵活性.
以下是使用受支持的第三方库时, 针对特定配置的一些说明.

#### 14.1.4.1. 使用 Ginkgo 构建

[Ginkgo](https://ginkgo-project.github.io/)
是面向多核系统的高性能线性代数库, 侧重于稀疏线性系统的求解.
它使用现代 C++ 实现(至少需要 C++14 兼容编译器才能构建),
GPU 内核使用 CUDA(针对英伟达设备), HIP(针对 AMD 设备)和
SYCL/DPC++(针对英特尔设备和其他支持的硬件)实现.

要在 SUNDIALS 中启用 Ginkgo, 请将 `ENABLE_GINKGO` 设为 ON,
并在 `Ginkgoo_DIR` 中提供 Ginkgo 安装根目录的路径.
此外, `SUNDIALS_GINKGO_BACKENDS` 必须设置为 Ginkgo 目标编译器/执行器的列表.
例如

```bash
% cmake \
> -DENABLE_GINKGO=ON \
> -DGinkgo_DIR=/path/to/ginkgo/installation \
> -DSUNDIALS_GINKGO_BACKENDS="REF;OMP;CUDA" \
> /home/myname/sundials/srcdir
```

Ginkgo 的 `SUNDIALS` 接口与设置为 `extended` 的 `SUNDIALS_PRECISION` 不兼容.

#### 使用 Kokkos 构建

[Kokkos](https://kokkos.github.io/kokkos-core-wiki/) 是一种现代 C++(至少需要 C++14)编程模型,
用于为基于多核 CPU 和 GPU(包括英伟达, AMD 和英特尔加速器)的系统编写性能可移植代码.

要在 SUNDIALS 中启用 `Kokkos`, 请将 `ENABLE_KOKKOS` 设为 ON,
并在 `Kokkos_DIR` 中提供 Kokkos 安装根目录的路径.
此外, `Kokkos-Kernels` 库还为线性代数提供了常用的计算内核.
要在 SUNDIALS 中启用 `Kokkos-Kernels`, 请将 `ENABLE_KOKKOS_KERNELS` 设为 `ON`,
并在 `KokkosKernels_DIR` 中提供 `Kokkos-Kernels` 安装根目录的路径, 例如

```bash
% cmake \
> -DENABLE_KOKKOS=ON \
> -DKokkos_DIR=/path/to/kokkos/installation \
> -DENABLE_KOKKOS_KERNELS=ON \
> -DKokkosKernels_DIR=/path/to/kokkoskernels/installation \
> /home/myname/sundials/srcdir
```

>注意事项
>Kokkos-Kernels 的最低支持版本为 3.7.00.

#### 14.1.4.3 使用 LAPACK 构建

要启用 `LAPACK`, 请将 `ENABLE_LAPACK` 选项设置为 `ON`.
如果包含 `LAPACK` 库的目录位于 `LD_LIBRARY_PATH` 环境变量中,
`CMake` 将相应地设置 `LAPACK_LIBRARIES` 变量,
否则 CMake 将尝试在标准系统位置中查找 LAPACK 库.
要明确告诉 CMake 使用什么库, 可以将 `LAPACK_LIBRARIES` 变量设置为 `LAPACK` 所需的库.

```bash
% cmake \
> -DCMAKE_INSTALL_PREFIX=/home/myname/sundials/instdir \
> -DEXAMPLES_INSTALL_PATH=/home/myname/sundials/instdir/examples \
> -DENABLE_LAPACK=ON \
> -DLAPACK_LIBRARIES=/mylapackpath/lib/libblas.so;/mylapackpath/lib/liblapack.so \
> /home/myname/sundials/srcdir

% make install
```

>注意事项
>如果没有可用的 Fortran 编译器来推断 Fortran 名称混淆方案,
则必须设置 `SUNDIALS_F77_FUNC_CASE` 和 `SUNDIALS_F77_FUNC_UNDERSCORES` 选项,
以绕过对 Fortran 编译器的检查, 并定义名称混淆方案.
在 `SUNDIALS` 早期版本中, 这些选项的默认值分别为 `lower` 和 `one`.

`SUNDIALS` 已通过 OpenBLAS 0.3.18 测试.

#### 14.1.4.4. 使用 KLU 构建

`KLU` 是一个 直接求解 电路仿真中出现的 稀疏非对称线性方程组 的软件包,
是稀疏矩阵软件套件 `SuiteSparse` 的一部分.
该库由 Texas A&M 大学开发,
可从 [SuiteSparse GitHub](https://github.com/DrTimothyAldenDavis/SuiteSparse) 存储库中获取.

要启用 KLU, 请将 `ENABLE_KLU` 设为 ON,
将 `KLU_INCLUDE_DIR` 设为 `KLU` 安装的 `include` 路径,
将 `KLU_LIBRARY_DIR` 设为 `KLU` 安装的 `lib` 路径.

CMake configure 将填充以下变量:
`AMD_LIBRARY`, `AMD_LIBRARY_DIR`, `BTF_LIBRARY`, `BTF_LIBRARY_DIR`,
`COLAMD_LIBRARY`, `COLAMD_LIBRARY_DIR` 和 `KLU_LIBRARY`.

`SUNDIALS` 已在 SuiteSparse 5.10.1 版本中进行了测试.

#### 14.1.4.5 使用 SuperLU_DIST 进行构建

`SuperLU_DIST` 是一个通用库, 用于在分布式内存环境下直接求 解大型, 稀疏, 非对称线性方程组.
该库由 Lawrence Berkeley 国家实验室开发,
可从 [SuperLU_DIST GitHub](https://github.com/xiaoyeli/superlu_dist) 存储库中获取.

要启用 `SuperLU_DIST`, 请将 `ENABLE_SUPERLUDIST` 设为 `ON`,
将 `SUPERLUDIST_DIR` 设为安装 `SuperLU_DIST` 的路径.
如果 `SuperLU_DIST` 是使用 `OpenMP` 构建的,
则 `SUPERLUDIST_OpenMP` 和 `ENABLE_OPENMP` 选项应设置为 ON.

`SUNDIALS` 支持 SuperLU_DIST v7.0.0 - v8.x.x, 并已通过 v7.2.0 和 v8.1.0 测试.

#### 14.1.4.6. 使用 SuperLU_MT 构建

SuperLU_MT 是一个通用库, 用于在 共享内存 并行计算机上直接求解大型, 稀疏, 非对称线性方程组.
该库由劳伦斯伯克利国家实验室开发,
可从 [SuperLU_MT GitHub](https://github.com/xiaoyeli/superlu_mt) 存储库中获取.

要启用 SuperLU_MT, 请将 `ENABLE_SUPERLUMT` 设为 ON,
将 `SUPERLUMT_INCLUDE_DIR` 设为 SuperLU_MT 安装的 SRC 路径,
并将 `SUPERLUMT_LIBRARY_DIR` 变量设为 SuperLU_MT 安装的 lib 路径.

同时, 变量 `SUPERLUMT_LIBRARIES` 必须设置为以分号分隔的 `SuperLU_MT` 所依赖的其他库的列表.
例如, 如果在构建 `SuperLU_MT` 时使用了外部 `blas` 库, 则应在该列表中包含 blas 库的完整路径.
此外, 变量 `SUPERLUMT_THREAD_TYPE` 必须设置为 `Pthread` 或 `OpenMP`.

在构建 `SUNDIALS` 求解器时, 请勿 混合使用 线程类型.
如果 `SUNDIALS` 通过将 `ENABLE_OPENMP` 或 `ENABLE_PTHREAD` 设置为 ON 启用了线程,
则 `SuperLU_MT` 应设置为使用相同的线程类型.

SUNDIALS 已在 3.1 版的 SuperLU_MT 中进行了测试.

#### 14.1.4.7. 使用 PETSc 构建

The Portable, Extensible Toolkit for Scientific Computation(PETSc)
是一套数据结构和例程, 用于模拟偏微分方程建模的应用.
该库由阿贡国家实验室开发,
可从 [PETSc GitLab](https://gitlab.com/petsc/petsc) 存储库中获取.

要启用 `PETSc`, 请将 `ENABLE_PETSC` 设为 `ON`, 并将 `PETSC_DIR` 设为 `PETSc` 的安装路径.
或者, 用户可以在 `PETSC_INCLUDES` 中提供包含路径列表,
在 `PETSC_LIBRARIES` 中提供 `PETSc` 库的完整路径列表.

SUNDIALS 定期使用最新的 PETSc 版本进行测试,
特别是在 SUNDIALS v6.6.2 版中使用 3.18.1 版本.
SUNDIALS 需要 PETSc 3.5.0 或更新版本.

#### 14.1.4.8 使用 hypre 构建

`hypre` 是一个高性能 预条件子 和 solvers 库,
采用 multigrid 方法, 用于在 大规模并行计算机 上求解 大型稀疏线性方程组.
该库由 Lawrence Livermore 国家实验室开发,
可从 [hypre GitHub](https://github.com/hypre-space/hypre) 存储库中获取.

要启用 `hypre`, 请将 `ENABLE_HYPRE` 设为 `ON`,
将 `HYPRE_INCLUDE_DIR` 设为 `hypre` 安装的 `include` 路径,
并将变量 `HYPRE_LIBRARY_DIR` 设为 `hypre` 安装的 `lib` 路径.

注意
必须配置 `SUNDIALS`, 使 `SUNDIALS_INDEX_SIZE` 与 `hypre` 安装中的 `HYPRE_BigInt` 兼容.
SUNDIALS 会定期使用最新版本的 hypre 进行测试,
特别是在 SUNDIALS v6.6.2 版之前的 2.26.0 版本.

#### 14.1.4.9 使用 MAGMA 构建

The Matrix Algebra on GPU and Multicore Architectures (MAGMA)
项目提供了一个类似于 LAPACK 的 dense 线性代数库,
但以 异构(heterogeneous)架构为目标.
该库由田纳西大学开发,
可从 [田纳西大学](https://icl.utk.edu/magma/index.html) 的网页上获取.

要启用 SUNDIALS 的 MAGMA 接口, 请将 `ENABLE_MAGMA` 设为 ON,
`MAGMA_DIR` 设为 `MAGMA` 安装路径,
`SUNDIALS_MAGMA_BACKENDS` 设为与 SUNDIALS 配合使用的所需 MAGMA 后端,
如 `CUDA` 或 `HIP`.

SUNDIALS 已在 v2.6.1 和 v2.6.2 版的 MAGMA 上进行了测试.

#### 14.1.4.10 使用 oneMKL

英特尔 [oneAPI 数学内核库](https://software.intel.com/content/www/us/en/develop/tools/oneapi/components/onemkl.html) (oneMKL)
包括 LAPACK dense线性代数例程 的 CPU 和 DPC++ 接口.
SUNDIALS oneMKL 接口以 DPC++ 例程为目标,
要使用 CPU 例程, 请参阅第 [14.1.4.3 节](https://sundials.readthedocs.io/en/latest/Install_link.html#installation-cmake-externallibraries-lapack).

要启用 SUNDIALS oneMKL 接口, 将 `ENABLE_ONEMKL` 设为 `ON`,
并将 `ONEMKL_DIR` 设为 oneMKL 的安装路径.

SUNDIALS 已通过 oneMKL 2021.4 版本的测试.

#### 14.1.4.11. 使用 CUDA 构建

The NVIDIA CUDA 工具包为使用 NVIDIA GPU 进行 GPU 加速计算提供了一个开发环境.
CUDA 工具包和兼容的英伟达驱动程序可从 [英伟达开发者网站获取](https://developer.nvidia.com/cuda-downloads).

要启用 `CUDA`, 请将 `ENABLE_CUDA` 设置为 `ON`.
如果 CUDA 安装在非标准位置,
系统可能会提示您使用 `CUDA` 工具包安装路径设置变量 `CUDA_TOOLKIT_ROOT_DIR`.
要启用 CUDA 示例, 请将 `EXAMPLES_ENABLE_CUDA` 设置为 `ON`.

SUNDIALS 已通过 CUDA 工具包 10 和 11 版本的测试.

#### 14.1.4.12. 使用 HIP 构建

`HIP`(heterogeneous-compute interface for portability)
允许开发人员为 `AMD` 和 `NVIDIA GPU` 创建可移植的应用程序.
HIP 可从 [HIP GitHub 代码库](https://github.com/ROCm-Developer-Tools/HIP)中获取.

要启用 `HIP`, 请将 `ENABLE_HIP` 设置为 `ON`,
并将 `AMDGPU_TARGETS` 设置为所需目标(例如 `gfx705`).
此外, 将 `CMAKE_C_COMPILER` 和 `CMAKE_CXX_COMPILER` 设置为指向 `hipcc` 的安装.

SUNDIALS 已在 5.0.0 至 5.4.3 之间的 HIP 版本上进行了测试.

#### 14.1.4.13 使用 RAJA 构建

RAJA 是 Lawrence Livermore 国家实验室开发的
performance portability layer,
可从 [RAJA GitHub 存储库](https://github.com/LLNL/RAJA)获取.

构建 SUNDIALS 的 RAJA 模块需要安装启用了 `CUDA`, `HIP` 或 `SYCL` 的 `RAJA`.
要启用 RAJA, 请将 `ENABLE_RAJA` 设为 ON,
将 `SUNDIALS_RAJA_BACKENDS` 设为所需的后端(CUDA, HIP 或 SYCL),
并根据所选后端将 ENABLE_CUDA, ENABLE_HIP 或 ENABLE_SYCL 设为 ON.
如果 RAJA 安装在非标准位置, 系统会提示将 `RAJA_DIR` 变量 RAJA CMake 配置文件的路径.
要启动 RAJA 示例的编译, 请将 `EXAMPLES_ENABLE_CXX` 设为 `ON`.

SUNDIALS 已在 RAJA 0.14.0 版进行了测试.

#### 14.1.4.14. 使用 XBraid

XBraid 是一个 实时并行库, 实现了 optimal-scaling multigrid reduction in time(MGRIT) 求解器.
该库由 劳伦斯利弗莫尔 国家实验室开发,
可从 [XBraid GitHub 存储库](https://github.com/XBraid/xbraid)中获取.

要启用 `XBraid` 支持, 请将 `ENABLE_XBRAID` 设为 ON,
将 `XBRAID_DIR` 设为 `XBraid` 的根安装位置, 或 XBraid 仓库克隆的位置.

注意事项

目前, `XBraid` 的 `braid_Int` 和 `braid_Real` 类型分别被硬编码为 `int` 和 `double`.
因此, 在配置 SUNDIALS 时,
必须将 `SUNDIALS_INDEX_SIZE` 设置为 `32`, 将 `SUNDIALS_PRECISION` 设置为 `double`.
此外, 在配置 SUNDIALS 时, 必须将 `ENABLE_MPI` 设置为 ON.

SUNDIALS 已通过 XBraid 3.0.0 版本的测试.

#### 14.1.5 测试构建和安装

如果 SUNDIALS 在配置时将 `EXAMPLES_ENABLE_<language>` 选项设置为 `ON`,
则在使用 `make` 命令build之后,  可以运行一组回归测试:

```bash
% make test
```

此外, 如果 `EXAMPLES_INSTALL` 也被设置为 `ON`,
那么在使用 `make install` 命令安装后,
可以运行 冒烟测试:

```bash
% make test_install
```

### 14.1.6 构建和运行 Examples

每个 SUNDIALS solver 都附带了一组示例, 演示基本用法.
要编译和安装示例, 至少要将 `EXAMPLES_ENABLE_<language>` 选项设为 `ON`,
并将 `EXAMPLES_INSTALL` 设为 `ON`.
用 `EXAMPLES_INSTALL_PATH` 变量指定示例的安装路径.
CMake 将生成 CMakeLists.txt 配置文件(如果在 Linux/Unix 系统上, 则生成 Makefile 文件),
这些文件将引用已安装的 `SUNDIALS` 头文件和库.

`CMakeLists.txt` 文件或传统的 Makefile 文件都可用于构建示例,
也可作为创建用户开发解决方案的模板.

要使用提供的 `Makefile`, 只需运行 make 即可编译并生成可执行文件.
要在已安装的示例目录中使用 CMake,
可运行 cmake(或 ccmake 或 cmake-gui, 以使用图形用户界面),
然后运行 make 来编译示例代码.
请注意, 如果使用 CMake, 它将用 CMake 生成的新 `Makefile` 覆盖传统的 Makefile.

运行示例的输出结果可与 SUNDIALS 发行版中的示例输出结果进行比较.

>注意
>由于机器架构, 编译器版本, 第三方库的使用等原因, 输出结果可能会有差异.

#### 14.1.7. 在 Windows 上配置, 编译和安装

CMake 也可用于在 Windows 上构建 SUNDIALS.
要在 Visual Studio 下构建 SUNDIALS, 应执行以下步骤:

+ 将下载的 tar 文件解压缩到一个目录中. 作为 `SOLVERDIR`
+ 创建单独的 `BUILDDIR`
+ 打开 Visual Studio 命令提示符并 cd 到 `BUILDDIR`
+ 运行 `cmake-gui ../SOLVERDIR`
    + 点击 "配置
    + 选中/取消选中要编译的求解器
    + 将 CMAKE_INSTALL_PREFIX 更改为 INSTDIR
    + 根据需要设置其他选项
    + 点击生成
+ 回到 VS 命令窗口:
+ 运行 msbuild ALL_BUILD.vcxproj
+ 运行 msbuild INSTALL.vcxproj
+ 生成的库将位于 INSTDIR 中.

现在也可以在 Visual Studio 中打开 SUNDIALS 项目.
双击 ALL_BUILD.vcxproj 文件打开项目.
构建整个解决方案, 创建 SUNDIALS 库.
要在您自己的项目中使用绪雅思 (SUNDIALS) 库, 您必须为您的项目设置 include 目录, 将绪雅思 (SUNDIALS) 库添加到您的项目解决方案中, 并将绪雅思 (SUNDIALS) 库设置为您项目的依赖项.

## 14.2. 安装库和导出头文件

使用 CMake SUNDIALS build系统, 命令

```bash
$ make install
```

将安装 LIBDIR 下的库和 INCLUDEDIR 下的公共头文件.
这些目录的值分别是 INSTDIR/lib 和 INSTDIR/include.
可以通过设置 CMake 变量 CMAKE_INSTALL_PREFIX 来更改位置.
虽然所有已安装的库都位于 LIBDIR/lib 下, 但公共头文件被进一步组织成 INCLUDEDIR/include 下的子目录.

下表列出了已安装的库和导出的头文件, 以供参考.
文件扩展名 .LIB 通常是共享库的 .so 和静态库的 .a.
请注意, 在本表中, 库的名称是相对于 LIBDIR 而言, 头文件的名称是相对于 INCLUDEDIR 而言.

典型的用户程序不需要明确包含 INCLUDEDIR/include/sundials 目录下的任何共享 SUNDIALS 头文件, 因为相应的求解器头文件已经明确包含了这些文件(例如, sunlinsol_dense.h 包含 sundials_dense.h).
不过, 这样做既合法又安全, 例如, 如果要使用 sundials_dense.h 中声明的函数来构建预处理, 这样做将非常有用.

14.2.1. 在其他 CMake 项目中将 SUNDIALS 作为第三方库使用
make install 命令也会安装一个 CMake 软件包配置文件, 其他 CMake 项目可以通过加载该文件来获取所有 i

14.2.1. 在其他 CMake 项目中将 SUNDIALS 作为第三方库使用
make install 命令也会安装一个 CMake 软件包配置文件, 其他 CMake 项目可以加载该文件, 以获取针对 SUNDIALS 构建所需的所有信息.
在使用项目的 CMake 代码中, 可以使用 find_package 命令来搜索配置文件,
该文件将与软件包版本文件 instdir/SUNDIALS_INSTALL_CMAKEDIR/SUNDIALSConfig.cmake 一起安装到 instdir/SUNDIALS_INSTALL_CMAKEDIR/SUNDIALSConfigVersion.cmake.
这些文件包含使用 SUNDIALS 的项目所需的所有信息, 包括导出的 CMake 目标.
SUNDIALS 导出的 CMake 目标与生成的库二进制文件遵循相同的命名约定, 例如 CVODE 的导出目标是 SUNDIALS::cvode.
下面的 CMake 代码片段展示了消费项目如何利用 SUNDIALS 软件包配置文件, 在自己的 CMake 项目中针对 SUNDIALS 进行构建.

```cmake
project(MyProject)

# 将 SUNDIALS_DIR 变量设为 SUNDIALS 的 instdir.

# 当使用 cmake CLI 命令时, 可以这样做:
# cmake -D SUNDIALS_DIR=/path/to/sundials/installation

find_package(SUNDIALS REQUIRED)

add_executable(myexec main.c)

# 通过导出目标链接到 SUNDIALS 库.

# 这只是一个示例, 用户应根据自己的使用情况链接到合适的目标.

# 用例.

target_link_libraries(myexec PUBLIC SUNDIALS::cvode SUNDIALS::nvecpetsc)
```

### sundial cmake 配置

### MSYS2 ucrt64, openBLAS

CMAKE_INSTALL_PREFIX: sundials 安装路径
PETSC_DIR: petsc 安装路径
LAPACK_LIBRARIES; blas 和 lapack 的路径.

```bash
cmake.exe \
-DCMAKE_BUILD_TYPE=Release \
-DBUILD_CVODES=OFF \
-DCMAKE_INSTALL_PREFIX=/c/cppLibs/sundials \
-DEXAMPLES_INSTALL_PATH=/c/cppLibs/sundials/examples \
-DEXAMPLES_ENABLE_C=ON \
-DEXAMPLES_ENABLE_CXX=ON \
-DENABLE_OPENMP=ON \
-DENABLE_MPI=ON \
-DEXAMPLES_ENABLE_F2003=ON \
-DBUILD_FORTRAN_MODULE_INTERFACE=ON \
-DMPIEXEC_EXECUTABLE=/home/yd/bin/mpiexec \
-DMPI_Fortran_COMPILER=/ucrt64/bin/mpif90 \
-DSUNDIALS_LOGGING_ENABLE_MPI=ON \
-DENABLE_PETSC=ON \
-DPETSC_DIR=/c/cppLibs/PETSc \
-DENABLE_LAPACK=ON \
-DLAPACK_LIBRARIES='c:/cppLibs/openBLASLAPACK/bin/libopenblas.dll;c:/cppLibs/openBLASLAPACK/bin/liblapack.dll' \
-G 'MSYS Makefiles' \
-B . -S .. --fresh
```

sundials nvector petsc 的CMAKE 脚本有问题,
使用这个 [修改过的版本](src-nvector-petsc-CMakeLists.txt).

### Linux, openBLAS

依情况而定, 可能需要指定 PETSc include 目录,
即`-DPETSC_INCLUDE_DIRS=/home/tom/myLibs/PETSc/include`

```bash
cmake \
-DCMAKE_BUILD_TYPE=Release \
-DCMAKE_INSTALL_PREFIX=/home/tom/myLibs/sundials \
-DEXAMPLES_INSTALL_PATH=/home/tom/myLibs/sundials/examples \
-DEXAMPLES_ENABLE_C=ON \
-DEXAMPLES_ENABLE_CXX=ON \
-DENABLE_OPENMP=ON \
-DENABLE_MPI=ON \
-DEXAMPLES_ENABLE_F2003=ON \
-DBUILD_FORTRAN_MODULE_INTERFACE=ON \
-DSUNDIALS_LOGGING_ENABLE_MPI=ON \
-DENABLE_PETSC=ON \
-DPETSC_DIR=/home/tom/myLibs/PETSc \
-DPETSC_INCLUDE_DIRS=/home/tom/myLibs/PETSc/include \
-DENABLE_LAPACK=ON \
-DLAPACK_LIBRARIES='/home/tom/myLibs/openBLASLAPACK/lib/libopenblas.so;/home/tom/myLibs/openBLASLAPACK/lib/liblapack.so' \
-G 'Unix Makefiles' \
-B . -S ..
```

## 测试

```bash
# 进入 算例目录
cd xxx/sundials/examples/ida/C_openmp
# 编译
gcc -o idaFoodWeb_kry_omp idaFoodWeb_kry_omp.c -lsundials_ida -lsundials_nvecopenmp -lm
# 执行
./idasFoodWeb_kry_omp
```

## windows cmake UI

[SUNDIALS 下载地址](https://computing.llnl.gov/projects/sundials/sundials-software)
[SUNDIALS 库的编译和使用](https://blog.csdn.net/qq_44246618/article/details/114240234)

在 `~/test` 下面建立三个目录

```bash
sundials-src
sundials-build
sundials-install
```

配置好 CMake 的目录
`Where is the source code` 和 `Where to build the binaries`,
然后反复点击 Configure, 直到红色消失(红色表示需要确认的选择),
期间可以根据需要配置 选项的值, 或者 开关的 `ON,OFF`

然后使用 VisualStudio 编译 `ALL_BUILD` 项目生成 dll,
编译 `INSTALL` 项目安装到指定的目录.

## 查看更改的选项

cmake 菜单栏 `Tools`->`Show My Changes`,
给出的输入如下, 主要是:

+ 设置了 `ENABLE_OPENMP`, 禁止了 `SUNDIALS_ENABLE_ERROR_CHECKS`
+ 修改了 install 时的路径
+ 选择构建 动态库 `BUILD_SHARED_LIBS`, 而不构建静态库 `BUILD_STATIC_LIBS`.

```bash
# Commandline options: 命令行指定的选项
-DENABLE_MPI:BOOL="0" -DEXAMPLES_ENABLE_CXX:BOOL="1" \
-DCMAKE_CXX_STANDARD:STRING="17" -DBUILD_KINSOL:BOOL="1" \
-DEXAMPLES_INSTALL_PATH:PATH="C:/Users/qingz/test/sundials/sundials-install/examples" \
-DCMAKE_INSTALL_PREFIX:PATH="C:/Users/qingz/test/sundials/sundials-install" \
-DENABLE_OPENMP:BOOL="1" -DBUILD_IDAS:BOOL="1" -DSUNDIALS_ENABLE_ERROR_CHECKS:BOOL="0" \
-DBUILD_SHARED_LIBS:BOOL="1" -DBUILD_CVODE:BOOL="1" \
-DBENCHMARKS_INSTALL_PATH:PATH="C:/Users/qingz/test/sundials/sundials-install/benchmark" \
-DBUILD_STATIC_LIBS:BOOL="0"

# Cache file: 缓存文件中的选项
ENABLE_MPI:BOOL=0
EXAMPLES_ENABLE_CXX:BOOL=1
CMAKE_CXX_STANDARD:STRING=17
BUILD_KINSOL:BOOL=1
EXAMPLES_INSTALL_PATH:PATH=C:/Users/qingz/test/sundials/sundials-install/examples
CMAKE_INSTALL_PREFIX:PATH=C:/Users/qingz/test/sundials/sundials-install
ENABLE_OPENMP:BOOL=1
BUILD_IDAS:BOOL=1
SUNDIALS_ENABLE_ERROR_CHECKS:BOOL=0
BUILD_SHARED_LIBS:BOOL=1
BUILD_CVODE:BOOL=1
BENCHMARKS_INSTALL_PATH:PATH=C:/Users/qingz/test/sundials/sundials-install/benchmark
BUILD_STATIC_LIBS:BOOL=0
```
