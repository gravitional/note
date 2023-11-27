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

