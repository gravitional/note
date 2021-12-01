# FIESTA 5: numerical high-performance Feynman integral evaluation

[FIESTA](https://bitbucket.org/feynmanIntegrals/fiesta)

## SUMMARY

在本文中, 我们介绍了FIESTA程序的新版本(Feynman Integral Evaluation by a Sector DecomposiTion Approach).
`FIESTA5` 是以性能为导向的--我们实施了各种改进, 以使费曼积分运算更快.
我们插入了两个新的积分器(integrators), 准蒙特卡罗(Quasi Monte Carlo)和张量训练(Tensor Train).
同时, FIESTA4的旧代码被升级到 `C++17` 标准, 并且大部分以不依赖于自制结构的方式重写, 如`哈希表`.
此外, 还有一些基本的改进, 这些改进与复数积分最为相关--新版本能够产生以前不可能产生的结果.

+ 作者: A.V. Smirnov, N. D. Shapurov, L. I. Vysotsky
+ 程序名: FIESTA5
+ 许可证Licensing provisions: GPLv3
+ 设计该程序所适用的操作系统: Unix, Linux, Mac OS X, Windows (在WSL下)
+ 使用的外部程序/库: [Wolfram Mathematica][], [KyotoCabinet][], [Cuba][], [QMC][], [Tensor Train][], [mimalloc][]

+ 问题的性质: `Sector 分解` 是一种众所周知的, 计算费曼积分的数值方法.
费曼积分在 `4 维`时空是发散的, 必须被正规化.
Sector 分解可以用来解决极点奇异性(singularities), 包括不同的阶段-- sector 分解本身,
回路分解(contour decomposition, 在物理的运动学的情况下, 意味着基函数改变符号, 因此导致复数的积分),
pole resolution, $\epsilon$ 展开, 和数值积分.

+ 解决方法: 大多数阶段是在 `Wolfram Mathematica` 中进行的(要求的版本是8.0或更高).
这部分通过在共享内存使用 Mathematica subkelnels 实现并行化.
因此, 使用 [KyotoCabinet][] 数据库引擎, 在硬盘上产生了一个数据库.
积分阶段是用 `C++` 编写, 可以在个人电脑上运行, 也可以通过 `MPI` 在超级计算机上运行. 它可以利用已安装的图形处理器单元.
作为默认的集成器(integrator), 我们使用 [Cuba][] 库中的 `Vegas`,  但也可以切换到 [QMC][]或 [Tensor Tran][].
可以使用 `mimalloc` 内存分配器来提高性能.

+ 限制条件: 该问题的复杂性主要受限于 CPU 时间, 用于完成积分, 以及达到所需精度.
+ 运行时间: 取决于问题的复杂性.

+ 参考:

[Wolfram Mathematica]: http://www.wolfram.com/mathematica/, commercial algebraic software;
[KyotoCabinet]: http://fallabs.com/kyotocabinet/, open source;
[Cuba]: http://www.feynarts.de/cuba/, open source;
[QMC:] https://github.com/mppmu/qmc/, open source;
[Tensor Train]: https://bitbucket.org/vysotskylev/c-tt-library, open source;
[mimalloc]: https://github.com/microsoft/mimalloc.git, open source;

## 介绍

Sector 分解是一种早已确立的, Feynman积分数值化计算的方法.

虽然 Sector 分解 本身在上个世纪的数学家论文中就有记载,
而在粒子物理学中则是由 Binoth 和Hienrich 首次提出,
而 Sector 分解程序的第一个公开版本是由Bogner和Weinzierl发表的.

现在有两个著名的公开的竞争性程序实现了 Sector 分解.
`SecDec` 由Binoth和Heinrich 设计, 后来被其他合作者改进并开源.
并在某一时期改名为 `pySecDec`. 另一个是 `FIESTA`.

在本文中, 我们不打算重复详细解释什么是 扇区分解, 以避免文字上的借用
(没有原创性的方法来重复介绍 扇区分解),
而将重点放在如何使 扇区分解, 从而有效地利用大部分的CPU和GPU 资源.
新发布的FIESTA是对以前的FIESTA的一个重大升级, 新版本的主要特点是:

+ 在物理的运动学情况下, 改进回路分解(contour decomposition)算法.  使得FIESTA在以前不能运行的情况下也能工作, 而且速度更快.
+ 提供了新的 integrators,  Quasi Monte Carlo 算法 和 Tensor Train 算法[23].
+ 多种内部代码优化, 包括对 `AVX`(single instruction multiple data, 单指令多数据)处理器指令的全面支持, 从而使积分计算速度更快.
+ 多种旨在加快计算的选项. 例如, 可以在不同的 sectors 播种(seed)不同数量的采样点, 并取决于它们对最终结果的贡献(平衡).
+ 用代码分析器(code profilers)测量积分计算, 有些占用大量时间的代码部分得到了改进.

同时, 新版本也是对代码库(codebase)的一次重大更新

+ `Mathematica` 部分的代码现在是一个不影响 global context 的 `Mathematica` 包, 所有函数都有使用说明.
+ 带有自制`哈希表`, `tries` 和 `vectors`的传统代码被删除, 并被适当的标准结构所取代.
+ 代码标准从c99升级到C++17.
+ 增加了 configure 脚本, 使之能够在编译前设置用户偏好
+ 在代码中加入了 `doxygen` 文档和部分测试(任何新的改动都会自动进行测试, 以便在许多情况下不会破坏行为).

虽然上面列出的几点并没有给普通用户带来直接的好处, 但我们认为这是一个重要的进步,
因为现在有一个更大的研究团队可能会支持和开发这个代码, 而且我们有计划在未来进行推进和发布.

本文其余部分的组织方式如下: 在下一节中, 我们对 `扇区分解` 方法进行了简要描述,
主要侧重于该方法的阶段(stage), 因为对它们的理解对于为FIESTA优化设置适当的选项至关重要.
我们还介绍了与每个阶段相关的 `FIESTA5` 的新特性(features).

在第三部分, 我们解释了如何安装 `FIESTA` 以及如何使用它.
在第4节中, 我们列出了所有的 `FIESTA` 选项, 并说明了如何根据实际的例子正确设置这些选项.
在第5节中, 我们提供了比较使用不同选项和不同版本的FIESTA的测试(benchmarks).

`扇区分解`方法是一种复杂的方法, 由多个阶段组成, 一个接一个自动进行.
不同的`扇区分解`程序可能有一些不同的方法, 所以我们将按照FIESTA中使用的方式列出这些阶段, 并给出它们的特点(characteristics)和可能的问题.

`扇区分解`从关于 `n` 个变量的积分开始, 积分区域为 $0$到 $\infty$,
但这个积分实际上是在有限区域内进行的, 因为积分包含一个所有变量之和的 `delta`函数,
这个 delta 函数在参数和等于 `1` 时等于`1`, 反之等于 `0`.
除了 delta 函数外, 积分还包括积分变量的多项式(polynomials)的乘积,
其`指数`(exponent) 取决于一个特殊的变量 $\varepsilon$, 这里$d=4-2\varepsilon$ 是时空维数,
$\varepsilon$ 很小, 我们只对积分的 $\varepsilon$ 级数的第一项感兴趣.

对于费曼积分, 公式如下:

$$\mathcal{F}\left(a_1,\cdots, a_n\right)=(i\pi^{d/2})^\ell \times \\
 \frac{\Gamma(A-\ell d/2)}{\prod_{j=1}^{n}\Gamma(a_j)}
 \int_{x_j\ge0} \mathrm{d}x_i \cdots \mathrm{d} x_n
 \delta \left(1-\sum_{i=1}^n x_i\right) \left(1-\prod_{j=1}^n x_{j}^{a_j-1}\right)
 \frac{U^{A-(\ell+1)d/2}}{\left(F-i 0\right)^{A-\ell d/2}} $$

在这里,  $\ell$ 是费曼积分的圈数, $A$ 是指标(indices)的求和, $U$ 和 $F$ 是积分变量的某些多项式,是根据图的构造定义的.
我们还应该注意到, 这种方法对更一般的积分也是有效的.
我们将列出我们使用的所有阶段, 关于细节可以参考以前关于FIESTA的论文.

如果 $F$ 是负数, 积分就没有虚数部分. 特别是, 这发生在所有运动学不变量为负的区域.
对于运动学不变量和质量的物理值, $F$ 函数可以是正的, 从而使得某个给定的费曼积分具有非零的虚部.
