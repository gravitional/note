# 帮助文档目录

## 圈积分

`A0` ; Passarino-Veltman 1点积分(蝌蚪 tadpole).

`Apart2` ; partial fractions 圈积分(只有非常简单的情况)
`ApartFF` ; partial fractions 任意圈积分

`B0`, `B00`, `B1`, `B11` ; Passarino-Veltman 2点积分(真空图 bubbles)
`C0` ; Passarino-Veltman 3 点积分(三角图,triangles)
`D0`; Passarino-Veltman 4 点积分(方框, boxes).
`DB0`; 积分 `B0` 相对于外部动量的导数
`DB1`; 积分 `B1` 相对于外部动量的导数

`FCApart`; `ApartFF` 的后端, 只对 `单圈积分` 起作用
`FCGramMatrix` ; 从给定的动量生成 Gram 矩阵
`FCGramDeterminant` ; 计算 Gram 行列式
`FCHideEpsilon` ; 用 $\Delta$ 代替 $1/\varepsilon-\gamma_E+\ln(4\pi)$
`FCShowEpsilon` ; 用 $1/\varepsilon-\gamma_E+\ln(4\pi)$ 代替 $\Delta$.

`FCIntegral`; 圈积分的头部

`FCLoopBasisIncompleteQ` ; 检查圈积分的传播子是否没有形成一组基(basis)
`FCLoopBasisOverdeterminedQ` ; 检查圈积分的传播子是否是 `线性依赖` 的.
`FCLoopBasisSplit` ; 检查 `圈积分` 是否可以分解为独立积分的乘积

`FCLoopBasisGetSize` ; 返回 拓扑结构 中的 传播子 的数量
`FCLoopBasisPropagatorsToTopology` ; 辅助函数, 生成描述 拓扑结构 的传播子列表
`FCLoopBasisCreateScalarProducts` ; 辅助函数, 生成所有可能的与圈动量相关的 标量积.
`FCLoopBasisExtract`; 辅助函数, 从圈积分中提取 标量积
`FCLoopBasisIntegralToPropagators` ; 辅助函数, 将 `圈积分` 转换为 `传播子` 列表

`FCLoopCanonicalize` ; 辅助函数, 将 单圈积分 的自由洛伦兹指数规范化.
`FCLoopEikonalPropagatorFreeQ` ; 检查积分是否包含 `Eikonal` 传播子
`FCLoopExtract` ; 提取圈积分

`FCLoopIBPReducableQ` ; 检查积分是否包含产生 `整数次方` 的传播子.
`FCLoopIsolate` ; 将 圈积分 包裹在指定的 Head 中
`FCLoopMixedIntegralQ` ; 检查积分是否同时依赖于 `4-向量` 和 `3-向量`

`FCLoopMixedToCartesianAndTemporal` ; 通过用 `时间分量` 和 `3向量` 表示, 消除对 `4向量` 的依赖.

`FCLoopNonIntegerPropagatorPowersFreeQ` ; 检查积分是否有 产生 `非整数幂次` 的传播子.
`FCLoopPropagatorPowersCombine` ; 将相同的 `传播子` 合并为, 提升到相应 `整数幂` 的传播子.
`FCLoopPropagatorPowersExpand` ; 将提高到 `整数幂` 的传播子, 改写为 传播子 的乘积.
`FCLoopSamePropagatorHeadsQ` ; 检查积分是否包含不同 type 的 传播子.

`FCLoopRemoveNegativePropagatorPowers` ; 改写传播子, 将其提高到负整数幂, 作为分子.
`FCLoopSolutionList` ; 辅助函数, 处理 `FCLoopCanonicalize` 的输出.

`FCLoopSplit` ; 将表达式分割成多部分, 包含不同类型的圈积分
`FCMultiLoopTID` ; 多环积分的张量还原(仅适用于非零格拉姆行列式).

`FeynAmpDenominatorCombine`; 组合传播子的乘积
`FeynAmpDenominatorExplicit`; 用 `标量积` 和 `质量` 重写 `FeynAmpDenominator`.
`FeynAmpDenominatorSimplify`; 通过平移 (shift) 来简化圈积分, 并检测因对称性而消失的积分.

`FeynAmpDenominatorSplit`; 将所有 `FeynAmpDenominator` 分割成单个传播子的乘积.
`FeynmanParametrize`; 为一些 `单圈积分` 引入 `Feynman` 参数化.

`FromTFI` ; 将 `TFI`, `TVI` 和 `TJI` 的 `Tarcer` 符号翻译成 `FeynCalc` 符号.
`GammaExpand`; 改写 `Gamma[n+m]`, 其中 `n` 是一个整数.
`GenPaVe`, `PaVe` ; 表示 invariant Passarino-Veltman 积分
`Hill`; 给出 `Hill` 等式

`HypergeometricAC`; 解析延拓 `Hypergeometric2F1` 函数
`HypergeometricIR`; 对所有 `Hypergeometric2F1[a,b,c,d]` 代入特定的积分表示.
`HypergeometricSE`; 通过其级数展开来表达 超几何函数
`HypExplicit` ; 通过其定义的求和, 来表示超几何函数.

`HypInt` ;用 `Hypergeometric2F1` 函数的积分定义来代替它们.
`IntegrateByParts`, `PartialIntegrate` ; 对特定的 费曼参数积分 进行 分部积分
`NPointTo4Point` ; 用 box 积分重写 IR-finite 单圈积分

`OneLoopSimplify` ; 简化 单圈费曼图 的振幅

`ToHypergeometric` ; 引入 `Hypergeometric2F1`
`PaVeOrder`; 以标准方式排列 `D0` 的参数
`PaVeReduce`; 将 Passarino-Veltman 积分化简到 `A0`, `B0`, `C0` 和 `D0`
`PaVeUVPart` ; 返回任意 `Passarino-Veltman` 函数的 `UV` 发散部分.

`RussianTrick`; 推导出 `2圈自能积分` 的 `IBP` 关系.
`SimplifyDeltaFunction`; 对 `DeltaFunction` 进行一些化简.

`Sn` ; 表示 $\pi^{n/2}/(2\pi)^n$

`CTdec`, `Tdec` ; 计算 圈积分 的 张量分解公式
`TFIOrder`; 以标准方式为一些 `TFI` 函数的参数排序

`TID`; 做单圈的张量积分分解
`TIDL`; 张量积分分解公式库

`ToDistribution`; 引入 `DeltaFunction`, `DeltaFunctionPrime` 和 `PlusDistribution`

`ToFI`, `ToTFI`; 将 单圈 和 2圈 标量自能积分转换为 Tarcer 记号
`ToPaVe`; 将 `标量单圈积分` 转换为 Passarino-Veltman 标量函数
`ToPaVe2` ; 将  Passarino-Veltman functions 函数 `A0`, `A00`, `B0`, `B1`, `B00`, `B11`, `C0` 和 `D0` 改写为 `PaVe` 对象.

`ToSFAD` ; 将 `FAD` 转换为 `SFAD`.
`TrickIntegrate` ; 积分一些特殊分布
