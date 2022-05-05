# sarah

[SARAH](https://sarah.hepforge.org/)

`SARAH` 是一个用于建立和分析SUSY和非SUSY模型的Mathematica软件包.
它计算所有 `顶点`, 质量矩阵, tadpoles 方程, tadpole 和自能的单环修正, 以及给定模型的双环 RGEs.
`SARAH` 为

+ FeynArts, CalcHep/CompHep 编写模型文件, 这些模型可以输入 MicrOmegas 来进行暗物质研究
+ 可以编写 MadGraph 5 支持的 UFO格式,
+ 以及 WHIZARD 和 OMEGA .

`SARAH` 也是第一个可用的频谱二阶生成器(spectrum-generator-generator)
基于导出的解析表达式, 它为 `SPheno` 创建源代码.

因此, 有可能在 `SPheno` 中实现新的模型, 而不需要手工编写任何 `Fortran` 代码.
`Vevacious` 的输出可用于检查给定模型和参数点的 全局最小值.
运行SARAH的速度很快, 它已经包括了一长串的SUSY和非SUSY模型,
而且新模型的实现也是高效和直接的.
