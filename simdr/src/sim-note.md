# Simdroid Solver Note

`electromagnetics>LowFreq>Electrics>Element` 下的目录的含义为:

Plane: 平面单元
Solid: 3D单元

+ Surface: 各种单元的边界/表面, 没有特定维数
`SurfaceTriangleEF` 表示三维体积表面的三角形单元,
类似地, `SurfaceQuadEF` 表示三维体积表面的四边形单元.
`EF` 后缀表示电场.

+ `ModelCreatorEF.cpp` 中给出了单元类型的枚举和字符串表示.
`SurfacePlaneBaseEF.cpp` 中给出了计算曲面面积的示例程序.
