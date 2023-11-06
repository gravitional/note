# PolygonIntersect 多边形相交工具类

## Intersect2D

```cpp
common::PolygonIntersect::Intersect2D(const double *points1, int nPoints1, const double *points2, int nPoints2)
```

二位多边形求交.
参数
points1 多边形1的坐标
nPoints1 多边形1的点数量
points2 多边形2的坐标
nPoints2 多边形2的点数量
返回
相交多边形的坐标(点顺序与多边形2的顺序一致)

### 结果示例

```wolfram
(*first polygon*)
p1 = {-0.00143, -0.00318};
p2 = {-0.00143, 0.0};
p3 = {0.00209, 0.0};
(*second polygon*)
q1 = {0, 0};
q2 = {-0.00307, 0};
q3 = {-0.0016146, -0.002877};
(*intersect polygon*)
i1 = {0, 0};
i2 = {-0.00143, 0};
i3 = {-0.00143, -0.0025482};
(*plot*)
dlt = {-1, 1}*2*10^-4;
Graphics[{
  {Opacity[.4], Black, Line[{p1, p2, p3, p1}]},
  {Opacity[.4], Red, Line[{q1, q2, q3, q1}]},
  {FontSize -> 16, Text["q1", q1 + dlt], Text["q2", q2 + dlt], 
   Text["q3", q3 + dlt]},
  {Opacity[1], Blue, Line[{i1, i2, i3, i1}]},
  {FontSize -> 16, Text["i1", i1], Text["i2", i2], Text["i3", i3]}
  }]
```
