# mathematica

## 坐标变换

+ 一般性的教程在: tutorial/ChangingCoordinateSystems

+ 使用`CoordinateChartData`查看某种坐标的数据, 如查看球坐标的度规:

```mathematica
CoordinateChartData["Spherical", "Metric", {r, \[Theta], \[CurlyPhi]}]
```

+ 使用`CoordinateTransformData`查看坐标变换的数据, 例如新旧坐标间的映射矩阵, 以及新旧正交基之间的变换矩阵.

```mathematica
CoordinateTransformData[ "Spherical" -> "Cartesian", "Mapping", {r, \[Theta], \[CurlyPhi]}]
```

+ 使用`TransformedField`对函数进行坐标变换, 得到新的坐标系下的函数. 例如:

```mathematica
TransformedField["Polar" -> "Cartesian",  r^2 Cos[\[Theta]], {r, \[Theta]} -> {x, y}]
```

+ 使用`CoordinateTransform`对离散的点进行坐标变换, 例如:

```mathematica
CoordinateTransform[ "Polar" -> "Cartesian", {r, \[Theta]}]
CoordinateTransform[{"Cartesian" -> "Polar", 2}, {1, -1}]
```
