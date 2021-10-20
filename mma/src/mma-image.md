# 图像处理

## 颜色数据,ColorData

mathematica 有一些默认的颜色集合, 可以通过 `ColorData` 调用, 可以用整数指定索引, 例如`ColorData[63]`.
在`LineLegend`这样的函数中, 如果在颜色参数的位置给一个整数, 会自动调用`ColorData[..]`.

ColorData["scheme"][par] 或 ColorData["scheme", par]
给出指定颜色方案中对应于参数值 par 的 RGBColor 对象.
ColorData["SiennaTones"]
ColorData["SiennaTones"][0.7]

### 图像平滑

guide/ImageRestoration

图像邻域处理
guide/ImageFilteringAndNeighborhoodProcessing

## SliceVectorPlot3D 空白解决办法

[https://mathematica.stackexchange.com/questions/240037/slicevectorplot3d-example-code-gives-empty-plot#](https://mathematica.stackexchange.com/questions/240037/slicevectorplot3d-example-code-gives-empty-plot#)

`mathematica`3D 渲染选项.

测试下面的方法:

```mathematica
Style[SliceVectorPlot3D[{y, -x, z},   "CenterPlanes", {x, -2, 2}, {y, -2, 2}, {z, -2, 2}],  RenderingOptions -> {"3DRenderingEngine" -> "Mesa"}]
```

```mathematica
Style[SliceVectorPlot3D[{y, -x, z},   "CenterPlanes", {x, -2, 2}, {y, -2, 2}, {z, -2, 2}],  RenderingOptions -> {"3DRenderingMethod" -> "BSPTree"}]
```

如果其中一个可以的话, 更改`Mathematica`的设置:
`Format -> Option Inspector -> Selection -> Global Preferences -> Lookup`, 然后搜索`3DRenderingEngine`, 然后选择`Mesa`.

## 调整3D图形的视角

`ViewPoint->{x,y,z}`给出视图点相对于包含物体的三维盒子中心的位置.

`ViewPoint`是在一个特殊的归一化坐标系中给出的, 在这个坐标系中, 封装盒最长边的长度为$1$, 封装盒的中心坐标为`{0,0,0}`.

`mathematica`使用的是右手坐标系,

+ `{0,-2,0}`: 正面, 在`y`轴负半轴, `Front`
+ `{0,2,0}`: 背面, 在`y`轴正半轴, `Back`
+ `{-2,0,0}`: 左面, `x`负半轴, `Left`
+ `{2,0,0}`: 右面, `x`正半轴, `Right`
+ `{0,0,2}`: 上方视角, `Above`
+
+ `{0,-2,2}`: 前上
+ `{0,-2,-2}`: 前下
+ `{-2,-2,0}`: 左下边角, `{Left,Front}`
+ `{2,-2,0}`: 右下边角,

+ `{0,0,Infinity}`: 从正上方看,平面视图.
+ `{0, -Infinity, 0}`: 从正前方看,平面视图.

按住`Alt`or`Ctrl`拖拽鼠标, 可以放大缩小, 即调整`ViewAngle`, 同时保持`ViewPoint`.

其他选项有:

`ViewVertical`,调整盒子的纵轴.
`ViewVector`
