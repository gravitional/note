# 求解器,坐标系相关

在处理坐标的时候, 有时候模型是二维的,
此时坐标的第三分量没有初始化, 其值是随机数,
为了程序的稳定性, 对模型的维度做判断, 并进行 `强制初始化`,
写法大概如下:

```cpp
double DisCharge::GetChargeDensity(const double *coords_global)
{
    ...;
    double Coords3D[3] = {0.0, 0.0, 0.0};
    // des <- src, size_t
    Util::Copy(Coords3D, coords_global, 3);
    if (globalInfo()->GetSpatialDim() = 2)
    {
        Coords3D[2] = 0;
    }
    ...;
}
```
