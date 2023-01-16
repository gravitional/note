# 坐标系相关操作

局域坐标, 球坐标, 调用坐标变换后, 返回的坐标分量次序, 见
Math/CoordSys/SphericalCoordSys.cpp:89

```cpp
coordLocal[0]=theta;
coordLocal[1]=phi;
coordLocal[2]=rho;
```

即 $\theta,\phi,r$, 与通常的约定相反.
