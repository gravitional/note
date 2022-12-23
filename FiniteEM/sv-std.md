# solver std 用例

+ 初始化嵌套 vector

```cpp
vector<vector<int>>& SubTriNdIndexs
SubTriNdIndexs={
    {0,4,7},
    {4,1,5},
    {5,2,6},
    {6,3,7},
    {4,6,7},
    {4,5,6},
}
```

对于逆时针标准编号的 Quad8 单元,
这些指标构成了 Quad8 单元的三角形再剖分.
编号都按逆时针顺序.
