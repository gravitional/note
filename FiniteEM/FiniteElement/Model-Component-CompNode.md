# 节点构件类 CompNode

```cpp
class FE_API CompNode : public Component
{
public:
    // 自由度数
    virtual int DofCount()=0;
    // 节点对应的方程号

    //--------------自由度约束相关
    // 判断自由度是否被约束
    // 放松单个自由度
    // 约束单个自由度
    // 放松所有(整体)自由度
    // 约束所有自由度 

public:
    double Coord[3]; // 节点坐标值
    vector<int> BelongEleIDs; // 节点所属的单元，不止一个
}

// 模板实例化, 自由度数为 DOF 的节点类, 
// 使用 std::bitset<DOF> 记录和设置约束
template<int DOF>
class Node : public CompNode{
public:
...
}
```
