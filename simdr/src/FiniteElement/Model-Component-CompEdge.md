# 边构件类 CompEdge

```cpp
class FE_API CompEdge : public Component
{
public:
    // 自由度数
    virtual int DofCount(){return 2;}
    // 边对应的方程号


    // 设置两个端点的编号
    // 判断自由度是否被约束
    // 放松单个自由度
    // 约束单个自由度

    // 返回边的长度
    // 返回边中点
    // 取边的方向
    
    int nodes[2]; //两个端点的编号

private:
    bool _restrained[2];// 记录端点的约束状态    
}
```
