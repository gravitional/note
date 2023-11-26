# 电磁自适应网格

```cpp
class CriteriaBase; //收敛判断类

class CriteriaManager; //管理不同的 收敛判断类

// 需要的接口
bool IsConvergence(); //返回是否收敛
std::vector<int> RefineElementList(); // 需要细化的单元列表

Residual //计算残差
```
