# solver model()

单例模式

持有模型数据,
构件,
节点, 边,  单元, 单元组,
材料, 自定义集合

### All Node

`GetAllNodeIDs()`; 获取所有 Node ID
`GetAllEdgeIDs()`; 获取所有 Edge ID
`GetAllElementIDs()`; 获取所有 Ele ID

### 获取 node id

根据 node id 获取 node 对象, 并获取 node 的坐标.

```cpp
CompNode* theNd = model()->GetNode(NdID);
theNd->Coord;
```
