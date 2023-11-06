// 管理分析中的 载荷，激励

class FE_API LoadJobBase : public Component
{
public:
    //--- 节点载荷/激励
    // 添加激励
    // 提取所有节点的索引列表
    // 提取id对应的节点激励

    //---单元载荷
    // 添加激励
    // 提取所有单元的索引列表
    // 提取id对应的单元激励

    //--- 更新激励

    //--- 适配器

protected:
    std::vector<Load *> _ndLoads;                  // 节点载荷存储
    std::vector<Load *> _eleLoads;                 // 节点载荷存储
    std::map<int, std::list<Load *>> _eleID2Loads; // 单元ID索引特定单元在和, 只有索引功能
    std::map<int, std::list<Load *>> _ndID2Loads;  // 节点ID索引特定单元在和, 只有索引功能
};
