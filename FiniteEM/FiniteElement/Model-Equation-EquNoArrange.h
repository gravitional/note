#include "FiniteElementHeader.h"
#include "Singleton.h"
#include <string>
#include <map>
#define FE_API

// 方程号排序类,从0开始
class FE_API EquNoArrange
{

public:
    // 设置单个ID对应的最大自由度数，在最开始调用，不设置时默认为8
    // 节点自由度的方程号排序
    void ArrangeNodes(const std::vector<int> &nodes, int dof = 0);
    // 边自由度的方程号排序
    // 单元自由度的方程号排序
    // 拉格朗日乘子方程号排序

    //-----
    // 设置节点相同编号，使 slave与master相同
    void EqualizeNodeDof(int slave, int master);
    // 设置Edge相同编号，使 slave与master相同
    // 设置相同拉格朗日乘子, 使 slave与master相同

    // 取节点方程好
    int *GetNodeEquNo(int id)
    {
        return &_equNo[_nodeIndex[id]];
    };
    // 取边方程号
    // 取单元方程号
    // 取拉格朗日乘子方程号

    //------
    // 取方程数
    int GetEquCount() { return _count; }
    // 取拉格朗日乘子数
    int GetLagCount() { return _lagCount; }

    // 设置方程数，当方程排序不应该由该类管理时使用
    void SetEquCount(double count){_count = count};

private:
    int _count;                  // 自由度总数
    int _lagCount;               // 拉格朗日乘子数
    std::vector<int> _equNo;     // rank-> 方程编号; 约束自由度的编号为-1
    std::vector<int> _nodeIndex; // rank-> 节点编号的rank,
    std::vector<int> _edgeIndex; // rank -> edge 编号的rank
    std::vector<int> _eleIndex;  // rank -> ele 编号的rank
    std::vector<int> _lagIndex;  // rank -> lag 编号的rank
};
