#include "FiniteElementHeader.h"
#include "Singleton.h"
#include <string>
#include <map>
#define FE_API

// 方程号排序类,从0开始;
class FE_API EquNoArrange
{

public:
    // 设置单个ID对应的最大自由度数，在最开始调用，不设置时默认为8
    void SetMaxDofPerID(int maxDof);
    // 节点自由度的方程号排序
    void ArrangeNodes(const std::vector<int> &nodes, int dof = 0);
    // 边自由度的方程号排序
    void ArrangeEdges(const std::vector<int> &edges, int dof = 0);
    // 单元自由度的方程号排序
    void ArrangeElements(const std::vector<int> &elements);
    // 拉格朗日乘子方程号排序
    void ArrangeLagrange(const std::vector<int> &ids, int countPerID = 1);

    //-----
    // 设置节点相同编号，使 slave与master相同
    void EqualizeNodeDof(int slave, int master);
    // 设置Edge相同编号，使 slave与master相同
    // 设置相同拉格朗日乘子, 使 slave与master相同

    // 取节点id的方程号, 如果多自由度, 返回的是起始地址, [0,1,2,)
    int *GetNodeEquNo(int id)
    {
        return &_equNo[_nodeIndex[id]];
    };
    // 取边方程号
    int *GetEdgeEquNo(int id);
    // 取单元方程号
    int *GetElementEquNo(int id);
    // 取拉格朗日乘子方程号
    int *GetLagEquNo(int id);

    //------
    // 取方程数
    int GetEquCount() { return _count; }
    // 取拉格朗日乘子数
    int GetLagCount() { return _lagCount; }

    // 设置方程数，当方程排序不应该由该类管理时使用
    void SetEquCount(double count){_count = count};

private:
    int _count;    // 自由度总数
    int _lagCount; // 拉格朗日乘子数
    // rank-> 方程编号; 如果节点有N个, 自由度为d, _equNo 的size是 N*d,
    // 受约束自由度的编号为-1
    std::vector<int> _equNo;
    std::vector<int> _nodeIndex; // rank-> 节点编号的rank,
    std::vector<int> _edgeIndex; // rank -> edge 编号的rank
    std::vector<int> _eleIndex;  // rank -> ele 编号的rank
    std::vector<int> _lagIndex;  // rank -> lag 编号的rank
};
