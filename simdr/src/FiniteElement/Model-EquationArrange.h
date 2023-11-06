#include "FiniteElementHeader.h"
#include "Singleton.h"
#include <string>
#include <map>
#define FE_API

// 方程号排序类,从0开始;
class FE_API EquationArrage final : public Singleton<EquationArrage>
{
public:
    EquationArrange(token);
    virtual ~EquationArrange();

    // 初始化，用于方程组重新设置并排序
    void Initialize();

    // 根据元素id设置自由度及激活情况
    //  长度为 dof, 0 为激活, -1 为未激活, 若为 nullptr 则全部激活
    void SetNodeDof(int id, int ndof, const int *restrain);
    void SetEdgeDof(int id, int ndof, const int *restrain);
    void SetElementDof(int id, int ndof);

    // 设置额外自由度（如 lagrange 乘子等）
    // return 额外自由度的索引id，类似节点号，用于提取方程号，注意此id 不是方程号!!!
    int setAuxDof(int ndof);

    //-----
    // 设置主从自由度, 从自由度编号与主自由度编号相同
    // slave 从id, 非方程号
    // master 主id, 非方程号
    // master dof 元素中的自由度号, 0-base, 非方程号
    void EqualizeNodeDof(int slave, int master, int dof);
    void EqualizeEdgeDof(int slave, int master, int dof);
    void EqualizeElementDof(int slave, int master, int dof);
    void EqualizeAuxDof(int slave, int master);

    // 创建方程号排序, 在初始化，设置自由度，以及主从后调用
    void Generate();

    // 方程数, 创建方程号排序后调用
    int EquCount() const;

    // 根据 元素id 取方程号，返回起始地址，创建方程号排序后调用
    //  取节点id的方程号, 如果多自由度, 返回的是起始地址, [0,1,2,)
    int *GetNodeEquNo(int id)
    {
        return &_equNo[_nodeIndex[id]];
    };

    // 根据元素id 取方程好，返回起始地址, 创建方程号排序后调用
    const int *GetEdgeEquNo(int id) const;
    const int *GetElementEquNo(int id) const;
    const int *GetLagEquNo(int id) const;

    // 设置方程数，当方程排序不应该由该类管理时使用
    void SetEquCount(int count) { _equCount = count; }

    // 取拉格朗日乘子数
    const int GetLagCount() { return _lagCount; }

private:
    struct EqualDof
    {
        EqualDOF(int slv, int mst, int d) : slave(slv), master(mast), dof(d) {}
        int slave;
        int master;
        int dof;
    }

    void
    SetDof(vector<int> &index, int id, int ndof, const int *restrain);
    void EliminateSlaveDof(vector<int> &index, vector<EqualDof> &equalDof);
    void SetSlaveEquNo(vector<int> &index, vector<EqualDof> equalDof);

private:
    int _equCount; // 自由度总数
    int _auxCount; // 拉格朗日乘子数
    // rank-> 方程编号; 如果节点有N个, 自由度为d, _equNo 的size是 N*d,
    // 受约束自由度的编号为-1
    std::vector<int> _equNo;
    std::vector<int> _nodeIndex; // rank-> 节点方程号 rank,
    std::vector<int> _edgeIndex; // rank -> edge 节点方程号 rank
    std::vector<int> _eleIndex;  // rank -> ele 节点方程号 rank
    std::vector<int> _auxIndex;  // rank -> lag 节点方程号 rank

    std::vector<EqualDof> _nodeEqualDof;
    std::vector<EqualDof> _edgeEqualDof;
    std::vector<EqualDof> _eleEqualDof;
    std::vector<EqualDof> _auxEqualDof;
};

// 全局函数
inline EquationArrange *equArr()
{
    return &EquationArrage::GetInstance();
}