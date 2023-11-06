#include <assert.h>
#include <vector>
#include <bitset>
#include "FiniteElementHeader.h"
#include "Component.h"
#include "Model.h"
#include "Equation.h"
#define FE_API

// 节点构件类 CompNode

class FE_API CompNode : public Component
{
public:
    // 自由度数
    virtual int DofCount() = 0;
    // 节点对应的方程号
    virtual int *EquNo() { return equation()->GetNodeEquNo(getID()); }

    //--------------自由度约束相关
    // 判断自由度是否被约束
    virtual bool IsRestrained(int i) = 0;
    // 放松单个自由度
    virtual void Unrestrain(int i) = 0;
    // 约束单个自由度
    virtual void Restrain(int i) = 0;

    // 放松所有(整体)自由度
    virtual void Unrestrain(const int r[]) = 0;
    // 约束所有自由度
    virtual void Restrain(const int r[]) = 0;

public:
    double Coord[3];               // 节点坐标值
    std::vector<int> BelongEleIDs; // 节点所属的单元(不止一个)
}

// 模板实例化, 自由度数为 DOF 的节点类,
// 使用 std::bitset<DOF> 记录和设置约束
template <int DOF>
class Node : public CompNode
{
public:
    Node();
    virtual ~Node();

    // 自由度数
    virtual int DofCount() { return DOF; }

    //--- 自由度约束
    // 判断自由度是否被约束
    virtual bool IsRestrained(int i) = 0;
    // 放松单个自由度
    virtual void Unrestrain(int i) = 0;
    // 约束单个自由度
    virtual void Restrain(int i) = 0;

    // 放松所有(整体)自由度
    virtual void Unrestrain(const int r[]) = 0;
    // 约束所有自由度
    virtual void Restrain(const int r[]) = 0;

private:
    std::bitset<DOF> _restraint;
};

template <int DOF>
Node<DOF>::node()
{
    _restraint.set();
}

template <int DOF>
Node<DOF>::~node()
{
}

template <int DOF>
bool Node<DOF>::IsRestrained(int i)
{
    assert(i < DOF);
    return _restraint.test(i);
}

template <int DOF>
void Node<DOF>::Unrestrain(int i)
{
    assert(i < DOF);
    return _restraint.reset(i);
}

template <int DOF>
void Node<DOF>::Restrain(int i)
{
    assert(i < DOF);
    return _restraint.set(i);
}

template <int DOF>
void Node<DOF>::Unrestrain(const int r[])
{
    for (int i = 0; i < DOF; i++)
    {
        if (r[i] == 0)
        {
            Unrestrain(i);
        }
    }
}
template <int DOF>
void Node<DOF>::Restrain(const int r[])
{
    for (int i = 0; i < DOF; i++)
    {
        if (r[i] == 1)
        {
            Restrain(i);
        }
    }
}