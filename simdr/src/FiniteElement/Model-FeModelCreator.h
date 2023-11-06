#include "FiniteElementHeader.h "
#include "ModelCreator.h"

// 有限元模型创建

class CompMaterial;

class FE_API FeModelCreator : public ModelCreator
{
protected:
    // 生成网格
    // 生成模型
    // 初始化

    //----
    // 生成节点
    // 生成单元
    // 生成材料
    // 生成初始化长
    // 生成分析
    // 生成物理场特有信息

    //---
    // 节点工厂方法
    // 单元工厂方法
    // 取单元的节点个数，表面单元包括所属实体id
    // 材料工厂方法
    // 分析工厂方法

    //---
    // 方程号排序
    void ArrangeEquationNo();
    // 方程号排序中的多点共自由度
    virtual void ArrangeMPI() {}
    // 方程号排序中的拉格朗日乘子
    virtual void ArrangeLag() {}

    //--区域分解
    void PartitionMesh();

    // 删除其他MPI节点上的对象

    //---
    // 写出网格信息
    // 结果输出类工厂方法

protected:
    FeMesh *_mesh;

public:
    int locID; // 局域坐标系

protected:
    int _compID;
    vector<int> _setIDs;
    VarValue _waveAmpl;
}
