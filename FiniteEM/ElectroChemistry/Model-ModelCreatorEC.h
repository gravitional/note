#prgama once
#include "FeModelCreator.h"

// 电化学分析，模型初始化

class ModelCreatorEC : public FeModelCreator
{

    RuntimeSelectionTypeName(ElectroChemistry);

protected:
    // 设置物理场信息
    // 初始化
    // 生成物理场特有信息
    virtual void CreateMisc(ControlReader &data);
    // 生成全局参数
    void CreateInfo(ControReader &data);
    // 生成约束

    // 节点工厂方法
    // 单元工厂方法
    // 取单元的节点个数，表面单元包括所属实体ID
    // 材料工厂方法
    // 分析工厂方法

protected:
    // 方程号排序中的多点共自由度

    // 删除其他MPI节点上的对象

private:
    // 施加约束
    void ApplyConstraint();
    // 调整构件所属MPI节点
    void AdjustRankComponent();
    // 建立单元列表
    void CreateElementList();
    // 结果输出类工厂方法
    virtual FieldWriterFe *NewFieldWriter();
}
