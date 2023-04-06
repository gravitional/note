#include <vector>
#define FE_API

// 单元基类 CompElement.h

class Component;
class CompMaterial;
class FE_API CompElement : public Component
{
public:
    // 判断单元类型,  Solid, Plane, Shell, Frame, Constraint, Connect, Contact, Entity

    // 单元维度
    // 单元本身的自由度数
    virtual int DofCount() { return 0; }
    // 单元对应的方程号

    // 顶点数, vertex
    // 节点数, node
    // 自然坐标维数
    // 高斯点数
    // 自由度数, 单元的成员的自由度?
    // 定位向量, vector<int>, 局域编号和全局编号的 联系表

    // 设置节点坐标
    // 设置表面单元所属实体
    virtual void SetOwner(int eleID);

    // 设置单元本身自由度约束
    // 设置节点的自由度约束, 1 为约束, 0为放松
    void SetBndryNodeDofRestraint(const int restraint[]);
    // 取单元实际节点号

    // 取单元中心点坐标
    void GetEleCoodCenter(double xyz[3]);

    // 取高斯信息, 采样点总数, 第m个采样点, 等参坐标值, 权重
    virtual void GetGaussIntgInfo(int IntgNum, int m, double *zeta, double &W) {}
    // 取单元温度
    virtual double GetEleCentroidTemperature(int jobID) { return 0.0; }

    // 取节点在单元中的序号, 局部编号
    int GetNodePos(int nodeID);

    int MatID;                     // 单元材料号
    std::vector<int> BndryNodeIDs; // 边界节点ID

protected:
    CompMaterial *GetMaterial();

    // MPI, SetRank, BelongRank?
};