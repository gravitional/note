#include <vector>

bool AlgPostAnalysisEC::PostProcessElementData
{

    auto &&types = GetResultManipEC()->GetOutputFields(); // 调用平台函数，拿到要输出的场类型
    int fNum = types.size();                              // 场类型的总数
    // 记录每个场的分量数
    std::vector<int> nComp(types.size());
    for (size_t i = 0; i < fNum; i++)
    {
        nComp[i] = GetResultManipEC()->GetFieldComponentNum(types[i]);
    }

    // JagArray, 即二维不等长数组; FieldType->pt,
    // pt 是高斯点上的场分量值, 例如: 矢量场{v[0],v[1],v[2]}; 标量场 {s[0]}
    std::vector<double *> FieldVec(fNum);
    // vector of refs to Array; FieldType -> data, data 是各高斯点 pt 的Join，且存储为一维, {pt1, pt2,...}
    std::vector<common::RefArray<double>> FData(fNum);

    // 1. 计算存储 积分点（高斯点）的场值

    // 2. 计算存储 节点的场值
    return true;
}