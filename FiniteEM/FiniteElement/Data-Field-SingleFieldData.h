#include <vector>
#include <unordered_map>
#include "JagArray.h"
#define HASH

// 单个场数据类型，本质为二维不定长数组，由 ID值 直接索引对应实数向量
class SingleFieldData
{
public:
    SingleFieldData();
    SingleFieldData(int num, int maxID);
    ~SingleFieldData();

    // 设置或添加数据
    void SetData(int id, const double *newData, int length);
    // 全局数据置零
    int SetZero();

    // 根据ID取数据, SingleFieldData 的数据结构
    common::RefArray<double> GetData(int id)
    {
        assert(HasData(id));
        return _data[_index[id]];
    }

    // 从另一个 SingleFieldData中拷贝数据，拷贝时乘系数
    void CopyData(SingleFieldData &other, double factor = 1.0);
    // 数值叠加另一个 SingleFieldData 中的数据，拷贝时乘系数
    void AddData(SingleFieldData &other, double factor = 1.0);
    // 是否含有某ID 数据
    bool HasData(int id);
    // 所有数据 乘系数
    void Multiply(double factor);
    // 返回ID列表
    std::vector<int> GetIDs() const;

private:
    common::JagArray<double> _data; // 数据, 二维不定长数组
#ifdef _HASH
    std::unordered_map<int, int> _index; // 由 ID值 索引到_data
#else
    std::vector<int> _index; // 由 ID值索引到_data
    int _maxID;              // 最大 ID值, 决定 _index 的长度
#endif
};
