/// FieldData, 场数据类

#include <vector>
#define FE_API

typedef int SteroNo;
typedef int FieldType;
typedef std::unordered_map<FieldType, SingleFieldData> OneDataType;

// 场数据类, 由`工况`及`场标签`共同索引单个场数据类 `SingleFieldData`
class FE_API FieldData : public RegObject
{
public:
    FieldData(const std::string &name, int predCount = 0, int predMaxID = 0);
    // 预设数据长度以及索引最大值
    void SetPredSize(int num, int maxID);
    // 全部数据置零

    // 某工况是否有场数据
    // 某工况是否有 某ID 的场数据
    // 取 单数据场 的 ID列表

    // 根据工况, 场类型, ID 取数据
    // id 相当于二维数组的行指标, length 相当于行元素的长度
    common::RefArray<double> GetData(SteroNo steroNo, FieldType field, int id);

    // 工况之间拷贝特定场数据, 乘系数 factor
    // 工况之间叠加特定长数据，乘系数 factor

    // 增加工况类型 的槽位
    // 删除工况数据

    // 保存二进制文件
    // 加载二进制文件

private:
    // 某工况增加场数据
    void AddFieldType(StoreNo storeNo, FieldType field);
    // 是否有某工况

private:
    stde::unordered_map<StoreNo, OneDataType> _data; // 数据，由工况，场标签映射到单个 场数据
    int _num;                                        // 预设数据长度
    int _maxID;                                      // 预设最大ID值.
}

FE_API FieldData &
createFieldData(const std::string &name, int predCount = 0, int predMaxID = 0)
{
    if (!objects()->Exist<FieldData>(name))
    {
        new FieldData(name, predCount, predMaxID);
    }
    auto &result = getFieldData(name);
    result.SetPredSize(predCount, predMaxID);
    return result;
}
FE_API FieldData &getFieldData(const std::string &name)
{
    return objects()->GetRef<FieldData>(name);
}

// 单个场数据类型，本质为二维不定长数组，由ID值直接索引对应实数向量
class SingleFieldData
{

public:
    SingleFieldData(int num, int maxID);

private:
    common::JagArray<double> _data; // 数据, 二维不定长数组
#ifdef _HASH
    std::unordered_map<int, int> _index; // 由ID 值索引到_data
#else
    std::vector<int> _index; // 由 id值索引到_data
    int _maxID;              // 最大ID值, 决定 _index 的长度
#endif
};
