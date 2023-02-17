#pragma once
#include "FiniteElementHeader.h"
#include <vector>
#include <unordered_map>
#include "CompPtrMap.h"
#define FE_API

// 组件容器，对 CompPtrMap 的简单封装
using namespace std;
class FE_API CompStorageGneneral
{
public:
    virtual ~CompStorageGneneral();

    // 添加组件
    template <typename T>
    bool Add(T *comp);

    // 添加组件并赋予新id
    // 根据id 得到组件
    // 得到个数
    template <typename T>
    int Count();

    // 得到T及子类的指针向量
    template <typename T>
    void GetPtrs(vector<T *> &ptrs);

private:
    void CheckAndCreateLabel(const string &label);

private:
    unordered_map<string, CompPtrMap> _compStorage;
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
