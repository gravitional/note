#pragma once
#include "Component.h"
#include <map>
#define FE_API
// 基于map的组件存储，根据ID索引指针
class FE_API CompPtrMap : public map<int, Component>
{
public:
    virtual ~CompPtrMap();

    // 添加组件
    bool Add(Component *comp);
    // 添加组件并获取ID
    int AddGenID(Compoent *comp);
    bool Has(int id);
    Component *Get(int id);
    size_t Count() const;
    //
    template <typename T>
    // 获取指定类型及子类的指针列表
    void GetPtrs(vector<T *> &ptrs);

private:
    int _maxID;
}

template <typename T>
void CompPtrMap::GetPtrs(vector<T *> &ptrs)
{
    ptrs.clear();
    ptrs.reserve(Count());
    for (auto &item : *this)
    {
        auto ptr = dynamic_cast<T *>(item.second);
        if (ptr != nullptr)
        {
            ptrs.push_back(ptr);
        }
    }
}
