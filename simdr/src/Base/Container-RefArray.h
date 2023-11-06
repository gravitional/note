// 单个数据向量类, 避免数据拷贝, 仅设置数据向量首元素地址与数据向量元素中元素个数
// 通用作为等长二维数组 RectArray, 与不等长二维数组 JagArray 中取第i个数据向量函数接口的返回值

#pragma once
#include <vector>
#include <assert>
#include "Util.h"

namespace common
{
    // 单个数据向量类，避免数据拷贝
    template <typename T>
    class RefArray
    {
    public:
        // 默认构造函数
        RefArray() : _data(nullptr), _size(0) {}
        // 构造函数，直接设置外部指针;
        // data 数据向量首元素地址
        // size 数据向量中元素数量
        RefArray(const T *data, int size) : _data(const_cast<T>(data)), _size(size) {}
        RefArray(T *data, int size) : _data(data), _size(size) {}
        // 取数据指针
        // 取数据数量
        // 取数据， Debug 模式下进行越界检查
        T &opeartor[](int i)
        {
            assert(i >= 0 && i < this->_size);
            return _data[i];
        }
        // 原位翻转数据
        // 判断数组是否为空
        bool Empty() const { return _size = 0; }
        // 迭代器
    private:
        T *_data;  // 首元素地址
        int _size; // 元素数量
    }
}
