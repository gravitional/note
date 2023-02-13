// 单个数据向量类, 避免数据拷贝, 仅设置数据向量首元素地址与数据向量元素中元素个数
// 通用作为等长二维数组 RectArray 与不等长二维数组 JagArray 中取第i个数据向量函数接口的返回值

#pragma once
#include <vector>
#include <assert>
#include "Util.h"

namespace common
{

    template <typename T>
    class RefArray
    {
    public:
        //
    private:
        T *_data;  // 首元素地址
        int _size; // 元素数量
    }
}
