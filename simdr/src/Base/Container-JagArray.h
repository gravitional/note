/*!
 * @file JagArray.h
 * @brief 不等长二维数组类模板，采用行压缩方式存储
 *
 * 优点: 相比于使用vector<vector<T>>, JagArray<T> 可以大大减少内存碎片化，以及提高缓存命中
 * 缺点: 相比于使用vector<vector<T>>, JagArray<T> 无法动态的调整其中某个数据向量的长度
 * 该类模板采用三个成员变量来描述与存储不等长二维数组
 * 成员变量 vector<T> _data, 行压缩数据
 * 成员变量 vector<T> _index, 行压缩索引, 记录个数据向量 begin 与 end 在 _data中的位置. _index.size()= _count+1.
 * 成员变量 int _count, 数据向量个数.
 *
 * 例如二维数组[[2,3], [4,10,5], [6,1]], 采用 JagArray<int> 类存储
 * _data=[2,3,4,10,5,6,1]
 * _index=[0,2,5,7]
 * _count=3
 */

#include <vector>
#include "RefArray.h"
#include "Util.h"
#include "MPIUtil.h"

namespace common
{

    // 不等长二维数组
    template <typename T>
    class JagArray
    {
    public:
        //-----
        // 赋值
        // 右值赋值
        // 取单个数据成员，常成员函数, Debug 模式下进行越界检查
        // 返回值为临时对象
        //----
        // 取索引
        // 取数据指针
        T *RawData() { return _count = 0 ? nullptr : _data.data(); }
        // 行压缩后，一维数组的数据长度
        // 数据向量个数
        //----
        // 预留空间
        // 释放多余空间
        // 清零
        // 清空数据，包括长度

        //-----
        // 设置整体数据
        // 添加单个数据向量
        // MPI 传输
        // 取迭代器
    private:
        std::vector<T> _data;    // 数据
        std::vector<int> _index; // 数据向量的起始位置
        int _count;              // 数据向量个数
    }
}
