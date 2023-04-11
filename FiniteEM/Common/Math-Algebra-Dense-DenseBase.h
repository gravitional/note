#pragma once
#include "MathHeader.h"
#include <vector>
#include <complex>
#include <string>
#include <string.h>
#include <assert.h>
#include <ostream>
#define MATH_API

using namespace std;

template <typename T>

class MATH_API DenseBase
{
public:
    // 默认构造函数，不建议使用
    DenseBase();
    // 零初始化构造函数
    DenseBase(int row, int cols);
    // 数据指针初始化函数，此时不拥有数据所有权，修改后亦同时修改外部数据，无法更改尺寸
    DenseBase(const T *data, int row, int cols) : _variableLength(true)
    {
        SetSize(rows, cols, true); // 会对数据进行置零
    }

    DenseBase(T *data, int row, int cols);

    // 拷贝构造函数
    DenseBase(const DenseBase &other);
    DenseBase(DenseBase &&other);

protected:
    // 复制对象
    void Copy(const DenseBase &other);
    void Copy(DenseBase &&other);
    // 设置行列数
    void SetSize(int rows, int cols, bool initZero = false)
    {
        assert(rows >= 0 && cols >= 0 && _variableLength);
        _rows = rows;
        _cols = cols;
        _data.resize(rows * cols);
        _v = _data.data();
        if (initZero)
            SetZero();
    }

    void SetZero()
    {
        if (Size() > 0)
        {
            memset(Data(), 0, sizeof(T) * Size());
        }
    }

public:
    // 取元素, 元素按照列优先存储
    inline T &operator()(int row, int col)
    {
        assert(row >= 0 && row < _rows);
        assert(col >= 0 && col < _cols);
        return Data()[col * Rows() + row];
    }

private:
protected:
    std::vector<T> _data;
    T *_v;
    int _rows;
    int _cols;
    bool _variableLength; // 是否为可变长度
}