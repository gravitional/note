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