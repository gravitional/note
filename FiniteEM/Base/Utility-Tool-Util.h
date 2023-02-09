#include "BaseHeader.h"
#include <vector>
#include <list>
#include <fstream>
#include <complex>
#include <algorithm>
#include <string.h>
#include <assert.h>

#define BASE_API

// 工具类
class BASE_API Util
{
private:
    static const double ZERO_EPS;

    //---array，数组工具
public:
    // x=0 赋值工具
    template <typename T>
    static void Zero(T *x, size_t n);
    template <typename T>
    static void Zero(std::vector<T> &x);
    // x=y
    template <typename T>
    static void Copy(T *dst, const T *src, size_t n);
    // x+=y
    template <typename T>
    static void Add(std::vector<T> &x, std::vector<T> &y);
    // x*=y
    template <typename T>
    static void Multiply(size_t n, T *x, T y);
    // append y to x, x~y
    template <typename T>
    static void Append(std::vector<T> &x, std::vector<T> &y);
    template <typename T>
    static void Append(std::vector<T> &x, T *y, size_t n);

    //------ find vlaue element
    template <typename ForwardIterator, typename T>
    static int FindIndex(ForwardIterator first, ForwardIterator last, const T &values);
    // find equal value element, return -1 if not found
    static int FindEqualIndex(const std::vector<double> &vec, double value);
    // find nearest value element
    static int FindNearestIndex(const std::vector<double> &vec, double value);
    // find first element >= value
    template <typename T>
    static int LowerBoundIndex(const std::vector<T> &x, const T &value);
    // find first element > value
    template <typename T>
    static int UpperBoundIndex(const std::vector<T> &x, const T &value);

    //---- sort and remove duplicated components
    template <typename T>
    static void SortAndDedup(const std::vector<T> &vec);

    //------ algebra, 代数
    // 是否合法
    static bool IsValid(double x);
    // compare
    // IsZero
    // IsEqual
    // IsLess
    // IsGreater
    // IsVecLess

    // 是否为单位矩阵

    // 2-norm，模长
    // dot product 向量点乘
    // Random 随机数， 返回 0~1 之间 double 类型的随机数
    // 共轭 conjuate

    //---- geometry 几何
public:
    // 计算两点距离
    // 2D 多边形面积
    // 三角形面积，四边形面积

    // vector, 矢量运算
    // 计算 vec2 在 vec1 垂直方向的投影

    //--- quick Sort, Hoare 排序
    template <typename T1, typename T2>
    static void QuickSort(T1 *keys, T2 *values, size_t start, size_t end);

private:
    template <typename T1, typename T2>
    static size_t QuickSortPartition(T1 *keys, T2 *values, size_t start, size_t end);

    //--- allocate, 内存分配
    // malloc space
    // free space
    // 释放指针并置空
    // 批量析构
    // 释放指针并清空

    //--- class type
public:
    template <typename TBase, typename TDerived>
    bool IsA(const TDerived *ptr);

    //--- int limit
public:
    // check if two integer multiplication exceed the limit of int.
    static bool IsMultExceed(const int a, const int b);
};

//-----
template <typename T>
void Util::Zero(T *x, size_t n)
{
    memset(x, 0, n * sizeof(T))
}

template <typename T>
void Util::Zero(std::vector<T> &x)
{
    memset(x.data(), 0, x.size() * sizeof(T))
}

template <typename T>
void Util::Copy(T *dst, const T *src, size_t n)
{
    memcpy(dst, src, n * sizeof(T))
}

template <typename T>
void Util::Add(std::vector<T> &x, std::vector<T> &y)
{
    assert(x.size() == y.size());
    for (size_t i = 0; i < x.size(); i++)
    {
        x[i] += y[i];
    }
}

template <typename T>
void Util::Multiply(size_t n, T *x, T y)
{
    assert(x.size() == y.size());
    for (size_t i = 0; i < n; i++)
    {
        x[i] *= y;
    }
}

template <typename T>
void Util::Append(std::vector<T> &x, std::vector<T> &y)
{
    x.insert(x.end(), y.begin(), y.end());
}

template <typename T>
void Util::Append(std::vector<T> &x, const T *y, size_t n)
{
    size_t m = x.size();
    x.resize(m + n);
    Copy(&x[m], y, n);
}

template <typename ForwardIterator, typename T>
int Util::FindIndex(ForwardIterator first, ForwardIterator last, const T &value)
{
    return lower_bound(first, last, value) - first;
}

template <typename T>
int Util::LowerBoundIndex(const std::vector<T> &x, const T &value)
{
    return lower_bound(x.begin(), x.end(), value) - x.begin();
}

template <typename T>
int Util::UpperBoundIndex(const std::vector<T> &x, const T &value)
{
    return upper_bound(x.begin(), x.end(), value) - x.begin();
}

//
template <typename T>
int Util::SortAndDedup(const std::vector<T> &vec)
{
    std::sort(vec.begin(), vec.end());
    auto iter = std::unique(vec.begin(), vec.end());
    vec.resize(iter - vec.begin());
    vec.shrink_to_fit();
}

template <typename T1, typename T2>
void Util::QuickSort(T1 *keys, T2 *values, size_t start, size_t end)
{
    if (start + 1 < end)
    {
        size_t m = QuickSortPartition(keys, values, start, end);
        QuickSort(keys, values, start, m);
        QuickSort(keys, values, m, end);
    }
}



//
template <typename T>
bool Util::Clear(std::list<T> &list)
{
    for (auto &x : list)
    {
        delete x;
    }
    list.clear();
}

// 判断类型 is a
template <typename TBase, typename TDerived>
bool Util::IsA(const TDerived *ptr)
{
    return dynamic_cast<const TBase *>(ptr) != nullptr;
}
