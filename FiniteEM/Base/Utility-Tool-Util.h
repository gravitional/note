#include <vector>
#include <list>
#include <fstream>
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
    static void FindIndex(ForwardIterator first, ForwardIterator last, const T &values);
    // find equal value element, return -1 if not found
    static void FindEqualIndex(const std::vector<double> &vec, double value);
    // find nearest value element
    static void FindNearestIndex(const std::vector<double> &vec, double value);
    // find first element >= value
    template <typename T>
    static void LowerBoundIndex(const std::vector<T> &x, const T &value);
    // find first element > value
    template <typename T>
    static void UpperBoundIndex(const std::vector<T> &x, const T &value);

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