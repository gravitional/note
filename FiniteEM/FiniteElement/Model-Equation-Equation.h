#include "FiniteElementHeader.h"
#include "Singleton.h"
#include <string>
#include <map>
#define FE_API

class EquNoArrange;

// 线性方程组管理类
class FE_API Equation : public Singleton<Equation>
{
    Equation(token);
    virtual ~Euqation();
    //--------- 方程排序
public:
    // 新建方程排序，已存在则清空
    // 删除一个方程排序
    // 设为当前方程排序
    // 取当前方程排序
    EquNoArrange *Get() { return _curArrage; }

    // 取当前节点方程号
    int *GetNodeEquNo(int id)
    {
        return _curArrange->GetNodeEquNo(id);
    };
    // 取当前边方程号
    int *GetEdgeEquNo(int id);
    // 取当前单元方程号
    int *GetElementEquNo(int id);
    // 取当前拉格朗日乘子方程号
    int *GetLagEquNo(int id);
    // 取方程数
    int Count();

private:
    EquNoArrange *_curArrange;
    std::map<std::string, EquNoArrange> _arrange;
};

// 全局函数
inline Equation *equation()
{
    return &Equation::GetInstance();
}