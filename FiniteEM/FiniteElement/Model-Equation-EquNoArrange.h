#include "FiniteElementHeader.h"
#include "Singleton.h"
#include <string>
#include <map>
#define FE_API

// 线性方程组管理类
class FE_API EquNoArrange
{

private:
    int _count;
    int _lagCount;
    std::vector<int> _equNo;
    std::vector<int> _nodeIndex;
    std::vector<int> _edgeIndex;
    std::vector<int> _eleIndex;
    std::vector<int> _lagIndex;
};
