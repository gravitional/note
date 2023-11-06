// 常值或函数参数类
#pragma once
#include "CommonHeader.h"
#include <vector>
#include <string>

using namespace std;
class ControlReader;
class Function;

namespace common
{
    class DataIO;
}

class COMMON_API VarValue:
{

public:
    //-------- 读写
    void Read();

    //----- 设置函数
    // 取函数名称，ID

    // 是否使用函数
    // 取函数指针

    // 赋值
    // 取值
    // 取值，隐式类型转换

    // 更新值，若经过计算则返回true
    bool UpdateValue(double var);
    bool UpdateValue(const std::vector<double> &vars);
    // 计算并返回值，但不更新
    // 计算并返回一阶导数
    // 计算并返回二阶导数

    // 常数或单自变量函数的积分

    // 范围内二次提醒积分值（单变量），y1 一次积分在 xmin处的值

private:
    // 记录新参数值，并返回是否需要重新计算
    bool UpdateLastVars(const std::vector<double> &vars);

private:
   double _value;
   Function *_func;
   int _yIndex;
   mutable std::vector<double> _lastVars;
};
