#pragma once
#include <string>
#include <unordered_map>

#define MATH_API
template <typename T>
class Singleton;

// 常量管理类
class MATH_API Constant : public Singleton<Constant>
{
public:
    //--- 动态常量
    void Read();
    // 从文件中读
    // 设置浮点数常量
    // 取浮点数常量
    // 设置几何向量常量
    // 取几何向量常量
private:
    std::unordered_map<std::string, double> _values;
    std::unordered_map<std::string, TsVector> _vector;
    //--- 静态常量
public:
    const double PI_2 = 1.5707963267948966192313216916398;   // 圆周率的平方
    const double PI = 3.1415926535897932384626433832795;     // 圆周率, pi
    const double PI2 = 6.283185307179586476925286766559;     // 2*pi
    const double Epsi0 = 8.854187817620389851E-12;           // 真空磁导率
    const double Mu0 = 1.2566370614359172953850573533118E-6; // 真空介电常数 (1/c^2*Mu0)
    const double Boltzmann = 1.3806505E-23;                  // 玻尔兹曼常数
    const double ElectronCharge = 1.60217662E-19;            // 电子电荷
    const double DegreeKelvin = 273.15;                      // 水三相点温度，开尔文温度
    const double UniversalGasConst = 8.3145;                 // 通用气体常数
    const double StefanBoltzmann = 5.67E-8;                  // Stefan-Boltzmann常数
};

// 全局函数, 分析单例
inline Constant *constant()
{
    return &Constant::GetInstance();
}