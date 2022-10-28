#include "singleton.h"
#include <iostream>
#include <initializer_list>

class A : public Singleton<A>
{
public:
    A() = default;
    //子类需要提供此构造函数,用于 call of GetInstance()
    A(token){};
    ~A() = default;
    int get()
    {
        return hold;
    }

private:
    int hold = 1;
};
// 定义全局函数, 用于获取单例
inline A *getTe()
{
    return &A::GetInstance();
}

int main(int *argc, char *argv[])
{
    auto a = getTe()->get();
    std::cout << a << std::endl;
}