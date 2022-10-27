#include "singleton.h"
#include <iostream>
#include<initializer_list>

class A : public Singleton<A>
{
public:
	A() = default;
	~A() = default;
	int get()
	{
		return hold;
	}

private:
	int hold = 1;
};
// 定义全局函数, 用于获取单例
inline A* getTe()
{
	return &A::GetInstance();
}

int main(int* argc, char* argv[])
{
	auto a = getTe()->get();
	std::cout << a << std::endl;
}