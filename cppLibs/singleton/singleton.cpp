#include "singleton.h"
#include <iostream>
class Te : public Singleton<Te>
{
public:
	Te() : hold(0) {};
	~Te() = default;
	int get()
	{
		return hold;
	}

private:
	int hold;
};
inline  Te* getTe() {
	return &Te::GetInstance();
}

int main(int* argc, char* argv[])
{
	auto a = getTe()->get();
	std::cout << a << std::endl;
}