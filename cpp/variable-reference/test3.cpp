#include "TestObj.h"
#include <iostream>

// Test3.cpp(局部对象引用):
class TestObj
{
public:
    TestObj()
    {
        std::cout << "default constructor\n";
    }

    TestObj(const TestObj &)
    {
        std::cout << "copy constructor\n";
    }

    virtual ~TestObj()
    {
        std::cout << "destructor\n";
    }

    void print() const
    {
        std::cout << "print\n";
    }

    int x = 1;
};

TestObj &func()
{
    TestObj t;
    return t;
}
int main()
{
    TestObj &t = func();
    std::cout << t.x << "\n";
    std::cout << t.x << "\n";
    return 0;
};
