#include "TestObj.h"
#include <iostream>

// 临时变量常引用-2:
#include <iostream>

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

const TestObj &func(const TestObj &t)
{
    return t;
}

int main()
{
    const TestObj &t = func(TestObj());
    std::cout << t.x << "\n";
    std::cout << t.x << "\n";
    return 0;
};