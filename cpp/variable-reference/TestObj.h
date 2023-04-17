// 演示变量的引用，不能返回局部变量的引用

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
};