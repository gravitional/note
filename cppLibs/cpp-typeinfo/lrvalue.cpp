#include <iostream>
#include "my_type_info.h"

void f(int &&a)
{ //右值引用重载
    std::cout << "overload: rvalue" << std::endl;
}

void f(int &a)
{ //左值引用重载
    std::cout << "overload: lvalue" << std::endl;
}

void g(int &&a)
{
    std::cout << std::boolalpha;
    std::cout << "The expr type of a : " << type_to_string<decltype(a)>() << std::endl;
    std::cout << "The value type of a is lvalue : " << is_lvalue<decltype((a))> << std::endl;
    f(a);
}

int main()
{
    int a = 10;
    std::cout << "*********************** f(a) overload:" << std::endl;
    f(a);
    f(10);
    std::cout << "*********************** g() overload:" << std::endl;
    g(10);
    g(std::move(a));
    return 0;
}
