#include <iostream>
#include "my_type_info.h"

int main(int *argc, char **argv)
{
    std::cout << "==================== typeid(): " << std::endl;
    // C++中如何打印类型信息?  typeid的局限性
    int a = 0;
    // class type_info
    std::cout << "a: " << typeid(a).name() << std::endl;                     // output: int
    std::cout << "decltype(a): " << typeid(decltype(a)).name() << std::endl; // output: int

    int &b = a;
    // type_info
    std::cout << "&a : " << typeid(b).name() << std::endl;                    // output: int
    std::cout << "decltype(&a): " << typeid(decltype(b)).name() << std::endl; // output: int
    // 比较两个类型是否相等
    std::cout << std::boolalpha;
    std::cout << "&a == a : " << (typeid(a) == typeid(b)) << std::endl; // output: true

    std::cout << "====================== type_to_string<decltype(a)>()" << std::endl;
    //在C++中如何得到表达式的 value type?
    // decltype可以推导出 expression type
    // int a = 0;
    std::cout << "The type of a : " << type_to_string<decltype(a)>() // output: int
              << std::endl;
    // 如果多加一个括号可以得到 value type
    std::cout << "The value type of a : " << type_to_string<decltype((a))>() // output: int&
              << std::endl;
    std::cout << "The value type of std::move(a) : " << type_to_string<decltype((std::move(a)))>()
              << std::endl; // output: int&&
    std::cout << "The type of 10 : " << type_to_string<decltype(10)>()
              << std::endl; // output: int
    std::cout << "The value type of 10 : " << type_to_string<decltype((10))>()
              << std::endl; // output: int

    //---------------------------------------------------------
    std::cout << "================== value type of 10:" << std::endl;
    std::cout << std::boolalpha;
    value_type(10);
    // int a = 10;
    std::cout << "================== value type of a:" << std::endl;
    value_type(a);
    std::cout << "================== value type of move(a)=========" << std::endl;
    value_type(std::move(a));

    //下面来看下, 一个 `右值引用` 类型的 `变量类型` 和 `值类型`
    std::cout << "===========================" << std::endl;
    int &&c = 20;
    std::cout << "The type of c : " << type_to_string<decltype(c)>() << std::endl;
    value_type(c);
}
