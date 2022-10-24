#include <iostream>
#include "my_type_info.h"

int main(int *argc, char **argv)
{
    // --------------------------------- C++中如何打印类型信息?
    // typeid的局限性
    int a = 0;
    // class type_info
    std::cout << typeid(a).name() << std::endl;           // output: int
    std::cout << typeid(decltype(a)).name() << std::endl; // output: int

    int &b = a;
    // type_info
    std::cout << typeid(b).name() << std::endl;           // output: int
    std::cout << typeid(decltype(b)).name() << std::endl; // output: int

    // 比较两个类型是否相等
    std::cout << std::boolalpha;
    std::cout << (typeid(a) == typeid(b)) << std::endl; // output: true

    //--------------------------------------------
    //在C++中如何得到一个表达式的值类型?
    // decltype可以推导出一个表达式的类型
    // int a = 0;
    std::cout << "The type of a : " << type_to_string<decltype(a)>() // int
              << std::endl;
    // 如果多加一个括号可以得到值类型
    std::cout << "The value type of a : " << type_to_string<decltype((a))>() // int&
              << std::endl;
    std::cout << "The value type of std::move(a) : " << type_to_string<decltype((std::move(a)))>()
              << std::endl; // int&&

    std::cout << "The type of 10 : " << type_to_string<decltype(10)>()
              << std::endl; // int
    std::cout << "The value type of 10 : " << type_to_string<decltype((10))>()
              << std::endl; // int

    //---------------------------------------------------------
    std::cout << "=======Test for 10, a, move(a)=====" << std::endl;
    std::cout << std::boolalpha;
    value_type(10);
    // int a = 10;
    std::cout << "===========================" << std::endl;
    value_type(a);
    std::cout << "===========================" << std::endl;
    value_type(std::move(a));

    //下面来看下, 一个 `右值引用` 类型的 `变量类型` 和 `值类型`
    std::cout << "===========================" << std::endl;
    int &&c = 20;
    std::cout << "The type of c : " << type_to_string<decltype(c)>() << std::endl;
    value_type(c);
}
