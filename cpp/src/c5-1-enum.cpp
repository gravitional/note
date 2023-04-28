// te1.cpp : 此文件包含 "main" 函数. 程序执行将在此处开始并结束.
#include <iostream>
#include <functional>
#include "runTest.h"
#include "magic_enum.hpp"

// include  https://github.com/Neargye/magic_enum/blob/master/include/magic_enum.hpp

using namespace std;
//-------------------------------------
using TestFn = function<int()>;

static int order = 0;

int runTest(TestFn fn)
{
    cout << "=============================== " << order << endl;
    fn();
    cout << endl
         << "-------------------------------" << endl;
    order++;
    return 0;
}

//-----------------------------------------------

enum class Color : int
{
    RED = -2,
    BLUE = 0,
    GREEN = 2,
    NONE
};

int test1()
{
    // Enum value to string
    Color color = Color::RED;
    auto color_name = magic_enum::enum_name(color);
    // color_name -> "RED"
    cout << color_name << endl;
    return 0;
}

int test2()
{
    // String to enum value

    std::string color_name{"GREEN"};
    auto color = magic_enum::enum_cast<Color>(color_name);
    int value = 0;
    if (color.has_value())
    {
        // color.value()->Color::GREEN
        value = static_cast<int>(color.value());
        cout << value << endl;
    }

    auto color_or_default = magic_enum::enum_cast<Color>(value).value_or(Color::NONE);
    cout << magic_enum::enum_name(color_or_default) << endl;
    return 0;
}

int test3()
{

    // Integer to enum value
    int color_integer = 2;
    auto color = magic_enum::enum_cast<Color>(color_integer);
    int value = 0;
    if (color.has_value())
    {
        // color.value() -> Color::BLUE
        value = static_cast<int>(color.value());
        cout << value << endl;
    }

    auto color_or_default = magic_enum::enum_cast<Color>(value).value_or(Color::NONE);
    cout << magic_enum::enum_name(color_or_default) << endl;
    return 0;
}

int test4()
{

    // Indexed access to enum value

    std::size_t i = 0;
    Color color = magic_enum::enum_value<Color>(i);
    // color -> Color::RED
    return 0;
}
//--------------------------------------------------
int main()
{
    runTest(test1);
    runTest(test2);
    runTest(test3);
    runTest(test4);
    return 0;
}