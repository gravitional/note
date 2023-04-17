#include "TestObj.h"

// Test1.cpp
TestObj func()
{
    return TestObj();
}

int main()
{
    TestObj obj = func();
    return 0;
};