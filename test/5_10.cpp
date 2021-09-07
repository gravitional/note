#include "Point.h"
#include <iostream>
using namespace std;

int main()
{
    Point::showCount(); //输出对象个数
    Point(a)(4, 5);     //定义对象a，构造函数使 count 增加1
    cout << " Pint A: " << a.getX() << " , " << a.getY() << endl;
    Point::showCount(); //输出对象个数
    a.showCount();      //也可以通过对象名调用.
    Point b(a);         //定义对象b，count 增加1.
    cout << " Pint B: " << b.getX() << " , " << b.getY() << endl;
    b.showCount(); //输出对象个数
    return 0;
}