#include <iostream>
using namespace std;

class Point
{
public:
    Point(int xx, int yy); //构造函数
    Point();
    ~Point();              //默认构造函数
    Point(const Point &p); //拷贝构造函数
    int getX();

private: //私有数据
    int x, y;
};

//成员函数的实现
Point::Point(int xx, int yy)
{
    x = xx;
    y = yy;
}
Point::~Point()
{
    cout << "remove garbage"<<endl ;
}
Point::Point() : x(0), y(0) {} //默认构造函数.
Point::Point(const Point &p)
{
    x = p.x;
    y = p.y;
    cout << "Calling the copy constructor" << endl;
}

int Point::getX()
{
    return x;
}

// 形参为Point 类对象的函数
void fun1(Point p)
{
    cout << p.getX() << endl;
    //p.setX(1);
}

// 返回值为Point 类对象的函数
Point fun2()
{
    Point c;
    return c;
}

// 主程序
int main()
{
    Point a; // 第一个对象A
    Point b(a);
    cout << "first time" << endl; // 情况一，用A初始化B，调用拷贝构造函数
    cout << b.getX() << endl;
    fun1(b); //情况二，对象b作为fun1的实参，调用拷贝构造函数
    cout << "second time" << endl;
    b = fun2(); // 情况三，函数的返回值是类对象，函数返回时调用拷贝构造函数
    cout << "third time" << endl;
    cout << b.getX() << endl;
    return 0;
}
