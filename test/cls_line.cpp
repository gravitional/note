#include <iostream>
#include <cmath>
using namespace std;

class Point
{
public:
    Point(int xx, int yy); //构造函数
    Point();               //默认构造函数
    Point(const Point &p); //拷贝构造函数
    int getX() { return x; }
    int getY() { return y; }

private: //私有数据
    int x, y;
};

Point::Point() : x(0), y(0) {}                 //默认构造函数.
Point::Point(int xx, int yy) : x(xx), y(yy) {} //构造函数.
Point::Point(const Point &p)
{
    x = p.x;
    y = p.y;
    cout << "Calling the copy constructor" << endl;
}

class Line
{
public:
    Line(Point xp1, Point xp2); //构造函数
    Line();
    Line(const Line &l); //拷贝构造函数
    double getLen() { return len; }

private:          //私有数据
    Point p1, p2; //Point 类的对象 p1,p2。先初始化p1
    double len;
};

//组合类的构造函数
Line::Line(Point xp1, Point xp2) : p1(xp1), p2(xp2)
{
    cout << "Callint the copy constructor of Line" << endl;
    double x = static_cast<double>(p1.getX() - p2.getX());
    double y = static_cast<double>(p1.getY() - p2.getY());
    len = sqrt(x * x + y * y);
}

//组合类的copy函数
Line::Line(const Line &l) : p1(l.p1), p2(l.p2)
{
    cout << "Callint the copy constructor of Line" << endl;
    len = l.len;
}

// 主程序
int main()
{
    Point myp1(1, 1), myp2(4, 5);
    Line line(myp1, myp2); //建立Line 类的对象
    Line line2(line);      //利用拷贝构造函数建立一个新对象.
    cout << "The length of the line is ";
    cout << line.getLen() << endl;
    cout << "The length of the line2 is ";
    cout << line2.getLen() << endl;
    return 0;
}

