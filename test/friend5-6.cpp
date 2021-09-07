#include <iostream>
#include <cmath>
using namespace std;

class Point //Point 类定义
{
private:
    int x, y;
    static int count; //静态数据成员声明，用于记录点的个数。

public: //外部接口
    Point(int x = 0, int y = 0) : x(x), y(y){};
    Point(Point &p);
    int getX() { return x; }
    int getY() { return y; }
    friend float dist(Point &a, Point &b);
};

Point::Point(Point &p) //复制构造函数
{
    x = p.x;
    y = p.y;
}

float dist(Point &a, Point &b) // 传递引用效率更高
{
    double x = a.x - b.x;
    double y = a.y - b.y;
    return static_cast<float>(sqrt(x * x + y + y));
}

int main()
{
    Point p1(1, 1), p2(500, 999);
    cout << "The distance is: ";
    cout << dist(p1, p2) << endl;
    return 0;
}
