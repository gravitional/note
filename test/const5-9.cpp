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
    friend float dist(const Point &p1, const Point &p2);
};

Point::Point(Point &p) //复制构造函数
{
    x = p.x;
    y = p.y;
}

float dist(const Point &p1, const Point &p2) // 传递引用效率更高
{
    double x = p1.x - p2.x;
    double y = p1.y - p2.y;
    return static_cast<float>(sqrt(x * x + y + y));
}

int main()
{
    Point myp1(1, 1), myp2(500, 999);
    cout << "The distance is: ";
    cout << dist(myp1, myp2) << endl;
    return 0;
}
