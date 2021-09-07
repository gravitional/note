#include <iostream>
using namespace std;

class Point //Point 类定义
{
private:
    int x, y;
    static int count; //静态数据成员声明，用于记录点的个数。

public: //外部接口
    Point(int x = 0, int y = 0) : x(x), y(y)
    {            //构造函数
        count++; //在构造函数中对 count 累加，所有对象共同维护同一个count
    };
    Point(Point &p);
    ~Point() { count--; }
    int getX() { return x; }
    int getY() { return y; }
    void showCount()
    { // 输出静态数据成员
        cout << " Object count " << count << endl;
    }
};

Point::Point(Point &p) //复制构造函数
{
    x = p.x;
    y = p.y;
    count++;
}

int Point::count = 0; //静态数据成员定义和初始化，使用类名限定。

int main()
{
    Point(a)(4, 5); //定义对象a，构造函数使 count 增加1
    cout << " Pint A: " << a.getX() << " , " << a.getY() << endl;
    a.showCount(); //输出对象个数
    Point b(a);    //定义对象b，count 增加1.
    cout << " Pint B: " << b.getX() << " , " << b.getY() << endl;
    b.showCount(); //输出对象个数
    return 0;
}
