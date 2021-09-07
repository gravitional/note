#include <iostream>
using namespace std;

class Point //Point 类定义
{

public: //外部接口
    Point(int x = 0, int y = 0) : x(x), y(y)
    {            //构造函数
        count++; //在构造函数中对 count 累加，所有对象共同维护同一个count
    };
    Point(const Point &p);
    ~Point() { count--; }
    int getX() { return x; }
    int getY() { return y; }
    static void showCount()
    { // 输出静态数据成员
        cout << " Object count: " << count << endl;
    }

private:
    int x, y;
    static int count; //静态数据成员声明，用于记录点的个数。
};