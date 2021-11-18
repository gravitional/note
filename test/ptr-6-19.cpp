#include <iostream>
#include <cassert>
using namespace std;

class Point
{
public:
    Point() : x(0), y(0)
    {
        cout << "Default Constructor called" << endl;
    }
    Point(int x, int y) : x(x), y(y)
    {
        cout << "Constructor called." << endl;
    }
    ~Point() { cout << "Destructor called." << endl; }
    int getX() const { return x; }
    int getY() const { return y; }
    void move(int newX, int newY)
    {
        x = newX;
        y = newY;
    }

private:
    int x, y;
};

class ArrayOfPoints
{ // 动态数组类
public:
    ArrayOfPoints(int size) : size(size)
    {
        points = new Point[size]; // 在构造函数中分配内存
    }
    ~ArrayOfPoints()
    {
        cout << "Deleting ..." << endl;
        delete[] points; //在析构函数中，用 delete 释放内存
    }
    Point &element(int index)// 返回值为引用类型, 我们访问数组元素，希望它是左值, 也就是 = 左边的值.
    {
        assert(index >= 0 && index < size);
        return points[index];
    }

private:
    Point *points; //指向动态数组首地址。
    int size;      //数组大小
};

int main()
{
    int count;
    cout << "Please enter the count of points: " << endl;
    cin >> count;
    ArrayOfPoints points(count);    // 创建数组对象
    points.element(0).move(5, 0);   //访问数组元素的成员
    points.element(1).move(15, 20); //访问数组元素的成员
    return 0;
}