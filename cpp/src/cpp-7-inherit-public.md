# 继承方式简介及公有继承

不同继承方式的影响主要体现在:

    派生类成员对基类成员的访问权限

    通过派生类对象对基类成员的访问权限

## 三种继承方式

    公有继承

    私有继承

    保护继承

公有继承(public)

    继承的访问控制

        基类的public和protected成员: 访问属性在派生类中保持不变;

        基类的private成员: 不可直接访问.

    访问权限

        派生类中的成员函数: 可以直接访问基类中的public和protected成员, 但不能直接访问基类的private成员;

        通过派生类的对象: 只能访问public成员.

例7-1 公有继承举例

+ Point.h

```cpp
#ifndef _POINT_H
#define _POINT_H
class Point {
    //基类Point类的定义
    public:
    //公有函数成员
        void initPoint(float x = 0, float y = 0){
            this->x = x;
            this->y = y;
        }
        void move(float offX, float offY){
            x += offX;
            y += offY;
        }
        float getX() const { return x; }
        float getY() const { return y; }
    private:
    //私有数据成员
        float x, y;
};
#endif //_POINT_H
```

Rectangle.h

```cpp
#ifndef _RECTANGLE_H
#define _RECTANGLE_H
#include "Point.h"
class Rectangle: public Point {
//派生类定义部分
public:
//新增公有函数成员
    void initRectangle(float x, float y, float w, float h) {
        initPoint(x, y);              //调用基类公有成员函数
        this->w = w;
        this->h = h;
    }
    float getH() const { return h; }
    float getW() const { return w; }
private:
//新增私有数据成员
    float w, h;
};
#endif //_RECTANGLE_H
```

+ main.cpp

```cpp
#include <iostream>
#include <cmath>
using namespace std;
#include "Rectangle.h"
int main() {
    Rectangle rect; //定义Rectangle类的对象
    //设置矩形的数据
    rect.initRectangle(2, 3, 20, 10);
    rect.move(3,2); //移动矩形位置
    cout << "The data of rect(x,y,w,h): " << endl;
    //输出矩形的特征参数
    cout << rect.getX() <<", "
        << rect.getY() << ", "
        << rect.getW() << ", "
        << rect.getH() << endl;
    return 0;
}
```