# 私有继承和保护继承

## 私有继承(private)

继承的访问控制
    基类的public和protected成员: 都以private身份出现在派生类中;
    基类的private成员: 不可直接访问.

访问权限

    派生类中的成员函数: 可以直接访问基类中的public和protected成员, 但不能直接访问基类的private成员;
    通过派生类的对象: 不能直接访问从基类继承的任何成员.

例7-2 私有继承举例

Point.h

```cpp
#ifndef _POINT_H
#define _POINT_H
class Point {   //基类Point类的定义
public: //公有函数成员
    void initPoint(float x = 0, float y = 0)
  { this->x = x; this->y = y;}
    void move(float offX, float offY)
  { x += offX; y += offY; }
    float getX() const { return x; }
    float getY() const { return y; }
private:    //私有数据成员
    float x, y;
};
#endif //_POINT_H
```

Rectangle.h

```cpp
#ifndef _RECTANGLE_H
#define _RECTANGLE_H
#include "Point.h"
class Rectangle: private Point {    //派生类定义部分
public: //新增公有函数成员
    void initRectangle(float x, float y, float w, float h) {
        initPoint(x, y); //调用基类公有成员函数
        this->w = w;
        this->h = h;
    }
    void move(float offX, float offY) {   Point::move(offX, offY);  }
    float getX() const { return Point::getX(); }
    float getY() const { return Point::getY(); }
    float getH() const { return h; }
    float getW() const { return w; }
private:    //新增私有数据成员
    float w, h;
};
#endif //_RECTANGLE_H
```

main.cpp

```cpp
#include <iostream>
#include <cmath>
using namespace std;

int main() {
    Rectangle rect; //定义Rectangle类的对象
    rect.initRectangle(2, 3, 20, 10);   //设置矩形的数据
    rect.move(3,2); //移动矩形位置
    cout << "The data of rect(x,y,w,h): " << endl;
    cout << rect.getX() <<", "  //输出矩形的特征参数
        << rect.getY() << ", "
        << rect.getW() << ", "
        << rect.getH() << endl;
    return 0;
}
```

## 保护继承(protected)

    继承的访问控制

        基类的public和protected成员: 都以protected身份出现在派生类中;

        基类的private成员: 不可直接访问.

    访问权限

        派生类中的成员函数: 可以直接访问基类中的public和protected成员, 但不能直接访问基类的private成员;

        通过派生类的对象: 不能直接访问从基类继承的任何成员.

    protected 成员的特点与作用

        对建立其所在类对象的模块来说, 它与 private 成员的性质相同.

        对于其派生类来说, 它与 public 成员的性质相同.

        既实现了数据隐藏, 又方便继承, 实现代码重用.

        如果派生类有多个基类, 也就是多继承时, 可以用不同的方式继承每个基类.

### protected 成员举例(补7-1)

```cpp
class A {
protected:
    int x;
};

int main() {
    A a;
    a.x = 5;//错误
}
class A {
protected:
    int x;
};
class B: public A{
public:
    void function();
};
void B:function() {
    x = 5;   //正确
}
```

### 多继承举例(补7-2)

```cpp
class A {
public:
    void setA(int);
    void showA() const;
private:
    int a;
};
class B {
public:
    void setB(int);
    void showB() const;
private:
    int b;
};
class C : public A, private B {
public:
    void setC(int, int, int);
    void showC() const;
private:
    int c;
};
```

## 多继承举例

```cpp
void  A::setA(int x) {
    a=x;
}
void B::setB(int x) {
    b=x;
}
void C::setC(int x, int y, int z) {
    //派生类成员直接访问基类的
    //公有成员
    setA(x);
    setB(y);
    c = z;
}

//其他函数实现略

int main() {
    C obj;
    obj.setA(5);
    obj.showA();
    obj.setC(6,7,9);
    obj.showC();
// obj.setB(6);  错误
// obj.showB(); 错误
    return 0;
}
```
