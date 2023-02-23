# 访问从基类继承的成员

## 作用域限定

当派生类与基类中有相同成员时:

    若未特别限定, 则通过派生类对象使用的是派生类中的同名成员.

    如要通过派生类对象访问基类中被隐藏的同名成员, 应使用基类名和作用域操作符(::)来限定.

例7-6 多继承同名隐藏举例

```cpp
#include <iostream>
using namespace std;
class Base1 {
public:
    int var;
    void fun() { cout << "Member of Base1" << endl; }
};
class Base2 {
public:
    int var;
    void fun() { cout << "Member of Base2" << endl; }
};
class Derived: public Base1, public Base2 {
public:
    int var;
    void fun() { cout << "Member of Derived" << endl; }
};

int main() {
    Derived d;
    Derived *p = &d;

  //访问Derived类成员
    d.var = 1;
    d.fun();

    //访问Base1基类成员
    d.Base1::var = 2;
    d.Base1::fun();

    //访问Base2基类成员
    p->Base2::var = 3;
    p->Base2::fun();

    return 0;
}
```

## 二义性问题

如果从不同基类继承了同名成员, 但是在派生类中没有定义同名成员, "派生类对象名或引用名.成员名", "派生类指针->成员名"访问成员存在二义性问题

解决方式: 用类名限定

二义性问题举例

class A {
public:
    void  f();
};
class B {
public:
    void f();
    void g()
};
class C: public A, piblic B {
public:
    void g();
    void h();
};

如果定义: C  c1;
则 c1.f() 具有二义性
而 c1.g() 无二义性(同名隐藏)
例7-7  多继承时的二义性和冗余问题

```cpp
//7_7.cpp
#include <iostream>
using namespace std;
class Base0 {   //定义基类Base0
public:
    int var0;
    void fun0() { cout << "Member of Base0" << endl; }
};
class Base1: public Base0 { //定义派生类Base1
public: //新增外部接口
    int var1;
};
class Base2: public Base0 { //定义派生类Base2
public: //新增外部接口
    int var2;
};

例7-7  多继承时的二义性和冗余问题
class Derived: public Base1, public Base2 {
public:
    int var;
    void fun()
  { cout << "Member of Derived" << endl; }
};

int main() {    //程序主函数
    Derived d;
    d.Base1::var0 = 2;
    d.Base1::fun0();
    d.Base2::var0 = 3;
    d.Base2::fun0();
    return 0;
}
```

![Derived类对象d的存储结构示意图](http://studio-tsinghua.xuetangx.com/asset-v1:TsinghuaX+00740043-91-20202+2020_T2+type@asset+block@ambigous.png)
