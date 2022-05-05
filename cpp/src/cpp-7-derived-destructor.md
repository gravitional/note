# 派生类的析构函数

    析构函数不被继承, 派生类如果需要, 要自行声明析构函数.

    声明方法与无继承关系时类的析构函数相同.

    不需要显式地调用基类的析构函数, 系统会自动隐式调用.

    先执行派生类析构函数的函数体, 再调用基类的析构函数.

例7-5 派生类对象析构举例

```cpp
#include <iostream>
using namespace std;
class Base1 {
public:
    Base1(int i)
   { cout << "Constructing Base1 " << i << endl; }
    ~Base1() { cout << "Destructing Base1" << endl; }
};
class Base2 {
public:
    Base2(int j)
   { cout << "Constructing Base2 " << j << endl; }
    ~Base2() { cout << "Destructing Base2" << endl; }
};
class Base3 {
public:
    Base3() { cout << "Constructing Base3 *" << endl; }
    ~Base3() { cout << "Destructing Base3" << endl; }
};

class Derived: public Base2, public Base1, public Base3 {
public:
    Derived(int a, int b, int c, int d): Base1(a), member2(d), member1(c), Base2(b)
  { }
private:
    Base1 member1;
    Base2 member2;
    Base3 member3;
};

int main() {
    Derived obj(1, 2, 3, 4);
    return 0;
}
```
