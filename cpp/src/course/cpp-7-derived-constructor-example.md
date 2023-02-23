# 派生类构造函数举例

例7-4 派生类构造函数举例

```cpp
#include <iostream>
using namespace std;
class Base1 {//基类Base1, 构造函数有参数
public:
    Base1(int i)
  { cout << "Constructing Base1 " << i << endl; }
};
class Base2 {//基类Base2, 构造函数有参数
public:
    Base2(int j)
  { cout << "Constructing Base2 " << j << endl; }
};
class Base3 {//基类Base3, 构造函数无参数
public:
    Base3()
  { cout << "Constructing Base3 *" << endl; }
};

class Derived: public Base2, public Base1, public Base3 {
public:
    Derived(int a, int b, int c, int d): Base1(a), member2(d), member1(c), Base2(b)
  //此处的次序与构造函数的执行次序无关
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
