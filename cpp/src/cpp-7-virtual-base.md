# 虚基类

+ 需要解决的问题

当派生类从多个基类派生, 而这些基类又共同基类, 则在访问此共同基类中的成员时, 
将产生冗余, 并有可能因冗余带来不一致性

+ 虚基类声明

以virtual说明基类继承方式
例: class B1:virtual public B

+ 作用
主要用来解决多继承时可能发生的对同一基类继承多次而产生的二义性问题
为最远的派生类提供唯一的基类成员, 而不重复产生多次复制

+ 注意:
在第一级继承时就要将共同基类设计为虚基类.

例7-8 虚基类举例

![virtual-base.png](http://sc0.ykt.io/ue_i/20200305/1235393518416039936.png)

```cpp
#include <iostream>
using namespace std;
class Base0 {
public:
    int var0;
    void fun0() { cout << "Member of Base0" << endl; }
};
class Base1: virtual public Base0 {
public:
    int var1;
};
class Base2: virtual public Base0 {
public:
    int var2;
};

class Derived: public Base1, public Base2 {
//定义派生类Derived
public:
    int var;
    void fun() {
        cout << "Member of Derived" << endl;
    }
};

int main() {
    Derived d;
    d.var0 = 2; //直接访问虚基类的数据成员
    d.fun0();     //直接访问虚基类的函数成员
    return 0;
}
```

## 虚基类及其派生类构造函数

`建立对象` 时所指定的类称为 `最远派生类`.

虚基类的成员是由 `最远派生类` 的构造函数通过调用虚基类的 `构造函数` 进行初始化的.

在整个继承结构中, 直接或间接继承 `虚基类` 的所有派生类, 
都必须在构造函数的成员初始化表中为 `虚基类的构造函数` 列出参数. 
如果未列出, 则表示调用该虚基类的默认构造函数.

在建立对象时, 只有 `最远派生类` 的构造函数调用虚基类的构造函数, 
其他类对虚基类构造函数的调用被忽略.

有虚基类时的构造函数举例(补7-4)

```cpp
#include <iostream>
using namespace std;

class Base0 {
public:
    Base0(int var) : var0(var) { }
    int var0;
    void fun0() { cout << "Member of Base0" << endl; }
};
class Base1: virtual public Base0 {
public:
    Base1(int var) : Base0(var) { }
    int var1;
};
class Base2: virtual public Base0 {
public:
    Base2(int var) : Base0(var) { }
    int var2;
};

class Derived: public Base1, public Base2 {
public:
    Derived(int var) : Base0(var), Base1(var), Base2(var)
   { }
    int var;
    void fun()
   { cout << "Member of Derived" << endl; }
};

int main() {    //程序主函数
    Derived d(1);
    d.var0 = 2; //直接访问虚基类的数据成员
    d.fun0();   //直接访问虚基类的函数成员
    return 0;
}
```
