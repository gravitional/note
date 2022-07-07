# 虚函数

问题: 还记得第7章的例子吗?
例7-3 类型转换规则举例

```cpp
#include <iostream>
using namespace std;
class Base1 { //基类Base1定义
public:
    void display() const {
        cout << "Base1::display()" << endl;
    }
};
class Base2: public Base1 { //公有派生类Base2定义
public:
    void display() const {
        cout << "Base2::display()" << endl;
    }
};
class Derived: public Base2 { //公有派生类Derived定义
public:
    void display() const {
        cout << "Derived::display()" << endl;
    }
};

void fun(Base1 *ptr) {  //参数为指向基类对象的指针
    ptr->display();     //"对象指针->成员名"
}
int main() {    //主函数
    Base1 base1;    //声明Base1类对象
    Base2 base2;    //声明Base2类对象
    Derived derived;    //声明Derived类对象

    fun(&base1);    //用Base1对象的指针调用fun函数
    fun(&base2);    //用Base2对象的指针调用fun函数
    fun(&derived);     //用Derived对象的指针调用fun函数

    return 0;
}
```

例8-4通过虚函数实现运行时多态

现在我们来改进一下第7章的程序

```cpp
#include <iostream>
using namespace std;

class Base1 {
public:
    virtual void display() const;  //虚函数
};
void Base1::display() const {
    cout << "Base1::display()" << endl;
}

class Base2::public Base1 {
public:
     virtual void display() const;
};
void Base2::display() const {
    cout << "Base2::display()" << endl;
}
class Derived: public Base2 {
public:
     virtual void display() const;
};
void Derived::display() const {
    cout << "Derived::display()" << endl;
}

void fun(Base1 *ptr) {
    ptr->display();
}

int main() {
    Base1 base1;
    Base2 base2;
    Derived derived;
    fun(&base1);
    fun(&base2);
    fun(&derived);
    return 0;
}
```

## 初识虚函数

+ 用virtual关键字说明的函数
+ 虚函数是实现运行时多态性基础
+ C++中的虚函数是动态绑定的函数

+ 虚函数必须是非静态的成员函数, 虚函数经过派生之后, 就可以实现运行过程中的多态.
+ 一般成员函数可以是虚函数
+ 构造函数不能是虚函数
+ 析构函数可以是虚函数

## 一般虚函数成员

+ 虚函数的声明

```cpp
virtual 函数类型 函数名(形参表);
```

+ 虚函数声明只能出现在类定义中的函数原型声明中, 而不能在成员函数实现的时候.
+ 在派生类中可以对基类中的成员函数进行覆盖.(在派生类中相同原型的函数)
+ 虚函数一般不声明为内联函数, 因为对虚函数的调用需要动态绑定, 而对内联函数的处理是静态的.

## virtual 关键字

+ 派生类可以不显式地用virtual声明虚函数, 这时系统就会用以下规则来判断派生类的一个函数成员是不是虚函数:
    + 该函数是否与基类的虚函数有相同的名称, 参数个数及对应参数类型;
    + 该函数是否与基类的虚函数有相同的返回值或者满足类型兼容规则的指针, 引用型的返回值;

+ 如果从名称, 参数及返回值三个方面检查之后, 派生类的函数满足上述条件, 就会自动确定为虚函数.
这时, 派生类的虚函数便覆盖了基类的虚函数.

+ 派生类中的虚函数还会隐藏基类中同名函数的 `所有其它重载形式`. (可使用 `类名::xx` 避免歧义)
+ 一般习惯于在派生类的函数中也使用 `virtual` 关键字, 以增加程序的可读性.
