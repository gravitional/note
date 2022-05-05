# 虚析构函数

为什么需要虚析构函数?

- 可能通过基类指针删除派生类对象;
- 如果你打算允许其他人通过基类指针调用对象的析构函数(通过delete这样做是正常的),
- 就需要让基类的析构函数成为虚函数, 否则执行delete的结果是不确定的.

## 不使用虚析构函数的例子

源代码:

```cpp
#include <iostream>
using namespace std;
class Base {
public:
    ~Base(); //不是虚函数
};
Base::~Base() {
    cout<< "Base destructor" << endl;
 }

class Derived: public Base{
public:
    Derived();
    ~Derived(); //不是虚函数
private:
    int *p;
};

// 使用
Derived::Derived(){
    p=new int(0);
}

Derived::~Derived(){
    cout<<"Derived destrutor" <<endl;
delete p;
}

void fun(Base* b){
    delete b;
}

int main(){
    Base *b = new Derived();
    fun(b);
    return 0;
}
```

## 使用虚析构函数

```cpp
#include <iostream>
using namespace std;
class Base {
public:
    virtual ~Base();
};
Base::~Base() {
    cout<< "Base destructor" << endl;
 }
class Derived: public Base{
public:
    Derived();
    virtual ~Derived();
private:
    int *p;
};
```
