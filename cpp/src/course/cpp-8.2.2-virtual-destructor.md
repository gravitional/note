# 虚析构函数

为什么需要虚析构函数?

- 可能通过基类指针删除派生类对象;
- 如果你打算允许其他人通过基类指针调用对象的析构函数(通过delete这样做是正常的),
- 就需要让基类的析构函数成为虚函数, 否则执行delete的结果是不确定的.

## virtual 析构函数

[C++基/抽象类的构造/析构(纯)虚函数](https://www.cnblogs.com/depend-wind/articles/12256533.html)

析构函数可定义为纯虚函数, 但也必须给出函数定义
Effective C++ 条歀07: 为多态基类声明virtual析构函数(Declare destructors virtual in polymorphic base classes)

在某些类里声明纯虚析构函数很方便.
`纯虚函数` 将产生 `抽象类`——不能实例化的类(即不能创建此类型的对象).
有些时候, 你想使一个类成为抽象类, 但刚好又没有任何纯虚函数.
怎么办?因为抽象类是准备被用做基类的, 基类必须要有一个虚析构函数,
`纯虚函数` 会产生抽象类, 所以方法很简单: 在想要成为抽象类的类里声明一个 `纯虚析构函数`.
这里是一个例子:

```cpp
class awov {
public:
    virtual ~awov() = 0; // 声明一个纯虚析构函数
};
```

这个类有一个纯虚函数, 所以它是抽象的, 而且它有一个虚析构函数, 所以不会有部分析构的问题.
但这里还有一件事: 必须提供纯虚析构函数的定义:

```cpp
//在 .cpp 文件中
awov::~awov() {  ...  } // 纯虚析构函数的定义
```

这个定义是必需的, 因为虚析构函数工作的方式是:
最底层的派生类的析构函数最先被调用, 然后各个基类的析构函数被调用.
这就是说, 即使是抽象类, 编译器也要产生对~awov的调用, 所以要保证为它提供函数体.
如果不这么做, 链接器就会检测出来, 最后还是得回去把它添上

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
