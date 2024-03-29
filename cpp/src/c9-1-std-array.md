# cpp note

## STL array

p423; STL 顺序容器的 array 类型

```cpp
array<int, 10>arr; // arr 为保存10 int 类型的数组
array<string, 20>astr; // astr 为保存 20  个string 类型的数组
```

大小(size) 是 array 类型的一部分, 因此不支持普通容器的构造函数;
默认构造的 array 是非空的, 拥有 size 个元素, 元素被默认初始化.

虽然不能对 内置数组 类型进行复制 或 对象赋值操作, 但 array 并无此限制.

## C++ 循环

p401; `for ` 循环形式 1

```cpp
for (auto& si:s)
    ci>>si;
transform(s.begin(), s.end(), ostream_iterator<int>(cout, " "), negate<int>());
```

p408; `for ` 循环形式 2

```cpp
template<class T, class Input It, class OutputIt>
void mySort(InputIt first, InputIt last, OutputIt result){
    vector<T>s;
    for(; first!=last; ++first)
    s.push_back(*first);
}
```

p426; `for ` 循环形式 3

```cpp
for(auto iter=str.begin(); iter!=str.end(); ++iter){
    s.push(* iter);
    while (!s.empty()){
    cout<<s.top();
    s.pop();
    }
}
```

## 基类虚函数

p317; 一般 `虚函数成员` 的声明语法是:

```cpp
virtual     函数类型    函数名(形参表);
```

```cpp
#include <iostream>
using namespace std;
class Base1 {//基类 Base1 定义
public:
    virtual void display() const; // 虚函数
};

void Base1::display() const{
    cout << "Base1::display()" << endl;
}

class Base2 : public Base1 {// 公有派生类 Base2 定义
public:
    virtual void display() const; // 覆盖基类的虚函数
};

void Base2::display() const{
    cout << "Base2::display()" << endl;
}

class Derived : public Base2 {// 公有派生类 Base2 定义
public:
    virtual void display() const; // 覆盖基类的虚函数
};

void DerivedBase2::display() const{
    cout << "Derived::display()" << endl;
}

void fun(Base1 *ptr) {// 参数为指向基类对象的指针
    ptr->display(); //"对象指针->成员名"
}
```

程序中使用对象指针来访问 函数成员, 这样绑定过程就是在 运行中完成, 实现了运行中的多态.

+ p342; 每个 `多态类型`(有虚函数的类型)的对象中,
都保存有一个指向 `虚表` 首地址的指针--`虚表指针`(vptr).

+ `虚表` 中保存了 指向虚函数的指针,  对于每个多态类型只有一个 `虚表`,
这一部分空间不会因为新对象的创建而有所增加.

### 纯虚函数

在 UML 中, `纯虚函数` 也称为抽象函数.

```cpp
virtual 函数类型 函数名(参数表)=0;
```

声明为纯虚函数之后, 基类中就可以不再给出函数的实现部分,
`纯虚函数` 的函数体由派生类给出.

>基类中仍然允许对纯虚函数给出实现,
>但即使给出, 也必须由派生类覆盖, 否则无法实例化.

对基类中纯虚函数定义的函数体的调用, 必须通过 `基类名: : 函数名(参数表)` 的形式.
如果将析构函数声明为 `纯虚函数`, 必须给出它的实现,
因为 `派生类` 的 析构函数 调用完后, 需要调用 `基类` 的纯虚函数.

`纯虚函数` 不同于 函数体为空的 `虚函数`:

+ 前者根本没有函数体, 后者函数体为空.
+ 前者不可以实例化, 后者可以实例化.
+ 相同点: 它们都可以派生出新的类, 在新类中覆盖虚函数的实现, 从而实现 `运行时多态`.
