# ::作用域符解析

[c++入门学习篇(1)之::作用域符解析](https://zhuanlan.zhihu.com/p/137383328)

在 `C++` 中, 作用域运算符为 `::`. 它用于以下目的.

## 当存在具有相同名称的局部变量时, 访问全局变量

```cpp
// C++ program to show that we can access a global variable
// using scope resolution operator :: when there is a local
// variable with same name
#include<iostream>
using namespace std;

int x;  // Global x

int main()
{
  int x = 10; // Local x
  cout << "Value of global x is " << ::x;
  cout << "\nValue of local x is " << x;
  return 0;
}
```

输出:

全局x的值为0
本地x的值为10

## 在类之外定义函数

```cpp
// C++ program to show that scope resolution operator :: is used
// to define a function outside a class
#include<iostream>
using namespace std;

class A
{
public:

   // Only declaration
   void fun();
};

// Definition outside class using ::
void A::fun()
{
   cout << "fun() called";
}

int main()
{
   A a;
   a.fun();
   return 0;
}
```

输出:

```bash
fun() called
```

## 访问类的静态变量

```cpp
// C++ program to show that :: can be used to access static
// members when there is a local variable with same name
#include<iostream>
using namespace std;

class Test
{
    static int x;
public:
    static int y;

    // Local parameter 'a' hides class member
    // 'a', but we can access it using ::
    void func(int x)
    {
       // We can access class's static variable
       // even if there is a local variable
       cout << "Value of static x is " << Test::x;

       cout << "\nValue of local x is " << x;
    }
};

// In C++, static members must be explicitly defined
// like this
int Test::x = 1;
int Test::y = 2;

int main()
{
    Test obj;
    int x = 3 ;
    obj.func(x);

    cout << "\nTest::y = " << Test::y;

    return 0;
}
```

输出:

静态x的值为1
本地x的值为3
测试:: y = 2

## 如果有多个继承

如果两个祖先类中存在相同的变量名, 则可以使用作用域运算符进行区分.

```cpp
// Use of scope resolution operator in multiple inheritance.
#include<iostream>
using namespace std;

class A
{
protected:
    int x;
public:
    A() { x = 10; }
};

class B
{
protected:
    int x;
public:
    B() { x = 20; }
};

class C: public A, public B
{
public:
   void fun()
   {
      cout << "A's x is " << A::x;
      cout << "\nB's x is " << B::x;
   }
};

int main()
{
    C c;
    c.fun();
    return 0;
}
```

输出:

A的x是10
B的x是20

## 对于命名空间

如果两个命名空间中都存在一个具有相同名称的类,
则可以将名称空间名称与作用域解析运算符一起使用, 以引用该类而不会发生任何冲突

```cpp
// Use of scope resolution operator for namespace.
#include<iostream>
int main(){
    std::cout << "Hello" << std::endl;
}
```

在这里, `cout` 和 `endl` 属于 `std` 命名空间.

## 在另一个类中引用一个类

如果另一个类中存在一个类, 我们可以嵌套使用作用域运算符

```cpp
// Use of scope resolution class inside another class.
#include<iostream>
using namespace std;

class outside
{
public:
      int x;
      class inside
      {
      public:
            int x;
            static int y;
            int foo();

      };
};
int outside::inside::y = 5;

int main(){
    outside A;
    outside::inside B;

}
```
