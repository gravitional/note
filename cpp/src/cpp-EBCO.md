# c++ 空基类优化

[STL中的空基类优化](https://zhuanlan.zhihu.com/p/259299672)
[C/C++编程: 空基类优化](https://blog.csdn.net/zhizhengguan/article/details/116059812)
[C++ 空白基类最优化](https://blog.csdn.net/buxizhizhou530/article/details/45888203)
[c++空白基类最优化](https://blog.csdn.net/luckyxiaoqiang/article/details/8494235)

对于 `c++` 中的一个空类

```cpp
class X {};
```

事实上并不是空的, `sizeof(X)` 并不等于 `0` ,  一般的结果是 `1`.
每个 `X` 的对象都有一个隐晦的 `1 bytes`, 是被编译器安插进去的一个 `char`,
这样可以使得这个 `class` 的两个 `objects`, 在内存中配置独一无二的地址.

当 `X` 作为另一个类的成员时, 如:

```cpp
class A
{
public:
    X x;
    int a;
};
```

由于 `X` 占一个字节, `int` 占 `4` 个字节,
再加上编译器的 `alignment`(齐位) 调整, `sizeof(Y) = 8`.

但是当一个类继承 `X` 时:

```cpp
class Y : public X
{
public:
    int a;
};
```

这时大部分编译器对于 `sizeof(Y)` 的结果是 `4`, 而不是 `8`.
这就是所谓的 `空白基类最优化`(empty base optimization-EBO 或 empty base class opimization-EBCO).
在 `空基类` 被继承后由于没有任何数据成员, 所以 `子类` 优化掉 `基类` 所占的 `1 byte`.
`EBO` 并不是c++标准所规定必须的, 但是大部分编译器都会这么做.

下面是做的测试

```cpp
#include<iostream>
using namespace std;

class Empty{};
class Son1:public Empty{};
class Component
{
    int x;  Empty t;
};
class Son2:public Empty{ int x;};

int main()
{
    Empty t;
    //1, 被编译器安插进去的一个char,
    //这样可以使得这个class的两个objects在内存中配置独一无二的地址
    printf("size of Empty: %d\n",sizeof(Empty));
    printf("size of t: %d\n",sizeof(t));        //1
    printf("size of Son1: %d\n",sizeof(Son1));  //1
    printf("size of Component: %d\n",sizeof(Component));  //8, 字节对齐
    printf("size of Son2: %d\n",sizeof(Son2));  //4, 空白基类最优化
    system("pause"); return 0;
}
```

>有人可能会问是否需要考虑this指针?

我觉得是不需要考虑的. 首先看一下this指针的定义:
`this 指针` 是作为参数传递给每个成员变量的.
它是指向 `本类对象` 的指针, 它的值是当前 `被调用的成员函数` 所在的对象的 `起始地址`.
也就是说 `this指针` 是不存在于成员变量中的.

## 空类, empty class

[空类(empty class)](https://www.jianshu.com/p/ff9eb9c381c1)
