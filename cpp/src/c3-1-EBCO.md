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

## ss

C++类常为 `空`, 这就意味着在运行期其内部表示不耗费任何内存.
这常见于只包含类型成员, 非虚成员函数和静态数据成员的类,
而非静态数据成员, 虚函数和虚基类会在运行期耗费内存

即使是空类, 其大小也不会为 `0`

```cpp
# include <iostream>

class EmptyClass{
};

class EmptyClass1{
    static  int i;
};
int EmptyClass1::i = 1;

class EmptyClass2{
    static  int i;
    void test(){};
};

class EmptyClass3{
    typedef int Int;
};

class NoEmptyClass{
    int i = 0;
};
int main(){
    printf("%lu\n", sizeof(EmptyClass));
    printf("%lu\n", sizeof(EmptyClass1));
    printf("%lu\n", sizeof(EmptyClass2));
    printf("%lu\n", sizeof(EmptyClass3));
    printf("%lu\n", sizeof(NoEmptyClass));
}
```

## 布局原则

C++的设计者不允许类的大小为0, 其原因有很多,
比如由它们构成的数组, 其大小必然也是 `0`, 这会导致指针运算中普遍使用的性质失效.
比如, 假设类型 `ZeroSizedT` 的大小为 `0`, 则下面的操作会出现错误:

```cpp
ZeroSizedT  z[10];
auto v =  &z[9]  - &z[2];  // 计算两个指针或者地址之间的距离
```

通常而言, 上面的差值, 一般是用两个地址之间的字节数除以类型大小而得到的, 而类型大小为 `0` 就不妙了.

虽然不存在 `0` 大小的类, 但这扇门也没有彻底关死.
C++规定, 当 `空类作为基类` 时不需要为其分配任何空间, 除非与 同类型的其他对象 或子对象 发生地址冲突.
这个就叫做 `空基类优化技术`(EBCO).

看个例子:

```cpp
# include <iostream>

class EmptyClass{
};

class EmptyTwo : public EmptyClass{

};
class EmptyThree : public EmptyTwo{

};

int main(){
    printf("%lu\n", sizeof(EmptyClass));
    printf("%lu\n", sizeof(EmptyTwo));
    printf("%lu\n", sizeof(EmptyThree));
}
```

如果编译器支持空基类优化, 上面程序的所有输出结果相同, 但是均不为0.
也就是说, 在类 EmptyTwo 中的类 EmptyClass没有分配空间. 如下图:

![img](https://img-blog.csdnimg.cn/20210423161547857.png)

如果不支持空基类优化, 上面程序的输出结果不同. 布局如下图:

![img2](https://img-blog.csdnimg.cn/2021042316171410.png)

再看个例子:

```cpp
# include <iostream>

class EmptyClass{
};

class EmptyTwo : public EmptyClass{

};
class NoEmpty :public EmptyClass,  public EmptyTwo{

};

int main(){
    printf("%lu\n", sizeof(EmptyClass));
    printf("%lu\n", sizeof(EmptyTwo));
    printf("%lu\n", sizeof(NoEmpty));
}
```

![img3](https://img-blog.csdnimg.cn/20210423162203338.png)

`NoEmpty` 为什么不为空类呢?这是因为 `NoEmpty` 的基类 `EmptyClass和` `EmptyTwo` 不能分配到同一地址空间,
否则 `EmptyFoo` 的基类 `EmptyClass` 和 `NoEmpty` 的 `EmptyClass` 会撞到同一地址空间上.
(发生地址冲突)

换句话说, 两个相同类型的子对象偏移量相同, 这是C++布局规则不允许的

![img3](https://img-blog.csdnimg.cn/20210423162203338.png)

对空基类优化进行限制的根据原因在于: *我们需要能比较两个指针是否指向同一对象*.
由于指针几乎总是用地址内部表示, 所以我们必须保证两个不同的地址(即两个不同的指针)对应两个不同的对象

## 成员作基类

对于数据成员, 则不存在类似空基类优化的技术, 否则遇到指向成员的指针时就会出现问题.
因此我们可以考虑将成员变量实现为(私有)基类的形式.

在模板中考虑这个问题非常有必要, 因为模板参数常常可能是空类;
但是对于一般情况, 我们并不能依赖这条规则(即模板参数常常可能是基类);
而且如果对某一个模板参数一无所知, 也不能很容易就实现空基类优化.
看个例子:

```cpp
template<typename T1, typename T2>
class MyClass{
 private:
  T1 a;
  T2 b;
};
```

模板参数T1和T2之一或全部, 都有可能是空类,
那 `MyClass<T1, T2>` 就不能得到最优布局, 每个这样的实例就可能会浪费一个字的内存.

把模板参数直接作为基类可以解决这个问题:

```cpp
template<typename T1, typename T2>
class MyClass : private T1, private T2{};
```

但是, 比如当T1和T2是int这样的基本类型时, 上面的做法很有问题;
另外, 当T2和T2类型相同时, 也会出问题
(这个问题可以用模板特化或者通过添加中间层进行继承的方法[示例: 模板的命名参数]解决);

还有一个很大的问题就是增加基类会改变接口;
还有一个问题就是继承模板参数甚至能影响到成员函数是否为虚.
显然, 引入EBCO会引来很多不必要的麻烦

如果一个已知的模板参数的类型必然是类, 该模板的另一个成员类型不是空类,
那么有一个方法更加可行, 大概想法是借助 `EBCO`, 把可能为空的类型参数与这个成员"合"起来, 比如对于:

```cpp
template<typename CustomClass>
class Optimizable{
 CustomClass info; // 可能为空
 void * storage;
};
```

可以将其改写为:

```cpp
template<typename CustomClass>
class Optimizable{
 private:
  BaseMemberPair<CustomClass, void *> info_and_storage;
}
```

虽然实现可能麻烦了, 但是性能可以显著提高
`BaseMemberPair` 的实现如下:

```cpp
template<typename Base, typename Member>
class BaseMemberPair : private Base{
private:
    Member member;
public:
   // 构造函数
    BaseMemberPair(Base const & b, Member const & m) : Base(b), member(m) {
    }

   // 通过first()来访问基类数据
    Base const & first() const {
        return (Base const  &) *this;
    }
    Base & first()  {
        return (Base   &) *this;
    }

 // 通过second()来访问基类的成员变量
    Member const & second() const {
        return this->member;
    }
    Member & second()  {
        return this->member;
    }
};
```

封装在 `BaseMemberPair` 中的数据成员(其存储方式在Base为空时可得到优化),
需要通过成员函数 `first` 和 `second` 访问
