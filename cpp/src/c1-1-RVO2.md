# 返回临时对象

[不能返回局部/临时变量的引用吗?](https://zhuanlan.zhihu.com/p/504301231)

## 问题

写C++的时候, 大家经常准守一条原则: 不返回局部变量/临时变量的 `指针` 或 `引用`,
没毛病, 局部/临时变量所在空间会随着函数调用, 代码块等离开作用域时自动销毁.
然而, 如果是返回局部/临时变量的常引用是否是可行的呢?

## 前置知识

为了对这个问题进行详细描述, 我们先介绍一些需要提前了解的知识

### RVO和NRVO

[什么是完整的RVO以及NRVO过程?](https://www.zhihu.com/question/48964719/answer/2401048658)

在没有RVO以及NRVO优化的情况下, 返回一个对象的值, 然后用来初始化一个新的对象,
如下文中的Test1代码所示, 会先用 `局部对象` 拷贝构造 `临时对象`(函数返回值),
然后再用 `临时对象` 拷贝构造 `main` 中的对象.

整个过程中会创建和销毁多个对象, 开销较大.
在 `g++` 命令上加上 `-fno-elide-constructors` 参数可以禁止RVO和NRVO.

`RVO`(Return Value Optimization)称为(非具名) `返回值优化`,
个人理解: 相比没有优化的版本, RVO会优化掉, 使用函数中的 `非具名返回对象` 拷贝构造 `临时对象` 这一步骤,
实际实现中将需要创建的对象引用传递进来进行构建

`NRVO`(Named Return Value Optimization)称为 `具名返回值优化`.
个人理解: 相比RVO, NRVO可以更进一步, 直接使用 main 中的对象 `obj`, 代替函数内的具名对象,
整个创建过程中只使用main中的对象

在这个TestObj我们给默认构造函数, 还有拷贝构造函数, 以及析构函数都加上的输出,
用来比较清晰的了解对象的创建, 销毁与拷贝过程.

[test1,test2](./c1-1-RVO.cpp)

Test1代码很简单, 我们创建了一个func函数用来返回一个局部对象的拷贝(注意这里不是引用).
然后将这个对象赋值给main中的obj,
我们使用一下两条命令分别编译并执行得到的结果如下所示
(千万不要使用cmake加禁止优化参数, 貌似cmake会加其他参数有一定的优化, 就不是下面的结果了).
Test1和Test2主要的差别是, Test2在func内建立了一个具名的对象.

```bash
# (1)
g++ Test1.cpp -o Test1 && ./Test1
# (2)
g++ Test1.cpp -fno-elide-constructors -o Test1 && ./Test1
# (3)
g++ Test2.cpp -o Test2 && ./Test2

# (1)执行结果
default constructor
destructor

# (2)执行结果
default constructor
copy constructor
destructor
copy constructor
destructor
destructor

# (3)执行结果
default constructor
destructor
```

先解释一下(2)中的结果:

```c
default constructor //func内局部对象默认构造函数调用
copy constructor    // 局部对象拷贝到临时对象的拷贝构造调用
destructor          // 局部对象析构
copy constructor    // 临时对象拷贝到main中对象的拷贝构造调用
destructor          // 临时对象析构
destructor         // main中对象析构
```

符合非优化情况下的逻辑.

(1)和(3)按道理应该(1)会多一次func内局部对象的构造和析构,
不知道是不是少加了什么参数(有机会还是看看汇编最好);
这里(1)(3)应该是进行了NRVO优化,
即: 将main中的对象放到func内部进行构造(不会再创建局部和临时对象)

## 返回局部或临时对象的引用

通常局部或临时对象在函数调用完之后会被进行销毁, 所以返回其引用会导致段错误(如果是指针会是随机值)

[test3](./c1-1-RVO.cpp): 局部对象引用. 结果:

```bash
$ g++ main.cpp -o main && ./main
main.cpp: In function 'TestObj& func()':
main.cpp:26:13: warning: reference to local variable 't' returned [-Wreturn-local-addr]
     TestObj t;
             ^
default constructor
destructor
Segmentation fault
```

结果发生了段错误.

[test3](./c1-1-RVO.cpp): 临时对象引用. 直接发生编译错误

```bash
 error: invalid initialization of non-const reference of type 'TestObj&' from an rvalue of type 'TestObj'
     TestObj &t = func();
```

## 返回常引用

常引用即: const T&和普通引用(左值引用)最大的不同是,
常引用可以绑定到所有的左值, 右值. 这里先不谈论右值引用. 看下面四种情况

```cpp
int a=1;         //定义一个int变量a
int &b1=a;       // 将引用b1绑定到a上
const int &b2=a; // 将常引用b2绑定到a上
int &c1=1;       // 将引用c1绑定到常量1上, 这里会报错的!!!!!!!
const int &c2=1; //将常引用c2绑定到常量1上, ok
```

左值引用和常引用的主要区别是, 左值引用必须绑定到一个可以具有地址的变量上,
像常量, 表达式, 函数返回值等等可能存在寄存器上, 不能作为左值引用.
常引用使用绑定到所有的变量上.

[局部变量常引用](./c1-1-RVO.cpp). 结果:

```bash
     TestObj t;
             ^
default constructor
destructor
Segmentation fault
```

[临时变量常引用1](./c1-1-RVO.cpp). 结果:

```bash
default constructor
1
1
destructor
```

[临时变量常引用2](./c1-1-RVO.cpp). 结果:

```bash
default constructor
destructor
1
1
```

综上结果可以看到, `常引用` 是可以绑定到 `临时值` 的(`函数返回临时值` 或者 `传参的临时值`),
换一种说法就是: 常引用 可以延长临时变量的生命周期.
而 `局部变量` 是不能被绑定到引用或者常引用的.

## 结论

不能返回 `局部变量` 的引用和常引用(段错误)
可以返回 `临时变量`(传参或者函数返回)的 `常引用`(常引用可以延长临时变量的生命周期)
