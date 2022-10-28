# c++ typedef

[C++ typedef的详细用法](https://zhuanlan.zhihu.com/p/413574268)
[C++typedef的详细用法](https://blog.csdn.net/hai008007/article/details/80651886)
[typedef和#define 的区别](https://www.zhihu.com/question/29798061/answer/144423125)

搞懂了c++创始人写的 `<the design and evolution of cpp>` 中的下面这个例子, 有助于你理解 `typdef`:

```cpp
typedef int P();
typedef int Q();
class X {
    static P(Q); // 等价于`static int Q()`, Q 在此作用域中不再是一个类型
    static Q(P); // 等价于`static int Q(int ())`, 定义了一个名为Q的function
};
```

这是一个极好的例子, 先问一下 `typedef int P()` 到底做了什么? 其实是:
定义了 函数type `P`, 参数为空, 返回值为 `int`

### 官方定义

初次接触此类 `typedef` 用法的程序员直观上理解这个例子比较困难,
我们来看一下 `typedef` 的官方定义:

>`Typedef` 的语法并不是 `typedef [type] [new name]`, `[new name]`部分并不总是出现在末尾.
>应该如此看待: 如果 `[some declaration]` 声明一个 `变量`, 则`typedef [same declaration]` 将定义一个 `类型`.

### 隐藏技能

`typedef` 定义的新类型, 使用时可以省略括号.
什么意思?

```cpp
typedef int NUM;
NUM a = 10; // 也可写成`NUM(a) = 10;`
NUM(b) = 12; // 也可写成`NUM b = 12;`
```

### 举例

先从初级的开始:

+ 整形

```cpp
typedef int x; // 定义了一个名为x的int类型
```

+ 结构体

```cpp
typedef struct { char c; } s; // 定义名为s的struct类型
```

+ 指针

```cpp
typedef int* p; //定义了一个名为p的指针类型, 它指向int
```

### 接下来是高级的

注意标识符不一定在最后:

+ 数组

```cpp
typedef int A[];  // 定义名为A的类型, 实际是int的数组
```

+ 函数

```cpp
typedef int f(); // 定义一个名为f, 参数为空, 返回值为int的函数类型
typedef int g(int); // 定义一个名为g, 含一个int参数, 返回值为int行的函数类型
```

现在回过头看:

```cpp
typedef int P();
static P(Q);
```

应该就比较好理解了,  `P` 是新定义的 `function` 类型, 它返回值为 `int`, 无参数.
根据说明2, `P(Q);` 实际上等价于 `P Q`,
声明 `Q` 是返回值为 `int`, 无参数的函数.

### 这玩意有什么用呢?

我们都知道C++语言里, 函数都是 `先声明后使用的`(除非在使用之前定义), 看以下例子:

```cpp

#include <iostream>
#include <stdio.h>
#include <string>

typedef int P();                                                                                 // 简单的
typedef void Q(int *p, const std::string &s1, const std::string &s2, size_t size, bool is_true); // 复杂的
class X
{
public:
    P(eat_shit); // 等价于声明`int eat_shit();`
    Q(bullshit); // 等价于声明`void bullshit(int *p, const string& s1, const string& s2, size_t size, bool is_true);`
};

int main()
{
    X *xx;
    printf("shit ret: %d\n", xx->eat_shit());
    int a[] = {1, 3, 4, 5, 7};
    xx->bullshit(a, "foo", "bar", sizeof(a) / sizeof(int), true);
}

int X::eat_shit()
{
    return 888;
}

void X::bullshit(
    int *p,
    const std::string &s1, const std::string &s2,
    size_t size, bool is_true)
{
    std::cout
        << "s1: " << s1 << ", s2: " << s2 << ", size: " << size << std::endl;
    printf("elems:\n");
    for (int i = 0; i < size; i++)
    {
        printf("%d %s", *p++, (i == size - 1) ? "" : ",");
    }
    printf("\n");
}
```

## typedef的四种用法

### 定义类型别名

```cpp
char *pa, *pb;
//char* pa,pb;

typedef char* pCHAR;
pCHAR pa,pb;
```

上面是声明两个指向字符变量的指针的两种方法,
普通的写法显然没有使用 `typedef` 的形式更方便和直观,
而且同时声明多个指针变量容易漏写星号.

### typedef struct

在c语言中 `typedef struct` 定义结构名, 在声明时可以省略 `struct` 关键字.
声明 `struct` 新对象时, 必须带上 `struct`, 即: `struct [结构名] [对象名]`

```c
struct A
{
    int x;
    int y;
}
struct A a;
```

而使用typedef之后可以直接写为: `[结构名] [对象名]`.

```c
typedef struct B
{
    int x;
    int y;
}pB;
pB b;
```

而在C++中定义结构体无需 `typedef`, 如下

```cpp
struct A{
    int m;
}
A a;
```

而 `C++` 中无 `typedef` 时, 在末尾定义的是变量,
可以直接使用它对结构中的成员变量进行赋值;
而有 `typedef` 时, 在末尾定义的是结构类型,
相当于为 `struct` 定义的结构类型换了一个新的名字.
使用时, 需要先定义变量, 然后对结构中的成员变量进行赋值.

```cpp
struct Teacher
{
    int age;
}Tea;  //Tea是一个变量
​
typedef struct Student
{
    int age;
}Stu;  //Stu是一个结构体类型 = struct Student
​
void main()
{
    Tea.age = 30;  //为结构成员赋值
    Stu Zhang;   //先声明结构类型变量
    Zhang.age = 15;   //访问结构成员并赋值
}
```

### 定义与平台无关的数据类型

比如定义一个叫 `REAL` 的浮点类型, 在目标平台一上, 让它表示最高精度的类型为:

```cpp
typedef long double REAL;
//在不支持 long double 的平台二上, 改为:
typedef double REAL;
//在连 double 都不支持的平台三上, 改为:
typedef float REAL;
```

也就是说, 当跨平台时, 只要改下 `typedef` 本身就行, 不用对其他源码做任何修改.
标准库就广泛使用了这个技巧, 比如 `size_t`.
另外, 因为 `typedef` 是定义了一种类型的新别名,
不是简单的字符串替换, 所以它比宏来得稳健(虽然用宏有时也可以完成以上的用途).

### 为复杂的声明定义简单别名

```cpp
int *(*a[5])(int, char*); //原声明
​
typedef int *(*pFun)(int, char*);
pFun a[5];
```

在原来的声明里逐步用别名替换一部分复杂声明,
如此循环, 把带变量名的部分留到最后替换, 得到的就是原声明的最简化版.

```cpp
void (*b[10])(void(*)()); //原声明
​
//先替换右边括号, pFunParam为别名1
typedef void (*pFunParam);
//再替换左边括号, pFun2为别名2
typedef void (*pFun2)(pFunParam);
​
//原声明简化版
pFun2 b[10];
```

## 如何理解复杂声明和定义

在阅读 `Linux` 的内核代码是经常会遇到一些复杂的声明和定义, 例如:

```cpp
void * (* (*fp1) (int)) [10];
​
float (* (*fp2) (int, int, float)) (int);
​
typedef double (* (* (*fp3) ()) [10]) ();
fp3 a;
​
int (* (*fp4()) [10]) ();
```

刚看到这些声明或者定义时, 一些初学者甚至有一定经验的工程师都有可能费解.
要理解这些复杂的声明和定义, 应该由浅而深, 逐步突破. 下面先看一些简单的定义:

```cpp
int a; //定义一个整型数
int *p; //定义一个指向整型数的指针
int **pp; //定义一个指向指针的指针, 它指向的指针指向一个整型数
​
p = &a;   // p指向整数a所在的地址
pp = &p;  // pp指向指针p
```

如下所示, 定义 `整数型数组` 的指针, 指向 `整数型数组`.

```cpp
int arr[10]; //定义一个包含10个整型数的数组
int (*pArr) [10]; //定义指针, 指向数组, 包含10个int
​
pArr = &arr;
```

如下所示, 包含指向函数的指针的数组, 这些函数有整型参数和整型返回值.

```cpp
//定义一个指向函数的指针,
//被指向的函数有一个整型参数并返回整型值
int (*pfunc) (int);

// 定义一个包含10个指针的数组, 其中包含的指针指向函数,
// 这些函数有一个整型参数并返回整型值
int (*arr[10]) (int);
​
arr[0] = pfunc;
```

### 右左法则

当声明和定义逐渐复杂时, 需要使用用于理解复杂定义的 "右左法则 ":

从变量名看起, 先往右, 再往左, 碰到圆括号就调转阅读的方向;
括号内分析完就跳出括号, 还是先右后左的顺序. 如此循环, 直到分析完整个定义.
然后再来分析 `int (*pfunc) (int)`;

+ 找到变量名 `pfunc`, 先往右是圆括号, 调转方向,
+ 左边是一个`*`号, 这说明 `pfunc` 是一个指针;
+ 然后跳出这个圆括号, 先看右边, 又遇到圆括号, 这说明 `(*pfunc)` 是一个函数,
+ 所以 `pfunc` 是一个指向这类函数的指针, 即函数指针,
这类函数具有一个 `int` 类型的参数, 返回值类型是 `int`.

同样的, 对于 `int (*arr[10]) (int);`

+ 找到变量名 `arr`, 先往右是 `[]` 运算符, 说明 `arr` 是一个数组;
+ 再往左是一个 `*` 号, 说明 `arr`数组的元素是指针.
注意: 这里的`*`修饰的不是 `arr`, 而是 `arr[10]`.
原因是 `[]` 运算符的优先级比 `*` 要高, `arr` 先与 `[]`结合.

+ 跳出圆括号, 先往右又遇到圆括号, 说明 `arr` 数组的元素是指向函数的指针,
它指向的函数有一个 `int` 类型的参数, 返回值类型是 `int`.

那么, 怎么判断定义的是函数指针, 还是数组指针, 或是数组呢?可以抽象出几个模式:

```cpp
// 变量名var与*结合, 被圆括号括起来, 右边是参数列表. 表明这是函数指针
typedef (*var)(...);
//变量名var与*结合, 被圆括号括起来, 右边是[]运算符. 表示这是数组指针
typedef (*var)[];
// 变量名var先与[]结合, 说明这是一个数组
//(至于数组包含的是什么, 由旁边的修饰决定)
typedef (*var[])...;
```

下面可以利用右左法则去分析复杂的声明和定义

### exa1

```cpp
void * (* (*fp1) (int)) [10];
```

+ 找到变量名 `fp1`, 往右看是圆括号, 调转方向往左看到 `*`号, 说明 `fp1` 是一个指针;
+ 跳出内层圆括号, 往右看是参数列表, 说明指针 `fp1` 指向函数,
+ 接着往左看是 `*` 号, 说明指向的 `函数返回值` 是 `指针`;
+ 再跳出外层圆括号, 往右看是 `[]` 运算符, 说明函数返回的, 是指向 `数组` 的 `指针`.
+ 往左看是 `void*`, 说明数组包含的类型是 `void *`.

>形如 `int (*) [10]` 的定义, `(*)` 中的 `*` 是孤立的, 所以 `*` 的类型要靠 `()` 外的信息提供,
>考虑 `int a [10]` 的含义是 `a` 是数组, 数组元素是 `int`;
> 类比可知, `*` 指向数组, 数组元素是 `int`.

简言之 , `fp1` 是指向 `函数` 的 `指针`,
该函数接受一个 `整型参数`, 并返回指针`B`, `B`指向数组`C`, `C`含有 `10`个 `void指针`.

### exa2

```cpp
float (* (*fp2) (int, int, float)) (int);
```

+ 找到变量名 `fp2`, 往右看是圆括号, 调转方向往左看到 `*`号, 说明 `fp2` 是指针;
+ 跳出内层圆括号, 往右看是参数列表, 说明 `fp2` 是函数指针,
+ 接着往左看是 `*` 号, 说明指向的函数返回值是指针;
+ 再跳出外层圆括号, 往右看还是 `参数列表`, 说明返回的指针是 `函数指针`,
+ 该函数有一个 `int` 类型的参数, 返回值类型是 `float`.

简言之, `fp2` 是指向函数的 `指针`, 该函数接受三个参数 `(int, int, float)`,
且返回指向函数的 `指针`, 该函数接受 `整型参数` 并返回 `float`.

### exa3

如果创建许多复杂的定义, 可以使用 `typedef`.
这一条显示 `typedef` 是如何缩短复杂的定义的.

```cpp
typedef double (* (* (*fp3) ()) [10]) ();
fp3 a;
```

+ 跟前面一样, 先找到变量名 `fp3`(这里fp3其实是新类型名),
+ 往右看是圆括号, 调转方向往左是 `*`, 说明 `fp3` 是一个指针;
+ 跳出圆括号, 往右看是 `空参数列表`, 说明 `fp3` 是一个函数指针,
+ 接着往左是 `*` 号, 说明该函数的返回值是一个指针;
+ 跳出第二层圆括号, 往右是 `[]` 运算符, 说明函数的返回值是一个 `数组指针`,
+ 接着往左是 `*` 号, 说明数组中包含的是指针;
+ 跳出第三层圆括号, 往右是参数列表,
说明数组中包含的是函数指针, 这些函数没有参数, 返回值类型是 `double`.

简言之, `fp3` 是指向函数的 `指针`,
该函数无参数, 且返回含有 `10` 个指向 `函数指针` 的 `数组` 的 `指针`,
这些函数 `没参数` 且返回 `double` 值.

这二行接着说明: `a` 是 `fp3` 类型中的一个.

```cpp
int (* (*fp4()) [10]) ();
```

### exa4

```cpp
int (* (*fp4()) [10]) ();
```

这里 `fp4` 不是变量定义, 而是一个函数声明.

+ 找到变量名 `fp4`, 往右是 `空参数列表`, 说明 `fp4` 是一个函数,
+ 接着往左是 `*` 号, 说明函数返回值是 `指针`;
+ 跳出里层圆括号, 往右是 `[]` 运算符, 说明 `fp4` 的函数返回值是 `指向数组的指针`,
+ 往左是 `*` 号, 说明数组中包含的元素是指针;
+ 跳出外层圆括号, 往右是 `空参数列表`, 说明数组中包含的元素是函数指针,
这些函数没有参数, 返回值的类型是 `int`.

简言之, `fp4` 是返回指针的函数,
该指针指向含有 `10` 个函数指针的数组, 这些函数 `空参数` 且返回 `整型值`.

## 用typedef简化复杂的声明和定义

### exa2

```cpp
int (a[10]) (int, char*);
```

用前面的 "右左法则", 可以知道 `a` 是包含 `10` 个函数指针的数组,
这些函数的参数列表是 `(int, char*)`, 返回值类型是 `int`.
如果要定义相同类型的变量 `b`, 都得重复书写:

```cpp
int (b[10]) (int, char*);
```

为了避免重复复杂的定义, 用 `typedef` 来简化复杂的声明和定义.
`typedef` 可以给现有的类型起个别名.
这里用 `typedef` 给以上 `a`, `b`的类型起个别名:

```cpp
// 在之前定义的前面加入typedef, 然后将变量名a替换成类型名A
typedef int *(*A[10]) (int, char*);
```

现在要再定义相同类型的变量`c`, 只需要:

```cpp
A c;
```

### exa3

```cpp
void (*b[10]) (void (*)());
```

先替换右边括号里面的参数, 将 `void (*)()` 的类型起个别名 `pParam`:

```cpp
typedef void (*pParam) ();
```

再替换左边的变量b, 为b的类型起个别名B:

```cpp
typedef void (*B)(pParam);
```

原声明的简化版:

```cpp
B b[10];
```
