# C++ 初始化

[C++中五花八门的初始化](https://zhuanlan.zhihu.com/p/365769082)
[为什么C++的初始化规则这么复杂](https://www.zhihu.com/question/403578855/answer/1306943217)

## 总结

+ `初始化` 的概念: 创建变量 `同时` 赋予它值, 不同于 `赋值`, 后者要 `先擦除` 原来的值
+ 类的 `构造函数` 控制其对象的初始化过程, 无论何时只要 `类的对象` 被创建就会执行 `构造函数`
+ 如果对象未被用户指定 `初始值`, 那么这些变量会被执行 `默认初始化`,
默认值取决于 `变量类型` 和定义变量的 `位置`

+ 无论何时只要 类的对象 被创建就会执行 `构造函数`,
通过 `显式调用` 构造函数进行初始化被称为 `显式初始化`, 否则叫做 `隐式初始化`.

+ 使用 `=`(等号)初始化一个 `class` 变量, 执行的是 `拷贝初始化`,
编译器会把等号右侧的 `初始值` 拷贝到新创建的对象中去, 不使用等号则执行的是 `直接初始化`

+ 传统C++中 `列表初始化` 仅能用于普通 `数组` 和 `POD` 类型,
C++11新标准将 `列表初始化` 应用于所有对象的初始化, 但是习惯上
    + `内置类型` 用 `=` 初始化,
    + `class type` 用 `T()` (构造函数圆括号) 显式初始化,
    + `vector`, `map` 和 `set` 等容器类习惯用 `列表初始化`

>POD, Plain Old Data, 即没有构造, 析构和虚函数的类或结构体

## 变量类型 和 初始化语法

+ 引用
+ 字符数组
+ 聚合类型(aggregate type):数组, POD(没有构造, 析构和虚函数的 struct 和 union)
+ class type; struct, class, union
+ 内置对象类型和 enum

```cpp
class T;
T t;                //`默认`初始化(default-initialization)
T t(args...);       //`圆括号`初始化(非列表, direct-initialization,non-list)
T t = init;         //`等号`初始化(非列表, copy-initialization, non-list)
T t{ args... };     //`花括号`初始化(direct-list-initialization)
T t = { args... };  //`花括号等号`初始化(copy-list-initialization)
```

## 构造函数

ZhengLi,P107.

+ 调用时 `无需提供参数` 的构造函数称为 `默认构造函数`.
+ 如果类中没有写构造函数, 编译器会自动生成一个 `隐含的默认构造函数`,
它的 `参数列表` 和 `函数体` 皆为空. 如果类中声明了构造函数(无论是否有参数),
编译器便不会再为之生成 `隐含构造函数`. 例如以下 `Clock` 类:

```cpp
class Clock{
public:
    Clock(){} //编译系统生成的 隐含默认构造函数
}
```

>虽然本例中编译系统生成的 隐含构造函数不做任何时期(函数体为空),
>但有时 `函数体为空` 的构造函数并非无所事事,
它还要负责 `基类的构造` 和 `本类中成员对象` 的构造.
由程序员显式指定的 `空函数体` 的 默认构造函数,
同样会负责 `基类的构造` 和 `本类中成员对象` 的构造.

## 初始化语法

## 值初始化,动态内存

对于 `内置类型` 变量, `初始化` 过程就是 `赋值`,

```cpp
int *point;
point=new int(2);
```

动态分配了用于存放 `int` 类型数据的内存空间, 并将初值 `2` 放入该空间中,
然后将地址赋给指针 `point`.

### int()细节

对于 `基本数据类型`, 如果不希望在分配内存后设定初值, 可以把括号省略, 例如:

```cpp
int *point=new int;
```

如果保留括号, 但括号中不写任何数值, 则表示用 `0` 对该对象初始化, 例如:

```cpp
int *point=new int();
```

如果建立的对象是某个类的 `实例对象`,
就要根据 `初始化参数列表` 的参数类型和个数调用该类的构造函数.

### new T()细节

在用 `new` 建立一个类的对象时, 如果该类存在 `用户定义的默认构造函数`,
则 `new T` 和 `new T()`这两种写法的效果相同, 都会调用 `用户的默认构造函数`.
但若用户未定义默认构造函数:

+ 使用 `new T` 创建对象时, 会调用系统生成的 `隐含的默认构造函数`;
+ 使用 `new T()` 创建对象时, 除了执行系统默认构造函数的操作之外,
还会给内置类型和指针类型的成员用 `0` 赋初值, 而且这一过程是递归的.
也就是对于成员对象 `b`, 如果 `b` 也没有用户定义的默认构造函数,
也会用 `0` 给内置和 `指针类型` 赋初值.

## 初始化不等于赋值

+ 初始化的含义是创建变量时赋予其一个初始值,
而赋值的含义是把对象的当前值擦去, 并用一个新值替代它.

+ C++定义了初始化的好几种不同形式, 例如我们定义一个 `int` 变量并初始化为 0, 有如下4种方式:

```cpp
int i = 0;
int i = {0};
int i{0};
int i(0);
```

注意 `int i();` 的意思是声明 `i` 为 int 型函数, 不接收参数.
如果需要默认初始化, 正确的形式是:

```cpp
int i; // 在函数内部不会初始化
int i=int();
int i{};
int i={};
```

## 默认初始化与值初始化

>Tips: C不允许用户自定义 `默认值` 从而提高性能(增加函数调用的代价),
>C++默认也不做初始化从而提高性能, 但是C++提供了 `构造函数` 让用户显式设置 `默认初始值`.
>有个例外是, 把 `全局变量` 初始化为 `0` 仅仅在程序启动时会有成本,
>因此定义在 `任何函数之外` 的变量会被初始化为`0`.
>如果定义变量时没有指定初始值, 则变量会被 `默认初始化` 或 `值初始化`,
>此时变量被赋予了 `默认值`, 这个默认值取决于 `变量类型` 和 `定义位置`.

```cpp
#include <iostream>
​
class Cat {
 public:
    std::string name;
    Cat() = default;
};
​
int main() {
    Cat cat1;          // 默认初始化
    Cat cat2 = Cat();  // 显式请求值初始化, 值为Cat()
}
```

### 内置类型的默认初始化

+ Tips: 建议初始化每一个 `内置类型` 的变量,
原因在于定义在 `函数内部` 的 `内置类型` 变量的值是未定义的,
如果试图拷贝或者以其他形式访问此类值是一种错误的编程行为且很难调试.

+ 如果内置类型的变量未被 `显式初始化`, 它的值由定义的位置决定.
定义于任何函数体之外的变量会被初始化为 `0`,
+ 定义在 `函数体内部` 的内置类型变量将 `不被初始化`(uninitialized),
一个未被初始化的内置类型变量的值时未定义的,
如果试图拷贝或以其他形式访问此类值将引发错误.

```cpp
#include <iostream>
int global_value;  // 默认初始化为0
​
int main() {
    int local_value;  // 使用了未初始化的局部变量
    int* new_value = new int;
    std::cout << "new_value:" << *new_value << std::endl;       // 未定义
    std::cout << "global_value:" << global_value << std::endl;  // 0
    std::cout << "local_value:" << local_value << std::endl;    // 未定义, 且会报warning
​
    return 0;
}
```

### class type的默认初始化

+ 定义一个 `class type` 变量但是没有指定 `初始值时`, 会使用 `默认构造函数` 来初始化,
所以没有 `默认构造函数` 的类不能执行默认初始化.

+ 定义于任何 `函数体之外` 的 `class type`的变量, 会先进行 `零初始化`再执行 `默认初始化`,
+ 定义在函数体内部的 `class变量` 会直接执行 `默认初始化`.

```cpp
#include <iostream>
​
// Cat类使用合成的默认构造函数
class Cat {
 public:
    int age;
};
​
​
// Dog类使用自定义的默认构造函数
class Dog {
 public:
    int age;
    Dog() {}  // 默认构造函数, 但是不会初始化age
};
​
// 在函数体外部定义的类会先执行零初始化, 再执行默认初始化,
// 因此虽然默认构造函数不会初始化age变量, 但age仍然是0
Cat global_cat; //全局对象
Dog global_dog; //全局对象
​
int main() {
    Cat local_cat; //局部对象
    Dog local_dog; //局部对象
    std::cout << "global_cat age:" << global_cat.age << std::endl;  // 0
    std::cout << "global_dog age:" << global_dog.age << std::endl;  // 0
    std::cout << "local_cat age:" << local_cat.age << std::endl;    // 随机值
    std::cout << "local_dog age:" << local_dog.age << std::endl;    // 随机值
    return 0;
}
```

没有 `默认构造函数` 的类是不能执行 默认初始化的:

```cpp
#include <iostream>
​
// Cat类禁用默认构造函数, 无法默认初始化
class Cat {
 public:
    int age;
    Cat() = delete;
};
​
int main() {
    Cat local_cat;  // 编译报错: use of deleted function 'Cat::Cat()'
    return 0;
}
```

从本质上讲, 类的初始化取决于 `构造函数` 中对数据成员的初始化,
如果没有在构造函数的 `初始值列表` 中显式地初始化数据成员,
那么成员将在 `构造函数体` 之前执行 `默认初始化`, 例如:

```cpp
// 通过构造函数初始值列表初始化数据成员: 数据成员通过提供的初始值进行初始化
class Cat {
 public:
    int age;
    explicit Cat(int i) : age(i) {}
};
​
// 数据成员先进行默认初始化, 再通过构造函数参数进行赋值操作
// 这种方法虽然合法但是比较草率, 造成的影响依赖于数据成员的类型
class Dog {
 public:
    int age;
    explicit Dog(int i) {
        age = i;
    }
};
```

### 数组的默认初始化

1. 如果定义数组时提供了初始值列表,
那么未定义的元素若是内置类型或者有合成的默认构造则会先进行零初始化,
如果元素是类类型, 再执行默认构造函数.
2. 如果定义数组时未提供初始化列表, 则每个元素执行默认初始化

```cpp
class Cat {
 public:
    int age;
};
​
​
int main() {
    /* 内置类型在函数内部默认初始化, 随机值 */
    int int_array[5];
    for (int i = 0; i < 5; i++) {
        std::cout << int_array[i] << std::endl;  // 全都是随机值
    }
​
    /* 定义数组使用初始值列表, 除了前两个元素外都是0 */
    int int_array2[5] = { 22, 33 };
    for (int i = 0; i < 5; i++) {
        std::cout << int_array2[i] << std::endl;  // 22,33,0,0,0
    }
​
    /* 定义数组使用初始值列表, 都是0 */
    int int_array3[5] = {};
    for (int i = 0; i < 5; i++) {
        std::cout << int_array3[i] << std::endl;  // 0,0,0,0,0
    }
​
    /* 数组元素为类且使用初始值列表时 */
    Cat *my_cat = new Cat;
    Cat cat_array[5] = { *my_cat };
    for (int i = 0; i < 5; i++) {
        std::cout << cat_array[i].age << std::endl;  // 随机值,0,0,0,0
    }
​
    return 0;
}
```

### 内置类型的值初始化(不推荐)

对于类类型而言, 不指定初始值时 会调用它的默认构造函数,
因此不存在 `默认初始化` 和 `值初始化` 的区别.
但是对于 内置类型值 初始化和 默认初始化不同,
只不过实际开发中我们建议 `显式初始化` 内置类型来避免产生未定义值的代码:

```cpp
int *pi1 = new int;               // 默认初始化: *pi1的值未定义
int *pi2 = new int();             // 值初始化: *pi2的值为0
​
int *pia1 = new int[10];          // 10个默认初始化的int: 值未定义
int *pia2 = new int[10]();        // 10个值初始化的int: 值都为0
​
string *psa1 = new string[10];    // 10个默认初始化的string: 都为空
string *psa2 = new string[10]();  // 10个值初始化的string: 都为空
```
