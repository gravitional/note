# C++ 初始化

[C++中五花八门的初始化](https://zhuanlan.zhihu.com/p/365769082)
[为什么C++的初始化规则这么复杂](https://www.zhihu.com/question/403578855/answer/1306943217)

## 总结

+ `初始化` 的概念: 创建变量同时赋予它值, 不同于 `赋值`, 后者要先擦除原来的值
+ 类的 `构造函数` 控制其对象的初始化过程, 无论何时只要 `类的对象` 被创建就会执行 `构造函数`
+ 如果对象未被用户指定 `初始值`, 那么这些变量会被执行 `默认初始化`,
默认值取决于 `变量类型` 和定义变量的 `位置`

+ 无论何时只要类的对象被创建就会执行 `构造函数`,
通过 `显式调用` 构造函数进行初始化被称为 `显式初始化`, 否则叫做 `隐式初始化`
+ 使用`等号`(`=`)初始化一个 `class 变量` 执行的是 `拷贝初始化`,
编译器会把等号右侧的初始值拷贝到新创建的对象中去, 不使用等号则执行的是 `直接初始化`

+ 传统C++中 `列表初始化` 仅能用于普通 `数组` 和 `POD` 类型,
C++11新标准将 `列表初始化` 应用于所有对象的初始化, 但是习惯上

+ `内置类型` 用 `=`(等号) 初始化,
+ `class type` 用 `T()` (构造函数圆括号) 显式初始化,
+ `vector`, `map` 和 `set` 等容器类习惯用 `列表初始化`

>POD, Plain Old Data, 即没有构造, 析构和虚函数的类或结构体

## 变量类型 和 初始化语法

+ 引用
+ 字符数组
+ 聚合类型(aggregate type):数组, POD(没有构造, 析构和虚函数的 struct 和 union)
+ class type; struct, class, union
+ 内置对象类型和 enum

+ `默认`初始化(default-initialization), 例如 `T t;`
+ `圆括号`初始化(非列表, direct-initialization,non-list), 例如 `T t(args...);`
+ `等号`初始化(非列表, copy-initialization, non-list), 例如 `T t = init;`
+ `花括号`初始化(direct-list-initialization), 例如 `T t{ args... };`
+ `花括号等号`初始化(copy-list-initialization), 例如 `T t = { args... };`

## 初始化语法

## 动态内存,值初始化

对于 `内置类型` 变量, 初始化过程就是赋值,

```cpp
int *point;
point=new int(2);
```

动态分配了用于存放 `int` 类型数据的内存空间, 并将初值 2 放入该空间中,
然后将地址赋给指针 `point`.

>细节:对于基本数据类型, 如果不希望在分配内存后设定初值, 可以把括号省略, 例如:

```cpp
int *point=new int;
```

如果保留括号, 但括号中不写任何数值, 则表示用 `0` 对该对象初始化, 例如:

```
int *point=new int();
```

如果建立的对象是某个类的实例对象,
就要根据初始化参数列表的参数类型和个数调用该类的构造函数.

>细节 在用 new 建立一个类的对象时, 如果该类存在用户定义的默认构造函数,
则 `new T` 和 `new T()`这两种写法的效果相同, 都会调用用户的默认构造函数.
但若用户未定义默认构造函数, 使用 `new T` 创建对象时, 会调用系统生成的隐含的默认构造函数;
使用 `new T()` 创建对象时, 除了执行系统默认构造函数的操作之外,
还会给内置类型和指针类型的成员用 `0` 赋初值, 而且这一过程是递归的.
也就是对于成员对象 `b`, 如果 `b` 也没有用户定义的默认构造函数, 也会用 `0` 给内置和指针类型赋初值.

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

## 默认初始化与值初始化

>Tips: C不允许用户自定义 `默认值` 从而提高性能(增加函数调用的代价),
>C++默认也不做初始化从而提高性能, 但是C++提供了 `构造函数` 让用户显式设置 `默认初始值`.
>有个例外是, 把 `全局变量` 初始化为 `0` 仅仅在程序启动时会有成本,
>因此定义在 `任何函数之外` 的变量会被初始化为`0`.
>如果定义变量时没有指定初始值, 则变量会被 `默认初始化` 或 `值初始化`,
>此时变量被赋予了默认值, 这个默认值取决于变量类型和定义位置.

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

>Tips: 建议初始化每一个内置类型的变量, 原因在于定义在函数内部的内置
类型变量的值是未定义的, 如果试图拷贝或者以其他形式访问此类值是一种错误的编程行为且很难调试
.如果内置类型的变量未被显式初始化, 它的值由定义的位置决定. 定义于任何函数体之外的变量会被
初始化为0, 定义在函数体内部的内置类型变量将不被初始化(uninitialized), 一个未被初始化的内置
类型变量的值时未定义的, 如果试图拷贝或以其他形式访问此类值将引发错误.

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

定义一个类变量但是没有指定初始值时, 会使用 `默认构造函数` 来初始化,
所以没有默认构造函数的类不能执行默认初始化.
定义于任何 `函数体之外` 的 class 类型变量会先进行 `零初始化`, 再执行默认初始化,
定义在函数体内部的 `class变量` 会直接执行 `默认初始化`.

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
Cat global_cat;
Dog global_dog;
​
int main() {
    Cat local_cat;
    Dog local_dog;
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
那么成员将在 `构造函数体` 之前执行 默认初始化, 例如:

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

对于类类型而言, 不指定初始值下会调用它的默认构造函数,
因此不存在默认初始化和值初始化的区别.
但是对于内置类型值初始化和默认初始化不同,
只不过实际开发中我们建议显式初始化内置类型来避免产生未定义值的代码:

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

## 隐式初始化与显式初始化

### 概念

无论何时只要类的对象被创建就会执行构造函数,
通过显式调用构造函数进行初始化被称为显式初始化, 否则叫做隐式初始化.

```cpp
#include <iostream>
​
// Cat提供两个构造函数
class Cat {
 public:
    int age;
    Cat() = default;
    explicit Cat(int i) : age(i) {}
};
​
int main() {
    Cat cat1;           // 隐式初始化: 调用默认构造函数
    Cat cat2(10);       // 隐式初始化: 调用一个形参的构造函数
​
    Cat cat3 = Cat();   // 显式初始化: 调用默认构造函数
    Cat cat4 = Cat(5);  // 显式初始化: 调用一个形参的构造函数
​
    // 构造函数还可以搭配new一起使用, 用于在堆上分配内存
    Cat *cat5 = new Cat();
    Cat *cat6 = new Cat(3);
    delete cat5;
    delete cat6;
​
    return 0;
}
```

还有一些操作不会显式调用类的构造函数, 比如:

通过一个实参调用的构造函数定义了从构造函数参数类型向类类型隐式转换的规则
拷贝构造函数定义了用一个对象初始化另一个对象的隐式转换

```cpp
#include <iostream>
​
// Cat提供两个构造函数
class Cat {
 public:
    int age;
    // 接收一个参数的构造函数定义了从int型向类类型隐式转换的规则, explicit关键字可以组织这种转换
    Cat(int i) : age(i) {}
    // 拷贝构造函数定义了从一个对象初始化另一个对象的隐式转换
    Cat(const Cat &orig) : age(orig.age) {}
};
​
int main() {
    Cat cat1 = 10;    // 调用接收int参数的拷贝构造函数
    Cat cat2 = cat1;  // 调用拷贝构造函数
​
    std::cout << cat1.age << std::endl;
    std::cout << cat2.age << std::endl;
    return 0;
}
​
// 输出:
10
10
```

### explicit禁用构造函数定义的类型转换

例如智能指针就把构造函数声明为explict, 所以智能指针只能直接初始化.
我们也可以通过explicit禁用掉上面提到的两种隐式转换规则:

```cpp
#include <memory>
​
class Cat {
 public:
    int age;
    Cat() = default;
    // 必须显式调用拷贝构造函数
    explicit Cat(const Cat &orig) : age(orig.age) {}
};
​
int main() {
    Cat cat1;
    Cat cat2(cat1);      // 正确: 显式调用拷贝构造函数
    // Cat cat3 = cat1;  // 错误: explicit关键字限制了拷贝构造函数的隐式调用
​
    // std::shared_ptr<int> sp = new int(8);    // 错误: 不支持隐式调用构造函数
    std::shared_ptr<int> sp(new int(8));        // OK
    return 0;
}
```

### 只允许一步隐式类型转换

编译器只会自动执行一步隐式类型转换,
如果隐式地使用两种转换规则, 那么编译器便会报错:

```cpp
class Cat {
 public:
    std::string name;
    Cat(std::string s) : name(s) {}  // 允许string到Cat的隐式类型转换
};
​
int main() {
    // 错误: 不存在从const char[8]到Cat的类型转换, 编译器不会自动把const char[8]转成string, 再把string转成Cat
    // Cat cat1 = "tomocat";
​
    // 正确: 显式转换成string, 再隐式转换成Cat
    Cat cat2(std::string("tomocat"));
​
    // 正确: 隐式转换成string, 再显式转换成Cat
    Cat cat3 = Cat("tomocat");
}
```

## 直接初始化与拷贝初始化

如果使用等号(=)初始化一个类变量, 实际上执行的是拷贝初始化,
编译器把等号右侧的值拷贝到新创建的对象中区;
如果不使用等号, 那么执行的是直接初始化.

以string为例:

```cpp
string s1 = "tomocat";    // 拷贝初始化
string s2("tomocat");     // 直接初始化
string s3(10, 'c');       // 直接初始化, s3内容为cccccccccc
​
// s4拷贝初始化
string s4 = string(10, 'c');
// 等价于
string temp = string(10, 'c');
string s4 = temp;
```

## 列表初始化

### C++98/03与C++11的列表初始化

在 C++98/03 中, 普通数组 和 `POD`(Plain Old Data, 即没有构造, 析构和虚函数的类或结构体)
类型可以使用花括号(`{}`)进行初始化, 即 `列表初始化`.
但是这种初始化方式仅限于上述提到的两种数据类型:

```cpp
int main() {
    // 普通数组的列表初始化
    int arr1[3] = { 1, 2, 3 };
    int arr2[] = { 1, 3, 2, 4 };  // arr2被编译器自动推断为int[4]类型

    // POD类型的列表初始化
    struct data {
        int x;
        int y;
    } my_data = { 1, 2 };
}
```

C++11 新标准中 `列表初始化` 得到了全面应用,
不仅兼容了传统C++中普通数组和POD类型的列表初始化, 还可以用于任何其他类型对象的初始化:

```cpp
#include <iostream>
#include <string>
​
class Cat {
 public:
    std::string name;
    // 默认构造函数
    Cat() {
        std::cout << "default constructor of Cat" << std::endl;
    }
    // 接受一个参数的构造函数
    Cat(const std::string &s) : name(s) {
        std::cout << "normal constructor of Cat" << std::endl;
    }
    // 拷贝构造函数
    Cat(const Cat &orig) : name(orig.name) {
        std::cout << "copy constructor of Cat" << std::endl;
    }
};
​
int main() {
    /*
     * 内置类型的列表初始化
     */
    int a{ 10 };       // 内置类型通过初始化列表的 直接初始化
    int b = { 10 };    // 内置类型通过初始化列表的 拷贝初始化
    std::cout << "a:" << a << std::endl;
    std::cout << "b:" << b << std::endl;
​
    /*
     * 类类型的列表初始化
     */
    Cat cat1{};                 // 类类型调用 默认构造函数 的 列表初始化
    std::cout << "cat1.name:" << cat1.name << std::endl;
    Cat cat2{ "tomocat" };        // 类类型调用 普通构造函数 的 列表初始化
    std::cout << "cat2.name:" << cat2.name << std::endl;
​
    // 注意列表初始化前面的 等于号 并不会影响 初始化行为, 这里并不会调用 拷贝构造函数
    Cat cat3 = { "tomocat" };     // 类类型调用 普通构造函数 的 列表初始化
    std::cout << "cat3.name:" << cat3.name << std::endl;
    // 先通过 列表初始化 构造右侧 Cat临时对象,
    // 再调用 拷贝构造函数(从输出上看好像编译器优化了, 直接调用 普通构造函数 而不会调用 拷贝构造函数)
    Cat cat4 = Cat{ "tomocat" };
    std::cout << "cat4.name:" << cat4.name << std::endl;
​
    /*
     * new申请堆内存的列表初始化
     */
    int *pi = new int{ 100 };
    std::cout << "*pi:" << *pi << std::endl;
    delete pi;
    int *arr = new int[4] { 10, 20, 30, 40 };
    std::cout << "arr[2]:" << arr[2] << std::endl;
    delete[] arr;
}
​
// 输出:
a:10
b:10
default constructor of Cat
cat1.name:
normal constructor of Cat
cat2.name:tomocat
normal constructor of Cat
cat3.name:tomocat
normal constructor of Cat
cat4.name:tomocat
*pi:100
arr[2]:30
```

### vector中圆括号与花括号的初始化

总的来说, `圆括号` 是通过调用 `vector` 的 `构造函数` 进行初始化的,
如果使用了 `花括号` 那么初始化过程,
会尽可能会把 `花括号` 内的值当做 `元素初始值` 的列表来处理.
如果初始化时使用了 `花括号` 但是提供的值又无法用来 `列表初始化`,
那么就考虑用这些值来调用 `vector` 的构造函数了.

```cpp
#include <string>
#include <vector>
​
int main() {
    std::vector<std::string> v1{"tomo", "cat", "tomocat"};  // 列表初始化: 包含3个string元素的vector
    // std::vector<std::string> v2("a", "b", "c");          // 错误: 找不到合适的构造函数
​
    std::vector<std::string> v3(10, "tomocat");             // 10个string元素的vector, 每个string初始化为"tomocat"
    std::vector<std::string> v4{10, "tomocat"};             // 10个string元素的vector, 每个string初始化为"tomocat"
​
    std::vector<int> v5(10);     // 10个int元素, 每个都初始化为0
    std::vector<int> v6{10};     // 1个int元素, 该元素的值时10
    std::vector<int> v7(10, 1);  // 10个int元素, 每个都初始化为1
    std::vector<int> v8{10, 1};  // 2个int元素, 值分别是10和1
}
```

### 初始化习惯

尽管 C++11 将 `列表初始化` 应用于所有对象的初始化, 但是

+ `内置类型` 习惯于用 `等号初始化`,
+ `类类型` 习惯用 `构造函数圆括号` 显式初始化,
+ `vector`, `map` 和 `set` 等容器类习惯用列表初始化.

```cpp
#include <string>
#include <vector>
#include <set>
#include <map>
​
class Cat {
 public:
    std::string name;
    Cat() = default;
    explicit Cat(const std::string &s) : name(s) {}
};
​
int main() {
    // 内置类型初始化(包括string等标准库简单类类型)
    int i = 10;
    long double ld = 3.1415926;
    std::string str = "tomocat";
​
    // 类类型初始化
    Cat cat1();
    Cat cat2("tomocat");
​
    // 容器类型初始化(当然也可以用圆括号初始化, 列表初始化用于显式指明容器内元素)
    std::vector<std::string> v{"tomo", "cat", "tomocat"};
    int arr[] = {1, 2, 3, 4, 5};
    std::set<std::string> s = {"tomo", "cat"};
    std::map<std::string, std::string> m = {{"k1", "v1"}, {"k2", "v2"}, {"k3", "v3"}};
    std::pair<std::string, std::string> p = {"tomo", "cat"};
​
    // 动态分配对象的列表初始化
    int *pi = new int {10};
    std::vector<int> *pv = new std::vector<int>{0, 1, 2, 3, 4};
​
    // 动态分配数组的列表初始化
    int *parr = new int[10]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
}
```

### 列表初始化返回值

C++11新标准规定, 函数可以通过 `列表初始化` 来对函数返回的临时量进行初始化:

```cpp
#include <string>
#include <vector>
​
std::vector<std::string> foo(int i) {
    if (i < 5) {
        return {};  // 返回一个空vector对象
    }
    return {"tomo", "cat", "tomocat"};  // 返回列表初始化的vector对象
}
​
int main() {
    foo(10);
}
```

### initializer_list形参

前面提到 C++11 支持所有类型的初始化,
对于 `类类型` 而言, 虽然我们使用 `列表初始化` 它会自动调用匹配的构造函数,
但是我们也能显式指定接受 `初始化列表` 的构造函数.
C++11引入了 `std::initializer_list`, 允许 `构造函数` 或其他函数像参数一样使用初始化列表,
这才真正意义上为类对象的初始化, 与普通数组和 POD 的初始化方法提供了统一的桥梁.

Tips:

1. 类对象在被 `列表初始化` 时会优先调用 `列表初始化` 构造函数,
如果没有 `列表初始化` 构造函数, 则会根据提供的 `花括号` 值调用匹配的构造函数
3. C++11 新标准提供了两种方法用于处理可变数量形参,
第一种是我们这里提到的 `initializer_list` 形参(所有的形参类型必须相同),
另一种是 `可变参数模板`(可以处理不同类型的形参)

```cpp
#include <initializer_list>
#include <vector>
​
class Cat {
 public:
    std::vector<int> data;
    Cat() = default;
    // 接受初始化列表的构造函数
    Cat(std::initializer_list<int> list) {
        for (auto it = list.begin(); it != list.end(); ++it) {
            data.push_back(*it);
        }
    }
};
​
int main() {
    Cat cat1 = {1, 2, 3, 4, 5};
    Cat cat2{1, 2, 3};
}
```

初始化列表除了用于对象构造函数上, 还可以作为普通参数形参:

```cpp
#include <initializer_list>
#include <string>
#include <iostream>
​
void print(std::initializer_list<std::string> list) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        std::cout << *it << std::endl;
    }
}
​
int main() {
    print({"tomo", "cat", "tomocat"});
}
```
