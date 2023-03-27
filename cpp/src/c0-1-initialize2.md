# 隐式初始化与显式初始化

## 概念

无论何时只要类的对象被创建就会执行构造函数,
通过 `显式调用构造函数` 进行初始化被称为显式初始化, 否则叫做隐式初始化.

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

## explicit禁用构造函数定义的类型转换

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

## 只允许一步隐式类型转换

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
string s1 = "tomocat";    // 拷贝初始化, 调用拷贝构造函数
string s2("tomocat");     // 直接初始化, 调用 构造函数
string s3(10, 'c');       // 直接初始化, s3内容为cccccccccc
​
// s4拷贝初始化
string s4 = string(10, 'c');
// 等价于
string temp = string(10, 'c');
string s4 = temp;
```

## 列表初始化

## C++98/03与C++11的列表初始化

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

## vector中圆括号与花括号的初始化

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

## 初始化习惯

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

## 列表初始化返回值

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

## initializer_list形参

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
