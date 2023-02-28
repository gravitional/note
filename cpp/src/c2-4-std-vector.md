# cppreference std::vector

[std::vector](https://zh.cppreference.com/w/cpp/container/vector)
[emplace_back()和push_back()的对比, 前者不能替代后者的例子](https://blog.csdn.net/qq_36607894/article/details/107981615)

## emplace_back

以vector为例

![emplace_back](https://img-blog.csdnimg.cn/20200903133604130.png)

每一次插入新元素, 如果size没超过capacity, 那么最终只有超尾迭代器失效;
而如果size超过了capacity, 则所有的迭代器和引用全部都会失效.

总之, 不管什么容器的emplace_back函数, 其参数都是右值引用.
传入的参数的值是右值, 实参是对它们的右值引用.

### 测试代码: emplace_back()少一次复制操作, 所以效率更高

这俩代码都是别的网友写的.
这个代码说明参数为左值引用的push_back方法要调用构造函数和复制构造函数,
说明确实要先构造一个临时对象, 再把临时对象用copy构造拷贝到数组最后面, 确实费时.

```cpp
#include <iostream>
#include <cstring>
#include <vector>
using namespace std;

class A {
public:
    A(int i){
        str = to_string(i);
        cout << "构造函数" << endl;
    }
    ~A(){}
    A(const A& other): str(other.str){
        cout << "拷贝构造" << endl;
    }

public:
    string str;
};

int main()
{
    vector<A> vec;
    vec.reserve(10);
    for(int i=0;i<10;i++){
        vec.push_back(A(i)); //调用了10次构造函数,和10次拷贝构造函数,
//      vec.emplace_back(i);  //调用了10次构造函数, 一次拷贝构造函数都没有调用过
    }

}
```

下面这个代码详细分析了五种原型,
左值参数的, 右值参数的push_back,以及emplace_back. 发现:

+ 左值参数的push_back要调用 `构造函数` 和 `复制构造`
+ 右值参数的push_back要调用 `构造函数` 和 `移动构造`
emplace_back要调用一次构造而已

```cpp
#include <vector>
#include <string>
#include "time_interval.h"

class Foo {
public:
    Foo(std::string str) : name(str) {
        std::cout << "constructor" << std::endl;
    }
    Foo(const Foo& f) : name(f.name) {
        std::cout << "copy constructor" << std::endl;
    }
    Foo(Foo&& f) : name(std::move(f.name)){
        std::cout << "move constructor" << std::endl;
    }

private:
    std::string name;
};
int main() {

    std::vector<Foo> v;
    int count = 10000000;
    v.reserve(count);       //预分配十万大小, 排除掉分配内存的时间

    {
        TIME_INTERVAL_SCOPE("push_back T:");
        Foo temp("ceshi");
        v.push_back(temp);// push_back(const T&), 参数是左值引用
        //打印结果:
        //constructor
        //copy constructor
    }

    v.clear();
    {
        TIME_INTERVAL_SCOPE("push_back move(T):");
        Foo temp("ceshi");
        v.push_back(std::move(temp));// push_back(T &&), 参数是右值引用
        //打印结果:
        //constructor
        //move constructor
    }

    v.clear();
    {
        TIME_INTERVAL_SCOPE("push_back(T&&):");
        v.push_back(Foo("ceshi"));// push_back(T &&), 参数是右值引用
        //打印结果:
        //constructor
        //move constructor
    }

    v.clear();
    {
        std::string temp = "ceshi";
        TIME_INTERVAL_SCOPE("push_back(string):");
        v.push_back(temp);// push_back(T &&), 参数是右值引用
        //打印结果:
        //constructor
        //move constructor
    }

    v.clear();
    {
        std::string temp = "ceshi";
        TIME_INTERVAL_SCOPE("emplace_back(string):");
        v.emplace_back(temp);// 只有一次构造函数, 不调用拷贝构造函数, 速度最快
        //打印结果:
        //constructor
    }
}
```

### 例子2

cpp reference网站给的对比示例: 以双向链表容器list为例

```cpp
#include <list>
#include <string>
#include <iostream>

struct President
{
    std::string name;
    std::string country;
    int year;

    President(std::string p_name, std::string p_country, int p_year)
        : name(std::move(p_name)), country(std::move(p_country)), year(p_year)
    {
        std::cout << "I am being constructed.\n";
    }
    President(President&& other)
        : name(std::move(other.name)), country(std::move(other.country)), year(other.year)
    {
        std::cout << "I am being moved.\n";
    }
    President& operator=(const President& other) = default;
};

int main()
{
    std::list<President> elections;
    std::cout << "emplace_back:\n";
    elections.emplace_back("Nelson Mandela", "South Africa", 1994);//直接把成员参数传给emplace_back

    std::list<President> reElections;
    std::cout << "\npush_back:\n";
    reElections.push_back(President("Franklin Delano Roosevelt", "the USA", 1936));//传给push_back的却必须是一个已经构造好的对象, 所以要先构造, push_back内部再调用移动构造实现添加到容器末尾的功能

    std::cout << "\nContents:\n";
    for (President const& president: elections) {
        std::cout << president.name << " was elected president of "
                  << president.country << " in " << president.year << ".\n";
    }
    for (President const& president: reElections) {
        std::cout << president.name << " was re-elected president of "
                  << president.country << " in " << president.year << ".\n";
    }
}
```

可以看到, `emplace_back` 直接构造, 通过把参数直接传给构造函数实现.

但是 `push_back` 先构造, 然后把构造的这个对象用移动构造再构造一遍. 所以构造了两次.

```bash
emplace_back:
I am being constructed.

push_back:
I am being constructed.
I am being moved.

Contents:
Nelson Mandela was elected president of South Africa in 1994.
Franklin Delano Roosevelt was re-elected president of the USA in 1936.
```

### emplace_back()无法替代push_back()的例子

```cpp
// 假定下面使用的 uinique_ptr 支持隐式构造
vector<unique_ptr<INT>> v;
v.push_back(new INT(10));
v.emplace_back(new INT(10));
```

如果对该 vector 的本次操作需要 `resize`, 且 `resize` 抛出了一个异常.
在这种情况下,  `push_back` 是绝对安全的, 而 emplace_back 则有可能发生内存泄漏.
因为使用 `push_back`, 在抛出异常前,  `unique_ptr` 已经构造成功, 可以正常释放内存.
但是如果使用 emplace_back, 在抛出异常前, `unique_ptr` 还未进行构造, 这里就只是单纯的一个指针, 不会自动释放.
