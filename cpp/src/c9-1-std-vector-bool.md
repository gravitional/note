# vector<bool> 特化模板

[C++ vector＜bool＞ 的巨坑与range_based_for](https://blog.csdn.net/Timothy6/article/details/110688913)
[C++ reference:](http://www.cplusplus.com/reference/vector/vector-bool/)
[想知道vector<bool>有多坑人吗](https://blog.csdn.net/x313695373/article/details/13966573)

最近写代码的时候写了下面这样的代码:

```cpp
#include <vector>
using ::std::vector;
int main()
{
    vector<vector<bool>> v(10, vector<bool>(10, false));
    for (auto &vx : v)
        for (auto &velem : vx)
        {
            velem = true;
        }
    return 0;
}
```

这段看似平常的代码居然会编译错误, 但是如果把 `bool` 换成 `int`, 那么编译就会非常正确:

```cpp
#include <vector>
using ::std::vector;
int main()
{
    vector<vector<int>> v(10, vector<int>(10, 0));
    for (auto &vx : v)
        for (auto &velem : vx)
        {
            velem = 1;
        }
    return 0;
}
```

搜索 C++ reference1, 在里面我们可以看到 `vector<bool>` 原来是vector模板类的一个偏特化,
而并不是由 vector 直接实例化得到的. C++ reference给我们的解释是,
为了便于把 `bool` 优化成比特存储, 而不需要一个字节来存储.
为了探寻 `range_based_for` 失败的原因, 我们查阅C++标准2中 `vector<bool>` 的定义(略去了不必要的代码):

```cpp
namespace std
{
    template <class Allocator>
    class vector<bool, Allocator>
    {
    public:
        // types:
        using value_type = bool;
        using size_type = implementation - defined;      // see 26.2
        using iterator = implementation - defined;       // see 26.2
        using const_iterator = implementation - defined; // see 26.2
        // bit reference:
        class reference
        {
            friend class vector;
            reference &operator=(const bool x) noexcept;
            /*some code*/
        };

        iterator begin() noexcept;
        iterator end() noexcept;

        // element access:
        reference operator[](size_type n);
        reference at(size_type n);
        reference front();
    };
    /*some code*/
}
```

结果非常的amazing啊!我们对 `vector<bool>` 的每个元素进行访问的时候,
实际上访问的都是 `vector<bool>::reference` 这个类, 而不是其成员本身.
毕竟如果编译器想要将其每个元素优化成二进制位的话,
C++语言由于不存在直接访问二进制位的操作, 就无法直接访问其每个元素, 所以统一为访问这个类来对每个元素进行引用.
了解了这些, 我们想解答编译出错的事情, 还需要知道range_based_for的实现.
C++标准2中如此规定range_based_for:

```cpp
for( for-range-declaration : for-range-initializer ) statement
/*is equivalent to*/
{
    auto &&__range = for-range-initializer;
    auto __begin = begin-expr;
    auto __end = end-expr;
    for (; __begin != __end; ++__begin) {
        for-range-declaration = *__begin;
        statement
    }
}
```

那么, 我们对一维情况 `vector<bool> v; for (auto& velem : v) {}` 进行等价展开, 它就等价于:

```cpp
vector<bool> v;
{
    auto &&__range = v;             // auto被推导为vector<bool>&, 因此__range为auto&&&, 即auto&
    auto __begin = __range.begin(); // auto被推导为vector<bool>::ierator
    auto __end = __range.end();     // 同上
    for (; __begin != __end; ++__begin)
    {
        auto &velem = __begin.operator*(); // error! velem被推导为vector<bool>::reference&
        statement
    }
}
```

显然标记了//error!的那一行就产生了错误!因为 `__begin.operator*()` 是一个函数调用,
而函数的返回值是一个右值, 即 `vector<bool>::reference` 的临时对象,
而我们却给它赋值给了左值引用 `vector<bool>::reference&`, 因此会报出编译错误.

那么, 我们如果要修改v的元素该怎么办呢?这是一个令人发指的操作: 
注意到我们的 `vector<bool>::reference是对vector<bool>` 的一个元素的引用(可以理解为指针),
那么我们只需要值传递 `vector<bool>::reference` 即可, 不需要进行引用传递!即代码如下:

```cpp
::std::vector<bool> v(10, false);
for (auto velem : v)
{
    velem = true;
}
::std::cout << (int)v[5] << ::std::endl;
```

输出的结果果然是1!这个操作与其他类型的vector完全不同.
因为我们知道, 把 `vector<bool>` 换成 `vector<int>` 的话:

```cpp
::std::vector<int> v(10, 0);
for (auto velem : v)
{
    velem = 1;
}
::std::cout << (int)v[5] << ::std::endl;
```

它的输出结果显然仍然是0, 因为值传递不1能改变v内元素的值, 而 `vector<bool>` 与我们的习惯完全不同!这是需要特别注意的.

我们回到最开始的问题, 我起初报错的代码就可以改成:

```cpp
#include <vector>
using ::std::vector;
int main()
{
    vector<vector<bool>> v(10, vector<bool>(10, false));
    for (auto &vx : v)
        for (auto velem : vx) // 此处为auto, 并非auto&!
        {
            velem = true;
        }
    return 0;
}
```

这就是 `vector<bool>` 与普通的vector的不同之处.
因此, C++ reference中也警告我们, 我们无法使用 `无特化的bool类型`.
如果这种特化的bool类型不能满足我们的需求,
我们只能用 `char`, `unsigned char` 或其他的封装类型来代替, 或者使用其他的容器如deque来代替.
