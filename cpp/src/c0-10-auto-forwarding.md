# auto类型推导

[auto类型推导](https://www.cnblogs.com/0xfffffff0/p/10285472.html)
[auto&&, 万能引用和完美转发](https://zhuanlan.zhihu.com/p/435689642?utm_id=0)

## auto&&,万能引用和完美转发

[C++11右值引用](https://zhuanlan.zhihu.com/p/141412154)

对于一个函数, 由于每个参数的 `value type` 可能是 左值引用 或 右值引用,
针对所有可能的左右值引用组合, 特化所有模板是不现实的.
为解决这个问题, 有时候符号 `&&` 并不一定代表 `右值引用`, 它也可能是 `左值引用`,
这叫做 `通用引用`(universal reference), 不过这种情况仅发生在 `模板参数类型` 或 `auto推导`.

>我们在考虑表达式的 `值类型` 是什么时, 真的是在关心它的 `value type`吗?
>不, 我们并不需要关心它的 `value type`.
>真正需要关注的, 是这个表达式所持有的资源, 能不能被偷取.

C++中一共有三种 `value type`, 它们分别为 `左值`, `纯右值` 和 `将亡值`.

对于一个 `纯右值` 而言, 它的 `生存周期` 只有 `一行代码`,
为了延长它的 `生存周期`, 我们可以用一个 `右值引用` 将其绑定,
一旦绑定它就变成了一个左值, 虽然类型为 `T&&`.
这样就产生了一个疑问, 函数重载是依据 `value type` 还是 `expr type`(表达式类型)?

```cpp
void f(int &&a) { //右值引用重载
    std::cout << "overload: rvalue" << std::endl;
}

void f(int &a) { //左值引用重载
    std::cout << "overload: lvalue" << std::endl;
}

void g(int &&a) {
    std::cout << std::boolalpha;
    std::cout << "The type of a : " << type_to_string<decltype(a)>() << std::endl;
    std::cout << "The value type of a is lvalue : " << is_lvalue<decltype((a))> << std::endl;
    f(a);
}

// a 的 expr 类型是 int&&, 但 int&& 本身的 value type 还是 lvalue
g(10);

//输出:

The type of a : int&&
The value type of a is lvalue : true
overload: lvalue
```

从上述输出中可以看出, 函数的 `左值引用` 和 `右值引用` 重载,
所依据的是 `value type`, 而不是 `arg type`.
其中, `纯右值` 会先隐式转换为 `将亡值`, 再选择重载的函数.
所以, 我们最后需要考虑的情况就分两种, 传入的表达式为 `lvalue` 或者 `xvalue`:

+ 函数参数接受的表达式是 `xvalue`, 对于这种表达式而言, 我们可以偷取它所持有的资源.
+ 函数参数接受的表达式是 `lvalue`, 对于这种表达式而言, 我们不能偷取它所持有的资源.

接下来的任务, 就是区分出传入的表达式, 是 `左值` 还是 `将亡值`, 一共有三方法:

+ 上述例子中函数的 `左值引用` 和 `右值引用` 重载
+ `模板方法` + `万能引用` + `完美转发`
+ `auto&&` + `完美转发`

### 模板方法+万能引用+完美转发

有了上述一的方法, 很自然的我们可以想到模板的方法,
将两个重载函数合为一个函数模板.

+ 第一步; 合并两个 `重载函数` 变为 `函数模板`, 并引入万能引用 `T&&`.
万有引用中的 `T`, 推导的是 `传入表达式` 的 `值类型`,
当传入 `左值` 时, `T` 推导为 `T&`, 并且 `a` 的 expr类型 变为 `T&`(左值);
当传入 `将亡值` 时, `T` 推导为 `T&&`, 并且 `a` 的 expr类型 变为 `T&&`(右值).
所以最后 `a的类型` 只与 `传入表达式` 的 `value type` 有关,
但是不管传入表达式的值类型是什么, `a` 的 `value type` 都会变为 lvalue(左值).

+ 这其中会有一个 `引用折叠`, 即如果传入的 `a`本身就是 `左值`/`右值引用`,
则 `& &&` 会变为 `&`; `&& &&` 会变为 `&&`, 因此最终不会产生 `引用的引用`,
后者在 C++ 中是不允许存在的.

```cpp
template<typename T>
void f(T&& a) {
    if constexpr(is_lvalue<decltype((a))>){
        std::cout << "overload: lvalue" << std::endl;
    }else if constexpr(is_xvalue<decltype((a))>){
        std::cout << "overload: xvalue" << std::endl;
    }
}
```

输出:

```out
The type of a : int&&
The value type of a is lvalue : true
overload: lvalue
```

+ 第二步; 将 `a` 的 `值类型` 与 `a`的 expr type(即传入表达式的 value type)进行同步,
在第一步中, 我们已经知道, `传入表达式的值类型` 变为了 `a` 的 expr类型.
那么我们就可以利用 `decltype` 来获取 `a`的 expr类型,
并且利用 `static_cast<decltype(a)>(a)` 来将 `a` 的 `值类型` 同步到 `a` 的 `expr type`.

```cpp
template<typename T>
void f(T&& a) {
    if constexpr(is_lvalue<
        decltype((
            static_cast<decltype(a)>(a) // 用a的 expr type, 更新a的 value type
            ))>){
        std::cout << "overload: lvalue" << std::endl;
    }else if constexpr(is_xvalue<
                decltype((
                    static_cast<decltype(a)>(a) // decltype 见 ZhengLi,P55, 用于编译期获取expr类型
                    ))>){
        std::cout << "overload: xvalue" << std::endl;
    }
}
```

调用

```cpp
int a = 20;
f(10);
f(a);
// 输出:
overload: xvalue
overload: lvalue
```

到此, 我们得到了想要的结果. 其中 `static_cast<decltype(a)>(a)` 和标准库中的
完美转发函数 `std::forward<T>(a)` 的原理是一样的.

### `std::forward<T>(a)`

```cpp
template<typename T>
void f1(T&& a) {
    if constexpr(is_lvalue<decltype((
        std::forward<T>(a)
        ))>){
        std::cout << "lvalue" << std::endl;
    }else if constexpr(is_xvalue<decltype((
        std::forward<T>(a)
        ))>){
        std::cout << "xvalue" << std::endl;
    }
}
```

这个函数也能得到我们想要的结果, 而它的原理是:

```cpp
static_cast<T&&>(a)
```

这个 `T` 就是上面模板中的 `T`, 我们知道 `T` 中保存的是 `传入表达式` 的 `value type`,
所以, `static_cast<T&&>(a)` 的作用就是将 `a` 的 `value type` 转化为 `传入表达式` 的 `value type`.
这与表达式 `static_cast<decltype(a)>(a)` 所做的是同一件事.

### auto&& + 完美转发

`auto&&` 和上面模板的原理是一样的,
区别只是 `auto` 取代了 `T`, 来推导 `值类型`.

```cpp
void g() {
    auto&& a = 10;
    std::cout << "The expr type of a: " << type_to_string<
        decltype(a) // a的 expr type, int&&
        >() << std::endl;
    std::cout << "The value type of a: " << type_to_string<
        decltype((a)) // a的 value type, int&
        >() << std::endl;
    if constexpr(is_lvalue<decltype(
        (static_cast<decltype(a)>(a)) //
        )>){
        std::cout << "overload: lvalue" << std::endl;
    }else if constexpr(is_xvalue<decltype(
        (static_cast<decltype(a)>(a))
        )>){
        std::cout << "overload: xvalue" << std::endl;
    }
}

输出:

```bahs
The expr type of a: int&&
The value type of a:  int&
// 经过完美转发后 a的value type重新变成xvalue, 后续重载和 输入表达式10相同
overload: xvalue
```

## 确定表达式的值类型

[C++(二): 如何确定表达式的值类型](https://zhuanlan.zhihu.com/p/435605194)

## another doc

[现代 C++: 右值引用, 移动语意, 完美转发](https://cloud.tencent.com/developer/article/1637076)

`右值引用`(rvalue reference)是 C++11 为了实现
`移动语义`(move semantic)和 `完美转发`(perfect forwarding)而提出来的.

`右值引用`, 简单说就是绑定在 `右值` 上的 `引用`.
`右值` 的内容可以直接 `移动`(move)给 `左值对象`, 而不需要进行开销较大的 `深拷贝`(deep copy).

### 移动语义

下面这个例子:

 `v2 = v1` 调用的是 `拷贝赋值` 操作符, `v2` 复制了 `v1` 的内容 —— 复制语义.
 `v3 = std::move(v1)`  调用的是 `移动赋值` 操作符, 将 `v1` 的内容移动给 `v3` —— 移动语义.

```cpp
std::vector<int> v1{1, 2, 3, 4, 5};
std::vector<int> v2;
std::vector<int> v3;

v2 = v1;
std::cout << v1.size() << std::endl;  // 输出 5
std::cout << v2.size() << std::endl;  // 输出 5

v3 = std::move(v1); // move
std::cout << v1.size() << std::endl;  // 输出0
std::cout << v3.size() << std::endl;  // 输出 5
```

为了实现 `移动语义`, C++ 增加了与 `拷贝构造函数`(copy constructor)和 `拷贝赋值操作符`(copy assignment operator)
对应的 `移动构造函数`(move constructor)和 `移动赋值` 操作符(move assignment operator),
通过 `函数重载` 机制, 来确定应该调用 `拷贝语义` 还是 `移动语义`
(参数是 `左值引用` 就调用 拷贝语义;参数是 右值引用 就调用 移动语义).

再来看一个简单的例子:

```cpp
#include <iostream>
#include <string>
#include <vector>

class Foo {
 public:
  // 默认构造函数
  Foo() { std::cout << "Default Constructor: " << Info() << std::endl; }

  // 自定义构造函数
  Foo(const std::string& s, const std::vector<int>& v) : s_(s), v_(v) {
    std::cout << "User-Defined Constructor: " << Info() << std::endl;
  }

  // 析构函数
  ~Foo() { std::cout << "Destructor: " << Info() << std::endl; }

  // 拷贝构造函数
  Foo(const Foo& f) : s_(f.s_), v_(f.v_) {
    std::cout << "Copy Constructor: " << Info() << std::endl;
  }

  // 拷贝赋值操作符
  Foo& operator=(const Foo& f) {
    s_ = f.s_;
    v_ = f.v_;
    std::cout << "Copy Assignment: " << Info() << std::endl;
    return *this;
  }

  // 移动构造函数
  Foo(Foo&& f) : s_(std::move(f.s_)), v_(std::move(f.v_)) {
    std::cout << "Move Constructor: " << Info() << std::endl;
  }

  // 移动赋值操作符
  Foo& operator=(Foo&& f) {
    s_ = std::move(f.s_);
    v_ = std::move(f.v_);
    std::cout << "Move Assignment: " << Info() << std::endl;
    return *this;
  }

  std::string Info() {
    return "{" + (s_.empty() ? "'empty'" : s_) + ", " +
           std::to_string(v_.size()) + "}";
  }

 private:
  std::string s_;
  std::vector<int> v_;
};

int main() {
  std::vector<int> v(1024);

  std::cout << "================ Copy =======================" << std::endl;
  Foo cf1("hello", v);
  Foo cf2(cf1);  // 调用拷贝构造函数
  Foo cf3;
  cf3 = cf2;  // 调用拷贝赋值操作符

  std::cout << "================ Move =========================" << std::endl;
  Foo f1("hello", v);
  Foo f2(std::move(f1));  // 调用移动构造函数
  Foo f3;
  f3 = std::move(f2);  // 调用移动赋值操作符
  return 0;
}
```

简单封装了一个类 `Foo`, 重点是实现:

拷贝语意: 拷贝构造函数 `Foo(const Foo&)`, 拷贝赋值操作符 `Foo& operator=(const Foo&)`.
移动语意: 移动构造函数 `Foo(Foo&&)`, 移动赋值操作符 `Foo& operator=(Foo&&)`.
拷贝语意相信大部分人都比较熟悉了, 也比较好理解.
在这个例子中, 每次都会拷贝 `s_` 和 `v_` 两个成员, 最后 `cf1`, `cf2`, `cf3` 三个对象的内容都是一样的.

每次执行移动语意, 是分别调用 `s_` 和 `v_` 的移动语意函数——理论上只需要对内部指针进行修改, 所以效率较高.
执行移动语意的代码片段了出现了一个标准库中的函数 `std::move` —— 它可以将参数强制转换成一个右值.
本质上是告诉编译器, 我想要 `move` 这个参数——
最终能不能 `move` 是另一回事——可能对应的类型没有实现移动语意, 可能参数是 `const` 的.

有一些场景可能拿到的值直接就是右值, 不需要通过 `std::move` 强制转换, 比如:

```cpp
Foo GetFoo() {
  return Foo("GetFoo", std::vector<int>(11));
}
....
Foo f3("world", v3);
....
f3 = GetFoo(); // GetFoo 返回的是一个右值, 调用移动赋值操作符
```

### 完美转发

C++ 通过了一个叫 `std::forward `的函数模板来实现完美转发.
这里直接使用 Effective Modern C++ 中的例子作为说明.
在前面的例子上, 我们增加如下的代码:

```cpp
// 重载判断: 接受 const 左值引用
void Process(const Foo& f) {
  std::cout << "lvalue reference" << std::endl;
  // ...
}

// 重载判断: 接受右值引用
void Process(Foo&& f) {
  std::cout << "rvalue reference" << std::endl;
  // ...
}

template <typename T>
void LogAndProcessNotForward(T&& a) {
  std::cout << a.Info() << std::endl;
  Process(a);
}

template <typename T>
void LogAndProcessWithForward(T&& a) {
  std::cout << a.Info() << std::endl;
  Process(std::forward<T>(a)); // 区别在于调用了 std::forward
}

LogAndProcessNotForward(f3);             // 输出 lvalue reference
LogAndProcessNotForward(std::move(f3));  // 输出 lvalue reference

LogAndProcessWithForward(f3);            // 输出 lvalue reference
LogAndProcessWithForward(std::move(f3)); // 输出 rvalue reference
```

+ `LogAndProcessNotForward(f3);` 和 `LogAndProcessWithForward(f3);` 都输出 `lvalue reference`,
这一点都不意外, 因为 `f3` 本来就是左值.
+ `LogAndProcessNotForward(std::move(f3));` 输出 `lvalue reference` 是因为,
虽然参数 `a` 绑定到右值, 但是参数 `a` 本身是左值.
+ `LogAndProcessWithForward(std::move(f3));` 使用了 `std::forward` 对参数进行转发,
`std::forward` 的作用就是: 当参数绑定到 `右值` 时, 就将参数转换成 `右值`.

## R字符串原始字面量

[C++11 R字符串原始字面量](https://blog.csdn.net/sandrew0916/article/details/109525562)

`原始字符串字面量` 的定义为:

```cpp
R "xxx(raw string)xxx"
```

其中, 原始字符串必须用括号 `()` 括起来,
括号的前后可以加其他字符串 `xxx`, 所加的字符串会被忽略,
并且 `xxx` 必须在括号两边同时出现.

```cpp
#include <iostream>
#include <string>

int main()
{
    // 一个普通的字符串, '\n'被当作是转义字符, 表示一个换行符.
    std::string normal_str = "First line.\nSecond line.\nEnd of message.\n";
    //---输出:
    First line.
    Second line.
    End of message.

    // 一个raw string, '\'不会被转义处理. 因此, "\n"表示两个字符: 字符反斜杠 和 字母n.
    std::string raw_str = R"(First line.\nSecond line.\nEnd of message.\n)";
    //---输出:
    //First line.\nSecond line.\nEnd of message.\n

    std::cout << normal_str << std::endl;
    std::cout << raw_str << std::endl;
    std::cout << R"foo(Hello, world!)foo" << std::endl;
    //---输出:
    //Hello, world!

    // raw string可以跨越多行, 其中的空白和换行符都属于字符串的一部分.
    std::cout <<R"(
                   Hello,
                   world!
                   )" << std::endl;
    //---输出:

    //              Hello,
    //              world!

    return 0;
}
```
