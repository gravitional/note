# auto类型推导

[auto类型推导](https://www.cnblogs.com/0xfffffff0/p/10285472.html)
[auto&&, 万能引用和完美转发](https://zhuanlan.zhihu.com/p/435689642?utm_id=0)
[C++(二): 如何确定表达式的值类型](https://zhuanlan.zhihu.com/p/435605194)

## 左值 or 右值

到底什么时候是 `左值`? 什么时候是 `右值`?是不是有点混乱?
在 C++ 中, 每个表达式(expression)都有两个特性:

+ has identity? —— 是否有唯一标识, 比如 `地址`, `指针`.
有唯一标识的表达式在 C++ 中被称为 `glvalue`(generalized lvalue),
为了方便记忆, 不妨称之为 `符号值`(symbol value).

+ can be moved from? —— 是否可以安全地移动(编译器), 既能够转移 `所有权`.
可以安全地移动的表达式在 C++ 中被成为 `rvalue`(右值).
可以通过等号(`=`), 转移所有权给左边变量.

根据这两个特性, 可以将表达式分成 4 类:

+ 唯一, 不能移动 - 这类表达式在 C++ 中被称为 `lvalue`.
+ 唯一, 可以移动 - 这类表达式在 C++ 中被成为 `xvalue`(expiring value).
+ 不唯一, 可以移动 - 这类表达式在 C++ 中被成为 `prvalue`(pure rvalue).
+ 不唯一, 不可移动 -C++ 中不存在这类表达式.

简单总结一下这些 value categories 之间的关系:

可以移动的值都叫 `rvalue`, 包括 `xvalue` 和 `prvalue`.
有唯一标识的值都叫 `glvalue`, 包括 `lvalue` 和  `xvalue`.
`std::move` 的作用就是将 `lvalue` 转换成 `xvalue`.

![C-expression](https://ask.qcloudimg.com/http-save/7176906/t89kd9ja5y.png?imageView2/2/w/1620)

对于一个变量 `A`, `A` 的类型是 C++ 语言中解析的类型,
`A` 的值类型是 `A` 的底层实现类型, 如 `int&`.

### 表达式类型和值类型

在编程语言中, 变量 `a`(标识符) 可以 `绑定`(bind) 到另一个的表达式上 `expr`
(可以简单也可以复杂), 例如:

```cpp
int a=10;
const int& b=10;
std::string a= "Hello, world";

const int &b = 5; // 指向常量的 左值引用
int &&c = 5; // 指向右值的 右值引用
c=10; // c 指向的内容变成 10

void g() {}; //定义一个函数
void (&a)() = g; //指向函数的 左值引用
void (&&a)() = g; //指向函数的 右值引用
```

+ 变量 `a` 的 `表达式类型`(expr type), 指的是 `a` 绑定的内容(指向的目标)的类型, 例如:
    + `int a=10;`, `a` 的 expr type 是 `int`;
    + `const int& b=10`,  `b` 的 expr type 是 `const int&`;
    + `int&& c = 5;`, `c` 的 expr type 是 `int&&`.

+ 而变量 `a`  的 `值类型`(value type), 指的是变量 `a` 本身的 `lvalue`, `rvalue`, `xvlaue` 分类.
    + `引用`(reference) 可以看成 `变量的别名`, 反过来, 普通变量也可以看成是 `引用`.
    只不过 `解引用` 过程是编译器通过 `符号表` 自动完成的.
    + `引用` 的底层是通过 `指针`(pointer) 实现的, `指针` 是存储内存地址的数据类型,
    通常定义的 `指针` 具有唯一标识符, 不可移动, 所以是 `lvalue`,
    + 所以 `普通变量`,  `指针`, `引用` 的 `值类型` 都是  `lvalue`.

+ 函数的 `左值引用` 和 `右值引用` 重载, 依赖的是 `函数参数`(argument) 的 `值类型`, 而不是 `表达式类型`.
其中 `rvalue` 被隐含地转换成 `xvalue`(唯一标识符+可以改变所有权),
所以参数类型要么是 `lvalue`, 要么是 `xvalue`.

## auto&&,万能引用和完美转发

>我们在考虑表达式的 `值类型` 是什么时, 真的是在关心它的 `value type`吗?
>不, 我们并不需要关心它的 `value type`.
>真正需要关注的, 是这个表达式所持有的资源, 能不能被偷取.

在上一篇文章中, 介绍了C++中一共有三种值类型,
它们分别为 `左值`, `纯右值` 和 `将亡值`.

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

+ 第一步; 合并两个重载函数变为一个函数模板, 并引入万能引用T&&.
万有引用中的T, 推导的是表达式的值类型,
当传入左值时, T推导为T&, 并且a的类型变为T&;
当传入一个将亡值时, T推导为T&&, 并且a的类型变为T&&.
所以最后a的类型只与传入表达式的值类型有关,
但是不管传入表达式的值类型是什么, a的值类型都会变为左值.

+ 这其中会有一个 `引用折叠`, 即如果传入的 `a`本身就是左值/右值引用,
则 `& &&` 会变为 `&`; `&& &&` 会变为 `&&`, 因此最终不会产生引用的引用.

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

+ 第二步 将a的值类型与表达式的值类型进行同步, 在第一步中, 我们已经知道, 传入的表达式的值类型变为了a的类型.
那么我们就可以利用decltype来获取值类型, 并且利用static_cast<decltype(a)>(a)来将a的类型转化为a的值类型.

```cpp
template<typename T>
void f(T&& a) {
    if constexpr(is_lvalue<
        decltype((
            static_cast<decltype(a)>(a) // 根据 a 的 expr type, 更新 a 的type
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

到此, 我们得到了想要的结果. 其中 `static_cast<decltype(a)>(a)`
和标准库中的 完美转发函数 `std::forward<T>(a)` 的原理是一样的.

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

这个 `T` 和就是模板中的 `T`, 我们知道 `T` 中保存的是传入表达式的 `值类型`,
所以, `static_cast<T&&>(a)` 的作用就是将 `a` 的值类型转化为传入表达式的 `值类型`.
这与表达式 `static_cast<decltype(a)>(a)` 所做的是同一件事.

### auto&& + 完美转发

`auto&&` 和上面模板的原理是一样的,
区别只是 `auto` 取代了 `T`, 来推导 `值类型`.

```cpp
void g() {
    auto&& a = 10;
    std::cout << type_to_string<
        decltype(a) // a的 类型, int&&
        >() << std::endl;
    std::cout << type_to_string<
        decltype((a)) // a的 值类型, int&
        >() << std::endl;
    if constexpr(is_lvalue<decltype(
        (static_cast<decltype(a)>(a)) //
        )>){
        std::cout << "lvalue" << std::endl;
    }else if constexpr(is_xvalue<decltype(
        (static_cast<decltype(a)>(a))
        )>){
        std::cout << "xvalue" << std::endl;
    }
}
输出

// a的类型
int&&
// a的值类型为lvalue
int&
// 经过完美转发后a的值类型重新变为了xvalue
xvalue
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
