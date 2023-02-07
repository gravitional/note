# 确定表达式的值类型

[C++(二): 如何确定表达式的值类型](https://zhuanlan.zhihu.com/p/435605194)
[现代 C++: 右值引用, 移动语意, 完美转发](https://cloud.tencent.com/developer/article/1637076)

`右值引用`(rvalue reference)是 C++11 为了实现
`移动语义`(move semantic)和 `完美转发`(perfect forwarding)而提出来的.

`右值引用`, 简单说就是绑定在 `右值` 上的 `引用`.
`右值` 的内容可以直接 `移动`(move)给 `左值对象`, 而不需要进行开销较大的 `深拷贝`(deep copy).

## 移动语义

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

## 完美转发

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
