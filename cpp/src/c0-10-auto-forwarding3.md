# 万能引用

[C++11中的通用引用](https://www.yuanguohuo.com/2018/05/25/cpp11-universal-ref/)
[现代C++之万能引用, 完美转发, 引用折叠](https://zhuanlan.zhihu.com/p/99524127)

从语法上来看, 声明右值引用看起来和声明 `普通引用` 很像
--后者现在被称为左值引用(lvalue references), 只不过你需要用 `&&` 而不是 `&`.
下面这个函数需要一个类型为rvalue-reference-to-Widget:的参数:

void f(Widget&& param);
假设右值引用是使用&&声明的, 那么假设类型声明中出现&& 表示右值引用似乎是合理的. 事实并非如此:

```cpp
Widget&& var1 = someWidget;      // here, "&&" means rvalue reference

auto&& var2 = var1;              // here, "&&" does not mean rvalue reference

template<typename T>
void f(std::vector<T>&& param);  // here, "&&" means rvalue reference

template<typename T>
void f(T&& param);               // here, "&&"does not mean rvalue reference
```

在本文当中, 我会对类型声明中 "&&" 可能具有的两种含义进行阐释, 讲解如何区分它们,
并且会引入一个新术语以便在交流的时候清楚的表明在当前说的"&&"是哪种含义.
正确的区分这两种含义非常重要,
因为如果你看到 `&&` 就以为是右值引用的话, 你可是会误读很多c++11代码的.

## 引入万能引用

这个问题的本质实际上是, 类型声明当中的"&&"有的时候意味着rvalue reference,
但有的时候意味着rvalue reference 或者 lvalue reference.
因此, 源代码当中出现的 "&&" 有可能是 "&" 的意思,
即是说语法上看着像rvalue reference ("&&"), 但实际上却代表着一个lvalue reference ("&").
在这种情况下, 此种引用比lvalue references 或者 rvalue references都要来的更灵活.

Rvalue references只能绑定到右值上, lvalue references除了可以绑定到左值上,
在某些条件下还可以绑定到右值上.
这里某些条件绑定右值为: 常左值引用绑定到右值, 非常左值引用不可绑定到右值!

例如:

```cpp
string &s = "asd";  // error
const string &s = "asd";  // ok
```

规则简化如下:

```cpp
左值引用   == {左值}
右值引用   == {右值}
常左值引用 ==  {右值}
```

相比之下, 声明中带 "&&" 的, 可能是lvalue references 或者 rvalue references 的引用可以绑定到任何东西上.
这种引用灵活也忒灵活了, 值得单独给它们起个名字.
我称它们为 universal references(万能引用或转发引用, 通用引用).

拓展: 在资料[6]中提到了const的重要性!

例如:

string f() { return "abc"; }
​
void g() {
    const string &s = f();       // still legal?
    cout << s << endl;
}
上面g函数中合法?

答案是合法的, 原因是s是个左值, 类型是常左值引用, 而f()是个右值,
前面提到常左值引用可以绑定到右值!所以合法, 当然把const去掉, 便是不合法!
