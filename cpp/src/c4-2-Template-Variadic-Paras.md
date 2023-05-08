# c++ 可变参数模板

[C++的可变参数模板](https://zhuanlan.zhihu.com/p/104450480)
[省略号和可变参数模板](https://learn.microsoft.com/zh-cn/cpp/cpp/ellipses-and-variadic-templates)

## 语法

`可变参数模板` 以两种方式使用省略号.
在参数名称的左侧, 表示"参数包", 在参数名称的右侧, 将 参数包展开 为单独的名称.

下面是 可变类模板 定义语法的基本示例:

```C++
template<typename... Arguments> class classname;
```

对于参数包和 展开, 可以根据偏好在省略号周围添加空格, 如以下示例所示:

```C++
template<typename ...Arguments> class classname;
```

或以下示例:

```C++
template<typename ... Arguments> class classname;
```

本文使用第一个示例中显示的约定,  (省略号附加到 typename) .

在前面的示例中, `Arguments` 是一个参数包.
类 classname 可以接受可变数量的参数, 如以下示例所示:

```C++
template<typename... Arguments> class vtclass;

vtclass< > vtinstance1;
vtclass<int> vtinstance2;
vtclass<float, bool> vtinstance3;
vtclass<long, std::vector<int>, std::string> vtinstance4;
```

通过使用可变类模板定义, 还可以至少需要一个参数:

```C++
template <typename First, typename... Rest> class classname;
```

下面是 可变函数模板 语法的基本示例:

```C++
template <typename... Arguments> returntype functionname(Arguments... args);
```

然后, 参数 Arguments 包将展开以供使用, 如下一部分所示.

可以采用其他类型的可变函数模板语法, 包括但不限于以下示例:

```C++
template <typename... Arguments> returntype functionname(Arguments&... args);
template <typename... Arguments> returntype functionname(Arguments&&... args);
template <typename... Arguments> returntype functionname(Arguments*... args);
```

也允许使用说明符(如 const):

```C++
template <typename... Arguments> returntype functionname(const Arguments&... args);
```

与可变参数模板类定义一样, 可以生成要求至少一个参数的函数:

```C++
template <typename First, typename... Rest> returntype functionname(const First& first, const Rest&... args);
```

可变参数模板使用 `sizeof...()` 运算符(与旧的 `sizeof()` 运算符无关):

```C++
template<typename... Arguments>
void tfunc(const Arguments&... args)
{
    constexpr auto numargs{ sizeof...(Arguments) };

    X xobj[numargs]; // array of some previously defined type X

    helper_func(xobj, args...);
}
```

## 有关省略号位置的更多信息

此前, 本文介绍以以下形式定义 `参数包`(parameter packs) 和 `展开`(expansions) 的省略号放置:
>在参数名称 `左侧`, 它表示参数包; 而在参数名称`右侧`, 将参数包展开为单独的名称.
虽然从技术上讲如此, 但在翻译到代码时可能会让人困惑.  请注意以下几点:

+ 在模板参数列表 (`template <parameter-list>`) 中, `typename...` 引入了模板参数包.

+ 在 `参数声明子句`(parameter-declaration-clause): `func(parameter-list)` 中,
`顶级` 省略号引入了函数参数包, 并且 `省略号位置` 很重要:

```C++
// v1 is NOT a function parameter pack:
template <typename... Types> void func1(std::vector<Types...> v1);

// v2 IS a function parameter pack:
template <typename... Types> void func2(std::vector<Types>... v2);
```

+ 其中省略号 紧跟 在参数名称的后面, 这样才是一个 `参数包展开`(parameter pack expansion).

## 示例

说明可变函数模板机制的一个好方法是在重写某些功能的 `printf` 重写中使用它:

```C++
#include <iostream>

using namespace std;

void print() {
    cout << endl;
}

template <typename T> void print(const T& t) {
    cout << t << endl;
}

template <typename First, typename... Rest> void print(const First& first, const Rest&... rest) {
    cout << first << ", ";
    print(rest...); // recursive call using pack expansion syntax
}

int main()
{
    print(); // calls first overload, outputting only a newline
    print(1); // calls second overload

    // these call the third overload, the variadic template,
    // which uses recursion as needed.
    print(10, 20);
    print(100, 200, 300);
    print("first", 2, "third", 3.14159);
}
```

## 备注

大多数合并可变函数模板的实现使用某种形式的递归, 但与传统递归略有不同.
传统的递归涉及使用同一签名调用自身的函数.  (它可能会重载或模板化, 但每次选择相同的签名. )
可变参数递归涉及通过使用变化(基本都是减少)
数量的参数来调用可变参数函数模板, 从而每次都清除不同的签名.
仍然需要"基案例", 但递归的性质不同.
