# c++ 结构化绑定

[[现代C++]使代码简洁的两种解包方式](https://blog.csdn.net/qq_17291647/article/details/116280091)

## std::tie

C++11通过标准库的方式提供, 用于 `std::tuple`, `std::pair` 类型的数据解包.
函数签名为:

```cpp
#include <tuple>
template< class... Types >
tuple<Types&...> tie( Types&... args ) noexcept;
```

从参数和返回值类型上能大体猜到这个函数的工作原理:
它实际上将绑定变量通过引用方式传入 `tie` 函数, `tie` 函数将这些参数组装成一个tuple,
等号右侧的tuple的整体赋值给这个tuple, 从而实现tuple每个位置的值都绑定到相应的变量上.

使用方式也比较简单, 先声明绑定变量, 然后将这些变量传入 `tie` 函数:

```cpp
int i;
double d;
bool b;
std::tie(i, d, b) = t;
```

代码已经有点Haskell的味道了, 虽然解包的代码少了, 但绑定的变量需要事先声明, 显得代码不少.
在Haskell中, 如果不想捕获某个值, 可以使用wildcard.
如, 程序可能用不到第2个值, 可以这么写:

```cpp
let (i, _, b) = t
```

`std::tie` 也提供了类似的功能, 使用 `std::ignore` 代替变量即可:

```cpp
int i;
bool b;
std::tie(i, std::ignore, b) = t;
```

特别说明下, `tie` 函数也可以对 `std::pair` 解包,
因为 `std::tuple` 重载了赋值操作符, 所以赋值操作符右侧为pair的情况也能处理.

```cpp
template< class U1, class U2 >
constexpr tuple& operator=( const pair<U1,U2>& p );

template< class U1, class U2 >
constexpr tuple& operator=( pair<U1,U2>&& p );
```

`tie` 函数使用起来比较简单, 但也没多大亮点.
仅仅采用标准库实现的, 限制还是比较多的:

+ 绑定变量必须先定义: 某些类型的变量可能不太容易声明, 这样就很难使用tie获取元素.
+ 适用范围窄: 仅用于tuple和pair
+ 只有拷贝语义: 解包时, 右侧的元素值只能复制给左侧的值, 如果对象较大, 复制成本会比较高.

## Structured Bindings

C++17中引入的一个语言特性, 叫做Structured Binding, 有了它, 就不必用std::tie了.
这一部分主要介绍这个新的语言特性的用法, 通过示例代码给出.

这个语法支持三种类型的结构的绑定, 下面一一说明:

## 绑定数组的元素

C++主要有两种数组类型:

+ 继承自C语言的原始数组
+ C++11标准库引入的 `std::array` 类型

都支持结构绑定:

```cpp
int rawArray[] = {1, 2};
std::array array {3, 4};

auto [x, y] = rawArray; // x=1, y=1
auto [a, b] = array;    // a=3, b=4
```

结构绑定配合auto关键字使用, 方括号内的是元素被绑定的变量名, 与数组的元素个数一致.
上例中的绑定相当于将数组元素直接拷贝到对应的变量里, 语义跟普通的赋值操作是一样的.
普通的变量能支持const, volatile等修饰符, 而且支持引用(&, &&), 结构绑定同样支持.

```cpp
std::array<std::string> arr {std::string("a"), std::string("b")};
const auto& [a, b] = arr;    // a="a", b="b"
```

上例中, a和b是常量字符串的引用类型.
注意 `const`, `&` 和 `auto` 三者之间的位置, 可以根据实际需要省略 `const`, `&`,
语义跟普通的变量是一致的, 在后续的示例中就不突出这些用法了.

### 绑定tuple-like类型的元素

tuple-like类型是指行为跟tuple一致的类型, 怎么才算行为一致, 这里留个坑, 后续填.
现在只需知道标准库的 `std::tuple` 和 `std::pair` 肯定是 `tuple-like` 类型即可.

用法示例如下:

```cpp
auto p = std::make_pair(1, 2);
auto t = std::make_tuple(3, 4);

auto [x, y] = p;  // x=1, y=1
auto [a, b] = t;  // a=3, b=4
```

### 绑定类的数据成员

还能绑定结构体数据成员, 需要保证被绑定的数据成员是public的.

```cpp
struct Person {
    std::string name;
    int age;
};

Person p {"name", 66};
auto [n, a] = p;  // n="name", a=66
```

是不是很简单.
