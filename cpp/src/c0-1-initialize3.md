# cpp initialize 3

[初始化](https://zh.cppreference.com/w/cpp/language/initialization)

根据上下文, `initializer` 可能导致:

+ 值初始化, Value initialization, 例如 `std::string s{};`,
+ 直接初始化, Direct initialization, 例如 `std::string s("hello");`
+ 复制初始化, Copy initialization,例如 `std::string s = "hello";`
+ 列表初始化, List initialization,  例如 `std::string s{'a', 'b', 'c'};`
+ 聚合初始化, Aggregate initialization, 例如 `char a[3] = {'a', 'b'};`
+ 引用初始化, Reference initialization, 例如 `char& c = a[0];`

+ 如果不提供初始化器, 那么就会应用默认初始化的规则.

```cpp
T 对象;
new T
new T()  // C++03 前
```

## 列表初始化

[List-initialization](https://en.cppreference.com/w/cpp/language/list_initialization)
[Can std::initializer_list be specialized?](https://stackoverflow.com/questions/62010405/can-stdinitializer-list-be-specialized)

Otherwise, if T is a specialization of std::initializer_list, 
the T object is direct-initialized or copy-initialized, depending on context, 
from a prvalue of the same type initialized from (until C++17) the braced-init-list.
指的就是:

```cpp
std::initializer_list<int> i3 = { 1, 2, 3 };
```

[Working Draft, Standard for Programming Language C++](https://eel.is/c++draft/)
[Broyden's method](https://en.wikipedia.org/wiki/Broyden%27s_method)
