# std::initializer_list

勿与成员初始化器列表混淆

在 header `<initializer_list>` 定义

```cpp
template< class T >
class initializer_list;(C++11 起)
```

`std::initializer_list<T>` 类型对象是 轻量代理对象(lightweight proxy object),
提供对 `const T` 类型对象的 `数组` 的访问.

`std::initializer_list` 对象在这些时候自动构造:

+ 用 `花括号初始化列表` list-initialize 对象, 对应的 `构造函数` 接受 `std::initializer_list` 参数
+ 以 `花括号初始化列表` 赋值的右运算数, 或 `函数调用参数`, 
而对应的 `赋值运算符`/函数 接受 std::initializer_list 参数
+ 在 ranged for 循环中使用, `花括号初始化列表` 被绑定到 `auto`

`initializer_list` 可由 `一对指针` 或 `指针与长度` 实现. 
复制 `std::initializer_list` 不会复制其底层对象.

底层数组是 `const T[N]` 类型的 `临时数组`, 
其中每个元素都从 `原始初始化列表` 的对应元素 `复制初始化`
(除非窄化转换非法, except that narrowing conversions are invalid). 
底层数组的生存期与任何其他 `临时对象` 相同, 
除了从 `array` 初始化 initializer_list 对象会延长 `array` 的生存期, 
恰如 [绑定引用到临时量](https://en.cppreference.com/w/cpp/language/reference_initialization#Lifetime_of_a_temporary)
(有例外, 例如对于初始化非静态类成员, initializing a non-static class member). 
底层数组可以分配在 `只读内存`.

若声明了 `std::initializer_list` 的显式特化 或偏特化, 则程序为谬构(ill-formed).
