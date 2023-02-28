# C++ typename

[C++typename的由来和用法](https://zhuanlan.zhihu.com/p/335777990)

那么问题来了, 什么情况下, `class` 定义之后, 编译不能通过呢?

```cpp
template<typename T>
void fun(const T& proto){
    T::const_iterator it(proto.begin());
}
```

发生编译错误是因为编译器不知道 `T::const_iterator` 是个 `type`. 万一它是个变量呢?
`T::const_iterator` 的解析有着逻辑上的矛盾:
直到确定了 `T` 是什么东西, 编译器才会知道 `T::const_iterator` 是不是 ` type`;
然而当模板被解析时, `T` 还是不确定的.
这时我们声明它为 `type` 才能通过编译:

委员会决定引入一个新的关键字, 这就是 `typename`.
千呼万唤始出来, 我们来看看C++标准:

对于用于模板定义的依赖于 `模板参数` 的名称, 只有在实例化的参数中存在这个类型名,
或者这个名称前使用了 `typename` 关键字来修饰, 编译器才会将该名称当成是 `类型`.
除了以上这两种情况, 绝不会被当成是类型.

因此, 如果你想直接告诉编译器 `T::const_iterator` 是 type 而不是 variable, 只需用 `typename` 修饰:

```cpp
typename    T::const_iterator it(proto.begin());
```

这样编译器就可以确定 `T::const_iterator` 是 type,
而不再需要等到实例化时期才能确定, 因此消除了前面提到的歧义.

## 嵌套从属类型

事实上类型 `T::const_iterator` 依赖于 `模板参数T`,
模板中依赖于 `模板参数` 的名称称为 `从属名称`(dependent name),
当 `从属名称` 嵌套在 `类` 里面时, 称为 `嵌套从属名称`(nested dependent name).
其实 `T::const_iterator` 还是 `嵌套从属类型名称`(nested dependent type name).

嵌套从属名称是需要用 `typename` 声明的, 其他的名称是不可以用 `typename` 声明的.
比如下面是一个合法的声明:

```cpp
template<typename T>
void fun(const T& proto ,typename  T::const_iterator it);
```
