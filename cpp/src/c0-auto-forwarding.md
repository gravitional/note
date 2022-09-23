# auto类型推导

[auto类型推导](https://www.cnblogs.com/0xfffffff0/p/10285472.html)
[auto&&, 万能引用和完美转发](https://zhuanlan.zhihu.com/p/435689642?utm_id=0)
[C++(二): 如何确定表达式的值类型](https://zhuanlan.zhihu.com/p/435605194)

## auto&&, 万能引用和完美转发

>我们在考虑表达式的 `值类型` 是什么时, 真的是在关心它的 `value type`吗?
>不, 我们并不需要关心它的 `value type`.
>真正需要关注的, 是这个表达式所持有的资源, 能不能被偷取.

在上一篇文章中, 介绍了C++中一共有三种值类型,
它们分别为 `左值`, `纯右值` 和 `将亡值`.

对于一个 `纯右值` 而言, 它的生存周期只有 `一行代码`,
为了延长它的生存周期, 我们可以用一个 `右值引用` 将其绑定,
一旦绑定它就变成了一个左值, 虽然类型为 `T&&`.
这样就产生了一个疑问, 函数重载是依据 `value type` 还是 `arg type`(声明类型)?
也就是实参类型, 还是 形参类型?

```cpp
void f(int &&a) { //右值引用重载
    std::cout << "rvalue" << std::endl;
}

void f(int &a) { //左值引用重载
    std::cout << "lvalue" << std::endl;
}

void g(int &&a) {
    std::cout << std::boolalpha;
    std::cout << "The type of a : " << type_to_string<decltype(a)>() << std::endl;
    std::cout << "The value type of a is lvalue : " << is_lvalue<decltype((a))> << std::endl;
    f(a);
}

g(10);

//输出:

The type of a : int&&
The value type of a is lvalue : true
lvalue
```

从上述输出中可以看出, 函数的 `左值引用` 和 `右值引用` 重载,
所依据的是 `value type`, 而不是 `arg type`.
其中, `纯右值` 会先隐式转换为 `将亡值`, 再选择重载的函数.
所以, 我们最后需要考虑的情况就分两种, 传入的表达式为 `lvalue` 或者 `xvalue`:

+ 函数参数接受的表达式是 `xvalue`, 对于这种表达式而言, 我们可以偷取它所持有的资源.
+ 函数参数接受的表达式是 `lvalue`, 对于这种表达式而言, 我们不能偷取它所持有的资源.

接下来的任务, 就是区分出传入的表达式, 是 `左值` 还是 `将亡值`, 一共有三方法:

+ 上述例子中函数的 `左值引用` 和 `右值引用` 重载
+ 模板方法+万能引用+完美转发
+ `auto&&` + 完美转发

### 模板方法+万能引用+完美转发

有了上述一的方法,
很自然的我们可以想到模板的方法, 将两个重载函数合为一个函数模板.

+ 第一步; 合并两个重载函数变为 `函数模板`, 并引入万能引用`T&&`.
万能引用中的 `T`, 推导的是 `argument expr` 的 `value type`,
当传入 `左值` 时, `T` 推导为 `T&`, 并且函数体中 `a` 的 `type` 变为 `T&`;
当传入 `将亡值` 时, `T` 推导为 `T&&`, 并且函数体中 `a` 的类型变为 `T&&`.

+ 所以最后 `a` 的类型只与传入表达式的 `value type` 有关,
但是不管传入表达式的 `值类型` 是什么, `a` 的值类型都会变为 `左值`.
即使 `参数a` 绑定到右值, 但是 `参数a` 本身是左值.

+ 这其中会有一个 `引用折叠`, 即如果传入的 `a`本身就是左值/右值引用,
则 `& &&` 会变为 `&`; `&& &&` 会变为 `&&`, 因此最终不会产生引用的引用.

```cpp
template<typename T>
void f(T&& a) {
    if constexpr(is_lvalue<decltype((a))>){
        std::cout << "lvalue" << std::endl;
    }else if constexpr(is_xvalue<decltype((a))>){
        std::cout << "xvalue" << std::endl;
    }
}
```

输出:

```out
The type of a : int&&
The value type of a is lvalue : true
lvalue
```

+ 第二步; 使 `a` 的 `arg type` 同步为 `value type`,
在第一步中我们已经知道, `a` 的 `type` 是传入的表达式的 `value type`.
那么我们就可以利用 `decltype` 来获 `value type`,
并且利用 `static_cast<decltype(a)>(a)` 来将 `a` 的类型转化为 `a` 的值类型.
即确保 形参的类型 等于 实参的类型.

```cpp
template<typename T>
void f(T&& a) {
    if constexpr(is_lvalue<
        decltype((
            static_cast<decltype(a)>(a) // 根据传给 a 实参 type, 更新 a 的type
            ))>){
        std::cout << "lvalue" << std::endl;
    }else if constexpr(is_xvalue<
                decltype((
                    static_cast<decltype(a)>(a) // decltype 见 ZhengLi,P55, 用于编译期获取expr类型
                    ))>){
        std::cout << "xvalue" << std::endl;
    }
}
```

调用

```cpp
int a = 20;
    f(10);
    f(a);
// 输出:
xvalue
lvalue
```

到此, 我们得到了想要的结果. 其中 `static_cast<decltype(a)>(a)` 和
标准库中的完美转发函数 `std::forward<T>(a)` 的原理是一样的.

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

[现代 C++: 右值引用, 移动语意, 完美转发](https://cloud.tencent.com/developer/article/1637076)

### 左值 or 右值

到底什么时候是左值?什么时候是右值?是不是有点混乱?
在 C++ 中, 每个表达式(expression)都有两个特性:

+ has identity? —— 是否有唯一标识, 比如 `地址`, `指针`.
有唯一标识的表达式在 C++ 中被称为 glvalue(generalized lvalue).
+ can be moved from? —— 是否可以安全地移动(编译器).
可以安全地移动的表达式在 C++ 中被成为 rvalue.

根据这两个特性, 可以将表达式分成 4 类:

+ 唯一, 不能移动 - 这类表达式在 C++ 中被称为 `lvalue`.
+ 唯一, 可以移动 - 这类表达式在 C++ 中被成为 xvalue(expiring value).
+ 不唯一, 不可移动 - 这类表达式在 C++ 中被成为 prvalue(pure rvalue).
+ 不唯一, 不可移动 -C++ 中不存在这类表达式.

简单总结一下这些 value categories 之间的关系:

可以移动的值都叫 `rvalue`, 包括 `xvalue` 和 `prvalue`.
有唯一标识的值都叫 `glvalue`, 包括 `lvalue` 和  `xvalue`.
`std::move` 的作用就是将 `lvalue` 转换成 `xvalue`.

![C-expression](https://ask.qcloudimg.com/http-save/7176906/t89kd9ja5y.png?imageView2/2/w/1620)
