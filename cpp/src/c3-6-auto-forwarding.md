# auto&&,万能引用和完美转发

[auto类型推导](https://www.cnblogs.com/0xfffffff0/p/10285472.html)
[auto&&, 万能引用和完美转发](https://zhuanlan.zhihu.com/p/435689642?utm_id=0)
[C++11右值引用](https://zhuanlan.zhihu.com/p/141412154)

对于一个函数, 由于每个参数的 `value type` 可能是`左值引用`或`右值引用`,
针对所有可能的左右值引用组合, 特化所有模板是不现实的.
为解决这个问题, 有时候符号`&&` 并不一定代表`右值引用`, 它也可能是`左值引用`,
这叫做 `通用引用`(universal reference), 不过这种情况仅发生在`模板参数类型`或`auto推导`.

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

## 模板方法+万能引用+完美转发

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

## `std::forward<T>(a)`

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

## auto&& + 完美转发

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
