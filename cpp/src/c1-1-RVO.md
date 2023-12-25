# c++ 返回值优化

[对右值引用使用std::move, 对通用引用使用std::forward](https://cntransgroup.github.io/EffectiveModernCppChinese/5.RRefMovSemPerfForw/item25.html)

如果你在 `按值返回` 的函数中, 返回值绑定到 `右值引用` 或者 `通用引用`形参上,
需要对返回的引用使用 `std::move`或者`std::forward`.
要了解原因, 考虑两个矩阵相加的`operator+`函数, 左侧的矩阵为右值(可以被用来保存求值之后的和):

```cpp
Matrix //按值返回, 而非引用
operator+(Matrix&& lhs, const Matrix& rhs) // 形参lhs指向右值内存(可移动), 符号lhs 本身是左值
{
    lhs += rhs;
    return std::move(lhs);       //移动lhs到返回值中
}
```

通过在 `return` 语句中将 `lhs` 转换为右值(通过 `std::move`),
`lhs` 可以移动到返回值的内存位置. 如果省略了 `std::move` 调用,

```cpp
Matrix                              //同之前一样
operator+(Matrix&& lhs, const Matrix& rhs)
{
    lhs += rhs;
    return lhs;                     //拷贝lhs到返回值中
}
```

`lhs` 是个左值的事实, 会强制编译器拷贝它到返回值的内存空间.
假定 `Matrix` 支持移动操作, 并且比拷贝操作效率更高,
在 `return` 语句中使用 `std::move` 的代码效率更高.

如果`Matrix`不支持移动操作, 将其转换为右值不会变差,
因为右值可以直接被`Matrix`的拷贝构造函数拷贝(见Item23).
如果`Matrix`随后支持了移动操作, operator+将在下一次编译时受益.
就是这种情况, 通过将`std::move`应用到按值返回的函数中要返回的右值引用上,
不会损失什么(还可能获得收益).

使用 `通用引用` 和 `std::forward` 的情况类似.
考虑函数模板reduceAndCopy收到一个未规约(unreduced)对象Fraction,
将其规约, 并返回一个规约后的副本.
如果原始对象是右值, 可以将其移动到返回值中(避免拷贝开销),
但是如果原始对象是左值, 必须创建副本, 因此如下代码:

```cpp
template<typename T>
Fraction                            //按值返回
reduceAndCopy(T&& frac)             //通用引用的形参
{
    frac.reduce();
    return std::forward<T>(frac);  //移动右值, 或拷贝左值到返回值中
}
```

如果std::forward被忽略, `frac` 就被无条件复制到 `reduceAndCopy` 的返回值内存空间.

## 对于按值返回局部对象的函数, 不要使用`move`

换句话说, 如果有个按值返回局部对象的函数, 像这样,

```cpp
Widget makeWidget()                 //makeWidget的"拷贝"版本
{
    Widget w;                       //局部对象
    …                               //配置w
    return w;                       //"拷贝"w到返回值中
}
```

你可能想要`优化`代码, 把"拷贝"变为移动:

```cpp
Widget makeWidget()                 //makeWidget的移动版本
{
    Widget w;
    …
    return std::move(w);            //移动w到返回值中(不要这样做!)
}
```

我的注释告诉你这种想法是有问题的, 但是问题在哪?

这是错的, 因为对于这种优化, 标准化委员会远领先于开发者.
早就为人认识到的是, makeWidget的`拷贝`版本可以避免复制局部变量w的需要,
通过在分配给函数返回值的内存中构造`w`来实现.
这就是所谓的返回值优化(return value optimization, RVO), 这在C++标准中已经实现了.

对这种好事遣词表达是个讲究活,
因为你想只在那些不影响软件外在行为的地方允许这样的拷贝消除(copy elision).
对标准中教条的(也可以说是有毒的)絮叨做些解释, 这个特定的好事就是说,
编译器可能会在按值返回的函数中消除对局部对象的拷贝(或者移动),
如果满足

+ `局部对象` 与函数 `返回值` 的类型相同;
+ `局部对象` 就是要返回的东西.

适合的局部对象包括, 大多数局部变量(比如makeWidget里的`w`),
还有作为return语句的一部分而创建的 `临时对象`.

`函数形参` 不满足要求.
一些人区分的更细致, 将 `RVO` 专门应用于 未命名的(即临时的)局部对象,
将 NRVO(named return value optimization) 专用于对 `命名对象` 的返回值优化.
把这些记在脑子里, 再看看makeWidget的"拷贝"版本:

```cpp
Widget makeWidget()                 //makeWidget的"拷贝"版本
{
    Widget w;
    …
    return w;                       //"拷贝"w到返回值中
}
```

这里两个条件都满足, 你一定要相信我, 对于这些代码,
每个合适的C++编译器都会应用RVO来避免拷贝w.
那意味着makeWidget的"拷贝"版本实际上不拷贝任何东西.

移动版本的`makeWidget`行为与其名称一样(假设Widget有移动构造函数),
将w的内容移动到makeWidget的返回值位置.
但是为什么编译器不使用RVO消除这种移动,
而是在分配给函数返回值的内存中再次构造w呢?
答案很简单: 它们不能. 条件(2)中规定, 仅当返回值为局部对象时,
才进行RVO, 但是makeWidget的移动版本不满足这条件, 再次看一下返回语句:

```cpp
return std::move(w);
```

返回的已经不是局部对象w, 而是 **w的引用**——`std::move(w)`的结果.
返回局部对象的引用不满足RVO的第二个条件, 所以编译器必须移动`w`到函数返回值的位置.
开发者试图对要返回的局部变量用 `std::move` 帮助编译器优化, 反而限制了编译器的优化选项.

但是RVO就是个优化. 编译器不被要求消除拷贝和移动操作, 即使他们被允许这样做.
或许你会疑惑, 并担心编译器用拷贝操作惩罚你, 因为它们确实可以这样.
或者你可能有足够的了解, 意识到有些情况很难让编译器实现RVO,
比如当函数不同控制路径返回不同局部变量时.
(编译器必须产生一些代码在分配的函数返回值的内存中构造适当的局部变量,
但是编译器如何确定哪个变量是合适的呢?)

如果这样, 你可能会愿意以移动的代价来保证不会产生拷贝.
那就是, 极可能仍然认为应用 `std::move` 到一个要返回的局部对象上是合理的,
只因为可以不再担心拷贝的代价.

那种情况下, 应用 `std::move` 到一个局部对象上仍然是一个坏主意.
C++标准关于RVO的规则表明, 如果满足RVO的条件,
但是编译器选择不执行拷贝消除, 则 **返回的对象必须被视为右值**.
实际上, 标准要求当RVO被允许时, 或者实行拷贝消除,
或者将 `std::move` 隐式应用于返回的局部对象.
因此, 在makeWidget的`拷贝`版本中,

```cpp
Widget makeWidget()                 //同之前一样
{
    Widget w;
    …
    return w;
}
```

编译器或者 不消除 `w` 的拷贝, 或者 把函数看成下面这样:

```cpp
Widget makeWidget()
{
    Widget w;
    …
    return std::move(w);            //把w看成右值, 因为不执行拷贝消除
}
```

这种情况与按值返回函数形参的情况很像.
`形参` 们没资格参与函数返回值的拷贝消除,
但是如果作为返回值的话编译器会将其视作右值. 结果就是, 如果代码如下:

```cpp
Widget makeWidget(Widget w)         //传值形参, 与函数返回的类型相同
{
    …
    return w;
}
```

编译器必须看成像下面这样写的代码:

```cpp
Widget makeWidget(Widget w)
{
    …
    return std::move(w);
}
```

这意味着, 如果对从按值返回的函数返回来的局部对象使用 `std::move`,
你并不能帮助编译器(如果不能实行拷贝消除的话, 他们必须把局部对象看做右值),
而是阻碍其执行优化选项(通过阻止RVO).

在某些情况下, 将 `std::move` 应用于局部变量可能是一件合理的事
(即你把 变量 传给函数, 并且知道不会再用这个变量),

```cpp
void fun(int); // 一个函数
void funA(){
int a;
fun(std::move(a));
}
```

但是满足RVO的 `return` 语句或者 `返回传值形参` 并不在此列.

请记住:

+ 最后一次使用时, 在右值引用上使用 `std::move`, 在通用引用上使用 `std::forward`.
+ 对 `按值返回` 的函数要返回的 `右值引用` 和 `通用引用` 形参, 执行相同的操作.
+ 如果 `局部对象` 可以被返回值优化消除, 就绝不使用 `std::move` 或者 `std::forward`.
