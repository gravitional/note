# 理解模板类型推导

[理解模板类型推导](https://cntransgroup.github.io/EffectiveModernCppChinese/1.DeducingTypes/item1.html)

```cpp
template<typename T>
void f(ParamType param);

f(expr);                        //从expr中推导T和ParamType
```

### 情景一: ParamType是一个指针或引用, 但不是通用引用

最简单的情况是 `ParamType` 是一个指针或者引用, 但非 `通用引用`.
在这种情况下, 类型推导会这样进行:

+ 如果expr的类型是一个引用, 忽略引用部分
+ 然后expr的类型与ParamType进行模式匹配来决定 `T`

举个例子, 如果这是我们的模板,

```cpp
template<typename T>
void f(T& param);               //param是一个引用
```

我们声明这些变量,

```cpp
int x=27;                       //x是int
const int cx=x;                 //cx是const int
const int& rx=x;                //rx是指向作为const int的x的引用
```

在不同的调用中, 对param和T推导的类型会是这样:

```cpp
f(x);                           //T是int, param的类型是int&
f(cx);                          //T是const int, param的类型是const int&
f(rx);                          //T是const int, param的类型是const int&
```

## 情景二: ParamType是一个通用引用

模板使用通用引用形参的话, 那事情就不那么明显了.
这样的形参被声明为像右值引用一样
(也就是, 在函数模板中假设有一个类型形参`T`, 那么 `通用引用` 声明形式就是 `T&&`),
它们的行为在传入左值实参时大不相同.
完整的叙述请参见Item24, 在这有些最必要的你还是需要知道:

+ 如果 `expr` 是左值, `T` 和 `ParamType` 都会被推导为 `左值引用`.
这非常不寻常, 第一, 这是模板类型推导中唯一一种 `T` 被推导为引用的情况.
+ 第二, 虽然 `ParamType` 被声明为 `右值引用` 类型, 但是最后推导的结果是 `左值引用`.
+ 如果expr是右值, 就使用正常的(也就是情景一)推导规则

举个例子:

```cpp
template<typename T>
void f(T&& param);              //param现在是一个通用引用类型

int x=27;                       //如之前一样
const int cx=x;                 //如之前一样
const int & rx=cx;              //如之前一样

f(x);                           //x是左值, 所以T是int&,
                                //param类型也是int&

f(cx);                          //cx是左值, 所以T是const int&,
                                //param类型也是const int&

f(rx);                          //rx是左值, 所以T是const int&,
                                //param类型也是const int&

f(27);                          //27是右值, 所以T是int,
                                //param类型就是int&&
```

Item24详细解释了为什么这些例子是像这样发生的.
这里关键在于通用引用的类型推导规则是不同于普通的左值或者右值引用的.
尤其是, 当通用引用被使用时, 类型推导会区分左值实参和右值实参, 但是对非通用引用时不会区分.

## 情景三: ParamType既不是指针也不是引用

当ParamType既不是指针也不是引用时, 我们通过传值(pass-by-value)的方式处理:

```cpp
template<typename T>
void f(T param);                //以传值的方式处理param
```

这意味着无论传递什么param都会成为它的一份拷贝——一个完整的新对象.
事实上param成为一个新对象这一行为会影响T如何从expr中推导出结果.

+ 和之前一样, 如果expr的类型是一个引用, 忽略这个引用部分
+ 如果忽略expr的引用性(reference-ness)之后, expr是一个const, 那就再忽略const.
如果它是volatile, 也忽略volatile(volatile对象不常见,
它通常用于驱动程序的开发中. 关于volatile的细节请参见Item40)

因此

```cpp
int x=27;           //如之前一样
const int cx=x;     //如之前一样
const int & rx=cx;  //如之前一样

f(x);    //T和param的类型都是int
f(cx);   //T和param的类型都是int
f(rx);   //T和param的类型都是int
```

## 数组实参

上面的内容几乎覆盖了模板类型推导的大部分内容, 但这里还有一些小细节值得注意,
比如 `数组类型` 不同于 `指针类型`, 虽然它们两个有时候是可互换的.
关于这个错觉最常见的例子是, 在很多上下文中 `数组` 会退化为指向它的 `第一个元素的指针`.
这样的退化允许像这样的代码可以被编译:

```cpp
const char name[] = "J. P. Briggs";     //name的类型是const char[13]

const char * ptrToName = name;          //数组退化为指针
```

在这里 `const char*` 指针 `ptrToName` 会由 `name` 初始化,
而 `name` 的类型为 `const char[13]`, 这两种类型(`const char*` 和 `const char[13]`)是不一样的,
但是由于数组退化为指针的规则, 编译器允许这样的代码.

但要是一个数组传值给一个模板会怎样?会发生什么?

```cpp
template<typename T>
void f(T param); //传值形参的模板

f(name); //T和param会推导成什么类型?
```

我们从一个简单的例子开始, 这里有一个函数的形参是数组, 是的, 这样的语法是合法的,

```cpp
void myFunc(int param[]);
```

但是数组声明会被视作指针声明, 这意味着 `myFunc` 的声明和下面声明是等价的:

```cpp
void myFunc(int* param);  //与上面相同的函数
```

`数组`与 `指针形参` 这样的等价是C语言的产物,
`C++` 又是建立在C语言的基础上, 它让人产生了一种数组和指针是等价的的错觉.

因为 `数组形参` 会视作 `指针形参`, 所以传值给模板的一个 `数组类型` 会被推导为一个 `指针类型`.
这意味着在模板函数f的调用中, 它的类型形参T会被推导为 `const char*`:

```cpp
f(name);  //name是一个数组, 但是T被推导为const char*
```

但是现在难题来了, 虽然函数不能声明 `形参` 为真正的 `数组`,
但是可以接受 `指向数组的引用`! 所以我们修改 `f` 为传引用:

```cpp
template<typename T>
void f(T& param);                       //传引用形参的模板
我们这样进行调用,

f(name);                                //传数组给f
```

`T` 被推导为了真正的数组!这个类型包括了数组的大小,
在这个例子中T被推导为 `const char[13]`,
`f`的形参(对这个数组的引用)的类型则为 `const char (&)[13]`.
是的, 这种语法看起来简直有毒,
但是知道它将会让你在关心这些问题的人的提问中获得大神的称号.

有趣的是, 可声明指向数组的引用的能力,
使得我们可以创建一个模板函数来推导出数组的大小:

```cpp
//在编译期间返回一个数组大小的常量值(//数组形参没有名字,
//因为我们只关心数组的大小)
template<typename T, std::size_t N>                     //关于
constexpr std::size_t arraySize(T (&)[N]) noexcept      //constexpr
{                                                       //和noexcept
    return N;                                           //的信息
}                                                       //请看下面
```

在Item15提到将一个函数声明为 `constexpr` 使得结果在编译期间可用.
这使得我们可以用一个花括号声明一个数组,
然后第二个数组可以使用第一个数组的大小作为它的大小, 就像这样:

```cpp
int keyVals[] = { 1, 3, 7, 9, 11, 22, 35 };             //keyVals有七个元素

int mappedVals[arraySize(keyVals)];                     //mappedVals也有七个
```

当然作为一个现代C++程序员, 你自然应该想到使用 `std::array` 而不是内置的数组:

```cpp
std::array<int, arraySize(keyVals)> mappedVals;         //mappedVals的大小为7
```

至于 `arraySize` 被声明为 `noexcept`, 会使得编译器生成更好的代码, 具体的细节请参见Item14.

## 函数实参

在C++中不只是数组会退化为指针, 函数类型也会退化为一个函数指针,
我们对于数组类型推导的全部讨论都可以应用到函数类型推导和退化为函数指针上来. 结果是:

```cpp
void someFunc(int, double);         //someFunc是一个函数,
                                    //类型是void(int, double)

template<typename T>
void f1(T param);                   //传值给f1

template<typename T>
void f2(T & param);                 //传引用给f2

f1(someFunc);                       //param被推导为指向函数的指针,
                                    //类型是void(*)(int, double)
f2(someFunc);                       //param被推导为指向函数的引用,
                                    //类型是void(&)(int, double)
```

这个实际上没有什么不同, 但是如果你知道数组退化为指针, 你也会知道函数退化为指针.

这里你需要知道: `auto` 依赖于模板类型推导.
正如我在开始谈论的, 在大多数情况下它们的行为很直接.
在通用引用中对于左值的特殊处理使得本来很直接的行为变得有些污点,
然而, 数组和函数退化为指针把这团水搅得更浑浊.
有时你只需要编译器告诉你推导出的类型是什么.
这种情况下, 翻到item4,它会告诉你如何让编译器这么做.

## 请记住

+ 在模板类型推导时, 有引用的实参会被视为无引用, 他们的引用会被忽略
+ 对于`通用引用` 的推导, `左值实参` 会被特殊对待
+ 对于 `传值类型` 推导, `const`和 `volatile` 实参会被认为是non-const的和non-volatile的
+ 在模板类型推导时, `数组名` 或者 `函数名` 实参会退化为指针, 除非它们被用于初始化引用.
