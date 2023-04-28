# cpp meta

虽然函数不能声明形参为真正的数组, 但是可以接受指向数组的引用!所以我们修改f为传引用:

```cpp
template<typename T>
void f(T& param);                       //传引用形参的模板
```

我们这样进行调用,

```cpp
f(name);                                //传数组给f
```

`T` 被推导为了真正的数组!这个类型包括了数组的大小,
在这个例子中T被推导为 `const char[13]`,
`f`的形参(对这个数组的引用)的类型则为 `const char (&)[13]`.
是的, 这种语法看起来简直有毒, 但是知道它将会让你在关心这些问题的人的提问中获得大神的称号.

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
这使得我们可以用一个花括号声明一个数组, 然后第二个数组可以使用第一个数组的大小作为它的大小, 就像这样:

```cpp
int keyVals[] = { 1, 3, 7, 9, 11, 22, 35 };             //keyVals有七个元素
int mappedVals[arraySize(keyVals)];                     //mappedVals也有七个
```

当然作为一个现代C++程序员, 你自然应该想到使用 `std::array` 而不是内置的数组:

```cpp
std::array<int, arraySize(keyVals)> mappedVals;         //mappedVals的大小为7
```

至于 `arraySize` 被声明为 `noexcept`,
会使得编译器生成更好的代码, 具体的细节请参见Item14.

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

这个实际上没有什么不同,
但是如果你知道数组退化为指针, 你也会知道函数退化为指针.
