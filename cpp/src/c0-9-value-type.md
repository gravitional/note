# 如何确定表达式的值类型

[如何确定表达式的值类型](https://zhuanlan.zhihu.com/p/435605194)

![type](https://pic4.zhimg.com/80/v2-40f626aa77172dac408c0c0644c07aef_720w.webp)

C++本来只有 `左值` 和 `右值`, 但是为了能充分利用右值, 减少内存的分配, 从而引入了 `将亡值`.
左值可以通过 `std::move()` 转换成将亡值, 右值也可以通过`std::move()`或者`隐式类型转换`变为将亡值.
`将亡值` 仅仅是个标记, 表示该表达式所持有的资源, 可以被 `偷取`(转移所有权).

```cpp
// C++中如何打印类型信息?
// typeid的局限性
int a;
// class type_info
std::cout << typeid(a).name() << std::endl; // int
std::cout << typeid(decltype(a)).name() << std::endl; //int

int &b = a;
// type_info
std::cout << typeid(b).name() << std::endl;
std::cout << typeid(decltype(b)).name() << std::endl;

// 比较两个类型是否相等
std::cout << std::boolalpha;
std::cout << (typeid(a) == typeid(b)) << std::endl;
```

### 如何打印表达式的类型信息?

```cpp
template<typename T>
std::string type_to_string() {
#if defined(_MSC_VER)
    std::string type_name{__FUNCSIG__}; // 用于获取函数签名
    // class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >
    //    __cdecl type_to_string<int&>(void)
    auto start_pos = type_name.find_first_of('<',
                                             std::string(typeid(std::string).name()).size()) + 1;
    auto end_pos = type_name.find_last_of('>');
    return type_name.substr(start_pos, (end_pos - start_pos));

#elif defined(__clang__)
    std::string type_name{ __PRETTY_FUNCTION__ };
    auto start_pos = type_name.find_first_of('=') + 2;
    auto end_pos = type_name.find_first_of(']', start_pos);
    return type_name.substr(start_pos, (end_pos-start_pos));

#elif defined(__GNUC__)
    std::string type_name{ __PRETTY_FUNCTION__ };
    // std::__cxx11::string type_to_string() [with T = int&; std::__cxx11::string = std::__cxx11::basic_string<char>]
    auto start_pos = type_name.find_first_of('=') + 2;
    auto end_pos = type_name.find_first_of(';', start_pos);
    return type_name.substr(start_pos, (end_pos-start_pos));
#endif
}
```

在 `C++` 中如何得到表达式 的 `值类型`?

```cpp
int a = 10;
// decltype可以推导出一个表达式的类型
std::cout << "The expr type of a : " << type_to_string<decltype(a)>()
          << std::endl;
// 如果多加一个括号可以得到值类型
std::cout << "The value type of a : " << type_to_string<decltype((a))>()
          << std::endl;
std::cout << "The value type of std::move(a) : " << type_to_string<decltype((std::move(a)))>()
          << std::endl;

std::cout << "The expr type of 10 : " << type_to_string<decltype(10)>()
          << std::endl;
std::cout << "The value type of 10 : " << type_to_string<decltype((10))>()
            << std::endl;
```

输出:

```cpp
// a的类型
The expr type of a : int
// a的值类型
The value type of a : int&
// std::move(a)的值类型
The value type of a : int&&

// 10的类型
The expr type of 10 : int
// 10的值类型
The value type of 10 : int
```

从上面这几个例子可以看出, `decltype((*))` 推导出的值类型, 分为三类:

+ `&&` 作为限定符的 `将亡值`(xvalue)
+ `&` 作为限定符的 `左值`(lvalue)
+ `无限定符` 的 `纯右值`(prvalue)

从而, 可以通过 `std::is_lvalue_reference_v` 和
`std::is_rvalue_reference_v` 这两个模板类来判断 `左值`  `和将亡值`,
不属于它们中任意一个的就是纯右值. 代码如下:

```cpp
template<typename T>
concept is_lvalue = std::is_lvalue_reference_v<T>;

template<typename T>
concept is_xvalue = std::is_rvalue_reference_v<T>;

template<typename T>
concept is_prvalue = !(is_lvalue<T> || is_xvalue<T>);

template<typename T>
concept is_rvalue = is_prvalue<T> || is_xvalue<T>;

template<typename T>
concept is_glvalue = is_lvalue<T> || is_xvalue<T>;
```

使用时传入的 `模板参数` 是 `decltype((a))` 这样的形式.
注意: 上述 `concept` 只能用于判定 `值类型`, 无法实际在模板中使用, 因为模板中传入的是 `类型参数`.

再定义一个宏:

```cpp
#define value_type(value)                                        \
    do                                                           \
    {                                                            \
        std::cout << #value " is lvalue : "                      \
                  << is_lvalue<decltype((value))> << std::endl;  \
        std::cout << #value " is xvalue : "                      \
                  << is_xvalue<decltype((value))> << std::endl;  \
        std::cout << #value " is prvalue : "                     \
                  << is_prvalue<decltype((value))> << std::endl; \
        std::cout << #value " is rvalue : "                      \
                  << is_rvalue<decltype((value))> << std::endl;  \
        std::cout << #value " is glvalue : "                     \
                  << is_glvalue<decltype((value))> << std::endl; \
    } while (0)\
```

使用

```cpp
std::cout << std::boolalpha;
value_type(10);
int a = 10;
std::cout << "===========================" << std::endl;
value_type(a);
std::cout << "===========================" << std::endl;
value_type(std::move(a));
```

输出:

```bash
10 is lvalue : false
10 is xvalue : false
10 is prvalue : true
10 is rvalue : true
10 is glvalue : false
===========================
a is lvalue : true
a is xvalue : false
a is prvalue : false
a is rvalue : false
a is glvalue : true
===========================
std::move(a) is lvalue : false
std::move(a) is xvalue : true
std::move(a) is prvalue : false
std::move(a) is rvalue : true
std::move(a) is glvalue : true
```

`10` 是个纯右值, 同时也属于右值;
`a` 是左值, 同时也属于 `glvalue`;
`std::move(a)` 是一个将亡值, 所以他也属于rvalue和glvalue.

为什么上面的例子要使用 `std::move(a)`, 而不直接使用一个 `右值引用` 类型的变量呢?
下面来看下, 一个 `右值引用` 类型的 `变量类型` 和 `值类型`.

```cpp
std::cout << "===========================" << std::endl;
int &&c = 20;
std::cout << "The type of c : " << type_to_string<decltype(c)>() << std::endl;
value_type(c);
```

输出:

```bash
The type of c : int&&
c is lvalue : true
c is xvalue : false
c is prvalue : false
c is rvalue : false
c is glvalue : true
```

+ 从上面的输出可以看出, 虽然 `b` 的类型为 `右值引用`, 但它却是个 `左值`.
+ `int&& b = 20` 这一表达式, 实际上是先为 `20` 在栈上分配一块 `4字节` 的内存,
再将 `20` 存到这片内存中, 再将 `内存地址` 赋值给 `b`.
+ 即 `b` 绑定的内容要理解成 `右值引用`, 但 `b` 这块存储本身是左值类型的,
`b` 变量具有唯一标识, 不能移动.

```c++
mov     eax, 20
mov     DWORD PTR [rbp-12], eax
lea     rax, [rbp-12]
mov     QWORD PTR [rbp-8], rax
```

+ `rbp`    栈底地址
+ `rsp`    栈顶地址
+ `dword`  双字 就是四个字节
+ `ptr`    pointer缩写 即指针
+ []里的数据是一个地址值, 这个地址指向一个双字型数据

+ 比如 `mov  DWORD PTR [rbp-12], eax`; 把内存地址 `rbp-12` 中的双字型(32位)数据赋给 `eax`
+ `lea`; effective address; 比如: `lea rax, [rbp-12]`; 将 `rbp-12` 这个地址赋值给 `rax`

## 表达式的具体分类

### lvalue

```cpp
struct Person{
    int age;
    static int weight;
};

int& f() {
    int* a = new int{10};
    return *a;
}

void g() {}

int main()
{
    std::cout << std::boolalpha;
    // 1. 普通变量
    int a = 10;
    std::cout << "a is lvalue : " << is_lvalue<decltype((a))> << std::endl;
    // a is lvalue : true

    // 2. 函数
    std::cout << "main() is lvalue : " << is_lvalue<decltype((main))> << std::endl;
    // main() is lvalue : true

    // 3. 成员变量
    Person p;
    std::cout << "p.age is lvalue : " << is_lvalue<decltype((p.age))> << std::endl;
    std::cout << "Person::weight is lvalue : " << is_lvalue<decltype((Person::weight))> << std::endl;
    // p.age is lvalue : true
    // Person::weight is lvalue : true

    // 4. 字符类型的字面量
    std::cout << R"("hello world" is lvalue : )" << is_lvalue<decltype(("hello world"))> << std::endl;
    std::cout << R"(The expr type of "hello world" : )" << type_to_string<decltype("hello world")>() << std::endl;
    // "hello world" is lvalue : true
    // The expr type of "hello world" : const char (&)[12]

    // 5. 左值引用类型的函数返回值
    std::cout << "f()->int& is lvalue : " << is_lvalue<decltype((f()))> << std::endl;
    // f()->int& is lvalue : true

    // 6. 函数不管怎么变都是左值
    std::cout << R"(The expr type of g : )" << type_to_string<decltype(g)>() << std::endl;
    std::cout << "====================== void(&g1)() = g" << std::endl;
    void(&g1)() = g;
    std::cout << R"(The expr type of g1 : )" << type_to_string<decltype(g1)>() << std::endl;
    std::cout << R"(&g is lvalue : )" << is_lvalue<decltype((g1))> << std::endl;
    // The expr type of g1 : void (&)()
    // &g is lvalue : true

    std::cout << "====================== void(&&g2)() = g" << std::endl;
    void(&&g2)() = g;
    std::cout << R"(The expr type of g2 : )" << type_to_string<decltype(g2)>() << std::endl;
    std::cout << R"(&&g is lvalue : )" << is_lvalue<decltype((g2))> << std::endl;
    // The expr type of g2 : void (&&)()
    // &&g is lvalue : true

    // 函数 std::move 后变成右值
    std::cout << "============================ std::move(g)" << std::endl;
    std::cout << R"(The expr type of std::move(g) : )" << type_to_string<decltype(std::move(g))>() << std::endl;
    std::cout << R"(std::move(g) is rvalue : )" << is_rvalue<decltype((std::move(g)))> << std::endl;
    // The expr type of std::move(g) : void(__cdecl&&)(void)
    // std::move(g) is rvalue : true
}
```

### prvalue

```cpp
std::cout << std::boolalpha;
// 1. 除字符串字面量以外的其它字面量
std::cout << "10 is prvalue : " << is_prvalue<decltype((10))> << std::endl;
std::cout << "true is prvalue : " << is_prvalue<decltype((true))> << std::endl;
std::cout << "nullptr is prvalue : " << is_prvalue<decltype((nullptr))> << std::endl;
// 10 is prvalue : true
// true is prvalue : true
// nullptr is prvalue : true

// 2. 函数返回值
std::cout << "main()->int is prvalue : " << is_prvalue<decltype((main()))> << std::endl;
// main()->int is prvalue : true

// 3. 临时对象
std::cout << "Person{} is prvalue : " << is_prvalue<decltype((Person{}))> << std::endl;

// 4. 取地址符
std::cout << "& is prvalue : " << is_prvalue<decltype((&main))> << std::endl;

// 5. lambda表达式
std::cout << "lambda is prvalue : " << is_prvalue<decltype(([](){}))> << std::endl;
```

### xvalue

```cpp
struct Person{
    int age;
    static int weight;
};

int& f() {
    int* a = new int{10};
    return *a;
}

void g() {}

int&& h(){
    return 10;
}
int main()
{
    std::cout << std::boolalpha;
    // 1. std::move
    int a = 10;
    std::cout << "std::move(10) is xvalue : " << is_xvalue<decltype((std::move(10)))> << std::endl;
    std::cout << "std::move(a) is xvalue : " << is_xvalue<decltype((std::move(a)))> << std::endl;
    // std::move(10) is xvalue : true
    // std::move(a) is xvalue : true

    // 2. static_cast<T&&>
    std::cout << "static_cast<T&&>(10) is xvalue : " << is_xvalue<decltype((static_cast<int&&>(10)))> << std::endl;
    std::cout << "static_cast<T&&>(a) is xvalue : " << is_xvalue<decltype((static_cast<int&&>(a)))> << std::endl;

    // 3. 函数返回值为T&&类型
    std::cout << "h()->int&& is xvalue : " << is_xvalue<decltype((h()))> << std::endl;
    // h()->int&& is xvalue : true

    // 4. rvalue 对象的非静态成员
    std::cout << "Person{}.age is xvalue : " << is_xvalue<decltype((Person{}.age))> << std::endl;
    Person p;
    std::cout << "std::move(p).age is xvalue : " << is_xvalue<decltype((std::move(p).age))> << std::endl;
    // Person{}.age is xvalue : true
    // std::move(p).age is xvalue : true
}
```

## std::move中的一个坑

```cpp
struct Person{
    int age;
    static int weight;
    std::string& name;
};

int main()
{
    std::cout << std::boolalpha;
    std::string name = "xiaoming";
    Person p{10, name};
    std::cout << "===============" << std::endl;
    value_type(std::move(p).age);
    std::cout << "===============" << std::endl;
    value_type(std::move(p).weight);
    std::cout << "===============" << std::endl;
    value_type(std::move(p).name);
}
```

输出:

```bash
===============
std::move(p).age is lvalue : false
std::move(p).age is xvalue : true
std::move(p).age is prvalue : false
std::move(p).age is rvalue : true
std::move(p).age is glvalue : true
===============
std::move(p).weight is lvalue : true
std::move(p).weight is xvalue : false
std::move(p).weight is prvalue : false
std::move(p).weight is rvalue : false
std::move(p).weight is glvalue : true
===============
std::move(p).name is lvalue : true
std::move(p).name is xvalue : false
std::move(p).name is prvalue : false
std::move(p).name is rvalue : false
std::move(p).name is glvalue : true
```

从上述输出, 可以看出 `std::move(p).age` 是个 `xvalue;`
`std::move(p).weight` 和 `std::move(p).name` 都是 `lvalue`.
也就是说, `weight` 和 `name` 都不是 `Person` 类的一部分, `Person` 类对于这两个成员变量没有所有权.
如果希望改变它们的值类型, 应改为 `std::move(p.weight)` 和 `std::move(p.name)`,

```cpp
std::cout << std::boolalpha;
std::string name = "xiaoming";
Person p{10, name};
std::cout << "===============" << std::endl;
value_type(std::move(p.age));
std::cout << "===============" << std::endl;
value_type(std::move(p.weight));
std::cout << "===============" << std::endl;
value_type(std::move(p.name));
===============
std::move(p.age) is lvalue : false
std::move(p.age) is xvalue : true
std::move(p.age) is prvalue : false
std::move(p.age) is rvalue : true
std::move(p.age) is glvalue : true
===============
std::move(p.weight) is lvalue : false
std::move(p.weight) is xvalue : true
std::move(p.weight) is prvalue : false
std::move(p.weight) is rvalue : true
std::move(p.weight) is glvalue : true
===============
std::move(p.name) is lvalue : false
std::move(p.name) is xvalue : true
std::move(p.name) is prvalue : false
std::move(p.name) is rvalue : true
std::move(p.name) is glvalue : true
```

这样, 三个成员变量都变成了 `xvalue`.
