# C++ 左值右值

[C++(二): 如何确定表达式的值类型](https://zhuanlan.zhihu.com/p/435605194)
[理解 C/C++ 中的左值和右值](https://nettee.github.io/posts/2018/Understanding-lvalues-and-rvalues-in-C-and-C/)

## 常见左值右值总结

### 左值lvalue

+ 普通变量; `int a=10;`
+ 函数; `main()`
+ 对象的成员变量; `p.age`, 其中 `p` 是 `Person`类的对象
+ 字符类型的字面量; `"hello world"`
+ 函数返回的 `左值引用`; 即`f()->int&`, 则 `f()` 是左值
+ 指向函数的 `左值引用` 或者 `右值引用`

```cpp
void g() {}; //定义一个函数
void (&g1)() = g; //指向函数的 左值引用语法, g1 是左值
void (&&g2)() = g; //指向函数的 右值引用语法, g2 是左值
```

### 右值rvalue

+ 除raw字符串以外的其他字面量; `10`
+ 普通函数返回值; `main()`
+ 临时对象; `Person{}`,其中 `Person`是类or结构体
+ 取地址符; `&main`
+ lambda 表达式; `[](){}`

## 将亡值xvalue

+ `std::move`; std::move(10)
+ `static_cast<T&&>`; `static_cast<T&&>(10)`
+ 函数返回`右值引用`; 即`h()->int&&`, 则 `h()` 是 `xvalue`.
+ rvalue 对象的非静态成员; `Person{}.age`, `std::move(p).age`
其中 `Person`是类or结构体, `p` 是 `Person` 类的对象

## 左值 or 右值

到底什么时候是 `左值`? 什么时候是 `右值`?是不是有点混乱?
在 C++ 中, 每个表达式(expression)都有两个特性:

+ has identity? —— 是否有唯一标识, 比如 `地址`, `指针`.
有唯一标识的表达式在 C++ 中被称为 `glvalue`(generalized lvalue),
为了方便记忆, 不妨称之为 `符号值`(symbol value).
`lvalue` 也被称为 `locator value`,

+ can be moved from? —— 是否可以安全地移动(编译器), 既能够转移 `所有权`.
可以安全地移动的表达式在 C++ 中被成为 `rvalue`(右值).
可以通过等号(`=`), 转移所有权给左边变量.
`rvalue` 也被称为 `read value`.

根据这两个特性, 可以将表达式分成 4 类:

+ 唯一, 不能移动 - 这类表达式在 C++ 中被称为 `lvalue`.
+ 唯一, 可以移动 - 这类表达式在 C++ 中被成为 `xvalue`(expiring value).
+ 不唯一, 可以移动 - 这类表达式在 C++ 中被成为 `prvalue`(pure rvalue).
+ 不唯一, 不可移动 -C++ 中不存在这类表达式.

简单总结一下这些 value categories 之间的关系:

可以移动的值都叫 `rvalue`, 包括 `xvalue` 和 `prvalue`.
有唯一标识的值都叫 `glvalue`, 包括 `lvalue` 和  `xvalue`.
`std::move` 的作用就是将 `lvalue` 或者 `prvalue` 转换成 `xvalue`.

![C-expression](https://ask.qcloudimg.com/http-save/7176906/t89kd9ja5y.png?imageView2/2/w/1620)

### expr type 和 value type;表达式类型和值类型

对于变量 `A`, `A` 的 `expr类型` 是 C++语言通常类型论中的 type, 如 `int`, `int&`, `int&&`
`A` 的 `value type` 是符号 `A` 内存的底层基本类型, 只有 `lvalue`, `prvalue`, `xvalue`,
用来区分这块内存是否 `有唯一标识`, 是否 `可以移动`.

所谓 `右值引用` 语法, 就是给 `rvalue`(prvalue, xvlaue)重新分配所有权,
或者说重新给这块内存起一个变量名.

```cpp
int a = 10; // a的 expr type 为 int, a 是 lvalue, 不可移动
int &&c=10; // 正确, 右值引用 绑定到纯右值, prvalue 可以移动
int &&d = a; //错误, 右值引用不能绑定到左值, lvalue 不能移动
int &&d = std::move(a); //正确, 右值引用 绑定到xvalue, xvalue 可以移动
```

可以归纳为:
`non-const左值引用` 只能绑定到 `non-const左值` 上, 简称 `左值引用绑左值`
`const左值引用` 可以绑定到 `non-const左值`, `const左值`, `non-const右值`, `常量右值` 等所有的值类型.
简称 `const 引用` 绑一切.

```cpp
int a=10;
const int &b = 10; // 指向常量的 左值引用语法
int &&c = 10; // 指向右值的 右值引用语法
c=10; // c 指向的内容变成 10

void g() {}; //定义一个函数
void (&g1)() = g; //指向函数的 左值引用语法
void (&&g2)() = g; //指向函数的 右值引用语法
```

+ 变量 `a` 的 `表达式类型`(expr type), 指的是 `a` 绑定的内容(指向的目标)的类型, 例如:
    + `int a=10;`, `a` 的 expr type 是 `int`;
    + `const int& b=10`,  `b` 的 expr type 是 `const int&`;
    + `int&& c = 5;`, `c` 的 expr type 是 `int&&`.

+ 而变量 `a`  的 `值类型`(value type), 指的是变量 `a` 本身的 `lvalue`, `rvalue`, `xvlaue` 分类.
    + `引用`(reference) 可以看成 `变量的别名`, 反过来, 普通变量也可以看成是 `引用`.
    只不过 `解引用` 过程是编译器通过 `符号表` 自动完成的.
    + `引用` 的底层是通过 `指针`(pointer) 实现的, `指针` 是存储内存地址的数据类型,
    通常定义的 `指针` 具有唯一标识符, 不可移动, 所以是 `lvalue`,
    + 所以 `普通变量`,  `指针`, `引用` 的 `值类型` 都是  `lvalue`.

+ 函数的 `左值引用` 和 `右值引用` 重载, 依赖的是 `函数参数`(argument) 的 `值类型`, 而不是 `表达式类型`.
其中 `rvalue` 被隐含地转换成 `xvalue`(唯一标识符+可以改变所有权),
所以参数类型要么是 `lvalue`, 要么是 `xvalue`.
