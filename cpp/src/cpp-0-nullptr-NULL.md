# C/C++中的NULL到底是什么

[C/C++中的NULL到底是什么](https://blog.csdn.net/digitalkee/article/details/103003300)

熟悉C++的童鞋都知道,
为了避免"野指针"(即指针在首次使用之前没有进行初始化)的出现,
我们声明一个指针后最好马上对其进行初始化操作.
如果暂时不明确该指针指向哪个变量, 则需要赋予 `NULL` 值.
除了 `NULL` 之外, C++11 新标准中又引入了 `nullptr` 来声明一个"空指针",
这样, 我们就有下面三种方法来获取一个"空指针":
如下:

```cpp
int *p1 = NULL; // 需要引入cstdlib头文件
int *p2 = 0;
int *p3 = nullptr;
```

新标准中建议使用nullptr代替NULL来声明空指针.
到这里, 大家心里有没有疑问: 为什么C++11要引入nullptr?
它与NULL相比又有什么不同呢?这就是我们今天要解决的问题.

## C/C++中的NULL到底是什么

我们查看一下C和C++的源码, 不难发现:

### NULL在C++中的定义

[NULL在C++中被明确定义为整数0](http://blog.csdn.net/Xiejingfa/article/details/50478512)

```cpp
/* Define NULL pointer value */

#ifndef NULL
    #ifdef __cplusplus
        #define NULL 0
#else /* __cplusplus */
    #define NULL ((void *)0)
    #endif /* __cplusplus */
#endif /* NULL */
```

### NULL在C中的定义

在C中, NULL通常被定义为如下:

```c
#define NULL    ((void *)0)
```

也就是说NULL实质上是一个 `void *` 指针.

那么问题又来了,
我们从一开始学习C++的时候就被告诫C++是兼容C的, 为什么对于NULLC++却不完全兼容C呢?
通过查找维基百科, 才发现这其中的原因.
简单地说, C++之所以做出这样的选择, 根本原因和C++的 `函数重载` 机制有关.
考虑下面这段代码:

```cpp
void Func(char *);
void Func(int);

int main()
{
Func(NULL);
}
```

如果C++让NULL也支持 `void*` 的隐式类型转换, 这样编译器就不知道应该调用哪一个函数.

## 为什么要引入nullptr

C++把`NULL`定义为`0`, 解决了函数重载后的函数匹配问题,
但是又引入了另一个"问题", 同样是上面这段代码:

```cpp
void Func(char *);
void Func(int);

int main()
{
Func(NULL); // 调用Func(int)
}
```

由于我们经常使用NULL表示空指针, 所以从程序员的角度来看,
`Func(NULL)` 应该调用的是 `Func(char *)`,
但实际上NULL的值是 `0`, 所以调用了 `Func(int)`.
`nullptr` 关键字真是为了解决这个问题而引入的.

另外我们还有注意到 `NULL` 只是一个宏定义, 而 `nullptr` 是一个 `C++` 关键字.

## nullptr如何使用

`nullptr` 关键字用于标识空指针, 是 `std::nullptr_t` 类型的(constexpr)变量.
它可以转换成任何指针类型和bool布尔类型
(主要是为了兼容普通指针可以作为条件判断语句的写法), 但是不能被转换为整数.

```cpp
char *p1 = nullptr; // 正确
int *p2 = nullptr; // 正确
bool b = nullptr; // 正确. if(b)判断为false
int a = nullptr; // error
```
