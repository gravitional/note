# C++ dynamic_cast

[C++强制类型转换操作符dynamic_cast](https://www.cnblogs.com/xiangtingshen/p/10851851.html)

`dynamic_cast` 是四个强制类型转换操作符中最特殊的一个,
它支持运行时 `识别指针或引用`.
首先, `dynamic_cast` 依赖于 RTTI(Run-Time Type Identification)信息, 其次在转换时,
`dynamic_cast` 会检查转换的 source对象是否真的可以转换成target类型,

这种检查不是语法上的, 而是真实情况的检查.

## dynamic_cast主要用于"安全地向下转型"

`dynamic_cast` 用于类继承层次间的指针或引用转换.
主要还是用于执行 `安全的向下转型`(safe downcasting),
也即是 `基类对象 `的指针或引用转换为同一继承层次的其他指针或引用.

至于 `向上转型` (即派生类指针或引用类型转换为其基类类型),
本身就是安全的, 尽管可以使用 `dynamic_cast` 进行转换, 但这是没必要的,
普通的转换已经可以达到目的, 毕竟使用 `dynamic_cast` 是需要开销的.

代码见 [auto_cast](../exa1-dynamica_cast/auto_cast.cpp)

## dynamic_cast与继承层次的指针

对于 `向下转型` 有两种情况.

+ 一种是基类指针所指对象是派生类类型的, 这种转换是安全的;
+ 另一种是基类指针所指对象为 `基类类型`,
在这种情况下 `dynamic_cast` 在运行时做检查, 转换失败, 返回结果为`0`(`nullptr`);

代码见 [dy_cast](../exa1-dynamica_cast/dy_cast.cpp)

## dynamic_cast和引用类型

在前面的例子中, 使用了 `dynamic_cast` 将 `基类指针` 转换为 `派生类指针`,
也可以使用 `dynamic_cast` 将基类引用转换为派生类引用.

同样的, 引用的向上转换总是安全的:

```cpp
Derived c;
Derived & der2= c;
Base & base2= dynamic_cast<Base&>(der2);//向上转换, 安全
base2.Show();
```

所以, 在引用上, `dynamic_cast` 依旧是常用于 `安全的向下转型`.
与指针一样, 引用的向下转型也可以分为两种情况,
与指针不同的是并 `不存在空引用`,
所以引用的 `dynamic_cast`检测失败时会抛出一个 `bad_cast`异常:

代码见 [dy_cast](../exa1-dynamica_cast/dy_cast.cpp)

## 用dynamic_cast转换的Base类至少带有一个虚函数

当类中拥有 `至少一个虚函数` 的时候,
编译器会为该类构建出一个 `虚函数表`(virtual method table), 虚函数表记录了 `虚函数的地址`.
如果该类派生了其他子类, 且子类定义并实现了基类的虚函数,
那么虚函数表会将该函数指向新的地址.

虚表是C++多态实现的一个重要手段, 也是 `dynamic_cast` 操作符转换能够进行的前提条件.
当类没有虚函数表的时候(也即一个虚函数都没有定义),
`dynamic_cast` 无法使用 `RTTI`, 不能通过编译
(个人猜想...在VS17.4.0上是这样).

当然, 虚函数表的建立对效率是有一定影响的,
构建虚函数表, 由表查询函数, 都需要时间和空间上的消耗.
所以, 除了必须声明 `virtual`(对于 `多态基类` 而言), 不要轻易使用virtual函数.
对于虚函数的进一步了解, 可以查看
`<Effective C++>` 条款07: 为多态基类声明virtual析构函数.
