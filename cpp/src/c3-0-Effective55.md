# Effective C++

## 1 让自己习惯C++

### 01 view C++ as a federation of languages

+ `C`
+ `Object-Oriented C++`; 带类的 `C`
+ `Template C++`; 泛型编程
+ `STL`; 标准模板库, 容器，迭代器，算法，函数对象`

### 02 Prefer consts, enums, and inlines to `#define`

宁可以编译器替换预处理器

+ `enum hack`; 一个属于枚举类型(enumerated type)的数值，可充当 ints 被使用.

```cpp
class GamePlayer{
private:
    enum {NumTurns=5}; // "the num hack"-- 令NumTurns 称为5 个一个记号名称
    int scores[NumTurns]; //这样就没问题了.
}
```

### 03 尽可能使用Const

### 04 确定对象被使用前已先被初始化

## 2 构造/析构/赋值运算

### 05 了解C++默默编写并调用哪些函数

### 06 若不想使用编译器自动生成的函数，就该明确拒绝

### 07 为多态基类声明virtual析构函数

### 08 别让异常逃离析构函数

### 09 绝不在构造和析构函数中调用virtual函数

在 `base class` 构造期间, `virtual` 函数不是 `virtual` 函数.

`OO C` 的规则:
在 derived class 对象的 base class 构造期间,对象的类型是 base class 而不是 derived class。
运行时类型信息(runtime type information), dynamic_cast, typeid.

对于析构函数同理. 一旦 derived class 析构函数开始执行，
对象内的 derived class 成员变量便呈现未定义值.

### 10 令 `operator=` 返回`reference to *this`

```cpp
int x, y, z;
x=y=z=15;
```

### 11 在`operator=` 中处理 `自我赋值`

```cpp
a[i]=a[j]; //潜在的自我赋值
*px =*py; // 潜在的自我赋值
```

```cpp
Widget& Widget::operator=(const Widget& rhs) {
    Widget temp(rhs); // 为 rhs 制作一份复件(副本)
    swap(temp); //将 *this 和上述复件的数据交换
    return *this;
}
```

```cpp
Widget& Widget::operator=(Widget rhs) {
    swap(rhs); //注意这里是 pass by value
    return *this; //将 *this 和复件的数据交换
}
```

### 12 复制对象时勿忘其每一个成分

## 3 资源管理

### 13 以对象管理资源

### 14 在资源管理类中小心 `copying` 行为

### 15 在资源管理类中提供对原始资源的访问

### 16 成对使用 new 和 delete 时要采取相同形式

### 17 以独立语句将 newed 对象置入智能指针

## 4 设计与声明

### 18 让接口容易被正确使用,不容易被误用

### 19 设计 `class` 犹如设计 `type`

### 20 宁以 `pass-by-reference to const` 替换 `pass-by-value`

### 21 必须返回对象时,别妄想返回其 reference

### 22 将成员变量声明为 private

### 23 宁以 non-member, non-friend 替换 member 函数

### 24 若所有参数皆需类型转换, 请为此采用 non-member 函数

`operator*`

### 25 考虑写出一个不抛异常的 `swap` 函数

## 5 实现

### 26 尽可能延后变量定义式出现的时间

### 27 尽量少做转型动作

### 28 避免返回 handles 指向对象内部成分

### 29 为`异常安全`而努力是值得的

### 30 透彻了解 inlining 的里里外外

### 31 将文件间的编译依存关系降至最低

## 6 继承与面向对象设计

### 32 确定你的public继承塑膜出is-a关系

### 33 避免遮掩继承而来的名称

### 34 区分接口继承和实现继承

### 35 考虑virtual函数以外的其他选择

### 36 绝不重新定义继承而来的non-virtual函数

### 37 绝不重新定义继承而来的缺省参数值

### 38 通过复合塑模出has-a 或“根据某物实现出”

### 39 明智而审慎地使用private继承

### 40 明智而审慎地使用多重继承

## 7 模板与泛型编程

### 41 了解隐式接口和编译器多态

### 42 了解typename的双重含义

### 43 学习处理模板化基类内的名称

### 44 将与参数化无关的代码抽离templates

### 45 运用成员函数模板接受所有兼容类型

### 46 需要类型转换时请为模板定义非成员函数

### 47 请使用 traits classes 表现类型信息

### 48 认识 template 元编程

### 8 定值 new 和 delete

### 49了解 new-handler 的行为

### 50 了解new 和 delete 的合理替换时机

### 51 编写 new 和 delete 时需固守常规

### 52 写了 placement new 也要写 placement delete

### 53 不要轻忽编译器的警告

### 54 让自己熟悉包括 TR1 在内的标准程序库

### 55 让自己熟悉Boost
