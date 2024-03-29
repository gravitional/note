# 模板和群体数据

群体可以分为 线性群体和非线性群体

线性群体中的元素按位置排列有序
非先行群体不用位置顺序来标识元素.

群体数据的组织, 指涉及到的算法.
例如排序和查找算法, sorting & searching.

直接插入排序,
直接选择排序,
交换排序(起泡排序)

顺序查找
折半查找.

## 线性群体

线性群体按照对元素的不同访问方法,
又可以分成 直接访问, 顺序访问 和 索引访问.

`直接访问` 指可以直接访问群体中的任一个元素,
而不必访问该元素之前的元素, 例如通过数组下标进行访问.

`顺序访问` 指只能按元素的排列顺序从头开始依次访问各个元素.

+ 两种特殊的线性群体, 分别称作 栈(stack)和队列(queue).

栈是后进先出的(LIFO), 可以访问的一段称为栈顶,
另一端称为栈底, 分别用栈顶指针和栈底指针标记.
压栈和弹栈.

队列是先进先出的(FIFO),
可以添加元素的一段称为 `队尾`, 可以删除元素的一段称为 `队头`
分别用队头指针和队尾指针标记, 操作为 入队 和 出队.

## 数组

c++ 数组是 直接访问的 先行群体, 元素可以通过下标直接访问.
它的大小在编译时确定, 无法更改.

动态数组 vector 是由任意多个位置连续的, 类型相同的元素组成 的直接访问线性群体,
元素可以在程序运行时改变.

一般把 `=` 和 `[]` 运算符重载为, 返回值类型为对象的引用.
这样结果可以用于习惯的表达式, 例如

```cpp
a[3]=5
(a=b)++;
```

一般还会声明返回常引用的版本, 例如

```cpp
T& operator[](int i);
const T& operator[](int i) const; // const 版本
```

+ 若不对 `=` 运算符进行重载, 系统会为其自动生成一个隐含的版本,
该版本会对每个 `数据成员` 执行 `=` 运算符.

+ 语法规定, `=`,`[]`, `()`, `->` 只能被重载为成员函数,
而且派生类中的 `=` 运算符总会隐藏基类中的 `=` 运算符函数.

### 指针转换运算符的作用

c++ 中, 如果想将自定义类型 `T` 的对象隐含或显式地
转换为 `S类型`, 可以将 `operator S` 定义为 T 类型的成员函数.
这样, 当发生 `T类型对象` 到 `S类型` 的隐式转换,
或者用 `static_cast` 显示转换到 S 类型时,该成员函数会被调用.

`转换操作符` 的重载函数不用指定 `返回值` 的类型, 也不能写 `void`.
这是因为返回类型一定和 `操作符名称` 相同.

```cpp
class Array{
private:
T* list; // T 类型指针, 用于存放动态分配的数组内存首地址.
int size;
...
}
operator T*(); // 重载到 T* 类型的转换, 使 Array 对象可以起到 C++ 普通数组的作用
operator const T*() const; // 到 T* 类型转换 操作符 的 const 版本.
```

除了这种自定义类型转换, 还有一种是通过 `构造函数`, P144; Sec 4.8.1

## 顺序访问线性群体--链表类

链表是一种动态数据结构, 可以用来表示顺序访问的线性群体.
链表是由一系列 `结点` 组成的, 节点可以在运行时动态生成.
每个结点包括 `数据域` 和 指向链表中 下一个 `结点` 的指针.

指针是维系节点的纽带, 可以不止一个,
因此有 双向链表 和 单向链表,
即是否有指针分别指向 前趋结点 和 后继结点.

链表的第一个结点称为头结点, 最后一个结点称为 尾结点,
尾结点的后继指针为空(NULL).

## 模板

+ 模板本身不是类, 它不能表示数据类型.
函数模板本身也不是函数, 编译器并不会为函数模板生成目标代码.
+ 类模板 实例化 时必须提供实参列表, 即使均采用 默认实参,  仍需要放置空列表 `<>`.

```cpp
template<typename T=double>
class Point{
public:
    Point(T _x=0, T _y=0): x(-x), y(-y){}
private:
    T x;
    T y;
};
Point<int> point(); // 给定模板参数, 定义整数点对象 (0, 0)
Point<> point(); // 模板参数列表 <> 为空, 默认double 类型初始化.
```

+ 编译器可以根据函数调用的 `实参类型` 自动推导出 `函数模板` 的参数,
也可以显式指定参数.

+

```cpp
outputArray(a,A_COUNT);
outputArray<int>(a,A_COUNT);
```

### 模板实例化

模板实例化是 `按需进行的`, 只有被调用的类型才会实例化.
这种实例化被称为 `隐含实例化`.

对类模板 进行 实例化 时, 只有它的 `成员的声明` 会被实例化,
而对类模板 成员函数定义 的实例化也是 按需进行的,
只有一个函数会被使用时, 它才会被实例化.

类的静态数据成员的实例化, 同样是按需进行的.

总结, 必须使用下列两种方式之一, 组织模板代码:

+ 把`函数模板`, `类模板` 放在头文件中
+ 对 `函数模板`, `类模板` 进行显式实例化.

#### 多文件结构中模板的组织

在多文件结构中, 模板的 `按需实例化机制` 决定了,
`函数模板`, `类模板成员函数` 和 `类模板静态数据成员`
的 `声明` 和 `定义` 都要放在 `头文件(.h)` 中.

>因为一般的编译系统 对 每个源文件(.cpp) 的编译是分别进行的,
>编译 `a.cpp` 时一般不能访问 `b.cpp`.

#### 显式实例化

显式实例化的一般语法形式是:

```cpp
template 实例化目标的声明;
```

例如:

```cpp
template<class T>
void outputArray(const T* array, int count){...};
//...
//显式实例化
template void outputArray<int>(const int* array, int count);
```

通过在 `源文件` 中, 对所有可能实例化的情况进行 `穷举`,
可以把 `函数模板`, `类模板成员函数` 和 `类模板静态数据成员`
的 `定义`, 放在源文件中(.cpp).

## 为模板定义特殊的实现

### 模板的特化(total specify)

```cpp
template<>
class Stack<bool,32>{
private:
...
}

// 现在, Stack<bool,32> 中的成员函数和非静态成员,
// 相当于在普通的类中, 无论是否被使用, 相关的目标代码都会被生成,
// 因此它们的定义应当放在源文件中, 而非头文件中
void Stack<bool,32>::push(bool item){...}
void Stack<bool,32>::pop(){...}

// 函数模板也可以特化
template<>
void outputArray<char>(const char* array, int count){...};
```

### 模板的偏特化(partial specify)

`偏特化` 不完全 定死 模板中的 所有参数,
即一部分模板参数固定, 而一部分模板参数可变,
只能对 类模板 使用.

```cpp
template<class T, int SIZE=50>
class Stack{...};

//偏特化
template<int SIZE>
class Stack<bool, SIZE>{...};
```

模板特化的结果是 `普通的` 类和函数,
而偏特化的结果仍然是 `模板`, 它保留了一部分 `未定的参数`.
第一行的 `template<int SIZE>`  中的 `SIZE` 就是偏特化后所保留的 `模板参数`.

对于偏特化的类模板, 给成员函数下定义时, 仍然要声明泛型参数

```cpp
template<int SIZE>
void Stack<bool,SIZE>::push(bool item){...}
template<int SIZE>
void Stack<bool,SIZE>::pop(){...}
```

+ C++ 的偏特化还允许将某一个 `模板参数` 所能表示的 `类型范围` 缩窄,

```cpp
// 能接受任何类型参数的类模板
tempalte <class T>
class X{...};

// 偏特化为以下形式,
tempalte <class T>
class X<T*> {...};
```

如果用 `指针类型` 作为 X模板 的参数, 那么将会使用 `偏特化` 的模板,
否则会使用普通的模板.

+ 下面的偏特化模板只接受 `常指针` 作为 X 的类型参数, 是更加特殊的版本.

```cpp
tempalte <class T>
class X<const T*> {...};
```

### 类模板和函数模板的默认实参

## 模板元编程
