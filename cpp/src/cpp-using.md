# C++ using 关键字

[C++ using用法](https://blog.csdn.net/chaipp0607/article/details/100128842)
[C++中using的三种用法](https://zhuanlan.zhihu.com/p/156155959)

```cpp
#include <iostream>

using namespace std; //引入命名空间

class ClassOne
{
public:
    int w;
protected:
    int a;
};

class ClassTwo
{
public:
    using ModuleType = ClassOne; //指定别名
};

template <typename ClassType>class ClassThree : private ClassType
{
public:
    using typename ClassType::ModuleType; //在子类中引用基类的成员
    ModuleType m;
    ClassThree() = default;
    virtual ~ClassThree() = default;
};

void main()
{
    ClassThree<ClassTwo>::ModuleType a;
}
```

在上面代码中, 一共有三处使用了 `using`, 它们的作用为:

+ 引入命名空间
+ 指定别名
+ 在子类中引用基类的成员

## 引入命名空间

指定命名空间是C++ `using namespace` 中最常被用到的地方, 在第3行中的:

```cpp
using namespace std;
```

## 指定别名

`using` 的另一个作用是指定别名,
一般都是 `using a = b;` 这样的形式出现, 比如在13行中:

```cpp
using ModuleType = ClassOne;
```

`ModuleType` 是 `ClassOne` 的一个别名.
`using` 这个作用也比较常见, 比如在 `vector.h` 中就有:

```cpp
template<class _Ty,class _Alloc = allocator<_Ty>>class vector: public _Vector_alloc<_Vec_base_types<_Ty, _Alloc>>
{
public:
    using value_type = _Ty;
    using allocator_type = _Alloc;
}
```

即 `value_type` 是 `_Ty` 的一个别名, `value_type a;` 和 `_Ty a;` 是同样的效果.

## 在子类中引用基类的成员

`using` 的第三个作用是子类中引用基类的成员,
一般都是 `using Base::a;` 这样的形式出现, 比如在22行中:

```cpp
using typename ClassType::ModuleType;
```

它和一般形式有些区别, 就是它加了个 `typename` 修饰,
这是因为类 `ClassThree` 本身是个模板类, 它的基类 `ClassType` 是个模板,
这个`typename` 和 `using` 其实没有什么关系.
如果 `ClassType` 不是模板的话, 这行代码就可以写成:

```cpp
using ClassType::ModuleType;
```

剩下的就是 `using` 的作用, 它引用了基类中的成员 `ModuleType`,
`ModuleType` 又是类 `ClassOne` 的别名, 所以后面 `ModuleType m;`
相当于定义 `对象m`, 对于子类成员 `m` 来说, 这样的效果和下面是相同的:

```cpp
typename ClassType::ModuleType m;
```

不同之处在于 `using` 还修改了基类成员的访问权限, 子类 `ClassThree` `私有继承ClassType`,
所以 `ClassType` 中公共成员 `ModuleType` 在子类 `ClassThree` 是私有的,
它不能被外部访问. 但是使用 `using` 后, 在 `main()` 函数中可以使用.
