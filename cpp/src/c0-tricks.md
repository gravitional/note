# cpp 使用惯例

## 枚举值作为常量

zhengli, p389

```cpp
template<int SIZE>
class Stack<bool,SIZE>{
private:
    enum{
            UNIT_BITS=sizeof(unsigned*8),
            ARRAY_SIZE=(SIZE-1)/UNIT_BITS+1;
};
//...
```

本例利用了一个小技巧, 把 枚举值 当作 整型常量使用.
由于 枚举值 全部会在 编译时计算出来, 而且可以自动转换为整数,
因此可以通过匿名枚举来达到定义 常量 的目的.

匿名枚举的功能, 完全可以用类的静态成员常量来完成,
但由于历史上有些编译器对 静态成员常量 的编译时求值 支持得不好,
所以人们习惯于使用枚举.

## typedef

zhengli, p54, p213

定义类型别名, `typedef` 的不同用法,

```cpp
typedef double Area,Volume;
typedef int Natural;
Natural i1, i2;
Area a;
Volume v;
```

```cpp
typedef int (*DoubleIntFunc)(double);
DoubleIntFunc funcPtr;
```

声明了名称为 `funcPtr`, 类型为 `DoubleIntFunc` 的函数,
具体的类型就是 `DoubleIntFunc` 所处位置对应的类型.
`typedef` 会推导出他的类型.
