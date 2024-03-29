# cpp links

[编程常用英语词汇](https://www.runoob.com/w3cnote/common-english-terminology-in-programming.html)
[超级实用的 Visual Studio 技巧](https://zhuanlan.zhihu.com/p/260205834)
[Visual Studio 中的键盘快捷方式](https://docs.microsoft.com/zh-cn/visualstudio/ide/default-keyboard-shortcuts-in-visual-studio?view=vs-2022)

[C++类型转换之reinterpret_cast](https://zhuanlan.zhihu.com/p/33040213)
[nullptr详解](https://blog.csdn.net/u010983763/article/details/53667468)
[C++11 nullptr: 初始化空指针](http://c.biancheng.net/view/7887.html)

[如何系统学习C++](https://mp.weixin.qq.com/s/WW_X12bTm94iaCgWBgYtJw)

+ ZhengLi,P84; `默认参数` 需要在 `原型声明` 中给出, `定义` 中不能再出现 `默认形参值`

+ ZhengLi,P35; 运算符优先级, 最高的是

```cpp
[] () . -> 后置++ 后置--
```

最低的是 `,`.

+ ZhengLi,P261 保护继承; 保护成员可以被派生类访问, 但不能被类外部的代码访问.

+ ZhengLi,P157; 静态数据成员; 在类内仅仅进行引用性声明,
在文件作用域的某个地方使用 类名限定 进行定义性声明,
静态数据成员需要通过这种方式分配内存空间.

+ Pragma omp parallel for schedule(guided)

+ vector 用法

+ constexpr & const, page29, page 83; 常量表达式, 常量函数, 常量

+ 用数组作为函数参数; p195, p206
把数组作为参数时, 一般不指定第一维的大小, 即使指定, 也会被忽略.
数组作为函数形参, 等价于元素类型的指针作为形参, 下面的写法是等价的

```cpp
void f(int p[]);
void f(int p[3]);
void f(int *p);
```

## 虚表和虚表指针

virtual Table, vptr, P342

## C 库函数 - memcpy()

[C 库函数 - memcpy()](https://www.runoob.com/cprogramming/c-function-memcpy.html)
[C函数之memcpy()函数用法](https://blog.csdn.net/tigerjibo/article/details/6841531)

## C++ namespace

```cpp
using namespace std;
namespace common{

}
```

## class 内的 static 变量

`static` 变量在类内声明(.h 文件)
在类外 定义(.cpp)
声明时并不分配内存, 只有定义时才分配, 所以可以递归定义
例如一个表示单位量纲的类:

```cpp
class Dimension{
public:
    Dimension(int mass, int length, int time); // 构造函数, 表示 质量, 长度, 时间的幂次
    ...
    static const Dimension Less; //声明 static 常量, 无量纲
    static const Dimension Mass; //质量量纲
}
```

看起来是递归的, 声明 `Less` or `Mass` 时,
class `Dimension` 还没有构造完成,
但实际上, 对于 static 变量, `.h` 中只是给出声明,
在下面的 `.cpp` 文件 才实际进行分配内存的操作.
所以上面的写法是合法的.

```cpp
const Dimension::Less(0,0,0);
const Dimension::Mass(1,0,0);
```
