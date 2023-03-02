# C++ 数据类型

使用编程语言进行编程时, 需要用到各种变量来存储各种信息.
变量保留的是它所存储的值的内存位置. 这意味着, 当您创建一个变量时, 就会在内存中保留一些空间.

您可能需要存储各种数据类型(比如`字符型`, `宽字符型`, `整型`, `浮点型`, `双浮点型`, `布尔型`等)的信息,
操作系统会根据变量的数据类型, 来分配内存和决定在保留内存中存储什么.

### 基本的内置类型

`C++` 为程序员提供了种类丰富的内置数据类型和用户自定义的数据类型.
下表列出了七种基本的 `C++` 数据类型:

| 类型     | `关键字`  |
| -------- | --------- |
| 布尔型   | `bool`    |
| 字符型   | `char`    |
| 整型     | `int`     |
| 浮点型   | `float`   |
| 双浮点型 | `double`  |
| 无类型   | `void`    |
| 宽字符型 | `wchar_t` |

其实 `wchar_t` 是这样来的:

```cpp
typedef short int wchar_t;
```

所以 `wchar_t` 实际上的空间是和 `short int` 一样.

一些基本类型可以使用一个或多个类型修饰符进行修饰:

+ `signed`
+ `unsigned`
+ `short`
+ `long`

下表显示了各种变量类型在内存中存储值时需要占用的内存, 以及该类型的变量所能存储的最大值和最小值.

注意: 不同系统会有所差异.

| 类型                 | 位         | 范围                                                               |
| -------------------- | ---------- | ------------------------------------------------------------------ |
| `char`               | 1个字节    | `-128到127 或者 0到255`                                            |
| `unsigned char`      | 1 个字节   | `0到255`                                                           |
| `signed char`        | 1个字节    | `-128到127`                                                        |
| `int`                | 4个字节    | `-2147483648到2147483647`                                          |
| `unsigned  int`      | 4个字节    | `0到4294967295`                                                    |
| `signed int`         | 4个字节    | `-2147483648到2147483647`                                          |
| `short int`          | 2个字节    | `-32768到32767`                                                    |
| `unsigned short int` | 2个字节    | `0到65,535`                                                        |
| `signed short int`   | 2个字节    | `-32768到32767`                                                    |
| `long int`           | 8个字节    | `-9,223,372,036,854,775,808 到 9,223,372,036,854,775,807`          |
| `signed long int`    | 8个字节    | `-9,223,372,036,854,775,808 到 9,223,372,036,854,775,807`          |
| `unsigned long int`  | 8个字节    | `0到18,446,744,073,709,551,615`                                    |
| `float`              | 4个字节    | `精度型占4个字节(32位)内存空间, +/- 3.4e +/- 38 (~7 个数字)`     |
| `double`             | 8个字节    | `双精度型占8个字节(64位)内存空间, +/- 1.7e +/- 308 (~15 个数字)` |
| `long double`        | 16个字节   | `长双精度型16个字节(128位)内存空间, 可提供18-19位有效数字. `     |
| `wchar_t`            | 2或4个字节 | `1个宽字符`                                                        |

从上表可得知, 变量的大小会根据编译器和所使用的电脑而有所不同.
下面实例会输出您电脑上各种数据类型的大小.

```cpp
#include<iostream>

using namespace std;

int main()
{
    cout << "type: \t\t" << "************size**************"<< endl;
    cout << "bool: \t\t" << "所占字节数: " << sizeof(bool);
    cout << "\t最大值: " << (numeric_limits<bool>::max)();
    cout << "\t\t最小值: " << (numeric_limits<bool>::min)() << endl;
    cout << "char: \t\t" << "所占字节数: " << sizeof(char);
    cout << "\t最大值: " << (numeric_limits<char>::max)();
    cout << "\t\t最小值: " << (numeric_limits<char>::min)() << endl;
    cout << "signed char: \t" << "所占字节数: " << sizeof(signed char);
    cout << "\t最大值: " << (numeric_limits<signed char>::max)();
    cout << "\t\t最小值: " << (numeric_limits<signed char>::min)() << endl;
    cout << "unsigned char: \t" << "所占字节数: " << sizeof(unsigned char);
    cout << "\t最大值: " << (numeric_limits<unsigned char>::max)();
    cout << "\t\t最小值: " << (numeric_limits<unsigned char>::min)() << endl;
    cout << "wchar_t: \t" << "所占字节数: " << sizeof(wchar_t);
    cout << "\t最大值: " << (numeric_limits<wchar_t>::max)();
    cout << "\t\t最小值: " << (numeric_limits<wchar_t>::min)() << endl;
    cout << "short: \t\t" << "所占字节数: " << sizeof(short);
    cout << "\t最大值: " << (numeric_limits<short>::max)();
    cout << "\t\t最小值: " << (numeric_limits<short>::min)() << endl;
    cout << "int: \t\t" << "所占字节数: " << sizeof(int);
    cout << "\t最大值: " << (numeric_limits<int>::max)();
    cout << "\t最小值: " << (numeric_limits<int>::min)() << endl;
    cout << "unsigned: \t" << "所占字节数: " << sizeof(unsigned);
    cout << "\t最大值: " << (numeric_limits<unsigned>::max)();
    cout << "\t最小值: " << (numeric_limits<unsigned>::min)() << endl;
    cout << "long: \t\t" << "所占字节数: " << sizeof(long);
    cout << "\t最大值: " << (numeric_limits<long>::max)();
    cout << "\t最小值: " << (numeric_limits<long>::min)() << endl;
    cout << "unsigned long: \t" << "所占字节数: " << sizeof(unsigned long);
    cout << "\t最大值: " << (numeric_limits<unsigned long>::max)();
    cout << "\t最小值: " << (numeric_limits<unsigned long>::min)() << endl;
    cout << "double: \t" << "所占字节数: " << sizeof(double);
    cout << "\t最大值: " << (numeric_limits<double>::max)();
    cout << "\t最小值: " << (numeric_limits<double>::min)() << endl;
    cout << "long double: \t" << "所占字节数: " << sizeof(long double);
    cout << "\t最大值: " << (numeric_limits<long double>::max)();
    cout << "\t最小值: " << (numeric_limits<long double>::min)() << endl;
    cout << "float: \t\t" << "所占字节数: " << sizeof(float);
    cout << "\t最大值: " << (numeric_limits<float>::max)();
    cout << "\t最小值: " << (numeric_limits<float>::min)() << endl;
    cout << "size_t: \t" << "所占字节数: " << sizeof(size_t);
    cout << "\t最大值: " << (numeric_limits<size_t>::max)();
    cout << "\t最小值: " << (numeric_limits<size_t>::min)() << endl;
    cout << "string: \t" << "所占字节数: " << sizeof(string) << endl;
    // << "\t最大值: " << (numeric_limits<string>::max)() << "\t最小值: " << (numeric_limits<string>::min)() << endl;
    cout << "type: \t\t" << "************size**************"<< endl;
    return 0;
}
```

本实例使用了`endl`, 这将在每一行后插入一个换行符, `<<` 运算符用于向屏幕传多个值.
我们也使用 `sizeof()` 函数来获取各种数据类型的大小.

当上面的代码被编译和执行时, 它会产生以下的结果, 结果会根据所使用的计算机而有所不同:

```cpp
type:         ************size**************
bool:         所占字节数: 1    最大值: 1        最小值: 0
char:         所占字节数: 1    最大值:         最小值: ?
...
```

## 数据的编码表示

`10`进制转`R`进制: 除`R`取余数, 位数依次升高.
`10`进制小数转`R`进制: 乘`R`取整数部分, 小数部分再乘`R`取整数迭代, 位数依次降低.
`R`进制转`10`进制: 科学计数法, 级数求和.

整数的几种编码:

+ 原码: 以`0`表示`+`号, `1`表示`-`号, 但是这样`0`的表示不唯一, 且正负号需要单独的运算规则
+ 模数: 以钟表为例, `12`就是模数.
+ 补数: 一个数减去另一个数, 等于第一个数加上第二个数的补数. 例如: `8(-2)=8+10(mod 12)=6`
+ 反码: 对于负整数: 原码符号位不变(仍是`1`), 其余各位取反; 对于正整数, 原码就是补码. 反码是求补码的中间码.
+ 补码: 补码=`反码的最低位+1`

整数使用补码的优点是:

+ `0`的表示唯一
+ 符号位可作为数值参与运算
+ 补码运算的结果仍为补码
+ 补码再求补即可得到原码

如果负数之和得正数, 或正数之和得负数, 说明运算结果`溢出`.

***
小数的两种表示方法: 定点方案, 与浮点方案. 现在一般都用浮点数, 也就是小数点位置可变.
$N=M\times 2^E$
`E`:`2`的幂次, 称为数字`N`的阶码, 反映了该浮点数表示的数据范围.
`M`:`N`的尾数, 其位数反映了数据的精度.

字符在计算机中通过编码表示: 西文常用`ASCII`码, `7`位二进制数表示一个字符, 最多为`2^7=128`个.
汉字编码: 中国国家标准: `GB 18030-2005 信息技术中文编码字符集`

布尔类型, 默认转换, 非零数据转换成`false`, 其他数值转换成`true`.

## 输入输出流

cout << 表达式 << 表达式 ...
cin >> 表达式 >> 表达式...

`I/O`流类库操纵符.

+ `dec` : 十进制表示
+ `hex` : 十六进制表示
+ `oct` : 八进制表示
+ `ws` : 提取空白符
+ `endl` : 插入换行符, 并刷新流
+ `ends` : 插入空字符
+ `setprecision(int)` : 设置浮点数的小数位数(包括小数点)
+ `setw(int)` : 设置域宽

例如:

```c++
cout << setw(5) << setprecision(3) << 3.1415 ;
```

## 过程控制

### if switch

```c++
if  (表达式1)
{...}
else if (表达式2)
{...}
else if
```

注意`if`和`else`的匹配关系:

```c++
if  (表达式1)
{
   if () 语句1
}
else
```

`switch` 语句: 没有`break`不会默认跳出, 每个 `case `都应该包含`break`; `case`包含多个语句, 无需`{}`, 因为`case, break`就相当于括号.
表达式, 判断值都是`int` or `char`类型.

```c++
switch (表达式){
case 判断值0: xxx; break;
case 判断值1: xxx; break;
...
default:
}
```

### while, do while, for

```c++
int i=1, sum=0;
while (i<=10) {
sum +=i;
i++;
}
```

`do while`会先执行一次循环体. 而`while`先判断.

```c++
do {
   语句
}
while (判断)
```

`for`语句明确控制循环次数. `for( 初始语句;循环体;循环后语句)`

```c++
for (int k=1; k<=n; k++){
语句
}
```

`for`语句中的范围`for`形式, 用于遍历一个容器中的序列:

```c++
for (声明: 表达式)
   语句
```

+ `break`: 跳出最内层的循环体
+ `continue`: 提前结束本次循环, 进入下一次.
+ `goto`: 跳转到任意地方.

## 自定义类型

给类型起一个别名

```c++
typedef 已有类型名 新类型名表 //c 语言继承的办法
// 或者
using 新类型名 = 已有类型名
using Area = double
```

### 枚举类型

`enum 枚举类型名 {变量值列表}`

```c++
enum Weekday {SUN,MON,TUE,WED,THU,FRI,SAT}
```

也可以定义限定类型的枚举类型. 将整数值赋值给枚举类型要进行强制类型转换.

### auto, decltype 类型

`auto `: 编译器通过初始值自动推断变量的类型. 例如:

```cpp
auto val=val1+ val2
```

如果都是`int`, 则`val` is `int`.
如果有一个`double`, 则`val`是`doule`类型.

`decltype(cls) j =2`:表示`j`的初始值为`2`, 但类型与`cls`一致.

### struct

`struct` 把一组相互关联的数据整合在一起. 例如:

```c++
struct MyTimeStruct
{
   unsigned int year;
   unsigned int month;
   unsigned int day;
}
// 赋值和调用
myTime={2015,3,16}
myTime.day
```

## 函数

+ 内联函数
+ `constexpr`函数
+ 带默认参数值的函数
+ 函数重载: 编译时绑定--早绑定, 晚绑定
+ `C++` 系统函数

### 函数定义

函数定义的语法形式

```c++
类型标识符 函数名(形式参数表)
{
语句序列
return x // void 函数不需要写 return
}
```

形式参数的形式为: `type1 name1, ....`

### 函数调用

调用函数需要声明`函数原型`: 类型标识符 被调函数名 (含类型说明的形参表),  调用使用`函数名(实参列表)`.
`调用堆栈`

随机函数: `rand()`, `srand()`.

函数的嵌套调用和递归调用.

### 参数传递

函数被调用时才分配形参的存储单元
实参可以是常量, 变量或表达式
实参类型必须与形参相符
值传递是传递参数值, 即单向传递
引用传递可以实现双向传递
常数引用作参数可以保障实参数据的安全

### 引用 `&`

引用(`&`)是标识符的别名.
定义引用时 `必须同时对它进行初始化`, 指向一个已存在的对象. 例如

```c++
int i,j;
int &ri = i; //定义 int 引用 ri, 初始化为变量i的引用
j=10
ri=j; // 相当于 i=j
```

一旦引用被初始化后, 就不能改为指向其他对象.  引用可以作为形参.

### 含有可变参数的函数

类模板: `initializer_list`

```cpp
initializer_list<string> ls; //initializer_list 的元素类型是 string
initializer_list<int> ls; //initializer_list 的元素类型是 int
```

`initializer_list` 比较特殊的是, 其对象中的元素永远是常量值, 无法改变 `initializer_list` 对象中元素的值.
含有 `initializer_list` 形参的函数也可以同时拥有形参.

### 内联函数

编译器实现的, 没有调用子函数开销. 声明内联函数用关键字`inline`.  编译时用函数体进行替换, 节省了参数传递, 控制转移等开销.

内联函数不能有循环语句和`switch`语句. 必须出现在第一次调用之前, 不能使用异常接口声明.

`inline`只是建议, 编译器不一定理会.

### constexpr 函数

`constexpr` 修饰的函数, 在其所有参数都是`constexpr`时, 一定返回 `constexpr`.例如

```cpp
constexpr int get_size(){return 20;}
constexpr int foo = get_size(); \\正确: foo 是一个常量表达式
```

编译期间可计算函数.

### 函数的默认参数

```cpp
int add(int x=5,int y=6){
   return x+y;
}
int main(){
   add(10,20); //10+20
   add(10); // 10+6
   add(); //5+6
}

带有默认值的形参必须在形参列表的最右, 即默认参数的右边不能有无默认值的参数;
调用时实参与形参的结合次序是从左到右.

```cpp
int add(int x,int y=6, int z=6);
```

如果函数声明在前, 则在声明中给定默认值;
如果函数定义在前, 则在定义中给定默认值;

### 函数的重载

在编译的时候实现, 利用静态的多态性实现. 允许功能相似的函数在相同的作用域内以相同函数名声明, 从而形成重载.
方便使用与记忆.

```cpp
int add (int x, int y);
float add(float add, float y);
```

```cpp
int add (int x, int y);
int add (int x, int y, int z);
```

### c++系统函数

使用系统函数要包含相应的头文件. 如`cmath`.

## 面向对象

### 声明类

构造函数, 析构函数

抽象, 封装, 继承, 多态.

封装: 只通过外部接口调用, 以特定的访问权限.
实现封装: 类声明中的`{}`

多态: 同一名称, 不同的功能实现方式.
目的: 减少程序标识符的个数.

***
设计`类`就是设计`类型`.

```cpp
class 类名称
{
public:
公有成员(外部接口)
private:
私有成员
protected:
保护成员
}
```

紧跟在类名称后面声明私有成员, 则关键字`private`可以省略.

### 对象定义的语法

```cpp
类名 对象名; // 例如
Clock myClock
```

类中的成员可以占直接互相访问, 外部要访问使用`对象名.成员名`.

***
类中的函数可以在类外使用`类::函数名`定义函数体. 比较简单的函数, 可以声明为内联成员函数.
内联函数体中不要有复杂结构(如循环语句和`switch`语句)

+ 直接把函数体放在类的声明中
+ 在类外使用`inline`关键字声明.

### 初始化构造函数

类中的特殊函数, 用于描述初始化算法. 在对象被创建时使用特定的值构建对象, 将对象初始化为一个特定的初始状态.

构造函数的形式:

+ 函数名与类名相同
+ 不能定义返回值类型, 也不能有`return`语句.
+ 可以有形式参数, 也可以没有形式参数
+ 可以是内联函数
+ 可以重载
+ 可以带默认参数值

构造函数在创建对象时, 自动被调用. 默认构造函数要么不需要实参, 要么全部参数带有默认值. default constructor.

```cpp
Clock(); //这两个构造函数不能同时出现.
Clock(int newH...);
```

隐含生成的构造函数: 如果程序未定义构造函数, 编译器将自动生成一个默认构造函数.

+ 参数表为空, 不为数据成员设置初始值.
+ 如果类内定义了成员的初始值, 则使用内类定义的初始值.
+ 如果没有定义类内的初始值, 则以默认方式初始化.
+ 基本类型的数据默认初始化是不确定的.

可以使用`Clock()=default`强制编译器生成默认构造函数.

构造函数可以有多个重载形式.

### 委托构造函数

委托构造函数使用类的其他构造函数执行初始化过程. 例如:

```cpp
Clock(int newH, int newM, int newS):hour(newH),minute(newM),second(newS){}
Clock():Clock(0,0,0){} //默认构造函数可以调用带参数的构造函数.
```

### 复制构造函数

使用对象`A`给对象`B`初始化, 需要用到复制构造函数.

```cpp
class 类名{
public: 类名(形参); //初始化构造函数
类名(const 类名 &对象名);// 复制构造函数, const 是为了保证常引用, 单向引用, 保持安全性
//...
};
//...
类名::类(const 类名 &对象名)//复制构造函数的实现
{ 函数体 }
```

调用复制构造函数的情况:

+ 对象`A`初始化对象`B`
+ 函数的形参是类的对象, 形实结合使用复制构造.
+ 如果函数的返回值是类的对象, 函数执行完返回一个临时无名对象, 传递给主调函数, 也发生复制构造.

如果没有定义, 则编译器产生隐含的复制构造函数.
将两个对象的数据成员一一对应.

如果不希望对象被复制构造.

`c++98`: 将复制函数声明为 `private`, 并且不提供函数实现.
`c++11`:用`=delete`提示编译器不生成默认复制构造函数.

```cpp
class Point{
   public:
   Point(int xx=0,int yy=0){x=xx; y=yy;}
   Point(const Point& p)=delete;// 提示编译器不生成默认复制构造函数
   private:
   int x,y;//私有数据
};
```

```cpp
int main(){
   Point a; // 第一个对象A
   Point b(a); // 情况一, 用A初始化B, 调用拷贝构造函数
   cout <<b.getX() <<endl; //情况二, 对象B作为fun1的实参, 调用拷贝构造函数
   fun1(b); // 情况三, 函数的返回值是类对象, 函数返回时调用拷贝构造函数
   b=fun2();
   cout << b.getX()<<endl;
   return 0;
}
```

### 析构函数

当一个对象被调用时, 会自动调用构造函数. 当一个对象消亡, 会自动调用析构函数.
析构函数完成对象被删除前的一些清理工作.

比如函数中的局部对象, 在函数调用结束时要消亡. 在对象的生存周期结束的时刻, 系统自动调用析构函数.
如果程序中未声明析构函数, 编译器将自动产生一个析构函数, 其函数体为空.

析构函数的原型: `~类名()`;

析构函数没有参数, 没有返回类型, 没有`return`语句.

在`main`函数`return`之前, 要将所有存活的对象析构.

### 类的组合

类中的 `成员` 是另一个类的 `对象`--- `部件类`.
可以在已有抽象的基础上实现更复杂的抽象.

`类组合` 的构造函数设计: 不仅要负责本类中的基本类型成员的初始化, 还要将 `对象成员` 初始化.

声明形式:

```cpp
类名::类名(对象成员所需的形参, 本类成员形参):对象1(参数), 对象2(参数), ...
{// 函数体其他语句}
```

#### 构造组合类对象的初始化次序

`成员` 按照在类体中声明的次序初始化, 而不是按照 `初始化列表` 的给出次序.

首先对 `构造函数` 初始化列表中列出的成员: 基本成员和 `对象成员`, 进行初始化,
初始化次序是成员 在类体中 `定义` 的次序.

`对象成员`构造函数的调用次序: 按 `对象成员` 的定义顺序, 先声明者先构造.
`初始化列表`中未出现的成员对象, 调用默认构造函数(无形参)初始化.

处理完初始化列表之后, 再执行 `构造函数` 的函数体.

形实参结合的时候, 是从右边往左边传递的, 先结合右边.

### 前向引用声明

两个类中的函数, 相互引用对方的类名.

类应该先声明, 后使用. 如果需要在某个类声明之前, 就引用该类, 则应该进行引用声明.
前向引用声明只为程序引入一个标识符, 类体可以在其他地方声明.

```cpp
calss B; //前向引用声明
class A{
   public:
   void f(B b);
};
class B{
   public:
   void g(A a);
}
```

前向引用声明注意事项:

+ 在提供一个完整的类声明之前, 不能声明该类的对象, 也不能在内联函数成员中使用该类的对象.
+ 当使用前向引用声明时, 只能使用被声明的符号, 而不能涉及类的任何细节.

```cpp
class Fred; //前向引用声明
class Barney{
   Fred x; //错误:类 Fred 的声明尚不完善,无法创建对象.
};
class Fred{
   Barney y;
};
```

### UML 简介

`UML`是可视化的面向对象的建模语言.

`UML`有三个基本的部分:

+ 事物(Things)
+ 关系(Relationships)
+ 图(Diagrams)

类图举例:

```UML
Clock
--------
- hour: int
- minute:int
- second:int
+ showTime();void
+ setTime(newH:int=0,newM:int=0,newS:int=0);void
```

对象图

```UML
myClock : Clock
--------
- hour: int
- minute:int
- second:int
```

依赖关系

```UML
类A---->类B
```

+ 关联作用.
+ 包含关系--聚集:
   共享聚集: 部分可以参加多个整体.比如窗体和输入法.
   组成聚集:整体拥有各个部分,整体与部分共存, 如果整体不存在了, 部分也不存在.如窗体和按钮
+ 继承关系--泛化: 子类继承父类.

### 结构体

结构体是一种特殊形态的类. 它与类的唯一区别是: 类的缺省访问是`priavte`, 结构体的缺省访问权限是`public`.

什么时候用结构体?
定义主要用来保存数据, 而没有什么操作.
人们习惯将结构体的数据成员设为公有, 因此用结构体更方便.
主要为了兼容`C`.

```cpp
struct 结构体名称{
   公有成员
   protected:
   保护型成员
   private:
   私有成员
}
```

结构体中可以有数据成员和函数成员, `C`只有数据成员.

### 结构体的初始化

+ 如果一个结构体的全部数据成员都是公共成员
+ 没有用户定义的`构造函数`.
+ 没有基类和虚函数

这个结构体可以用下面的语法形式初始化:

```cpp
类型名 变量名 = {成员数据1初值,成员数据2初值,....}
```

例如用结构体表示学生的基本信息.

```cpp
#include <iostream>
#include<iomanip>
#include<string>
using namespace std;

struct Student
{//学生信息结构体
    int num; //学号
    string name; //姓名, 字符串对象
    char sex; //性别
    int age; //年龄
};
int main(){
    Student stu={97001,"Lin Lin",'F',19}
    cout<<"Num: "<<stu.num<<endl;
    cout<<"Name: "<<stu.name<<endl;
    cout<<"Sex: "<<stu.sex<<endl;
    cout<<"Age: "<<stu.age<<endl;
    return 0;
}
```

### 联合体

不常用的数据类型, 联合体的目的是存储空间的共用. 定义形式:

```cpp
union 联合体名称{
   公有成员
   protected:
   保护型成员
   private:
   私有成员
};
```

特点:

+ 成员共用`同一组`内存单元.
+ 任何两个成员不会同时有效.

```cpp
union Mark{ //表示成绩的联合体
   char grade; //等级制的乘积
   bool pass; //只记是否通过课程的乘积
   int percent; //百分制的乘积.
};
```

如果`int percent`占四个`bit`,则 `Mark` 占用`4`个字节, 即按照最长的乘员类型存储.

### 无名联合体

还可以定义无名联合体, 它们共用内存.

```cpp
union{
   int i;  float f;
}
i =10; //在程序中使用
f=2.2;
```

### 枚举类

枚举类也称为强类型枚举. 之前的枚举元素只能是整数的子集.

枚举类定义:

```cpp
enum class 枚举类型名: 底层类型{枚举值列表}; //默认的底层类型是 int 类型
//例如:
enum class Type {General, Light, Medium,Heavy};
enum class Type:char{General, Light, Medium,Heavy};
enum class Category {General=1, Pistol,MachineGun,Cannon}; //可以指定枚举常量的值, 后面依次递增.
```

枚举类的优势:

+ 强作用域:其作用域限制在枚举类中. 使用时应该使用`Type::General`,所以枚举值可以重名.
+ 转换限制: 枚举类对象不可以与整型隐式地互相转换.
+ 可以指定底层类型: 如`enum class Type:char{Light, Medium,Heavy};`

```cpp
#include<iostream>
using namespace std;
enum class Side{Right,Left};
enum class Thing{Wrong,Right}; //不冲突
int main{
   Side s=Side::Right;
   Thing w=Thing::Wrong;
   cout <<(s ==w) <<endl; //编译错误, 无法直接不同枚举类.
   return 0;
}
```

## 标识符的作用域与可见性

+ 属于类的成员: 静态数据成员
+ 用于处理静态数据成员的函数: 静态成员函数.

友元: 对一些类外的函数, 其他的类, 给予授权, 使之可以访问类的私有成员.
为了安全, 可以用`const`修改.

### 标识符的作用域与可见性

+ 函数原型作用域: 函数原型中的参数, 也就是形参表的括号之内, 写不写名字都可以.
+ 局部作用域
+ 类作用域
+ 文件作用域
+ 命名空间作用域

函数原型作用域

```c++
double area(double radius);
```

***
局部作用域, 或者叫块作用域.

+ 函数的形参, 在块中声明的标识符; 用`{}`创建作用域.
+ 作用域自声明处起, 限于块中.

***
类作用域:

类的成员具有类作用域, 其范围包括类体和成员函数体.

在类作用域以外访问类的成员:
静态成员: 通过类名, 或者该类的对象名, 对象引用访问. 例如`对象.属性`
非静态成员: 通过类名, 或者该类的对象名, 对象引用, 对象指针访问.

***
文件作用域:
不在前述各个作用域中出现的声明, 就具有文件作用域;
其作用域开始于声明点, 结束于文件尾;

#### 可见性

即使一个标识符在作用域之内, 也不一定是可见的.
可见性从引用角度来说, 表示从内层作用域向外层作用域"看"时能看见什么.
如果标识符在某处可见, 就可以引用.

如果标识符在外层中声明, 且在内层中没有被同名标识符覆盖, 则可见.
如果重名, 则不可见.

### 对象的生存期

C++程序中, 对象和变量的生存期分为静态生存期和动态生存期.

***
静态生存期

+ 静态生存期与程序的运行期相同.
+ 在文件作用域中生命的对象具有这种周期.
+ 在函数内部声明静态生存期对象, 要冠以关键字`static`

***
动态生存期

开始于程序执行到声明点时, 结束于最近的作用域结束时.
块作用域中声明的, 没有用`static`修饰的对象, 是动态生存期的对象(习惯上称为局部生存期对象).

### 类的静态数据成员

+ 用关键字`static`声明,
+ 为该类的所有对象共享, 静态数据成员具有静态生存周期
+ 必须在类外定义和初始化, 用`::`来指明所属的类.

### 静态函数成员

静态函数成员用于处理`静态数据`. 静态函数不知道自己被哪个具体对象`实例`调用. 它用来处理类的`公有静态成员`.

对象的一般`成员函数`, 即`非静态函数`, 通常隐含了用指针传入一个`this`指针, 指向`对象`本身.

### 类的友元

+ 友元是`C++`提供的一种破坏数据封装和数据隐藏的机制.
+ 通过将一个模块声明为另一个模块的友元, 一个模块能够引用到另一个模块中本该被隐藏的信息.
+ 可以声明友元函数和友元类.
+ 为了确保数据的完整性, 及数据封装与隐藏的原则, 建议慎用友元.

#### 友元函数

友元函数是在类声明中由关键字`friend`修饰说明的非成员函数, 在它的函数体重能够通过对象名访问`pravite`和`protected`成员.
作用: 增加灵活性, 封装和效率的平衡.
访问对象中的成员需要通过对象名.

### 友元类

+ 若一个类为另一个类的友元, 则此类的所有成员都能访问对方类的私有成员.
+ 声明语法: 将友元类名在另一个类中使用`friend`修饰说明.

友元关系是单向的, 声明`B`类是`A`类的友元!=`A`类是`B`类的友元.

## 共享数据的保护

### 常类型

+ `常对象`: 必须进行初始化, 不能被更新.  专门用来处理常对象的函数称为常函数.

```c++
const 类名 对象名
```

常成员: 用 `const` 进行修饰的类成员: `常数据` 成员和 `常函数` 成员

+ `常引用`: 被引用的对象不能被更新.

```c++
const 类型说明符 &引用名;
```

+ `常数组`: 数组元素不能被更新

```c++
类型说明符 const 数组名[大小]
```

### 常指针

指向 `常量` 的指针

### 常成员函数

+ 使用 `const` 关键字说明的函数.
+ 常成员函数不更新 `对象` 的`数据成员`. 常成员函数说明格式:

```cpp
类型说明符 函数名(参数表) const;
```

+ 这里, `const`是函数类型的一个组成部分, 因此在实现部分也要带`const`关键字.
+ `const`关键字可以被用于参与对重载函数的区分
+ 通过 `常对象`只能调用它的`常成员函数`.
+ 普通 `对象`也可以调用`常函数`, 所以常函数可以主动声明为`const`.

### 常引用

在`友元函数`中用`常引用`作参数, 既能获得较高的`执行效率`, 又能保证`实参`的安全性.

### 多文件结构和编译预处理命令

`c++` 程序的一般组织结构

一个工程文件可以划分为多个源文件, 例如:

+ 类声明文件(`.h` 文件)
+ 类实现文件(`.cpp`文件)
+ 类的使用文件(`main()`所在的`.cpp`文件)

利用工程来组合各个文件.

自己写的头文件通常带`.h`, 调用时用`inclue "xxx.h"`用`"`括起来的, 默认会在当前目录寻找.
而用`<iostream>`括起来的, 会到安装的默认目录去找, 且没有后缀`h`.

实例的程序可以用如下命令编译:

```bash
g++ -lstdc++ Point.h Point.cpp 5_10.cpp -o test
```

#### 外部变量

+ 一个`变量`, 除了在定义它的`源文件`中可以使用, 还能被其他`文件`使用.
+ `文件作用域`中定义的`变量`, 默认情况下都是`外部变量`.
+ 如果需要在其他`文件`中使用, 需要用 `extern` 关键字声明.

#### 外部函数

+ 在所有类之外声明的函数(也就是非成员函数), 都是具有文件作用域的.
+ 这样的函数都可以在不同的编译单元中被调用.
+ 只要在调用之前进行引用性声明(即声明函数原型)即可.

#### 将变量和函数限制在编译单元内

在匿名命名空间中定义的变量和函数, 都不会暴露给其他的编译单元. 语法形式为:

```c++
namespace { //匿名的命名空间
   int n;
   void f() {n++;}
}
```

#### 标准c++库

标准c++库是一个极为灵活并可扩展的可重用软件模块的集合.
标准c++类与组件在逻辑上分为6种类型:

+ 输入/输出类, iostream
+ 容器类与抽象数据类型, 类模板,数据操作.
+ 存储管理类,
+ 算法.查找, 排序等等.
+ 错误支持
+ 运行环境支持

#### 编译预处理

+ `#include`包含指令:将一个源文件嵌入到当前源文件中该点处.
    + `#include<文件名>`: 按标准方式搜索, 文件位于`c++`系统目录的 `include` 子目录下.
    + `#include"文件名"`: 首先在当前目录中搜索, 若没有, 再按标准方式搜索.

+ `#define`: 宏定义指令.
    + 定义符号常量, 很多情况下已被`const`定义语句取代.
    + 定义带参数宏, 已被内联函数取代.

+ `#undef`: 删除由`#define`定义的宏, 使之不再起作用.

+ 条件编译指令 -- `#if`,`#endif`,`#ifdef`

```c++
#if 常量表达式1
   程序正文1 //当"常量表达式1" 非零时编译
#elif 常量表达式2
程序正文2 //当"常量表达式2" 非零时编译
#else
程序正文3 //其他情况下编译
#endif
```

***
如果"标识符"经`#defined`定义过, 且未经`#undef`删除, 则编译程序段`1`; 否则编译程序段`2`

```c++
#ifdef 标识符
   程序段1
#else
   程序段2
#endif
```

如果"标识符"未被`#defined`定义过, 则编译程序段`1`; 否则编译程序段`2`

```c++
#ifndef 标识符
   程序段1
#else
   程序段2
#endif
```

可以这么使用: 在头文件中设置`标识符`, 如果`标识符`未被定义过, 就编译某段程序, 并定义`标识符`.
如果这段程序被多次包含, 就会避免重复编译这段程序.
这样多次`include`时, 也不会重复定义函数和常量.