# Cpp 语法理解

## const int* 与 int const*

[C++里 const int* 与 int const* 有什么区别?](https://www.zhihu.com/question/443195492/answer/1723886545)

不管const写成如何, 读懂别人写的const和*满天飞的类型的金科玉律是
const默认作用于其左边的东西, 否则作用于其右边的东西:

const applies to the thing left of it.
If there is nothing on the left then it applies to the thing right of it.[1]

例如,

    const int* const

只有右边有东西, 所以const修饰int成为常量整型, 然后*再作用于常量整型. 所以这是a pointer to a constant integer(指向一个整型, 不可通过该指针改变其指向的内容, 但可改变指针本身所指向的地址)

    int const *

再看这个, const左边有东西, 所以const作用于int, *再作用于int const所以这还是 a pointer to a constant integer(同上)

    int* const

这个const的左边是*, 所以const作用于指针(不可改变指向的地址), 所以这是a constant pointer to an integer, 可以通过指针改变其所指向的内容但只能指向该地址, 不可指向别的地址.

    const int* const

这里有两个const. 左边的const 的左边没东西, 右边有int那么此const修饰int. 右边的const作用于*使得指针本身变成const(不可改变指向地址), 那么这个是a constant pointer to a constant integer, 不可改变指针本身所指向的地址也不可通过指针改变其指向的内容.

    int const * const

这里也出现了两个const, 左边都有东西, 那么左边的const作用于int, 右边的const作用于*, 于是这个还是是a constant pointer to a constant integer

    int const * const *

懒得分析了, 照葫芦画瓢, a pointer to a constant pointer to a constant integer, 其实就是指向上边那个的东西的指针.

    int const * const * const

上边的指针本身变成const, a constant pointer to a constant pointer to a constant integer.

再扯两句, 因为const的作用机理和阅读习惯, 码农之间产生了两种写const的流派, 一个是所谓的western const style也即把const写在西边(左边), 例如const int, 符合人类语言习惯;另一个流派叫eastern const style, 遵照上边的规则把const写在东边(右边), 例如int const. 从代码可读性易维护性出发, 强烈推荐把const写在右边(包括自己学的时候老师如是安利. . . ), 可以跟指针的作用范围很好地统一起来不至于混乱.

[const-before-or-const-after](https://stackoverflow.com/questions/5503352/const-before-or-const-after)

***
唯一区别是后者无脑从右往左读就行.

```cpp
int const: pointer to const int
int const *const: const pointer to const int
int *const: const pointer to int
int *const *: pointer to const pointer to int
```

## C++中引用传递与指针传递区别

[https://www.iteye.com/blog/xinklabi-653643](https://www.iteye.com/blog/xinklabi-653643)

从概念上讲. 指针从本质上讲就是存放变量地址的一个变量, 在逻辑上是独立的, 它可以被改变, 包括其所指向的地址的改变和其指向的地址中所存放的数据的改变.

而引用是一个别名, 它在逻辑上不是独立的, 它的存在具有依附性, 所以引用必须在一开始就被初始化, 而且其引用的对象在其整个生命周期中是不能被改变的(自始至终只能依附于同一个变量).

在C++中, 指针和引用经常用于函数的参数传递, 然而, 指针传递参数和引用传递参数是有本质上的不同的:

指针传递参数本质上是值传递的方式, 它所传递的是一个地址值. 值传递过程中, 被调函数的形式参数作为被调函数的局部变量处理, 即在栈中开辟了内存空间以存放由主调函数放进来的实参的值, 从而成为了实参的一个副本. 值传递的特点是被调函数对形式参数的任何操作都是作为局部变量进行, 不会影响主调函数的实参变量的值. (这里是在说实参指针本身的地址值不会变)

而在引用传递过程中, 被调函数的形式参数虽然也作为局部变量在栈中开辟了内存空间, 但是这时存放的是由主调函数放进来的实参变量的地址. 被调函数对形参的任何操作都被处理成间接寻址, 即通过栈中存放的地址访问主调函数中的实参变量. 正因为如此, 被调函数对形参做的任何操作都影响了主调函数中的实参变量.

引用传递和指针传递是不同的, 虽然它们都是在被调函数栈空间上的一个局部变量, 但是任何对于引用参数的处理都会通过一个间接寻址的方式操作到主调函数中的相关变量. 而对于指针传递的参数, 如果改变被调函数中的指针地址, 它将影响不到主调函数的相关变量. 如果想通过指针参数传递来改变主调函数中的相关变量, 那就得使用指向指针的指针, 或者指针引用.

为了进一步加深大家对指针和引用的区别, 下面我从编译的角度来阐述它们之间的区别:

程序在编译时分别将指针和引用添加到符号表上, 符号表上记录的是变量名及变量所对应地址. 指针变量在符号表上对应的地址值为指针变量的地址值, 而引用在符号表上对应的地址值为引用对象的地址值. 符号表生成后就不会再改, 因此指针可以改变其指向的对象(指针变量中的值可以改), 而引用对象则不能修改.

最后, 总结一下指针和引用的相同点和不同点:

### 相同点:

+ 都是地址的概念;

指针指向一块内存, 它的内容是所指内存的地址;而引用则是某块内存的别名.

### 不同点:

+ 指针是一个实体, 而引用仅是个别名;

+ 引用只能在定义时被初始化一次, 之后不可变;指针可变;引用"从一而终", 指针可以"见异思迁";

+ 引用没有const, 指针有const, const的指针不可变;(具体指没有int& const a这种形式, 而const int& a是有 的,  前者指引用本身即别名不可以改变, 这是当然的, 所以不需要这种形式, 后者指引用所指的值不可以改变)

+ 引用不能为空, 指针可以为空;

+ "sizeof 引用"得到的是所指向的变量(对象)的大小, 而"sizeof 指针"得到的是指针本身的大小;

+ 指针和引用的自增(++)运算意义不一样;

+ 引用是类型安全的, 而指针不是 (引用比指针多了类型检查

### 引用的概念

引用引入了对象的一个同义词. 定义引用的表示方法与定义指针相似, 只是用&代替了*.
例如:  Point pt1(10,10);
Point &pt2=pt1; 定义了pt2为pt1的引用. 通过这样的定义, pt1和pt2表示同一对象.
需要特别强调的是引用并不产生对象的副本, 仅仅是对象的同义词. 因此, 当下面的语句执行后:
pt1.offset(2, 2);
pt1和pt2都具有(12, 12)的值.
引用必须在定义时马上被初始化, 因为它必须是某个东西的同义词. 你不能先定义一个引用后才
初始化它. 例如下面语句是非法的:

```cpp
Point &pt3;
pt3=pt1;
```

那么既然引用只是某个东西的同义词, 它有什么用途呢?
下面讨论引用的两个主要用途: 作为函数参数以及从函数中返回左值.

### 引用参数

1, 传递可变参数
传统的c中, 函数在调用时参数是通过值来传递的, 这就是说函数的参数不具备返回值的能力.
所以在传统的c中, 如果需要函数的参数具有返回值的能力, 往往是通过指针来实现的. 比如, 实现
两整数变量值交换的c程序如下:
void swapint(int *a,int *b)
{
int temp;
temp=*a;
a=*b;
*b=temp;
}

使用引用机制后, 以上程序的c++版本为:
void swapint(int &a,int &b)
{
int temp;
temp=a;
a=b;
b=temp;
}
调用该函数的c++方法为: swapint(x,y); c++自动把x,y的地址作为参数传递给swapint函数.

2, 给函数传递大型对象
当大型对象被传递给函数时, 使用引用参数可使参数传递效率得到提高, 因为引用并不产生对象的
副本, 也就是参数传递时, 对象无须复制. 下面的例子定义了一个有限整数集合的类:
const maxCard=100;
Class Set
{
int elems[maxCard]; // 集和中的元素, maxCard 表示集合中元素个数的最大值.
int card; // 集合中元素的个数.
public:
Set () {card=0;} //构造函数
friend Set operator * (Set ,Set ) ; //重载运算符号*, 用于计算集合的交集 用对象作为传值参数
// friend Set operator * (Set & ,Set & ) 重载运算符号*, 用于计算集合的交集 用对象的引用作为传值参数
...
}
先考虑集合交集的实现
Set operator *( Set Set1,Set Set2)
{
Set res;
for(int i=0;i<Set1.card;++i)
for(int j=0;j>Set2.card;++j)
if(Set1.elems[i]==Set2.elems[j])
{
res.elems[res.card++]=Set1.elems[i];
break;
}
return res;
}
由于重载运算符不能对指针单独操作, 我们必须把运算数声明为 Set 类型而不是 Set * .
每次使用*做交集运算时, 整个集合都被复制, 这样效率很低. 我们可以用引用来避免这种情况.
Set operator *( Set &Set1,Set &Set2)
{ Set res;
for(int i=0;i<Set1.card;++i)
for(int j=0;j>Set2.card;++j)
if(Set1.elems[i]==Set2.elems[j])
{
res.elems[res.card++]=Set1.elems[i];
break;
}
return res;
}

### 引用返回值

如果一个函数返回了引用, 那么该函数的调用也可以被赋值. 这里有一函数, 它拥有两个引用参数并返回一个双精度数的引用:
double &max(double &d1,double &d2)
{
return d1>d2?d1:d2;
}
由于max()函数返回一个对双精度数的引用, 那么我们就可以用max() 来对其中较大的双精度数加1:
max(x,y)+=1.0;

## Reading C type declarations

[Reading C type declarations](http://unixwiz.net/techtips/reading-cdecl.html)
[如何理解c/c++语言的声明以及类型-Colliot的回答](https://www.zhihu.com/question/49673652/answer/1044845311)
[如何理解c/c++语言的声明以及类型-invalid s](https://www.zhihu.com/question/49673652/answer/117197517)

很简单, 从被声明的标识符开始, 先右后左, 遇括号返回.

int A;
很简单, A是一个int

int *A;

从A开始: A是一个变量, *表示它是一个指针变量, int表示这个指针变量指向int

int **A;

A是一个变量, *表示它是一个指针变量, 第二个*表示前一个指针变量指向的类型还是一个指针,
int表示A这个指针变量指向的指针变量指向一个int

int A[5];

A是一个变量, (向右)这个变量是一个有5个元素的数组, (向左)这个数组存储int

int *A[5];

A是一个变量, (向右)这个变量是一个有5个元素的数组, (向左)这个数组存储指针, 指针指向的类型是int

int **A[5];

A是一个变量, (向右)这个变量是一个有5个元素的数组, (向左)这个数组存储指针,
指针指向的类型还是一个指针, 最终指向的数据类型是int

当然还有很多更为复杂, 连老鸟都能搞晕的写法.
那些就属于魔道了. 真需要作复杂的声明时,
建议用typedef一层层定义别名, 最后用typedef的别名完成声明.
这样写出的声明易懂, 无歧义, 便于阅读.

[C语言复杂声明解析](https://blog.csdn.net/wangweixaut061/article/details/6549768)
