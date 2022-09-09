# 指针

## 用指针访问数组

+ 定义指向数组元素的指针:

```cpp
int a[10], *pa
pa=&a[0]; 或 pa=a;
```

+ 等效的形式:

    + `*pa` 就是 `a[0]`, `*(pa+1)` 就是 `a[1]`, ... , `*(pa+i)` 就是 `a[i]`
    + `a[i]`, `*(pa+i)`, `*(a+i)`, `pa[i]` 都是等效的.

## 指针数组

数组是有一组类型相同的, 一组连续存储的, 具有顺序关系的元素构成的, 如果元素是指针的话, 就是指针数组.
例如:

    Point *pa[2];
    由 pa[0], pa[1] 两个只针组成

```cpp
#include  <iostream>
using namespace std;
int main(){
    int line1[]={1,0,0}; //矩阵的第一行
    int line2[]={0,1,0}; //矩阵的第二行
    int line3[]={0,0,0}; //矩阵的第三行

    //定义整型指针数组并初始化
    int *pLine[3] ={line1, line2, line3};
    cout << "Matrix test:" << endl;
    //输出矩阵
    for (int i=0;i<3,i++){
        for (int j=0, j<3,j++)
        cout << pLine[i][j] << " ";
        cout << endl;
    }
    return 0;
}
```

对比例6-8 中的指针数组和如下二维数组

```cpp
int array2[3][3]={{1,0,0,},{0,1,0},{0,0,1}};
```

指针数组是树形结构存放,
二维数组是将行连接起来变成一维数组存放.

## 以指针作为函数的参数

函数参数可以是指针.
函数的参数传递分成`值传递`和`引用传递`. 传`指针`是引用传递的一种.

+ 需要数据双向传递时(引用也可以达到此效果)
+ 需要传递一组数据, 只传首地址运行效率比较高

```cpp
#include <iostream>
using namespace std;

//将实数 x 分成整数部分和小数部分, 形参 intpart, fracpart 是指针
void splitFloat(float x, int *intPart, float *fracPart){
    *intPart= static_cast<int>(x); // 取x的整数部分
    *fracPart=x-*intPart; // 取x的小数部分
}
int main(){
    cout << "Enter 3 float point number;" << endl;
    for (int i=0;i<3;i++){
        float x,f;
        int n;
        cin >. x;
        splitFloat(x,  &n,  &f); // 变量地址作为实参; &n, &f 为地址运算, 实参是地址.
        cout <<"Interger Part= " <<n << " Fraction Part = "<< f << endl;
    }
    return 0;
}
```

+ 用指向常量的指针作参数

```cpp
#include<iostream>
using namespace std;
const int N=6;
void print(const int *p, int n); // 使用常指针避免修改
int main(){
    int array[n];
    for (int i=0; i<N; i++){
        cin >> array{i};
    }
    print(arrary,N);
    return 0;
}
void print(const int *p, int n){
    cout<< "{" <<*p;
    for(int i=1;i<n;i++){
        cout <<", "<<*(p+i);
        cout<<"}" <<endl;
    }
}
```

## 指针类型的函数

即返回值为`指针`类型的函数, 向主调函数返回指针.

+ 指针函数的定义形式:

```cpp
存储类型 数据类型 *函数名(){
    // 函数体语句
}
```

不要将非静态局部地址用作函数的返回值.

错误的例子: 在子函数中定义局部变量后将其地址返回给主函数, 就是非法地址.

```cpp
int main(){
    int* function();
    int *ptr=function();
    *ptr=5; // 危险的访问!
    return 0;
}
int *function(){
        int local=0; // 非静态局部变量作用域和寿命都仅局限于本函数内
        return &local;
} // 函数运行结束时, 变量 local 被释放
```

+ 返回的指针要确保在主调函数中是有效的, 合法的地址.

正确的例子:
主函数中定义的数组, 在子函数中对该元素进行某种操作后, 返回其中一个元素的地址, 这就是合法有效的地址.

```cpp
#include<iostream>
using namespace std;
int main(){
    int array[10]; //主函数中定义的数组
    int *search(int* a, int num);
    for(int i=0;i<10;i++)
    {cin>>array[i]};
    int *zeroptr=search(array,10);// 将主函数中数组的首地址传给子函数
    return 0;
}
int* search (int* a, int num){ //指针a 指向主函数中定义的数组
for(int i=0;i<num;i++){
    if(a[i]==0)
        return &a[i]; // 返回第i个元素的地址, 指向的元素是在主函数中定义的
}
}// 函数运行结束时, a[i] 的地址仍有效
```

+ 正确的例子:
在子函数中通过动态内存分配 `new` 操作取得的内存地址返回给主函数是合法有效的,
但是内存分配和释放不在同一级别, 要注意不能忘记释放, 避免内存泄漏.

```cpp
#include<iostream>
using namespace std;
int main(){
   int *newintvar();
   int* intptr= newintvar();
   *intptr=5;//访问的是合法有效的地址
   delete intptr;// 如果忘记在这里释放, 会造成内存泄漏
   return 0;
}
int* newintvar(){
    int* p=new int();
    return p;//返回的地址指向的是动态分配的空间
}// 函数运行结束时, p 的地址仍有效
```

## 指向函数的指针

运行中的地址, 在内存中占有存储空间, 一段段代码或者函数, 它们也有地址.
如果指针中容纳的是函数代码的起始地址, 这个指针就称为指向函数的指针.

+ 函数指针的定义

```cpp
存储类型 数据类型 (*函数指针名)(); // 如果没有 (), 则变成了返回指针类型的函数.
```

含义: 函数指针指向的是程序代码存储区, 的起始位置, 也就是函数代码的首地址.

### 函数指针的典型用途--实现函数回调

用函数指针充当函数名

+ 通过函数指针调用的函数: 例如将函数的指针作为参数传递给一个函数, 使得在处理相似事件的时候, 可以灵活地使用不同的方法.
+ 调用者不关心谁是被调用者: 需知道存在一个具有特定原型和限制条件的被调用函数

### 函数指针举例

+ 编写一个计算函数 compute, 对两个整数进行各种计算.
有一个形参为指向具体算法函数的指针, 根据不同的实参函数, 用不同的算法进行计算.
+ 编写三个函数: 求两个整数的最大值, 最小值和平均值, 分别用这三个函数作为实参, 测试 compute 函数.

```cpp
#include<iostream>
using namespace std;

int compute(int a, int b, int(*func)(int,int)){
    return func(a,b);
    }

int max(int a,int b) //求最大值
{return ((a>b)?a:b);}

int min(int a,int b) //求最大值
{return ((a<b)?a:b);}

int main(){
    int a,b,res;
    cout<< "请输入整数 a: "; cin>> a;
    cout<< "请输入整数 b: "; cin>> b;

    res= compute(a,b,&max);
    cout<<"Max of "<< a << " and " << b << " is " << res <<endl;
    res= compute(a,b,&min);
    cout<<"min of "<< a << " and " << b << " is " << res <<endl;
    res= compute(a,b,&sum);
    cout<<"Sum of "<< a << " and " << b << " is " << res <<endl;
}
```

## 对象指针

+ 用来容纳对象地址的指针, 定义形式

    类名 *对象指针名

例子:

```cpp
Point a(5,10);
    point *ptr;
    ptr = &a;
```

+ 通过指针访问对象成员

    对象指针名 -> 成员名

例: `ptr->getx()` 相当于 `(*ptr).getx()`; 例子:

```cpp
#include<iostream>
using namespace std;

class Point {
    public:
        Point(int x=0, int y=0):x(x),y(y){}
        int getX() const{return x;}
        int getY() const{return y;}
    private:
        int x,y;
};

int main(){
    Point a(4,5);
    Point *p1=&a; // 定义对象指针, 用 a的对象初始化
    cout <<p1->getX()<<endl; //用指针访问对象成员
    cout << a.getX()<<endl; //用对象名访问对象成员
    }
```

### this 指针

+ 隐含于类的每一个非静态成员函数中(即属于对象的成员函数).
+ 指出成员函数所操作的对象.
    + 当通过一个对象调用成员函数时, 系统先将该对象的地址赋给this指针, 然后调用成员函数, 成员函数对对象的数据成员进行操作时, 就隐含使用了 `this` 指针.
+ 例如,  Point 类的 getX 函数中的语句:

        return x;
        相当于
        return this->x;

按照一般的逻辑, 函数不关心是被谁调用的, 所以需要用 `this` 声明.

```cpp
class Fred; //前向引用声明
class Barney{
    Fred x; //错误: 类 Fred 的声明尚不完善, 不知道分配多少字节的空间
};
class Fred{
    Barney y;
}
```

```cpp
class Fred; //前向引用声明
class Barney{
    Fred *x;
};
class Fred{
    Barney y;
}
```

## 动态分配与内存释放

`动态分配` 的内存, 没有名称, 只能用 `首地址` 访问.

+ 动态申请内存操作符 `new`

    new 类型名T (初始化参数列表)

+ 功能: 在程序执行期间, 申请用于存放 T 类型对象的内存空间, 并依初值列表赋以初值.
+ 结果值:
    + 成功: T 类型的指针, 指向新分配的内存; 失败: 抛出异常

+ 释放内存操作符 delete

    delete 指针p

+ 功能: 释放指针 p 所指向的内存. p必须是 new 操作的返回值(地址).

例子:

```cpp
#include <iostream>
using namespace std;

class Point
{
public:
    Point() : x(0), y(0)
    {
        cout << "Default Constructor called" << endl;
    }
    Point(int x, int y) : x(x), y(y)
    {
        cout << "Constructor called." << endl;
    }
    ~Point() { cout << "Destructor called." << endl; }
    int getX() const { return x; }
    int getY() const { return y; }
    void move(int newX, int newY)
    {
        x = newX;
        y = newY;
    }

private:
    int x, y;
};

int main()
{
    cout << "Step one: " << endl;
    Point *ptr1 = new Point; //调用默认构造函数
    delete ptr1;             // 实例程序, 删除对象, 自动调用析构函数

    cout << "Step two: " << endl;
    ptr1 = new Point(1, 2); //动态分配另一个对象
    delete ptr1; // 删除并不是删除 ptr1, 而是删除指向的对象.

    return 0;
}
```

## 申请和释放动态数组(1)

编写程序时, `不能确定` 数组的大小.

### 分配和释放动态数组

+ 分配:  `new 类型名T[数组长度]`
    数组长度可以是任何整数表达式, 在运行时计算

+ 释放: `delete[] 数组名 p`
    释放指针 `p` 所指向的数组, `p` 必须是用 `new` 分配得到的 `数组首地址`.
    不仅要有 `数组名`, 还要有 `方括号`. 如果没有方括号, 只释放 `数组首元素` 的地址.

例子:

```cpp
#include <iostream>
using namespace std;
// Point 类的定义
class Point
{
public:
    Point() : x(0), y(0)
    {
        cout << "Default Constructor called" << endl;
    }
    Point(int x, int y) : x(x), y(y)
    {
        cout << "Constructor called." << endl;
    }
    ~Point() { cout << "Destructor called." << endl; }
    int getX() const { return x; }
    int getY() const { return y; }
    void move(int newX, int newY)
    {
        x = newX;
        y = newY;
    }

private:
    int x, y;
};

int main()
{
    Point *ptr = new Point[2]; //创建对象数组
    ptr[0].move(5, 10);        //通过指针访问数组元素的成员
    ptr[1].move(15, 20);       //通过指针访问数组元素的成员
    cout << "Deleteing..." << endl;
    delete[] ptr; // 删除整个对象数组, 注意要加 []
    return 0;
}
```

### 动态创建多维数组

    new 类型名T[第1维长度][第2维长度]...;

+ 如果内存申请成功, `new` 运算符返回一个指向新分配内存首地址的指针.
+ 例如:

    ```cpp
    char (*fp)[3]; // fp 是一个指向子数组整体的指针
    fp=new char[2][3];
    ```

        fp ->       fp[0][0]
                        fp[0][1]
                        fp[0][2]
        fp+1 ->  fp[1][0]
                        fp[1][1]
                        fp[1][2]

指向数组元素的, 指针加一, 指向下一个元素.
指向多维数组行的, 指针加一, 指向下一行元素.

```cpp
#include <iostream>
using namespace std;

int main()
{
    int(*cp)[9][8] = new int[7][9][8];
    for (int i = 0; i < 7; i++)
        for (int j = 0; j < 7; j++)
            for (int k = 0; k < 7; k++)
                *(*(*(cp + i) + j) + k) = (i * 100 + j * 10 * k);

    for (int i = 0; i < 7; i++)
    {
        for (int j = 0; j < 7; j++)
        {
            for (int k = 0; k < 7; k++)
            {
                cout << cp[i][j][k] << " ";
            }
            cout << endl;
        }
        cout << endl;
    }
    delete[] cp; //多维数组的释放也只需要一个方括号
    return 0;
}
```

## 申请和释放动态数组(2)

申请和释放的过程有些繁琐, 将动态数组封装成类

+ 更加简洁, 便于管理
+ 可以在访问数组元素前检查下标是否越界

```cpp
#include <iostream>
#include <cassert>
using namespace std;

class Point
{
public:
    Point() : x(0), y(0)
    {
        cout << "Default Constructor called" << endl;
    }
    Point(int x, int y) : x(x), y(y)
    {
        cout << "Constructor called." << endl;
    }
    ~Point() { cout << "Destructor called." << endl; }
    int getX() const { return x; }
    int getY() const { return y; }
    void move(int newX, int newY)
    {
        x = newX;
        y = newY;
    }

private:
    int x, y;
};

class ArrayOfPoints
{ // 动态数组类
public:
    ArrayOfPoints(int size) : size(size)
    {
        points = new Point[size]; // 在构造函数中分配内存
    }
    ~ArrayOfPoints()
    {
        cout << "Deleting ..." << endl;
        delete[] points; //在析构函数中, 用 delete 释放内存
    }
    Point &element(int index)// 返回值为引用类型, 我们访问数组元素, 希望它是左值, 也就是 = 左边的值.
    {
        assert(index >= 0 && index < size);
        return points[index];
    }

private:
    Point *points; //指向动态数组首地址.
    int size;      //数组大小
};

int main()
{
    int count;
    cout << "Please enter the count of points: " << endl;
    cin >> count;
    ArrayOfPoints points(count);    // 创建数组对象
    points.element(0).move(5, 0);   //访问数组元素的成员
    points.element(1).move(15, 20); //访问数组元素的成员
    return 0;
}
```

>为什么 elements 函数返回对象的引用?
>返回 `引用` 可以用来操作封装数组对象内部的数组元素. 如果返回`值`则只是返回了一个`副本`, 也就是`右值`.
>通过`副本`是无法操作原来数组中的元素的. 只有返回`左值` 才能修改数组内的元素.

## 智能指针

手动 `new/delete` 具有安全隐患. 在 `C++ 11` 中提供了 `智能指针`.

+ `unique_ptr`: 不允许多个指针共享资源, 可以用标准库中的 `move` 函数转移指针(所有权).
+ `shared_ptr`: 多个指针共享资源.
+ `weak_ptr`: 可复制 `shared_ptr`, 但其构造或者释放对资源不产生影响.

## vector 对象

`vector` 是标准库中的 `类模板`, 使用起来就像数组, 但更加安全方便.
为什么需要 `vector` ?

+ 封装`任何类型`的动态数组, 自动创建和删除.
+ 数组下标越界检查.

+ `vector` 对象的定义

    vector<元素类型> 数组对象名(数组长度);

例如:

    vector<int> arr(5)

建立大小为`5`的`int` 数组

+ vector 对象的使用

+ 对数组元素的引用, 与普通数组具有相同的形式:

    vector对象名 [下标表达式]
    vector数组对象名不表示数组首地址

+ 获取数组长度: 用 `size` 函数

    vector对象名.size()

例 6-20 vector 应用举例

```cpp
#include <iostream>
#include <vector>
using namespace std;

int main()
{
    unsigned n;
    cout << "n= ";
    cin >> n;
    vector<double> arr(n); //创建数组对象
    cout << "Please input" << n << "real numbers: " << endl;
    for (unsigned i = 0; i < n; i++)
        cin >> arr[i];

    cout << "Average = " << average(arr) << endl;
    return 0
}

// 计算数组 arr 中元素的平均值

double average(const vector<double> &arr)
{
    double sum = 0;
    for (unsigned i = 0; i < arr.size(); i++)
        sum += arr[i];
    return sum / arr.size();
}
```

+ 基于范围的 for 循环配合 `auto` 举例

```cpp
#include <vector>
#include <iostream>

int main()
{
    std::vector<int> v = {1, 2, 3};
    for (auto i = v.begin(); i != v.end(); ++i) // 自动类型, 随初始值推导类型
        std::cout << *i << std::endl;

    for (auto e : v) // 基于范围的 for 循环, 同样是自动类型
        std::cout << e << std::endl;
}
```

## 浅层复制与深层复制

+ 浅层复制; 实现对象间数据元素的一一对应复制.
    复制对象需要复制构造函数, 默认复制构造函数实现的是数据成员一一对应的复制, 被称为浅层复制.
+ `深层复制`; 当被复制的对象数据成员是指针类型时, 不是复制该指针成员本身, 而是将指针对象进行复制.
    如果类的数据成员中有指针, 而指针指向动态分配的内存.

```cpp
#include <iostream>
#include <cassert>
using namespace std;

class Point
{
public:
    Point() : x(0), y(0)
    {
        cout << "Default Constructor called" << endl;
    }
    Point(int x, int y) : x(x), y(y)
    {
        cout << "Constructor called." << endl;
    }
    ~Point() { cout << "Destructor called." << endl; }
    int getX() const { return x; }
    int getY() const { return y; }
    void move(int newX, int newY)
    {
        x = newX;
        y = newY;
    }

private:
    int x, y;
};

class ArrayOfPoints
{ // 动态数组类
public:
    ArrayOfPoints(int size) : size(size)
    {
        points = new Point[size]; // 在构造函数中分配内存
    }
    ~ArrayOfPoints()
    {
        cout << "Deleting ..." << endl;
        delete[] points; //在析构函数中, 用 delete 释放内存
    }
    Point &element(int index) // 返回值为引用类型, 我们访问数组元素, 希望它是左值, 也就是 = 左边的值.
    {
        assert(index >= 0 && index < size);
        return points[index];
    }

private:
    Point *points; //指向动态数组首地址.
    int size;      //数组大小
};

int main()
{
    int count;
    cout << "Please enter the count of points: ";
    cin >> count;
    ArrayOfPoints pointsArray1(count); //创建对象数组
    pointsArray1.element(0).move(5, 10);
    pointsArray1.element(1).move(15, 20);

    ArrayOfPoints pointsArray2(pointsArray1); //创建副本, 这里使用默认的复制构造函数

    cout << "Copy of pointsArray1:" << endl;
    cout << "Point_0 of array2: " << pointsArray2.element(0).getX() << ", " << pointsArray2.element(0).getY() << endl;
    cout << "Point_1 of array2: " << pointsArray2.element(1).getX() << ", " << pointsArray2.element(1).getY() << endl;
    pointsArray1.element(0).move(25, 30);
    pointsArray1.element(1).move(35, 40);

    cout << "After the moving of pointsArray1:" << endl;
    cout << "Point_0 of array2: " << pointsArray2.element(0).getX() << ", " << pointsArray2.element(0).getY() << endl;
    cout << "Point_1 of array2: " << pointsArray2.element(1).getX() << ", " << pointsArray2.element(1).getY() << endl;
    return 0;
}
```

默认的拷贝复制函数, 将 `pointsArray1` 的栈上内容,即地址,原封不动地复制到 `pointsArray2`,
数组的内容没有复制, 析构的时候, 会对相同的内存地址释放两次, 所以会出错.

```cpp
#include <iostream>
#include <cassert>
using namespace std;

class Point
{
public:
    Point() : x(0), y(0)
    {
        cout << "Default Constructor called" << endl;
    }
    Point(int x, int y) : x(x), y(y)
    {
        cout << "Constructor called." << endl;
    }
    ~Point() { cout << "Destructor called." << endl; }
    int getX() const { return x; }
    int getY() const { return y; }
    void move(int newX, int newY)
    {
        x = newX;
        y = newY;
    }

private:
    int x, y;
};

class ArrayOfPoints
{ // 动态数组类
public:
    ArrayOfPoints(const ArrayOfPoints &pointsArray); //拷贝构造声明
    ArrayOfPoints(int size) : size(size)
    {
        points = new Point[size]; // 在构造函数中分配内存
    }
    ~ArrayOfPoints()
    {
        cout << "Deleting ..." << endl;
        delete[] points; //在析构函数中, 用 delete 释放内存
    }
    Point &element(int index) // 返回值为引用类型, 我们访问数组元素, 希望它是左值, 也就是 = 左边的值.
    {
        assert(index >= 0 && index < size);
        return points[index];
    }

private:
    Point *points; //指向动态数组首地址.
    int size;      //数组大小
};

ArrayOfPoints::ArrayOfPoints(const ArrayOfPoints &v)
{
    size = v.size;
    points = new Point[size];
    for (int i = 0; i < size; i++)
        points[i] = v.points[i];
};

int main()
{
    int count;
    cout << "Please enter the count of points: ";
    cin >> count;
    ArrayOfPoints pointsArray1(count); //创建对象数组
    pointsArray1.element(0).move(5, 10);
    pointsArray1.element(1).move(15, 20);

    ArrayOfPoints pointsArray2(pointsArray1); //创建副本, 这里使用默认的复制构造函数

    cout << "Copy of pointsArray1:" << endl;
    cout << "Point_0 of array2: " << pointsArray2.element(0).getX() << ", " << pointsArray2.element(0).getY() << endl;
    cout << "Point_1 of array2: " << pointsArray2.element(1).getX() << ", " << pointsArray2.element(1).getY() << endl;
    pointsArray1.element(0).move(25, 30);
    pointsArray1.element(1).move(35, 40);

    cout << "After the moving of pointsArray1:" << endl;
    cout << "Point_0 of array2: " << pointsArray2.element(0).getX() << ", " << pointsArray2.element(0).getY() << endl;
    cout << "Point_1 of array2: " << pointsArray2.element(1).getX() << ", " << pointsArray2.element(1).getY() << endl;
    return 0;
}
```

按照同样大小, 构造了一个数组, 通过 `for` 循环, 将`数组1`地内容逐字拷贝到`数组2`中.
这样就达到了深层复制地效果.

## 移动构造

`move` instead `copy`:

+ `移动构造` 是 `C++ 11` 标准中提供地一种新的构造方法.
+ `C++11` 之前, 如果要将源对象地状态转移到目标对象只能通过复制.
    在某些情况下, 我们没有必要复制对象-- 只需要移动它们.

+ `C++11` 引入移动语义: 将源对象资源地控制权全部交给目标对象.
+ 移动构造函数

当`临时对象`在被复制后, 就不再被利用了.
我们完全可以把`临时对象`的资源直接移动, 这样就避免了多余的复制操作.

什么时候触发移动构造? 有可被利用的`临时对象`. 移动构造函数:

    class_name(class_name &&)

例: 函数返回含有指针成员的对象

+ 版本一: 使用深层复制构造函数
    返回时构造临时对象, 动态分配将临时对象返回到主调函数, 然后删除临时对象.
+ 版本二: 使用移动构造函数
    将要返回的局部对象转移到主调函数, 省去了构造和删除临时对象的过程.

### 使用深层复制构造

```cpp
#include <iostream>
using namespace std;

class IntNum
{
public:
    IntNum(int x = 0) : xptr(new int(x))
    { //构造函数
        cout << "Calling constructor..." << endl;
    }
    IntNum(const IntNum &n) : xptr(new int(*n.xptr)) // * n. xptr 即 n 的指针 对应的值
    {                                                //复制构造函数
        cout << "Calling copy constructor..." << endl;
    }
    ~IntNum()
    { //析构函数
        delete xptr;
        cout << "Destrcuting... " << endl;
    }
    int getInt() { return *xptr; }

private:
    int *xptr;
};

// 返回值为 IntNum 类对象
IntNum getNum()
{
    IntNum a;
    return a;
}

int main()
{
    cout << getNum().getInt() << endl;
    return 0;
}
```

老版本 C++ 会构造临时无名对象, 发生拷贝.

### 使用移动构造

```cpp
#include <iostream>
using namespace std;

class IntNum
{
public:
    IntNum(int x = 0) : xptr(new int(x))
    { //构造函数
        cout << "Calling constructor..." << endl;
    }
    IntNum(const IntNum &n) : xptr(new int(*n.xptr)) // * n. xptr 即 n 的指针 对应的值
    {                                                //复制构造函数
        cout << "Calling copy constructor..." << endl;
    }
    IntNum(IntNum &&n) : xptr(n.xptr) // 把参数指针赋值给正在构造的指针
    {                                 // 移动构造函数
        n.xptr = nullptr;             //把参数 n 的指针置为空指针, 析构函数作用在上面不发生任何事情
        cout << "Calling move constructor ... " << endl;
    }
    ~IntNum()
    { //析构函数
        delete xptr;
        cout << "Destrcuting... " << endl;
    }
    int getInt() { return *xptr; }

private:
    int *xptr;
};

// 返回值为 IntNum 类对象
IntNum getNum()
{
    IntNum a;
    return a;
}

int main()
{
    cout << getNum().getInt() << endl;
    return 0;
}
```

>`&&` 是右值引用:
>即将消亡的值是右值, 函数返回的临时变量也是右值.

`IntNum &n` 可以绑定到左值, 而 `IntNum &&n` 可以绑定到即将消亡的对象, 也就是右值.

## C风格字符串

`C++` 中可以使用字符串常量, `"abc"`, 但基本数据类型中没有字符串变量.
在 `C++` 中, 推荐使用标准模板库中的 `string` 类.

### 字符串常量

+ 例: "program"
+ 各字符连续, 顺序存放, 每个字符占一个字节, 以 `\0` 结尾,相当于一个隐含创建的字符串常量数组.
+ "program" 出现在表达式中, 表示这一 char 数组的首地址
+ 首地址可以赋给 char 常量指针:

    const char *STRING1= "program";

+ 用字符数组存储字符串(C风格字符串)

例如:

```cpp
char str[8] ={'p', 'r', 'o', 'g', 'r', 'a', 'm', '\0'};
char str[8] ="program";
char str[] ="program";
```

+ 用字符数组表示字符串的缺点:
    + 执行连接, 拷贝, 比较等操作, 都需要显式调用库函数, 很麻烦
    + 当字符串长度很不确定时, 需要用 new 动态创建字符数组, 最后用 delete 释放, 很繁琐
    + 字符串实际长度大于为它分配的空间时, 会产生数组下标越界的错位.

## string 类

`string` 类是 C++ 标准库中封装起来的字符串, 相当于封装好的字符数组.
不用考虑长度越界, 下标越界.

### string 类常用的构造函数

+ string(); //默认构造函数, 建立一个长度为`0`的串, 例:

    string s1; // 它会根据需要延展.

+ string(const char *s); // 用指针 s 所指向的字符串常量初始化 string 对象. 例:

    string s2="abc";

+ string(const string & rhs); // 复制构造函数, 例:

    string s3=s2;

### string 类常用操作

+ `s+t` 将串 s 和 t 连接成一个新串
+ `s=t` 用 t 更新 s
+ `s ==t` 判断 s 与 t 是否相等
+ `s != t`
+ `s<t` 判断 s 是否小于等于 t (按字典顺序比较)
+ `s<=t`
+ `s>t`
+ `s>=t`
+ `s[i]` 访问串中下标为 `i` 的字符

例子:

```cpp
string s1= "abc", s2="def";
string s3=s1+s2; // 结果是 "abcdef"
bool s4=(s1<s2); // 结果是 true
char s5=s2[1]; // 结果是'e'
```

### 输入整行字符

+ `getline` 可以输入整行字符串(要包含 string 头文件). 例如:

    getline(cin,s2);

+ 输入字符串时, 可以使用其它分隔符作为字符串结束的标志(例如逗号, 分号), 将分隔符作为 `getline` 的第 `3`参数即可. 例如:

    getline(cin, s2, ',' );

例 6-24 用 `getline` 输入字符串

```cpp
#include <string>
#include <iostream>
using namespace std;

int main()
{
    for (int i = 0; i < 2; i++)
    {
        string city, state;
        getline(cin, city, ',');
        getline(cin, state);
        cout << "City:" << city << " State:" << state << endl;
    }
    return 0;
}
```
