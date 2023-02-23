# C++ extern 关键字

[C++中extern关键字用法小结](https://www.cnblogs.com/broglie/p/5524932.html)
[C++ extern](https://zhuanlan.zhihu.com/p/267130913)
[C/C++中extern关键字详解](https://www.jianshu.com/p/111dcd1c0201)

## extern修饰变量和函数

在C语言中, 修饰符extern用在变量或者函数的声明前,
用来说明"此变量/函数是在别处定义的, 要在此处引用". extern声明不是定义, 即不分配存储空间.

先来看一段代码

```cpp
/*  basic_stdy.h */

#ifndef_BASIC_STDY_H_
#define_BASIC_STDY_H_

//extern int a;                                  //在头文件中声明,必须加上extern, 否则就是重定义
//void fun();                                    //不用加extern也可以

#endif
/*  basic_stdy.cpp */
#include"basic_stdy.h"
#include<iostream>
using namespace std;

int a(2);

void fun(){
            cout << a <<endl;
}
/*main.cpp*/
#include<iostream>
#include "basic_stdy.h"
using namespace std;

extern int a;                                                 //ok不用包含头文件,  变量只要声明即可
extern void fun();                                            //ok不用包含头文件,  函数只要声明即可

int main(int argc,char **argv){

            cout << a << endl;
            fun();

            system("pause");
            return 0;
}
```

也就是说, 在一个文件中定义了变量和函数,  在其他文件中要使用它们,  可以有两种方式:

+ 使用头文件, 然后声明它们, 然后其他文件去包含头文件
+ 在其他文件中直接extern

## extern"C" 作用

C++语言在编译的时候为了解决函数的多态问题,
会将函数名和参数联合起来生成一个中间的函数名称, 而C语言则不会,
因此会造成链接时无法找到对应函数的情况, 此时C函数就需要用extern "C"进行链接指定,
这告诉编译器, 请保持我的名称, 不要给我生成用于链接的中间函数名.

比如说你用C 开发了一个DLL 库, 为了能够让C ++语言也能够调用你的DLL 输出(Export) 的函数,
你需要用extern "C" 来强制编译器不要修改你的函数名.

通常, 在C 语言的头文件中经常可以看到类似下面这种形式的代码:

```cpp
#ifdef __cplusplus
extern "C" {
#endif

/**** some declaration or so *****/

#ifdef __cplusplus
}
#endif
```

现在要写一个c语言的模块, 供以后使用(以后的项目可能是c的也可能是c++的),
源文件事先编译好, 编译成.so或.o都无所谓. 头文件中声明函数时要用条件编译包含起来, 如下:

```cpp
#ifdef __cpluscplus
extern "C" {
#endif

//some code

#ifdef __cplusplus
}
#endif
```

也就是把所有函数声明放在some code的位置.

如果这个模块已经存在了, 可能是公司里的前辈写的, 反正就是已经存在了,
模块的.h文件中没有extern "C"关键字, 这个模块又不希望被改动的情况下,
可以这样, 在你的c++文件中, 包含该模块的头文件时加上extern "C", 如下:

```cpp
extern "C" {
#include "test_extern_c.h"
}
```

上面例子中, 如果仅仅使用模块中的1个函数, 而不需要include整个模块时,
可以不include头文件, 而单独声明该函数, 像这样:

```cpp
extern "C"{
int ThisIsTest(int, int);
}
```

注意: 当单独声明函数时候,  就不能要头文件,
或者在头文件中不能写extern intThisIsTest(int a, int b);
否则会有error C2732: 链接规范与"ThisIsTest"的早期规范冲突, 这个错误.

## 声明和定义知识点

定义也是声明, extern声明不是定义, 即不分配存储空间.
extern告诉编译器变量在其他地方定义了. eg:

```cpp
extern int i; //声明, 不是定义
int i; //声明, 也是定义
```

如果声明有初始化式, 就被当作定义, 即使前面加了extern.
只有当extern声明位于函数外部时, 才可以被初始化. eg:

```cpp
extern double pi=3.1416; //定义
```

函数的声明和定义区别比较简单, 带有{}的就是定义, 否则就是声明.eg:

```cpp
extern double max(double d1,double d2); //声明
double max(double d1,double d2){} //定义
```

除非有extern关键字, 否则都是变量的定义.eg:

```cpp
extern inti; //声明
inti; //定义
```

>注: basic_stdy.h中有char
>glob_str[];而basic_stdy.cpp有char
>glob_str;此时头文件中就不是定义, 默认为extern

## 程序设计风格:

+ 不要把变量定义放入.h文件, 这样容易导致重复定义错误.
+ 尽量使用static关键字把变量定义限制于该源文件作用域, 除非变量被设计成全局的.
+ 也就是说, 可以在头文件中声明一个变量, 在用的时候包含这个头文件就声明了这个变量.
