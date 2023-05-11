# c++函数调用约定

[C/C++ 函数调用约定(__cdecl, __stdcall, __fastcall)](https://blog.csdn.net/hellokandy/article/details/54603055)
[调用约定__cdecl, __stdcall和__fastcall](https://blog.csdn.net/luoweifu/article/details/52425733)

调用函数时, 计算机常用栈来存放函数执行需要的参数,
由于栈的空间大小是有限的,
在windows下, 栈是向低地址扩展的数据结构, 是一块连续的内存区域.
这句话的意思是栈顶的地址和栈的最大容量是系统预先规定好的,
如果申请的空间超过栈的剩余空间时, 将提示overflow.

在函数调用时, 第一个进栈的是主函数中后的下一条指令(函数调用语句的下一条可执行语句)的地址,
然后是函数的各个参数, 在大多数的C编译器中, 参数是由右往左入栈的, 然后是函数中的局部变量.
注意静态变量是不入栈的!

VC中默认调用是 __cdecl 方式, Windows API 使用 __stdcall 调用方式,
在 DLL 导出函数中, 为了跟 Windows API 保持一致, 建议使用 `__stdcall` 方式.

C/C++ 函数调用约定, 主要是对以下两个方面进行了约定:

1. 当参数个数多于一个时, 按照什么顺序把参数压入堆栈. -- 调用函数时, 参数的入栈顺序.
2. 函数调用后, 由谁来把堆栈恢复原状. ---调用结束后, 由谁(调用者还是被调用者)负责将参数出栈.

在高级语言中, 就是通过函数的 `调用方式`(call convention) 来说明这两个问题的.

## 常见的调用方式有

C 语言:  __cdecl, __stdcall, __fastcall, naked, __pascal.

C++ 语言:  __cdecl, __stdcall, __fastcall, naked,
__pascal, __thiscall, 比 C 语言多出一种 `__thiscall` 调用方式.

下面就分别介绍这几种调用方式:

### stdcall

`__stdcall` 是 StandardCall 的缩写, 是C++的标准调用方式.
stdcall 调用方式又被称为 Pascal 调用方式.
在Microsoft C++系列的C/C++编译器中,
使用 PASCAL 宏, WINAPI 宏和 CALLBACK 宏来指定函数的调用方式为 stdcall.

其声明语法为:

```cpp
int _stdcall function(int a, int b);
```

`stdcall` 的调用方式意味着:

+ 参数从右向左依次压入堆栈.
+ 由被调用函数自己来恢复堆栈, 称为自动清栈.
+ 函数名自动加前导下划线, 后面紧跟着一个`@`, 其后紧跟着参数的大小.

### cdecl

`__cdecl` 是C Declaration的缩写(declaration, 声明),
cdecl调用方式又称为C调用方式, 是C语言缺省的调用方式.

其声明语法为:

```c
int function(int a, int b) // 不加修饰符就是C调用方式
int _cdecl function(int a, int b) // 明确指定用C调用方式
```

`cdecl` 的调用方式意味着:

+ 参数从右向左依次压入堆栈.
+ 由调用者恢复堆栈, 称为手动清栈.
+ 函数名自动加前导下划线.

由于是由调用者来恢复堆栈, 因此C调用方式允许函数的参数个数是不固定的, 这是C语言的一大特色.

### fastcall

`fastcall` 按照名字上理解就可以知道, 它是一种快速调用方式, 因为它通过 CPU 寄存器来传递参数.
此方式的函数的第一个和第二个DWORD参数通过ecx和edx传递, 后面的参数从右向左的顺序压入栈.
被调用函数清理堆栈.

其声明语法为:

```cpp
int fastcall function(int a, int b);
```

### thiscall

thiscall 调用方式是唯一一种不能 `显式指定` 的修饰符.
它是C++类成员函数缺省的调用方式.
由于成员函数调用还有一个this指针, 因此必须用这种特殊的调用方式.

thiscall 调用方式意味着:

+ 参数从右向左压入栈.
+ 如果参数个数确定, this指针通过ecx传递给被调用者;
如果参数个数不确定, this指针在所有参数压入栈后被压入栈.

参数个数不定的, 由调用者清理堆栈, 否则由函数自己清理堆栈.
可以看到, 对于参数个数固定的情况, 它类似于stdcall, 不定时则类似于cdecl.

### naked call

是一种比较少见的调用方式, 一般高级程序设计语言中不常见.
函数的声明调用方式和实际调用方式必须一致, 否则编译器会产生混乱.
