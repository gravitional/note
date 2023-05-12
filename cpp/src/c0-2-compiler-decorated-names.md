# c++ 编译器 修饰名

[其他 MSVC 生成工具](https://learn.microsoft.com/zh-cn/cpp/cpp/cdecl?view=msvc-170)
[Decorated names](https://learn.microsoft.com/en-us/cpp/build/reference/decorated-names?view=msvc-170)

C和C++程序中的 `函数`, `数据` 和 `对象` 在内部是由它们的 `修饰名`(decorated names) 表示的.
`修饰名` 是编译器在编译一个对象, 数据或函数定义时创建的一个编码字符串.

它将 `调用约定`(calling conventions), `类型`, `函数参数` 和其他信息与名称一起记录下来.
这种名称修饰, 也被称为名称混杂(name mangling), 帮助链接器在链接可执行文件时找到正确的函数和对象.

修饰的命名约定在Visual Studio的不同版本中都有变化, 而且在不同的目标架构上也可能是不同的.
为了与使用Visual Studio创建的源文件正确链接,
C和C++ DLLs和库应该使用相同的编译器工具集, 标志和目标架构来编译.

## C++ 修饰名的格式

C++函数的修饰名称包含以下信息:

+ 函数的名称.

+ 如果函数是一个成员, 那么该函数是 成员函数.
修饰可以包括 外套类的父类, 以此类推.

+ 如果是名字空间的一部分, 函数所属的名字空间.
+ 函数参数的类型.
+ 调用约定.
+ 该函数的返回类型.
+ 一个可选的 `target-specific` 元素.
在ARM64EC对象中, `$$h`标签被插入到名称中.

函数和类的名称 被编码在 修饰后的名称中.
`修饰名` 的其余部分 仅对编译器和链接器 有内部意义.
下面是未修饰和修饰过的C++名称的例子.

未修饰名   修饰名

int a(char){int i=3;return i;}; ?a@@YAHD@Z
void __stdcall b::c(float){}; ?c@b@@AAGXM@Z

## C语言修饰名的格式

[__cdecl](https://learn.microsoft.com/zh-cn/cpp/cpp/cdecl?view=msvc-170)

C函数的修饰形式取决于其声明中使用的调用惯例, 如下表所示.
这也是C++代码被声明为具有 `extern "C"` 链接时使用的修饰格式.
默认的调用约定是 `__cdecl`.
在64位环境中, `C` 或 `extern "C"` 函数只有在使用 `__vectorcall` 调用约定时才能被修饰.

调用约定 修饰

+ `__cdecl`; 前导下划线 (_)
+ `__stdcall`; 前面的下划线(_)和后面的at符号(@), 后面是参数列表中十进制的字节数.
+ `__fastcall`; 前导和尾随的 at符号(@), 后面是代表参数列表中字节数的十进制数字
+ `__vectorcall`; 两个尾随的符号 (`@@`) 后面是参数列表中的十进制字节数

对于具有C链接的ARM64EC函数
(无论是作为C编译还是通过使用 `extern "C"` 编译), 在修饰的名称前加一个`#`.
