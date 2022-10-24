# C++__PRETTY_FUNCTION__和__FUNCSIG__宏简单使用

[C++__PRETTY_FUNCTION__和__FUNCSIG__宏简单使用](https://zhuanlan.zhihu.com/p/466735374)

这里给出例子.

+ `CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.15)

project ( testprj )

set ( PRJ_COMPILE_FEATURES )
set ( PRJ_COMPILE_DEFINITIONS )

message ( STATUS "MSVC = ${MSVC}" )

if ( MSVC )
    list ( APPEND PRJ_COMPILE_DEFINITIONS USE_MSVC )
endif()

list ( APPEND PRJ_COMPILE_FEATURES cxx_std_20 )

add_executable( ${PROJECT_NAME}
    main.cpp
)

target_compile_features ( ${PROJECT_NAME}
    PRIVATE
        ${PRJ_COMPILE_FEATURES}
)

target_compile_definitions ( ${PROJECT_NAME}
    PRIVATE
        ${PRJ_COMPILE_DEFINITIONS}
)
```

+ C++代码文件, `main.cpp`

```cpp
#include <iostream>

#ifdef USE_MSVC
    #define FUNCTION_DETAIL __FUNCSIG__
#else
    #define FUNCTION_DETAIL __PRETTY_FUNCTION__
#endif

void print()
{
    std::cout << FUNCTION_DETAIL << ": " << std::endl;
}

template<typename T, typename... Args>
void print(T t, Args... args)
{
    std::cout << FUNCTION_DETAIL << ": " << t << std::endl;
    print(args...);
}

int main()
{
    print( 1, 2.5, 'a', 2022, "hello!");
    return 0;
}
```

+ windows11+powershell, `./testprj` 输出

```powershell
.\Debug\testprj.exe

PS D:\work\modern_cmake_work\ModernCMake\codes\moderncpp\variadic\variadic01\build> .\Debug\testprj.exe
void __cdecl print<int,double,char,int,const char*>(int,double,char,int,const char *): 1
void __cdecl print<double,char,int,const char*>(double,char,int,const char *): 2.5
void __cdecl print<char,int,const char*>(char,int,const char *): a
void __cdecl print<int,const char*>(int,const char *): 2022
void __cdecl print<const char*,>(const char *): hello!
void __cdecl print(void):
```

+ 在ubuntu21.10下: `./testprj` 输出

```bash
eric@ubuntu:~/work/cmake_work/study/variadic/variadic01/build$ ./testprj
void print(T, Args ...) [with T = int; Args = {double, char, int, const char*}]: 1
void print(T, Args ...) [with T = double; Args = {char, int, const char*}]: 2.5
void print(T, Args ...) [with T = char; Args = {int, const char*}]: a
void print(T, Args ...) [with T = int; Args = {const char*}]: 2022
void print(T, Args ...) [with T = const char*; Args = {}]: hello!
void print():
```
