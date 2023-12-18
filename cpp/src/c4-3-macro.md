# c c++ 宏

[#if defined 宏定义常用"与", "或", "非"判断](https://zhuanlan.zhihu.com/p/267410514)
[完美解析C/C++条件预处理编译: #define, #if , #ifdef, #elif, #endif](https://blog.csdn.net/weixin_41194129/article/details/108260439)

`#if`, `#ifdef` 语法

```c
#if 表达式
    #ifdef 标识符
        #ifndef 标识符
    #elif 表达式
    #else
#endif
```

`#ifdef`, `#ifndef` 检查标识符是否被定义为宏名, 不检查宏的值
`#if`, `#elif` 后面是常量表达式, 检查宏的值

## 宏定义限制 `#ifdef`

[#ifdef inside #define](https://stackoverflow.com/questions/5586429/ifdef-inside-define)

如下的宏定义非法

```c
#define COV_ON(x) \
    #ifdef COVERAGE_TOOL \
        _Pragma (COVERAGE #x)
    #endif
```

不能在 `#define` 中使用 `#ifdef`.
(`#` 不是 `#define` 中允许的字符).
那么有什么解决办法吗?
只能通过:

```c
#ifdef COVERAGE_TOOL
#define COV_ON(x) _Pragma (COVERAGE #x)
#else
#define COV_ON(x)
#endif
```

## 布尔逻辑

1. `与`判断 (即判断多个宏是否同时定义)

```c
#if defined(WIN32) && !defined(__LWIP_OPT_H__) && !defined(LWIP_HDR_OPT_H)
#endif /* curl_socket_typedef */
```

2. `或` 判断(即判断多个宏是否有其中一个定义)

```c
#if defined(_AIX) || defined(__NOVELL_LIBC__) || defined(__NetBSD__) || \
    defined(__minix) || defined(__SYMBIAN32__) || defined(__INTEGRITY) || \
    defined(ANDROID) || defined(__ANDROID__) || defined(__OpenBSD__) || \
   (defined(__FreeBSD_version) && (__FreeBSD_version < 800000))
#include <sys/select.h>
#endif
```

3. `非`判断(即判断多个宏是否同时都没有定义)

```c
#if !defined(WIN32) && !defined(__WATCOMC__) && !defined(__VXWORKS__)
#include <sys/time.h>
#endif
```

## gcc 暂时关闭编译器警告

[如何优雅的屏蔽GCC编译器警告](https://www.jianshu.com/p/9939dc4a44fb)

作为一个合格的C语言程序员,
在编译代码的时候一定要加上`-W`和 `-Wall`选项,
要保证代码中没有任何的warning信息, 提高代码的可靠性.
`-Wall` 选项意思是编译后显示所有警告, `-W` 选项只显示编译器认为会出现错误的警告.

但是有些时候, 有些功能模块尚待开发, 有些函数也没有完成功能,
虽然不影响程序的正常执行, 但是待开发的代码难免会有warning输出.
屏蔽 warning 的编译器指令如下

```c
#pragma GCC diagnostic ignored "警告类型"
```

如果只想暂时屏蔽(局部范围生效)
则使用 `push` 和 `pop`, 完整代码如下:

```c
#include <stdio.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wformat="
#pragma GCC diagnostic ignored "-Wreturn-type"

void print_message(char *str, int num) {
  printf("%s : %d\n", str, &num);
  return;
}

int main(int argc, char *argv[]) {
  int i, j, k, t;
  t = 0;
  for (i = 0; i < 3; i++) {
    print_message("J value is", j++);
  }
}

#pragma GCC diagnostic pop
```

## 例子和递归宏

[C语言宏定义使用总结与递归宏](https://www.cnblogs.com/CodeWorkerLiMing/p/12750890.html)

## GCC, CLANG 宏

[clang gcc 中__clang__ __GNUC__宏定义区别](https://blog.csdn.net/wandersky0822/article/details/119867367)
[3.7.2 Common Predefined Macros](https://gcc.gnu.org/onlinedocs/gcc-5.1.0/cpp/Common-Predefined-Macros.html)

`GCC`定义的宏包括:

+ `__GNUC__`
+ `__GNUC_MINOR__`
+ `__GNUC_PATCHLEVEL__`
+ `__GNUG__`

`Clang`除了支持GCC定义的宏之外还定义了:

+ `__clang__`
+ `__clang_major__`
+ `__clang_minor__`
