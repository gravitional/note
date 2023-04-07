# pthread debug

## Windows使用pthread错误解决方法

[Windows下C++使用pthread错误解决方法](https://blog.csdn.net/qq_33194301/article/details/104879626)

## -lpthread和-pthread的区别

[gcc编译选项-lpthread和-pthread的区别](https://zhuanlan.zhihu.com/p/89678862)

编译命令改为:

```bash
g++ -o main main.cpp -pthread
```

程序正常执行, 区别就是将-lpthread改为-pthread.

lpthread和pthread的区别

`-lpthread` 是较为老式的解决方法, pthread新加了对于宏D_REENTRANT的定义,
`-pthread` 会被展开为 `-D_REENTRANT -lpthread`, 它不仅可以链接pthread库,
还可以打开系统头文件中的各种多线程支持分支, 比如, 我们常常使用的错误码标志errno,
如果没有定义_REENTRANT, 则实现为一个全局变量;
若是定义了_REENTRANT, 则会实现为每线程独有, 从而避免线程竞争错误.

`-pthread` 可移植性较强: 在Linux中, pthread是作为一个单独的库存在的(libpthread.so),
但是在其他Unix变种中却不一定, 比如在FreeBSD中是没有单独的pthread库的,
因此在FreeBSD中不能使用-lpthread来链接pthread, 而使用-pthread则不会存在这个问题,
因为FreeBSD的编译器能正确将-pthread展开为该系统下的依赖参数.

同样道理, 其他不同的变种也会有这样那样的区别,
如果使用-lpthread, 则可能在移植到其他Unix变种中时会出现问题,
为了保持较高的可移植性, 我们最好还是使用 `-pthread`
(尽管这种做法未被接纳成为C标准, 但已基本是事实标准).
因此建议以后都使用-pthread选项来编译.

## 静态链接pthread库

[C/C++静态链接pthread库的坑[-static -pthread]](https://blog.csdn.net/Kajima_/article/details/111415651)
