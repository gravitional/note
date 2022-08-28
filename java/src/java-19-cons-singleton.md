# 单例

[单例](https://www.liaoxuefeng.com/wiki/1252599548343744/1281319214514210)

>保证一个类仅有一个实例, 并提供一个访问它的全局访问点.

单例模式(Singleton)的目的是为了保证在一个进程中, 某个类有且仅有一个实例.

因为这个类只有一个实例, 因此, 自然不能让调用方使用new Xyz()来创建实例了. 所以, 单例的构造
方法必须是private, 这样就防止了调用方自己创建实例, 但是在类的内部, 是可以用一个静态字段来
引用唯一创建的实例的:

public class Singleton { // 静态字段引用唯一实例: private static final Singleton INSTANCE
    = new Singleton();

    // private构造方法保证外部无法实例化:
    private Singleton() {
    }
}

那么问题来了, 外部调用方如何获得这个唯一实例?

答案是提供一个静态方法, 直接返回实例:

public class Singleton { // 静态字段引用唯一实例: private static final Singleton INSTANCE
    = new Singleton();

    // 通过静态方法返回实例:
    public static Singleton getInstance() {
        return INSTANCE;
    }

    // private构造方法保证外部无法实例化:
    private Singleton() {
    }
}

或者直接把static变量暴露给外部:

public class Singleton { // 静态字段引用唯一实例: public static final Singleton INSTANCE =
    new Singleton();

    // private构造方法保证外部无法实例化:
    private Singleton() {
    }
}

所以, 单例模式的实现方式很简单:

    只有private构造方法, 确保外部无法实例化;
    通过private static变量持有唯一实例, 保证全局唯一性;
    通过public static方法返回此唯一实例, 使外部调用方能获取到实例.

Java标准库有一些类就是单例, 例如Runtime这个类:

Runtime runtime = Runtime.getRuntime();

有些童鞋可能听说过延迟加载, 即在调用方第一次调用getInstance()时才初始化全局唯一实例, 类似
这样:

public class Singleton { private static Singleton INSTANCE = null;

    public static Singleton getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new Singleton();
        }
        return INSTANCE;
    }

    private Singleton() {
    }
}

遗憾的是, 这种写法在多线程中是错误的, 在竞争条件下会创建出多个实例. 必须对整个方法进行加
锁:

public synchronized static Singleton getInstance() { if (INSTANCE == null) { INSTANCE =
    new Singleton(); } return INSTANCE; }

但加锁会严重影响并发性能. 还有些童鞋听说过双重检查, 类似这样:

public static Singleton getInstance() { if (INSTANCE == null) { synchronized
    (Singleton.class) { if (INSTANCE == null) { INSTANCE = new Singleton(); } } } return
        INSTANCE; }

然而, 由于Java的内存模型, 双重检查在这里不成立. 要真正实现延迟加载, 只能通过Java的
ClassLoader机制完成. 如果没有特殊的需求, 使用Singleton模式的时候, 最好不要延迟加载, 这样会
使代码更简单.

另一种实现Singleton的方式是利用Java的enum, 因为Java保证枚举类的每个枚举都是单例, 所以我们
只需要编写一个只有一个枚举的类即可:

public enum World { // 唯一枚举: INSTANCE;

	private String name = "world";

	public String getName() { return this.name; }

	public void setName(String name) { this.name = name; } }

枚举类也完全可以像其他类那样定义自己的字段, 方法, 这样上面这个World类在调用方看来就可以这
么用:

String name = World.INSTANCE.getName();

使用枚举实现Singleton还避免了第一种方式实现Singleton的一个潜在问题: 即序列化和反序列化会绕
过普通类的private构造方法从而创建出多个实例, 而枚举类就没有这个问题.

那我们什么时候应该用Singleton呢?实际上, 很多程序, 尤其是Web程序, 大部分服务类都应该被视作
Singleton, 如果全部按Singleton的写法写, 会非常麻烦, 所以, 通常是通过约定让框架(例如
Spring)来实例化这些类, 保证只有一个实例, 调用方自觉通过框架获取实例而不是new操作符:

@Component // 表示一个单例组件public class MyService { ... }

因此, 除非确有必要, 否则Singleton模式一般以"约定"为主, 不会刻意实现它. 练习

使用两种Singleton模式实现单例:

从下载练习: Singleton练习 (推荐使用IDE练习插件快速下载)小结

Singleton模式是为了保证一个程序的运行期间, 某个类有且只有一个全局唯一实例;

Singleton模式既可以严格实现, 也可以以约定的方式把普通类视作单例.
