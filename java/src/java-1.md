# java-1.md

[java教程-廖雪峰](https://www.liaoxuefeng.com/wiki/1252599548343744/1255876875896416)

## java快速入门

本章的主要内容是快速掌握Java程序的基础知识, 了解并使用变量和各种数据类型, 介绍基本的程序流程控制语句. 

## Java简介

Java最早是由SUN公司(已被Oracle收购)的詹姆斯.高斯林(高司令, 人称Java之父)在上个世纪90年代初开发的一种编程语言, 最初被命名为Oak, 目标是针对小型家电设备的嵌入式应用, 结果市场没啥反响. 谁料到互联网的崛起, 让Oak重新焕发了生机, 于是SUN公司改造了Oak, 在1995年以Java的名称正式发布, 原因是Oak已经被人注册了, 因此SUN注册了Java这个商标. 随着互联网的高速发展, Java逐渐成为最重要的网络编程语言. 

Java介于编译型语言和解释型语言之间. 编译型语言如C, C++, 代码是直接编译成机器码执行, 但是不同的平台(x86, ARM等)CPU的指令集不同, 因此, 需要编译出每一种平台的对应机器码. 解释型语言如Python, Ruby没有这个问题, 可以由解释器直接加载源码然后运行, 代价是运行效率太低. 而Java是将代码编译成一种“字节码”, 它类似于抽象的CPU指令, 然后, 针对不同平台编写虚拟机, 不同平台的虚拟机负责加载字节码并执行, 这样就实现了“一次编写, 到处运行”的效果. 当然, 这是针对Java开发者而言. 对于虚拟机, 需要为每个平台分别开发. 为了保证不同平台, 不同公司开发的虚拟机都能正确执行Java字节码, SUN公司制定了一系列的Java虚拟机规范. 从实践的角度看, JVM的兼容性做得非常好, 低版本的Java字节码完全可以正常运行在高版本的JVM上. 

随着Java的发展, SUN给Java又分出了三个不同版本：

+ Java SE：Standard Edition
+ Java EE：Enterprise Edition
+ Java ME：Micro Edition

这三者之间有啥关系呢？

<pre style="font-size: 12px; line-height: 12px; border: medium none; white-space: pre; background-color: transparent;"><code class="language-ascii" style="font-family: &quot;Courier New&quot;, Consolas, monospace;">┌───────────────────────────┐
│Java EE                    │
│    ┌────────────────────┐ │
│    │Java SE             │ │
│    │    ┌─────────────┐ │ │
│    │    │   Java ME   │ │ │
│    │    └─────────────┘ │ │
│    └────────────────────┘ │
└───────────────────────────┘
</code></pre>

简单来说, Java SE就是标准版, 包含标准的JVM和标准库, 而Java EE是企业版, 它只是在Java SE的基础上加上了大量的API和库, 以便方便开发Web应用, 数据库, 消息服务等, Java EE的应用使用的虚拟机和Java SE完全相同. 

Java ME就和Java SE不同, 它是一个针对嵌入式设备的“瘦身版”, Java SE的标准库无法在Java ME上使用, Java ME的虚拟机也是“瘦身版”. 

毫无疑问, Java SE是整个Java平台的核心, 而Java EE是进一步学习Web应用所必须的. 我们熟悉的Spring等框架都是Java EE开源生态系统的一部分. 不幸的是, Java ME从来没有真正流行起来, 反而是Android开发成为了移动平台的标准之一, 因此, 没有特殊需求, 不建议学习Java ME. 

因此我们推荐的Java学习路线图如下：

+ 首先要学习Java SE, 掌握Java语言本身, Java核心开发技术以及Java标准库的使用；
+ 如果继续学习Java EE, 那么Spring框架, 数据库开发, 分布式架构就是需要学习的；
+ 如果要学习大数据开发, 那么Hadoop, Spark, Flink这些大数据平台就是需要学习的, 它们都基于Java或Scala开发；
+ 如果想要学习移动开发, 那么就深入Android平台, 掌握Android App开发. 

无论怎么选择, Java SE的核心技术是基础, 这个教程的目的就是让你完全精通Java SE！

### Java版本

从1995年发布1.0版本开始, 到目前为止, 最新的Java版本是Java 13

### 名词解释

初学者学Java, 经常听到JDK, JRE这些名词, 它们到底是啥？

    JDK：Java Development Kit
    JRE：Java Runtime Environment

简单地说, JRE就是运行Java字节码的虚拟机. 但是, 如果只有Java源码, 要编译成Java字节码, 就需要JDK, 因为JDK除了包含JRE, 还提供了编译器, 调试器等开发工具. 

二者关系如下：

<pre style="font-size: 12px; line-height: 12px; border: medium none; white-space: pre; background-color: transparent;"><code class="language-ascii" style="font-family: &quot;Courier New&quot;, Consolas, monospace;">  ┌─    ┌──────────────────────────────────┐
  │     │     Compiler, debugger, etc.     │
  │     └──────────────────────────────────┘
 JDK ┌─ ┌──────────────────────────────────┐
  │  │  │                                  │
  │ JRE │      JVM + Runtime Library       │
  │  │  │                                  │
  └─ └─ └──────────────────────────────────┘
        ┌───────┐┌───────┐┌───────┐┌───────┐
        │Windows││ Linux ││ macOS ││others │
        └───────┘└───────┘└───────┘└───────┘
</code></pre>

要学习Java开发, 当然需要安装JDK了. 

那JSR, JCP……又是啥？

+ JSR规范：Java Specification Request
+ JCP组织：Java Community Process

为了保证Java语言的规范性, SUN公司搞了一个JSR规范, 凡是想给Java平台加一个功能, 比如说访问数据库的功能, 大家要先创建一个JSR规范, 定义好接口, 这样, 各个数据库厂商都按照规范写出Java驱动程序, 开发者就不用担心自己写的数据库代码在MySQL上能跑, 却不能跑在PostgreSQL上. 

所以JSR是一系列的规范, 从JVM的内存模型到Web程序接口, 全部都标准化了. 而负责审核JSR的组织就是JCP. 

一个JSR规范发布时, 为了让大家有个参考, 还要同时发布一个“参考实现”, 以及一个“兼容性测试套件”：

+ RI：Reference Implementation
+ TCK：Technology Compatibility Kit

比如有人提议要搞一个基于Java开发的消息服务器, 这个提议很好啊, 但是光有提议还不行, 得贴出真正能跑的代码, 这就是RI. 如果有其他人也想开发这样一个消息服务器, 如何保证这些消息服务器对开发者来说接口, 功能都是相同的？所以还得提供TCK. 

通常来说, RI只是一个“能跑”的正确的代码, 它不追求速度, 所以, 如果真正要选择一个Java的消息服务器, 一般是没人用RI的, 大家都会选择一个有竞争力的商用或开源产品. 

参考：Java消息服务JMS的JSR：`https://jcp.org/en/jsr/detail?id=914`

### 安装JDK

因为Java程序必须运行在JVM之上, 所以, 我们第一件事情就是安装JDK. 
搜索JDK 13, 确保从[Oracle的官网][]下载最新的稳定版JDK：

[Oracle的官网]: https://www.oracle.com/java/technologies/javase-downloads.html

找到`Java SE 13.x`的下载链接, 下载安装即可. 
设置环境变量

安装完JDK后, 需要设置一个`JAVA_HOME`的环境变量, 它指向JDK的安装目录. 
在Windows下, 它是安装目录, 类似：

```java
C:\Program Files\Java\jdk-13
```

在Mac下, 它在`~/.bash_profile`里, 它是：

export JAVA_HOME=`/usr/libexec/java_home -v 13`

然后, 把`JAVA_HOME`的`bin`目录附加到系统环境变量PATH上. 在Windows下, 它长这样：

```java
Path=%JAVA_HOME%\bin;<现有的其他路径>
```

在Mac下, 它在`~/.bash_profile`里, 长这样：

```java
export PATH=$JAVA_HOME/bin:$PATH
```

把`JAVA_HOME`的`bin`目录添加到`PATH`中是为了在任意文件夹下都可以运行`java`. 
打开命令提示符窗口, 输入命令`java -version`

如果你看到的版本号不是`13`, 而是`12`, `1.8`之类, 说明系统存在多个JDK, 且默认JDK不是JDK 13, 需要把JDK 13提到PATH前面. 

细心的童鞋还可以在JAVA_HOME的bin目录下找到很多可执行文件：

+ `java`：这个可执行程序其实就是JVM, 运行Java程序, 就是启动JVM, 然后让JVM执行指定的编译后的代码；
+ `javac`：这是Java的编译器, 它用于把Java源码文件(以.java后缀结尾)编译为Java字节码文件(以.class后缀结尾)；
+ `jar`：用于把一组.class文件打包成一个.jar文件, 便于发布；
+ `javadoc`：用于从Java源码中自动提取注释并生成文档；
+ `jdb`：Java调试器, 用于开发阶段的运行调试. 

### 第一个Java程序

我们来编写第一个Java程序. 

打开文本编辑器, 输入以下代码：

```java
public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }
}
```

在一个Java程序中, 你总能找到一个类似：

```java
public class Hello {
    ...
}
```

的定义, 这个定义被称为`class`(类), 这里的类名是`Hello`, 大小写敏感, `class`用来定义一个类, `public`表示这个类是公开的, `public`, `class`都是Java的关键字, 必须小写, `Hello`是类的名字, 按照习惯, 首字母H要大写. 而花括号{}中间则是类的定义. 

注意到类的定义中, 我们定义了一个名为main的方法：

```java
    public static void main(String[] args) {
        ...
    }
```

方法是可执行的代码块, 一个方法除了方法名`main`, 还有用()括起来的方法参数, 这里的`main`方法有一个参数, 参数类型是`String[]`, 参数名是`args`, `public`, `static`用来修饰方法, 这里表示它是一个公开的静态方法, `void`是方法的返回类型, 而花括号`{}`中间的就是方法的代码. 

方法的代码每一行用;结束, 这里只有一行代码, 就是：

```java
        System.out.println("Hello, world!");
```

它用来打印一个字符串到屏幕上. 

`Java`规定, 某个类定义的`public static void main(String[] args)`是`Java`程序的固定入口方法, 因此, `Java`程序总是从`main`方法开始执行. 

注意到`Java`源码的缩进不是必须的, 但是用缩进后, 格式好看, 很容易看出代码块的开始和结束, 缩进一般是4个空格或者一个`tab`. 

最后, 当我们把代码保存为文件时, 文件名必须是`Hello.java`, 而且文件名也要注意大小写, 因为要和我们定义的类名`Hello`完全保持一致. 

### 如何运行Java程序

`Java`源码本质上是一个文本文件, 我们需要先用`javac`把`Hello.java`编译成字节码文件`Hello.class`, 然后, 用`java`命令执行这个字节码文件：

<pre style="font-size: 12px; line-height: 12px; border: medium none; white-space: pre; background-color: transparent;"><code class="language-ascii" style="font-family: &quot;Courier New&quot;, Consolas, monospace;">┌──────────────────┐
│    Hello.java    │&lt;─── source code
└──────────────────┘
          │ compile
          ▼
┌──────────────────┐
│   Hello.class    │&lt;─── byte code
└──────────────────┘
          │ execute
          ▼
┌──────────────────┐
│    Run on JVM    │
└──────────────────┘
</code></pre>

因此, 可执行文件`javac`是编译器, 而可执行文件`java`就是虚拟机. 

第一步, 在保存Hello.java的目录下执行命令`javac Hello.java`：

```java
$ javac Hello.java
```

如果源代码无误, 上述命令不会有任何输出, 而当前目录下会产生一个`Hello.class`文件：

```java
$ ls
Hello.class Hello.java
```

第二步, 执行`Hello.class`, 使用命令`java Hello`：

```java
$ java Hello
Hello, world!
```

注意：给虚拟机传递的参数`Hello`是我们定义的类名, 虚拟机自动查找对应的`class`文件并执行. 

有一些童鞋可能知道, 直接运行`java Hello.java`也是可以的：

```java
$ java Hello.java
Hello, world!
```

这是Java 11新增的一个功能, 它可以直接运行一个单文件源码！

需要注意的是, 在实际项目中, 单个不依赖第三方库的Java源码是非常罕见的, 所以, 绝大多数情况下, 我们无法直接运行一个Java源码文件, 原因是它需要依赖其他的库. 

#### 小结

+ 一个Java源码只能定义一个`public`类型的`class`, 
并且`class`名称和文件名要完全一致；
+ 使用`javac`可以将`.java`源码编译成`.class`字节码；
+ 使用`java`可以运行一个已编译的`Java`程序, 参数是类名. 

### 使用IDE

### 使用IDE联系插件

## java程序基础

本节我们将介绍Java程序的基础知识, 包括：

+ Java程序基本结构
+ 变量和数据类型
+ 整数运算
+ 浮点数运算
+ 布尔运算
+ 字符和字符串
+ 数组类型

### Java程序基本结构

我们先剖析一个完整的Java程序, 它的基本结构是什么：

```java
/**
 * 可以用来自动创建文档的注释
 */
public class Hello {
    public static void main(String[] args) {
        // 向屏幕输出文本:
        System.out.println("Hello, world!");
        /* 多行注释开始
        注释内容
        注释结束 */
    }
} // class定义结束
```

因为Java是面向对象的语言, 一个程序的基本单位就是`class`, `class`是关键字, 这里定义的`class`名字就是Hello：

```java
public class Hello { // 类名是Hello
    // ...
} // class定义结束
```

类名要求：

+ 类名必须以英文字母开头, 后接字母, 数字和下划线的组合
+ 习惯以大写字母开头

要注意遵守命名习惯, 好的类命名：

+ Hello
+ NoteBook
+ VRPlayer

不好的类命名：

+ hello
+ Good123
+ Note_Book
+ _World

注意到`public`是访问修饰符, 表示该`class`是公开的. 

不写`public`, 也能正确编译, 但是这个类将无法从命令行执行. 

在class内部, 可以定义若干方法(method)：

```java
public class Hello {
    public static void main(String[] args) { // 方法名是main
        // 方法代码...
    } // 方法定义结束
}
```

方法定义了一组执行语句, 方法内部的代码将会被依次顺序执行. 

这里的方法名是`main`, 返回值是`void`, 表示没有任何返回值. 

我们注意到`public`除了可以修饰`class`外, 也可以修饰方法. 而关键字`static`是另一个修饰符, 它表示静态方法, 后面我们会讲解方法的类型, 
目前, 我们只需要知道, Java入口程序规定的方法必须是静态方法, 方法名必须为`main`, 括号内的参数必须是`String`数组. 

方法名也有命名规则, 命名和`class`一样, 但是首字母小写：

好的方法命名：

+ main
+ goodMorning
+ playVR

不好的方法命名：

+ Main
+ good123
+ good_morning
+ _playVR

在方法内部, 语句才是真正的执行代码. Java的每一行语句必须以分号结束：

```java
public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, world!"); // 语句
    }
}
```

在Java程序中, 注释是一种给人阅读的文本, 不是程序的一部分, 所以编译器会自动忽略注释. 

Java有3种注释, 第一种是单行注释, 以双斜线开头, 直到这一行的结尾结束：

```java
// 这是注释...
```

而多行注释以`/*`星号开头, 以`*/`结束, 可以有多行：

```java
/*
这是注释
blablabla...
这也是注释
*/
```

还有一种特殊的多行注释, 以`/**`开头, 以`*/`结束, 如果有多行, 每行通常以星号开头：

```java
/**
 * 可以用来自动创建文档的注释
 * 
 * @auther liaoxuefeng
 */
public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }
}
```

这种特殊的多行注释需要写在类和方法的定义处, 可以用于自动创建文档. 

Java程序对格式没有明确的要求, 多几个空格或者回车不影响程序的正确性, 但是我们要养成良好的编程习惯, 注意遵守Java社区约定的编码格式. 

那约定的编码格式有哪些要求呢？其实我们在前面介绍的Eclipse IDE提供了快捷键`Ctrl+Shift+F`(macOS是`⌘+⇧+F`)帮助我们快速格式化代码的功能, Eclipse就是按照约定的编码格式对代码进行格式化的, 所以只需要看看格式化后的代码长啥样就行了. 具体的代码格式要求可以在Eclipse的设置中`Java-Code Style`查看. 

### 变量和数据类型

#### 变量

什么是变量？

变量就是初中数学的代数的概念, 例如一个简单的方程, `x`, `y`都是变量：

```java
y=x2+1y=x^2+1y=x2+1
```

在Java中, 变量分为两种：基本类型的变量和引用类型的变量. 

我们先讨论基本类型的变量. 

在Java中, 变量必须先定义后使用, 在定义变量的时候, 可以给它一个初始值. 例如：

```java
int x = 1;
```

上述语句定义了一个整型`int`类型的变量, 名称为`x`, 初始值为`1`. 

不写初始值, 就相当于给它指定了默认值. 默认值总是`0`. 

来看一个完整的定义变量, 然后打印变量值的例子：

```java
// 定义并打印变量
public class Main {
    public static void main(String[] args) {
        int x = 100; // 定义int类型变量x, 并赋予初始值100
        System.out.println(x); // 打印该变量的值
    }
}
```

变量的一个重要特点是可以重新赋值. 例如, 对变量`x`, 先赋值`100`, 再赋值`200`, 观察两次打印的结果：

```java
// 重新赋值变量
public class Main {
    public static void main(String[] args) {
        int x = 100; // 定义int类型变量x, 并赋予初始值100
        System.out.println(x); // 打印该变量的值, 观察是否为100
        x = 200; // 重新赋值为200
        System.out.println(x); // 打印该变量的值, 观察是否为200
    }
}
```

注意到第一次定义变量`x`的时候, 需要指定变量类型`int`, 因此使用语句`int x = 100;`. 
而第二次重新赋值的时候, 变量`x`已经存在了, 不能再重复定义, 因此不能指定变量类型`int`, 必须使用语句`x = 200;`. 

变量不但可以重新赋值, 还可以赋值给其他变量. 让我们来看一个例子：

```java
// 变量之间的赋值
public class Main {
    public static void main(String[] args) {
        int n = 100; // 定义变量n, 同时赋值为100
        System.out.println("n = " + n); // 打印n的值

        n = 200; // 变量n赋值为200
        System.out.println("n = " + n); // 打印n的值

        int x = n; // 变量x赋值为n(n的值为200, 因此赋值后x的值也是200)
        System.out.println("x = " + x); // 打印x的值

        x = x + 100; // 变量x赋值为x+100(x的值为200, 因此赋值后x的值是200+100=300)
        System.out.println("x = " + x); // 打印x的值
        System.out.println("n = " + n); // 再次打印n的值, n应该是200还是300？
   }
}
```

变量可以反复赋值. 注意, 等号`=`是赋值语句, 不是数学意义上的相等

#### 基本数据类型

基本数据类型是CPU可以直接进行运算的类型. Java定义了以下几种基本数据类型：

+ 整数类型：byte, short, int, long
+ 浮点数类型：float, double
+ 字符类型：char
+ 布尔类型：boolean

Java定义的这些基本数据类型有什么区别呢？要了解这些区别, 我们就必须简单了解一下计算机内存的基本结构. 

计算机内存的最小存储单元是字节(`byte`), 一个字节就是一个`8`位二进制数, 即8个`bit`. 
它的二进制表示范围从`00000000`~`11111111`, 换算成十进制是`0`~`255`, 换算成十六进制是`00`~`ff`. 

内存单元从`0`开始编号, 称为内存地址. 每个内存单元可以看作一间房间, 内存地址就是门牌号. 

一个字节是`1byte`, `1024`字节是`1K`, `1024K`是`1M`, `1024M`是`1G`, `1024G`是`1T`. 一个拥有`4T`内存的计算机的字节数量就是：

```java
4T = 4 x 1024G
   = 4 x 1024 x 1024M
   = 4 x 1024 x 1024 x 1024K
   = 4 x 1024 x 1024 x 1024 x 1024
   = 4398046511104
```

不同的数据类型占用的字节数不一样. 我们看一下Java基本数据类型占用的字节数：

<pre style="font-size: 12px; line-height: 12px; border: medium none; white-space: pre; background-color: transparent;"><code class="language-ascii" style="font-family: &quot;Courier New&quot;, Consolas, monospace;">       ┌───┐
  byte │   │
       └───┘
       ┌───┬───┐
 short │   │   │
       └───┴───┘
       ┌───┬───┬───┬───┐
   int │   │   │   │   │
       └───┴───┴───┴───┘
       ┌───┬───┬───┬───┬───┬───┬───┬───┐
  long │   │   │   │   │   │   │   │   │
       └───┴───┴───┴───┴───┴───┴───┴───┘
       ┌───┬───┬───┬───┐
 float │   │   │   │   │
       └───┴───┴───┴───┘
       ┌───┬───┬───┬───┬───┬───┬───┬───┐
double │   │   │   │   │   │   │   │   │
       └───┴───┴───┴───┴───┴───┴───┴───┘
       ┌───┬───┐
  char │   │   │
       └───┴───┘
</code></pre>

`byte`恰好就是一个字节, 而`long`和`double`需要`8`个字节. 

#### 整型

对于整型类型, Java只定义了带符号的整型, 因此, 最高位的`bit`表示符号位(`0`表示正数, `1`表示负数). 各种整型能表示的最大范围如下：

+ `byte`：-128 ~ 127
+ `short`: -32768 ~ 32767
+ `int`: -2147483648 ~ 2147483647
+ `long`: -9223372036854775808 ~ 9223372036854775807

我们来看定义整型的例子：

```java
// 定义整型
public class Main {
    public static void main(String[] args) {
        int i = 2147483647;
        int i2 = -2147483648;
        int i3 = 2_000_000_000; // 加下划线更容易识别
        int i4 = 0xff0000; // 十六进制表示的16711680
        int i5 = 0b1000000000; // 二进制表示的512
        long l = 9000000000000000000L; // long型的结尾需要加L
    }
}
```

特别注意：同一个数的不同进制的表示是完全相同的, 例如`15`=`0xf`＝`0b1111`. 

#### 浮点型

浮点类型的数就是小数, 因为小数用科学计数法表示的时候, 小数点是可以“浮动”的, 
如`1234.5`可以表示成`12.345x102`, 也可以表示成`1.2345x103`, 所以称为浮点数. 

下面是定义浮点数的例子：

```java
float f1 = 3.14f;
float f2 = 3.14e38f; // 科学计数法表示的3.14x10^38
double d = 1.79e308;
double d2 = -1.79e308;
double d3 = 4.9e-324; // 科学计数法表示的4.9x10^-324
```

对于`float`类型, 需要加上`f`后缀. 

浮点数可表示的范围非常大, `float`类型可最大表示`3.4*10^38`, 而`double`类型可最大表示`1.79*10^308`. 

#### 布尔类型

布尔类型`boolean`只有`true`和`false`两个值, 布尔类型总是关系运算的计算结果：

```java
boolean b1 = true;
boolean b2 = false;
boolean isGreater = 5 > 3; // 计算结果为true
int age = 12;
boolean isAdult = age >= 18; // 计算结果为false
```

Java语言对布尔类型的存储并没有做规定, 因为理论上存储布尔类型只需要`1 bit`, 但是通常`JVM`内部会把`boolean`表示为4字节整数. 

#### 字符类型

字符类型`char`表示一个字符. Java的`char`类型除了可表示标准的`ASCII`外, 
还可以表示一个`Unicode`字符：

```java
// 字符类型
public class Main {
    public static void main(String[] args) {
        char a = 'A';
        char zh = '中';
        System.out.println(a);
        System.out.println(zh);
    }
}
```

注意`char`类型使用单引号`'`, 且仅有一个字符, 要和双引号`"`的字符串类型区分开. 

##### 常量

定义变量的时候, 如果加上`final`修饰符, 这个变量就变成了常量：

```java
final double PI = 3.14; // PI是一个常量
double r = 5.0;
double area = PI * r * r;
PI = 300; // compile error!
```

常量在定义时进行初始化后就不可再次赋值, 再次赋值会导致编译错误. 

常量的作用是用有意义的变量名来避免魔术数字(Magic number), 例如, 不要在代码中到处写`3.14`, 而是定义一个常量. 如果将来需要提高计算精度, 我们只需要在常量的定义处修改, 例如, 改成`3.1416`, 而不必在所有地方替换`3.14`. 

根据习惯, 常量名通常全部大写. 

#### var关键字

有些时候, 类型的名字太长, 写起来比较麻烦. 例如：

```java
StringBuilder sb = new StringBuilder();
```

这个时候, 如果想省略变量类型, 可以使用`var`关键字：

```java
var sb = new StringBuilder();
```

编译器会根据赋值语句自动推断出变量`sb`的类型是`StringBuilder`. 对编译器来说, 语句：

```java
var sb = new StringBuilder();
```

实际上会自动变成：

```java
StringBuilder sb = new StringBuilder();
```

因此, 使用`var`定义变量, 仅仅是少写了变量类型而已. 

#### 变量的作用范围

在`Java`中, 多行语句用`{ }`括起来. 很多控制语句, 例如条件判断和循环, 都以`{ }`作为它们自身的范围, 例如：

```java
if (...) { // if开始
    ...
    while (...) { while 开始
        ...
        if (...) { // if开始
            ...
        } // if结束
        ...
    } // while结束
    ...
} // if结束
```

只要正确地嵌套这些`{ }`, 编译器就能识别出语句块的开始和结束. 而在语句块中定义的变量, 它有一个作用域, 就是从定义处开始, 到语句块结束. 超出了作用域引用这些变量, 编译器会报错. 举个例子：

```java
{
    ...
    int i = 0; // 变量i从这里开始定义
    ...
    {
        ...
        int x = 1; // 变量x从这里开始定义
        ...
        {
            ...
            String s = "hello"; // 变量s从这里开始定义
            ...
        } // 变量s作用域到此结束
        ...
        // 注意, 这是一个新的变量s, 它和上面的变量同名, 
        // 但是因为作用域不同, 它们是两个不同的变量:
        String s = "hi";
        ...
    } // 变量x和s作用域到此结束
    ...
} // 变量i作用域到此结束
```

定义变量时, 要遵循作用域最小化原则, 尽量将变量定义在尽可能小的作用域, 并且, 不要重复使用变量名. 

#### 小结

`Java`提供了两种变量类型：基本类型和引用类型

+ 基本类型包括整型, 浮点型, 布尔型, 字符型. 
+ 变量可重新赋值, 等号是赋值语句, 不是数学意义的等号. 
+ 常量在初始化后不可重新赋值, 使用常量便于理解程序意图. 

### 整数运算

Java的整数运算遵循四则运算规则, 可以使用任意嵌套的小括号. 
四则运算规则和初等数学一致. 例如：

```java
// 四则运算
public class Main {
    public static void main(String[] args) {
        int i = (100 + 200) * (99 - 88); // 3300
        int n = 7 * (5 + (i - 9)); // 23072
        System.out.println(i);
        System.out.println(n);
    }
}
```

整数的数值表示不但是精确的, 而且整数运算永远是精确的, 
即使是除法也是精确的, 因为两个整数相除只能得到结果的整数部分：

```java
int x = 12345 / 67; // 184
```

求余运算使用`%`：

```java
int y = 12345 % 67; // 12345÷67的余数是17
```

特别注意：整数的除法对于除数为`0`时运行时将报错, 但编译不会报错. 

#### 溢出

要特别注意, 整数由于存在范围限制, 如果计算结果超出了范围, 就会产生溢出, 
而溢出不会出错, 却会得到一个奇怪的结果：

```java
// 运算溢出
public class Main {
    public static void main(String[] args) {
        int x = 2147483640;
        int y = 15;
        int sum = x + y;
        System.out.println(sum); // -2147483641
    }
}
```

要解释上述结果, 我们把整数`2147483640`和`15`换成二进制做加法：

<pre style="font-size: 12px; line-height: 12px; border: medium none; white-space: pre; background-color: transparent;"><code class="language-ascii" style="font-family: &quot;Courier New&quot;, Consolas, monospace;">  0111 1111 1111 1111 1111 1111 1111 1000
+ 0000 0000 0000 0000 0000 0000 0000 1111
-----------------------------------------
  1000 0000 0000 0000 0000 0000 0000 0111
</code></pre>

由于最高位计算结果为`1`, 因此, 加法结果变成了一个负数. 

要解决上面的问题, 可以把`int`换成`long`类型, 由于`long`可表示的整型范围更大, 所以结果就不会溢出：

```java
long x = 2147483640;
long y = 15;
long sum = x + y;
System.out.println(sum); // 2147483655
```

还有一种简写的运算符, 即`+=`, `-=`, `*=`, `/=`, 它们的使用方法如下：

```java
n += 100; // 3409, 相当于 n = n + 100;
n -= 100; // 3309, 相当于 n = n - 100;
```

#### 自增/自减

Java还提供了`++`运算和`--`运算, 它们可以对一个整数进行`加1`和`减1`的操作：

```java
// 自增/自减运算
public class Main {
    public static void main(String[] args) {
        int n = 3300;
        n++; // 3301, 相当于 n = n + 1;
        n--; // 3300, 相当于 n = n - 1;
        int y = 100 + (++n); // 不要这么写
        System.out.println(y);
    }
}
```

注意`++`写在前面和后面计算结果是不同的, `++n`表示先`加1`再引用`n`, `n++`表示`先引用n再加1`. 不建议把`++`运算混入到常规运算中, 容易自己把自己搞懵了. 

#### 移位运算

在计算机中, 整数总是以二进制的形式表示. 例如, int类型的整数7使用4字节表示的二进制如下：

```java
00000000 0000000 0000000 00000111
```

可以对整数进行移位运算. 对整数`7`左移1位将得到整数`14`, 左移两位将得到整数`28`：

```java
int n = 7;       // 00000000 00000000 00000000 00000111 = 7
int a = n << 1;  // 00000000 00000000 00000000 00001110 = 14
int b = n << 2;  // 00000000 00000000 00000000 00011100 = 28
int c = n << 28; // 01110000 00000000 00000000 00000000 = 1879048192
int d = n << 29; // 11100000 00000000 00000000 00000000 = -536870912
```

左移`29`位时, 由于最高位变成`1`, 因此结果变成了负数. 

类似的, 对整数`28`进行右移, 结果如下：

```java
int n = 7;       // 00000000 00000000 00000000 00000111 = 7
int a = n >> 1;  // 00000000 00000000 00000000 00000011 = 3
int b = n >> 2;  // 00000000 00000000 00000000 00000001 = 1
int c = n >> 3;  // 00000000 00000000 00000000 00000000 = 0
```

如果对一个负数进行右移, 最高位的`1`不动, 结果仍然是一个负数：

```java
int n = -536870912;
int a = n >> 1;  // 11110000 00000000 00000000 00000000 = -268435456
int b = n >> 2;  // 10111000 00000000 00000000 00000000 = -134217728
int c = n >> 28; // 11111111 11111111 11111111 11111110 = -2
int d = n >> 29; // 11111111 11111111 11111111 11111111 = -1
```

还有一种不带符号的右移运算, 使用`>>>`, 它的特点是符号位跟着动, 
因此, 对一个负数进行`>>>`右移, 它会变成正数, 原因是最高位的`1`变成了`0`：

```java
int n = -536870912;
int a = n >>> 1;  // 01110000 00000000 00000000 00000000 = 1879048192
int b = n >>> 2;  // 00111000 00000000 00000000 00000000 = 939524096
int c = n >>> 29; // 00000000 00000000 00000000 00000111 = 7
int d = n >>> 31; // 00000000 00000000 00000000 00000001 = 1
```

对`byte`和`short`类型进行移位时, 会首先转换为`int`再进行位移. 

仔细观察可发现, 左移实际上就是不断地`x2`, 右移实际上就是不断地`/2`. 

#### 位运算

位运算是按位进行与, 或, 非和异或的运算. 

与运算的规则是, 必须两个数同时为`1`, 结果才为`1`：

```java
n = 0 & 0; // 0
n = 0 & 1; // 0
n = 1 & 0; // 0
n = 1 & 1; // 1
```

或运算的规则是, 只要任意一个为`1`, 结果就为`1`：

```java
n = 0 | 0; // 0
n = 0 | 1; // 1
n = 1 | 0; // 1
n = 1 | 1; // 1
```

非运算的规则是, `0`和`1`互换：

```java
n = ~0; // 1
n = ~1; // 0
```

`异或`运算的规则是, 如果两个数不同, 结果为`1`, 否则为`0`：

```java
n = 0 ^ 0; // 0
n = 0 ^ 1; // 1
n = 1 ^ 0; // 1
n = 1 ^ 1; // 0
```

对两个整数进行位运算, 实际上就是按位对齐, 然后依次对每一位进行运算. 例如：

```java
// 位运算
public class Main {
    public static void main(String[] args) {
        int i = 167776589; // 00001010 00000000 00010001 01001101
        int n = 167776512; // 00001010 00000000 00010001 00000000
        System.out.println(i & n); // 167776512
    }
}
```

上述按位与运算实际上可以看作两个整数表示的`IP`地址`10.0.17.77`和`10.0.17.0`, 
通过与运算, 可以快速判断一个`IP`是否在给定的网段内. 

#### 运算优先级

在Java的计算表达式中, 运算优先级从高到低依次是：

+ `()`
+ `!` `~` `++` `--`
+ `*` `/` `%`
+ `+` `-`
+ `<<` `>>` `>>>`
+ `&`
+ `|`
+ `+=` `-=` `*=` `/=`

记不住也没关系, 只需要加括号就可以保证运算的优先级正确. 

#### 类型自动提升与强制转型

在运算过程中, 如果参与运算的两个数类型不一致, 那么计算结果为较大类型的整型. 
例如, `short`和`int`计算, 结果总是`int`, 原因是`short`首先自动被转型为`int`：

```java
// 类型自动提升与强制转型
public class Main {
    public static void main(String[] args) {
        short s = 1234;
        int i = 123456;
        int x = s + i; // s自动转型为int
        short y = s + i; // 编译错误!
    }
}
```

也可以将结果强制转型, 即将大范围的整数转型为小范围的整数. 
强制转型使用(类型), 例如, 将`int`强制转型为`short`：

```java
int i = 12345;
short s = (short) i; // 12345
```

要注意, 超出范围的强制转型会得到错误的结果, 原因是转型时, `int`的两个高位字节直接被扔掉, 仅保留了低位的两个字节：

```java
// 强制转型
public class Main {
    public static void main(String[] args) {
        int i1 = 1234567;
        short s1 = (short) i1; // -10617
        System.out.println(s1);
        int i2 = 12345678;
        short s2 = (short) i2; // 24910
        System.out.println(s2);
    }
}
```

因此, 强制转型的结果很可能是错的. 

#### 练习

计算前N个自然数的和可以根据公示：

```java
(1+N)×N2\frac{(1+N)\times N}22(1+N)×N​
```

请根据公式计算前N个自然数的和：

```java
// 计算前N个自然数的和
public class Main {
    public static void main(String[] args) {
        int n = 100;

        System.out.println(sum);
        System.out.println(sum == 5050 ? "测试通过" : "测试失败");
    }
}
```

从下载练习：计算前N个自然数的和 (推荐使用IDE练习插件快速下载)

#### 小结

+ 整数运算的结果永远是精确的；
+ 运算结果会自动提升；
+ 可以强制转型, 但超出范围的强制转型会得到错误的结果；
+ 应该选择合适范围的整型(int或long), 没有必要为了节省内存而使用byte和short进行整数运算. 
