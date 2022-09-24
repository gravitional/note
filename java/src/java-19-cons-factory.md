# 工厂方法

设计模式中的工厂方法

[工厂方法](https://www.liaoxuefeng.com/wiki/1252599548343744/1281319170474017)

>定义一个用于创建对象的 `接口`(interface), 让 `子类` 决定实例化哪一个`类`.
>Factory Method 使一个类的 `实例化` 延迟到其 `子类`.

工厂方法即 Factory Method, 是一种 `对象创建型` 模式.
工厂方法的目的是使得 `创建对象` 和 `使用对象` 是分离的,
并且客户端总是引用 `抽象工厂` 和 `抽象产品`:

<pre class="ascii"><code class="language-ascii"
style="font-family: JetBrainsMono, &quot;Courier New&quot;, Consolas, monospace;">
┌─────────────┐      ┌─────────────┐
│   Product   │      │   Factory   │
└─────────────┘      └─────────────┘
       ▲                    ▲
       │                    │
┌─────────────┐      ┌─────────────┐
│ ProductImpl │&lt;─ ─ ─│ FactoryImpl │
└─────────────┘      └─────────────┘
</code></pre>

我们以具体的例子来说: 假设我们希望实现一个解析 `字符串` 到 `Number` 的Factory,
可以定义如下:

```java
public interface NumberFactory {
    Number parse(String s);
    }
```

有了工厂接口, 再编写一个工厂的 `实现类`:

```java
public class NumberFactoryImpl implements NumberFactory {
    public Number parse(String s) {
    return new BigDecimal(s);
    }
}
```

而产品接口是 `Number`, `NumberFactoryImpl` 返回的实际产品是 `BigDecimal`.

那么客户端如何创建 `NumberFactoryImpl` 呢?
通常我们会在接口 `Factory` 中定义一个静态方法 `getFactory()` 来返回真正的子类:

```java
public interface NumberFactory { // 创建方法:
    Number parse(String s);
    // 获取工厂实例:
    static NumberFactory getFactory() {
        return impl;
    }
    static NumberFactory impl = new NumberFactoryImpl();
}
```

在客户端中, 我们只需要和工厂接口 `NumberFactory` 以及抽象产品 `Number` 打交道:

```java
NumberFactory factory = NumberFactory.getFactory();
Number result = factory.parse("123.456");
```

`调用方` 可以完全忽略真正的工厂 `NumberFactoryImpl` 和实际的产品 `BigDecimal`,
这样做的好处是允许创建产品的代码 `独立地变换`, 而不会影响到 `调用方`.

有的童鞋会问: 一个简单的 `parse()` 需要写这么复杂的工厂吗?
实际上大多数情况下我们并不需要 `抽象工厂`, 而是通过 `静态方法` 直接返回产品, 即:

```java
public class NumberFactory {
    public static Number parse(String s) {
        return new BigDecimal(s);
    }
}
```

这种简化的使用 `静态方法` 创建产品的方式称为 `静态工厂方法`(Static Factory Method).
静态工厂方法广泛地应用在 Java 标准库中. 例如:

```java
Integer n = Integer.valueOf(100);
```

`Integer` 既是 `产品` 又是 `静态工厂`.
它提供了静态方法 `valueOf()` 来创建 `Integer`.
那么这种方式和直接写 `new Integer(100)` 有何区别呢?
我们观察 `valueOf()` 方法:

```java
public final class Integer {
    public static Integer valueOf(int i) {
        if (i >= IntegerCache.low && i <= IntegerCache.high)
        return IntegerCache.cache[i + (-IntegerCache.low)];
        return new Integer(i);
        }
    ...
}
```

它的好处在于,
`valueOf()` 内部可能会使用 `new` 创建一个 新的 `Integer` 实例,
但也可能直接返回一个 `缓存的` Integer实例.
对于调用方来说, 没必要知道 `Integer` 创建的细节.
工厂方法可以隐藏创建产品的细节,
且不一定每次都会真正创建产品,
完全可以返回缓存的产品, 从而提升速度并减少内存消耗.

如果调用方直接使用 `Integer n = new Integer(100)`,
那么就失去了使用缓存优化的可能性.

我们经常使用的另一个静态工厂方法是 `List.of()`:

```java
List<String> list = List.of("A", "B", "C");
```

这个静态工厂方法接收 `可变参数`, 然后返回 `List接口`.
需要注意的是, 调用方获取的产品总是 `List接口`, 而且并不关心它的 `实际类型`.
即使调用方知道 `List` 产品的实际类型是 `java.util.ImmutableCollections$ListN`,
也不要去强制转型为子类, 因为静态工厂方法 `List.of()` 保证返回 `List`,
但也完全可以修改为返回 `java.util.ArrayList`.

这就是里氏替换原则:
返回实现 `接口` 的任意子类都可以满足该方法的要求, 且不影响调用方.
总是引用 `接口` 而非实现类, 能允许变换 `子类` 而不影响 `调用方`, 即尽可能面向抽象编程.

和 `List.of()` 类似,
我们使用 `MessageDigest` 时, 为了创建某个 `摘要算法`,
总是使用静态工厂方法 `getInstance(String)`:

```java
MessageDigest md5 = MessageDigest.getInstance("MD5");
MessageDigest sha1 =MessageDigest.getInstance("SHA-1");
```

调用方通过产品名称获得产品实例, 不但调用简单,
而且获得的引用仍然是 `MessageDigest` 这个抽象类.

## 练习

使用静态工厂方法实现一个类似 `20200202` 的整数转换为 `LocalDate`:

```java
public class LocalDateFactory {
    public static LocalDate fromInt(int yyyyMMdd) {
         ...
         }
}
```

从下载练习: 静态工厂方法练习 (推荐使用IDE练习插件快速下载)

## 小结

+ 工厂方法是指定义 `工厂接口` 和 `产品接口`, 如 `getFactory()`,  `parse("123.456")`
但如何创建 `实际工厂` 和 `实际产品` 被推迟到 `子类实现`,
从而使 `调用方` 只和 `抽象工厂` 与 `抽象产品` 打交道.

+ 实际更常用的是更简单的 `静态工厂方法`, 它允许工厂内部对创建产品进行优化.

+ 调用方尽量持有 `接口` 或 `抽象类`, 避免持有 `具体类型` 的子类,
以便 `工厂方法` 能随时切换不同的 子类 返回, 却不影响调用方代码.
