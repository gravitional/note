# rust basic

[When should we use unwrap vs expect in Rust](https://stackoverflow.com/questions/61301581/when-should-we-use-unwrap-vs-expect-in-rust)

Rust没有函数重载, 所以应该有一种方法来声明"unwrap with a message", 这是预期的.

+ `expect` == `unwrap` with a message
+ `expect_err` == `unwrap_err` with a message

关于 `unwrap vs expect` 的使用场景, Rust Book(第9章)说:

使用 `expect` 而不是 `unwrap`, 并提供良好的错误消息可以传达您的意图, 并使跟踪 panic 的来源更容易.
因为这个错误消息以我们指定的文本开始, 这样更容易找到代码中这个错误消息的来源.

## `?`操作符

[What is this question mark operator about?](https://stackoverflow.com/questions/42917566/what-is-this-question-mark-operator-about)

而 `?`操作符 可以处理 Reuslt 或 Option

```rust
let output = match File::create(filename) {
Ok(f) => { f }
Err(e) => { return Err(e); }
};
```

等价却更简洁的写法如下:

```rust
let output = File::create(filename)?;
```

`Rust` 没有异常.
它有 `panic!()`, 但不鼓励将它们用于错误处理(它们用于处理不可恢复的错误).
在Rust中, 错误处理使用Result.
一个典型的例子是:

```rust
fn halves_if_even(i: i32) -> Result<i32, Error> {
    if i % 2 == 0 {
        Ok(i / 2)
    } else {
        Err(/* something */)
    }
}

fn do_the_thing(i: i32) -> Result<i32, Error> {
    let i = match halves_if_even(i) {
        Ok(i) => i,
        Err(e) => return Err(e),
    };

    // use `i`
}
```

这很好, 因为:

在编写代码时, 你不能偶然忘记处理错误,
在阅读代码时, 您可以立即看到这里有可能出现错误.
然而, 它并不理想, 因为它非常啰嗦.
这里是问号运算符的位置?出现的原因.

以上可以重写为:

```rust
fn do_the_thing(i: i32) -> Result<i32, Error> {
    let i = halves_if_even(i)?;

    // use `i`
}
```

这样更简洁.
这里的 `?` 相当于上面的match语句, 并附加转型
简而言之:

+ 如果OK, 它将 uppacks the `Result`
+ 如果不OK, 则 `return` error, 并对错误值调用 [From::from][],
可能将其转换为另一种类型.

这有点神奇, 但错误处理需要一些魔力来减少样板文件,
并且与 exceptions 不同,
它可以立即看到哪些函数调用 可能会或不会出错: 那些带有`?`装饰的

magic的体现之一是, `?` 也适用于 `Option`:

```rust
// Assume
// fn halves_if_even(i: i32) -> Option<i32>

fn do_the_thing(i: i32) -> Option<i32> {
    let i = halves_if_even(i)?;

    // use `i`
}
```

`?` 操作符, 在Rust 1.13.0版本中稳定, 由(不稳定的)[Try trait]提供支持.

参见:
[问号是操作符吗?相当于试试!宏?](https://stackoverflow.com/q/40545332/155423)
[为什么 try!()和?在不返回 Option或Result的函数中失败?](https://stackoverflow.com/q/30555477/155423)

[From::from]: https://doc.rust-lang.org/std/convert/trait.From.html#tymethod.from
[Try trait]: https://doc.rust-lang.org/std/ops/trait.Try.html
