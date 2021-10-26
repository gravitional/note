# 性能比较: 循环对迭代器

为了决定使用哪个实现, 我们需要知道哪个版本的 `search` 函数更快一些: 是直接使用 `for` 循环的版本还是使用迭代器的版本.

我们运行了一个性能测试, 通过将阿瑟·柯南·道尔的"福尔摩斯探案集"的全部内容加载进 `String` 并寻找其中的单词 `"the"`.
如下是 for 循环版本和迭代器版本的 search 函数的性能测试结果:

test bench_search_for  ... bench:  19,620,300 ns/iter (+/- 915,700)
test bench_search_iter ... bench:  19,234,900 ns/iter (+/- 657,200)

结果迭代器版本还要稍微快一点!
这里我们将不会查看性能测试的代码, 我们的目的并不是为了证明他们是完全等同的,
而是得出一个怎样比较这两种实现方式性能的基本思路.

对于一个更全面的性能测试, 将会检查不同长度的文本, 不同的搜索单词, 不同长度的单词和所有其他的可变情况.
这里所要表达的是: `迭代器`, 作为一个高级的抽象, 被编译成了与 `手写的底层代码` 大体一致性能代码.
`迭代器` 是 `Rust` 的 `零成本抽象`(zero-cost abstractions)之一, 它意味着抽象并不会引入`运行时开销`,
它与本贾尼·斯特劳斯特卢普(C++ 的设计和实现者)在 "Foundations of C++"(2012) 中所定义的 `零开销`(zero-overhead)如出一辙:

>In general, C++ implementations obey the `zero-overhead` principle: What you don't use, you don't pay for.
>And further: What you do use, you couldn't hand code any better.
>Bjarne Stroustrup "Foundations of C++"
>
>从整体来说, C++ 的实现遵循了零开销原则: 你不需要的, 无需为他们买单.
>更有甚者的是: 你需要的时候, 也不可能找到其他更好的代码了.
>本贾尼·斯特劳斯特卢普 "Foundations of C++"

作为另一个例子, 这里有一些取自于`音频解码器`的代码.
解码算法使用`线性预测`数学运算(linear prediction mathematical operation), 根据之前样本的线性函数来预测将来的值.
这些代码使用迭代器链来对作用域中的三个变量进行了某种数学计算:

一个叫 `buffer` 的数据 slice, 一个有 12 个元素的数组 `coefficients`, 和一个代表位移位数的 `qlp_shift`.
例子中声明了这些变量但并没有提供任何值;
虽然这些代码在其上下文之外没有什么意义, 不过仍是一个简明的现实中的例子,
来展示 Rust 如何将高级概念转换为底层代码:

```rust
let buffer: &mut [i32]; // 数据 slice
let coefficients: [i64; 12]; // 数组
let qlp_shift: i16;

for i in 12..buffer.len() {
    let prediction = coefficients.iter()
                                 .zip(&buffer[i - 12..i])
                                 .map(|(&c, &s)| c * s as i64) // as 进行强制类型转换
                                 .sum::<i64>() >> qlp_shift;  // >> 右移位操作
    let delta = buffer[i];
    buffer[i] = prediction as i32 + delta;
}
```

为了计算 `prediction` 的值, 这些代码遍历了 `coefficients` 中的 `12` 个值,
使用 `zip` 方法, 将 `系数` 与 `buffer` 的前 12 个值组合在一起.
接着将每一对值相乘, 再将所有结果相加, 然后将总和右移 qlp_shift 位.

像音频解码器这样的程序通常最看重计算的`性能`.
这里, 我们创建了一个`迭代器`, 使用了两个`适配器`, 接着`消费`(consume)了其值.

Rust 代码将会被编译为什么样的`汇编代码`呢?
好吧, 在编写本书的这个时候, 它被编译成与手写的相同的汇编代码.
要遍历 `coefficients` 的`值`, 完全用不到循环: Rust 知道这里会迭代 12 次, 所以它`展开`(unroll)了循环.
`展开`是一种优化, 它移除循环控制代码的开销, 并替换成迭代中的使用的重复代码.

所有的系数都被储存在了 `寄存器` 中, 这意味着访问他们非常快. 运行时(runtime)也不需要对数组访问进行边界检查.
所有这些 Rust 能够提供的优化使得结果代码极为高效. 现在知道这些了, 请放心大胆的使用 `迭代器` 和 `闭包` 吧!
他们使得代码看起来更高级, 但并不为此引入`运行时`性能损失(runtime performance penalty).

## 总结

闭包和迭代器是 Rust 受函数式编程语言观念所启发的功能.
他们对 Rust 以底层的性能来明确的表达高级概念的能力有很大贡献.
闭包和迭代器的实现达到了不影响运行时性能的程度.
这正是 Rust 竭力提供零成本抽象的目标的一部分.

现在我们改进了我们 I/O 项目的(代码)表现力, 让我们看一看更多 cargo 的功能,
他们将帮助我们准备好将项目分享给世界.
