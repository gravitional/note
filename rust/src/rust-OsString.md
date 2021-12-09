# OsString

[Struct std::ffi::OsString](https://doc.rust-lang.org/std/ffi/struct.OsString.html)

    pub struct OsString { /* 字段省略 */ }

一种可以表示 owned , mutable platform-native `字符串` 的类型, 但又可以 `廉价地` 与 `Rust` 字符串相互转换.

+ 对这种类型的需求源于以下事实:

+ 在 `Unix` 系统中, 字符串通常是`非零字节`的任意序列, 在许多情况下被解释为 `UTF-8`.
+ 在 `Windows` 系统中, 字符串通常是非零的`16-bit`值的任意序列, 在有效的情况下被解释为 `UTF-16`.
+ 在 `Rust` 中, 字符串总是有效的 `UTF-8`, 它可能包含`零`.

`OsString` 和 `OsStr` 通过同时表示 `Rust` 和平台原生的字符串值来弥合这一差距,
特别是允许 `Rust` 字符串在可能的情况下, 无代价地转换为 `OS` 字符串.
这样做的后果是 `OsString` 实例不是 `NUL` 结尾的;
为了传递给例如 `Unix` 系统调用, 你应该创建一个 [`CStr`](https://doc.rust-lang.org/std/ffi/struct.CStr.html)

`OsString` 之于 [&OsStr][], 就像 [String][] () 之于 [&str][]  一样:
每对中的前者是 `owned 字符串`; 后者是`借用的引用`(borrowed references).

注意, `OsString` 和 `OsStr` 内部不一定以平台的原生形式持有字符串;
在 `Unix` 上, 字符串被存储为 `8-bit` 值的序列,
而在 `Windows` 上, 尽管字符串是基于 `16-bit`值的, 正如刚才讨论的那样,
但在 `Windows` 上 `字符串` 也被存储为`8位值`的序列, 以 `UTF-8` 的一个不太严格的变体编码.
在处理 `容量` 和 `长度值` 时, 这一点很有必要了解.

## 创建`OsString`

+ 从 `Rust字` 符串: `OsString` 实现了`From<String>`, 所以你可以使用 `my_string.into()` 从普通的 `Rust` 字符串创建 `OsString`.
+ 从切片: 就像你可以从空的 `Rust` 字符串开始, 然后用 `String::push_str` 压入一些 `&str` 子串切片一样,
你可以用 `OsString::new` 方法创建空的 `OsString`, 然后用 `OsString::push` 方法将`字符串切片`放入其中.

## 提取对整个`OS字符串`的借用引用

你可以使用 `OsString::as_os_str` 方法从 `OsString` 中得到 `&OsStr`;
它相当于对整个字符串的借用引用.

## 转换

关于 `OsString` 实现从/到 native 表示的转换的特性的讨论, 请参见[模块的顶层转换文档][].

[&OsStr]: https://doc.rust-lang.org/std/ffi/struct.OsStr.html
[String]: https://doc.rust-lang.org/std/string/struct.String.html
[&str]: https://doc.rust-lang.org/std/primitive.str.html
[模块的顶层转换文档]: https://doc.rust-lang.org/std/ffi/index.html#conversions
