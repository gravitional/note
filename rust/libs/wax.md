# rust wax 库

Wax 是一个 Rust 库, 它
提供了可与文件路径和目录树进行匹配的 opinionated , portable globs.
Globs 使用熟悉的语法, 支持 强调组分边界 语义的表达式特性.

## 基本用法

将路径与 glob 匹配:

```rust
use wax::{Glob, Pattern};

let glob = Glob::new("*.png").unwrap();
assert!(glob.is_match("logo.png"));
```

将路径与带有匹配文本(捕获)的 glob 匹配:

```bash
use wax::{CandidatePath, Glob, Pattern};

let glob = Glob::new("**/{*.{go,rs}}").unwrap();

let path = CandidatePath::from("src/main.go");
let matched = glob.matched(&path).unwrap();

assert_eq!("main.go", matched.get(2).unwrap());
```

将目录树与 glob 匹配:

```rust
use wax::Glob;

let glob = Glob::new("**/*.{md,txt}").unwrap();
for entry in glob.walk("doc") {
    let entry = entry.unwrap();
    // ...
}
```

将目录树与带有否定词的 glob 匹配:

```bash
use wax::{Glob, LinkBehavior};

let glob = Glob::new("**/*.{md,txt}").unwrap();
for entry in glob
    .walk_with_behavior("doc", LinkBehavior::ReadTarget)
    .not(["**/secret/**"])
    .unwrap()
{
    let entry = entry.unwrap();
    // ...
}
```

将一个路径与多个 globs 匹配:

```rust
use wax::{Glob, Pattern};

let any = wax::any([
    "src/**/*.rs",
    "tests/**/*.rs",
    "doc/**/*.md",
    "pkg/**/PKGBUILD",
]).unwrap();
assert!(any.is_match("src/token/mod.rs"));
```

详见下文.

## 构建

glob 被编码为 UTF-8 字符串, 称为 glob 表达式,
类似于 Unix 路径, 由分隔符分隔的 nominal组件 组成.
Wax API 中最基本的类型是 Glob,
它是通过 inherent函数 或 标准转换trait 从 glob表达式 构建的.
在大多数应用程序接口中, 数据都是尽可能借用的,
但大多数类型也可以使用 `into_owned` 方法将数据复制到 owned 实例中.

```rust
use wax::Glob;

let glob = Glob::new("site/img/logo.svg").unwrap();
```

不仅 API 是为可移植性而设计的, `glob表达式` 也是如此.
无论平台或操作系统如何, glob 都支持相同的功能, 使用相同的语法.
**glob 表达式有别于路径**, [后者在语法和功能上因平台而异](https://docs.rs/crate/wax/latest#schemes-and-prefixes).

在 glob 表达式中, 正斜线 `/` 是唯一的路径组件分隔符,
禁止使用反斜线 `\`(反斜线可用于转义序列, 但不支持字面序列`\\`).
这意味着不可能在 nominal路径分量中表示 `\`,
但这个字符一般是禁止使用的, 不使用它可以避免混淆.

Globs 执行有关元字符, 模式 和 组件边界 的各种规则,
[拒绝无意义的表达.](https://docs.rs/crate/wax/latest#errors-and-diagnostics)
虽然这些规则有时会使 glob 表达式更难构成,
但它们也使 glob 表达式更一致, 更容易推理, 更不易出错.

## 模式

glob 类似于 Unix 路径, 但还支持可与路径和目录树匹配的 patterns.
模式使用的语法类似于 Unix shell 和 git 等工具中的 globbing,
但也有一些重要区别.

```rust
use wax::Glob;

let glob = Glob::new("**/*.{go,rs}").unwrap();
assert!(glob.is_match("src/lib.rs"));
```

模式 形成的 捕获可用于提取匹配文本(在许多正则表达式引擎中都能看到).
在上面的示例中, 有三种模式可以查询匹配的文本:
`**/`, `*` 和 `{go,rs}`.
每个 glob 表达式都隐含了对 complete matched text 的捕获.

Globs 使用一致且有主见的格式, **模式不可配置**;
特定全局表达式的语义始终相同.
例如, **`*` 不会跨越组件边界** 进行匹配.
Components 是路径和文件系统树的重要组成部分,
只有树通配符 `**`(见下文)才会隐式跨组件匹配.

## 通配符

通配符可以匹配路径中的任意文本, 是 globs 提供的最基本模式
(也可能是最熟悉的模式).

零或更多通配符 `*` 和 `$` 可匹配组件中的零个或多个任意字符(绝不是路径分隔符).
`零或更多通配符` 不能与其他 `零或更多通配符` 相邻.
`*` 通配符比较 eager , 会匹配尽可能长的文本,
而 `$` 通配符比较懒惰, 会匹配尽可能短的文本.

如果后面跟着一个 literal,
`*` 会在该字面的最后一次出现时停止, 而 `$` 则在第一次出现时停止.

exactly-one 通配符 `?` 可匹配组件中的任何单个字符(绝不匹配路径分隔符).
exactly-one 通配符不会自动分组,
因此连续的通配符模式(如`???`)会对每个`?`通配符形成不同的捕获.
[另一种方法](https://docs.rs/crate/wax/latest#alternatives)
是将 exactly-one 的通配符分组为单个捕获, 如 `{???}`.

tree 通配符 `**` 可匹配任何字符, 并允许跨过 零个或多个 components.
**这是唯一一种可以跨任意组件边界隐式匹配的模式;**
**所有其他模式都不能跨组件边界隐式匹配.**

当tree通配符 参与匹配且未终止 模式时, 其捕获的文本包括尾部分隔符.
如果tree通配符不参与匹配, 则其捕获文本为空字符串.

tree通配符必须以正向斜线或 terminations(表达式的开头 或 结尾)分隔.
tree通配符和路径分隔符是不同的, 构成tree通配符的任何相邻正斜线都会一起解析.
树状通配符中的 Rooting斜线 是有意义的, glob 表达式
`**/*.txt` 和 `/**/*.txt` 的区别在于前者是相对的(没有root),
而后者有 root.

如果 glob 表达式仅包含 tree通配符,
那么它将匹配任何及所有路径和任何及所有目录树的全部内容, 包括 root.

## 字符类, Character Classes

字符类 匹配组件中 a group of literals and ranges
的任何单个字符(绝不匹配路径分隔符).

类由方括号 `[...]` 分隔. 单个字母依字面指定.
例如 `[ab]` 匹配 `a` or `b`.
字符范围由两个字符组成, 中间用连字符分隔,
如 `[x-z]` 用于匹配 `x`, `y` 或 `z`.
字符类精确匹配字符, 且始终区分大小写,
因此 `[ab]` 和 `{a,b}` 的表达式不一定相同.

在一个字符类中, 可以使用任意数量的字符字面量和范围.
例如, `[qa-cX-Z]` 匹配 q, a, b, c, X, Y 或 Z 中的任何一个.

可以在字符类模式的开头加上感叹号 `!` 来否定字符类.
例如 `[!a]` 匹配 `a` 之外的字符. **这些是唯一支持否定的模式.**

虽然 globs 也支持通过反斜杠转义,
但使用字符类可以转义 `*`, `$` 等元字符.
要匹配字符类中的控制字符 `[`, `]` 和 `-`, 必须通过反斜线转义,
如 `[a\-]` 来匹配 `a` 或 `-`.

字符类具有显著的平台特定行为,
因为它们匹配本地路径中的任意字符, 但从不匹配路径分隔符.
这意味着, 如果一个字符类在特定平台上只包含路径分隔符,
那么该字符类将被视为空类, 什么也不匹配.

例如, 在表达式 `a[/]b` 中,
字符类 `[/]` 在 Unix 和 Windows 上不匹配任何内容.
这种字符类不会被拒绝, 因为任意字符的作用取决于平台.
在实践中, 这很少成为问题, **但应避免使用此类模式**.

字符类本身的作用有限,
但可以与[重复的字符类](https://docs.rs/crate/wax/latest#repetitions)很好地组合在一起.

## 备选字符

备选字符匹配由一个或多个逗号分隔的子球体组成的任意序列,
并用大括号 `{...,...}`分隔.
例如, `{a?c,x?z,foo}` 可匹配 `a?c`, `x?z` 或 `foo` 中的任意一个sub-globs.
候选词可以任意 nested and composed with[重复](https://docs.rs/crate/wax/latest#repetitions).

无论 sub-globs 的内容如何, Alternatives 都会形成 单个捕获.
该捕获是由sub-glob的完整匹配形成的,
因此如果备选`{a?c,x?z}`匹配 `abc`, 那么捕获的文本将是 `abc`(而不是 b).

Alternatives can be used to group captures using a single sub-glob
例如, `{*.{go,rs}}` 可用于捕获具有特定扩展名的整个文件名,
而 `{???}` to group a sequence of exactly-one wildcards.

Alternatives 必须考虑邻接规则和相邻模式.
例如, 允许使用 `*{a,b*}`, 但不允许使用 `*{a,*b}`.
此外, 它们不能包含由单个树通配符 `**` 组成的 sub-glob,
cannot root a glob expression
as this could cause the expression to match or walk overlapping trees.

## 重复

重复匹配指定次数的 sub-glob.
重复由尖括号和分隔冒号 ` <...:...>` 分隔,
冒号前是一个 `sub-glob`, 冒号后是一个可选的边界说明.
例如, `<a*/:0,>` 匹配 sub-glob `a*/` 零次或多次.
虽然不像树通配符那样隐式, **但重复可以跨组件边界进行匹配**(并且可以包含树通配符).
Repetitions may be arbitrarily nested and composed with alternatives.

边界说明由包含上下限组成, 上下限之间用逗号分隔,
例如 `:1,4` 匹配 1 到 4 次. 上限是可选的, 可以省略.
例如, `:1,` 匹配一次或多次(注意尾部的逗号`,`).
单个上限是收敛的, 因此 `:3` 恰好匹配三次(上下限都是3).
如果没有指定上下限, 那么子全局搜索匹配一次或多次,
因此 `<a:>` 和 `<a:1,>` 是等效的.
类似地, 如果冒号 : 也省略了, 那么子全局搜索匹配零次或多次,
因此 `<a>` 和 `<a:0,>` 是等效的.

无论 sub-glob的内容如何, Repetitions  都形成单个 capture .
捕获由 sub-glob的完全匹配形成.
如果 `<abc/>` 匹配 `abc/abc/`, 那么捕获的文本将是 `abc/abc/`.

Repetitions 可以很好地与 [字符类](https://docs.rs/crate/wax/latest#character-classes)组合.
大多数时候, 像 `{????}` 这样的全局表达式就足够了,
但是更具体的表达式 `<[0-9]:4>` 进一步将匹配的字符约束为数字.
Repetitions 也可以更简洁, 例如 `<?:8>`.

此外, repetitions 可以形成tree表达式, 进一步约束组件,
例如 `<[!.]*/>[!.]*`, 以匹配 任何组件中都不含前导点`.` 的路径.

重复必须考虑邻接规则和邻接模式.
例如, `a/<b/**:1,>` 是允许的, 但是 `<a/**:1,>/b` 不允许.
此外, 它们不能包含由单个分隔符`/`, 单个零或多个通配符`*`或`$`组成的子glob,
也不能包含单个树通配符 `**`.
下界为零的 Repetitions 不能作为全局表达式的根,
因为这可能会导致表达式 match or walk overlapping trees.

## 组合子 Combinators

Glob模式可以使用 `any` 组合子 组合和匹配在一起.
`any` 接受一个 `IntoIterator`,
其items是编译后的 `Pattern` 或 `str` 片段.
输出是一个 `Any` , 它实现了 `Pattern` 并能有效地匹配它的任何输入模式.

```rust
use wax::{Glob, Pattern};

let any = wax::any(["**/*.txt", "src/**/*.rs"]).unwrap();
assert!(any.is_match("src/lib.rs"));
```

与[alternatives](https://docs.rs/crate/wax/latest#alternatives)方法不同,
`Any` 支持具有 overlapping trees 的模式(rooted 和 unrooted表达式).
然而, 组合器只能执行逻辑匹配, 不能拿 `Any` 匹配 directory tree
(as with `Glob::walk` ).

## 标志和大小写敏感性

标志切换 glob 的匹配行为.
重要的是, 标志是 glob 表达式的一部分, 而不是 API.
Behaviors 按照它们在 glob 表达式中出现的顺序, 紧随标志而切换.

标志由带有问号 `(?...)` 的圆括号分隔, 并且可以出现在 glob 表达式的任何地方,
只要它们不拆分树通配符(例如, `a/*(?i)*` 是不允许的).
每个标志由一个字符表示, 可以通过在对应字符之前加上减号 `-` 来反转.
标志按照它们在 `(?...)` 中出现的顺序切换.

唯一支持的标志是 case-insensitivty 标志 `i`.
默认情况下, Glob表达式使用与目标平台的文件系统API相同的大小写敏感性
(Unix区分大小写, Windows区分大小写), 但`i`可以根据需要进行显式切换.

例如, `(?-i)photos/**/*.(?i){jpg,jpeg}`
匹配 photos 目录下的文件路径,
该文件路径具有 case-sensitive base
和 case-insensitive 扩展名jpg或jpeg.

当使用 `Glob::partition` 对 [Glob表达式进行分区](https://docs.rs/crate/wax/latest#partitioning-and-semantic-literals)时,
Wax会考虑字面量, 其配置的大小写敏感性,
以及目标平台的文件系统API的大小写敏感性.
在没有 flags 的Glob表达式中, Partitioning 不受影响.

## 错误和诊断

`GlobError ` 类型表示在 building a pattern
或 walking a directory tree 时可能出现的错误条件.
`GlobError` 及其子错误通过 [thiserror](https://github.com/dtolnay/thiserror)
实现 标准的 `Error`和 `Display` traits.

Wax可选地集成 [miette crate](https://github.com/zkat/miette), 它可以用于捕获和显示诊断信息.
这对于向提供Glob表达式的用户报告错误非常有用.
当启用时, 错误类型实现 Diagnostic trait.

```bash
Error: wax::glob::adjacent_zero_or_more

  x malformed glob expression: adjacent zero-or-more wildcards `*` or `$`
   ,----
 1 | doc/**/*{.md,.tex,*.txt}
   :        |^^^^^^^^|^^^^^^^
   :        |        | `-- here
   :        |        `-- in this alternative
   :        `-- here
   `----
```

Wax还提供了 inspection APIs,
允许代码查询 glob metadata, 比如 captures 和 variance.

```rust
use wax::Glob;

let glob = Glob::new("videos/**/{*.{mp4,webm}}").unwrap();
assert_eq!(2, glob.captures().count());
```

## Cargo Features

Wax提供了一些可选的集成和特性, 可以通过下面描述的Cargo特性进行切换.

|特性 |默认 |依赖项| 描述
|---|---|---|---|
|miette|没有|miette, tardar|与miette集成, 提供诊断错误类型和报告.|
|walk|是|walkdir| 提供与 directory trees 匹配 globs的 APIs.|

特性可以在crate的 `Cargo.toml` manifest.

```toml
[dependency.wax]
version = "^0.x.0"
default-features = false
features = [
    "miette",
    "walk"
]
```

## 不支持的路径特性 Unsupported Path Features

任何未被识别为 分隔符 或 模式 的组件都被解释为字面量.
结合严格的规则, 这意味着 **一些特定于平台的路径特性** 不能直接在globs中使用.
这个限制是由设计决定的, 对于某些用例, 可能需要额外的代码来弥补这个差距.

## 分区和语义字面量 Partitioning and Semantic Literals

Globs不支持 *当前目录* 或 *父目录* 的概念.
路径分量 `.` 和 `..` 被解释为 literals, 并且只与相应的组件匹配路径
(即使在Unix和Windows上).

例如, 全局变量 `src/../*.rs` 匹配路径 `src/../lib`.
但**不** 匹配语义上等效的 `lib.rs`.

当父目录组件遵循全局变量中的模式时, 它们的含义不明确, 而且效用也小得多.
然而这些组件是 intuitive,
并且当它们位于 variant patterns 之前(即作为前缀)时,
对于转义工作目录通常很重要.

例如, `../src/**/*.rs` 比 glob `src/**/../*.rs`
具有更明显的预期意义.

如上所示, 第一个glob只匹配 literal 路径组件.
而不是用父目录替换它的路径.

Glob::partition 可用于 isolate `patterns`前面的 语义组件,
并对其应用语义路径操作(即`..`).
`partition`将 Glob 分为一个不变的 `PathBuf` 前缀和一个可变的 `Glob`后缀.
这里, 不变性意味着 that the partition contains no glob patterns
that resolve differently than an equivalent native path
using the target platform's file system APIs.

前缀可以根据需要与glob结合使用.

```rust
use dunce; // Avoids UNC paths on Windows.
use std::path::Path;
use wax::{Glob, Pattern};

let path: &Path = /* ... */ // Candidate path.

let directory = Path::new("."); // Working directory.
let (prefix, glob) = Glob::new("../../src/**").unwrap().partition();
let prefix = dunce::canonicalize(directory.join(&prefix)).unwrap();
if dunce::canonicalize(path)
    .unwrap()
    .strip_prefix(&prefix)
    .map(|path| glob.is_match(path))
    .unwrap_or(false)
{
    // ...
}
```

此外, 可以使用 `Glob::has_semantic_literals`
检测 glob 中的, 在目标平台上具有 特殊语义的 literal components.
当启用 `miette` 特性时, 这些字面值将作为警告报告.

```rust
use wax::Glob;

let glob = Glob::new("../**/src/**/main.rs").unwrap();
assert!(glob.has_semantic_literals());
```

## 方案和前缀 Schemes and Prefixes

虽然globs可以被rooted, 但它们不能包括schemes和Windows路径前缀.

例如, Windows UNC共享路径`\\server\share\src`不能直接表示为glob.

这可能是有限制的, 但Wax的设计明确禁止了这一点:
Windows前缀和其他卷组件是不可移植的.

相反, 当需要时, 必须使用额外的native路径或工作目录,
例如 [Nym提供的——tree选项](https://github.com/olson-sean-k/nym).
在大多数上下文中, globs是相对于某些这样的工作目录应用的.

## Non-nominal约束

Globs是严格的 nominal, 不支持任何 non-nominal constraints.
不可能根据glob表达式中的附加元数据(如修改时间戳)
直接过滤或以其他方式选择路径或文件.

但是, 用户代码可以查询任何匹配路径的元数据,
或者对于匹配结果中的目录树, 使用 `FileIterator::filter_tree` 应用这种过滤.

有关此类附加功能, 包括 metadata filters 和 transformations using matched text,
请参见[Nym](https://github.com/olson-sean-k/nym).

## 编码

Globs只对UTF-8编码的文本进行操作.
但是, 并非所有平台上的路径都使用这种编码.
Wax使用 CandidatePath 类型通过有损转换对本地路径进行重新编码,
当路径的一部分不能表示为有效的 UTF-8时, 这种转换使用Unicode替换码点.
在实践中, 大多数路径都可以用UTF-8进行无损编码,

但这意味着Wax无法匹配或捕获某些 literal byte strings.

## 稳定

在撰写本文时, Wax是实验性的, 不稳定的.
glob表达式的语法和语义可能会在版本 `0.y.z` 之间发生变化.
without warning nor deprecation.
