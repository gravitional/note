# rust-sd

[sd - search & displace](https://github.com/chmln/sd)

## readme

`sd` 是一个直观的查找和替换CLI.
为什么使用它而不是任何现有的工具?

+ 无痛的正则表达式

`sd` 使用你在 `JavaScript` 和 `Python` 中已经知道的 `regex` 语法.
忘掉处理sed或awk的怪癖吧--马上就能干活.

+ 字面字符串模式

非正则查找和替换. 不再有反斜线或记住哪些字符是特殊的, 需要被转义的.

+ 易读, 易写

查找和替换表达式被分割开来, 这使得它们易于阅读和书写. 不用再为未闭合和转义的`slash`es(正斜线)而烦恼.

+ 智能, 常识性的默认值

默认值遵循常识, 是为典型的日常使用而定制的.

## 与sed的比较

虽然 `sed` 可以做很多事, 但 `sd` 只专注于做一件事, 而且做得很好.

一些 `cherry-picked` 的例子, sed在其中大放异彩.

+ 替换所有出现的内容的更简单的语法.
    + sd: `sd before after`
    + sed: `sed s/before/after/g`

+ 用逗号替换新行.
    + sd: `sd '\n' ','`
    + sed: `sed ':a;N;$!ba;s/\n/,/g'`

+ 从含有斜线的字符串中提取东西.
        sd: echo "样本与/path/" | sd '.*(/.*/)' '$1'
        sed: 根据表达式的不同, 每次使用不同的分隔符, 这样命令就不会完全看不懂了
            echo "样本与/path/" | sed -E 's/.*(/.*\/)//1/g'
            echo "样本与/path/" | sed -E 's|.*(/.*/)||1|g' .
+ 对文件进行就地修改.
    + sd: sd在file.txt之后.
    + sed: 你需要记住使用-e, 否则一些平台会认为下一个参数是一个备份后缀
        + `sed -i -e 's/before/after/g' file.txt`

## 安装

### Cargo

[Cargo]: https://doc.rust-lang.org/cargo/getting-started/installation.html

[Cargo][] 是Rust的软件包管理器.

你可以通过以下方式安装 `Cargo`

```bash
curl https://sh.rustup.rs -sSf | sh
# 然后
cargo install sd
```

+ Alpine Linux

```bash
apk add sd
```

在安装之前, 确保启用相应的软件库.

+ Arch Linux

```bash
pacman -S sd
```

+ Gentoo (unc3nsored overlay)

```bash
emerge -av sys-apps/sd
```

在安装之前, 请确保启用了相应的[覆盖层](https://github.com/xxc3nsoredxx/unc3nsored).

+ Fedora

```bash
dnf install sd
```

+ FreeBSD

```bash
pkg install sd
```

+ Windows系统

```bash
choco install sd-cli
```

+ MacOS

```bash
brew install sd
```

+ Void Linux

```bash
xbps-install sd
```

## 快速指南

`String-litera`模式. 默认情况下, 表达式被当作 `regex` 处理. 使用 `-s` 或 `--string-mode` 来禁用 `regex`.

```bash
> echo 'lots((([]))) of special chars' | sd -s '((([])))' ''
lots of special chars
```

+ 基本的 `regex` 使用 - 让我们修剪一些尾部的空白

```bash
> echo 'lorem ipsum 23   ' | sd '\s+$' ''
lorem ipsum 23
```

+ 捕获组

有索引的捕获组.

```bash
> echo 'cargo +nightly watch' | sd '(\w+)\s+\+(\w+)\s+(\w+)' 'cmd: $1, channel: $2, subcmd: $3'
cmd: cargo, channel: nightly, subcmd: watch
```

命名的捕获组.

```bash
> echo "123.45" | sd '(?P<dollars>\d+)\.(?P<cents>\d+)' '$dollars dollars and $cents cents'
123 dollars and 45 cents
```

在不太可能的情况下, 如果你遇到歧义, 可以通过使用 `${var}` 而不是 `$var` 来解决. 下面是一个例子.

```bash
> echo '123.45' | sd '(?P<dollars>\d+)\.(?P<cents>\d+)' '$dollars_dollars and $cents_cents'
 and

> echo '123.45' | sd '(?P<dollars>\d+)\.(?P<cents>\d+)' '${dollars}_dollars and ${cents}_cents'
123_dollars and 45_cents
```

+ 在一个文件中查找和替换

```bash
> sd 'window.fetch' 'fetch' http.js
```

完事儿. 该文件被就地修改.

预览修改.

```bash
> sd -p 'window.fetch' 'fetch' http.js
```

+ 在整个项目中查找和替换, 这个例子使用[fd](https://github.com/sharkdp/fd).
优秀的unix哲学来拯救.

```bash
sd 'from "react"' 'from "preact"' $(fd --type file)
```

相同的, 但有备份(考虑版本控制).

```bash
for file in $(fd --type file); do
  cp "$file" "$file.bk"
  sd 'from "react"' 'from "preact"' "$file";
done
```

+ 边缘案例

如果替换前后的字符串以`--`开头, 需要在它前面加上 `--`(这是bash本身的限制).

```bash
echo "test/test" | sd '/' -- '--inteneded--'
test--inteneded--test

echo "start/--/end" | sd --string-mode -- '--' 'middle'
start/middle/end
```
