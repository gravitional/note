# nushell bugs

[Drive letter on Windows? #34](https://github.com/olson-sean-k/wax/issues/34)
[Wax — Rust 文件系统库](https://crates.org.cn/crates/wax)

## `glob c:\xxx`

由于 nushell 使用的库 `wax`, 对于包含复杂 `glob` 的调用

```nu
glob c:\sim-solver\Release\*.{dll,exe}
```

不支持 `c:` 因为跟 模式重复 的语法冲突, 不支持反斜杠 `\`.
所以要写成

```nu
glob c\:/sim-solver/Release/*.{dll,exe}
```

## `cp` 不支持 `glob` 生成的 `list<string>`

所以可以用下面的语法代替:

```bash
glob c\:/sim-solver/Release/*.{dll,exe} | each {|it| cp $it dll-test };
```
