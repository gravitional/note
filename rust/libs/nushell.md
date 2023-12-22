# nushell

## ls rm recursive

[nushell: list all files recursively](https://superuser.com/questions/1720919/nushell-list-all-files-recursively)

nushell 中使用的通配符库是 [crate/wax](https://docs.rs/crate/wax/latest)
简单地说, 对应 bash 下 `rm ./* -rf` 的命令是

```bash
rm **/* -rf
```

类似地, 如果像实现 `ls --recursive` 的效果, 使用

```bash
ls **/*
```

`ls -a` 用来显示隐藏目录中的文件.
如果只想返回文件名中有 `xxx` 的文件(而不是路径中的目录等等), 那么可以使用

```bash
ls -a **/*xxx* | where ($it.name | path basename) =~ 'xxx'
```

需要注意的是, 实验性的 Nushell glob 命令(0.61 版)
总是会使用 glob **/*xxx* 返回隐藏目录中的文件, 但它只返回文件名, 
而不是像 ls 那样返回 Nushell 列中的文件, 所以听起来并不适合你的使用情况:

有时我只需要通过其他列进行过滤

^find(不是内置的, 而是二进制的)也是如此.

不过, 请注意, 使用 ls 返回文件名进行处理确实有一些限制, 至少在目前的 Nushell 命令中是这样:

目前还无法像 ^find -xdev 选项那样将结果限制在单个文件系统中.

Nushell 的 ls **/* 会递归跟踪符号链接.
如果使用类似 ln -s ... parent 的选项, ls 就会进入无限循环.

一般来说, 在当前的 Nushell 版本中使用 ls **/... globs 会非常小心.
至少从目前来看, ^find 二进制文件可能是更安全的选择.

分享到
改进此答案
关注