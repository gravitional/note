# 键盘高级操作技巧

cmdline 第九章: 键盘高级操作技巧
以下命令将会露面:

+ `clear`
+ `history`

## 命令行编辑

Bash使用了一个名为`Readline`的库(共享的线程集合, 可以被不同的程序使用), 来实现命令行编辑.

利用历史命令

```bash
history | grep /usr/bin
```

可以通过类似`!88`的形式, 引用历史命令.
