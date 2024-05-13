# gdb

[你还在用GDB调试程序吗?](https://zhuanlan.zhihu.com/p/152274203)
[How to highlight and color gdb output during interactive debugging?](https://stackoverflow.com/questions/209534/how-to-highlight-and-color-gdb-output-during-interactive-debugging)

```bash
gdb test.exe -tui
```

## 命令行参数

`-pid number`; `-p number`
调试进程 ID 为 number 的程序.

`-symbols file`; `-s file`
仅从指定 file 文件中读取符号表.

`-q`; `-quiet`; `-silent`;
取消启动 GDB 调试器时打印的介绍信息和版权信息

`-cd directory`;
以 directory 作为启动 GDB 调试器的工作目录, 而非当前所在目录.

`--args 参数1 参数2...`; `--ar 参数1 参数2`
向可执行文件传递执行所需要的参数.

## gdb shortcut 短命令

`Enter` 重复上一个命令

`run`; `r`
`r 参数1 参数2...`; 给程序指定参数

`start`; 启动并暂停在 `main()` 的第一行
`start 参数1 参数2...`; 指定参数运行并暂停

break; `b`
continue; `c`
next; `n`
print; `p`
list; `l`
quit; `q`

detach; `det`; GDB 调试器和程序分离

`set args`; `set arg`; 设置程序的 cmd 参数
