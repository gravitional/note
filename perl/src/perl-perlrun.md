# perl doc

## perlrun

### -C

+ `-C [*number/list*]`; `-C` 标志控制着 `Perl` 的一些 `Unicode` 功能.

从 `5.8.1` 开始, `-C` 后面可以跟一个数字或 `选项字母` 的列表.
这些字母, 它们的数值和效果如下; 列出多个字母等于将 `对应数字` 相加.

+ `I`;  `1`; `STDIN` 被假定为 `UTF-8` 的格式
+ `O`;  `2`  `STDOUT` 将采用 `UTF-8` 格式
+ `E`;  `4`  `STDERR` 将采用 `UTF-8` 格式
+ `S`;  `7`; `I + O + E`

+ `i`;  `8`  `UTF-8` 是 `输入流` 的默认  `PerlIO 层`  (default PerlIO layer for input streams)
+ `o`;  `16`; `UTF-8` 是 `输出流` 的默认 `PerlIO层`
+ `D`;  `24`; `i + o`
+ `A`;  `32`; `@ARGV` 元素被认为是用 `UTF-8` 编码的字符串

+ `L`; `64`; 通常情况下, `IOEioA` 是无条件的,
但 `L` 使其有条件地取决于 `locale` 环境变量(`LC_ALL`, `LC_CTYPE` 和 `LANG`, 优先级依次递减)
--如果这些变量显示为 `UTF-8`, 则选定的 `IOEioA` 生效

+ `a`; `256`; 设置 `${^UTF8CACHE}` 为 `-1`, 以在调试模式(debug)下运行 `UTF-8` 缓存代码(caching code).

例如, `-COE` 和 `-C6` 都会在 `STDOUT` 和 `STDERR` 上开启 `UTF-8`.
重复添加字母只是多余的, 不能累积也不能切换(not cumulative nor toggling).

`io 选项` 意味着在当前文件范围内, 任何后续的 `open()` (或类似的 `I/O` 操作)都将隐含地应用 `:utf8` PerlIO 层,
换句话说, 期望从任何输入流接收 `UTF-8`, 并向任何输出流推送 `UTF-8`.
这只是默认情况, 在 `open()` 和 `binmode()` 中使用明确的 `layers`, 可以像平常一样操作 `流`(stream).

`-C` 本身(后面没有任何 `数字` 或 `选项列表` ), 或 `PERL_UNICODE` 环境变量 为空字符串 `""`, 其效果与 `-CSDL` 相同.
换句话说, 标准的 `I/O` 句柄和默认的 `open()` 层是 `UTF-8` 化的, **除非** `locale` 环境变量表明是 `UTF-8` locale.
这种行为遵循了 `Perl 5.8.0` 中*隐含的*(也是有问题的)`UTF-8`行为.
(参见 `perl581delta` 中的 `UTF-8 no longer default under UTF-8 locales`).

你可以使用 `-C0`(或将 `PERL_UNICODE` 设置为 `0`)来明确地禁用上述所有 `Unicode` 功能.

只读的 魔法变量 `${^UNICODE}` 反映此 `设置` 的 `数值`.
这个变量在 `Perl` 启动时被设置, 此后是只读的.
如果你想获得运行时的效果, 可以使用三参数 `open()`(见 `perlfunc` 中的 `open`),
两参数 `binmode()`(见 `perlfunc` 中的 `binmode`), 以及 `open` pragma(见 `open`).

在 `5.8.1` 之前的 `Perl` 中, `-C` 开关是 `Win32` 专用的开关, 可以使用 `Unicode-aware`的 "wide system call" Win32 APIs.
然而, 这个功能实际上没有被使用, 因此这个命令行开关被 "回收"(recycled)了.

注意: 从 `perl 5.10.1` 开始, 如果在 `#!` 行中使用 `-C` 选项, 也必须在命令行中指定 `-C` 选项,
因为在 `perl解释器` 的执行过程中, 此时 `标准流` 已经建立好了.
你也可以使用 `binmode()` 来设置 `I/O` 流的编码.
(Note: Since perl 5.10.1, if the `-C` option is used on the `"#!"` line, it must be specified on the command line as well,
since the `standard streams` are already set up at this point in the execution of the `perl` interpreter)

### -c

`-c` 使 `Perl` 检查程序的语法, 然后退出而不执行它.
实际上, 它 **会** 执行任何 `BEGIN`, `UNITCHECK` 或 `CHECK` 块以及任何 `use` 语句: 这些被认为是 `执行你的程序` 之外的事.
然而, `INIT` 和 `END` 块将被跳过.
