# Perl文件句柄的一些高级话题

open函数除了> >> <这三种最基本的文件句柄模式, 还支持更丰富的操作模式, 例如管道.
其实bashshell支持的重定向模式, perl都支持, 即使是2>&1这种高级重定向模式, perl也有对应的模式.

## 打开管道文件句柄

Perl程序内部也支持管道, 以便和外部命令进行交互. 例如, 将perl的输传递给cat命令, 或者将cat命
令执行结果输出给perl程序内部. 所以, perl有2种管道句柄模式: 句柄到管道, 管道到句柄.

例如, 将perl print语句的输出, 交给cat -n命令来输出行号:

open my $fh, "| cat -n"
  or die "Can't open: $!";

say {$fh} "hello world"; say {$fh} "HELLO WORLD";

再例如, 将cat -n命令的执行结果通过管道交给perl文件句柄:

open my $fh, "cat -n a.log |"
  or die "Can't open: $!";

while(<$fh>){ print "from pipe: $_"; }

虽然只有两种管道模式, 但有3种写法:

    命令(管道)输出到文件句柄模式: -|
    文件句柄输出到命令(管道)模式: |-
    |写在左边, 表示句柄到命令(管道), 等价于|-, |写在右边, 等价于命令(管道)到句柄, 等价于-|, 可以认为-代表的就是外部命令

上面第三点|的写法见上面的例子便可理解. 而|-和-|是作为open函数的模式参数的, 以下几种写法是
等价的:

```perl
open my $fh, "|tr '[a-z]' '[A-Z]'";
open my $fh, "|-", "tr '[a-z]' '[A-Z]'";
open my $fh, "|-", "tr", '[a-z]', '[A-Z]';

open my $fh, "cat -n '$file'|";
open my $fh, "-|", "cat -n '$file'";
open my $fh, "-|", "cat", "-n", $file;
```

管道还可以继续传递给管道:

```perl
open my $fh, "|tr '[a-z]' '[A-Z]' | cat -n";
```

## 以读写模式打开

默认情况下:

+ 以 `>` 模式打开文件时, 会先截断文件, 也就是说无法从此文件句柄关联的文件中读取原有数据, 且还会清空原有数据
+ 以 `>>` 模式打开文件时, 会将指针指向文件末尾以便追加数据, 但无法读取该文件数据, 因为数据在偏移指针的前面

如何以 `既可写又可读` 的模式打开文件句柄?在Perl中可以在模式前使用 `+` 符号来实现.

结合+的模式有3种, 都用来实现读写更新操作. 含义如下:

+ `<`: read-update, 如open FH, "+<$file"提供读写行为.
    如果文件不存在, 则open失败(以read为主, 写为辅), 如果文件存在, 则文件内容保留,
    但IO的指针放在文件开头, 也就是说无论读写操作, 都从开头开始, 写操作会从指针位置开始覆盖相同字节数量的数据
+ `>`: write-update, 如open FH, "+>$file"提供读写行为. 如果文件不存在, 则创建文件(以write为主, read为辅).
    如果文件存在, 则截断文件, 因此这种方式是先将文件清空然后写数据
+ `>>`: append-update, 如open FH, "+>>$file"提供读写行为.
    如果文件不存在, 则创建(以append为主, read为辅), 如果文件存在, 则将IO指针放到文件尾部

一般来说, 要同时提供读写操作, +<是最可能需要的模式. 另外两种模式, 如果要读取数据, 需要在执
行读取操作之前, 先将文件偏移指针跳转(使用seek函数)到某个字节位置处, 再从指针位置处开始向后
读取.

例如, 使用+<打开可供读, 写, 更新的文件句柄, 但不截断文件.

open my $fh, "+<", "/tmp/test.log" or die "Couldn't open file: $!";

## open打开STDOUT和STDIN

如果想要打开标准输入, 标准输出, 那么可以使用二参数格式的open, 并将-指定为文件名. 例如:

open LOG, "-";   # 打开标准输入open LOG, "<-";  # 打开标准输入open LOG, ">-";  # 打开标准
输出

没有类似的直接打开标准错误输出的方式. 如果有一个文件名就是-, 这时想要打开这个文件而不是标
准输入或标准输出, 那么需要将-文件名作为open的第三个参数.

open LOG, "<", "-";

## 创建临时文件

如果将open()函数打开文件句柄时的文件名指定为undef, 表示创建一个匿名文件句柄, 即临时文件.
这个临时文件将创建在/tmp目录下, 创建完成后将立即被删除, 但是perl进程会持有这个临时文件对应
的文件句柄直到文件句柄关闭. 这样, 这个文件就成了看不到却仍被进程使用的临时文件.

什么时候才能用上打开就立即删除的临时文件?只读或只写的临时文件都是没有意义的, 只有同时能读
写的文件句柄才是有意义的, 所以open的模式需要指定为+<或+>.

例如:

```perl
#!/usr/bin/perl use strict; use warnings; use 5.010;

# 创建临时文件
open my $tmp_file, '+<', undef or die "open filed: $!";

# 设置自动flush
select $tmp_file; $| = 1;;

# 这个临时文件已经被删除了
system("lsof -n -p $$ | grep 'deleted'");

# 写入一点数据
say {$tmp_file} "Hello World1"; say {$tmp_file} "Hello World2"; say {$tmp_file} "Hello
World3"; say {$tmp_file} "Hello World4";

# 指针移动到临时文件的头部来读取数据
seek($tmp_file, 0, 0);

select STDOUT; while(<$tmp_file>){ print "Reading from tmpfile: $_"; }
```

执行结果:

```out
perl  22685 root  3u  REG  0,2  0 108086391056997277 /tmp/PerlIO_JHnTx1 (deleted)
Reading from tmpfile: Hello World1
Reading from tmpfile: Hello World2
Reading from tmpfile: Hello World3
Reading from tmpfile: Hello World4
```

## 内存文件

如果将open()函数打开文件句柄时的文件名参数指定为一个标量变量的引用, 也就是不再读写具体的文
件, 而是读写内存中的变量, 这样就实现了一个内存IO的模式.

```perl
#!/usr/bin/perl

my $text = "Hello World1\nHello World2\n";

# 打开内存文件以便读取操作
open my $mem_file, "<", \$text or die "open failed: $!";

print scalar <mem_file>;

# 提供内存文件以供写入操作
$log = "" open mem_file, ">", \$log; print mem_file "abcdefg\n"; print mem_file
"ABCDEFG\n";

print $log;
```

如果内存文件操作的是STDOUT和STDERR这两个特殊的文件句柄, 如果需要重新打开它们, 一定要先关闭
它们再重新打开, 因为内存文件不依赖于文件描述符, 再次打开文件句柄不会覆盖文件句柄. 例如:

```perl
close STDOUT; open(STDOUT, ">", \$variable) or die "Can't open: $!";
```

现在, 写向标准输入的数据, 都将保存到变量variable中.  Perl的高级重定向

在shell中可以通过>&和<&实现文件描述符的复制(duplicate)从而实现更高级的重定向. 在Perl中也同
样能实现, 符号也一样, 只不过复制对象是文件句柄.
例如:

```perl
open $fh, ">&STDOUT" open $fh, ">&", "STDOUT" open $fh, ">&", "\*STDOUT"
```

都表示将写入$fh文件句柄的数据重定向到STDOUT所对应的目标文件(可能是普通文件, 也可能是终端,
管道等)中.

注意第三种写法, 不能省略\*STDOUT前面的\*, 这是一种比较古老且不推荐的写法, 但目前还存在, 且
有时候必须使用. 简单解释一下\*STDOUT的含义, 裸字文件句柄(如STDOUT/STDIN)是一种globs符号
(symbol), 它相当于是一种字符串, 和不带引号的裸字符串的含义是类似的. 在裸字串前面加上*表示
从perl内部的符号表中通配搜索名为STDOUT的符号, 如果能搜索成功, 那么*BAREWORD代表的就是一种
值, 它所保存的值, 此时*BAREWORD这种写法相当于一种是引用变量的写法$var. 前面加上\表示这种特
殊变量的引用, 此时它和前面示例的\$fh是同一种概念.

    注: 复制文件句柄时, 默认会在内部复制文件描述符, 并让新的文件句柄关联这个新的文件描述符.
    如果只是单纯的想复制文件句柄而不复制底层的文件描述符, 使用>&=语法. 如

    ```perl
    open(ALIAS, ">&=HANDLE");
    open ALIAS, ">&=", fileno HANDLE;
    ```

shell中很常用的一个用法是 `&>FILENAME` 或 `>FILENAME 2>&1`,
它们都表示标准错误和标准输出都输出到 `FILENAME中`.
在Perl中实现这种功能的方式为: (注意dup目标使用 `\*` 的方式, 且不加引号)

```perl
open my $fh, ">", "/dev/null" or die "Can't open: $!";
open STDOUT, ">&", \$fh or die "Can't dup:$!";
open STDERR, ">&", \$fh or die "Can't dup: $!";
```

或者简写一下:

```perl
open STDOUT, ">", "/dev/null" or die "Can't dup:$!";
open STDERR, ">&STDOUT" or die "Can't dup: $!";
```

测试下:

```perl
open my $fh, ">", "/tmp/a.log" or die "Can't open: $!";
open STDOUT, ">&", $fh or die "Can't dup LOG:$!";
open STDERR, ">&", $fh or die "Can't dup STDOUT: $!";

say "hello world stdout default";
say STDOUT "hello world stdout";
say STDERR "hello world stderr";
```

会发现所有到STDOUT和STDERR的内容都追加到/tmp/a.log文件中.

如果在同一perl程序中, STDOUT和STDERR有多个输出方向, 那么dup这两个文件句柄之前, 需要先将它
们保存起来. 需要的时候再还原回来:

```perl
# 保存STDOUT和STDERR到$oldout和$olderr
open(my $oldout, ">&STDOUT") or die "Can't dup STDOUT: $!";
open(my $olderr, ">&STDERR") or die "Can't dup STDERR: $!";

# 实现标准错误, 标准输出都重定向到foo.out的功能, 即"&>foo.out"
open(STDOUT, '>', "foo.out") or die "Can't redirect STDOUT: $!";
open(STDERR, ">&STDOUT")     or die "Can't dup STDOUT: $!";

# 还原回STDOUT和STDERR
open(STDOUT, ">&", $oldout) or die "Can't dup \$oldout: $!";
open(STDERR, ">&", $olderr) or die "Can't dup \$olderr: $!";
```
