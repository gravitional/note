# open打开文件

Perl使用open函数打开文件并指定稍后要操作该文件所对应的文件句柄. open函数的用法较为复杂, 通
常采用以下两种形式:

```perl
open FILEHANDLE,EXPR       # 两参数的open
open FILEHANDLE,MODE,FILE  # 三参数的open
```

两参数的open是比较古老的用法, 它更简洁但不安全(比如可以注入删除所有文件的命令), 三参数的
open是推荐的用法. 对于个人写的Perl脚本而言, 如果能够确保自己知道不同用法的行为, 那么可使用
任何一种形式.

例如, 打开/tmp/a.log文件以备后续读取其中内容:

```perl
open my $fh, "</tmp/a.log";     # 两参数形式
open my $fh, "<", "/tmp/a.log"; # 三参数形式
```

open的第一个参数my $fh表示声明一个标量变量作为文件句柄, 如果之前已经声明好变量, 则可以省略
my关键字.

<表示以只读方式打开文件/tmp/a.log, 这和Shell的输入重定向符号一样. 实际上, Perl还使用>表示
以覆盖写方式打开文件, >>表示以追加写方式打开文件, 这都和Shell中的重定向语法一致.

```perl
# 覆盖写方式打开/tmp/x.log: 将先清空文件
open my $fh, ">", "/tmp/x.log";  # 三参数形式open my $fh, ">/tmp/x.log";      # 两参数形式

# 追加写方式打开/tmp/y.log
open my $fh, ">>", "/tmp/y.log"; open my $fh, ">>/tmp/y.log";
```

< > >>等符号表示打开文件的模式: 以只读, 覆盖写或者追加写模式打开文件. 如果省略打开模式, 则
默认以只读方式打开文件. 因此, 下面两种写法等价:

open my $fh, "<", "/tmp/a.log"; open my $fh, "/tmp/a.log";

打开文件有时候会失败, 比如文件不存在, 没有权限打开文件, 打开文件的数量达到了上限, 等等. 当
open打开文件失败时, 将返回布尔假值, 同时将操作系统的报错信息设置到特殊变量$!中. 很多时候,
open会结合or die一起使用, 表示打开文件失败时直接报错退出:

```perl
open my $fh, "<", "/tmp/a.log" or die "open file failed: $!";

# 或者换行书写:
open my $fh, "<", "/tmp/a.log" or die "open file failed: $!";
```

另外需要注意, 无论是(类)Unix系统还是Windows系统, 文件路径都使用/作为路径分隔符. 例如, 打开
Windows系统C分区路径下的一个文件:

open my $fh, "<", "C:/a/b/c.txt";

操作文件句柄后, 如果可以确定不再使用该文件句柄来读写数据, 则需要使用close函数关闭文件句柄.
这样可以尽快回收资源, 否则如果存在大量未关闭的文件句柄, 可能会达到操作系统设置的文件打开数
量上限.

open my $fh, "<", "/tmp/a.log" or die "can't open file: $!"; ... close $fh or die "close
failed";

幸运的是, Perl会在离开作用域时自动回收文件句柄, 这使得写Perl代码时可以不用太过于关注关闭文
件句柄这件事情. 更详细的细节将在后面的章节展开描述.

```perl
{
  open my $fh, "<", "/tmp/a.log" or die "can't open file: $!";
  ...
}  # 离开作用域时自动关闭文件句柄$fh
```

## 非变量形式的文件句柄

除了标量变量形式的文件句柄, Perl还允许使用大写的裸字(bareword)文件句柄, 这种文件句柄是比较
古老的用法, 目前仍然支持, 且有一部分预定义的文件句柄仍然采用这种形式的文件句柄, 比如名为
DATA的文件句柄.
例如:

```perl
open FH, "<", "/tmp/a.log";
```

这表示打开/tmp/a.log文件, 并指定名为FH的文件句柄与之关联.

在某些场景下, 这种写法比较简洁, 只要看到是大写的裸字, 基本上可以推断它是一个文件句柄. 但这
种用法有时候不安全, 因为这种裸字文件句柄是全局的, 退出作用域后Perl不会自动关闭回收这种文件
句柄, 而且可能会影响程序的其他地方或受其他地方的影响.

例如:

```perl
{
  open FH, "/tmp/a.log" or die;
}  # 退出作用域后, 文件句柄仍然可用
```

## 理解文件IO的读写偏移指针

当在Perl中使用open函数打开文件时, Perl会请求操作系统的open系统调用, 由操作系统的open系统调
用来打开文件, 操作系统会为本次open操作分配一个文件描述符并反馈给perl. 在Perl层次上, perl会
维护指定的文件句柄与该文件描述符的关系.

操作系统打开文件时, 操作系统还会为本次open操作维护一个文件的IO偏移指针, IO偏移指针决定了下
次读, 写操作从哪个位置开始. 例如, 某次操作后IO偏移指针的位置在第100个字节处(第99字节后第
100字节前的中间位置), 那么下一次读取或写入操作将从第100个字节处开始读取, 如果读取或写入了
10个字节, 则更新偏移指针到109字节位置处, 再下次的读取或写入将从第109字节位置处开始.

需要记住的是, 以不同方式打开文件时, IO偏移指针初始放置的位置不同:

+ 以只读方式打开文件时, 偏移指针初始时被放置在文件开头, 即第1个字节前, 使得下次读取操作可以从第一个字节开始读取
+ 以覆盖写方式打开文件时, 会先清空文件, 并将偏移指针放置在文件开头, 使得下次写入操作会从第一个字节处开始写入
+ 以追加写方式打开文件时, 不会清空文件, 而是直接将偏移指针放置在文件的尾部, 即最后一个字节之后, 使得下次写入操作会将数据追加在文件的最尾部

需要注意, 每一次open操作都有属于自己的IO偏移指针, open同一个文件两次, 操作系统会分别为这两
次open维护两个IO偏移指针, 这两个偏移指针互不影响. 比如在Perl中读写文件句柄1, 只会更新文件
句柄1对应的IO偏移指针, 不会影响文件句柄2对应的IO偏移指针.

但是, 打开同一个文件多次并通过多个文件句柄操作同一个文件时, 可能会让读写的数据变得混乱. 例
如, 两次以追加写方式打开文件, 初始时它们的文件偏移指针都在文件的尾部, 比如在100字节位置处,
当通过文件句柄1写入10字节数据, 这只会更新文件句柄1对应的偏移指针到109字节处, 文件句柄2对应
的偏移指针仍放置在100字节处, 如果此时通过文件句柄2写入8个字节, 之前写入的10字节数据的前8个
字节将被覆盖.
