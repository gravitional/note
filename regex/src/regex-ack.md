# linxu-ack

[Linux ack命令使用详解 ](https://man.comptechs.cn/ack.html)

`ack` 官网: [ack is a grep-like source code search tool.](https://beyondgrep.com/)

## 安装

ubuntu下要安装`ack-grep`, 因为在`debian`系中, `ack`这个名字被其他的软件占用了.
ubuntu 20 可以直接安装

```re
sudo apt-get install ack
```

参数说明:  这些参数在linux上的使用频率是相当高的, 尤其是你用`vim`做为IDE的话

+ `-c` ; 统计
+ `-i` ; 不区分大小写
+ `-h` ; 不显示名称
+ `-l` ; 只显文件名
+ `-n` ; 加行号
+ `-v` ; 显示不匹配
+ `-r`; 递归搜索子目录
+ `-f`; 只打印将被搜索的文件, 而不实际进行任何搜索.  必须不指定 `PATTERN`, 否则它将被当作搜索的路径.

## 实例

在记忆的时候大体上可以分为这几个部分:

+ Searching 代码搜索
+ Search output 搜索结果处理
+ File presentation 文件展示
+ File finding 文件查找
+ File inclusion/exclusion 文件过滤

### grep常用操作

```re
ack -r 'hello_world' # 简单用法
ack '^hello_world' . # 简单正则
ls -l | ack '.py' # 管道用法
```

### Searching

简单的文本搜索, 默认是递归的.

```re
ack hello
ack -i hello
ack -v hello
ack -w hello
ack -Q 'hello*'
```

### 搜索结果,Search File

对搜索结果进行处理, 比如只显示一个文件的一个匹配项, 或者xxx

```re
ack --line=1       # 输出所有文件第二行
ack -l 'hello'     # 包含的文件名
ack -L 'print'     # 非包含文件名
```

### 文件展示,File presentation

输出的结果是以什么方式展示呢, 这个部分有几个参数可以练习下

```re
ack hello --pager='less -R'    # 以less形式展示
ack hello --noheading      # 不在头上显示文件
ack hello --nocolor        # 不对匹配字符着色
```

### 查找文件,File finding

没错, 它可以查找文件, 以省去你要不断的结合 `find` 和 `grep` 的麻烦, 虽然 `linux` 的思想是一个工具做好一件事.

```re
ack -f hello.py     # 查找全匹配文件
ack -g hello.py$    # 查找正则匹配文件
ack -g hello  --sort-files     # 查找然后排序
```

### 文件过滤,File Inclusion/Exclusion

如果你曾经在搜索项目源码是不小心命中日志中的某个关键字的话, 你会觉得这个有用.

```re
ack --python hello       # 查找所有python文件
ack -G hello.py$ hello   # 查找匹配正则的文件
```

## 定义你自己的类型

ack允许你在预定义的类型之外, 定义你自己的类型(types).
这是用 `命令行选项` 完成的, 最好放在一个 `.ackrc` 文件中--这样你就不必反复定义你的类型了.
在下面的例子中, 选项将总是显示在一个命令行上, 以便于复制和粘贴.

文件类型可以用 `--type=xxx` 选项来指定, 也可以把`文件类型`本身作为一个`选项`来指定.
例如, 如果你创建的文件类型是 `cobol`, 你可以指定 `--type=cobol` 或者直接指定 `--cobol`.
文件类型必须至少是两个字符的长度.  这就是为什么 `C` 语言是 `--cc`, `R` 语言是 `--rr`.

`ack --perl foo` 在所有 `perl` 文件中搜索 `foo`.
`ack --help-types` 告诉你, `perl` 文件是以 `.pl`, `.pm`, `.pod` 或 `.t` 结尾的文件.

那么, 如果你想在搜索 `--perl` 文件时也包括 `.xs` 文件呢?
`ack --type-add perl:ext:xs --perl foo` 可以达到效果.
`--type-add` 在现有的类型上添加额外的扩展.

如果你想定义一个新的类型, 或者完全重新定义一个现有的类型, 那么就使用 `--type-set`.
`ack --type-set eiffel:ext:e,eiffel` 定义类型 `eiffel` 包括扩展名为 `.e` 或 `.eiffel` 的文件.
所以要 搜索所有包含 `Bertrand` 的 `eiffel` 文件, 可以使用

    ack --type-set eiffel:ext:e,eiffel --eiffel Bertrand

像往常一样, 你也可以写 `--type=eiffel` 而不是 `--eiffel`. 也可以否定, 所以 `--noeiffel` 将所有 `eiffel` 文件排除在搜索之外.
重新定义也有效: `ack --type-set cc:ext:c,h`, 则 `.xs` 文件不再属于 `cc` 类型.

当在 `.ackrc` 文件中定义你自己的类型时, 你必须使用以下方法:

    --type-set=eiffel:ext:e,eiffel

或者在不同的行中写上

    --type-set
    eiffel:ext:e,eiffel

以下内容在 `.ackrc` 文件中不起作用.

    --type-set eiffel:ext:e,eiffel

为了查看所有当前定义的类型, 使用 `--help-types`, 例如: `ack --type-set backup:ext:bak --type-add perl:ext:perl --help-types`
除了基于`extension`的过滤之外, `ack` 还提供了额外的过滤类型.
通用语法是 `--type-set TYPE:FILTER:ARGS`; `ARGS` 取决于 `FILTER` 的值.

+ `is:FILENAME`; is 过滤器与目标文件名完全匹配. 它正好刚好一个参数, 即要匹配的文件名.
    例如:

        --type-set make:is:Makefile

+ `ext:EXTENSION[,EXTENSION2[,...]]`;
    `ext` 过滤器将目标文件的`扩展名`与一个`扩展名列表`相匹配.  扩展名不需要`前导点`.
    例子:

        --type-set perl:ext:pl,pm,t

+ `match:PATTERN`;
    `match`过滤器根据正则表达式匹配目标文件名.  正则表达式在搜索时是不区分大小写的.
    例如:

        --type-set make:match:/(gnu)?makefile/

+ `firstlinematch:PATTERN`;
    `firstlinematch` 根据`正则表达式` 匹配目标文件的第一行.  和 `match` 一样, 这里的正则表达式也不区分大小写.
    例子:

        --type-add perl:firstlinematch:/perl/

## 搜索和替换

>如果 `ack` 能进行搜索和替换, 那不是很好吗? (search & replace)
>不, `ack` 永远是只读的.  `Perl` 有一个很好的方法在文件中进行搜索和替换, 使用 `-i`, `-p` 和 `-n` 开关.

你当然可以使用 `ack` 来选择你要更新的文件.
例如, 要把所有 `PHP` 文件中的 `foo` 改为 `bar`, 你可以在 Unix shell 中这样做.

```bash
$ perl -i -p -e's/foo/bar/g' $(ack -f --php)
```

### perl -ipe

+ `-i[*extension*]`;

指定由 `<>` 结构处理的文件将被就地编辑.(in-place)
它通过重命名输入文件, 以`original name`打开输出文件,  并选择该输出文件作为`print()`语句的默认文件.
如果提供扩展名, 则用于修改旧文件的名称, 以用于备份, 并遵循下列规则:

如果没有提供`扩展名`, 并且你的系统支持,那么原始的`*file*` 将保持打开, 不带名字, 而输出则被重定向到`新文件`,
它的名字是原始的 `*filnemae*`. 当 `perl` 退出时, 无论是否干净, 原始的 `*file*` 都会被 unlinked.

如果扩展名不包含 `*`, 那么它将被作为后缀附加到当前文件名的末尾.
如果扩展名确实包含一个或多个 `*` 符, 那么每个 `*`都会被替换成当前文件名.
用Perl术语来说, 你可以把它看成是.

```perl
($backup = $extension) =~ s/\*/$file_name/g;
```

这允许你为备份文件添加`前缀`, 而不仅仅是`后缀`.
除了后缀之外, 还可以为备份文件添加前缀.

```bash
$ perl -pi'orig_*' -e 's/bar/baz/' fileA  # backup to 'orig_fileA'
```

甚至可以将原始文件的备份, 放到另一个`目录`中(前提是该目录已经存在).

```bash
$ perl -pi'old/*.orig' -e 's/bar/baz/' fileA # 备份到'old/fileA.orig' .
```

这些命令是等价的:

```bash
$ perl -pi -e 's/bar/baz/' fileA     # 覆盖当前文件
$ perl -pi'*' -e 's/bar/baz/' fileA # 覆盖当前文件

$ perl -pi'.orig' -e 's/bar/baz/' fileA # 备份到'fileA.orig'.
$ perl -pi'*.orig' -e 's/bar/baz/' fileA # 备份到'fileA.orig' .
```

在shell中, 说

```bash
$ perl -p -i.orig -e "s/foo/bar/; ... "
```

与使用该程序是一样的.

```perl
#!/usr/bin/perl -pi.orig
s/foo/bar/;
```

这相当于

```perl
#!/usr/bin/perl
$extension = '.orig';
LINE: while (<>) {
    if ($ARGV ne $oldargv) {
        if ($extension !~ /\*/) {
            $backup = $ARGV . $extension;
        }
        else {
            ($backup = $extension) =~ s/\*/$ARGV/g;
        }
        rename($ARGV, $backup);
        open(ARGVOUT, ">$ARGV");
        select(ARGVOUT);
        $oldargv = $ARGV;
    }
    s/foo/bar/;
}
continue {
    print;  # this prints to original filename
}
select(STDOUT);
```

`-i`形式除了不需要比较 `$ARGV` 和 `$oldargv` 来知道文件名何时改变.
然而, 它确实使用  `ARGVOUT` 用于选定的`filehandle`.
请注意, 在循环之后, `STDOUT` 会被恢复为默认的输出文件柄.

如上所示, `Perl` 创建了备份文件, 无论输出是否真的被改变. 所以这只是一种花哨的复制文件的方式 .

```bash
$ perl -p -i'/some/file/path/*' -e 1 file1 file2 file3...
# 或
$ perl -p -i'.orig' -e 1 file1 file2 file3...
```

你可以使用不带括号的 `eof` 来定位每个输入文件的结尾, 如果你想 `追加` 到每个文件中, 或者重置行数(见 `perlfunc` 中 `eof` 的例子).
如果对于一个给定的文件, `Perl` 无法按照扩展名中的规定创建备份文件, 那么它将跳过该文件而继续下一个文件(如果它存在的话).
关于文件权限和 `-i` 的问题的讨论,
见 "Why does Perl let me delete read-only files? Why does -i clobber protected files? Isn't this a bug in Perl?" in perlfaq5.

你不能用 `-i` 来创建目录, 或者 strip 文件拓展名.

`Perl` 在文件名中展开`~`, 这很好, 因为有些人用它来做备份文件.

```bash
$ perl -pi~ -e 's/foo/bar/' file1 file2 file3...
```

请注意, 因为 `-i` 在创建同名的新文件之前会重命名或删除原文件, Unix风格的 `软链接` 和 `硬链接` 不会被保留.

最后, 当命令行上没有给出文件时, `-i` 开关不会妨碍执行.
在这种情况下, 不做任何备份(当然, 不能确定原始文件), 处理过程从 `STDIN` 到 `STDOUT`, 正如预期的那样.

+ `-p`; 导致 Perl 对你的程序进行如下循环, 这使得它在`filename`参数上的迭代有点像 `*sed*`.

```perl
LINE:
  while (<>) {
      ...             # your program goes here
  } continue {
      print or die "-p destination: $!\n";
  }
```

如果`file` 由于某种原因不能被打开, Perl会警告你, 并转到下一个文件.
请注意, 这些行是自动打印的. 在打印过程中发生的错误会被视为`fatal`.
要抑制打印, 请使用 `-n` 开关. `-p` 覆盖 `-n` 开关.

`BEGIN` 和 `END` 块可以用来在隐式循环之前或之后捕获控制(capture control), 就像在 `*awk*` 中一样.

+ `-e *commandline*`; 可以用来输入一行程序. 
如果给出了 `-e`, Perl 将不会在`argument`列表中寻找`filename`. 
多个 `-e` 命令可以用来建立一个多行的脚本. 请务必按照正常程序的规则使用`分号`.
