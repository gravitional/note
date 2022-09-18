# 目录句柄

目录句柄和文件句柄类似, 可以打开它, 并读取或迭代目录中的文件列表. 只不过需要使用:

+ opendir替换open函数
+ 使用readdir来替换readline, readdir在列表上下文返回一个文件列表, 在标量上下文则迭代读取下一个文件名
    + readdir不会递归到子目录中
    + readdir取得的文件名不包含父目录自身, 只包含它所读取目录内的文件名部分
+ 使用closedir来替代close
+ 注意每个Unix的目录下都包含两个特殊目录.和..

注: 除了打开目录句柄读取目录中的文件名外, 还可以通过通配模式来匹配或迭代文件.

例如:

```perl
$dir = "/usr/local/java";
opendir JAVAHOME, $dir
    or die "Can't open dir: $!";

foreach $file (readdir JAVAHOME){
    say "filepath: $dir/$file";
}
closedir JAVAHOME;
```

从Perl v5.012版本开始, 可以直接在while循环中使用readdir. 例如:

```perl
use 5.012;

$dir = "/usr/local/java";
opendir(my $dh, $dir) || die "Can't open $dir: $!";
while (readdir $dh) {
  say "filepath: $dir/$_";
}
closedir $dh;
```

如果想要跳过 `.` 和 `..` 这两个特殊的目录, 需要加上判断:

```perl
use 5.012;

$dir = "/usr/local/java";
opendir(my $dh, $dir) || die "Can't open $dir: $!";
while (readdir $dh) {
  next if $_ =~ /^\.{1,2}$/;
  say "filepath: $dir/$_";
}
closedir $dh;
```
