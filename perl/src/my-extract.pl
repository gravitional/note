#! /usr/bin/perl -w
use v5.32;
use File::Find;
use File::Spec;
use File::Basename;
use Term::ANSIColor;
use autodie;

# mesh.data 文件句柄
open my $mesh_fh, '<', 'mesh.dat';
#
my @tagList  = ();
my @tagStart = ();
my @parse    = ();
my %parse    = ();
my $term;
my $termOrd = 0;

say "let's find out the data!\n";
while (<$mesh_fh>) {
    given ($_) {
        when (/(\w+) +\{/) {    # 若匹配到 xxx {, 例如 node {
            $term = $1 . ".$termOrd";    # $1: 匹配到的正则分组1, 例如 node
            ++$termOrd;
            say "find: `$term' @ $.";    # $. 表示读取到的文件中的行号
            push @tagList,  $term;       # 记录起始标志node, 压入列表顶端
            push @tagStart, $.;          # 记录起始行号
        }
        when (/\}/) {                    # 若匹配到结束符 }
            say "find: `}'  that end `$term' @ $.";

            # 制作映射表, {node => [开始行数，结束行数]}
            push @parse, ( pop @tagList, [ pop @tagStart, $. ] )
        }
    }
}
close $mesh_fh;
%parse = @parse;    # 转换成 hash, 类似 python dict

# foreach迭代遍历key, 打开 输出Handle
foreach my $k ( sort keys %parse ) {
    open my $data_fh, '>', "$k.txt";    #存储结果的 txt

    # 从 mesh.dat 中提取数据
    open my $mesh_fh, '<', 'mesh.dat';
    while (<$mesh_fh>) {
        if ( $. > $parse{$k}->[0] && $. < $parse{$k}->[1] ) {
            $_ =~ s#[\[\],]#  #g;    #删除多余的字符
            $_ =~ s#^\s+##g;         #删除多余的空白
            print {$data_fh} $_;     # 输出结果到对应的 txt
        }
    }
    close $mesh_fh;                  #关闭句柄
    close $data_fh;
}
