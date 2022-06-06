# perl 脚本例子

## 合并子文件夹

```perl
#! /usr/bin/perl -w
use 5.032;
use File::Find;
use File::Spec;
use File::Basename;
use Term::ANSIColor;

my ( @dir_paths, @old_paths, @new_paths );

# 先找出所有的目录, 不包含 "."
my @all_files = glob '*';
for (@all_files) {
    push @dir_paths, $_ if ( -d $_ );
}
say colored( ['bold blue'], "The sub directories are:\n" ), "@dir_paths\n";

# 重命名文件, 移动到上层
sub wanted {

    # 测试是否存在
    return unless -f;

    # 文件的旧路径
    my $basename = basename $File::Find::name;
    push @old_paths, $File::Find::name;

    # 文件的新路径
    my $new_path = $File::Find::dir . "_" . $basename;
    push @new_paths, $new_path;

    # rename path: old  to new
    if ( rename $File::Find::name => $new_path ) {

        #success, do nothing
    }
    else {
        warn "rename $File::Find::name to $new_path failed: $!\n";
    }
}

# 调用 find 函数, 不 cd 到子目录下，此时 $_ == $File::Find::name
find( { no_chdir => 1, wanted => \&wanted }, @dir_paths );

# print the change
say colored( ['bold blue'], "The old paths are:\n" ), "@old_paths";
say colored( ['bold blue'], "The new paths are:\n" ), "@new_paths";

# 删除空文件夹
foreach my $dir (@dir_paths) {
    rmdir $dir or warn "cannot rmdir $dir: $!\n";
}
```
