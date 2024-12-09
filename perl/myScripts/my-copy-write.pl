#! /usr/bin/perl -w
use 5.032;
use strict;
use warnings;
use Cwd        qw(cwd abs_path);
use File::Path qw(make_path);
use File::HomeDir;
use File::Spec;
use File::Copy::Recursive qw(rcopy rmove rcopy_glob rmove_glob);

# https://perl-book.junmajinlong.com/ch10/3_file_copy_move.html
# https://perldoc.perl.org/File::Glob
#---- 支持通配的拷贝和移动，采用File::Glob::bsd_glob()的通配规则
# perltidy -nst -b -bext='/' -gnu -pbp -i=2 -ci=2 -ce -nbl -bt=2 -sbt=2 -pt=2 -nsfs -nsak=s -dws
my $home   = File::HomeDir->my_home;
my $source = File::Spec->catfile('c:',    'sim-solver', 'Release');
my $target = File::Spec->catfile("$home", 'test',       'dll-test');
say($source);
say($target);

# 创建 target 目录
make_path "$target";
rcopy_glob("${source}/*.{dll,exe}", "$target") or die $!;
if ($@) {
  warn "Caught exception: $@";
}
