#! /usr/bin/perl -w
use v5.32;
use File::Find;
use File::Spec;
use File::Basename;
use Term::ANSIColor;
use autodie;
use utf8;

open my $nameList, '<:encoding(UTF-8)', 'cpp.prn';

while (<$nameList>) {
    my @lines = split /<sep>/, $_;
    ( my $old_name = $lines[0] ) =~ s/ +$//;
    ( my $new_name = $lines[1] ) =~
s/[\\\/\(\)\[\]\(\)\<\> "\"\"<>[]·. , ,, \;;: ~～\:\=\-, \&\^\$\#\@\!!\??\*\+]/_/g;
    $new_name =~ s/^_+//;
    $new_name =~ s/$/.mp4/;
    chomp($new_name);

    #  防止覆盖.
    die "Oops! A file called '$new_name' already exists.\n" if -e $new_name;
    say "rename: $old_name ->  $new_name";

    rename( $old_name, $new_name )
      or warn "Can't rename '$old_name' to '$new_name': $!";
}

close $nameList;
