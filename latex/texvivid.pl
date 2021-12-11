#!/usr/bin/env perl
use strict;
use warnings;
use 5.030;

my ( $texPath, $texEngine ) = @_;

if ($texEngine) {
    $texEngine = "xelatex";
}

# format
# print with color
sub echo2 {
    my $delimiter1 = "-------------------------------------------------------";
    my $delimiter2 = "-------------------------------------------------------";
    print "${delimiter1}\n\033[1;44m\033[1;37m@_\033[0;0m\n${delimiter2}";
}

# print current directory
echo2 "current directory is: ", system("pwd");

# tex main file default name: main.tex
my $texUsual = "main.tex";
echo2 "texUsual name is: $texUsual";

# current tex file list
my @texArr = system
"find . -mindepth 1 -maxdepth 1 -type f -iname '*.tex' -print0 | xargs --null echo";
my @texHere;
if ( $#texArr > 4 ) {
    @texHere = @texArr[ 0 .. 4 ];
    echo2 "The first 5 .tex files here are:";
}
elsif ( $#texArr == 0 ) {
    echo2 "there is no .tex file";

}
else {
    @texHere = @texArr;
    echo2 @texHere;
}
