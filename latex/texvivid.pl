#!/usr/bin/env perl
use 5.030;
use strict;
use warnings;
use IPC::System::Simple qw(system systemx capture capturex);
use Options;
use autodie;

# format: print with color
sub echo2 {
    my $delimiter1 = "-------------------------------------------------------";
    my $delimiter2 = "-------------------------------------------------------";
    print "${delimiter1}\n\033[1;44m\033[1;37m@_\033[0;0m\n${delimiter2}";
}

# parse commandline parameters
my $options = new Options(
    params => [
        [ 'files',  'f', (""), 'The latex file to process.' ],
        [ 'engine', 'e', '-xelatex', 'The latex engine used to compile.' ]
    ],
    flags => [ [ 'help', 'h', 'Display this usage guide.' ], ]
);

# parse default options source (@ARGV)
my %results = $options->get_options();

# provide usage, for get help
if ( $options->get_result('help') ) {
    $options->print_usage();
    exit(1);
}

my $engine  = $results{'engine'};
my @texPath = $options->get_result('files');

# echo2 "texPath: @texPath, length, @texPath", "empty?", @texPath == ();

# print current directory
echo2 "current directory is: ", capture("pwd");

# tex main file default name: main.tex
# my $texUsual = "main.tex";
# echo2 "texUsual name is: $texUsual";

# current tex file list
my @texHere = capture(
"find . -mindepth 1 -maxdepth 1 -type f -iname '*.tex' -print0 | xargs --null echo"
);
echo2 "texHere: @texHere";

my @texArr;

if ( $texPath[0] ne '' ) {
    @texArr = @texPath;
}
elsif ( @texHere > 4 ) {
    @texArr = @texHere[ 0 .. 4 ];
    echo2 "The first 5 .tex files here are:";
}
elsif ( @texHere == 0 ) {
    echo2 "there is no .tex file";
}
else {
    @texArr = @texHere;
}

# echo2 "texArr: @texArr";

foreach (@texArr) {

    # tex processing now
    echo2 "the tex processed is $_";
    chomp( my $tBase = capture("basename -s .tex $_") );
    echo2 "tBase: $tBase";

    # build *.tex one by one, with latexmk
    capture(
"latexmk $engine -cd -pv -view=pdf -bibtex  -recorder -file-line-error -interaction=nonstopmode -synctex=1 $tBase"
    );

    # my $log_name = $tBase . ".log";
    # open my $log_file, '<:encoding(UTF-8)', $log_name;
    # ## dump error message
    # echo2 'echo message';
    # print( grep { /\[\d+\]/i } <$log_file> );

   # dump warnings
   # grep -m 10 -Pi -n --color -B 0 -A 8 "\[\d+\]" "$tBase.log";
   # # dump errors, many forms
   # echo2 'echo errors 1';
   # grep -Pi -m 15 -n --color -B 1 -A 6 -e "! LaTeX Error:" "$tBase.log";
   # echo2 'echo error 2';
   # grep -Pi -m 15 -n --color -B 1 -A 6 -e "\? x" "$tBase.log";
   # echo2 'echo errors 3';
   # grep -Pi -m 15 -n --color -B 0 -A 6 -e "\.tex:\d+:" "$tBase.log";
   # # echo2 'echo errors 4'
   # # grep -Pi -m 15 -n --color -B 0 -A 6 -e "! Package tikz Error:" "${t}.log"
   # # the position of errors, like l.123
   # echo2 'the error line postion';
   # grep -Pi -m 15 -n --color -B 1 -A 1 "l\.\d+" "$tBase.log";
}
