#!/usr/bin/env perl
use 5.030;
use strict;
use warnings;
use IPC::System::Simple qw(system systemx capture capturex);
use Options;

my $options = new Options(
    params => [
        [ 'files',  'f', "",        'The latex file to process.' ],
        [ 'engine', 'e', 'xelatex', 'The latex engine used to compile.' ]
    ],
);

# parse default options source (@ARGV)
my %results = $options->get_options();

# provide usage, for get help
if ( $options->get_result('help') ) {
    $options->print_usage();
    exit(1);
}

my $engine  = $results{ 'engine', 'files' };
my @texPath = $options->get_result('files');

# format: print with color
sub echo2 {
    my $delimiter1 = "-------------------------------------------------------";
    my $delimiter2 = "-------------------------------------------------------";
    print "${delimiter1}\n\033[1;44m\033[1;37m@_\033[0;0m\n${delimiter2}";
}

# print current directory
print "current directory is: ", capture("pwd");

# tex main file default name: main.tex
my $texUsual = "main.tex";
echo2 "texUsual name is: $texUsual";

# current tex file list
my @texArr = system(
"find . -mindepth 1 -maxdepth 1 -type f -iname '*.tex' -print0 | xargs --null echo"
);

my @texHere;

if ( @texPath > 0 ) {
    @texHere = @texPath;

}
elsif ( $#texArr > 4 ) {
    @texHere = @texArr[ 0 .. 4 ];
    echo2 "The first 5 .tex files here are:";
}
elsif ( $#texArr == 0 ) {
    echo2 "there is no .tex file";

}
else {
    @texHere = @texArr;
}
echo2 @texHere;

foreach (@texHere) {
    print $_;
}

foreach( @texHere) {
	# tex processing now
	echo2 "the tex processed is $_";
	my $tBase=system ("basename -s '.tex' $_");
	# build *.tex one by one, with latexmk
    system(	"latexmk -silent -$engine -pv -view=pdf -bibtex -cd -recorder -file-line-error -interaction=nonstopmode -synctex=1 $tBase" );
	## dump error message
	echo2 'echo message';
	# dump warnings
	grep -m 10 -Pi -n --color -B 0 -A 8 "\[\d+\]" "$tBase.log";
	# dump errors, many forms
	echo2 'echo errors 1';
	grep -Pi -m 15 -n --color -B 1 -A 6 -e "! LaTeX Error:" "$tBase.log";
	echo2 'echo error 2';
	grep -Pi -m 15 -n --color -B 1 -A 6 -e "\? x" "$tBase.log";
	echo2 'echo errors 3';
	grep -Pi -m 15 -n --color -B 0 -A 6 -e "\.tex:\d+:" "$tBase.log";
	# echo2 'echo errors 4'
	# grep -Pi -m 15 -n --color -B 0 -A 6 -e "! Package tikz Error:" "${t}.log"
	# the position of errors, like l.123
	echo2 'the error line postion';
	grep -Pi -m 15 -n --color -B 1 -A 1 "l\.\d+" "$tBase.log";
}
