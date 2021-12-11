#!/usr/bin/env perl
use strict;
use warnings;
use 5.030;
##****************************************

# my $count = 0;
# while ( $count < 10 ) {
#     $count += 2;
#     print "count is now $count \n";
# }

# my $n = 1;
# my $sum;
# while ( $n < 10 ) {
#     $sum += $n;
#     $n   += 2;
# }
# print "The total was $sum. \n";

# my $string .= "more test\n";
# print $string;

my $maximum = &max( 3, 5, 10, 4, 6 );

sub max {
    my ($max_so_far) = shift @_;
    foreach (@_) {
        if ( $_ > $max_so_far ) {
            $max_so_far = $_;
        }
    }
    $max_so_far;
}

print $maximum, "\n";

state @array = qw\a b c\;

print @array, "\n";
