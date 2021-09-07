#!/usr/bin/env perl
use strict;
use warnings;
##****************************************

my $string = "a tonne of feathers or a tonne of bricks";
while($string =~ m/(\w+)/g) {
  print "'".$1."'\n";
}