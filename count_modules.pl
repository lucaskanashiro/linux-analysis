#!/usr/bin/perl -w

use strict;
use warnings;
use Cwd;
use File::Find qw(finddepth);

my $num_args = $#ARGV + 1;

if($num_args != 2){
  print "\nUsage: count_modules.pl <directory> <csv_file>\n";
  exit;
}

my $dir = $ARGV[0];
my $csv_file = $ARGV[1];
my $writer;

my @files;
finddepth(sub {
  return if($_ eq '.' || $_ eq '..');
  push @files, $File::Find::name;
}, $dir);

my $pwd = cwd();

# remove "." and ".." from @files
shift @files;
shift @files;

my $modules=0;

foreach (@files) {
  $modules++ if ($_ =~ m/.*\.[c|cpp|h|hpp]/);
}

my $version = $1 if ($dir =~ m/([^\/]+)\/?$/);

open($writer, '>>', $csv_file) or die $!;
print $writer "Version,Modules\n" if (-z $csv_file);
print $writer $version.",".$modules."\n";
close($writer);

