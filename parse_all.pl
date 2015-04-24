#!/usr/bin/perl -w

use strict;
use warnings;
use Cwd;

my $num_args = $#ARGV + 1;

if($num_args != 2){
  print "\nUsage: parse_all.pl <directory> <csv_file>\n";
  exit;
}

my $dir = $ARGV[0];
my $csv_file = $ARGV[1];

opendir(DIR, $dir) || die "Can't open directory $dir: $!";
my @files = sort {$a cmp $b} readdir(DIR);
closedir(DIR);

my $pwd = cwd();

# remove "." and ".." from @files
shift @files;
shift @files;

foreach (@files) {
  `$pwd/parse_single_report.pl $pwd/$dir/$_ $csv_file`;

  print "Parse failed: $!\n" if ($? == -1);
}

