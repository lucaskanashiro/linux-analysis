#!/usr/bin/perl -w

use strict;
use warnings;

my $num_args = $#ARGV + 1;

if($num_args != 2){
  print "\nUsage: parse_single_report.pl <cppcheck_report> <CSV_file>\n";
  exit;
}

my $file = $ARGV[0];
my $csv_file = $ARGV[1];
my $handler;
my $writer;

open($handler, '<', $file) or die $!;
open($writer, '>>', $csv_file) or die $!;

my $cwe476=0;
my $cwe457=0;
my $cwe401=0;
my $version;

while(<$handler>) {
  my ($file, $line, $error);
  if ( $_ =~ m/\[(\S+):(\d*)\]:\s*\(error\)\s*(.*):/ ) {
    $file = $1;
    $line = $2;
    $error = $3;

    $version = $1 if ($file =~ m/([^\/]+)/);

    $cwe476++ if ($error eq 'Possible null pointer dereference');
    $cwe457++ if ($error eq 'Uninitialized variable');
    $cwe401++ if ($error eq 'Memory leak');
  }
}

print $writer "Version,CWE476,CWE457,CWE401\n" if (-z $csv_file);
print $writer $version.",".$cwe476.",".$cwe457.",".$cwe401."\n";

close($handler);
close($writer);

