use strict;
use warnings;
use File::Temp qw(tempfile);
use IO::Handle;

# Create a temporary file
my ($fh, $filename) = tempfile(SUFFIX => '.h', UNLINK => 1);
print $fh <<EOF;
#define Py_LIMITED_API 0x030B0000
#define PY_SSIZE_T_CLEAN
#include <Python.h>
EOF
$fh->flush;

my $python_includes = `python-config --includes`;
chomp $python_includes;

local $ENV{CC} //= 'cc';
open my $fhcc, "cc $python_includes -E $filename |"
  or die "Could not open $filename: $!";

my $is_system_inc = 0;
while (my $line = <$fhcc>) {
  chomp $line;

  if ($line =~ /# \d+ "(.*)"(( \d)*)/) {
    my @options = split ' ', $2;
    if (grep { $_ eq 3 } @options) {
      $is_system_inc = 1;
    } else {
      $is_system_inc = 0;
    }
    next;
  }
  unless ($is_system_inc) {
    print "$line\n";
  }
}
