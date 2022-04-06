use strict;
use warnings;
while (<>) {
    print if /^.*\b(\w+)\1\b\s.*$/;
}