use strict;
use warnings;
while (<>) {
    print if /^.*\([^\(\)]*\w+[^\(\)]*\).*$/ ;
}