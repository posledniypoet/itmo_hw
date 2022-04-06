use strict;
use warnings;
while (<>) {
    s/a.*?aa.*?aa.*?a/bad/g;
    print ;
}
