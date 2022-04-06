use strict;
use warnings;
while (<>) {
    print if /^.*(z|x|y)\w{5,17}(z|x|y).*$/ ;
}