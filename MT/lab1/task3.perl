use strict;
use warnings;
while (<>) {
    print if /^(.*cat.*)|(.*caT.*)|(.*cAt.*)|(.*cAT.*)|(.*Cat.*)|(.*CaT.*)|(.*CAt.*)|(.*CAT.*)$/ ;
}