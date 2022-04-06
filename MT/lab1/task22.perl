use strict;
use warnings;
my $ind = 0;
my $cnt = 0;
while (<>) {
    if(/^\s+?$/ && $ind == 0){
        s/\s+?//g ;
    } else{
        if(/\S/ && $cnt > 0){
            $cnt = 0;
            print"\n"

        }
        $ind = 1;
        if(/^\s+?\S.*$/){
            s/\s*//;
            s/\s\s*\s/ /g;
        }
        if(/^.*\S\s+$/){
            s/\s+\Z/\n/;
            s/\s\s*\s/ /g;
        }
        if(/^\s+?$/ && $cnt == 0){
            $cnt++;
        }
        if(/^\s+?$/ && $cnt > 0){
            $cnt++;
            s/\s*//g;
        }


    }
    print;
}
