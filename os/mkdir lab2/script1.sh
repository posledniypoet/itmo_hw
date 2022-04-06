#!/bin/bash

proc=$(ps -U alexander -o pid,command | tail -n +2 | sed -r "s/\s*([^ ]+)\s(.*)/\1:\2/")
lines_cnt=$(echo "$proc"|wc -l)
echo "Total process count: $lines_cnt" > 2_1.txt
echo "$proc" >> 2_1.txt
