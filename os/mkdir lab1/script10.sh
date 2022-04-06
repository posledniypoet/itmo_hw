#!/bin/bash
man bash | sed 's/ /\n/g' | tr "[:upper:]" "[:lower:]" | sort | uniq -c | sort -n -r | awk 'length($2) >= 4 {print $2}' | head -3
