#!/bin/bash

str1=""
str2=""
read str3
if [ "$str3" = "q" ] 
then
    echo "Not a single line was entered."
    exit
fi
str2=$str3
while [[ "$str1" != "q" ]]; do
      str2="$str2$str1"
      read str1
done

echo "$str2"     

