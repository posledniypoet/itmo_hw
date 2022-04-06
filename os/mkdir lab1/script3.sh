#!/bin/bash

echo "Choose action"
echo "1 = nano"
echo "2 = vim"
echo "3 = links"
echo "4 = exit"
read a
case $a in
1)
    nano
;;
2)
     vim
;;
3)
    links
;;
esac

if [[ a -ge 5 ]]
then
    echo "Incorrect menu option."
fi
