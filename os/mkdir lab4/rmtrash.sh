#!/bin/bash

TRASHDIR=~/.trash
TRASHLOG=~/.trash.log
FILE="$PWD/$1"

LNAME=$(date +"%s")

if [[ $# != 1 ]]; then
	echo "Invalid count of arguments."
	exit 1
fi

if [[ ! -f $FILE ]]; then
	echo "Invalid filename."
	exit 1
fi

if [[ ! -d $TRASHDIR ]]; then
	mkdir $TRASHDIR
	touch $TRASHLOG
fi

ln "$FILE" "$TRASHDIR/$LNAME"
echo "$FILE $LNAME" >> $TRASHLOG
rm "$FILE"
