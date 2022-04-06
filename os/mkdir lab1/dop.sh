#!/bin/bash
if [[ $# != 12 ]]; then
	echo "Invalid count of arguments."
	exit 1
else
for param in "$@"
do
let new=$param*$RANDOM
echo "Обработка параметра $param дала $new"
done
fi
