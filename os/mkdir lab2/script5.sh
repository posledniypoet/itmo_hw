#!/bin/bash
ppid_last=""
sum=0
num=0
while read line
do
ppid_now=$(echo $line| awk -F ' ' '{print $3}'| awk -F '=' '{print $2}')
art_now=$(echo $line| awk -F ' ' '{print $5}'| awk -F '=' '{print $2}')
if [[ $ppid_now == $ppid_last ]]
then num=$(($num + 1))
sum=$(echo "scale=9; $sum + $art_now" | bc)
else if [[ $num != 0 ]]
	then echo Average_Sleeping_Children_of_ParentID=$ppid_last is $(echo "scale=9; $sum / $num" | bc) >> 2_5.txt
	fi
	sum=$art_now
	num=1
	ppid_last=$ppid_now
fi
echo $line >> 2_5.txt
done < 2_4.txt 
