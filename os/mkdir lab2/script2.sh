files=$(ls /sbin)
for file in $(echo  "$files" )
do
	ps aux | grep $file | awk -F " " ' { print $2 } ' 
done > 2_2.txt

