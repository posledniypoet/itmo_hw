
#!/bin/bash
for pid in $(ps -eo pid h)
do
size_now=$(grep -h -i -s "^read_bytes:" /proc/$pid/io | awk -F " " '{print $2}')
if [[ -z $size_now ]]
then size_now=0
fi
echo $pid:$size_now$'\n'>>file1.tmp
done
sleep 60
echo > file2.tmp
for pid in $(ps -eo pid h)
do
find_pid=$(grep -i -s -h "^$pid:" file1.tmp)
if [[ -z $find_pid ]]
then old_size=0
else old_size=$(echo $find_pid |awk -F ":" '{print $2}')
fi
size_now=$(grep -h -i -s "read_bytes:" /proc/$pid/io | awk -F " " '{print $2}')
if [[ -z $size_now ]]
then size_now=0
fi
echo $pid:$(($size_now - $old_size)) >> file2.tmp
done
sort -rt ':' -nk 2 file1.tmp | head -n 3
rm file2.tmp
rm file1.tmp
