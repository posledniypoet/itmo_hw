
#!/bin/bash
p=$(ps h -eo pid)
for pid in $p
do
status_file="/proc/"$pid"/status"
sched_file="/proc/"$pid"/sched"
ppid=$(grep -h -s -i "ppid:" $status_file | awk -F" " '{print $2}')
sum_exec_runtime=$(grep -h -s -i "sum_exec_runtime" $sched_file | awk -F" " '{print $3}')
nr_switches=$(grep -h -s -i "nr_switches" $sched_file | awk -F" " '{print $3}')
if [[ $sum_exec_runtime != "" ]]
then art=$(echo "scale=9; $sum_exec_runtime / $nr_switches" | bc)
else art=0
fi
if [[ -n "$art" ]] 
then echo "$pid $ppid $art"
fi

done |sort -nk2 | awk '{print "ProcessID="$1" : Parent_ProcessID="$2" : Average_Running_Time="$3}' > 2_4.txt

