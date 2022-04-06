
#!/bin/bash
pids=$(ps -eo pid h)
max_size=0
pid_max_proc=""
for pid in $pids
do
now_size=$(grep -i -h -s "VmSize:" "/proc/"$pid"/status" | awk -F " " '{print $2}')
if [[ $max_size -lt $now_size ]]
then max_size=$now_size
pid_max_proc=$pid
fi
done
echo $pid_max_proc $max_size
