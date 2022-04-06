#!/bin/bash
start_time=$(date '+%d-%m-%Y_%H-%M-%S')
reportFile=~/report
newDirectory=~/test
url=www.net_nikogo.ru
mkdir $newDirectory && { 
echo "catalog test was created succesfully" >> $reportFile
touch $newDirectory/$start_time
}
ping $url || echo $(date '+%d-%m-%Y_%H-%M-%S') "$url not found" >> $reportFile
