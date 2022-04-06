#!/bin/bash

lastBackupDate=$(ls ~ | grep -E "^Backup-" | sort -n | tail -1 | sed "s/Backup-//")
lastBackup="$HOME/Backup-${lastBackupDate}"
nowDate=$(date +"%Y-%m-%d")
nowTimestamp=$(date -d "$nowDate" +"%s")
lastBackupTimestamp=$(date -d "$lastBackupDate" +"%s")
timeDiff=$(echo "(${nowTimestamp} - ${lastBackupTimestamp}) / 60 / 60 / 24" | bc)
REPORT=~/.backup-report



if (( $timeDiff > 7 )) || [[ -z "$lastBackupDate" ]]; then
	mkdir "$HOME/Backup-${nowDate}"
	for obj in $(ls ~/source); do
		cp "$HOME/source/$obj" "$HOME/Backup-$nowDate"
	done
	flist=$(ls ~/source | sed "s/^/\t/")
	echo -e "Backup (${nowDate}) was created:\n${flist}" >> $REPORT
else
	changes=""
	for obj in $(ls ~/source); do
		if [[ -f "$lastBackup/$obj" ]]; then
			srcSize=$(wc -c "$HOME/source/${obj}" | awk '{print $1}')
			bakSize=$(wc -c "${lastBackup}/${obj}" | awk '{print $1}')
			sizeDiff=$(echo "${srcSize} - ${bakSize}" | bc)

			if (( $sizeDiff != 0 )); then
				mv "$lastBackup/$obj" "$lastBackup/$obj.$nowDate"
				cp "$HOME/source/$obj" $lastBackup
				changes="${changes}\n\tFile $obj updated. Previous version: $obj.$nowDate."
			fi
		else
			cp "$HOME/source/$obj" $lastBackup
			changes="${changes}\n\tFile $obj copied."
		fi
	done
	changes=$(echo $changes | sed 's/^\\n//')
	if [[ ! -z "$changes" ]]; then
		echo -e "Backup (${lastBackupDate}) updated:\n${changes}" >> $REPORT
	fi
fi
