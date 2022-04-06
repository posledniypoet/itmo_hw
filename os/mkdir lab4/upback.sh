#!/bin/bash

lastBackupDate=$(ls ~ | grep -E "^Backup-" | sort -n | tail -1 | sed 's/^Backup-//')
lastBackup="$HOME/Backup-${lastBackupDate}"

if [[ -z "$lastBackupDate" ]]; then
	echo "There's no any backup."
	exit 1
fi

if [[ ! -d ~/restore ]]; then
	mkdir ~/restore
else
	rm -r ~/restore
	mkdir ~/restore
fi

for obj in $(ls $lastBackup | grep -Ev "\.[0-9]{4}-[0-9]{2}-[0-9]{2}$"); do
	cp "${lastBackup}/${obj}" "$HOME/restore/${obj}"
done
