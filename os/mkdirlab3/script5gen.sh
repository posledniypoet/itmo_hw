
#!/bin/bash

while true; do
	read line
	echo "$line" > pipe0

	if [[ "$line" == "QUIT" ]]; then
		exit 0
	fi

	if [[ "$line" != "+" && "$line" != "*" && ! "$line" =~ [0-9]+ ]]; then
		exit 1
	fi
done
